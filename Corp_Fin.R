# Installing Libraries

library(tidyverse)
library(lubridate)
library(zoo)
library(DescTools)
library(texreg)
library(scales)
library(RColorBrewer)
library(plm)
library(lmtest)
library(modelsummary)
library(flextable)


#=========================================

# Stage 1. Identifying Sample 

# The firm-level data is pulled from LSEG Worldscope. 
# This code block removes stocks which were not listed for the entire sample, to plug back into the database.
# Identifying de-listed stocks in a global sample seems basically impossible.

#==========================================


sample <- read.csv("full_screen.csv", header = TRUE)
prices <- read.csv("screen_price_data.csv", header = TRUE)

#----------------------------------
# Reducing to Time Consistent Sample 
#----------------------------------

prices <- prices %>%
# Note Refinitiv can sometimes export dates as dmy
  mutate(Date = ymd(Date))
  mutate(Price.Close = as.numeric(Price.Close)) %>%
  group_by(Instrument) %>%
  fill(Price.Close) %>%
  mutate(Date = floor_date(Date, "month")) %>%
  distinct(Date, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(Instrument)

prices <- prices %>%
  na.omit() %>%  
  group_by(Instrument) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == max(n)) %>%
  select(-n) %>%
  group_by(Instrument) %>%
  summarise(n=n())

SAMPLE_REDUCED <- sample %>%
  filter(Instrument %in% prices$Instrument)

SAMPLE_REDUCED_RICS <- as_tibble(SAMPLE_REDUCED$Instrument)

# These files go back into Worldscope

write.csv(SAMPLE_REDUCED, file = "WP8_food_companies_sample.csv")
write.csv(SAMPLE_REDUCED_RICS, file = "WP8_RICs.csv")

#=========================================
# Stage 2. Data Analysis  

# The firm-level data is pulled from LSEG Worldscope. 
# This performs the descriptive and inferential stats analysis on the sample.
#==========================================


#----------------------------------
# Reading and cleaning Data
#----------------------------------

fundamentals <- read.csv("balance_sheet_data.csv", header = TRUE, stringsAsFactors = FALSE)

fundamentals <- fundamentals %>%
  mutate(Date = year(Date)) %>%
  arrange(Date) %>%
  select(-X)

fundamentals <- fundamentals %>%
  group_by(Instrument) %>%
  mutate(across(!Date, ~ifelse(.=="", NA, as.character(.)))) %>%
  fill(Country.of.Exchange) %>%
  fill(Earnings.Quality.Region) %>%
  fill(GICS.Sub.Industry.Name) %>%
  ungroup() %>%
  mutate(across(-c(Instrument, Date, Country.of.Exchange, GICS.Sub.Industry.Name, Earnings.Quality.Region), ~as.numeric(.x))) %>%
  mutate(
    Common.Stock.Buyback...Net       = coalesce(Common.Stock.Buyback...Net, 0),
    Short.Term.Debt...Notes.Payable  = coalesce(Short.Term.Debt...Notes.Payable, 0)
  )

# Targeted Data Corrections
# Some reporting issues were identified for a few companies, which are screened out below


# SIGMAFA.MX 2005
fundamentals <- fundamentals %>%
  mutate(Company.Market.Capitalization = ifelse(
    Instrument == "SIGMAFA.MX" & Date == 2005,
    NA, Company.Market.Capitalization))

# Turkish lira redenomination (Jan 2005, 6 zeros removed)
fundamentals <- fundamentals %>%
  mutate(
    Total.Assets = ifelse(
      Instrument %in% c("GUBRF.IS", "KENT.IS") & Date %in% 2000:2003 & Total.Assets > 1e12,
      NA, Total.Assets),
    Company.Market.Capitalization = ifelse(
      Instrument %in% c("GUBRF.IS", "KENT.IS", "MGROS.IS") & Date %in% 2003:2004
        & Company.Market.Capitalization > 1e10,
      NA, Company.Market.Capitalization))

#----------------------------------
# Grouping Data by region and segment
#----------------------------------
fundamentals <- fundamentals %>%
  # filter(!(GICS.Sub.Industry.Name == "Agricultural & Farm Machinery")) %>%
  # filter(!(GICS.Sub.Industry.Name == "Fertilizers & Agricultural Chemicals")) %>%
  mutate(Segment = case_when(grepl("Agricultural & Farm Machinery|Fertilizers & Agricultural Chemicals", GICS.Sub.Industry.Name) ~ "Inputs",
                             grepl("Consumer Staples Merchandise Retail|Soft Drinks & Non-alcoholic Beverages|Packaged Foods & Meats|Agricultural Products & Services", GICS.Sub.Industry.Name) ~ "Production & Processing",
                             .default = "Wholesale & Retail")
    
  )

fundamentals <- fundamentals %>%
  mutate(Region = recode(Earnings.Quality.Region,
                         "Japan"            = "Developed Asia and Oceanea",
                         "Developed Asia"   = "Developed Asia and Oceanea",
                         "Emerging Markets" = "Developing and Emerging Economies"
  ))
 
count_sample_segment <- fundamentals %>%
   group_by(Segment, Date) %>%
   summarise(n=n(), .groups = "drop")

count_sample_region <- fundamentals %>%
  group_by(Region, Date) %>%
  summarise(n=n(), .groups = "drop")


#----------------------------------
# Descriptive Statistics
#----------------------------------


# Function to Calculate and Visualise Key Metrics
calculate_metrics <- function(data, metric, group_var, y_label) {
  
  total_df <- data %>%
    group_by(Date) %>%
    summarise(a = median({{metric}}, na.rm = TRUE), .groups = "drop") %>%
    mutate({{group_var}} := "Total")
  
  grouped_df <- data %>%
    group_by({{group_var}}, Date) %>%
    summarise(a = median({{metric}}, na.rm = TRUE), .groups = "drop")
  
 combined_df <- bind_rows(total_df, grouped_df)
 
 group_col <- as.character(substitute(group_var))
 group_levels <- sort(unique(combined_df[[group_col]]))
 other_levels <- setdiff(group_levels, "Total")
 
 set1_colours <- brewer.pal(max(3, length(other_levels)), "Set1")
 colour_values <- setNames(
   c("black", set1_colours[seq_along(other_levels)]),
   c("Total", other_levels))
 shape_values <- setNames(
   c(16, 17:(16 + length(other_levels))),
   c("Total", other_levels))
 linetype_values <- setNames(
   c("solid", rep("dashed", length(other_levels))),
   c("Total", other_levels))
 
 plot <- combined_df %>%
   ggplot(aes(x = Date, y = a,
              colour = {{group_var}},
              shape = {{group_var}},
              linetype = {{group_var}})) +
   geom_line() +
   geom_point() +
   scale_x_continuous(breaks = seq(2000, 2025, 2)) +
   scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
   scale_colour_manual(values = colour_values) +
   scale_shape_manual(values = shape_values) +
   scale_linetype_manual(values = linetype_values) +
   labs(
     y = y_label,
     x = "Date",
     colour = group_col,
     shape = group_col,
     linetype = group_col
   ) +
   theme_minimal()
  
 return(list(data = combined_df, plot = plot))
  
}


# Investment


invest_Region <- fundamentals %>%
  group_by(Instrument) %>%
  calculate_metrics(Capital.Expenditures...Total * 100 / Company.Market.Capitalization,
                    Region, "Fixed Capital Expenditure (% of Market Cap)")

invest_Segment <- fundamentals %>%
  calculate_metrics(Capital.Expenditures...Total * 100 / Company.Market.Capitalization,
                    Segment, "Fixed Capital Expenditure (% of Market Cap)")

write.csv(invest_Region$data, "investment_Region.csv")
ggsave("investment_Region.png", invest_Region$plot, width = 10, height = 6, dpi = 500)
write.csv(invest_Segment$data, "investment_Segment.csv")
ggsave("investment_Segment.png", invest_Segment$plot, width = 10, height = 6, dpi = 500)

# Return on Assets

roa_Region <- fundamentals %>%
  group_by(Instrument) %>%
  calculate_metrics(Net.Income.after.Minority.Interest * 100 / Total.Assets,
                    Region, "Annual Return on Total Assets (%)")

roa_Segment <- fundamentals %>%
  calculate_metrics(Net.Income.after.Minority.Interest * 100 / Total.Assets,
                    Segment, "Annual Return on Total Assets (%)")


write.csv(roa_Region$data, "returnonassets_Region.csv")
ggsave("returnonassets_Region.png", roa_Region$plot, width = 10, height = 6, dpi = 500)
write.csv(roa_Segment$data, "returnonassets_Segment.csv")
ggsave("returnonassets_Segment.png", roa_Segment$plot, width = 10, height = 6, dpi = 500)


# Metric 1: Financial Assets

fin_assets_Region <- fundamentals %>% 
  calculate_metrics((Cash...Short.Term.Investments + Loans...Receivables...Total) * 100 / Total.Assets, 
                    Region, "Financial Assets / Total Assets (%)")


fin_assets_Segment <- fundamentals %>% 
  calculate_metrics((Cash...Short.Term.Investments + Loans...Receivables...Total) * 100 / Total.Assets, 
                    Segment, "Financial Assets / Total Assets (%)")

write.csv(fin_assets_Region$data, "financial_assets_Region.csv")
ggsave("financial_assets_Region.png", fin_assets_Region$plot, width = 10, height = 6, dpi = 500)
write.csv(fin_assets_Segment$data, "financial_assets_Segment.csv")
ggsave("financial_assets_Segment.png", fin_assets_Segment$plot, width = 10, height = 6, dpi = 500)


# Metric 2: Shareholder Payouts

share_pay_Region <- fundamentals %>%
  calculate_metrics((Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net)*100 / Shareholders.Equity...Common,
                    Region, "Shareholder Payouts / Total Common Equity (%)")


share_pay_Segment <- fundamentals %>%
  calculate_metrics((Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net)*100 / Shareholders.Equity...Common,
                    Segment, "Shareholder Payouts / Total Common Equity (%)") 

write.csv(share_pay_Region$data, "share_pay_Region.csv")
ggsave("share_pay_Region.png", share_pay_Region$plot, width = 10, height = 6, dpi = 500)
write.csv(share_pay_Segment$data, "share_pay_Segment.csv")
ggsave("share_pay_Segment.png", share_pay_Segment$plot, width = 10, height = 6, dpi = 500)


# Metric 3: Debt to Revenue

debt_rev_Region <- fundamentals %>%
  calculate_metrics((Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable)*100 / Revenue.from.Business.Activities...Total,
                    Region, "Total Short and Long Term Debt to Total Revenue (%)")

debt_rev_Segment <- fundamentals %>%
  calculate_metrics((Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable)*100 / Revenue.from.Business.Activities...Total,
                    Segment, "Total Short and Long Term Debt to Total Revenue (%)" )

write.csv(debt_rev_Region$data, "debt_rev_Region.csv")
ggsave("debt_rev_Region.png", debt_rev_Region$plot, width = 10, height = 6, dpi = 500)
write.csv(debt_rev_Segment$data, "debt_rev_Segment.csv")
ggsave("debt_rev_Segment.png", debt_rev_Segment$plot, width = 10, height = 6, dpi = 500)


#=========================================
# Inferential Statistics
# We estimate a Fixed Effects and a two step Difference GMM
#=========================================


#-------------------------------
# Build the panel
#-----------------------------=


panel_data <- fundamentals %>%
  mutate(
    inv = Capital.Expenditures...Total / Total.Assets,
    fa = (Cash...Short.Term.Investments + Loans...Receivables...Total) / Total.Assets,
    svo = (Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net) /
      Shareholders.Equity...Common,
    lev = (Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable) / Total.Assets,
    roa = Net.Income.after.Minority.Interest / Total.Assets,
    turnover = Revenue.from.Business.Activities...Total / Total.Assets,
    log_ta = log(Total.Assets)
  ) %>%
  select(Instrument, Date, inv, fa, svo, lev, roa, turnover, log_ta) %>%
  filter(if_all(c(inv, fa, svo, lev, roa, turnover, log_ta), is.finite)) %>%
  mutate(across(c(inv, fa, svo, lev, roa, turnover),
                ~ DescTools::Winsorize(.x, val = quantile(.x, probs = c(0.01, 0.99), na.rm = TRUE))))

pdata <- pdata.frame(panel_data, index = c("Instrument", "Date"))

datasummary_skim(panel_data %>% select(inv, fa, svo, lev, roa, turnover, log_ta))


#------------------------------
# Two way fixed effects model
#-------------------------------

fe_model <- plm(
  inv ~ lag(fa, 1) + lag(svo, 1) + lag(lev, 1) +
        lag(roa, 1) + lag(turnover, 1) + lag(log_ta, 1),
  data   = pdata,
  model  = "within",
  effect = "twoways"
)

fe_vcov <- vcovHC(fe_model, method = "arellano", type = "sss", cluster = "group")
coeftest(fe_model, vcov = fe_vcov)

#---------------------------
# Two step Difference GMM
#---------------------------


gmm_diff <- pgmm(
  inv ~ lag(inv, 1) + lag(fa, 1) + lag(svo, 1) + lag(lev, 1) +
        lag(roa, 1) + lag(turnover, 1) + lag(log_ta, 1)
  | lag(inv, 2:4) + lag(fa, 2:4) + lag(svo, 2:4) + lag(lev, 2:4) +
        lag(roa, 2:4) + lag(turnover, 2:4) + lag(log_ta, 2:4),
  data           = pdata,
  effect         = "twoways",
  model          = "twosteps",
  transformation = "d",
  collapse = TRUE
)

summary(gmm_diff, robust = TRUE)


# Outputs

gmm_vcov <- vcovHC(gmm_diff)

models <- list(
  "Fixed Effects"   = fe_model,
  "Difference GMM"  = gmm_diff
)

modelsummary(
  models,
  vcov      = list(fe_vcov, gmm_vcov),   # clustered FE; Windmeijer-corrected GMM
  stars     = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_rename = c(
    "lag(inv, 1)"      = "Lagged investment (I/K)",
    "lag(fa, 1)"       = "Financial assets / TA  (crowding-out)",
    "lag(svo, 1)"      = "Payouts / equity  (shareholder-value)",
    "lag(lev, 1)"      = "Debt / TA  (debt-trap)",
    "lag(roa, 1)"      = "Return on assets  (profit rate)",
    "lag(turnover, 1)" = "Sales / TA  (accelerator)",
    "lag(log_ta, 1)"   = "log(Total assets)  (size)"
  ),
  gof_omit  = "Adj|AIC|BIC|Log|RMSE",
  title     = "Financialisation and fixed capital investment in the agri-food sector",
  notes     = "FE: two-way within estimator, firm-clustered robust SEs. Difference GMM: two-step Arellano-Bond, Windmeijer (2005) corrected SEs, instruments = lagged levels t-2..t-4. All regressors lagged one period.",
  output    = "markdown"
)

screenreg(list("Fixed Effects" = fe_model, "Difference GMM" = gmm_diff),
          override.se   = list(sqrt(diag(fe_vcov)), sqrt(diag(gmm_vcov))),
          override.pval = list(...),               # optional
          custom.gof.rows = list(),                # it already adds Sargan/AR/obs
          digits = 3)

# To export the same table for the paper, swap the output argument, e.g.:
#   output = "regression_results.docx"   (needs flextable)
#   output = "regression_results.tex"    (LaTeX, needs kableExtra)

fe_se  <- sqrt(diag(fe_vcov))
fe_p   <- 2 * pnorm(-abs(coef(fe_model)  / fe_se))

gmm_se <- sqrt(diag(gmm_vcov))
gmm_p  <- 2 * pnorm(-abs(coef(gmm_diff) / gmm_se))

screenreg(
  list("Fixed Effects" = fe_model, "Difference GMM" = gmm_diff),
  override.se   = list(fe_se,  gmm_se),
  override.pval = list(fe_p,   gmm_p),
  digits        = 3
)

fe_ct <- lmtest::coeftest(fe_model, vcov = fe_vcov)

# GMM: take exactly what texreg extracts (already Windmeijer-corrected)
gmm_ex <- extract(gmm_diff)     # texreg object; @se and @pvalues are aligned

screenreg(
  list("Fixed Effects" = fe_model, "Difference GMM" = gmm_diff),
  override.se   = list(fe_ct[, "Std. Error"], gmm_ex@se),
  override.pval = list(fe_ct[, "Pr(>|t|)"],   gmm_ex@pvalues),
  digits        = 3
)
