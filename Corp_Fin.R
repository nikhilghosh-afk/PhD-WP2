# 1. Installing Libraries

install.packages("Cairo")
library(Cairo)
library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(plm)
library(modelsummary)

# 2. Reading & Cleaning Data

fundamentals <- read.csv("balance_sheet_data.csv", header = TRUE, stringsAsFactors = FALSE)

fundamentals <- fundamentals %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Date = floor_date(Date, "year")) %>%
  arrange(Date)

fundamentals <- fundamentals %>%
  group_by(Symbol) %>%
  mutate(across(!Date, ~ifelse(.=="", NA, as.character(.))))%>%
  fill(Country) %>%
  fill(SubRegion) %>%
  fill(GICS.Sub.Industry.Name) %>%
  mutate(Price = as.numeric(Price)) %>%
  mutate(across(7:16, as.numeric))
  

fundamentals <- fundamentals %>%
  # filter(!(GICS.Sub.Industry.Name == "Agricultural & Farm Machinery")) %>%
  # filter(!(GICS.Sub.Industry.Name == "Fertilizers & Agricultural Chemicals")) %>%
  mutate(Segment = case_when(grepl("Agricultural & Farm Machinery|Fertilizers & Agricultural Chemicals", GICS.Sub.Industry.Name) ~ "Inputs",
                             grepl("Consumer Staples Merchandise Retail|Soft Drinks & Non-alcoholic Beverages|Packaged Foods & Meats|Agricultural Products & Services", GICS.Sub.Industry.Name) ~ "Production & Processing",
                             .default = "Wholesale & Retail")
    
  )

fundamentals <- fundamentals %>%
  mutate(SubRegion = recode(SubRegion, "Japan" = "Developed Asia")) %>%
  mutate(SubRegion = recode(SubRegion, "Developed Asia" = "DAO")) %>%
  mutate(SubRegion = recode(SubRegion, "North America" = "NA")) %>%
  mutate(SubRegion = recode(SubRegion, "Developed Europe" = "DE")) %>%
  mutate(SubRegion = recode(SubRegion, "Emerging Markets" = "DEC"))
 
count_sample_segment <- fundamentals %>%
   group_by(Segment) %>%
   summarise(n=n()/16)

count_sample_region <- fundamentals %>%
  group_by(SubRegion) %>%
  summarise(n=n()/16)

# # 3.1 General Trends 
# 
# # Return on Equity by Region
# 
# roe_total_Region <- fundamentals %>%
#   group_by(Date) %>%
#   summarise(a = mean(Net.Income.after.Minority.Interest / Shareholders.Equity...Common, na.rm = TRUE),
#             .groups = "drop") %>%
#   mutate(SubRegion = "Total")
# 
# roe_grouped_Region <- fundamentals %>%
#   group_by(SubRegion, Date) %>%
#   summarise(a = mean(Net.Income.after.Minority.Interest / Shareholders.Equity...Common, na.rm = TRUE),
#             .groups = "drop") 
# 
# roe_Region <- bind_rows(roe_total_Region, roe_grouped_Region)
# 
# roe_Region %>%
#   ggplot(aes(x = Date,
#              y = a,
#              group = SubRegion,
#              linetype = SubRegion)) +
#   geom_line() +
#   scale_colour_brewer(palette = "Set1") +
#   labs(
#     y = "Average Return on Equity",
#     x = "Date",
#     linetype = "Region"
#   )
# 
# # Return on Equity by Sector
# 
# roe_total_Segment <- fundamentals %>%
#   group_by(Date) %>%
#   summarise(a = mean(Net.Income.after.Minority.Interest / Shareholders.Equity...Common, na.rm = TRUE),
#             .groups = "drop") %>%
#   mutate(Segment = "Total")
# 
# roe_grouped_Segment <- fundamentals %>%
#   group_by(Segment, Date) %>%
#   summarise(a = mean(Net.Income.after.Minority.Interest / Shareholders.Equity...Common, na.rm = TRUE),
#             .groups = "drop") 
# 
# roe_Segment <- bind_rows(roe_total_Segment, roe_grouped_Segment)
# 
# roe_Segment %>%
#   ggplot(aes(x = Date,
#              y = a,
#              group = Segment,
#              linetype = Segment)) +
#   geom_line() +
#   scale_colour_brewer(palette = "Set1") +
#   labs(
#     y = "Average Return on Equity",
#     x = "Date",
#     linetype = "Segment"
#   )

# 3.2. Constructing Metrics

calculate_metrics <- function(data, metric, group_var, y_label) {
  
  total_df <- data %>%
    group_by(Date) %>%
    summarise(a = mean({{metric}}, na.rm = TRUE), .groups = "drop") %>%
    mutate({{group_var}} := "Total")
  
  grouped_df <- data %>%
    group_by({{group_var}}, Date) %>%
    summarise(a = mean({{metric}}, na.rm = TRUE), .groups = "drop")
  
 combined_df <- bind_rows(total_df, grouped_df) 
 plot <- combined_df %>%
    ggplot(aes(x = Date, y = a, 
               colour = {{group_var}}, 
               shape = {{group_var}}, 
               linetype = {{group_var}})) +
    geom_line() +
    geom_point() +
    scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_colour_brewer(palette = "Set1") +
    labs(
      y = y_label,
      x = "Date",
      colour = as.character(substitute(group_var)),
      shape = as.character(substitute(group_var)),
      linetype = as.character(substitute(group_var))
    ) +
    theme_minimal()
  
 return(list(data = combined_df, plot = plot))
  
}

# Metric 1: Financial Assets

fin_assets_Region <- fundamentals %>% 
  calculate_metrics((Cash...Short.Term.Investments + Loans...Receivables...Total) * 100 / Total.Assets, 
                    SubRegion, "Financial Assets / Total Assets (%)")


fin_assets_Segment <- fundamentals %>% 
  calculate_metrics((Cash...Short.Term.Investments + Loans...Receivables...Total) * 100 / Total.Assets, 
                    Segment, "Financial Assets / Total Assets (%)")

# Metric 2: Shareholder Payouts

share_pay_Region <- fundamentals %>%
  calculate_metrics(Cash.Dividends.Paid...Common.Stock.Buyback...Net*100 / Shareholders.Equity...Common,
                    SubRegion, "Shareholder Payouts / Total Common Equity (%)")


share_pay_Segment <- fundamentals %>%
  calculate_metrics(Cash.Dividends.Paid...Common.Stock.Buyback...Net*100 / Shareholders.Equity...Common,
                    Segment, "Shareholder Payouts / Total Common Equity (%)") 


# Metric 3: Debt to Revenue

debt_rev_Region <- fundamentals %>%
  calculate_metrics((Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable)*100 / Revenue.from.Business.Activities...Total,
                    SubRegion, "Total Short and Long Term Debt to Total Revenue (%)")

debt_rev_Segment <- fundamentals %>%
  calculate_metrics((Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable)*100 / Revenue.from.Business.Activities...Total,
                    Segment, "Total Short and Long Term Debt to Total Revenue (%)" )

# # Metric 4: Intangible Assets
# 
# intang_assets_Region <- fundamentals %>%
#   calculate_metrics(Intangible.Assets...Total...Net*100 / Total.Assets,
#                     SubRegion, "Intangible Assets / Total Assets (%)")
# 
# intang_assets_Segment <- fundamentals %>%
#   calculate_metrics(Intangible.Assets...Total...Net*100 / Total.Assets,
#                     Segment, "Intangible Assets / Total Assets (%)")


# Outputs

write.csv(fin_assets_Region$data, "financial_assets_Region.csv")
ggsave("financial_assets_Region.png", fin_assets_Region$plot, width = 10, height = 6, dpi = 100)
write.csv(fin_assets_Segment$data, "financial_assets_Segment.csv")
ggsave("financial_assets_Segment.png", fin_assets_Segment$plot, width = 10, height = 6, dpi = 100)
write.csv(share_pay_Region$data, "share_pay_Region.csv")
ggsave("share_pay_Region.png", share_pay_Region$plot, width = 10, height = 6, dpi = 100)
write.csv(share_pay_Segment$data, "share_pay_Segment.csv")
ggsave("share_pay_Segment.png", share_pay_Segment$plot, width = 10, height = 6, dpi = 100)
write.csv(debt_rev_Region$data, "debt_rev_Region.csv")
ggsave("debt_rev_Region.png", debt_rev_Region$plot, width = 10, height = 6, dpi = 100)
write.csv(debt_rev_Segment$data, "debt_rev_Segment.csv")
ggsave("debt_rev_Segment.png", debt_rev_Segment$plot, width = 10, height = 6, dpi = 100)
# write.csv(intang_assets_Region$data, "intang_assets_Region.csv")
# ggsave("intang_assets_Region.png", intang_assets_Region$plot, width = 10, height = 6, dpi = 300)
# write.csv(intang_assets_Segment$data, "intang_assets_Segment.csv")
# ggsave("intang_assets_Segment.png", intang_assets_Segment$plot, width = 10, height = 6, dpi = 300)

# 4. Arellano-Bond Difference GMM Estimation
# Following Davis (2018, Metroeconomica) and Jibril et al. / Tori & Onaran (2022, CJE)
#
# Model: (I/K)_it = a1*(I/K)_{i,t-1} + a2*sales_{i,t-1} + a3*profit_{i,t-1}
#                  + a4*fin_{i,t-1} + a5*svo_{i,t-1} + a6*dtr_{i,t-1}
#                  + firm_FE + time_FE + e_it
#
# The Arellano-Bond estimator first-differences to remove firm fixed effects,
# then uses lagged levels (t-2 and deeper) as instruments for the endogenous
# differenced regressors.

# 4.1 Construct panel variables

panel_data <- fundamentals %>%
  mutate(
    Year = year(Date),
    # Dependent variable: Fixed investment rate (capex / total assets)
    fir = Capital.Expenditures...Total / Total.Assets,
    # Financialization measure 1: Financial assets ratio
    fin = (Cash...Short.Term.Investments + Loans...Receivables...Total) / Total.Assets,
    # Financialization measure 2: Shareholder payouts to equity
    svo = Cash.Dividends.Paid...Common.Stock.Buyback...Net / Shareholders.Equity...Common,
    # Financialization measure 3: Debt to revenue
    dtr = (Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable) / Revenue.from.Business.Activities...Total,
    # Control: Sales growth (log change in revenue) - demand proxy
    log_rev = log(Revenue.from.Business.Activities...Total),
    # Control: Profit rate (net income / total assets)
    profit = Net.Income.after.Minority.Interest / Total.Assets
  ) %>%
  select(Symbol, Year, fir, fin, svo, dtr, log_rev, profit) %>%
  filter(is.finite(fir), is.finite(fin), is.finite(svo), is.finite(dtr),
         is.finite(log_rev), is.finite(profit))

# Convert to pdata.frame (panel data frame required by plm)
pdata <- pdata.frame(panel_data, index = c("Symbol", "Year"))

# 4.2 Difference GMM (Arellano-Bond) estimation
#
# Formula structure for pgmm:
#   response ~ lagged DV + covariates | GMM instruments
#
# - The dependent variable and financialization measures are treated as
#   endogenous (instrumented with lags 2+)
# - log_rev and profit are controls for demand and profitability
# - transformation = "d" specifies difference GMM (Arellano-Bond)
# - effect = "twoways" includes time dummies
# - model = "twosteps" gives the efficient two-step estimator

# Model 1: Baseline with all three financialization measures
gmm_model <- pgmm(
  fir ~ lag(fir, 1) + lag(fin, 1) + lag(svo, 1) + lag(dtr, 1)
      + lag(log_rev, 1) + lag(profit, 1)
  | lag(fir, 2:99) + lag(fin, 2:99) + lag(svo, 2:99) + lag(dtr, 2:99),
  data = pdata,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

summary(gmm_model, robust = TRUE)

# 4.3 Diagnostic tests

# Sargan/Hansen test of overidentifying restrictions
# H0: instruments are valid. A p-value > 0.05 means instruments are acceptable.
sargan(gmm_model)

# Arellano-Bond test for serial correlation
# AR(1) in differences should be significant (expected by construction)
# AR(2) in differences should be INsignificant (validates moment conditions)
mtest(gmm_model, order = 1)  # AR(1) - expect significant
mtest(gmm_model, order = 2)  # AR(2) - expect insignificant (p > 0.05)

# 4.4 Alternative: One-step estimator (more robust standard errors)
gmm_onestep <- pgmm(
  fir ~ lag(fir, 1) + lag(fin, 1) + lag(svo, 1) + lag(dtr, 1)
      + lag(log_rev, 1) + lag(profit, 1)
  | lag(fir, 2:99) + lag(fin, 2:99) + lag(svo, 2:99) + lag(dtr, 2:99),
  data = pdata,
  effect = "twoways",
  model = "onestep",
  transformation = "d"
)

summary(gmm_onestep, robust = TRUE)

# 4.5 Output results with modelsummary
modelsummary(
  list("Two-Step GMM" = gmm_model, "One-Step GMM" = gmm_onestep),
  output = "gmm_results.txt"
)

