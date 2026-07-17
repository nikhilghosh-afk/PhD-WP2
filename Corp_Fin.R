# Installing Libraries

library(tidyverse)
library(lubridate)
library(zoo)
library(DescTools)
library(texreg)
library(scales)
library(plm)
library(lmtest)
library(modelsummary)
library(flextable)
library(ggokabeito)

# 
# #=========================================
# 
# # Stage 1. Identifying Sample 
# 
# # The firm-level data is pulled from LSEG Worldscope. 
# # This code block removes stocks which were not listed for the entire sample, to plug back into the database.
# # Identifying de-listed stocks in a global sample seems basically impossible.
# 
# #==========================================
# 
# 
# sample <- read.csv("full_screen.csv", header = TRUE)
# prices <- read.csv("screen_price_data.csv", header = TRUE)
# 
# #----------------------------------
# # Reducing to Time Consistent Sample 

# prices <- prices %>%
# # Note Refinitiv can sometimes export dates as dmy
#   mutate(Date = ymd(Date))
#   mutate(Price.Close = as.numeric(Price.Close)) %>%
#   group_by(Instrument) %>%
#   fill(Price.Close) %>%
#   mutate(Date = floor_date(Date, "month")) %>%
#   distinct(Date, .keep_all = TRUE) %>%
#   ungroup() %>%
#   arrange(Instrument)
# 
# prices <- prices %>%
#   na.omit() %>%  
#   group_by(Instrument) %>%
#   mutate(n = n()) %>%
#   ungroup() %>%
#   filter(n == max(n)) %>%
#   select(-n) %>%
#   group_by(Instrument) %>%
#   summarise(n=n())
# 
# SAMPLE_REDUCED <- sample %>%
#   filter(Instrument %in% prices$Instrument)
# 
# SAMPLE_REDUCED_RICS <- as_tibble(SAMPLE_REDUCED$Instrument)
# 
# # These files go back into Worldscope
# 
# write.csv(SAMPLE_REDUCED, file = "WP8_food_companies_sample.csv")
# write.csv(SAMPLE_REDUCED_RICS, file = "WP8_RICs.csv")

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


# 3 Function to Calculate and Visualise Key Metrics

calculate_metrics <- function(data, group_var, ..., ncol = 2, winsor = NA) {
  
  metric_quos <- enquos(..., .named = TRUE)      # names -> facet titles
  group_col   <- as.character(ensym(group_var))
  
  # cap <- function(x) {
  #   if (is.na(winsor) || winsor <= 0) return(x)
  #   qs <- quantile(x, c(winsor, 1 - winsor), na.rm = TRUE)
  #   pmin(pmax(x, qs[1]), qs[2])
  # }
  # 
  # For one metric: winsorise the firm-year values, then take the overall
  # "Total" mean by year and the group means by year, and stack them.
  calculate_single_metric <- function(q, label) {
    d <- data %>%
      transmute(Date, grp = {{ group_var }}, value = !!q) %>%
      filter(is.finite(value))
      # mutate(value = cap(value))
    total_df   <- d %>% 
      group_by(Date) %>%
      summarise(a = mean(value), .groups = "drop") %>% 
      mutate(grp = "Total")
    grouped_df <- d %>% 
      group_by(grp, Date) %>%
      summarise(a = mean(value), .groups = "drop")
    bind_rows(total_df, grouped_df) %>% 
      mutate(metric = label)
  }
  
  combined_df <- bind_rows(imap(metric_quos, calculate_single_metric)) %>%
    # keep panels in the order the metrics were supplied
    mutate(metric = factor(metric, levels = names(metric_quos)))

  other_levels  <- setdiff(sort(unique(combined_df$grp)), "Total")
  okabe_ito     <- unname(palette_okabe_ito(order = c(1, 2, 3, 5, 6, 7)))
  colour_values <- setNames(c("black", okabe_ito[seq_along(other_levels)]),
                            c("Total", other_levels))
  width_values  <- setNames(c(0.9, rep(0.5, length(other_levels))),
                            c("Total", other_levels))
  
  plot <- ggplot(combined_df,
                 aes(x = Date, y = a, colour = grp, linewidth = grp)) +
    geom_line() +
    facet_wrap(~ metric, scales = "free_y", ncol = ncol) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
    scale_colour_manual(values = colour_values) +
    scale_linewidth_manual(values = width_values, guide = "none") +
    labs(x = NULL, y = NULL, colour = group_col) +
    theme_minimal(base_size = 11) +
    theme(legend.position  = "bottom",
          panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"))
  
  return(list(data = combined_df, plot = plot))
}

# Five key metrics, faceted into a single figure per grouping. Swap the first
# argument (Region <-> Segment) to change the grouping; the metrics are identical.

metric_exprs <- rlang::exprs(
  "Fixed Capital Expenditure (% of Market Cap)"      =
    Capital.Expenditures...Total * 100 / Company.Market.Capitalization,
  "Annual Return on Total Assets (%)"                =
    Net.Income.after.Minority.Interest * 100 / Total.Assets,
  "Financial Assets / Total Assets (%)"              =
    (Cash...Short.Term.Investments + Loans...Receivables...Total) * 100 / Total.Assets,
  "Shareholder Payouts / Total Common Equity (%)"    =
    (Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net) * 100 /
    Shareholders.Equity...Common,
  "Total Short and Long Term Debt to Total Revenue (%)" =
    (Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable) * 100 /
    Revenue.from.Business.Activities...Total
)

metrics_Region  <- calculate_metrics(fundamentals, Region,  !!!metric_exprs)
metrics_Segment <- calculate_metrics(fundamentals, Segment, !!!metric_exprs)

write.csv(metrics_Region$data,  "metrics_Region.csv",  row.names = FALSE)
ggsave("metrics_Region.png",  metrics_Region$plot,  width = 11, height = 7, dpi = 150)
write.csv(metrics_Segment$data, "metrics_Segment.csv", row.names = FALSE)
ggsave("metrics_Segment.png", metrics_Segment$plot, width = 11, height = 7, dpi = 150)


# # Headline figure: the five indicators (overall "Total" only) each rebased to
# # 100 in the base year, on one common indexed scale. This puts the paper's
# # thesis - financialisation channels rising while investment falls - in a single
# # panel, without a dual axis. The "Total" series is identical across groupings,
# # so it is taken from the Region output.
# 
# short_labels <- c(
#   "Fixed Capital Expenditure (% of Market Cap)"         = "Investment",
#   "Annual Return on Total Assets (%)"                   = "Return on assets",
#   "Financial Assets / Total Assets (%)"                 = "Financial assets",
#   "Shareholder Payouts / Total Common Equity (%)"       = "Payouts / equity",
#   "Total Short and Long Term Debt to Total Revenue (%)" = "Debt / revenue"
# )
# 
# index_base <- metrics_Region$data %>%
#   filter(grp == "Total", is.finite(a)) %>%
#   group_by(metric) %>%
#   arrange(Date) %>%
#   mutate(index = 100 * a / first(a),                 # rebase each series to base year
#          label = short_labels[as.character(metric)]) %>%
#   ungroup()
# 
# indexed_plot <- ggplot(index_base, aes(Date, index, colour = metric)) +
#   geom_hline(yintercept = 100, linetype = "dashed", colour = "grey70") +
#   geom_line(linewidth = 0.8) +
#   # direct labels at the end of each line (short names) instead of a legend
#   ggrepel::geom_text_repel(
#     data = index_base %>% group_by(metric) %>% slice_max(Date, n = 1) %>% ungroup(),
#     aes(label = label), hjust = 0, direction = "y", size = 3.2,
#     nudge_x = 250, segment.colour = NA, min.segment.length = Inf) +
#   scale_x_date(date_breaks = "5 years", date_labels = "%Y",
#                expand = expansion(mult = c(0.02, 0.28))) +
#   scale_colour_okabe_ito(order = c(9, 1, 2, 3, 6)) +   # black, orange, sky, green, vermillion
#   labs(x = NULL, y = "Index (base year = 100)",
#        title = "Financialisation and fixed investment in agri-food, indexed to base year") +
#   guides(colour = "none") +                          # labels replace the legend
#   theme_minimal(base_size = 11) +
#   theme(panel.grid.minor = element_blank(),
#         plot.title = element_text(face = "bold", size = 11))
# 
# write.csv(index_base, "metrics_indexed.csv", row.names = FALSE)
# ggsave("metrics_indexed.png", indexed_plot, width = 10, height = 6, dpi = 150)



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

fe_ct  <- lmtest::coeftest(fe_model, vcov = fe_vcov)   # clustered FE
gmm_ex <- extract(gmm_diff)                            # Windmeijer GMM, aligned

# rename + order coefficients; anything not listed (e.g. time dummies) is dropped
coef_map <- list(
  "lag(inv, 1)"      = "Lagged investment (I/K)$_{t-1}$",
  "lag(fa, 1)"       = "Financial assets / TA (crowding-out)",
  "lag(svo, 1)"      = "Payouts / equity (shareholder-value)",
  "lag(lev, 1)"      = "Debt / TA (debt-trap)",
  "lag(roa, 1)"      = "Return on assets (profit rate)",
  "lag(turnover, 1)" = "Sales / TA (accelerator)",
  "lag(log_ta, 1)"   = "log(Total assets) (size)"
)

texreg(
  list(fe_model, gmm_diff),
  override.se        = list(fe_ct[, "Std. Error"], gmm_ex@se),
  override.pval      = list(fe_ct[, "Pr(>|t|)"],   gmm_ex@pvalues),
  custom.model.names = c("Fixed Effects", "Difference GMM"),
  custom.coef.map    = coef_map,
  stars              = c(0.01, 0.05, 0.1),
  digits             = 3,
  caption            = "Financialisation and fixed capital investment in the agri-food sector",
  caption.above      = TRUE,
  label              = "tab:fin_investment",
  booktabs           = TRUE,
  use.packages       = FALSE,   # booktabs/dcolumn assumed already in your preamble
  custom.note        = "\\item %stars. FE: two-way within estimator, firm-clustered robust SEs. Difference GMM: two-step Arellano--Bond, Windmeijer (2005) corrected SEs, instruments = lagged levels $t-2$ to $t-4$ (collapsed). All regressors lagged one period; time dummies included but not shown.",
  file               = "regression_results.tex"
)