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
# Following Jibril, Kaltenbrunner & Kesidou (2018, FMM WP No.27),
# Orhangazi (2008, CJE), Davis (2018, Metroeconomica), Tori & Onaran (2020, SER).
#
# Jibril et al. (2018) estimate SEPARATE models for each financialisation channel:
#   Model 1 (crowding-out):        fin + fl_ta + log_ta
#   Model 2 (shareholder-value):   fin + fl_ta + svo + log_ta
#   Model 3 (debt-trap):           fin + fl_ta + dtr + log_ta
#
# All models include the lagged dependent variable (dynamic panel).
# Adapted here with log(CapEx) as the dependent variable.
#
# Estimation details (following Jibril et al. 2018):
# - All RHS variables enter with a one-period lag
# - Instruments: lagged levels (t-2 to t-4) to limit instrument proliferation
# - Control variable: log(Total Assets) as firm-size proxy
# - FL/TA included as base variable in all models (Jibril et al. 2018)
# - Two-step estimator with Windmeijer (2005) corrected robust SEs
# - Time dummies to guard against cross-section correlation

# 4.1 Construct panel variables

panel_data <- fundamentals %>%
  mutate(
    Year = year(Date),
    # Dependent variable: log(Capital Expenditures)
    log_capex = log(Capital.Expenditures...Total),
    # Financialization measure 1: Financial assets ratio (crowding-out)
    fin = (Cash...Short.Term.Investments + Loans...Receivables...Total) / Total.Assets,
    # Financialization measure 2: Shareholder payouts to equity (shareholder-value)
    svo = Cash.Dividends.Paid...Common.Stock.Buyback...Net / Shareholders.Equity...Common,
    # Financialization measure 3: Debt to revenue (debt-trap)
    dtr = (Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable) / Revenue.from.Business.Activities...Total,
    # Financial liabilities / Total Assets (Jibril et al. 2018 base variable)
    fl_ta = (Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable) / Total.Assets,
    # Control: log(Total Assets) as firm-size proxy (Jibril et al. 2018)
    log_ta = log(Total.Assets)
  ) %>%
  select(Symbol, Year, log_capex, fin, svo, dtr, fl_ta, log_ta) %>%
  filter(is.finite(log_capex), is.finite(fin), is.finite(svo), is.finite(dtr),
         is.finite(fl_ta), is.finite(log_ta))

# Convert to pdata.frame (panel data frame required by plm)
pdata <- pdata.frame(panel_data, index = c("Symbol", "Year"))

# ============================================================================
# 4.2 Model 1: Crowding-out hypothesis (Jibril et al. 2018, eq. 1)
# ============================================================================
# Tests whether higher financial asset holdings crowd out real investment.
# Base specification: fin (FA/TA), fl_ta (FL/TA), log_ta

gmm_m1 <- pgmm(
  log_capex ~ lag(log_capex, 1) + lag(fin, 1) + lag(fl_ta, 1) + lag(log_ta, 1)
  | lag(log_capex, 2:4) + lag(fin, 2:4) + lag(fl_ta, 2:4) + lag(log_ta, 2:4),
  data = pdata,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

# ============================================================================
# 4.3 Model 2: Shareholder-value orientation (Jibril et al. 2018, eq. 2-3)
# ============================================================================
# Tests whether shareholder payouts (dividends + buybacks) reduce investment.
# Augments base with svo (shareholder payouts / equity)

gmm_m2 <- pgmm(
  log_capex ~ lag(log_capex, 1) + lag(fin, 1) + lag(fl_ta, 1) + lag(svo, 1) + lag(log_ta, 1)
  | lag(log_capex, 2:4) + lag(fin, 2:4) + lag(fl_ta, 2:4) + lag(svo, 2:4)
    + lag(log_ta, 2:4),
  data = pdata,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

# ============================================================================
# 4.4 Model 3: Debt-trap hypothesis (Jibril et al. 2018, eq. 4)
# ============================================================================
# Tests whether higher debt burdens constrain real investment.
# Augments base with dtr (total debt / revenue)

gmm_m3 <- pgmm(
  log_capex ~ lag(log_capex, 1) + lag(fin, 1) + lag(fl_ta, 1) + lag(dtr, 1) + lag(log_ta, 1)
  | lag(log_capex, 2:4) + lag(fin, 2:4) + lag(fl_ta, 2:4) + lag(dtr, 2:4)
    + lag(log_ta, 2:4),
  data = pdata,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

# ============================================================================
# 4.5 Display results and diagnostics
# ============================================================================

run_diagnostics <- function(model, label) {
  cat(paste0("\n", strrep("=", 70), "\n"))
  cat(paste0("  ", label, "\n"))
  cat(paste0(strrep("=", 70), "\n"))
  print(summary(model, robust = TRUE))
  cat("\nSargan test (H0: instruments valid, want p > 0.05):\n")
  print(sargan(model))
  cat("\nAR(1) test (expected significant):\n")
  print(mtest(model, order = 1))
  cat("\nAR(2) test (must be INsignificant, p > 0.05):\n")
  print(mtest(model, order = 2))
}

run_diagnostics(gmm_m1, "Model 1: Crowding-out (fin, fl_ta, log_ta)")
run_diagnostics(gmm_m2, "Model 2: Shareholder-value (fin, fl_ta, svo, log_ta)")
run_diagnostics(gmm_m3, "Model 3: Debt-trap (fin, fl_ta, dtr, log_ta)")

# ============================================================================
# 4.6 Output results
# ============================================================================

modelsummary(
  list(
    "M1: Crowding-out" = gmm_m1,
    "M2: SVO" = gmm_m2,
    "M3: Debt-trap" = gmm_m3
  ),
  output = "gmm_results.txt"
)

