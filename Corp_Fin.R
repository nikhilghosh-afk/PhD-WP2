# 1. Installing Libraries

library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(plm)
library(modelsummary)

# 2. Reading & Cleaning Data

fundamentals <- read.csv("balance_sheet_data.csv", header = TRUE, stringsAsFactors = FALSE)

fundamentals <- fundamentals %>%
  mutate(Date = dmy(Date)) %>%
  mutate(Date = floor_date(Date, "year")) %>%
  arrange(Date)

fundamentals <- fundamentals %>%
  group_by(Symbol) %>%
  mutate(across(!Date, ~ifelse(.=="", NA, as.character(.))))%>%
  fill(Country) %>%
  fill(SubRegion) %>%
  fill(GICS.Sub.Industry.Name) %>%
  mutate(Price = as.numeric(Price)) %>%
  mutate(Company.Market.Capitalization = as.numeric(Company.Market.Capitalization)) %>%
  mutate(across(7:17, as.numeric))

# 2.1 Targeted Data Corrections

# SIGMAFA.MX 2005: market cap recorded as ~2,000 instead of ~2 billion (data provider glitch)
fundamentals <- fundamentals %>%
  mutate(Company.Market.Capitalization = ifelse(
    Symbol == "SIGMAFA.MX" & year(Date) == 2005,
    NA, Company.Market.Capitalization))

# Turkish lira redenomination (Jan 2005, 6 zeros removed):
# some pre-2005 values recorded in old TL while the rest of the series is in new TL
fundamentals <- fundamentals %>%
  mutate(
    Total.Assets = ifelse(
      Symbol %in% c("GUBRF.IS", "KENT.IS") & year(Date) %in% 2000:2003 & Total.Assets > 1e12,
      NA, Total.Assets),
    Company.Market.Capitalization = ifelse(
      Symbol %in% c("GUBRF.IS", "KENT.IS", "MGROS.IS") & year(Date) %in% 2003:2004
        & Company.Market.Capitalization > 1e10,
      NA, Company.Market.Capitalization))

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
   summarise(n=n()/17)

count_sample_region <- fundamentals %>%
  group_by(SubRegion) %>%
  summarise(n=n()/17)

# 3 Function to Calculate and Visualise Key Metrics

calculate_metrics <- function(data, metric, group_var, y_label) {
  
  total_df <- data %>%
    group_by(Date) %>%
    summarise(a = mean({{metric}}, na.rm = TRUE), .groups = "drop") %>%
    mutate({{group_var}} := "Total")
  
  grouped_df <- data %>%
    group_by({{group_var}}, Date) %>%
    summarise(a = mean({{metric}}, na.rm = TRUE), .groups = "drop")
  
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
   scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
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

# 3.1 General Trends

# # 3.1 General Trends 

# Investment


invest_Region <- fundamentals %>%
  group_by(Symbol) %>%
  calculate_metrics(Capital.Expenditures...Total * 100 / Company.Market.Capitalization,
                    SubRegion, "Fixed Capital Expenditure (% of Market Cap)")

invest_Segment <- fundamentals %>%
  calculate_metrics(Capital.Expenditures...Total * 100 / Company.Market.Capitalization,
                    Segment, "Fixed Capital Expenditure (% of Market Cap)")

write.csv(invest_Region$data, "investment_Region.csv")
ggsave("investment_Region.png", invest_Region$plot, width = 10, height = 6, dpi = 100)
write.csv(invest_Segment$data, "investment_Segment.csv")
ggsave("investment_Segment.png", invest_Segment$plot, width = 10, height = 6, dpi = 100)

# Return on Assets

roa_Region <- fundamentals %>%
  group_by(Symbol) %>%
  calculate_metrics(Net.Income.after.Minority.Interest * 100 / Total.Assets,
                    SubRegion, "Fixed Capital Expenditure (% of Market Cap)")

roa_Segment <- fundamentals %>%
  calculate_metrics(Net.Income.after.Minority.Interest * 100 / Total.Assets,
                    Segment, "Fixed Capital Expenditure (% of Market Cap)")


write.csv(roa_Region$data, "returnonassets_Region.csv")
ggsave("returnonassets_Region.png", roa_Region$plot, width = 10, height = 6, dpi = 100)
write.csv(roa_Segment$data, "returnonassets_Segment.csv")
ggsave("returnon_Segment.png", roa_Segment$plot, width = 10, height = 6, dpi = 100)


# 3.2. Indicators of Financialisation

# Metric 1: Financial Assets

fin_assets_Region <- fundamentals %>% 
  calculate_metrics((Cash...Short.Term.Investments + Loans...Receivables...Total) * 100 / Total.Assets, 
                    SubRegion, "Financial Assets / Total Assets (%)")


fin_assets_Segment <- fundamentals %>% 
  calculate_metrics((Cash...Short.Term.Investments + Loans...Receivables...Total) * 100 / Total.Assets, 
                    Segment, "Financial Assets / Total Assets (%)")

write.csv(fin_assets_Region$data, "financial_assets_Region.csv")
ggsave("financial_assets_Region.png", fin_assets_Region$plot, width = 10, height = 6, dpi = 100)
write.csv(fin_assets_Segment$data, "financial_assets_Segment.csv")
ggsave("financial_assets_Segment.png", fin_assets_Segment$plot, width = 10, height = 6, dpi = 100)


# Metric 2: Shareholder Payouts

share_pay_Region <- fundamentals %>%
  calculate_metrics((Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net)*100 / Shareholders.Equity...Common,
                    SubRegion, "Shareholder Payouts / Total Common Equity (%)")


share_pay_Segment <- fundamentals %>%
  calculate_metrics((Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net)*100 / Shareholders.Equity...Common,
                    Segment, "Shareholder Payouts / Total Common Equity (%)") 

write.csv(share_pay_Region$data, "share_pay_Region.csv")
ggsave("share_pay_Region.png", share_pay_Region$plot, width = 10, height = 6, dpi = 100)
write.csv(share_pay_Segment$data, "share_pay_Segment.csv")
ggsave("share_pay_Segment.png", share_pay_Segment$plot, width = 10, height = 6, dpi = 100)


# Metric 3: Debt to Revenue

debt_rev_Region <- fundamentals %>%
  calculate_metrics((Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable)*100 / Revenue.from.Business.Activities...Total,
                    SubRegion, "Total Short and Long Term Debt to Total Revenue (%)")

debt_rev_Segment <- fundamentals %>%
  calculate_metrics((Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable)*100 / Revenue.from.Business.Activities...Total,
                    Segment, "Total Short and Long Term Debt to Total Revenue (%)" )

write.csv(debt_rev_Region$data, "debt_rev_Region.csv")
ggsave("debt_rev_Region.png", debt_rev_Region$plot, width = 10, height = 6, dpi = 100)
write.csv(debt_rev_Segment$data, "debt_rev_Segment.csv")
ggsave("debt_rev_Segment.png", debt_rev_Segment$plot, width = 10, height = 6, dpi = 100)




# Fixed Effects Model - Has Financialization Reduced Fixed Capital Investment?

panel_data <- fundamentals %>%
mutate(
    Year = year(Date),
    # Dependent variable: log(Capital Expenditures)
    log_capex = log(Capital.Expenditures...Total / Company.Market.Capitalization),
    # Financialization measure 1: Financial assets ratio (crowding-out)
    fin = (Cash...Short.Term.Investments + Loans...Receivables...Total) / Total.Assets,
    # Financialization measure 2: Shareholder payouts to equity (shareholder-value)
    svo = (Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net) / Shareholders.Equity...Common,
    # Financialization measure 3: Debt to revenue (debt-trap)
    dtr = (Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable) / Revenue.from.Business.Activities...Total,
    # Control: log(Total Assets) as firm-size proxy
    log_ta = log(Total.Assets)
  ) %>%
  select(Symbol, Year, log_capex, fin, svo, dtr, log_ta) %>%
  filter(is.finite(log_capex), is.finite(fin), is.finite(svo), is.finite(dtr),
         is.finite(log_ta))

# Convert to pdata.frame (panel data frame required by plm)
pdata <- pdata.frame(panel_data, index = c("Symbol", "Year"))


# Two-step Difference GMM

# # - Dependent variable: log(CapEx)
# # - All RHS variables lagged one period (predetermined)
# # - Instruments: lagged levels t-2 to t-4 (limits instrument proliferation)
# # - Two-way effects (firm FE removed by differencing, time dummies included)
# # - Two-step estimator with Windmeijer (2005) corrected robust SEs

gmm_levels <- pgmm(
  log_capex ~ lag(log_capex, 1) + lag(fin, 1) + lag(svo, 1) + lag(dtr, 1) + lag(log_ta, 1)
  | lag(log_capex, 2:4) + lag(fin, 2:4) + lag(svo, 2:4) + lag(dtr, 2:4)
  + lag(log_ta, 2:4),
  data = pdata,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

summary(gmm_levels, robust = TRUE)
