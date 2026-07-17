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

# 3 Descriptive Figures
#
# Two outputs:
#   (1) financialisation_panel.png - a 3 x 2 grid: the three financialisation
#       indicators (rows) split by the two grouping variables, Region and
#       Segment (columns); each panel shows the group means plus a black "Total"
#       reference line.
#   (2) investment_roa_total.png - fixed capital investment and return on assets
#       for the whole sample only (no grouping), as two lines on one panel.
#
# Both share the winsorising and tidy-summary helpers below.

# Cap a vector at its winsor / (1 - winsor) quantiles (pooled across firm-years),
# removing the extreme ratios produced by near-zero / negative denominators.
# Set winsor = NA to disable.
cap_winsor <- function(x, winsor = 0.01) {
  if (is.na(winsor) || winsor <= 0) return(x)
  qs <- quantile(x, c(winsor, 1 - winsor), na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])
}

# Tidy yearly means for a set of named metric expressions under one grouping.
# Returns long data: Date, grp, metric, a, group_type. With totals = TRUE an
# overall "Total" series (all firms) is stacked alongside the group means.
summarise_metrics <- function(data, group_var, metrics, group_label,
                              winsor = 0.01, totals = TRUE) {
  build_one <- function(q, label) {
    d <- data %>%
      transmute(Date, grp = {{ group_var }}, value = !!q) %>%
      filter(is.finite(value)) %>%
      mutate(value = cap_winsor(value, winsor))
    grouped <- d %>% group_by(grp, Date) %>%
      summarise(a = mean(value), .groups = "drop")
    if (totals) {
      total <- d %>% group_by(Date) %>%
        summarise(a = mean(value), .groups = "drop") %>% mutate(grp = "Total")
      grouped <- bind_rows(total, grouped)
    }
    grouped %>% mutate(metric = label)
  }
  bind_rows(imap(metrics, build_one)) %>%
    mutate(group_type = group_label,
           metric = factor(metric, levels = names(metrics)))
}

# --- Metric definitions -------------------------------------------------------
# Three financialisation indicators for the grouped panel...
fin_metrics <- rlang::exprs(
  "Financial Assets / Total Assets (%)" =
    (Cash...Short.Term.Investments + Loans...Receivables...Total) * 100 / Total.Assets,
  "Shareholder Payouts / Common Equity (%)" =
    (Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net) * 100 /
      Shareholders.Equity...Common,
  "Debt / Total Revenue (%)" =
    (Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable) * 100 /
      Revenue.from.Business.Activities...Total
)
# ...and two performance indicators for the total-sample figure.
perf_metrics <- rlang::exprs(
  "Fixed Capital Expenditure (% of Market Cap)" =
    Capital.Expenditures...Total * 100 / Company.Market.Capitalization,
  "Return on Total Assets (%)" =
    Net.Income.after.Minority.Interest * 100 / Total.Assets
)

# --- (1) Six-panel financialisation grid: 3 metrics (rows) x 2 groupings (cols)
fin_panel <- bind_rows(
  summarise_metrics(fundamentals, Region,  fin_metrics, "By Region"),
  summarise_metrics(fundamentals, Segment, fin_metrics, "By Segment")
)

# Colours: black Total line; distinct Okabe-Ito hues for every group across both
# groupings (4 regions + 3 segments), skipping the pale yellow.
grp_levels    <- setdiff(sort(unique(fin_panel$grp)), "Total")
okabe_ito     <- unname(palette_okabe_ito(order = c(1, 2, 3, 5, 6, 7, 8)))
colour_values <- setNames(c("black", okabe_ito[seq_along(grp_levels)]),
                          c("Total", grp_levels))
width_values  <- setNames(c(0.9, rep(0.5, length(grp_levels))),
                          c("Total", grp_levels))

fin_plot <- ggplot(fin_panel, aes(Date, a, colour = grp, linewidth = grp)) +
  geom_line() +
  facet_grid(metric ~ group_type, scales = "free_y", switch = "y",
             labeller = labeller(metric = label_wrap_gen(18))) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_colour_manual(values = colour_values) +
  scale_linewidth_manual(values = width_values, guide = "none") +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position   = "bottom",
        panel.grid.minor  = element_blank(),
        strip.placement   = "outside",
        strip.text        = element_text(face = "bold"),
        strip.text.y.left = element_text(angle = 0))

write.csv(fin_panel, "financialisation_panel.csv", row.names = FALSE)
ggsave("financialisation_panel.png", fin_plot, width = 10, height = 8, dpi = 150)

# --- (2) Investment and return on assets, total sample only -------------------
perf_total <- bind_rows(imap(perf_metrics, function(q, label) {
  fundamentals %>%
    transmute(Date, value = !!q) %>%
    filter(is.finite(value)) %>%
    mutate(value = cap_winsor(value)) %>%
    group_by(Date) %>%
    summarise(a = mean(value), .groups = "drop") %>%
    mutate(metric = label)
})) %>%
  mutate(metric = factor(metric, levels = names(perf_metrics)))

perf_plot <- ggplot(perf_total, aes(Date, a, colour = metric)) +
  geom_line(linewidth = 0.8) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_colour_okabe_ito() +
  labs(x = NULL, y = "%", colour = NULL,
       title = "Investment and profitability, total sample") +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 11))

write.csv(perf_total, "investment_roa_total.csv", row.names = FALSE)
ggsave("investment_roa_total.png", perf_plot, width = 9, height = 5, dpi = 150)


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