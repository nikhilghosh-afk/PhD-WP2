# Installing Libraries

library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(plm)
library(modelsummary)

# Stage 1. Cleaning Data


sample <- read.csv("full_screen.csv", header = TRUE)
prices <- read.csv("screen_price_data.csv", header = TRUE)


# Reducing to Time Consistent Sample 

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

# These files go back into Refinitiv

write.csv(SAMPLE_REDUCED, file = "WP8_food_companies_sample.csv")
write.csv(SAMPLE_REDUCED_RICS, file = "WP8_RICs.csv")

# Stage 2. Data Analysis

# Reading and cleaning Data

fundamentals <- read.csv("balance_sheet_data.csv", header = TRUE, stringsAsFactors = FALSE)

fundamentals <- fundamentals %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Date = floor_date(Date, "year")) %>%
  arrange(Date) %>%
  select(-X)

fundamentals <- fundamentals %>%
  group_by(Instrument) %>%
  mutate(across(!Date, ~ifelse(.=="", NA, as.character(.))))%>%
  fill(Country.of.Exchange) %>%
  fill(Earnings.Quality.Region) %>%
  fill(GICS.Sub.Industry.Name) %>%
  mutate(Price.Close = as.numeric(Price.Close)) %>%
  mutate(Company.Market.Capitalization = as.numeric(Company.Market.Capitalization)) %>%
  mutate(across(7:17, as.numeric))

# 2.1 Targeted Data Corrections

# SIGMAFA.MX 2005: market cap recorded as ~2,000 instead of ~2 billion (data provider glitch)
fundamentals <- fundamentals %>%
  mutate(Company.Market.Capitalization = ifelse(
    Instrument == "SIGMAFA.MX" & year(Date) == 2005,
    NA, Company.Market.Capitalization))

# Turkish lira redenomination (Jan 2005, 6 zeros removed):
# some pre-2005 values recorded in old TL while the rest of the series is in new TL
fundamentals <- fundamentals %>%
  mutate(
    Total.Assets = ifelse(
      Instrument %in% c("GUBRF.IS", "KENT.IS") & year(Date) %in% 2000:2003 & Total.Assets > 1e12,
      NA, Total.Assets),
    Company.Market.Capitalization = ifelse(
      Instrument %in% c("GUBRF.IS", "KENT.IS", "MGROS.IS") & year(Date) %in% 2003:2004
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

# 3 Function to Calculate and Visualise Key Metrics
#
# calculate_metrics() takes any grouping variable (Region, Segment, ...) plus a
# set of NAMED metric expressions, and returns one faceted figure with a panel
# per metric. Each panel shows the group means plus an overall "Total" line, so
# the five financialisation indicators are read as one comparable small-multiple
# rather than as five separate look-alike charts.
#
#   ...          named metric expressions; the name becomes the facet (panel) title
#   group_var    the grouping variable, unquoted (e.g. Region or Segment)
#   ncol         number of facet columns

calculate_metrics <- function(data, group_var, ..., ncol = 2) {

  metric_quos <- enquos(..., .named = TRUE)      # names -> facet titles
  group_col   <- as.character(ensym(group_var))

  # For one metric: overall "Total" mean by year + group means by year, stacked.
  build_one <- function(q, label) {
    total_df <- data %>%
      group_by(Date) %>%
      summarise(a = mean(!!q, na.rm = TRUE), .groups = "drop") %>%
      mutate(grp = "Total")
    grouped_df <- data %>%
      group_by({{ group_var }}, Date) %>%
      summarise(a = mean(!!q, na.rm = TRUE), .groups = "drop") %>%
      rename(grp = {{ group_var }})
    bind_rows(total_df, grouped_df) %>% mutate(metric = label)
  }

  combined_df <- bind_rows(imap(metric_quos, build_one)) %>%
    # keep panels in the order the metrics were supplied
    mutate(metric = factor(metric, levels = names(metric_quos)))

  # Colours: black reference line for "Total"; Okabe-Ito (colour-blind safe) for
  # the groups, assigned in a fixed order. Total is drawn slightly heavier.
  other_levels  <- setdiff(sort(unique(combined_df$grp)), "Total")
  okabe_ito     <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2",
                     "#D55E00", "#CC79A7", "#F0E442")
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




################################################################################
# 4. INFERENTIAL ANALYSIS: HAS FINANCIALISATION REDUCED FIXED CAPITAL INVESTMENT?
#
# Model follows the firm-level financialisation-investment literature:
#   - Orhangazi (2008, CJE) and Tori & Onaran (2018/2020) for the dynamic
#     investment (accumulation-rate) equation with an accelerator (sales) and a
#     profit-rate/internal-funds term;
#   - Davis (2017, J. Econ. Surveys) and Jibril, Kaltenbrunner & Kesidou (2018)
#     for the three financialisation channels (crowding-out, shareholder-value,
#     debt-trap) and the POLS / Fixed-Effects / difference-GMM estimation ladder.
#
# Design note: this is a SINGLE sector (agri-food) across MANY countries, so
# firm fixed effects absorb time-invariant country/firm heterogeneity and a full
# set of year dummies absorbs common (global) macro shocks.
#
# Data note: Refinitiv export does not contain net fixed capital stock (K = net
# PP&E), operating income, interest paid, or non-operating financial income.
# The exact Orhangazi/Tori-Onaran I/K accelerator cannot therefore be built
# verbatim; investment is normalised by total assets (the standard fallback for
# the accumulation rate) and profitability/demand are proxied by ROA and asset
# turnover. The three financialisation channels map directly onto Davis (2017).
################################################################################

library(lmtest)   # coeftest() for robust/clustered inference on the FE model

# ---- 4.1 Build the firm-year panel ------------------------------------------
# All variables in ratio form so coefficients are comparable across firms and
# countries. Winsorising (below) tames the extreme balance-sheet outliers that
# are endemic to firm-level data (Tori & Onaran, 2020, Sec. 4).

winsorise <- function(x, p = 0.01) {
  q <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

panel_data <- fundamentals %>%
  # A blank in these two items is structural, not missing: no reported buyback
  # means the firm repurchased no stock that year, and no reported short-term
  # debt means it carries none. Code them as 0 (standard Worldscope/Refinitiv
  # practice) so an otherwise-complete firm-year is not discarded. This alone
  # roughly doubles the estimation sample. Genuine denominators (total assets,
  # equity, revenue) are still required below.
  mutate(
    Common.Stock.Buyback...Net       = coalesce(Common.Stock.Buyback...Net, 0),
    Short.Term.Debt...Notes.Payable  = coalesce(Short.Term.Debt...Notes.Payable, 0)
  ) %>%
  mutate(
    Year = year(Date),

    # Dependent variable: rate of fixed capital accumulation (I / K, with total
    # assets proxying the capital stock). This is the analogue of Orhangazi's
    # I/K and of Jibril's investment-in-intangibles dependent variable.
    inv = Capital.Expenditures...Total / Total.Assets,

    # --- Financialisation channel 1: CROWDING-OUT (Davis 2017; Jibril FA/TA) ---
    # Financial assets as a share of total assets. Higher => firm tilts its
    # portfolio towards financial rather than productive assets.
    fa = (Cash...Short.Term.Investments + Loans...Receivables...Total) / Total.Assets,

    # --- Financialisation channel 2: SHAREHOLDER-VALUE (Davis 2017; Jibril) ----
    # Total payouts (dividends + net buybacks) relative to common equity.
    svo = (Dividends.Paid...Cash...Total...Cash.Flow + Common.Stock.Buyback...Net) /
      Shareholders.Equity...Common,

    # --- Financialisation channel 3: DEBT-TRAP (Jibril FL/TA; Tori-Onaran TD/TA)
    # Total (short + long term) debt relative to total assets.
    lev = (Debt...Long.Term...Total + Short.Term.Debt...Notes.Payable) / Total.Assets,

    # --- Control: profit rate / internal funds (Orhangazi (pi-CD)/K proxy) -----
    roa = Net.Income.after.Minority.Interest / Total.Assets,

    # --- Control: accelerator / demand (Orhangazi & Tori-Onaran S/K proxy) -----
    turnover = Revenue.from.Business.Activities...Total / Total.Assets,

    # --- Control: firm size (Jibril log(TA)) ----------------------------------
    log_ta = log(Total.Assets)
  ) %>%
  select(Instrument, Year, inv, fa, svo, lev, roa, turnover, log_ta) %>%
  # keep only finite observations (drops firm-years with missing balance-sheet
  # items or non-positive total assets)
  filter(if_all(c(inv, fa, svo, lev, roa, turnover, log_ta), is.finite)) %>%
  # winsorise the continuous variables at the 1st/99th percentiles
  mutate(across(c(inv, fa, svo, lev, roa, turnover), winsorise))

# Convert to a panel data frame (index = firm, year) for plm / pgmm
pdata <- pdata.frame(panel_data, index = c("Instrument", "Year"))

# Descriptive statistics of the estimation sample (cf. Jibril et al. Table 3)
datasummary_skim(panel_data %>% select(inv, fa, svo, lev, roa, turnover, log_ta))


# ---- 4.2 Fixed-Effects (within) estimator -----------------------------------
# Static two-way FE (firm + year), following Jibril et al. (Table 6). Firm
# effects control for time-invariant country/firm heterogeneity; year dummies
# absorb common global shocks. All regressors lagged one period (predetermined),
# as investment responds to the balance sheet with a delay.
#
# NB: the lagged dependent variable is deliberately EXCLUDED here. In a short-T
# dynamic panel the within-transformation makes the lagged dependent correlated
# with the demeaned error (Nickell 1981 bias) - which is precisely why we also
# estimate the difference-GMM model in 4.3.

fe_model <- plm(
  inv ~ lag(fa, 1) + lag(svo, 1) + lag(lev, 1) +
        lag(roa, 1) + lag(turnover, 1) + lag(log_ta, 1),
  data   = pdata,
  model  = "within",
  effect = "twoways"
)

# Standard errors clustered by firm and robust to heteroskedasticity and within
# firm serial correlation (Arellano 1987).
fe_vcov <- vcovHC(fe_model, method = "arellano", type = "sss", cluster = "group")
coeftest(fe_model, vcov = fe_vcov)


# ---- 4.3 Two-step difference-GMM (Arellano & Bond 1991) ---------------------
# Dynamic specification adding the lagged dependent variable (investment is a
# persistent, path-dependent process: Orhangazi 2008; Tori & Onaran 2020).
#   - transformation = "d"  : first-difference GMM (sweeps out firm effects)
#   - effect = "twoways"    : includes time dummies for common shocks
#   - model  = "twosteps"   : efficient two-step estimator
#   - instruments: lagged LEVELS t-2 to t-4 of all predetermined regressors
#     (2 <= t <= 4 limits instrument proliferation, as in Jibril et al.)
#   - summary(robust = TRUE): Windmeijer (2005) finite-sample corrected SEs

gmm_diff <- pgmm(
  inv ~ lag(inv, 1) + lag(fa, 1) + lag(svo, 1) + lag(lev, 1) +
        lag(roa, 1) + lag(turnover, 1) + lag(log_ta, 1)
  | lag(inv, 2:4) + lag(fa, 2:4) + lag(svo, 2:4) + lag(lev, 2:4) +
        lag(roa, 2:4) + lag(turnover, 2:4) + lag(log_ta, 2:4),
  data           = pdata,
  effect         = "twoways",
  model          = "twosteps",
  transformation = "d"
)

# Coefficients with Windmeijer-corrected robust SEs, plus the identifying
# diagnostics: Sargan/Hansen test of over-identifying restrictions and the
# Arellano-Bond AR(1)/AR(2) tests for serial correlation in the residuals.
# Validity requires: AR(1) significant, AR(2) NOT significant, Sargan not
# rejected.
summary(gmm_diff, robust = TRUE)


# ---- 4.4 Side-by-side results table -----------------------------------------
# Fixed-effects (clustered SEs) vs. two-step difference-GMM (Windmeijer SEs).
# For a two-step pgmm model vcovHC() returns the Windmeijer-corrected matrix.

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

# To export the same table for the paper, swap the output argument, e.g.:
#   output = "regression_results.docx"   (needs flextable)
#   output = "regression_results.tex"    (LaTeX, needs kableExtra)
