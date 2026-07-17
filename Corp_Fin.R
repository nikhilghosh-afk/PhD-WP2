# Installing Libraries

library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(ggokabeito)
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
