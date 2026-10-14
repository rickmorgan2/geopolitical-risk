# compute_bayes_d2_monthly.R
# ------------------------------------------------------------------------------
# Bayesian Latent Variable Model -- Strategic Import Reliance
# Monthly level: one risk score per country-year-month.
#
# -- Indicators ----------------------------------------------------------------
#   food_vol_risk          = -food_vol_anomaly_z        (food import shortfall = risk)
#   food_price_risk        =  food_price_z              (food price spike = risk)
#   food_basket_vol        =  food_cv_basket_wtd        (food price volatility = risk)
#   energy_basket_vol      =  energy_cv_basket_wtd      (energy price volatility = risk)
#   food_input_basket_vol  =  fertilizer_cv_basket_wtd  (fertilizer price volatility = risk)
#
# All oriented: higher = more risk before standardization.
#
# -- Model structure -----------------------------------------------------------
#   Hexavariate brms with fully crossed random effects:
#     (1 | country_code): long-run country-level import baseline
#     (1 | year): global annual conditions
#     (1 | month): seasonal patterns
#   rescor = TRUE. No AR(1). Missingness handled via LOCF within country.
#
# -- Source table --------------------------------------------------------------
#   silver.monthly_disruption_d2  (from compute_silver_comtrade.R)
#
# -- Outputs -------------------------------------------------------------------
#   gold.d2_monthly_posterior       country-year-month risk scores + CI
#   gold.d2_monthly_indicator_ts    country-year-month-indicator fitted values
#
# Expects: `con` (DBI connection), Bayes config vars from config.R
# ------------------------------------------------------------------------------

library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(brms)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

BAYES_CHAINS  <- if (exists("BAYES_CHAINS"))  BAYES_CHAINS  else 4L
BAYES_ITER    <- if (exists("BAYES_ITER"))    BAYES_ITER    else 2000L
BAYES_WARMUP  <- if (exists("BAYES_WARMUP"))  BAYES_WARMUP  else 1000L
BAYES_CORES   <- if (exists("BAYES_CORES"))   BAYES_CORES   else max(1L, parallel::detectCores() - 1L)
BAYES_BACKEND <- if (exists("BAYES_BACKEND")) BAYES_BACKEND else "cmdstanr"
BAYES_ALGO    <- if (exists("BAYES_ALGO"))    BAYES_ALGO    else "sampling"
BAYES_SEED    <- if (exists("BAYES_SEED"))    BAYES_SEED    else 42L

if (!exists("MODEL_CACHE_DIR")) {
  MODEL_CACHE_DIR <- "R/models/cache"
  dir.create(MODEL_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
}

write_gold <- function(df, table_name, con) {
  df <- mutate(df, score_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS gold")
  dbWriteTable(con, DBI::Id(schema = "gold", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [gold.%s] %d rows written", table_name, nrow(df)))
}

# ==============================================================================
# PULL AND PREPARE
# ==============================================================================

message("--- D2 Monthly: pulling from silver.monthly_disruption_d2 ---")

d_raw <- dbGetQuery(con, paste0("
  SELECT country_code, year, month,
         food_vol_anomaly_z,
         food_price_z,
         food_cv_basket_wtd,
         energy_cv_basket_wtd,
         fertilizer_cv_basket_wtd
  FROM silver.monthly_disruption_d2
  WHERE year >= ", M_TS_START_YR
)) |> as_tibble()

country_names <- dbGetQuery(
  con, "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
) |> as_tibble()

# ==============================================================================
# PREFLIGHT MISSINGNESS CHECK
# ==============================================================================

message("--- D2 Monthly: preflight missingness check (pre-LOCF) ---")

inds <- c("food_vol_anomaly_z", "food_price_z",
          "food_cv_basket_wtd", "energy_cv_basket_wtd", "fertilizer_cv_basket_wtd")

miss <- tibble(indicator = inds) |>
  mutate(
    n_missing   = map_int(indicator, ~ sum(is.na(d_raw[[.x]]))),
    pct_missing = round(n_missing / nrow(d_raw) * 100, 1)
  )

message(sprintf("  Total rows: %d", nrow(d_raw)))
walk2(miss$indicator, miss$pct_missing,
      ~ message(sprintf("  %-30s missing: %.1f%%", .x, .y)))

high_miss <- filter(miss, pct_missing > 40)
if (nrow(high_miss) > 0)
  warning(sprintf("D2 Monthly: high pre-LOCF missingness in %s",
          paste(high_miss$indicator, collapse = ", ")))

# ==============================================================================
# ORIENT, PATCH (LOCF), STANDARDIZE
# ==============================================================================

message("--- D2 Monthly: patching missingness and standardizing ---")

d_model_full <- d_raw |>
  arrange(country_code, year, month) |>
  group_by(country_code) |>
  fill(food_vol_anomaly_z, food_price_z,
       food_cv_basket_wtd, energy_cv_basket_wtd, fertilizer_cv_basket_wtd,
       .direction = "down") |>
  ungroup() |>
  mutate(
    food_vol_risk         = -food_vol_anomaly_z,        # shortfall = risk
    food_price_risk       =  food_price_z,              # price spike = risk
    food_basket_vol       =  food_cv_basket_wtd,        # volatility = risk
    energy_basket_vol     =  energy_cv_basket_wtd,      # volatility = risk
    food_input_basket_vol =  fertilizer_cv_basket_wtd   # volatility = risk
  ) |>
  mutate(
    across(c(food_vol_risk, food_price_risk,
             food_basket_vol, energy_basket_vol, food_input_basket_vol),
           ~ as.numeric(scale(.x)), .names = "{.col}_std")
  )

dropped_rows <- d_model_full |> filter(if_any(ends_with("_std"), is.na))

if (nrow(dropped_rows) > 0) {
  message("--- D2 Monthly: audit of dropped country-year-months (unrecoverable) ---")
  dropped_summary <- dropped_rows |>
    group_by(country_code, year) |>
    summarise(months_dropped = n(), .groups = "drop")
  print(dropped_summary, n = 20)
}

d_model <- d_model_full |> drop_na(ends_with("_std"))

message(sprintf("  %d total rows | %d used for fit | %d rows dropped",
                nrow(d_model_full), nrow(d_model), nrow(dropped_rows)))

# ==============================================================================
# MODEL
# ==============================================================================

message("--- D2 Monthly: specifying brms model ---")

bf_food_vol       <- bf(food_vol_risk_std         ~ 1 + (1 | country_code) + (1 | year) + (1 | month))
bf_food_price     <- bf(food_price_risk_std        ~ 1 + (1 | country_code) + (1 | year) + (1 | month))
bf_food_vol_bask  <- bf(food_basket_vol_std        ~ 1 + (1 | country_code) + (1 | year) + (1 | month))
bf_energy_bask    <- bf(energy_basket_vol_std      ~ 1 + (1 | country_code) + (1 | year) + (1 | month))
bf_fert_vol_bask  <- bf(food_input_basket_vol_std  ~ 1 + (1 | country_code) + (1 | year) + (1 | month))

priors_d2m <- c(
  set_prior("normal(0, 1)",       class = "Intercept", resp = "foodvolriskstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "foodpriceriskstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "foodbasketvolstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "energybasketvolstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "foodinputbasketvolstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "foodvolriskstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "foodpriceriskstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "foodbasketvolstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "energybasketvolstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "foodinputbasketvolstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "foodvolriskstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "foodpriceriskstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "foodbasketvolstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "energybasketvolstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "foodinputbasketvolstd"),
  set_prior("lkj(2)",             class = "rescor")
)

message(sprintf("--- D2 Monthly: fitting | algo=%s | chains=%d | iter=%d ---",
                BAYES_ALGO, BAYES_CHAINS, BAYES_ITER))

fit_d2m <- brm(
  mvbf(bf_food_vol, bf_food_price,
       bf_food_vol_bask, bf_energy_bask, bf_fert_vol_bask,
       rescor = TRUE),
  data       = d_model,
  prior      = priors_d2m,
  chains     = BAYES_CHAINS,
  iter       = BAYES_ITER,
  warmup     = BAYES_WARMUP,
  cores      = BAYES_CORES,
  backend    = BAYES_BACKEND,
  algorithm  = BAYES_ALGO,
  seed       = BAYES_SEED,
  save_pars  = save_pars(all = TRUE),
  file       = file.path(MODEL_CACHE_DIR, "d2_monthly_fit_v2"),
  file_refit = "on_change"
)

# -- Diagnostics ---------------------------------------------------------------
rhat_vals  <- posterior::rhat(fit_d2m)
n_rhat_bad <- sum(rhat_vals > 1.05, na.rm = TRUE)
message(sprintf("  Rhat > 1.05: %d (want 0)", n_rhat_bad))

n_divergent <- tryCatch(
  sum(bayesplot::nuts_params(fit_d2m)$Value[
    bayesplot::nuts_params(fit_d2m)$Parameter == "divergent__"]),
  error = function(e) NA_integer_
)
if (!is.na(n_divergent))
  message(sprintf("  Divergent transitions: %d (want 0)", n_divergent))

if (n_rhat_bad > 0 || (!is.na(n_divergent) && n_divergent > 0))
  warning("D2 Monthly: sampling issues -- check pairs(fit_d2m) and pp_check(fit_d2m)")

# ==============================================================================
# EXTRACT POSTERIOR
# ==============================================================================

message("--- D2 Monthly: extracting posterior ---")

resps <- c(
  foodvolriskstd         = "food_vol",
  foodpriceriskstd       = "food_price",
  foodbasketvolstd       = "food_basket_vol",
  energybasketvolstd     = "energy_basket_vol",
  foodinputbasketvolstd  = "food_input_basket_vol"
)

fitted_long <- map(names(resps), function(resp) {
  fitted(fit_d2m, newdata = d_model_full, summary = TRUE, resp = resp,
         allow_new_levels = TRUE) |>
    as_tibble() |>
    bind_cols(select(d_model_full, country_code, year, month)) |>
    transmute(
      country_code, year, month,
      indicator = resps[[resp]],
      fit_mean  = Estimate,
      fit_q05   = Q2.5,
      fit_q95   = Q97.5
    )
}) |>
  list_rbind() |>
  left_join(country_names, by = "country_code")

d2_monthly_posterior <- fitted_long |>
  group_by(country_code, year, month) |>
  summarise(
    theta_mean = mean(fit_mean, na.rm = TRUE),
    theta_q05  = mean(fit_q05,  na.rm = TRUE),
    theta_q95  = mean(fit_q95,  na.rm = TRUE),
    .groups    = "drop"
  ) |>
  mutate(
    .scale           = sd(theta_mean, na.rm = TRUE),
    risk_score       = pnorm(theta_mean, mean = 0, sd = .scale),
    risk_score_q05   = pnorm(theta_q05,  mean = 0, sd = .scale),
    risk_score_q95   = pnorm(theta_q95,  mean = 0, sd = .scale),
    risk_score_width = risk_score_q95 - risk_score_q05
  ) |>
  select(-.scale) |>
  left_join(country_names, by = "country_code")

# ==============================================================================
# WRITE
# ==============================================================================

write_gold(fitted_long,          "d2_monthly_indicator_ts", con)
write_gold(d2_monthly_posterior, "d2_monthly_posterior",    con)

message("--- D2 Monthly complete ---")
message(sprintf("  %d country-year-months | risk score [%.3f, %.3f] | mean CI width %.3f",
                nrow(d2_monthly_posterior),
                min(d2_monthly_posterior$risk_score, na.rm = TRUE),
                max(d2_monthly_posterior$risk_score, na.rm = TRUE),
                mean(d2_monthly_posterior$risk_score_width, na.rm = TRUE)))
