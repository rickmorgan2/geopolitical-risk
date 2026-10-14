# compute_bayes_d3_monthly.R
# Bayesian Latent Variable Model - Dimension 3: Export Fragility
# Monthly grain: one risk score per crossed: country, year, month.
#
# -- Indicators ----------------------------------------------------------------
#   export_vol_risk       = -export_vol_anomaly_z   (export shortfall = risk)
#   export_price_risk     = -export_basket_price_z  (price collapse = risk)
#   basket_price_vol_risk = cv_basket_wtd          (price volatility = risk)
#
# cv_basket_wtd is the Morgan-Reinhardt (2016) weighted basket price volatility:
#   SD of monthly log price first differences over a 12-month window (lagged 1 month),
#   weighted by lagged export value shares.
#
# Direction notes:
#   - export_vol_anomaly_z:   negative z = export collapse = risk -> negate
#   - export_basket_price_z:  price collapse hurts exporters -> negate
#   - cv_basket_wtd:          higher volatility = more risk (positive direction)
#
# -- Model structure -----------------------------------------------------------
#   Trivariate brms with varying intercepts per country-month combination.
#   Captures seasonal risk baseline while allowing monthly indicator movement.
#   Residual correlation (rescor) enabled. Missingness handled via LOCF patch.
#   Scoring: rescor precision-matrix weights applied per posterior draw;
#   row-wise quantiles yield genuine draw-based CIs on the composite score.
#
# -- Source table --------------------------------------------------------------
#   silver.monthly_disruption_d3  (from compute_silver_comtrade.R)
#
# -- Outputs -------------------------------------------------------------------
#   gold.d3_monthly_posterior       crossed: country, year, month risk scores + CI
#   gold.d3_monthly_indicator_ts    crossed: country, year, month-indicator fitted values
#
# Expects: `con` (DBI connection), Bayes config vars from config.R
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

message("--- D3 Monthly: pulling from silver.monthly_disruption_d3 ---")

d_raw <- dbGetQuery(con, paste0("
  SELECT country_code, year, month,
         export_vol_anomaly_z,
         export_basket_price_z,
         cv_basket_wtd
  FROM silver.monthly_disruption_d3
  WHERE year >= ", M_TS_START_YR
)) |> as_tibble()

country_names <- dbGetQuery(
  con, "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
) |> as_tibble()

# ==============================================================================
# ORIENT, PATCH (LOCF), AND STANDARDIZE
# ==============================================================================

message("--- D3 Monthly: preflight missingness check (pre-LOCF) ---")

inds <- c("export_vol_anomaly_z", "export_basket_price_z", "cv_basket_wtd")

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
  warning(sprintf("D3 Monthly: high pre-LOCF missingness in %s",
          paste(high_miss$indicator, collapse = ", ")))
message("--- D3 Monthly: patching missingness and standardizing ---")

d_model_full <- d_raw |>
  arrange(country_code, year, month) |>
  group_by(country_code) |>
  fill(export_vol_anomaly_z, export_basket_price_z,
       cv_basket_wtd, .direction = "down") |>
  ungroup() |>
  mutate(
    export_vol_risk       = -export_vol_anomaly_z,       # export shortfall = risk
    export_price_risk     = -export_basket_price_z,      # price collapse = risk
    basket_price_vol_risk =  cv_basket_wtd                # price volatility = risk
  ) |>
  mutate(
    across(c(export_vol_risk, export_price_risk, basket_price_vol_risk),
           ~ as.numeric(scale(.x)), .names = "{.col}_std")
  )

# Audit dropped rows (unrecoverable missingness)
dropped_rows <- d_model_full |>
  filter(if_any(ends_with("_std"), is.na))

if (nrow(dropped_rows) > 0) {
  message("--- D3 Monthly: audit of dropped crossed: country, year, months (unrecoverable) ---")
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

message("--- D3 Monthly: specifying brms model ---")

# (1 | country_code:month) captures latent annual export fragility baseline
bf_vol        <- bf(export_vol_risk_std       ~ 1 + (1 | country_code) + (1 | year) + (1 | month))
bf_price      <- bf(export_price_risk_std     ~ 1 + (1 | country_code) + (1 | year) + (1 | month))
bf_basket_vol <- bf(basket_price_vol_risk_std ~ 1 + (1 | country_code) + (1 | year) + (1 | month))

priors_d3m <- c(
  set_prior("normal(0, 1)",       class = "Intercept", resp = "exportvolriskstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "exportpriceriskstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "basketpricevolriskstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "exportvolriskstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "exportpriceriskstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "basketpricevolriskstd"),
  # basket_price_vol is more persistent (24m window); same prior family for simplicity
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "exportvolriskstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "exportpriceriskstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "basketpricevolriskstd"),
  set_prior("lkj(2)",             class = "rescor")
)

message(sprintf("--- D3 Monthly: fitting | algo=%s | chains=%d | iter=%d ---",
                BAYES_ALGO, BAYES_CHAINS, BAYES_ITER))

fit_d3m <- brm(
  mvbf(bf_vol, bf_price, bf_basket_vol, rescor = TRUE),
  data       = d_model,
  prior      = priors_d3m,
  chains     = BAYES_CHAINS,
  iter       = BAYES_ITER,
  warmup     = BAYES_WARMUP,
  cores      = BAYES_CORES,
  backend    = BAYES_BACKEND,
  algorithm  = BAYES_ALGO,
  seed       = BAYES_SEED,
  save_pars  = save_pars(all = TRUE),
  file       = file.path(MODEL_CACHE_DIR, "d3_monthly_fit"),
  file_refit = "on_change"
)

# -- Diagnostics ---------------------------------------------------------------
rhat_vals  <- posterior::rhat(fit_d3m)
n_rhat_bad <- sum(rhat_vals > 1.05, na.rm = TRUE)
message(sprintf("  Rhat > 1.05: %d (want 0)", n_rhat_bad))

# ==============================================================================
# EXTRACT POSTERIOR
# ==============================================================================

# -- Monthly scores ------------------------------------------------------------

message("--- D3 Monthly: extracting posterior ---")

resps <- c(
  exportvolriskstd      = "vol_risk",
  exportpriceriskstd    = "price_risk",
  basketpricevolriskstd = "basket_price_vol"
)

fitted_long <- map(names(resps), function(resp) {
  fitted(fit_d3m, newdata = d_model_full, summary = TRUE, resp = resp, allow_new_levels = TRUE) |>
    as_tibble() |>
    bind_cols(select(d_model_full, country_code, year, month)) |>
    transmute(
      country_code, year, month,
      indicator = resps[[resp]],
      fit_mean  = Estimate,
      fit_q05   = Q2.5,
      fit_q95   = Q97.5
    )
}) |> list_rbind()

d3_monthly_posterior <- fitted_long |>
  group_by(country_code, year, month) |>
  summarise(
    theta_mean  = mean(fit_mean, na.rm = TRUE),
    theta_q05   = mean(fit_q05, na.rm = TRUE),
    theta_q95   = mean(fit_q95, na.rm = TRUE),
    .groups     = "drop"
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

write_gold(fitted_long,          "d3_monthly_indicator_ts", con)
write_gold(d3_monthly_posterior, "d3_monthly_posterior",    con)

message("--- D3 Monthly complete ---")
