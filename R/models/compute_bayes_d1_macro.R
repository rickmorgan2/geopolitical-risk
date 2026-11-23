# compute_bayes_d1_macro.R
# OVERVIEW ####
# Bayesian Latent Variable Model -- Dimension 1: Macro-Fiscal Stability
#
# Estimates theta_macro: a country's latent buffer capacity to absorb shocks.
# Higher theta = stronger buffers = lower risk score.
#
# Indicators ####
#   ca     = current_account_pct_gdp  (WEO preferred, WDI fallback)
#   fiscal = fiscal_balance_pct_gdp   (WEO)
#   debt   = -govt_gross_debt_pct_gdp (WEO, negated so higher = more resilient)
#
# Model structure ####
#   Trivariate brms with fully crossed random effects:
#     (1 | country_code): long-run country-level baseline
#     (1 | year): global annual conditions
#   rescor = TRUE. No AR(1). Missingness handled via LOCF within country.
#
# Risk score ####
#   pnorm(-theta_mean, sd = sd(theta_mean)): higher buffer = lower risk.
#   CI bounds flip because pnorm(-.) is monotone decreasing.
#
# Outputs ####
#   gold.d1_macro_posterior   country-year risk scores + CI
#   gold.d1_indicator_ts      country-year-indicator fitted trajectories
#
# Expects: `con` (DBI connection to georisk.duckdb) from run_pipeline.R

# SETUP ####

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


# PULL AND PREPARE ####


message("--- D1: pulling macro indicators ---")

weo <- dbGetQuery(con, "
  SELECT iso3c AS country_code, year, indicator_label, value
  FROM bronze.imf_weo_annual
  WHERE indicator_label IN (
    'current_account_pct_gdp',
    'fiscal_balance_pct_gdp',
    'govt_gross_debt_pct_gdp'
  )
") |> pivot_wider(names_from = indicator_label, values_from = value)

wdi_ca <- dbGetQuery(con, "
  SELECT country_code, year, current_account_pct_gdp AS ca_wdi
  FROM silver.macro_derived
  WHERE current_account_pct_gdp IS NOT NULL
") |> select(country_code, year, ca_wdi)

d_raw <- weo |>
  left_join(wdi_ca, by = c("country_code", "year")) |>
  mutate(ca = coalesce(current_account_pct_gdp, ca_wdi)) |>
  transmute(
    country_code, year,
    ca,
    fiscal = fiscal_balance_pct_gdp,
    debt   = -govt_gross_debt_pct_gdp   # negate: higher debt = weaker buffer
  ) |>
  filter(year >= TS_START_YR)

country_names <- dbGetQuery(
  con, "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
) |> as_tibble()


# NA Fill (LOCF), STANDARDIZE ####

message("--- D1: preflight missingness check (pre-LOCF) ---")

inds <- c("ca", "fiscal", "debt")

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
  warning(sprintf("D1: high pre-LOCF missingness in %s",
          paste(high_miss$indicator, collapse = ", ")))
message("--- D1: patching missingness and standardizing ---")

d_model_full <- d_raw |>
  arrange(country_code, year) |>
  group_by(country_code) |>
  fill(ca, fiscal, debt, .direction = "down") |>
  ungroup() |>
  mutate(across(c(ca, fiscal, debt),
                ~ as.numeric(scale(.x)), .names = "{.col}_std"))

dropped_rows <- d_model_full |> filter(if_any(ends_with("_std"), is.na))

if (nrow(dropped_rows) > 0) {
  message("--- D1: audit of dropped country-years (unrecoverable) ---")
  print(dropped_rows |> select(country_code, year), n = 30)
}

d_model <- d_model_full |> drop_na(ends_with("_std"))

message(sprintf("  %d total country-years | %d used for fit | %d dropped",
                nrow(d_model_full), nrow(d_model), nrow(dropped_rows)))

# MODEL ####

message("--- D1: specifying brms model ---")

bf_ca     <- bf(ca_std     ~ 1 + (1 | country_code) + (1 | year))
bf_fiscal <- bf(fiscal_std ~ 1 + (1 | country_code) + (1 | year))
bf_debt   <- bf(debt_std   ~ 1 + (1 | country_code) + (1 | year))

priors_d1 <- c(
  set_prior("normal(0, 1)",       class = "Intercept", resp = "castd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "fiscalstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "debtstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "castd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "fiscalstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "debtstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "castd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "fiscalstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "debtstd"),
  set_prior("lkj(2)",             class = "rescor")
)

message(sprintf("--- D1: fitting | algo=%s | chains=%d | iter=%d ---",
                BAYES_ALGO, BAYES_CHAINS, BAYES_ITER))

fit_d1 <- brm(
  mvbf(bf_ca, bf_fiscal, bf_debt, rescor = TRUE),
  data       = d_model,
  prior      = priors_d1,
  chains     = BAYES_CHAINS,
  iter       = BAYES_ITER,
  warmup     = BAYES_WARMUP,
  cores      = BAYES_CORES,
  backend    = BAYES_BACKEND,
  algorithm  = BAYES_ALGO,
  seed       = BAYES_SEED,
  save_pars  = save_pars(all = TRUE),
  file       = file.path(MODEL_CACHE_DIR, "d1_macro_fit"),
  file_refit = "on_change"
)

## Diagnostics ####
rhat_vals  <- posterior::rhat(fit_d1)
n_rhat_bad <- sum(rhat_vals > 1.05, na.rm = TRUE)
message(sprintf("  Rhat > 1.05: %d (want 0)", n_rhat_bad))

n_divergent <- tryCatch(
  sum(bayesplot::nuts_params(fit_d1)$Value[
    bayesplot::nuts_params(fit_d1)$Parameter == "divergent__"]),
  error = function(e) NA_integer_
)
if (!is.na(n_divergent))
  message(sprintf("  Divergent transitions: %d (want 0)", n_divergent))

if (n_rhat_bad > 0 || (!is.na(n_divergent) && n_divergent > 0))
  warning("D1: sampling issues -- check pairs(fit_d1) and pp_check(fit_d1)")


## Extract Posteriors ####


message("--- D1: extracting posterior ---")

resps <- c(castd = "ca", fiscalstd = "fiscal", debtstd = "debt")

fitted_long <- map(names(resps), function(resp) {
  fitted(fit_d1, newdata = d_model_full, summary = TRUE, resp = resp,
         allow_new_levels = TRUE) |>
    as_tibble() |>
    bind_cols(select(d_model_full, country_code, year)) |>
    transmute(
      country_code, year,
      indicator = resps[[resp]],
      fit_mean  = Estimate,
      fit_q05   = Q2.5,
      fit_q95   = Q97.5,
      fit_width = Q97.5 - Q2.5
    )
}) |>
  list_rbind() |>
  left_join(country_names, by = "country_code") |>
  arrange(country_code, year, indicator)

# Higher buffer = lower risk: negate theta, bounds flip
d1_posterior <- fitted_long |>
  group_by(country_code, country_name, year) |>
  summarise(
    theta_mean  = mean(fit_mean, na.rm = TRUE),
    theta_q05   = mean(fit_q05,  na.rm = TRUE),
    theta_q95   = mean(fit_q95,  na.rm = TRUE),
    theta_width = theta_q95 - theta_q05,
    .groups     = "drop"
  ) |>
  mutate(
    .scale           = sd(theta_mean, na.rm = TRUE),
    risk_score       = pnorm(-theta_mean, mean = 0, sd = .scale),
    risk_score_q05   = pnorm(-theta_q95,  mean = 0, sd = .scale),  # bounds flip
    risk_score_q95   = pnorm(-theta_q05,  mean = 0, sd = .scale),
    risk_score_width = risk_score_q95 - risk_score_q05
  ) |>
  select(-.scale) |>
  arrange(country_code, year)

# WRITE  ####

write_gold(fitted_long,  "d1_indicator_ts",   con)
write_gold(d1_posterior, "d1_macro_posterior", con)

message("--- D1 complete ---")
message(sprintf("  %d country-years | risk score [%.3f, %.3f] | mean CI width %.3f",
                nrow(d1_posterior),
                min(d1_posterior$risk_score, na.rm = TRUE),
                max(d1_posterior$risk_score, na.rm = TRUE),
                mean(d1_posterior$risk_score_width, na.rm = TRUE)))
message(sprintf("  Wide risk CI (> 0.3): %d country-years",
                sum(d1_posterior$risk_score_width > 0.3, na.rm = TRUE)))
