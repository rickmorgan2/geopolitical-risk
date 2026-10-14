# compute_bayes_d3_annual.R
# ------------------------------------------------------------------------------
# Bayesian Latent Variable Model - Dimension 3: Export Fragility
# Annual grain: one risk score per country-year.
#
# -- Indicators ----------------------------------------------------------------
#   diversity_partner    = 1 - partner_hhi   (higher = more diverse = lower risk)
#   diversity_commodity  = 1 - commodity_hhi (higher = more diverse = lower risk)
#   export_independence  = 1 - value_share   (higher = less dependent = lower risk)
#
# -- Model structure -----------------------------------------------------------
#   Trivariate brms with fully crossed random effects:
#     (1 | country_code): long-run country-level baseline
#     (1 | year): global annual conditions
#   rescor = TRUE. No AR(1). Missingness handled via LOCF within country.
#
# -- Risk score ----------------------------------------------------------------
#   pnorm(-theta_mean, sd = sd(theta_mean)): higher resilience = lower risk.
#   CI bounds flip because pnorm(-.) is monotone decreasing.
#
# -- Source table --------------------------------------------------------------
#   silver.d3_export_fragility_annual
#
# -- Outputs -------------------------------------------------------------------
#   gold.d3_annual_posterior      country-year risk scores + CI
#   gold.d3_annual_indicator_ts   country-year-indicator fitted trajectories
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

message("--- D3 Annual: pulling from silver.d3_export_fragility_annual ---")

d_raw <- dbGetQuery(con, paste0("
  SELECT country_code, year,
         diversity_partner, diversity_commodity, export_independence
  FROM silver.d3_export_fragility_annual
  WHERE year >= ", TS_START_YR
)) |> as_tibble()

country_names <- dbGetQuery(
  con, "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
) |> as_tibble()

# ==============================================================================
# PATCH (LOCF), STANDARDIZE
# ==============================================================================

message("--- D3 Annual: preflight missingness check (pre-LOCF) ---")

inds <- c("diversity_partner", "diversity_commodity", "export_independence")

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
  warning(sprintf("D3 Annual: high pre-LOCF missingness in %s",
          paste(high_miss$indicator, collapse = ", ")))
message("--- D3 Annual: patching missingness and standardizing ---")

d_model_full <- d_raw |>
  arrange(country_code, year) |>
  group_by(country_code) |>
  fill(diversity_partner, diversity_commodity, export_independence,
       .direction = "down") |>
  ungroup() |>
  mutate(across(c(diversity_partner, diversity_commodity, export_independence),
                ~ as.numeric(scale(.x)), .names = "{.col}_std"))

dropped_rows <- d_model_full |> filter(if_any(ends_with("_std"), is.na))

if (nrow(dropped_rows) > 0) {
  message("--- D3 Annual: audit of dropped country-years (unrecoverable) ---")
  print(dropped_rows |> select(country_code, year), n = 30)
}

d_model <- d_model_full |> drop_na(ends_with("_std"))

message(sprintf("  %d total country-years | %d used for fit | %d dropped",
                nrow(d_model_full), nrow(d_model), nrow(dropped_rows)))

# ==============================================================================
# MODEL
# ==============================================================================

message("--- D3 Annual: specifying brms model ---")

bf_partner     <- bf(diversity_partner_std    ~ 1 + (1 | country_code) + (1 | year))
bf_commodity   <- bf(diversity_commodity_std  ~ 1 + (1 | country_code) + (1 | year))
bf_independent <- bf(export_independence_std  ~ 1 + (1 | country_code) + (1 | year))

priors_d3a <- c(
  set_prior("normal(0, 1)",       class = "Intercept", resp = "diversitypartnerstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "diversitycommoditystd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "exportindependencestd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "diversitypartnerstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "diversitycommoditystd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "exportindependencestd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "diversitypartnerstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "diversitycommoditystd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "exportindependencestd"),
  set_prior("lkj(2)",             class = "rescor")
)

message(sprintf("--- D3 Annual: fitting | algo=%s | chains=%d | iter=%d ---",
                BAYES_ALGO, BAYES_CHAINS, BAYES_ITER))

fit_d3a <- brm(
  mvbf(bf_partner, bf_commodity, bf_independent, rescor = TRUE),
  data       = d_model,
  prior      = priors_d3a,
  chains     = BAYES_CHAINS,
  iter       = BAYES_ITER,
  warmup     = BAYES_WARMUP,
  cores      = BAYES_CORES,
  backend    = BAYES_BACKEND,
  algorithm  = BAYES_ALGO,
  seed       = BAYES_SEED,
  save_pars  = save_pars(all = TRUE),
  file       = file.path(MODEL_CACHE_DIR, "d3_annual_fit"),
  file_refit = "on_change"
)

# -- Diagnostics ---------------------------------------------------------------
rhat_vals  <- posterior::rhat(fit_d3a)
n_rhat_bad <- sum(rhat_vals > 1.05, na.rm = TRUE)
message(sprintf("  Rhat > 1.05: %d (want 0)", n_rhat_bad))

n_divergent <- tryCatch(
  sum(bayesplot::nuts_params(fit_d3a)$Value[
    bayesplot::nuts_params(fit_d3a)$Parameter == "divergent__"]),
  error = function(e) NA_integer_
)
if (!is.na(n_divergent))
  message(sprintf("  Divergent transitions: %d (want 0)", n_divergent))

if (n_rhat_bad > 0 || (!is.na(n_divergent) && n_divergent > 0))
  warning("D3 Annual: sampling issues -- check pairs(fit_d3a)")

# ==============================================================================
# EXTRACT POSTERIOR
# ==============================================================================

message("--- D3 Annual: extracting posterior ---")

resps <- c(
  diversitypartnerstd   = "partner",
  diversitycommoditystd = "commodity",
  exportindependencestd = "independence"
)

fitted_long <- map(names(resps), function(resp) {
  fitted(fit_d3a, newdata = d_model_full, summary = TRUE, resp = resp,
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

# Higher resilience = lower risk: negate theta before pnorm, bounds flip
d3_annual_posterior <- fitted_long |>
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

# ==============================================================================
# WRITE
# ==============================================================================

write_gold(fitted_long,         "d3_annual_indicator_ts", con)
write_gold(d3_annual_posterior, "d3_annual_posterior",    con)

message("--- D3 Annual complete ---")
message(sprintf("  %d country-years | risk score [%.3f, %.3f] | mean CI width %.3f",
                nrow(d3_annual_posterior),
                min(d3_annual_posterior$risk_score, na.rm = TRUE),
                max(d3_annual_posterior$risk_score, na.rm = TRUE),
                mean(d3_annual_posterior$risk_score_width, na.rm = TRUE)))
message(sprintf("  Wide risk CI (> 0.3): %d country-years",
                sum(d3_annual_posterior$risk_score_width > 0.3, na.rm = TRUE)))
