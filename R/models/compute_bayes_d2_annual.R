# compute_bayes_d2_annual.R
# ------------------------------------------------------------------------------
# Bayesian Latent Variable Model - Dimension 2: Strategic Import Reliance
# Annual grain: one risk score per country-year.
#
# -- Indicators ----------------------------------------------------------------
#   diversity_food    = 1 - supplier_hhi_food    (higher = more diverse = lower risk)
#   diversity_energy  = 1 - supplier_hhi_energy  (higher = more diverse = lower risk)
#   diversity_cereal  = 1 - cereal_import_hhi    (higher = more diverse = lower risk)
#
# -- Model structure -----------------------------------------------------------
#   Trivariate brms with fully crossed random effects:
#     (1 | country_code): long-run country-level baseline
#     (1 | year): global annual conditions
#   rescor = TRUE. No AR(1). Missingness handled via LOCF within country.
#
# -- Risk score ----------------------------------------------------------------
#   pnorm(-theta_mean, sd = sd(theta_mean)): higher diversity = lower risk.
#   CI bounds flip because pnorm(-.) is monotone decreasing.
#
# -- Source table --------------------------------------------------------------
#   silver.d2_import_hhi_annual
#
# -- Outputs -------------------------------------------------------------------
#   gold.d2_annual_posterior      country-year risk scores + CI
#   gold.d2_annual_indicator_ts   country-year-indicator fitted trajectories
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

message("--- D2 Annual: pulling from silver.d2_import_hhi_annual ---")

d_raw <- dbGetQuery(con, paste0("
  SELECT country_code, year,
         diversity_food, diversity_energy, diversity_cereal, diversity_fertilizer
  FROM silver.d2_import_hhi_annual
  WHERE year >= ", TS_START_YR
)) |> as_tibble()

country_names <- dbGetQuery(
  con, "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
) |> as_tibble()

# ==============================================================================
# PATCH (LOCF), STANDARDIZE
# ==============================================================================

message("--- D2 Annual: preflight missingness check (pre-LOCF) ---")

inds <- c("diversity_food", "diversity_energy", "diversity_cereal", "diversity_fertilizer")

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
  warning(sprintf("D2 Annual: high pre-LOCF missingness in %s",
          paste(high_miss$indicator, collapse = ", ")))
message("--- D2 Annual: patching missingness and standardizing ---")

d_model_full <- d_raw |>
  arrange(country_code, year) |>
  group_by(country_code) |>
  fill(diversity_food, diversity_energy, diversity_cereal, diversity_fertilizer,
       .direction = "down") |>
  ungroup() |>
  mutate(across(c(diversity_food, diversity_energy, diversity_cereal, diversity_fertilizer),
                ~ as.numeric(scale(.x)), .names = "{.col}_std"))

dropped_rows <- d_model_full |> filter(if_any(ends_with("_std"), is.na))

if (nrow(dropped_rows) > 0) {
  message("--- D2 Annual: audit of dropped country-years (unrecoverable) ---")
  print(dropped_rows |> select(country_code, year), n = 30)
}

d_model <- d_model_full |> drop_na(ends_with("_std"))

message(sprintf("  %d total country-years | %d used for fit | %d dropped",
                nrow(d_model_full), nrow(d_model), nrow(dropped_rows)))

# ==============================================================================
# MODEL
# ==============================================================================

message("--- D2 Annual: specifying brms model ---")

bf_food       <- bf(diversity_food_std       ~ 1 + (1 | country_code) + (1 | year))
bf_energy     <- bf(diversity_energy_std     ~ 1 + (1 | country_code) + (1 | year))
bf_cereal     <- bf(diversity_cereal_std     ~ 1 + (1 | country_code) + (1 | year))
bf_fertilizer <- bf(diversity_fertilizer_std ~ 1 + (1 | country_code) + (1 | year))

priors_d2a <- c(
  set_prior("normal(0, 1)",       class = "Intercept", resp = "diversityfoodstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "diversityenergystd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "diversitycerealstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "diversityfoodstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "diversityenergystd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "diversitycerealstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "diversityfoodstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "diversityenergystd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "diversitycerealstd"),
  set_prior("normal(0, 1)",       class = "Intercept", resp = "diversityfertilizerstd"),
  set_prior("normal(0, 0.5)",     class = "sigma",     resp = "diversityfertilizerstd"),
  set_prior("student_t(3, 0, 1)", class = "sd",        resp = "diversityfertilizerstd"),
  set_prior("lkj(2)",             class = "rescor")
)

message(sprintf("--- D2 Annual: fitting | algo=%s | chains=%d | iter=%d ---",
                BAYES_ALGO, BAYES_CHAINS, BAYES_ITER))

fit_d2a <- brm(
  mvbf(bf_food, bf_energy, bf_cereal, bf_fertilizer, rescor = TRUE),
  data       = d_model,
  prior      = priors_d2a,
  chains     = BAYES_CHAINS,
  iter       = BAYES_ITER,
  warmup     = BAYES_WARMUP,
  cores      = BAYES_CORES,
  backend    = BAYES_BACKEND,
  algorithm  = BAYES_ALGO,
  seed       = BAYES_SEED,
  save_pars  = save_pars(all = TRUE),
  file       = file.path(MODEL_CACHE_DIR, "d2_annual_fit_v2"),
  file_refit = "on_change"
)

# -- Diagnostics ---------------------------------------------------------------
rhat_vals  <- posterior::rhat(fit_d2a)
n_rhat_bad <- sum(rhat_vals > 1.05, na.rm = TRUE)
message(sprintf("  Rhat > 1.05: %d (want 0)", n_rhat_bad))

n_divergent <- tryCatch(
  sum(bayesplot::nuts_params(fit_d2a)$Value[
    bayesplot::nuts_params(fit_d2a)$Parameter == "divergent__"]),
  error = function(e) NA_integer_
)
if (!is.na(n_divergent))
  message(sprintf("  Divergent transitions: %d (want 0)", n_divergent))

if (n_rhat_bad > 0 || (!is.na(n_divergent) && n_divergent > 0))
  warning("D2 Annual: sampling issues -- check pairs(fit_d2a)")

# ==============================================================================
# EXTRACT POSTERIOR
# ==============================================================================

message("--- D2 Annual: extracting posterior ---")

resps <- c(
  diversityfoodstd        = "food",
  diversityenergystd      = "energy",
  diversitycerealstd      = "cereal",
  diversityfertilizerstd  = "fertilizer"
)

fitted_long <- map(names(resps), function(resp) {
  fitted(fit_d2a, newdata = d_model_full, summary = TRUE, resp = resp,
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

# Higher diversity = lower risk: negate theta before pnorm, bounds flip
d2_annual_posterior <- fitted_long |>
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

write_gold(fitted_long,        "d2_annual_indicator_ts", con)
write_gold(d2_annual_posterior, "d2_annual_posterior",    con)

message("--- D2 Annual complete ---")
message(sprintf("  %d country-years | risk score [%.3f, %.3f] | mean CI width %.3f",
                nrow(d2_annual_posterior),
                min(d2_annual_posterior$risk_score, na.rm = TRUE),
                max(d2_annual_posterior$risk_score, na.rm = TRUE),
                mean(d2_annual_posterior$risk_score_width, na.rm = TRUE)))
