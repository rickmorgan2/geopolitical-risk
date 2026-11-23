# compute_tiers.R
# ------------------------------------------------------------------------------
# Assigns each country-year a risk tier based on D1-D4 posterior scores.
#
# Tier hierarchy (high to low risk):
#   Critical  -- extreme vulnerability; at least one dimension at crisis level
#   Elevated  -- broad-based or multi-dimensional risk
#   Guarded   -- moderate risk with at least one notable dimension
#   Moderate  -- below-average risk, no dominant concerns
#   Low       -- consistently low risk across all dimensions
#
# Classification logic:
#   1. Single-dimension trigger: if ANY dimension >= TRIGGER_SINGLE, assign
#      Critical immediately regardless of others (floor override).
#   2. Pattern rules (applied in order): mean score and count of dimensions
#      above tier-specific thresholds determine tier for all other cases.
#   3. Context flag boost: active alert flags (reserves, external debt, debt service)
#      can bump a country up one tier.
#
# Output: gold.risk_tiers
#   country_code, year, tier, tier_num (5=Critical..1=Low),
#   trigger_type (single_dim / pattern / flag_boost),
#   trigger_dim  (dimension that fired single-dim rule, else NA),
#   n_dims_elevated, mean_score, d1-d4 scores, active_flags
# ------------------------------------------------------------------------------

if (!exists("con")) stop("No DB connection. Source via run_pipeline.R")

library(dplyr)
library(tidyr)
library(DBI)
library(glue)

# ==============================================================================
# CONFIG
# ==============================================================================

# Inherit from config.R; guards catch standalone sourcing
TIER_YEAR      <- if (exists("TIER_YEAR"))      TIER_YEAR      else NULL     # NULL = all years

# Scores from pnorm() are approximately Uniform(0,1), so every threshold is
# effectively a percentile cutoff.
#
# Target distribution (approx):
#   Critical  ~10%  -- genuine outliers on multiple dimensions
#   Elevated  ~20%  -- broad elevated risk
#   Guarded   ~30%  -- above average; worth watching
#   Moderate  ~25%  -- below average
#   Low       ~15%  -- consistently low across all dimensions
#
# TRIGGER_SINGLE: top 2.5% on any single dimension fires Critical alone.
# Pattern rules use AND logic (both mean_ge AND n_ge must be met) to
# prevent a single barely-met criterion inflating upper tiers.
TRIGGER_SINGLE <- if (exists("TRIGGER_SINGLE")) TRIGGER_SINGLE else 0.975

# Pattern thresholds: AND logic -- both conditions must hold.
TIER_RULES <- list(
  Critical = list(mean_ge = 0.75, n_ge = 3L, n_threshold = 0.70),
  Elevated = list(mean_ge = 0.60, n_ge = 2L, n_threshold = 0.65),
  Guarded  = list(mean_ge = 0.45, n_ge = 2L, n_threshold = 0.50),
  Moderate = list(mean_ge = 0.30, n_ge = 1L, n_threshold = 0.40)
  # Low is the catch-all
)

# Context flag boost: TRUE = active flags can raise tier by one level
FLAG_BOOST <- if (exists("FLAG_BOOST")) FLAG_BOOST else TRUE

# Minimum number of scored dimensions required to assign a tier;
# countries below this threshold get tier = NA with a note
MIN_DIMS <- 2L

# ==============================================================================
# HELPERS
# ==============================================================================

write_gold <- function(df, table_name, con) {
  df <- mutate(df, score_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS gold")
  dbWriteTable(con, DBI::Id(schema = "gold", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  Written: gold.%s (%d rows)", table_name, nrow(df)))
}

# Apply tier rules to a single row; returns tier label
assign_tier <- function(d1, d2, d3, d4, n_flags) {
  scores <- c(d1 = d1, d2 = d2, d3 = d3, d4 = d4)
  n_scored <- sum(!is.na(scores))
  
  if (n_scored < MIN_DIMS) return(list(
    tier = NA_character_, tier_num = NA_integer_,
    trigger_type = "insufficient_data", trigger_dim = NA_character_
  ))
  
  mean_sc <- mean(scores, na.rm = TRUE)
  
  # -- 1. Single-dimension trigger -------------------------------------------
  trigger_dim <- names(which(scores >= TRIGGER_SINGLE))
  if (length(trigger_dim) > 0) return(list(
    tier         = "Critical",
    tier_num     = 5L,
    trigger_type = "single_dim",
    trigger_dim  = paste(trigger_dim, collapse = ",")
  ))
  
  # -- 2. Pattern rules -------------------------------------------------------
  tier_labels <- c("Critical", "Elevated", "Guarded", "Moderate")
  tier_nums   <- c(5L, 4L, 3L, 2L)
  
  for (i in seq_along(tier_labels)) {
    rule  <- TIER_RULES[[tier_labels[i]]]
    n_elev <- sum(scores >= rule$n_threshold, na.rm = TRUE)
    # AND logic: both mean and count must be met
    if (mean_sc >= rule$mean_ge && n_elev >= rule$n_ge) {
      tier     <- tier_labels[i]
      tier_num <- tier_nums[i]
      
      # -- 3. Context flag boost (cap at Critical) ----------------------------
      if (FLAG_BOOST && n_flags > 0 && tier_num < 5L) {
        tier_num <- tier_num + 1L
        tier     <- c("Low","Moderate","Guarded","Elevated","Critical")[tier_num]
        return(list(tier=tier, tier_num=tier_num,
                    trigger_type="flag_boost", trigger_dim=NA_character_))
      }
      return(list(tier=tier, tier_num=tier_num,
                  trigger_type="pattern", trigger_dim=NA_character_))
    }
  }
  
  # -- Low (catch-all) --------------------------------------------------------
  tier     <- "Low"
  tier_num <- 1L
  if (FLAG_BOOST && n_flags > 0) {
    tier_num <- 2L; tier <- "Moderate"
    return(list(tier=tier, tier_num=tier_num,
                trigger_type="flag_boost", trigger_dim=NA_character_))
  }
  list(tier=tier, tier_num=tier_num,
       trigger_type="pattern", trigger_dim=NA_character_)
}

# ==============================================================================
# PULL DATA
# ==============================================================================

message("--- Tiers: pulling dimensional posteriors ---")

year_filter <- if (!is.null(TIER_YEAR)) glue("AND year = {TIER_YEAR}") else ""

d1 <- dbGetQuery(con, glue("
  SELECT country_code, year,
    risk_score AS d1_score
  FROM gold.d1_macro_posterior
  WHERE risk_score IS NOT NULL {year_filter}
")) |> as_tibble()

d2 <- dbGetQuery(con, glue("
  SELECT country_code, year,
    risk_score AS d2_score
  FROM gold.d2_annual_posterior
  WHERE risk_score IS NOT NULL {year_filter}
")) |> as_tibble()

d3 <- dbGetQuery(con, glue("
  SELECT country_code, year,
    risk_score AS d3_score
  FROM gold.d3_annual_posterior
  WHERE risk_score IS NOT NULL {year_filter}
")) |> as_tibble()

d4 <- dbGetQuery(con, glue("
  SELECT country_code, year,
    d4_score
  FROM gold.d4_annual
  WHERE d4_score IS NOT NULL {year_filter}
")) |> as_tibble()

# Context flags from context layer
ctx <- dbGetQuery(con, "
  SELECT country_code, year,
    COALESCE(reserves_low,      FALSE) AS reserves_low,
    COALESCE(ext_debt_high,     FALSE) AS ext_debt_high,
    COALESCE(debt_service_high, FALSE) AS debt_service_high
  FROM gold.context_indicators
") |>
  as_tibble() |>
  mutate(n_flags = as.integer(reserves_low) + as.integer(ext_debt_high) +
           as.integer(debt_service_high))

# ==============================================================================
# JOIN AND CLASSIFY
# ==============================================================================

message("--- Tiers: joining dimensions and classifying ---")

scores <- d1 |>
  full_join(d2,  by = c("country_code", "year")) |>
  full_join(d3,  by = c("country_code", "year")) |>
  full_join(d4,  by = c("country_code", "year")) |>
  left_join(ctx |> select(country_code, year, n_flags),
            by = c("country_code", "year")) |>
  mutate(n_flags = coalesce(n_flags, 0L))

# Apply tier assignment row-wise
tiers <- scores |>
  rowwise() |>
  mutate(
    result       = list(assign_tier(d1_score, d2_score, d3_score, d4_score, n_flags)),
    tier         = result$tier,
    tier_num     = result$tier_num,
    trigger_type = result$trigger_type,
    trigger_dim  = result$trigger_dim
  ) |>
  ungroup() |>
  select(-result) |>
  mutate(
    n_dims_scored   = rowSums(!is.na(cbind(d1_score, d2_score, d3_score, d4_score))),
    n_dims_elevated = rowSums(
      cbind(d1_score, d2_score, d3_score, d4_score) >= 0.40, na.rm = TRUE
    ),
    mean_score = rowMeans(
      cbind(d1_score, d2_score, d3_score, d4_score), na.rm = TRUE
    ) |> round(3),
    tier = factor(tier,
                  levels = c("Critical","Elevated","Guarded","Moderate","Low"),
                  ordered = TRUE)
  )

# ==============================================================================
# DIAGNOSTICS
# ==============================================================================

tier_yr <- tiers |>
  filter(!is.na(tier)) |>
  count(year, tier) |>
  pivot_wider(names_from = tier, values_from = n, values_fill = 0L)

message("--- Tiers: distribution by year ---")
print(tier_yr)

# Countries with insufficient data
insuf <- tiers |> filter(trigger_type == "insufficient_data")
if (nrow(insuf) > 0)
  message(sprintf("  WARNING: %d country-years have insufficient dimensions for tier assignment",
                  nrow(insuf)))

# Flag-boost summary
boosted <- tiers |> filter(trigger_type == "flag_boost")
if (nrow(boosted) > 0)
  message(sprintf("  %d country-years boosted one tier by active context flags", nrow(boosted)))

# ==============================================================================
# WRITE
# ==============================================================================

write_gold(tiers, "risk_tiers", con)

message("--- Tiers: complete ---")
