# compute_d4_network.R
#
# Dimension 4: Network Contagion Risk
#
# A country's D4 score is the trade-weighted average of its import partners'
# composite risk scores. The intuition: a country that sources critical imports
# from structurally fragile partners inherits a share of their vulnerability --
# a supply shock in a high-risk partner propagates along the trade link.
#
# -- Why no new Bayesian model ------------------------------------------------
# D4 does not require a new sampler. It is a deterministic aggregation of
# already-estimated posteriors from D1, D2, and D3. Uncertainty from those
# models is propagated through the same weighted-average formula applied to
# q05 and q95 bounds, giving a D4 confidence interval without additional
# sampling.
#
# -- Design -------------------------------------------------------------------
# Partner composite score:
#   composite = w_d1 * D1 + w_d2 * D2 + w_d3 * D3
#   Applied separately to risk_score, risk_score_q05, risk_score_q95.
#
# Trade weights:
#   Annual bilateral import shares, restricted to pink_sheet + food + energy
#   imports. This commodity scope captures the channels where partner
#   fragility matters most -- commodity supply chains, not manufactured goods.
#   Annual weights broadcast to each month of that year.
#   Weights normalized to sum to 1 within reporter-year (or reporter-year-month).
#
# Monthly vs. annual:
#   D1: annual score broadcast to months (no monthly model).
#   D2: monthly scores used directly.
#   D3: monthly scores used directly.
#
# Self-links:
#   A country's own risk scores are excluded from its D4 computation.
#   Partners not present in any gold table are dropped (unscored partners
#   don't contribute -- their trade weight is redistributed to scored partners).
#
# -- Outputs ------------------------------------------------------------------
#   gold.d4_partner_detail  country-partner-year-month individual partner scores,
#                           trade weights, and category-level bilateral shares:
#                             food_import_share        share of food imports from partner
#                             fertilizer_import_share  share of fertilizer imports from partner
#                             energy_import_share      share of energy imports from partner
#                             export_share             share of exports going to partner
#   gold.d4_monthly         country-year-month D4 composite + CI
#   gold.d4_annual          country-year D4 composite + CI (December snapshot)
#
# Expects: `con` (DBI connection to econrisk.duckdb) from run_pipeline.R

library(DBI)
library(dplyr)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# ==============================================================================
# CONFIG
# ==============================================================================

# Dimension weights for partner composite score -- must sum to 1
W_D1 <- 1/3
W_D2 <- 1/3
W_D3 <- 1/3

stopifnot("Dimension weights must sum to 1" = abs(W_D1 + W_D2 + W_D3 - 1) < 1e-9)

# Minimum import share to include a partner (filters very minor trade links)
MIN_PARTNER_SHARE <- 0.01   # 1% of scoped imports

write_gold <- function(df, table_name, con) {
  df <- mutate(df, score_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS gold")
  dbWriteTable(con, DBI::Id(schema = "gold", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [gold.%s] %d rows written", table_name, nrow(df)))
}

# ==============================================================================
# TRADE WEIGHTS: annual bilateral import shares, pink_sheet + food + energy
# ==============================================================================

message("--- D4: computing trade weights ---")

# Pull from bronze table built by ingest_comtrade.R (comtrade_raw is dropped
# before this script runs so we cannot query the view directly)
trade_raw <- dbGetQuery(con, "
  SELECT country_code, year, partner_code, import_usd
  FROM bronze.comtrade_d4_bilateral
") |> as_tibble()

# Compute normalized import share within reporter-year
trade_weights <- trade_raw |>
  group_by(country_code, year) |>
  mutate(import_share = import_usd / sum(import_usd, na.rm = TRUE)) |>
  ungroup() |>
  # Drop self-links and minor partners
  filter(country_code != partner_code, import_share >= MIN_PARTNER_SHARE) |>
  # Re-normalize after filtering so weights sum to 1
  group_by(country_code, year) |>
  mutate(weight = import_share / sum(import_share, na.rm = TRUE)) |>
  ungroup() |>
  select(country_code, year, partner_code, weight)

message(sprintf("  Trade weights: %d reporter-year-partner links | %d reporters | years %d-%d",
                nrow(trade_weights), n_distinct(trade_weights$country_code),
                min(trade_weights$year), max(trade_weights$year)))

# ==============================================================================
# PULL GOLD POSTERIORS
# ==============================================================================

message("--- D4: pulling D1/D2/D3 posteriors ---")

# D1 annual -- broadcast to months in join below
d1 <- dbGetQuery(con, "
  SELECT country_code, year,
    risk_score       AS d1_score,
    risk_score_q05   AS d1_q05,
    risk_score_q95   AS d1_q95
  FROM gold.d1_macro_posterior
") |> as_tibble()

# D2 monthly
d2 <- dbGetQuery(con, "
  SELECT country_code, year, month,
    risk_score       AS d2_score,
    risk_score_q05   AS d2_q05,
    risk_score_q95   AS d2_q95
  FROM gold.d2_monthly_posterior
") |> as_tibble()

# D3 monthly
d3 <- dbGetQuery(con, "
  SELECT country_code, year, month,
    risk_score       AS d3_score,
    risk_score_q05   AS d3_q05,
    risk_score_q95   AS d3_q95
  FROM gold.d3_monthly_posterior
") |> as_tibble()

# ==============================================================================
# PARTNER COMPOSITE SCORES (monthly grain)
# ==============================================================================

message("--- D4: computing partner composite scores ---")

# Use D2 monthly as the time spine; join D1 annual (broadcast) and D3 monthly
partner_scores <- d2 |>
  left_join(d1, by = c("country_code", "year")) |>    # D1 annual broadcasts to all months
  left_join(d3, by = c("country_code", "year", "month")) |>
  transmute(
    country_code,
    year,
    month,
    composite       = W_D1 * d1_score + W_D2 * d2_score + W_D3 * d3_score,
    composite_q05   = W_D1 * d1_q05   + W_D2 * d2_q05   + W_D3 * d3_q05,
    composite_q95   = W_D1 * d1_q95   + W_D2 * d2_q95   + W_D3 * d3_q95
  ) |>
  filter(!is.na(composite))

message(sprintf("  Partner scores: %d country-year-months scored",
                nrow(partner_scores)))

# ==============================================================================
# PARTNER DETAIL TABLE
# Reporter-partner-year-month with individual scores and weights.
# This is the table the dashboard uses to show a country's full partner
# risk breakdown -- who its partners are, how much it trades with them,
# and what each partner's D1/D2/D3 risk looks like.
# ==============================================================================

message("--- D4: building partner detail table ---")

country_names <- dbGetQuery(
  con, "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
) |> as_tibble()

# Bilateral category shares -- annual, from compute_silver_comtrade.R
bilateral_shares <- dbGetQuery(con, "
  SELECT country_code, partner_code, year,
    food_import_share, fertilizer_import_share,
    energy_import_share, export_share
  FROM silver.bilateral_category_shares
") |> as_tibble()

d4_partner_detail <- trade_weights |>
  left_join(
    partner_scores |> distinct(year, month),
    by = "year",
    relationship = "many-to-many"
  ) |>
  # Join partner composite + individual D1/D2/D3 scores from their source tables
  left_join(
    partner_scores |> rename(partner_code = country_code),
    by = c("partner_code", "year", "month")
  ) |>
  left_join(
    d1 |> select(country_code, year, d1_score, d1_q05, d1_q95) |>
      rename(partner_code = country_code),
    by = c("partner_code", "year")
  ) |>
  left_join(
    d2 |> select(country_code, year, month, d2_score, d2_q05, d2_q95) |>
      rename(partner_code = country_code),
    by = c("partner_code", "year", "month")
  ) |>
  left_join(
    d3 |> select(country_code, year, month, d3_score, d3_q05, d3_q95) |>
      rename(partner_code = country_code),
    by = c("partner_code", "year", "month")
  ) |>
  filter(!is.na(composite)) |>
  group_by(country_code, year, month) |>
  mutate(weight_norm = weight / sum(weight, na.rm = TRUE)) |>
  ungroup() |>
  left_join(
    country_names |> rename(partner_name = country_name, partner_code = country_code),
    by = "partner_code"
  ) |>
  left_join(
    bilateral_shares,
    by = c("country_code", "partner_code", "year")
  ) |>
  select(
    country_code, year, month,
    partner_code, partner_name,
    weight_norm,
    food_import_share, fertilizer_import_share,
    energy_import_share, export_share,
    d1_score, d1_q05, d1_q95,
    d2_score, d2_q05, d2_q95,
    d3_score, d3_q05, d3_q95,
    composite, composite_q05, composite_q95
  ) |>
  arrange(country_code, year, month, desc(weight_norm))

# ==============================================================================
# D4 MONTHLY: trade-weighted average of partner composites
# ==============================================================================

message("--- D4: computing monthly network contagion scores ---")

# Broadcast annual trade weights to months via the partner_scores spine
d4_monthly <- trade_weights |>
  left_join(
    partner_scores |> distinct(year, month),
    by = "year",
    relationship = "many-to-many"
  ) |>
  # Rename for clarity: partner_code is now the "score lookup" key
  rename(partner_lookup = partner_code) |>
  left_join(
    partner_scores |>
      rename(partner_lookup = country_code,
             partner_composite     = composite,
             partner_composite_q05 = composite_q05,
             partner_composite_q95 = composite_q95),
    by = c("partner_lookup", "year", "month")
  ) |>
  # Drop partners with no score
  filter(!is.na(partner_composite)) |>
  # Re-normalize weights to scored partners only
  group_by(country_code, year, month) |>
  mutate(weight_norm = weight / sum(weight, na.rm = TRUE)) |>
  summarise(
    d4_score       = sum(weight_norm * partner_composite,     na.rm = TRUE),
    d4_score_q05   = sum(weight_norm * partner_composite_q05, na.rm = TRUE),
    d4_score_q95   = sum(weight_norm * partner_composite_q95, na.rm = TRUE),
    n_partners     = n(),
    .groups = "drop"
  ) |>
  mutate(d4_score_width = d4_score_q95 - d4_score_q05) |>
  arrange(country_code, year, month)

# ==============================================================================
# D4 ANNUAL: December snapshot
# ==============================================================================

d4_annual <- d4_monthly |>
  filter(month == 12L) |>
  select(country_code, year, d4_score, d4_score_q05, d4_score_q95,
         d4_score_width, n_partners) |>
  arrange(country_code, year)

# ==============================================================================
# WRITE
# ==============================================================================

write_gold(d4_partner_detail, "d4_partner_detail", con)
write_gold(d4_monthly,        "d4_monthly",        con)
write_gold(d4_annual,         "d4_annual",         con)

# ==============================================================================
# SUMMARY
# ==============================================================================

message("--- D4 complete ---")
message(sprintf("  Monthly: %d country-year-months | risk score [%.3f, %.3f] | mean CI width %.3f",
                nrow(d4_monthly),
                min(d4_monthly$d4_score, na.rm = TRUE),
                max(d4_monthly$d4_score, na.rm = TRUE),
                mean(d4_monthly$d4_score_width, na.rm = TRUE)))
message(sprintf("  Annual (Dec snapshot): %d country-years",
                nrow(d4_annual)))
message(sprintf("  Weights: D1=%.2f D2=%.2f D3=%.2f | min partner share=%.1f%%",
                W_D1, W_D2, W_D3, MIN_PARTNER_SHARE * 100))
