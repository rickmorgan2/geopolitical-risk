# compute_silver_usgs.R
#
# Silver-layer: critical mineral production profile from USGS MCS.
#
# Computes structural supply concentration metrics from mine production data.
# These are context-layer indicators -- surfaced as annotations on the dashboard
# rather than fed into Bayesian risk score arithmetic. Coverage is annual and
# limited (~22 minerals, ~2018 onwards), making them unsuitable for the AR(1)
# latent variable models used in D1-D3.
#
# -- What this measures -------------------------------------------------------
# The USGS Mineral Commodity Summaries give country-level mine production and
# (from MCS 2023 onwards) country-level reserves. From these we derive:
#
#   production_share  A country's share of world production for each mineral
#                     in a given year. High share = structural market power.
#
#   global_hhi        Herfindahl-Hirschman Index of world production for each
#                     mineral-year. High HHI = globally concentrated supply;
#                     any single disruption has outsized impact.
#
#   reserve_share     A country's share of world reserves (MCS 2023+).
#                     Forward-looking market power signal.
#
#   rp_ratio          Reserves / production: years of supply at current
#                     extraction rates. Structural constraint on future supply.
#
# -- Why not in D2/D3 scoring -------------------------------------------------
# Critical mineral export concentration has an ambiguous risk direction: a
# country dominating cobalt production (DRC) or rare earth processing (CHN)
# holds structural leverage, not pure vulnerability. Forcing this into a
# standard HHI-based risk score would misrepresent the risk dynamic.
# The correct use is as contextual annotation: flagging which countries are
# major producers, how globally concentrated each mineral's supply is, and
# providing interpretive context for D2 import scores of dependent importers
# and D3 export scores of mono-producers.
#
# -- Output -------------------------------------------------------------------
#   silver.critical_mineral_profile   country-mineral-year production context
#   silver.critical_mineral_global    mineral-year global supply concentration
#
# Expects: `con` (DBI connection to econrisk.duckdb) from run_pipeline.R

library(DBI)
library(dplyr)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

write_silver <- function(df, table_name, con) {
  df <- mutate(df, transform_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS silver")
  dbWriteTable(con, DBI::Id(schema = "silver", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [silver.%s] %d rows written", table_name, nrow(df)))
}

message("--- USGS silver: building critical mineral profiles ---")

production <- dbGetQuery(con, "
  SELECT iso3c AS country_code, usgs_commodity, year, production
  FROM bronze.usgs_production
  WHERE production IS NOT NULL AND production > 0
") |> as_tibble()

reserves <- dbGetQuery(con, "
  SELECT iso3c AS country_code, usgs_commodity, mcs_edition, reserves
  FROM bronze.usgs_reserves
  WHERE reserves IS NOT NULL AND reserves > 0
") |> as_tibble()

country_names <- dbGetQuery(
  con, "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
)

if (nrow(production) == 0)
  stop("bronze.usgs_production is empty -- run ingest_usgs.R first")

# ==============================================================================
# GLOBAL SUPPLY CONCENTRATION (mineral-year)
# ==============================================================================

# World total production per mineral-year; HHI of country shares
global_supply <- production |>
  group_by(usgs_commodity, year) |>
  summarise(
    world_production  = sum(production, na.rm = TRUE),
    n_producers       = n_distinct(country_code),
    global_hhi        = sum((production / sum(production))^2),
    top_producer_share = max(production / sum(production)),
    .groups = "drop"
  ) |>
  arrange(usgs_commodity, year)

# ==============================================================================
# COUNTRY-MINERAL PRODUCTION PROFILE (country-mineral-year)
# ==============================================================================

country_profile <- production |>
  left_join(
    global_supply |> select(usgs_commodity, year, world_production),
    by = c("usgs_commodity", "year")
  ) |>
  mutate(production_share = production / world_production) |>
  arrange(country_code, usgs_commodity, year) |>
  # YoY production change
  group_by(country_code, usgs_commodity) |>
  mutate(production_yoy = production / lag(production) - 1) |>
  ungroup()

# Join most recent reserves (snapshot -- not time-varying per country)
# Use the most recent mcs_edition available per country-commodity
reserves_latest <- reserves |>
  group_by(country_code, usgs_commodity) |>
  slice_max(mcs_edition, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(country_code, usgs_commodity, reserves, mcs_edition)

# World total reserves for share computation
world_reserves <- reserves_latest |>
  group_by(usgs_commodity) |>
  summarise(world_reserves = sum(reserves, na.rm = TRUE), .groups = "drop")

reserves_profile <- reserves_latest |>
  left_join(world_reserves, by = "usgs_commodity") |>
  mutate(reserve_share = reserves / world_reserves) |>
  select(country_code, usgs_commodity, reserves, reserve_share, mcs_edition)

# Join reserves and compute reserves-to-production ratio
critical_mineral_profile <- country_profile |>
  left_join(reserves_profile, by = c("country_code", "usgs_commodity")) |>
  # rp_ratio: years of supply at current production rate
  mutate(rp_ratio = if_else(production > 0, reserves / production, NA_real_)) |>
  left_join(country_names, by = "country_code") |>
  left_join(
    global_supply |> select(usgs_commodity, year, global_hhi, n_producers,
                             top_producer_share),
    by = c("usgs_commodity", "year")
  ) |>
  select(
    country_code, country_name, usgs_commodity, year,
    production, production_share, production_yoy,
    reserves, reserve_share, rp_ratio, mcs_edition,
    global_hhi, n_producers, top_producer_share
  ) |>
  arrange(usgs_commodity, desc(production_share), year)

# ==============================================================================
# WRITE
# ==============================================================================

write_silver(critical_mineral_profile, "critical_mineral_profile", con)
write_silver(global_supply,            "critical_mineral_global",  con)

# -- Summary ------------------------------------------------------------------

message("--- USGS silver complete ---")
message(sprintf("  critical_mineral_profile: %d rows | %d countries | %d minerals | years %d-%d",
                nrow(critical_mineral_profile),
                n_distinct(critical_mineral_profile$country_code),
                n_distinct(critical_mineral_profile$usgs_commodity),
                min(critical_mineral_profile$year, na.rm = TRUE),
                max(critical_mineral_profile$year, na.rm = TRUE)))

# Flag highly concentrated minerals (global HHI > 0.25 in most recent year)
recent_yr <- max(global_supply$year, na.rm = TRUE)
concentrated <- global_supply |>
  filter(year == recent_yr, global_hhi > 0.25) |>
  arrange(desc(global_hhi)) |>
  select(usgs_commodity, global_hhi, n_producers, top_producer_share) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

message(sprintf("  Highly concentrated minerals (global HHI > 0.25) in %d:", recent_yr))
print(concentrated)

# Top producers per highly concentrated mineral
walk(concentrated$usgs_commodity, \(mineral) {
  top <- critical_mineral_profile |>
    filter(usgs_commodity == mineral, year == recent_yr) |>
    arrange(desc(production_share)) |>
    slice_head(n = 5) |>
    select(country_code, production_share, reserve_share) |>
    mutate(across(where(is.numeric), \(x) round(x, 3)))
  message(sprintf("  %s top producers:", mineral))
  print(top)
})
