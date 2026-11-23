# ingest_comtrade.R
#
# Bronze-layer aggregation from the Comtrade bilateral parquet built by
# ingest_comtrade_build.R and enriched by ingest_price_series.R. Does NOT
# download or re-process raw data.
#
# -- Source parquet ------------------------------------------------------------
#   Path: COMTRADE_PARQUET (set in config.R)
#   Layout: hive-partitioned by iso3c={ISO3C}/yr={YEAR}/
#   Coverage: 173 countries, 227 active HS codes (core and extension sets,
#             D-027, D-029), 2010-present, monthly
#   Values: constant_value_2024 (deflated with the World Bank MUV index to 2024
#           US$, D-025); mirrored gaps filled
#   Broad headings (broad_heading = 1 in the crosswalk: chapters 47 and 72 and
#   chemical headings) count for exposure only and are left out of every table
#   here, so they never enter totals or shares (D-029).
#
# -- Output tables -------------------------------------------------------------
#
#   bronze.comtrade_food_net_trade
#     Annual net trade position by food subcategory (Cereals, Meat, Oils, etc.)
#
#   bronze.comtrade_cereal_hhi
#     Cereal import source HHI -- concentration of staple grain sourcing by partner
#
#   bronze.comtrade_food_price_volatility
#     Intra-year CV of monthly food import unit prices (within-year price swings)
#
#   bronze.comtrade_export_hhi
#     Export commodity HHI restricted to World Bank Pink Sheet commodities
#
#   bronze.comtrade_top5_commodities
#     Top 5 export and import commodities by value, long format with rank
#
#   bronze.comtrade_commodity_exposure
#     Critical mineral, energy, and food shares of total trade by flow direction
#
#   bronze.comtrade_mirror_share
#     Monthly share of trade value estimated via mirroring (partner-reported flows).
#     Surfaced on the dashboard as a data-quality transparency indicator.
#
# -- Design notes --------------------------------------------------------------
#   All value calculations use constant_value_2024 for cross-year comparability.
#   DuckDB reads the parquet directly via glob -- no data loaded into R memory.
#   View and temp table dropped at end; compute_silver_comtrade.R re-registers.
#
# Expects: `con` (DBI connection to georisk.duckdb) from run_pipeline.R

library(DBI)
library(dplyr)
library(stringr)
library(glue)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")
# COMTRADE_PARQUET, CROSSWALK_CSV, START_YR inherited from config.R

# -- Helpers -------------------------------------------------------------------

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

# -- Setup: register parquet view and load commodity metadata ------------------

message("--- Comtrade: registering parquet view ---")

glob <- paste0(COMTRADE_PARQUET, "/**/*.parquet")
if (!file.exists(COMTRADE_PARQUET))
  stop("Comtrade parquet not found: ", COMTRADE_PARQUET)

# Normalize to forward slashes -- DuckDB hive partitioning fails on Windows
# backslash paths when resolving individual partition files
glob <- gsub("\\\\", "/", glob)


# -- Country spine: restrict view to 175 sovereign states ---------------------
if (!exists("COUNTRY_FRAME")) {
  COUNTRY_FRAME <- dbReadTable(con, DBI::Id(schema = "bronze", table = "country_frame"))
}
# Write spine as a DuckDB temp table so the SQL view can join against it
dbWriteTable(con, "country_spine_tmp",
             data.frame(iso3c = COUNTRY_FRAME$iso3c),
             overwrite = TRUE, temporary = TRUE)

message("--- Comtrade: loading commodity metadata ---")
cmd_meta <- read.csv(CROSSWALK_CSV, colClasses = "character") |>
  filter(in_data == "TRUE") |>                          # plywood stays excluded (D-029)
  mutate(
    cmd_code         = ifelse(nchar(cmd_code) %% 2 == 1, paste0("0", cmd_code), cmd_code),
    pink_sheet       = as.integer(pink_sheet),
    broad_heading    = as.integer(coalesce(na_if(broad_heading, ""), "0")),
    conflict_mineral = as.integer(coalesce(na_if(conflict_mineral, ""), "0")),
    critical_mineral = 0L                               # Comtrade mineral flows tracked via USGS only
  ) |>
  select(cmd_code, commodity_name, pink_sheet, category, subcategory, price_source,
         price_name, in_data, parent_code, broad_heading, conflict_mineral, critical_mineral)
dbWriteTable(con, "cmd_meta", cmd_meta, overwrite = TRUE, temporary = TRUE)
dbWriteTable(con, DBI::Id(schema = "bronze", table = "cmd_meta"),
             cmd_meta, overwrite = TRUE)  # persisted for compute_silver_comtrade.R
message("  [bronze.cmd_meta] ", nrow(cmd_meta), " commodities persisted")

# Columns are matched by name: partitions written at different times may
# order them differently. Broad headings are left out (exposure only).
dbExecute(con, glue("
  CREATE OR REPLACE VIEW comtrade_raw AS
  SELECT p.*
  FROM read_parquet('{glob}', hive_partitioning = TRUE, union_by_name = TRUE) p
  INNER JOIN country_spine_tmp s USING (iso3c)
  INNER JOIN cmd_meta m ON p.cmd_code = m.cmd_code AND m.broad_heading = 0
  WHERE p.yr >= {START_YR}
"))
n_rows <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM comtrade_raw")$n
message(sprintf("  Parquet view registered: %s rows", format(n_rows, big.mark = ",")))

# -- A. Food Security ----------------------------------------------------------

message("--- Comtrade bronze: food security ---")

food_net_trade <- dbGetQuery(con, "
  SELECT
    iso3c, yr,
    subcategory AS food_subcategory,
    SUM(CASE WHEN flow_desc = 'Export' THEN  constant_value_2024 ELSE 0 END) AS export_value_usd,
    SUM(CASE WHEN flow_desc = 'Import' THEN  constant_value_2024 ELSE 0 END) AS import_value_usd,
    SUM(CASE WHEN flow_desc = 'Export' THEN  constant_value_2024
                                        ELSE -constant_value_2024 END)        AS net_trade_usd,
    COUNT(DISTINCT commodity_name) AS n_commodities
  FROM comtrade_raw
  WHERE category = 'Food' AND constant_value_2024 > 0
  GROUP BY iso3c, yr, subcategory
") |> as_tibble()

cereal_hhi <- dbGetQuery(con, "
  WITH imports AS (
    SELECT iso3c, yr, partner_iso3c,
           SUM(constant_value_2024) AS import_value
    FROM comtrade_raw
    WHERE flow_desc = 'Import' AND category = 'Food' AND subcategory = 'Cereals'
      AND constant_value_2024 > 0
      AND partner_iso3c NOT IN ('WLD', '_X', 'W00')
    GROUP BY iso3c, yr, partner_iso3c
  ),
  totals AS (SELECT iso3c, yr, SUM(import_value) AS total FROM imports GROUP BY iso3c, yr),
  shares AS (
    SELECT i.iso3c, i.yr, i.partner_iso3c, i.import_value / t.total AS share
    FROM imports i JOIN totals t USING (iso3c, yr)
  )
  SELECT iso3c, yr,
    SUM(share * share)            AS cereal_import_hhi,
    COUNT(DISTINCT partner_iso3c) AS n_cereal_suppliers,
    MAX(share)                    AS top_supplier_share
  FROM shares
  GROUP BY iso3c, yr
") |> as_tibble()

food_price_volatility <- dbGetQuery(con, "
  WITH monthly AS (
    SELECT iso3c, yr, mon, subcategory,
      SUM(constant_value_2024) / NULLIF(SUM(qty_comtrade), 0) AS unit_price
    FROM comtrade_raw
    WHERE flow_desc = 'Import' AND category = 'Food'
      AND constant_value_2024 > 0 AND qty_comtrade > 0
    GROUP BY iso3c, yr, mon, subcategory
  )
  SELECT iso3c, yr, subcategory,
    AVG(unit_price)                                  AS avg_unit_price,
    STDDEV(unit_price)                               AS sd_unit_price,
    STDDEV(unit_price) / NULLIF(AVG(unit_price), 0) AS price_cv,
    COUNT(mon)                                       AS n_months_observed
  FROM monthly
  GROUP BY iso3c, yr, subcategory
  HAVING COUNT(mon) >= 6
") |> as_tibble()
# NOTE: not currently consumed downstream. Context layer uses cv_basket_wtd and
# price_vol_cv from silver.comtrade_import_groups_ts (multi-year volatility).
# This table captures intra-year price swings -- a distinct measure worth
# retaining for future use (e.g. food security module, seasonal risk signals).

write_bronze(food_net_trade,        "comtrade_food_net_trade",        con)
write_bronze(cereal_hhi,            "comtrade_cereal_hhi",            con)
write_bronze(food_price_volatility, "comtrade_food_price_volatility", con)

# -- B. Trade Concentration ----------------------------------------------------

message("--- Comtrade bronze: trade concentration ---")

export_hhi <- dbGetQuery(con, "
  WITH commodity_exports AS (
    SELECT t.iso3c, t.yr, t.commodity_name,
      SUM(t.constant_value_2024) AS export_value
    FROM comtrade_raw t
    INNER JOIN cmd_meta m USING (cmd_code)
    WHERE t.flow_desc = 'Export' AND t.constant_value_2024 > 0 AND m.pink_sheet = 1
    GROUP BY t.iso3c, t.yr, t.commodity_name
  ),
  totals AS (SELECT iso3c, yr, SUM(export_value) AS total FROM commodity_exports GROUP BY iso3c, yr),
  shares AS (
    SELECT c.iso3c, c.yr, c.commodity_name, c.export_value,
           c.export_value / t.total AS share
    FROM commodity_exports c JOIN totals t USING (iso3c, yr)
  )
  SELECT iso3c, yr,
    SUM(share * share)             AS export_commodity_hhi,
    COUNT(DISTINCT commodity_name) AS n_export_commodities,
    SUM(export_value)              AS total_export_value_usd
  FROM shares
  GROUP BY iso3c, yr
") |> as_tibble()

top5_commodities <- dbGetQuery(con, "
  WITH flows AS (
    SELECT iso3c, yr, flow_desc, commodity_name, category, subcategory,
      SUM(constant_value_2024) AS trade_value_usd
    FROM comtrade_raw
    WHERE constant_value_2024 > 0
    GROUP BY iso3c, yr, flow_desc, commodity_name, category, subcategory
  ),
  ranked AS (
    SELECT *,
      ROW_NUMBER() OVER (PARTITION BY iso3c, yr, flow_desc ORDER BY trade_value_usd DESC) AS rank
    FROM flows
  )
  SELECT * FROM ranked WHERE rank <= 5
  ORDER BY iso3c, yr, flow_desc, rank
") |> as_tibble()

write_bronze(export_hhi,       "comtrade_export_hhi",       con)
write_bronze(top5_commodities, "comtrade_top5_commodities", con)

# -- C. Commodity Exposure -----------------------------------------------------

message("--- Comtrade bronze: commodity exposure ---")

commodity_exposure <- dbGetQuery(con, "
  WITH flagged AS (
    SELECT t.iso3c, t.yr, t.flow_desc, t.commodity_name, t.category,
           t.constant_value_2024
    FROM comtrade_raw t
    WHERE t.constant_value_2024 > 0
  ),
  totals AS (
    SELECT iso3c, yr, flow_desc,
      SUM(constant_value_2024)                                                 AS total_value,
      SUM(CASE WHEN category = 'Energy'  THEN constant_value_2024 ELSE 0 END) AS energy_value,
      SUM(CASE WHEN category = 'Food'    THEN constant_value_2024 ELSE 0 END) AS food_value,
      COUNT(DISTINCT commodity_name) AS n_commodities
    FROM flagged
    GROUP BY iso3c, yr, flow_desc
  ),
  top_commodity AS (
    SELECT DISTINCT ON (iso3c, yr, flow_desc)
      iso3c, yr, flow_desc,
      commodity_name           AS top_commodity_name,
      SUM(constant_value_2024) AS top_commodity_value
    FROM flagged
    GROUP BY iso3c, yr, flow_desc, commodity_name
    ORDER BY iso3c, yr, flow_desc, SUM(constant_value_2024) DESC
  )
  SELECT t.iso3c, t.yr, t.flow_desc,
    t.total_value AS total_value_usd,
    t.energy_value, t.food_value, t.n_commodities,
    t.energy_value / NULLIF(t.total_value, 0) AS energy_share,
    t.food_value   / NULLIF(t.total_value, 0) AS food_share,
    tc.top_commodity_name,
    tc.top_commodity_value / NULLIF(t.total_value, 0) AS top_commodity_share
  FROM totals t
  LEFT JOIN top_commodity tc USING (iso3c, yr, flow_desc)
  ORDER BY t.iso3c, t.yr, t.flow_desc
") |> as_tibble()

write_bronze(commodity_exposure, "comtrade_commodity_exposure", con)

# -- D. Mirror Share -----------------------------------------------------------
# Proportion of trade value estimated from partner-reported (mirrored) flows.

message("--- Comtrade bronze: mirror share ---")

mirror_share <- dbGetQuery(con, "
  SELECT
    iso3c AS country_code, yr AS year, mon AS month, flow_desc,
    SUM(constant_value_2024) AS total_value_usd,
    SUM(CASE WHEN mirrored = TRUE THEN constant_value_2024 ELSE 0 END) AS mirrored_value_usd,
    SUM(CASE WHEN mirrored = TRUE THEN constant_value_2024 ELSE 0 END) /
      NULLIF(SUM(constant_value_2024), 0) AS mirror_share
  FROM comtrade_raw
  WHERE constant_value_2024 > 0
  GROUP BY iso3c, yr, mon, flow_desc
") |> as_tibble()

write_bronze(mirror_share, "comtrade_mirror_share", con)

# -- E. D4 Bilateral Imports ---------------------------------------------------
# Annual partner-level import values for pink_sheet + food + energy commodities.
# Used by compute_d4_network.R for trade-weighted contagion scores.
# Must be built before comtrade_raw is dropped.

message("--- Comtrade bronze: D4 bilateral imports ---")

d4_bilateral <- dbGetQuery(con, "
  SELECT
    t.iso3c         AS country_code,
    t.yr            AS year,
    t.partner_iso3c AS partner_code,
    SUM(t.constant_value_2024) AS import_usd
  FROM comtrade_raw t
  LEFT JOIN cmd_meta m ON t.cmd_code = m.cmd_code
  WHERE t.flow_desc = 'Import'
    AND t.constant_value_2024 > 0
    AND t.partner_iso3c IS NOT NULL
    AND t.partner_iso3c NOT IN ('WLD', '_X', 'W00')
    AND (COALESCE(m.pink_sheet, 0) = 1
         OR t.category = 'Food'
         OR t.category = 'Energy')
  GROUP BY t.iso3c, t.yr, t.partner_iso3c
") |> as_tibble()

write_bronze(d4_bilateral, "comtrade_d4_bilateral", con)

# -- Cleanup -------------------------------------------------------------------

dbExecute(con, "DROP VIEW IF EXISTS comtrade_raw")
dbExecute(con, "DROP TABLE IF EXISTS cmd_meta")
dbExecute(con, "DROP TABLE IF EXISTS country_spine_tmp")

# -- Summary -------------------------------------------------------------------

message("--- Comtrade bronze ingest complete ---")
message(sprintf("  food_net_trade:        %d rows (%d country-years)",
                nrow(food_net_trade),
                n_distinct(paste(food_net_trade$iso3c, food_net_trade$yr))))
message(sprintf("  cereal_hhi:            %d rows", nrow(cereal_hhi)))
message(sprintf("  food_price_volatility: %d rows", nrow(food_price_volatility)))
message(sprintf("  export_hhi:            %d rows", nrow(export_hhi)))
message(sprintf("  top5_commodities:      %d rows", nrow(top5_commodities)))
message(sprintf("  commodity_exposure:    %d rows", nrow(commodity_exposure)))
message(sprintf("  mirror_share:          %d rows (%.1f%% of months have mirror data)",
                nrow(mirror_share),
                mean(mirror_share$mirror_share > 0, na.rm = TRUE) * 100))
message(sprintf("  d4_bilateral:          %d rows (%d reporter-year-partner links)",
                nrow(d4_bilateral),
                nrow(d4_bilateral)))
