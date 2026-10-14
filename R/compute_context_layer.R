# compute_context_layer.R
#
# Context layer: sparse indicators surfaced as dashboard annotations.
#
# These indicators were excluded from D1-D3 Bayesian models due to high
# missingness (> 30%) or annual-only grain, but remain analytically valuable
# when present. Rather than discarding them, this script assembles them into
# a clean context table that the dashboard narrative layer can join to country
# cards to enrich interpretation of the modelled risk scores.
#
# The pattern: if data exists for a country-year, surface it; if not, the
# risk score stands on its own without degrading from absence.
#
# -- Sources ------------------------------------------------------------------
#   silver.macro_derived (already computed by transform_silver.R):
#     reserves_months_imports  -- FX reserve adequacy; warn < 3 months
#     ext_debt_to_reserves     -- external debt vs. reserve buffer; warn > 1
#     debt_service_ratio       -- debt service / export earnings; warn > 20%
#
#   bronze.wb_wgi_raw (written by ingest_wgi.R):
#     regulatory_quality       -- WGI [-2.5, +2.5]; higher = better
#     control_of_corruption    -- WGI [-2.5, +2.5]; higher = less corrupt
#
#   silver.critical_mineral_profile (written by compute_silver_usgs.R):
#     major_producer flag      -- production_share > 5% in any mineral
#     n_dominant_minerals      -- count of minerals where share > 5%
#     most_concentrated_mineral -- mineral with highest global_hhi this country
#                                  produces at meaningful scale (share > 1%)
#
# -- Coverage flags -----------------------------------------------------------
# Each indicator block carries a `_available` boolean so the dashboard can
# show data availability at a glance rather than leaving unexplained NAs.
#
# -- Output -------------------------------------------------------------------
#   gold.context_indicators  country-year, all context indicators + flags
#
# Expects: `con` (DBI connection to econrisk.duckdb) from run_pipeline.R

library(DBI)
library(dplyr)
library(tidyr)
library(purrr)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

write_gold <- function(df, table_name, con) {
  df <- mutate(df, score_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS gold")
  dbWriteTable(con, DBI::Id(schema = "gold", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [gold.%s] %d rows written", table_name, nrow(df)))
}

message("--- Context layer: assembling sparse indicators ---")

# -- Country-year spine -------------------------------------------------------
# Use the D1 posterior as the reference spine: every country-year the
# dashboard will show a score for.
# Cap at the latest observed data year -- D1 includes WEO forecast years
# (up to 2030) but context indicators have no coverage beyond current data.
CONTEXT_MAX_YR <- dbGetQuery(con,
  "SELECT MAX(year) AS max_yr FROM silver.macro_derived"
)$max_yr

spine <- dbGetQuery(con, glue::glue("
  SELECT DISTINCT country_code, country_name, year
  FROM gold.d1_macro_posterior
  WHERE year <= {CONTEXT_MAX_YR}
")) |> as_tibble()

# BLOCK 1: External buffer indicators (from silver.macro_derived)
# reserves_months_imports, ext_debt_to_reserves, debt_service_ratio

message("  [1/6] External buffer indicators from silver.macro_derived")

ext_buffer <- dbGetQuery(con, "
  SELECT country_code, year,
    reserves_months_imports,
    ext_debt_to_reserves,
    debt_service_ratio
  FROM silver.macro_derived
") |>
  as_tibble() |>
  mutate(
    # Warn thresholds -- flagged as boolean for dashboard alert logic
    reserves_low       = !is.na(reserves_months_imports) & reserves_months_imports < 3,
    ext_debt_high      = !is.na(ext_debt_to_reserves)    & ext_debt_to_reserves > 1,
    debt_service_high  = !is.na(debt_service_ratio)       & debt_service_ratio > 0.2,
    # Coverage flags
    reserves_available    = !is.na(reserves_months_imports),
    ext_debt_available    = !is.na(ext_debt_to_reserves),
    debt_service_available = !is.na(debt_service_ratio)
  )

# BLOCK 2: WGI governance indicators (from bronze.wb_wgi_raw)

message("  [2/6] WGI governance indicators from bronze.wb_wgi_raw")

# Check if WGI table exists before querying
wgi_tables <- dbGetQuery(con,
  "SELECT table_name FROM information_schema.tables
   WHERE table_schema = 'bronze' AND table_name = 'wb_wgi_raw'"
)$table_name

if (length(wgi_tables) > 0) {
  wgi <- dbGetQuery(con, "
    SELECT country_code, period AS year, indicator_label, value
    FROM bronze.wb_wgi_raw
    WHERE indicator_label IN ('regulatory_quality', 'control_of_corruption')
  ") |>
    as_tibble() |>
    pivot_wider(names_from = indicator_label, values_from = value) |>
    mutate(
      # Composite governance score: mean of available WGI dimensions
      governance_score      = rowMeans(
        cbind(regulatory_quality, control_of_corruption), na.rm = TRUE
      ),
      governance_available  = !is.na(regulatory_quality) | !is.na(control_of_corruption),
      # Warn flag: governance score below -1 (weak institutions)
      governance_weak       = !is.na(governance_score) & governance_score < -1
    )
  message(sprintf("    WGI: %d country-years | %d countries",
                  nrow(wgi), n_distinct(wgi$country_code)))
} else {
  message("    WGI table not found -- run ingest_wgi.R first. Filling with NAs.")
  wgi <- spine |>
    select(country_code, year) |>
    mutate(
      regulatory_quality    = NA_real_,
      control_of_corruption = NA_real_,
      governance_score      = NA_real_,
      governance_available  = FALSE,
      governance_weak       = FALSE
    )
}

# BLOCK 3: Critical mineral producer flags (from silver.critical_mineral_profile)

message("  [3/6] Critical mineral flags from silver.critical_mineral_profile")

cm_tables <- dbGetQuery(con,
  "SELECT table_name FROM information_schema.tables
   WHERE table_schema = 'silver' AND table_name = 'critical_mineral_profile'"
)$table_name

if (length(cm_tables) > 0) {
  cm_raw <- dbGetQuery(con, "
    SELECT country_code, usgs_commodity, year,
           production_share, global_hhi
    FROM silver.critical_mineral_profile
  ") |> as_tibble()

  cm_context <- cm_raw |>
    group_by(country_code, year) |>
    summarise(
      # Major producer: any mineral where this country holds > 5% of world output
      major_producer      = any(production_share > 0.05, na.rm = TRUE),
      n_dominant_minerals = sum(production_share > 0.05, na.rm = TRUE),
      # Full list of minerals where production_share > 5%, sorted by global_hhi desc
      # so the most strategically concentrated appear first
      dominant_minerals   = list(
        usgs_commodity[production_share > 0.05] |>
          (\(m) m[order(-global_hhi[production_share > 0.05])])()
      ),
      max_strategic_hhi   = suppressWarnings(max(
        if_else(production_share > 0.05, global_hhi, NA_real_), na.rm = TRUE
      )),
      .groups = "drop"
    ) |>
    mutate(
      max_strategic_hhi = if_else(is.infinite(max_strategic_hhi), NA_real_,
                                  max_strategic_hhi),
      # Collapsed string for display / narrative prompt (e.g. "cobalt, lithium")
      dominant_minerals_str = map_chr(dominant_minerals,
                                      \(x) if (length(x) == 0) NA_character_
                                      else paste(x, collapse = ", ")),
      cm_available = TRUE
    )
  
  message(sprintf("    Critical minerals: %d major producers identified",
                  sum(cm_context$major_producer, na.rm = TRUE)))
  message(sprintf("    Dominant mineral list: max %d minerals per country (mean %.1f)",
                  max(cm_context$n_dominant_minerals),
                  mean(cm_context$n_dominant_minerals[cm_context$major_producer])))
} else {
  message("    critical_mineral_profile not found -- run compute_silver_usgs.R first. Filling with NAs.")
  cm_context <- spine |>
    select(country_code, year) |>
    mutate(
      major_producer        = FALSE,
      n_dominant_minerals   = 0L,
      dominant_minerals     = list(character(0)),
      dominant_minerals_str = NA_character_,
      max_strategic_hhi     = NA_real_,
      cm_available          = FALSE
    )
}

# BLOCK 4: Food import price volatility (from silver.comtrade_import_groups_ts)
# cv_basket_wtd, price_vol_cv for the food commodity group

message("  [4/6] Food import price volatility from silver.comtrade_import_groups_ts")

fv_tables <- dbGetQuery(con,
                        "SELECT table_name FROM information_schema.tables
   WHERE table_schema = 'silver' AND table_name = 'comtrade_import_groups_ts'"
)$table_name

if (length(fv_tables) > 0) {
  food_price_vol <- dbGetQuery(con, "
    SELECT country_code, year,
      cv_basket_wtd,
      price_vol_cv
    FROM silver.comtrade_import_groups_ts
    WHERE commodity_group = 'food'
  ") |>
    as_tibble() |>
    group_by(country_code, year) |>
    summarise(
      food_cv_basket_wtd    = mean(cv_basket_wtd, na.rm = TRUE),
      food_price_vol_cv     = mean(price_vol_cv,  na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Cross-sectional p75 warn flag; review threshold after first empirical run
      food_price_vol_high   = !is.na(food_cv_basket_wtd) &
        food_cv_basket_wtd > quantile(food_cv_basket_wtd, 0.75, na.rm = TRUE),
      food_price_vol_available = !is.na(food_cv_basket_wtd)
    )
  message(sprintf("    Food price vol: %d country-years | %d with high flag",
                  nrow(food_price_vol), sum(food_price_vol$food_price_vol_high, na.rm = TRUE)))
} else {
  message("    comtrade_import_groups_ts not found -- run compute_silver_comtrade.R first. Filling with NAs.")
  food_price_vol <- spine |>
    select(country_code, year) |>
    mutate(
      food_cv_basket_wtd       = NA_real_,
      food_price_vol_cv        = NA_real_,
      food_price_vol_high      = FALSE,
      food_price_vol_available = FALSE
    )
}


# ==============================================================================
# BLOCK 5: GDP (from bronze.gdp_annual)
# ==============================================================================

message("  [5/6] GDP from bronze.gdp_annual")

gdp_tables <- dbGetQuery(con,
  "SELECT table_name FROM information_schema.tables
   WHERE table_schema = 'bronze' AND table_name = 'gdp_annual'"
)$table_name

if (length(gdp_tables) > 0) {
  gdp_context <- dbGetQuery(con, "
    SELECT iso3c AS country_code, year,
           gdp_const_usd,
           gdp_growth_pct,
           gdp_per_capita_const_usd
    FROM bronze.gdp_annual
  ") |>
    as_tibble() |>
    mutate(gdp_available = !is.na(gdp_const_usd))
  message(sprintf("    GDP: %d country-years | %d countries",
                  nrow(gdp_context), n_distinct(gdp_context$country_code)))
} else {
  message("    bronze.gdp_annual not found -- run ingest_gdp.R first. Filling with NAs.")
  gdp_context <- spine |>
    select(country_code, year) |>
    mutate(
      gdp_const_usd             = NA_real_,
      gdp_growth_pct            = NA_real_,
      gdp_per_capita_const_usd  = NA_real_,
      gdp_available             = FALSE
    )
}

# BLOCK 6: Population (from bronze.population_annual)

message("  [6/6] Population from bronze.population_annual")

pop_tables <- dbGetQuery(con,
  "SELECT table_name FROM information_schema.tables
   WHERE table_schema = 'bronze' AND table_name = 'population_annual'"
)$table_name

if (length(pop_tables) > 0) {
  pop_context <- dbGetQuery(con, "
    SELECT iso3c AS country_code, year, population
    FROM bronze.population_annual
  ") |>
    as_tibble() |>
    mutate(population_available = !is.na(population))
  message(sprintf("    Population: %d country-years | %d countries",
                  nrow(pop_context), n_distinct(pop_context$country_code)))
} else {
  message("    bronze.population_annual not found -- run ingest_population.R first. Filling with NAs.")
  pop_context <- spine |>
    select(country_code, year) |>
    mutate(
      population           = NA_real_,
      population_available = FALSE
    )
}

# ASSEMBLE

context_indicators <- spine |>
  left_join(ext_buffer,     by = c("country_code", "year"),
            relationship = "one-to-one") |>  # one ext_buffer row per country-year
  left_join(wgi,            by = c("country_code", "year"),
            relationship = "one-to-one") |>  # one WGI row per country-year
  left_join(cm_context,     by = c("country_code", "year"),
            relationship = "one-to-one") |>  # one mineral profile row per country-year
  left_join(food_price_vol, by = c("country_code", "year"), relationship = "one-to-one") |>
  left_join(gdp_context,    by = c("country_code", "year"), relationship = "one-to-one") |>
  left_join(pop_context,    by = c("country_code", "year"), relationship = "one-to-one") |>
  mutate(
    across(ends_with("_available"), \(x) replace_na(x, FALSE)),
    across(ends_with("_low") | ends_with("_high") | ends_with("_weak"),
           \(x) replace_na(x, FALSE)),
    major_producer           = replace_na(major_producer, FALSE),
    cm_available             = replace_na(cm_available, FALSE),
    food_price_vol_high      = replace_na(food_price_vol_high, FALSE),
    food_price_vol_available = replace_na(food_price_vol_available, FALSE),
    gdp_available            = replace_na(gdp_available, FALSE),
    population_available     = replace_na(population_available, FALSE),
    # Fill list column NAs with empty character vectors
    dominant_minerals        = map(dominant_minerals,
                                   \(x) if (is.null(x)) character(0) else x)
  ) |>
  arrange(country_code, year)

# write_gold(context_indicators, "context_indicators", con)
write_gold(
  select(context_indicators, -dominant_minerals),
  "context_indicators",
  con
)
# -- Summary ------------------------------------------------------------------

message("--- Context layer complete ---")

recent_yr <- max(context_indicators$year, na.rm = TRUE)
ctx_recent <- filter(context_indicators, year == recent_yr)

message(sprintf("  Year %d coverage (of %d countries):", recent_yr,
                n_distinct(ctx_recent$country_code)))
message(sprintf("    reserves_months_imports : %d countries",
                sum(ctx_recent$reserves_available)))
message(sprintf("    ext_debt_to_reserves    : %d countries",
                sum(ctx_recent$ext_debt_available)))
message(sprintf("    debt_service_ratio      : %d countries",
                sum(ctx_recent$debt_service_available)))
message(sprintf("    WGI governance          : %d countries",
                sum(ctx_recent$governance_available)))
message(sprintf("    Critical mineral producers: %d countries",
                sum(ctx_recent$major_producer)))
message(sprintf("  Alert flags (%d):", recent_yr))
message(sprintf("    reserves_low (< 3m)     : %d countries",
                sum(ctx_recent$reserves_low)))
message(sprintf("    ext_debt_high (> 1x)    : %d countries",
                sum(ctx_recent$ext_debt_high)))
message(sprintf("    debt_service_high (>20%%): %d countries",
                sum(ctx_recent$debt_service_high)))
message(sprintf("    governance_weak (< -1)  : %d countries",
                sum(ctx_recent$governance_weak)))
message(sprintf("    food_price_vol_high     : %d countries",
                sum(ctx_recent$food_price_vol_high)))
message(sprintf("    gdp_available           : %d countries",
                sum(ctx_recent$gdp_available)))
message(sprintf("    population_available    : %d countries",
                sum(ctx_recent$population_available)))
