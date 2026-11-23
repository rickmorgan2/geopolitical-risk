# transform_silver.R
#
# Silver-layer transforms: derived ratios computed from bronze tables.
# All ratios described as "computed in silver layer" in the risk outline
# are produced here, plus the new indicators from the gap analysis.
#
# -- Design --------------------------------------------------------------------
#   Bronze: raw source data, one row per country-year-indicator (long format)
#   Silver: analytically ready, pivoted wide, with derived columns added
#
#   This script pivots bronze.wb_wdi_raw from long to wide in R (faster and
#   more legible than DuckDB PIVOT with dot-prefixed indicator IDs), computes
#   derived metrics, and writes to the silver schema.
#
# -- Country spine -------------------------------------------------------------
#   All bronze ingest scripts now filter to the 175-country spine defined in
#   bronze.country_frame (set by run_pipeline.R from country_code_frame.csv).
#   iso3c is the universal key. IMF WEO and WTO bronze tables carry iso3c
#   resolved at ingest; the diagnostic block below verifies no residual mismatches.
#
# -- Output tables -------------------------------------------------------------
#
#   silver.macro_derived  — key risk ratios, one row per country-year
#     Debt sustainability:
#       debt_service_ratio        = DT.TDS.DECT.CD / BX.GSR.GNFS.CD        (warn > 20%)
#       ext_debt_to_reserves      = DT.DOD.DSTC.CD / FI.RES.XGLD.CD        (warn > 1.0)
#     Capital & confidence:
#       net_fdi_pct_gdp           = (FDI in - FDI out) / GDP * 100          (warn: neg 2+ yrs)
#       net_portfolio_pct_gdp     = BN.KLT.PTXL.CD / NY.GDP.MKTP.CD * 100  (hot money proxy)
#     Credit & financial fragility:
#       credit_growth_accel       = YoY change in domestic_credit_pct_gdp   (warn: rapid accel)
#       credit_growth_ma3         = 3-year moving average of annual delta    (trend smoother)
#     Tariff:
#       tariff_gap                = bound_simple_avg - mfn_applied_wtd_avg  (warn > 15pp)
#
#   silver.fdi_trend_flags  — consecutive negative FDI years, warning flag
#
# Expects: `con` (DBI connection to georisk.duckdb) from run_pipeline.R

library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# -- Helpers -------------------------------------------------------------------

write_silver <- function(df, table_name, con) {
  df <- mutate(df, transform_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS silver")
  dbWriteTable(con, DBI::Id(schema = "silver", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [silver.%s] %d rows written", table_name, nrow(df)))
}

# Returns country_code + country_name from the authoritative frame.
# Prefers the in-memory COUNTRY_FRAME set by run_pipeline.R; falls back to DB.
get_country_frame <- function(con) {
  if (exists("COUNTRY_FRAME", envir = .GlobalEnv)) {
    get("COUNTRY_FRAME", envir = .GlobalEnv) |>
      distinct(iso3c, .keep_all = TRUE) |>
      select(country_code = iso3c, country_name)
  } else {
    dbGetQuery(con,
      "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
    ) |> as_tibble()
  }
}

lag1 <- function(x) lag(x, 1)  # safe lag within group

# 3-period backward-looking moving average of the annual delta
ma3_delta <- function(x) {
  d <- x - lag(x, 1)
  (d + lag(d, 1) + lag(d, 2)) / 3
}

# Return column if it exists in df, otherwise a vector of NAs.
# Prevents errors when ingest hasn't been re-run after adding a new indicator.
col_or_na <- function(df, col) {
  if (col %in% names(df)) df[[col]] else rep(NA_real_, nrow(df))
}

# -- Pivot WDI from long to wide -----------------------------------------------

message("--- Silver: pivoting WDI indicators wide ---")

wdi_long <- dbGetQuery(con, "
  SELECT country_code, period::INTEGER AS year, indicator_id, value
  FROM bronze.wb_wdi_raw
  WHERE value IS NOT NULL
")

# One column per indicator_id; dots in names are fine in R with backtick quoting
wdi_wide <- wdi_long |>
  pivot_wider(
    id_cols     = c(country_code, year),
    names_from  = indicator_id,
    values_from = value,
    values_fn   = first   # resolve any duplicates by taking first value
  )

# -- Compute derived ratios ----------------------------------------------------

message("--- Silver: computing derived ratios ---")

macro_derived <- wdi_wide |>
  arrange(country_code, year) |>
  group_by(country_code) |>
  mutate(
    # -- Debt sustainability (Section 7) ---------------------------------------
    debt_service_ratio   = `DT.TDS.DECT.CD` / `BX.GSR.GNFS.CD`,      # warn > 20%
    ext_debt_to_reserves = `DT.DOD.DSTC.CD` / `FI.RES.XGLD.CD`,      # warn > 1.0

    # -- Capital flows (Section 8) ---------------------------------------------
    net_fdi_pct_gdp       = (`BX.KLT.DINV.CD.WD` - `BM.KLT.DINV.CD.WD`) /
                              `NY.GDP.MKTP.CD` * 100,                  # warn: neg 2+ yrs

    # -- Credit & financial fragility ------------------------------------------
    credit_growth_accel  = `FS.AST.PRVT.GD.ZS` - lag1(`FS.AST.PRVT.GD.ZS`),
    credit_growth_ma3    = ma3_delta(`FS.AST.PRVT.GD.ZS`),

    # -- Tariff gap (Section 4) ------------------------------------------------
    tariff_gap           = `TM.TAX.MRCH.BC.ZS` - `TM.TAX.MRCH.WM.AR.ZS`  # warn > 15pp
  ) |>
  ungroup() |>
  mutate(
    # Columns added after initial ingest — col_or_na() returns NA if not yet fetched
    net_portfolio_pct_gdp = col_or_na(wdi_wide, "BN.KLT.PTXL.CD") /
                              `NY.GDP.MKTP.CD` * 100,
    reer_index            = col_or_na(wdi_wide, "PX.REX.REER"),

    # -- Coverage flags for dashboard display logic ----------------------------
    # FALSE for advanced economies — WB external debt DB covers borrowing countries only
    debt_metrics_available = !is.na(debt_service_ratio),
    # TRUE for landlocked countries — LSCI is definitionally NA, show "N/A" not blank
    is_landlocked          = country_code %in% c(
      "AFG", "AND", "ARM", "AZE", "BLR", "BDI", "BTN", "BOL", "BWA",
      "BFA", "CAF", "TCD", "ETH", "HUN", "KAZ", "XKX", "KGZ", "LAO",
      "LSO", "LIE", "LUX", "MKD", "MWI", "MLI", "MDA", "MNG", "NAM",
      "NPL", "NER", "PRY", "RWA", "SMR", "SRB", "SVK", "SSD", "SWZ",
      "TJK", "TZA", "TKM", "UGA", "UZB", "VAT", "ZMB", "ZWE"
    )
  ) |>
  select(
    country_code, year,
    # derived ratios
    debt_service_ratio, ext_debt_to_reserves,
    net_fdi_pct_gdp, net_portfolio_pct_gdp,
    credit_growth_accel, credit_growth_ma3,
    tariff_gap,
    # key source columns retained to avoid re-joining in the dashboard
    gdp_usd                 = `NY.GDP.MKTP.CD`,
    gdp_growth_pct          = `NY.GDP.MKTP.KD.ZG`,
    gdp_per_capita_const    = `NY.GDP.PCAP.KD`,
    reserves_months_imports = `FI.RES.TOTL.MO`,
    domestic_credit_pct_gdp = `FS.AST.PRVT.GD.ZS`,
    inflation_cpi_pct       = `FP.CPI.TOTL.ZG`,
    current_account_pct_gdp = `BN.CAB.XOKA.GD.ZS`,
    trade_pct_gdp           = `NE.TRD.GNFS.ZS`,
    fx_rate_lcu_per_usd     = `PA.NUS.FCRF`,
    lsci                    = `IS.SHP.GCNW.XQ`,
    reer_index,                                # already named by mutate above
    # coverage flags
    debt_metrics_available,
    is_landlocked
  )

# -- Diagnostic: countries dropped by source -----------------------------------
# For each bronze table, report codes present in that source but absent from the
# authoritative frame. Run before the filter so nothing is silently discarded.
# Output goes to console; redirect to a file if you want a persistent audit log.

country_frame <- get_country_frame(con)
frame_codes   <- country_frame$country_code

report_dropped <- function(codes, source_label) {
  dropped <- sort(unique(codes[!codes %in% frame_codes & !is.na(codes)]))
  if (length(dropped) == 0) {
    message(sprintf("  %-35s all codes in frame", source_label))
  } else {
    message(sprintf("  %-35s %d dropped: %s",
                    source_label, length(dropped), paste(dropped, collapse = ", ")))
  }
}

message("--- Country frame diagnostic (codes not in established country frame) ---")

# WDI / macro_derived (already in R memory)
report_dropped(wdi_wide$country_code, "WDI (wb_wdi_raw)")

# IMF WEO: iso3c now resolved at ingest; report any that slipped through
weo_iso <- dbGetQuery(con,
  "SELECT DISTINCT iso3c FROM bronze.imf_weo_annual WHERE iso3c IS NOT NULL"
)$iso3c
report_dropped(weo_iso, "IMF WEO (imf_weo_annual)")

# Comtrade tables (use iso3c)
ct_iso <- dbGetQuery(con,
  "SELECT DISTINCT iso3c FROM bronze.comtrade_export_hhi"
)$iso3c
report_dropped(ct_iso, "Comtrade HHI (comtrade_export_hhi)")

ct_food_iso <- dbGetQuery(con,
  "SELECT DISTINCT iso3c FROM bronze.comtrade_food_net_trade"
)$iso3c
report_dropped(ct_food_iso, "Comtrade food (comtrade_food_net_trade)")

ct_exp_iso <- dbGetQuery(con,
  "SELECT DISTINCT iso3c FROM bronze.comtrade_commodity_exposure"
)$iso3c
report_dropped(ct_exp_iso, "Comtrade exposure (commodity_exposure)")

# USGS (use iso3c after crosswalk)
usgs_iso <- dbGetQuery(con,
  "SELECT DISTINCT iso3c FROM bronze.usgs_production"
)$iso3c
report_dropped(usgs_iso, "USGS production (usgs_production)")

usgs_res_iso <- dbGetQuery(con,
  "SELECT DISTINCT iso3c FROM bronze.usgs_reserves"
)$iso3c
report_dropped(usgs_res_iso, "USGS reserves (usgs_reserves)")

# WTO: iso3c now resolved at ingest via iso2c/iso3n lookups; report residual mismatches
wto_iso <- dbGetQuery(con,
  "SELECT DISTINCT iso3c FROM bronze.wto_tariff_sectors WHERE iso3c IS NOT NULL"
)$iso3c
report_dropped(wto_iso, "WTO tariff sectors")

message("--- End diagnostic ---")

# -- Filter to authoritative country frame -------------------------------------
# inner_join drops WB aggregates, territories, and any iso3c not in the frame.
# Attaches canonical country_name.

n_before <- nrow(macro_derived)

macro_derived <- macro_derived |>
  inner_join(country_frame, by = "country_code") |>
  relocate(country_code, country_name, year)

message(sprintf("  macro_derived: %d → %d rows (%d country-years dropped)",
                n_before, nrow(macro_derived), n_before - nrow(macro_derived)))

write_silver(macro_derived, "macro_derived", con)

# -- FDI trend flag ------------------------------------------------------------
# Flag countries with 2+ consecutive years of net FDI outflows (warning threshold).

fdi_trend <- macro_derived |>
  select(country_code, year, net_fdi_pct_gdp) |>
  filter(!is.na(net_fdi_pct_gdp)) |>
  arrange(country_code, year) |>
  group_by(country_code) |>
  mutate(
    fdi_negative   = net_fdi_pct_gdp < 0,
    consec_neg_yrs = {
      x   <- fdi_negative
      out <- integer(length(x))
      cnt <- 0L
      for (i in seq_along(x)) {
        cnt    <- if (!is.na(x[i]) && x[i]) cnt + 1L else 0L
        out[i] <- cnt
      }
      out
    },
    fdi_warning = consec_neg_yrs >= 2L
  ) |>
  ungroup()

write_silver(fdi_trend, "fdi_trend_flags", con)

# -- Summary -------------------------------------------------------------------

message("--- Silver transform complete ---")
message(sprintf("  macro_derived:    %d rows | %d countries | years %d-%d",
                nrow(macro_derived),
                n_distinct(macro_derived$country_code),
                min(macro_derived$year, na.rm = TRUE),
                max(macro_derived$year, na.rm = TRUE)))
message(sprintf("  fdi_trend_flags:  %d rows | %d countries currently flagged",
                nrow(fdi_trend),
                fdi_trend |> filter(year == max(year), fdi_warning) |>
                  distinct(country_code) |> nrow()))
