# ingest_worldbank.R
#
# Bronze-layer ingestion from World Bank WDI via wbstats.
#
# -- Indicator groups ----------------------------------------------------------
#
#   GROUP 1 — Macro-financial stability (Section 1 & 7 of risk outline)
#     Reserves, imports, debt, debt service — feeds FX reserve cover ratio
#     and debt sustainability ratios. FI.RES.TOTL.MO is the Section 1 metric
#     computed directly by the WB, saving a silver-layer calculation.
#
#   GROUP 2 — Trade & capital flows (Sections 2 & 8)
#     Exports, FDI flows, trade openness, LSCI
#
#   GROUP 3 — Tariff time series (Section 4)
#     MFN applied + bound rates as annual time series back to ~2000.
#     Complements the WTO Tariff Profiles snapshot (wto_tariff_sectors) by
#     showing trend in bound-applied gap — is escalation headroom growing?
#
#   GROUP 4 — Growth & real economy (dashboard context layer)
#     GDP growth, GDP per capita, capital formation
#
#   GROUP 5 — External & monetary (dashboard context layer)
#     Current account (historical; WEO adds forecasts), trade openness,
#     inflation, domestic credit, government consumption
#
# -- Batching note -------------------------------------------------------------
#   WDI API becomes unreliable above ~15 indicators per call.
#   Indicators are split into batches of 10; results are stacked long.
#
# -- Country metadata ----------------------------------------------------------
#   wb_countries() is written to bronze.wb_countries_raw.
#   This table serves as the country spine for joins across all bronze tables
#   in the silver transform layer (iso3c is the reference key).
#
# Note: WGI indicators are ingested separately in ingest_wgi.R.
#
# Expects: `con` (DBI connection to econrisk.duckdb) from run_pipeline.R

library(wbstats)
library(dplyr)
library(purrr)
library(DBI)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# -- Config --------------------------------------------------------------------

START_YR <- 2010
END_YR   <- as.integer(format(Sys.Date(), "%Y")) - 1  # last completed year

BATCH_SIZE <- 10  # max indicators per WDI API call

# Named vector: WDI indicator code -> human-readable label
# All source codes are preserved as-is in the indicator column of the output.
WDI_INDICATORS <- c(

  # -- Group 1: Macro-financial stability --------------------------------------
  FI.RES.XGLD.CD    = "reserves_excl_gold_usd",          # S1: reserves excl. gold
  FI.RES.TOTL.CD    = "total_reserves_usd",               # S1: total reserves incl. gold
  FI.RES.TOTL.MO    = "reserves_months_imports",          # S1: FX cover ratio (computed)
  BM.GSR.GNFS.CD    = "imports_goods_services_usd",       # S1: monthly import burn denominator
  DT.DOD.DSTC.CD    = "ext_debt_shortterm_usd",           # S7: short-term external debt
  DT.TDS.DECT.CD    = "debt_service_total_usd",           # S7: total debt service P+I

  # -- Group 2: Trade & capital flows ------------------------------------------
  BX.GSR.GNFS.CD    = "exports_goods_services_usd",       # S7: debt service denominator
  BX.KLT.DINV.CD.WD = "fdi_inflows_usd",                  # S8: FDI inflows
  BM.KLT.DINV.CD.WD = "fdi_outflows_usd",                 # S8: FDI outflows
  IS.SHP.GCNW.XQ    = "lsci",                             # S2: Liner Shipping Connectivity Index

  # -- Group 3: Tariff time series ----------------------------------------------
  TM.TAX.MRCH.WM.AR.ZS = "tariff_mfn_applied_wtd_avg",   # S4: MFN applied (trade-weighted)
  TM.TAX.MRCH.BC.ZS    = "tariff_bound_simple_avg",       # S4: bound rate simple avg
                                                           # gap = bound - applied (silver)

  # -- Group 4: Growth & real economy ------------------------------------------
  NY.GDP.MKTP.CD    = "gdp_usd",                          # base: nominal GDP
  NY.GDP.MKTP.KD.ZG = "gdp_growth_pct",                   # growth rate
  NY.GDP.PCAP.KD    = "gdp_per_capita_const_usd",         # per capita, constant USD
  NE.GDI.TOTL.ZS    = "capital_formation_pct_gdp",        # investment climate signal

  # -- Group 5: External, monetary & fiscal ------------------------------------
  BN.CAB.XOKA.GD.ZS = "current_account_pct_gdp",         # S8: CA balance (historical)
  NE.TRD.GNFS.ZS    = "trade_pct_gdp",                    # trade openness
  FP.CPI.TOTL.ZG    = "inflation_cpi_pct",                # S6: monetary stability
  FS.AST.PRVT.GD.ZS = "domestic_credit_private_pct_gdp",  # credit expansion risk
  NE.CON.GOVT.ZS    = "gov_consumption_pct_gdp",           # fiscal pressure signal

  # -- Group 6: Exchange rate ----------------------------------------------------
  PA.NUS.FCRF        = "fx_rate_lcu_per_usd",              # official rate, LCU per USD
  PX.REX.REER        = "reer_index",                       # real effective exchange rate index

  # -- Group 7: Portfolio flows --------------------------------------------------
  # Net portfolio investment (equity + debt combined, BoP basis).
  # Coverage is spottier than FDI — many small economies missing.
  BN.KLT.PTXL.CD     = "net_portfolio_investment_usd"      # S8: hot money proxy
)

# -- Helpers -------------------------------------------------------------------

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

# Fetch one batch; retries up to 3x with backoff, returns NULL on total failure
fetch_wdi_batch <- function(indicator_codes, start, end, max_tries = 3) {
  for (attempt in seq_len(max_tries)) {
    result <- tryCatch({
      wb_data(
        indicator   = indicator_codes,
        start_date  = start,
        end_date    = end,
        return_wide = FALSE   # long: one row per country-year-indicator
      ) |>
        mutate(indicator_label = WDI_INDICATORS[indicator_id]) |>
        select(
          country_code = iso3c,           # ISO3 — source id and join key
          country_name = country,
          indicator_id,                   # source WDI code (e.g. "NY.GDP.MKTP.CD")
          indicator_label,                # human-readable label
          period       = date,
          value
        )
    }, error = function(e) {
      message(sprintf("  batch attempt %d/%d failed: %s", attempt, max_tries, e$message))
      NULL
    })

    if (!is.null(result)) return(result)
    if (attempt < max_tries) Sys.sleep(attempt * 10)  # 10s, 20s backoff
  }
  warning(sprintf("  batch failed after %d attempts: %s",
                  max_tries, paste(indicator_codes, collapse = ", ")))
  NULL
}

# -- Fetch WDI indicators in batches ------------------------------------------

message(sprintf("--- World Bank WDI: fetching %d indicators in batches of %d ---",
                length(WDI_INDICATORS), BATCH_SIZE))

indicator_batches <- split(
  names(WDI_INDICATORS),
  ceiling(seq_along(WDI_INDICATORS) / BATCH_SIZE)
)

options(timeout = 120)  # WB API can be slow; default 60s is too short

wdi_raw <- map(indicator_batches, \(batch) {
  message(sprintf("  batch: %s", paste(batch, collapse = ", ")))
  fetch_wdi_batch(batch, START_YR, END_YR)
}) |>
  compact() |>    # drop any NULL batches (failed after retries)
  list_rbind()


# -- Reconcile WDI country codes to spine -------------------------------------
# wbstats returns ISO3c directly; also returns WB regional/income aggregates.
# Filter to the 175-country spine; log what is dropped.

if (!exists("COUNTRY_FRAME")) {
  COUNTRY_FRAME <- dbReadTable(con, DBI::Id(schema = "bronze", table = "country_frame"))
}

valid_iso3c <- COUNTRY_FRAME$iso3c

dropped_wdi <- sort(unique(wdi_raw$country_code[!wdi_raw$country_code %in% valid_iso3c]))
if (length(dropped_wdi) > 0)
  message(sprintf("  [WDI] %d non-spine codes dropped (aggregates/territories): %s",
                  length(dropped_wdi), paste(dropped_wdi, collapse = ", ")))

wdi_raw <- wdi_raw |> filter(country_code %in% valid_iso3c)

write_bronze(wdi_raw, "wb_wdi_raw", con)

# -- Fetch country metadata -----------------------------------------------------
# Country spine: iso3c is the reference key for all silver-layer joins.
# Retains all source identifier fields (iso2c, iso3c, WB country code).

message("--- World Bank: fetching country metadata ---")

wb_meta_raw <- wb_countries() |>
  filter(region != "Aggregates")      # drop regional/global aggregates

# wb_countries() column names vary by wbstats version — select defensively
wb_meta <- wb_meta_raw |>
  select(
    iso3c, iso2c, country, capital_city, longitude, latitude,
    region, income_level, lending_type,
    # iso3c codes for sub-classifications (available in current wbstats)
    any_of(c("region_iso3c", "income_level_iso3c", "lending_type_iso3c"))
  )

message(sprintf("  wb_countries() columns available: %s",
                paste(names(wb_meta_raw), collapse = ", ")))

write_bronze(wb_meta, "wb_countries_raw", con)

message("--- World Bank ingest complete ---")
message(sprintf("  WDI rows: %d across %d indicators, %d countries",
                nrow(wdi_raw),
                n_distinct(wdi_raw$indicator_id),
                n_distinct(wdi_raw$country_code)))  # renamed from iso3c in fetch
