# ingest_imf.R
#
# Bronze-layer ingestion from IMF WEO DataMapper API (httr2 + JSON).
#
# -- What happened to the SDMX monthly CPI series -----------------------------
#   The IMF SDMX 3.0 CPI dataflow (api.imf.org/external/sdmx/3.0/...) returns
#   an empty dataset skeleton for all key combinations tested, the codelist
#   endpoint returns an empty body, and the all-wildcards key returns 404.
#   The endpoint appears to require authentication for data access.
#
#   Monthly CPI coverage:
#     Annual CPI (FP.CPI.TOTL.ZG) is already in bronze.wb_wdi_raw via WDI.
#     If monthly granularity is needed in a later phase, candidates are:
#       - IMF IFS via paid API key (api.imf.org with X-Api-Key header)
#       - FRED API (fred.stlouisfed.org) — free, well-documented, US + major economies
#       - World Bank monthly DDP endpoint (harder to parse, limited coverage)
#     For the current dashboard use case, annual CPI is sufficient.
#
#   bronze.imf_ifs_monthly is written as an empty placeholder so QC scripts and
#   silver transforms don't error on a missing table.
#
# -- WEO DataMapper API --------------------------------------------------------
#   Annual series with 5-year forward projections — not available in WDI.
#     - Current account % GDP  (BCA_NGDPD)
#     - General govt gross debt % GDP  (GGXWDG_NGDP)
#     - Fiscal balance % GDP  (GGXCNL_NGDP)
#     - CPI inflation % change  (PCPIPCH)  -- feeds D1 expansion and D5
#     - Real GDP growth %  (NGDP_RPCH)     -- feeds D1 expansion
#
# -- Output tables -------------------------------------------------------------
#   bronze.imf_ifs_monthly   — empty placeholder (see note above)
#   bronze.imf_weo_annual    — annual WEO series + 5yr forecasts
#
# Expects: `con` (DBI connection to econrisk.duckdb) from run_pipeline.R

library(httr2)
library(dplyr)
library(purrr)
library(DBI)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# -- Config --------------------------------------------------------------------

START_YR <- 2010
WEO_BASE <- "https://www.imf.org/external/datamapper/api/v1"

WEO_INDICATORS <- c(
  BCA_NGDPD    = "current_account_pct_gdp",
  GGXWDG_NGDP  = "govt_gross_debt_pct_gdp",
  GGXCNL_NGDP  = "fiscal_balance_pct_gdp",
  PCPIPCH      = "inflation_cpi_pct",
  NGDP_RPCH    = "gdp_growth_pct"
)

# -- Helpers -------------------------------------------------------------------

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

# -- A. Monthly CPI placeholder ------------------------------------------------
# Written so downstream QC and silver scripts don't error on a missing table.

message("--- IMF IFS monthly: writing empty placeholder (SDMX endpoint unavailable) ---")

ifs_monthly <- tibble(
  imf_country_code = character(),
  indicator_code   = character(),
  indicator_label  = character(),
  time_period_raw  = character(),
  year             = integer(),
  month            = integer(),
  value            = numeric()
)
write_bronze(ifs_monthly, "imf_ifs_monthly", con)

# -- B. IMF WEO DataMapper — annual series with forecasts ---------------------

message("--- IMF WEO DataMapper: annual series (httr2) ---")

fetch_weo <- function(indicator_code) {
  message(sprintf("  pulling WEO: %s (%s)", indicator_code, WEO_INDICATORS[indicator_code]))

  raw <- request(sprintf("%s/%s", WEO_BASE, indicator_code)) |>
    req_timeout(60) |>
    req_retry(max_tries = 3, backoff = \(i) i * 5) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  values <- raw[["values"]][[indicator_code]]

  map(names(values), \(country_code) {
    series <- values[[country_code]]
    tibble(
      imf_country_code = country_code,
      indicator_code   = indicator_code,
      indicator_label  = WEO_INDICATORS[indicator_code],
      time_period_raw  = names(series),
      year             = as.integer(names(series)),
      value            = as.double(unlist(series))
    )
  }) |>
    list_rbind() |>
    filter(!is.na(value), year >= START_YR)   # retain forecast years intentionally
}

weo_annual <- names(WEO_INDICATORS) |>
  map(fetch_weo) |>
  list_rbind()


# -- Reconcile WEO codes to iso3c ----------------------------------------------
# WEO DataMapper uses ISO3-alpha codes for most countries with a few exceptions.
# WEO_EXCEPTIONS: WEO code -> iso3c override (NA = not a sovereign state in frame)
WEO_EXCEPTIONS <- c(
  "UVK" = "XKX",          # Kosovo - WEO uses UVK; ISO uses XKX (if in frame)
  "WBG" = NA_character_,  # West Bank & Gaza - not sovereign in frame
  "CUW" = "CUW",          # Curacao - already ISO3; keep for explicitness
  # Aggregates returned by WEO (EMU, G20, etc.) will be NA after lookup - correct
  "SSD" = "SSD"           # South Sudan - should match directly; belt-and-suspenders
)

# COUNTRY_FRAME from run_pipeline.R; fallback to DB for standalone runs
if (!exists("COUNTRY_FRAME")) {
  COUNTRY_FRAME <- dbReadTable(con, DBI::Id(schema = "bronze", table = "country_frame"))
}

iso3c_lookup <- setNames(COUNTRY_FRAME$iso3c, COUNTRY_FRAME$iso3c)  # iso3c -> iso3c

weo_reconciled <- weo_annual |>
  mutate(
    iso3c = coalesce(
      iso3c_lookup[imf_country_code],       # direct match (most WEO codes = ISO3)
      WEO_EXCEPTIONS[imf_country_code]      # exception overrides
    )
  )

unresolved_weo <- weo_reconciled |>
  filter(is.na(iso3c)) |>
  distinct(imf_country_code) |>
  pull()

if (length(unresolved_weo) > 0)
  message(sprintf("  [IMF WEO] %d code(s) unresolved (aggregates/territories, dropped): %s",
                  length(unresolved_weo), paste(sort(unresolved_weo), collapse = ", ")))

weo_annual <- weo_reconciled |>
  filter(!is.na(iso3c)) |>          # keep only the 175-country spine
  relocate(iso3c, .before = imf_country_code)

write_bronze(weo_annual, "imf_weo_annual", con)

# -- Summary -------------------------------------------------------------------

message("--- IMF ingest complete ---")
message(sprintf("  WEO annual:   %d rows (%d indicators, %d countries, incl. forecasts)",
                nrow(weo_annual),
                n_distinct(weo_annual$indicator_code),
                n_distinct(weo_annual$imf_country_code)))
