# ingest_wgi.R
#
# Bronze-layer ingestion: World Bank World Governance Indicators (WGI)
#
# Source: WGI dataset Excel file downloaded from the World Bank.
#   https://www.worldbank.org/en/research/brief/world-governance-indicators
#   File: data/raw_wgi/wgidataset_with_sourcedata-2025.xlsx
#
# The WB API (wbstats) does not yet carry 2024 WGI data. This script reads
# directly from the downloaded Excel file until the API catches up.
#
# -- Sheet structure ----------------------------------------------------------
#   One sheet per WGI dimension: va, pv, ge, rq, rl, cc
#   Row 1: header
#   Columns: ID | country_name | country_code | region | income |
#            year | dimension | n_sources | estimate | std_error |
#            lower_ci | upper_ci
#
# -- Indicators ingested -------------------------------------------------------
#   rq  Regulatory quality        [-2.5, +2.5]; higher = better
#   cc  Control of corruption     [-2.5, +2.5]; higher = less corrupt
#
#   Other available dimensions (not currently ingested):
#   va  Voice and accountability
#   pv  Political stability / absence of violence
#   ge  Government effectiveness
#   rl  Rule of law
#
# -- Country code notes --------------------------------------------------------
#   WGI uses WB codes which are mostly ISO3 with two exceptions in the frame:
#   ADO (Andorra) -> AND
#   ANT (Netherlands Antilles, former) -> not in frame, dropped
#
# -- Output -------------------------------------------------------------------
#   bronze.wb_wgi_raw   long format, one row per country-year-indicator
#                       same schema as bronze.wb_wdi_raw for consistent joins
#
# Expects: `con` (DBI connection to econrisk.duckdb) from run_pipeline.R

library(DBI)
library(dplyr)
library(purrr)
library(readxl)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# -- Config --------------------------------------------------------------------

WGI_PATH <- "data/raw_wgi/wgidataset_with_sourcedata-2025.xlsx"
START_YR  <- 2010L

# Dimensions to ingest — add others from the comment above when needed
WGI_DIMS <- c(
  rq = "regulatory_quality",
  cc = "control_of_corruption"
)

# WB codes that differ from ISO3 within our 175-country frame
WGI_CODE_EXCEPTIONS <- c(
  ADO = "AND"   # Andorra: WGI uses ADO, ISO3 is AND
  # ANT (Netherlands Antilles) is defunct — not in frame, drops naturally
)

# -- Helpers -------------------------------------------------------------------

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

# Read one WGI sheet and return a tidy long tibble
read_wgi_sheet <- function(path, sheet, indicator_label) {
  read_excel(path, sheet = sheet, col_names = TRUE) |>
    select(
      country_name = 2,
      country_code = 3,   # WB code — reconciled below
      year         = 6,
      indicator_id = 7,   # dimension shortcode: "rq", "cc", etc.
      estimate     = 9,
      std_error    = 10,
      lower_ci     = 11,
      upper_ci     = 12
    ) |>
    mutate(
      year             = as.integer(year),
      indicator_label  = indicator_label,
      estimate         = as.double(estimate),
      std_error        = as.double(std_error),
      lower_ci         = as.double(lower_ci),
      upper_ci         = as.double(upper_ci)
    ) |>
    filter(!is.na(estimate), year >= START_YR)
}

# -- Ingest --------------------------------------------------------------------

message("--- WGI: reading from Excel ---")
message(sprintf("  File: %s", WGI_PATH))

if (!file.exists(WGI_PATH)) {
  stop(sprintf(
    "WGI Excel file not found at '%s'. ",
    "Download from https://www.worldbank.org/en/research/brief/world-governance-indicators ",
    "and place at that path.", WGI_PATH
  ))
}

wgi_raw <- imap(WGI_DIMS, \(label, sheet) {
  message(sprintf("  reading sheet: %s (%s)", sheet, label))
  read_wgi_sheet(WGI_PATH, sheet, label)
}) |>
  list_rbind()

# -- Reconcile country codes to ISO3 ------------------------------------------

if (!exists("COUNTRY_FRAME")) {
  COUNTRY_FRAME <- dbReadTable(con, DBI::Id(schema = "bronze", table = "country_frame"))
}

valid_iso3c <- COUNTRY_FRAME$iso3c

wgi_raw <- wgi_raw |>
  mutate(
    country_code = coalesce(WGI_CODE_EXCEPTIONS[country_code], country_code)
  )

dropped <- sort(unique(wgi_raw$country_code[!wgi_raw$country_code %in% valid_iso3c]))
if (length(dropped) > 0)
  message(sprintf("  [WGI] %d codes not in frame (territories/aggregates, dropped): %s",
                  length(dropped), paste(dropped, collapse = ", ")))

wgi_raw <- wgi_raw |>
  filter(country_code %in% valid_iso3c) |>
  # Reorder to match wb_wdi_raw schema for consistent downstream joins
  select(
    country_code,
    country_name,
    indicator_id,
    indicator_label,
    period  = year,
    value   = estimate,
    std_error,
    lower_ci,
    upper_ci
  )

write_bronze(wgi_raw, "wb_wgi_raw", con)

# -- Summary -------------------------------------------------------------------

message("--- WGI ingest complete ---")
message(sprintf("  %d rows | %d indicators | %d countries | years %d-%d",
                nrow(wgi_raw),
                n_distinct(wgi_raw$indicator_id),
                n_distinct(wgi_raw$country_code),
                min(wgi_raw$period, na.rm = TRUE),
                max(wgi_raw$period, na.rm = TRUE)))

for (dim in names(WGI_DIMS)) {
  n_countries_latest <- wgi_raw |>
    filter(indicator_id == dim) |>
    filter(period == max(period)) |>
    nrow()
  message(sprintf("  %s (%s): %d countries in latest year (%d)",
                  dim, WGI_DIMS[dim], n_countries_latest,
                  max(wgi_raw$period[wgi_raw$indicator_id == dim])))
}
