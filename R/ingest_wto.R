# ingest_wto.R
#
# Bronze-layer ingestion for WTO tariff and institutional risk data.
# Designed for a country-focused risk dashboard: every observation
# supports two views of the user-selected country:
#
#   VIEW 1 — Country as IMPORTER  (inbound escalation risk)
#     Gap = Bound avg − MFN applied avg, by MTN product group
#     Wide gap --> legal headroom to raise tariffs; risk to companies
#     exporting TO this country
#
#   VIEW 2 — Country as EXPORTER  (export market access risk)
#     Rates this country's top partners impose on ITS goods
#     High partner MFN rates / low preferential margins → export
#     revenue at risk if trade frictions escalate
#
# -- Sources -------------------------------------------------------------------
#
#   A. WTO Tariff Profiles — Summary (3 files: all / ag / non-ag)
#      URL: stable "daily_update" endpoint, always current annual edition
#      Content: 153 countries × 22 metrics (bound/applied averages, peak
#               tariffs, duty-free shares, etc.) for three product scopes
#      Output: bronze.wto_tariff_summary  (long: 1 row per country × scope)
#
#   B. WTO Tariff Profiles — Individual country Excel files
#      URL: same stable ZIP (TariffProfiles_E.zip)
#      Filename convention: {WTO_2letter_code}_E.xlsx  (e.g. IN_E.xlsx)
#      Sheet: "English"  — fixed 63-row layout, verified against WTP 2025
#
#      PART A.2  rows 25–46  (22 MTN product groups, VIEW 1)
#        Col C: bound_avg        Col D: bound_duty_free_pct
#        Col E: bound_max        Col F: binding_coverage_pct
#        Col G: applied_avg      Col H: applied_duty_free_pct
#        Col I: applied_max      Col J: import_share_pct
#        Derived: bound_applied_gap = bound_avg − applied_avg
#        Note: applied cols are NULL for countries without recent IDB submission
#
#      PART B    rows 53–57 (ag) + 59–63 (non-ag)  (top 5 partners each, VIEW 2)
#        Col A: rank_partner_name (e.g. "1. European Union")
#        Col C: year              Col D: bilateral_imports_mn_usd
#        Col E: diversification_hs2_digit (95% trade coverage)
#        Col F: diversification_hs6_digit
#        Col G: mfn_avg_simple    Col H: mfn_avg_weighted
#        Col I: pref_margin_weighted   Col J: duty_free_imports_pct
#
#      Output: bronze.wto_tariff_sectors   (VIEW 1 — importer, by MTN group)
#               bronze.wto_export_partners  (VIEW 2 — exporter, partner rates)
#
#   C. MPIA Void Indicator  (Section 5 — Recourse Layer)
#      Binary: MPIA participation = 1, absence = 0
#      Non-participation → WTO rulings can be frozen ("appeal into the void")
#      Source: hardcoded; verify annually:
#        https://www.wto.org/english/tratop_e/dispu_e/mpia_participants_e.htm
#      Output: bronze.wto_mpia_raw
#
# -- Country identifier note ---------------------------------------------------
#   Summary files carry WTO numeric ISO codes (e.g. "008" = Albania).
#   Country profile filenames use WTO 2-letter codes (e.g. "IN" = India).
#   Both are mapped to ISO3 alpha in the silver transform via a join against
#   bronze.wb_countries_raw (built in ingest_worldbank.R).
#   Country name is extracted from row 1 of each profile file as a fallback.
#
# -- Refresh cadence -----------------------------------------------------------
#   WTO Tariff Profiles: annual release ~July. "daily_update" URLs always
#   serve the current edition. ZIPs cached locally for 30 days.
#   Delete data/raw/TariffProfiles*.zip to force a fresh download.
#
# Expects: `con` (DBI connection to econrisk.duckdb) from run_pipeline.R

library(httr2)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(DBI)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# -- Country spine lookups -----------------------------------------------------
if (!exists("COUNTRY_FRAME")) {
  COUNTRY_FRAME <- dbReadTable(con, DBI::Id(schema = "bronze", table = "country_frame"))
}

# iso3n -> iso3c: pad iso3n to 3 digits so it matches WTO zero-padded numeric codes
# e.g. iso3n=4 -> key "004" matches WTO code "004" (Afghanistan)
iso3n_lookup <- setNames(
  COUNTRY_FRAME$iso3c,
  str_pad(as.character(COUNTRY_FRAME$iso3n), 3, "left", "0")
)

# WTO numeric codes that diverge from ISO 3166-1 numeric — override them here
WTO_NUMERIC_EXCEPTIONS <- c(
  "892" = "SRB",   # Serbia: WTO uses 892; ISO 3166-1 is 688
  "893" = "MNE"    # Montenegro: WTO uses 893; ISO 3166-1 is 499
)

# iso2c -> iso3c: used by tariff_sectors / export_partners (WTO 2-letter codes)
# WTO 2-letter codes follow ISO 3166-1 alpha-2 for sovereign states.
# Known cases where WTO code differs from standard iso2c or represents non-sovereign entity:
WTO2_EXCEPTIONS <- c(
  "EU" = NA_character_,   # European Union aggregate - drop
  "GZ" = NA_character_,   # Gaza Strip - not sovereign in frame
  "TW" = NA_character_,   # Chinese Taipei - not in standard frame
  "HK" = NA_character_,   # Hong Kong SAR - not sovereign in frame
  "MO" = NA_character_,   # Macao SAR - not sovereign in frame
  "XK" = "XKX"            # Kosovo - WTO uses XK; ISO uses XKX
)
iso2c_lookup <- setNames(COUNTRY_FRAME$iso3c, COUNTRY_FRAME$iso2c)

valid_iso3c <- COUNTRY_FRAME$iso3c

# -- Config --------------------------------------------------------------------

RAW_DIR    <- "data/raw_wto"
CACHE_DAYS <- 30

WTO_SUMMARY_URL  <- paste0(
  "https://www.wto.org/english/res_e/statis_e/daily_update_e/",
  "tariff_profiles/TariffProfilesSummary_E.zip"
)
WTO_PROFILES_URL <- paste0(
  "https://www.wto.org/english/res_e/statis_e/daily_update_e/",
  "tariff_profiles/TariffProfiles_E.zip"
)

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# -- Helpers -------------------------------------------------------------------

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

cached_download <- function(url, destfile) {
  needs_dl <- !file.exists(destfile) ||
    difftime(Sys.time(), file.mtime(destfile), units = "days") > CACHE_DAYS
  if (needs_dl) {
    message(sprintf("  Downloading %s...", basename(destfile)))
    request(url) |> req_timeout(180) |> req_perform(path = destfile)
  } else {
    age <- round(as.numeric(difftime(Sys.time(), file.mtime(destfile), units = "days")))
    message(sprintf("  Using cache (%dd old): %s", age, basename(destfile)))
  }
  destfile
}

unzip_to_temp <- function(zipfile) {
  td <- tempfile(); dir.create(td)
  unzip(zipfile, exdir = td)
  list.files(td, full.names = TRUE, recursive = TRUE)
}

# "-" = unbound tariff line; "N" = no MFN applied data submitted — both -> NA
to_num <- function(x) {
  s <- str_trim(as.character(x))
  s[grepl("^-+\\s*$|^N$", s)] <- NA_character_
  suppressWarnings(as.numeric(s))
}

# -- A. Summary tariff tables --------------------------------------------------
#
# ZIP contains: Summ_all_EN_WTP25.xlsx, Summ_ag_EN_WTP25.xlsx,
#               Summ_nonag_EN_WTP25.xlsx
# Each: skip 6 header rows, then 153 country data rows, 22 positional columns.
# Column map verified against WTP 2025 (see header comment block above).

message("--- WTO Tariff Profiles: summary tables ---")

summary_zip   <- cached_download(WTO_SUMMARY_URL,
                                 file.path(RAW_DIR, "TariffProfilesSummary_E.zip"))
summary_files <- unzip_to_temp(summary_zip) |>
  keep(\(f) str_detect(f, regex("summ.*\\.xlsx?$", ignore_case = TRUE)))

scope_from_path <- function(path) {
  case_when(
    str_detect(path, regex("_all_",   ignore_case = TRUE)) ~ "all",
    str_detect(path, regex("_ag_",    ignore_case = TRUE)) ~ "ag",
    str_detect(path, regex("_nonag_", ignore_case = TRUE)) ~ "nonag",
    TRUE ~ "unknown"
  )
}

parse_summary_file <- function(filepath) {
  scope <- scope_from_path(filepath)

  raw <- read_excel(filepath, sheet = "Sheet1",
                    skip = 6, col_names = FALSE) |>
    select(1:22)  # drop duplicate name + flag cols beyond col 22

  names(raw) <- c(
    "wto_numeric_code",        "reporter_name",
    "year_mfn_applied",        "binding_coverage_pct",
    "bound_simple_avg",        "applied_simple_avg",
    "duty_free_bound_pct",     "duty_free_applied_pct",
    "nonadval_bound_pct",      "nonadval_applied_pct",
    "duties_gt15_bound_pct",   "duties_gt15_applied_pct",
    "duties_gt3avg_bound_pct", "duties_gt3avg_applied_pct",
    "concessions_not_impl_pct",
    "max_duty_bound",          "max_duty_applied",
    "distinct_rates_bound",    "distinct_rates_applied",
    "coeff_var_bound",         "coeff_var_applied",
    "n_mfn_lines"
  )

  raw |>
    filter(!is.na(wto_numeric_code)) |>
    mutate(
      product_scope    = scope,
      reporter_name    = str_trim(reporter_name),
      wto_numeric_code = str_pad(str_trim(as.character(wto_numeric_code)), 3, "left", "0"),
      across(c(binding_coverage_pct:n_mfn_lines), to_num),
      bound_applied_gap = bound_simple_avg - applied_simple_avg  # escalation headroom
    ) |>
    select(product_scope, wto_numeric_code, reporter_name, year_mfn_applied, everything())
}

tariff_summary <- map(summary_files, parse_summary_file) |> bind_rows()

# Resolve wto_numeric_code -> iso3c via padded iso3n; exceptions for non-standard codes
tariff_summary <- tariff_summary |>
  mutate(
    iso3c = coalesce(
      iso3n_lookup[wto_numeric_code],          # padded ISO numeric match
      WTO_NUMERIC_EXCEPTIONS[wto_numeric_code] # WTO-specific overrides (Serbia, Montenegro)
    )
  )

unresolved_sum <- tariff_summary |>
  filter(is.na(iso3c)) |>
  distinct(wto_numeric_code, reporter_name) |>
  mutate(label = paste0(wto_numeric_code, " (", reporter_name, ")")) |>
  pull(label)
if (length(unresolved_sum) > 0)
  message(sprintf("  [WTO tariff_summary] %d code(s) unresolved (dropped): %s",
                  length(unresolved_sum), paste(sort(unresolved_sum), collapse = ", ")))

tariff_summary <- tariff_summary |>
  filter(!is.na(iso3c)) |>
  relocate(iso3c, .after = wto_numeric_code)

write_bronze(tariff_summary, "wto_tariff_summary", con)

# -- B. Individual country profiles -------------------------------------------
#
# Layout is fixed across all ~170 country files (verified WTP 2025 edition).
# Sheet: "English". File naming: {WTO_2letter_code}_E.xlsx
# All row references below are 1-indexed (as read by read_excel).
#
# PART A.2 — product groups (rows 25–46, cols A–J)
# PART B   — export partners:
#              ag section:    label row 52,  data rows 53–57
#              non-ag section:label row 58,  data rows 59–63
#
# Countries without a recent MFN applied tariff submission (e.g. KN = Saint
# Kitts and Nevis) will have NULL applied cols — parsed as NA, kept in output.

message("--- WTO Tariff Profiles: individual country files ---")

profiles_zip  <- cached_download(WTO_PROFILES_URL,
                                 file.path(RAW_DIR, "TariffProfiles_E.zip"))
profile_xlsx  <- unzip_to_temp(profiles_zip) |>
  keep(\(f) str_detect(f, regex("_e\\.xlsx?$", ignore_case = TRUE)))

message(sprintf("  Parsing %d country files...", length(profile_xlsx)))

parse_country_profile <- function(filepath) {
  # WTO 2-letter code from filename (e.g. "IN_E.xlsx" -> "IN")
  wto2 <- str_extract(basename(filepath), "^[A-Z]{2}")
  if (is.na(wto2)) return(NULL)

  raw <- tryCatch(
    suppressMessages(
      read_excel(filepath, sheet = "English", col_names = FALSE)
    ),
    error = \(e) { warning(sprintf("%s: %s", basename(filepath), e$message)); NULL }
  )
  if (is.null(raw) || nrow(raw) < 63) return(NULL)

  # Country name from row 1, col 1
  country_name <- str_trim(as.character(raw[[1, 1]]))

  # -- PART A.2: product groups (rows 25–46) ----------------------------------
  # 22 rows, one per MTN product group. Applied cols are NA for non-reporters.
  sectors <- raw[25:46, c(1, 3:10)] |>
    setNames(c(
      "mtn_group",
      "bound_avg",
      "bound_duty_free_pct",
      "bound_max",
      "binding_coverage_pct",
      "applied_avg",
      "applied_duty_free_pct",
      "applied_max",
      "import_share_pct"
    )) |>
    mutate(
      wto2_code    = wto2,
      country_name = country_name,
      mtn_group    = str_trim(as.character(mtn_group)),
      across(c(bound_avg:import_share_pct), to_num),
      bound_applied_gap = bound_avg - applied_avg   # core escalation metric
    ) |>
    filter(!is.na(mtn_group), str_length(mtn_group) > 0) |>
    select(wto2_code, country_name, mtn_group, everything())

  # -- PART B: export partners (ag rows 53–57, non-ag rows 59–63) -------------
  # 5 partners per product scope. Partner name includes rank prefix ("1. EU").
  parse_partner_block <- function(rows, product_scope) {
    raw[rows, c(1, 3:10)] |>
      setNames(c(
        "rank_partner_raw",
        "year",
        "bilateral_imports_mn_usd",
        "diversification_hs2",      # no. of 2-digit HS covering 95% of trade
        "diversification_hs6",      # no. of 6-digit HS covering 95% of trade
        "mfn_avg_simple",           # avg MFN rate partner charges on this country
        "mfn_avg_weighted",
        "pref_margin_weighted",     # weighted preferential margin (MFN - pref rate)
        "duty_free_imports_pct"     # % of imports entering duty-free
      )) |>
      mutate(
        wto2_code     = wto2,
        country_name  = country_name,
        product_scope = product_scope,
        # strip rank number: "1. European Union" -> "European Union"
        partner_name  = str_trim(str_remove(as.character(rank_partner_raw), "^\\d+\\.\\s*")),
        across(c(year:duty_free_imports_pct), to_num)
      ) |>
      filter(!is.na(partner_name), str_length(partner_name) > 0) |>
      select(wto2_code, country_name, product_scope, partner_name, everything(),
             -rank_partner_raw)
  }

  partners <- bind_rows(
    parse_partner_block(53:57, "ag"),
    parse_partner_block(59:63, "nonag")
  )

  list(sectors = sectors, partners = partners)
}

parsed          <- map(profile_xlsx, parse_country_profile) |> compact()
tariff_sectors  <- map(parsed, "sectors")  |> compact() |> bind_rows()
export_partners <- map(parsed, "partners") |> compact() |> bind_rows()

# Resolve wto2_code -> iso3c via iso2c; exceptions for non-sovereign/aggregate codes
add_iso3c_wto2 <- function(df, label) {
  df <- df |>
    mutate(
      iso3c = coalesce(
        iso2c_lookup[wto2_code],      # primary: ISO2 match
        WTO2_EXCEPTIONS[wto2_code]    # exception overrides
      )
    )
  unresolved <- df |>
    filter(is.na(iso3c)) |>
    distinct(wto2_code) |>
    pull()
  if (length(unresolved) > 0)
    message(sprintf("  [%s] %d code(s) unresolved (dropped): %s",
                    label, length(unresolved), paste(sort(unresolved), collapse = ", ")))
  df |>
    filter(!is.na(iso3c)) |>
    relocate(iso3c, .after = wto2_code)
}

tariff_sectors  <- add_iso3c_wto2(tariff_sectors,  "WTO tariff_sectors")
export_partners <- add_iso3c_wto2(export_partners, "WTO export_partners")

write_bronze(tariff_sectors,  "wto_tariff_sectors",  con)
write_bronze(export_partners, "wto_export_partners", con)

# -- C. MPIA Void Indicator ----------------------------------------------------
# Last verified: January 2025. EU expanded to 27 individual ISO-3 member codes.

message("--- WTO MPIA: void indicator ---")

MPIA_NON_EU <- c(
  "AUS", "BRA", "CAN", "CHL", "CHN", "COL", "CRI", "ECU",
  "GTM", "HKG", "ISL", "IDN", "KAZ", "KOR", "LIE", "MAC",
  "MEX", "MDA", "MNE", "NZL", "NIC", "NGA", "MKD", "NOR",
  "PAK", "PER", "PHL", "SGP", "CHE", "THA", "UKR", "URY"
)
MPIA_EU <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
  "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
  "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
  "SVN", "ESP", "SWE"
)

mpia_raw <- tibble(
  iso3        = c(MPIA_NON_EU, MPIA_EU),
  mpia_member = 1L,
  eu_bloc     = c(rep(0L, length(MPIA_NON_EU)), rep(1L, length(MPIA_EU))),
  as_of_year  = 2025L
) |>
  rename(iso3c = iso3) |>
  filter(iso3c %in% valid_iso3c)

write_bronze(mpia_raw, "wto_mpia_raw", con)

message("--- WTO ingest complete ---")
message(sprintf("  Summary rows:       %d (%d countries × 3 scopes)",
                nrow(tariff_summary), nrow(tariff_summary) / 3))
message(sprintf("  Sector rows (V1):   %d across %d countries",
                nrow(tariff_sectors), n_distinct(tariff_sectors$wto2_code)))
message(sprintf("  Partner rows (V2):  %d across %d countries",
                nrow(export_partners), n_distinct(export_partners$wto2_code)))
message(sprintf("  MPIA members:       %d (%d EU)", nrow(mpia_raw), length(MPIA_EU)))
