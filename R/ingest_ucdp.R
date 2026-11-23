# ingest_ucdp.R
# ------------------------------------------------------------------------------
# Outcome data for the conflict forecasts (D-018): UCDP organized violence per
# country-month and violence type.
#
# Sources -- public downloads (the UCDP API now requires an access token):
#   GED, final, annual        downloads/ged/ged{XY}-csv.zip      (UCDP_GED_VERSION)
#   Candidate, monthly        downloads/candidateged/GEDEvent_v{YY}_0_{M}.csv
#     Release v{YY}_0_{M} holds the events dated in month M of 20YY (~98%) plus a
#     few late additions for earlier months, published early the next month.
#     Releases from UCDP_CANDIDATE_FIRST_YEAR on are kept: they rebuild what was
#     known at each point in time for backtests (D-021).
#   Candidate, year to date   downloads/candidateged/GEDEvent_v{YY}_01_{YY}_{MM}.csv
#     UCDP's revised state of a year after GED's last year; it adds and drops
#     events relative to the monthly releases.
#
# Outputs:
#   bronze.ucdp_ged_events          final GED events (selected columns)
#   bronze.ucdp_candidate_events    every monthly Candidate release, stacked and labeled
#   bronze.ucdp_candidate_current   the revised year-to-date Candidate files
#   silver.ucdp_country_month       fatalities per country, month and violence type
#                                   (sb = state-based, ns = non-state, os = one-sided),
#                                   zero-filled; final GED through its last year, the
#                                   latest Candidate state after that
#
# Conventions: an event counts in the month of its date_start. Countries map from
# UCDP's Gleditsch-Ward country_id to iso3c through bronze.country_frame (gwcode).
# Expects: `con` from run_pipeline.R and the UCDP_* values in config.R.
# ------------------------------------------------------------------------------

library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# -- Config --------------------------------------------------------------------

if (!exists("UCDP_RAW_DIR"))              UCDP_RAW_DIR              <- "data/raw_ucdp"
if (!exists("UCDP_GED_VERSION"))          UCDP_GED_VERSION          <- "26.1"
if (!exists("UCDP_CANDIDATE_FIRST_YEAR")) UCDP_CANDIDATE_FIRST_YEAR <- 2021L

UCDP_BASE_URL <- "https://ucdp.uu.se/downloads"
UCDP_TYPES    <- c("1" = "sb", "2" = "ns", "3" = "os")

# Column order of GED / Candidate files (some Candidate releases ship without
# a header row; v24.0.1 is one)
UCDP_COLUMNS <- c(
  "id", "relid", "year", "active_year", "code_status", "type_of_violence",
  "conflict_dset_id", "conflict_new_id", "conflict_name", "dyad_dset_id",
  "dyad_new_id", "dyad_name", "side_a_dset_id", "side_a_new_id", "side_a",
  "side_b_dset_id", "side_b_new_id", "side_b", "number_of_sources",
  "source_article", "source_office", "source_date", "source_headline",
  "source_original", "where_prec", "where_coordinates", "where_description",
  "adm_1", "adm_2", "latitude", "longitude", "geom_wkt", "priogrid_gid",
  "country", "country_id", "region", "event_clarity", "date_prec",
  "date_start", "date_end", "deaths_a", "deaths_b", "deaths_civilians",
  "deaths_unknown", "best", "high", "low", "gwnoa", "gwnob"
)

KEEP_COLUMNS <- c(
  "id", "relid", "year", "code_status", "type_of_violence", "conflict_new_id",
  "dyad_new_id", "side_a_new_id", "side_a", "side_b_new_id", "side_b",
  "country", "country_id", "region", "event_clarity", "date_prec",
  "date_start", "date_end", "where_prec", "priogrid_gid", "latitude",
  "longitude", "deaths_a", "deaths_b", "deaths_civilians", "deaths_unknown",
  "best", "low", "high"
)

INT_COLUMNS <- c(
  "year", "type_of_violence", "conflict_new_id", "dyad_new_id", "side_a_new_id",
  "side_b_new_id", "country_id", "event_clarity", "date_prec", "where_prec",
  "priogrid_gid", "deaths_a", "deaths_b", "deaths_civilians", "deaths_unknown",
  "best", "low", "high"
)

# -- File names and downloads --------------------------------------------------

ged_zip_name <- function(version) {
  sprintf("ged%s-csv.zip", gsub(".", "", version, fixed = TRUE))
}

candidate_release_name <- function(year, month) {
  sprintf("GEDEvent_v%02d_0_%d.csv", year %% 100, month)
}

candidate_ytd_name <- function(year, month) {
  sprintf("GEDEvent_v%02d_01_%02d_%02d.csv", year %% 100, year %% 100, month)
}

# Download once; a release is immutable once published. Returns TRUE when the
# file is present afterwards. Missing releases (HTTP 404) return FALSE.
download_if_missing <- function(url, dest) {
  if (file.exists(dest) && file.size(dest) > 0) return(TRUE)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  part <- paste0(dest, ".part")
  ok <- tryCatch({
    utils::download.file(url, part, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (ok && file.exists(part) && file.size(part) > 0) {
    if (!file.rename(part, dest)) file.copy(part, dest, overwrite = TRUE)
    unlink(part)
    return(TRUE)
  }
  unlink(part)
  FALSE
}

# -- Reading and standardizing -------------------------------------------------

# Read a GED or Candidate CSV (with or without a header row) to KEEP_COLUMNS,
# typed. Columns a file lacks (final GED has no code_status) come back NA.
read_ucdp_csv <- function(path) {
  first      <- readLines(path, n = 1, warn = FALSE)
  has_header <- grepl("relid", first, fixed = TRUE)
  if (has_header) {
    # Only the kept columns: the final GED file is ~275 MB
    raw <- read_csv(path, col_select = any_of(KEEP_COLUMNS),
                    col_types = cols(.default = "c"), show_col_types = FALSE,
                    progress = FALSE)
  } else {
    raw <- read_csv(path, col_names = UCDP_COLUMNS,
                    col_types = cols(.default = "c"), show_col_types = FALSE,
                    progress = FALSE)
    if (ncol(raw) != length(UCDP_COLUMNS))
      stop(sprintf("%s: no header and %d columns (expected %d)",
                   basename(path), ncol(raw), length(UCDP_COLUMNS)))
  }
  standardize_events(raw)
}

standardize_events <- function(raw) {
  for (col in setdiff(KEEP_COLUMNS, names(raw))) raw[[col]] <- NA_character_
  raw[KEEP_COLUMNS] |>
    mutate(
      across(all_of(INT_COLUMNS), \(x) suppressWarnings(as.integer(x))),
      across(c(latitude, longitude), \(x) suppressWarnings(as.numeric(x))),
      date_start = as.Date(substr(date_start, 1, 10)),
      date_end   = as.Date(substr(date_end, 1, 10))
    )
}

# UCDP country_id is a Gleditsch-Ward code. Map through the pipeline's frame
# first, then countrycode's "gwn" table; anything left stays NA.
map_countries <- function(events, frame) {
  frame <- frame |>
    transmute(country_id = as.integer(gwcode), country_code = iso3c) |>
    filter(!is.na(country_id)) |>
    distinct(country_id, .keep_all = TRUE)
  out <- left_join(events, frame, by = "country_id")
  missing <- is.na(out$country_code) & !is.na(out$country_id)
  if (any(missing) && requireNamespace("countrycode", quietly = TRUE))
    out$country_code[missing] <- suppressWarnings(countrycode::countrycode(
      out$country_id[missing], origin = "gwn", destination = "iso3c"))
  out
}

# -- Candidate state -----------------------------------------------------------

# What UCDP's Candidate data say now about the months after GED's last year:
# inside a year-to-date file's window that file is the state (it drops events
# the monthly releases still carry); elsewhere, the monthly releases, keeping
# each event's latest release.
latest_candidate_state <- function(releases, ytd) {
  in_ytd_window <- function(ev) {
    if (is.null(ytd) || nrow(ytd) == 0) return(rep(FALSE, nrow(ev)))
    win <- ytd |> distinct(window_year, window_month)
    ym  <- as.integer(format(ev$date_start, "%Y")) * 100L +
           as.integer(format(ev$date_start, "%m"))
    covered <- unlist(map2(win$window_year, win$window_month,
                           \(y, m) y * 100L + seq_len(m)))
    ym %in% covered
  }
  from_releases <- releases |>
    arrange(id, desc(release_year * 100L + release_month)) |>
    distinct(id, .keep_all = TRUE)
  from_releases <- from_releases[!in_ytd_window(from_releases), , drop = FALSE]
  bind_rows(from_releases, ytd)
}

# -- Country-month aggregation -------------------------------------------------

# Fatalities per country, month and violence type between two months
# (inclusive), zero-filled for every country in `countries`.
aggregate_country_month <- function(events, countries, first_month, last_month,
                                    source, source_version) {
  months <- seq(as.Date(first_month), as.Date(last_month), by = "month")
  agg <- events |>
    filter(!is.na(country_code), country_code %in% countries,
           !is.na(date_start), date_start >= min(months),
           date_start < seq(max(months), by = "month", length.out = 2)[2]) |>
    mutate(year  = as.integer(format(date_start, "%Y")),
           month = as.integer(format(date_start, "%m")),
           type  = unname(UCDP_TYPES[as.character(type_of_violence)])) |>
    filter(!is.na(type)) |>
    group_by(country_code, year, month, type) |>
    summarise(fatalities_best = sum(best, na.rm = TRUE),
              fatalities_low  = sum(low,  na.rm = TRUE),
              fatalities_high = sum(high, na.rm = TRUE),
              n_events        = n(), .groups = "drop")
  expand_grid(country_code = sort(unique(countries)), date = months,
              type = unname(UCDP_TYPES)) |>
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(format(date, "%m"))) |>
    select(-date) |>
    left_join(agg, by = c("country_code", "year", "month", "type")) |>
    mutate(across(c(fatalities_best, fatalities_low, fatalities_high, n_events),
                  \(x) coalesce(as.integer(x), 0L)),
           source = source, source_version = source_version)
}

# -- Writers -------------------------------------------------------------------

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name), df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

write_silver <- function(df, table_name, con) {
  df <- mutate(df, transform_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS silver")
  dbWriteTable(con, DBI::Id(schema = "silver", table = table_name), df, overwrite = TRUE)
  message(sprintf("  [silver.%s] %d rows written", table_name, nrow(df)))
}

# -- Run -----------------------------------------------------------------------

run_ucdp_ingest <- function(con, today = Sys.Date()) {
  message("--- UCDP: final GED ", UCDP_GED_VERSION, " ---")
  ged_dir <- file.path(UCDP_RAW_DIR, "ged")
  zip     <- file.path(ged_dir, ged_zip_name(UCDP_GED_VERSION))
  if (!download_if_missing(file.path(UCDP_BASE_URL, "ged", basename(zip)), zip))
    stop("Could not download ", basename(zip), "; check UCDP_GED_VERSION")
  csv_in_zip <- grep("\\.csv$", utils::unzip(zip, list = TRUE)$Name, value = TRUE)[1]
  if (!file.exists(file.path(ged_dir, csv_in_zip)))
    utils::unzip(zip, files = csv_in_zip, exdir = ged_dir)
  frame <- dbGetQuery(con, "SELECT gwcode, iso3c FROM bronze.country_frame")
  ged   <- read_ucdp_csv(file.path(ged_dir, csv_in_zip)) |> map_countries(frame)
  ged_last_year <- max(ged$year, na.rm = TRUE)
  message(sprintf("  %s events, %d-%d", format(nrow(ged), big.mark = ","),
                  min(ged$year, na.rm = TRUE), ged_last_year))

  message("--- UCDP: monthly Candidate releases ---")
  cand_dir  <- file.path(UCDP_RAW_DIR, "candidate")
  this_year <- as.integer(format(today, "%Y"))
  this_mon  <- as.integer(format(today, "%m"))
  wanted <- expand_grid(release_year = UCDP_CANDIDATE_FIRST_YEAR:this_year,
                        release_month = 1:12) |>
    filter(release_year * 100L + release_month < this_year * 100L + this_mon)
  releases <- pmap(wanted, \(release_year, release_month) {
    f <- candidate_release_name(release_year, release_month)
    if (!download_if_missing(file.path(UCDP_BASE_URL, "candidateged", f),
                             file.path(cand_dir, f))) return(NULL)
    read_ucdp_csv(file.path(cand_dir, f)) |>
      mutate(release = sprintf("%d.0.%d", release_year %% 100, release_month),
             release_year = release_year, release_month = release_month)
  }) |> compact() |> list_rbind()
  if (is.null(releases) || nrow(releases) == 0) stop("No Candidate releases could be read")
  releases <- map_countries(releases, frame)
  latest_release <- releases$release[which.max(releases$release_year * 100L + releases$release_month)]
  message(sprintf("  %d releases (latest %s), %s events", n_distinct(releases$release),
                  latest_release, format(nrow(releases), big.mark = ",")))

  message("--- UCDP: year-to-date Candidate files after GED ---")
  ytd <- map((ged_last_year + 1L):this_year, \(y) {
    last_m <- if (y == this_year) this_mon - 1L else 12L
    for (m in rev(seq_len(max(0L, last_m)))) {
      f <- candidate_ytd_name(y, m)
      if (download_if_missing(file.path(UCDP_BASE_URL, "candidateged", f),
                              file.path(cand_dir, f)))
        return(read_ucdp_csv(file.path(cand_dir, f)) |>
                 mutate(window_year = y, window_month = m, file = f))
    }
    NULL
  }) |> compact() |> list_rbind()
  if (is.null(ytd)) ytd <- tibble()
  if (nrow(ytd) > 0) ytd <- map_countries(ytd, frame)
  message(sprintf("  %s", if (nrow(ytd) == 0) "none" else paste(unique(ytd$file), collapse = ", ")))

  write_bronze(ged,      "ucdp_ged_events",       con)
  write_bronze(releases, "ucdp_candidate_events", con)
  write_bronze(ytd,      "ucdp_candidate_current", con)

  message("--- UCDP: country-month fatalities ---")
  countries <- frame$iso3c
  cm_ged <- aggregate_country_month(
    ged, countries, "1989-01-01", sprintf("%d-12-01", ged_last_year),
    source = "ged", source_version = UCDP_GED_VERSION)

  state <- latest_candidate_state(
    releases |> filter(date_start > as.Date(sprintf("%d-12-31", ged_last_year))),
    if (nrow(ytd) > 0) ytd else NULL)
  cm_cand <- NULL
  if (nrow(state) > 0) {
    last_month <- max(state$date_start, na.rm = TRUE)
    cm_cand <- aggregate_country_month(
      state, countries, sprintf("%d-01-01", ged_last_year + 1L),
      format(last_month, "%Y-%m-01"), source = "candidate",
      source_version = paste(c(unique(ytd$file), latest_release), collapse = " + "))
  }
  cm <- bind_rows(cm_ged, cm_cand)
  write_silver(cm, "ucdp_country_month", con)

  unmatched <- bind_rows(ged, state) |> filter(is.na(country_code)) |>
    count(country_id, country, wt = best, name = "fatalities")
  if (nrow(unmatched) > 0) {
    message("  Events outside the country frame (not counted):")
    print(unmatched, n = 20)
  }
  totals <- cm |> filter(year >= ged_last_year - 2) |>
    group_by(year, type, source) |>
    summarise(fatalities = sum(fatalities_best), .groups = "drop") |>
    pivot_wider(names_from = type, values_from = fatalities)
  message("  Fatalities (best estimate) by year and type, recent years:")
  print(totals)
  invisible(cm)
}

run_ucdp_ingest(con)
