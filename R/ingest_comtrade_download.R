# ingest_comtrade_download.R
# ------------------------------------------------------------------------------
# Step 0a: Download monthly bilateral trade CSVs from the UN Comtrade API.
#
# Collects trade data for 174 HS commodity codes (WB Pink Sheet + US critical
# minerals) across 173 countries, one country-year at a time.
#
# Output: one CSV per country-year in COMTRADE_CSV_DIR (e.g. USA_2024.csv), and
# a download manifest (COMTRADE_MANIFEST) with one row per country-year:
# status (ok / no_data / error / interrupted), rows, months covered, API calls,
# timestamp. The reporter's World totals go to COMTRADE_WORLD_DIR (same file
# names), so partner coverage can be measured (D-014).
#
# Run ingest_comtrade_build.R afterward to convert CSVs to parquet.
#
# -- Usage --------------------------------------------------------------------
# Sourcing this file only defines functions. The full update, step by step in
# the agreed order, is run_comtrade_update.R at the repo root. Single calls:
#
#   run_comtrade_download(2026, countries = "AFG")     # smoke test: 2 calls
#   run_comtrade_download(2026)                        # one year, all countries
#   run_comtrade_download(2017)                        # fills only missing/failed
#   run_comtrade_download(2024, retry_no_data = TRUE)  # re-ask non-reporters
#   run_comtrade_download(2018, countries = "THA",     # re-download what was
#                         refresh_before = "2026-11-21") #  fetched before a date
#   run_comtrade_download(2026, dry_run = TRUE)        # show the plan, no calls
#
#   run_comtrade_world_download(2010:2023)             # World totals only, for
#                                                      #  files that lack them
#   check_world_totals("PHL", 2026)                    # one-off check, 2 calls
#
# -- Why partner = "everything" -----------------------------------------------
# Listing all ~172 partners explicitly made every request URL ~2,000
# characters, and Comtrade now rejects that with HTTP 414 (URI Too Long).
# Requests now leave the partner list out (~890 characters) and the partner
# set is applied after download: the same set as earlier years (country frame
# minus the reporter), which also drops World and aggregate areas. If a 414
# still occurs, or a call reaches the 100k-row cap, the commodity chunk is
# halved and retried.
#
# -- Resume and refresh -------------------------------------------------------
# The manifest, not file size, decides what to download:
#   never attempted, or status "error"  -> download
#   "ok" / "no_data" in a closed year   -> skip ("no_data" is Comtrade's valid
#                                          answer for a non-reporter)
#   "ok" / "no_data" in a recent year   -> re-download once older than
#                                          REFRESH_AFTER_DAYS, because Comtrade
#                                          keeps adding months and revising
# The first run builds the manifest from the CSVs already on disk; nothing is
# deleted. A country-year file is written only when every call for it
# succeeds (temp file + rename), so partial or header-less files can't occur.
#
# -- World totals ---------------------------------------------------------------
# A country-year download keeps the World rows that come with it. Files
# downloaded before that existed get theirs from run_comtrade_world_download():
# partner = World only, up to WORLD_REPORTERS_PER_CALL reporters per call, so
# ~12 calls cover a year instead of ~346. Its own manifest
# (COMTRADE_WORLD_MANIFEST) has a row per country-year; a batch's calls are
# logged on its first reporter so daily totals add up.
#
# -- API limits -----------------------------------------------------------------
# Free tier: 500 calls/day, 100k rows per call. 173 countries x 2 chunks = 346
# calls per year of data. All runs share DAILY_CALL_BUDGET per UTC day and stop
# cleanly at it or on an auth/quota error (HTTP 401/403/429); rerunning the
# same command resumes where it stopped.
#
# Requests are performed here, not by ct_get_data(): comtradr waits out any
# Retry-After that Comtrade sends, and once the daily quota is spent that wait
# is hours (the 2026-11-20 run sat asleep at PHL). Rate-limit waits up to
# MAX_RETRY_WAIT_S are still retried; a longer one stops the run at once.
#
# Requires: COMTRADE_API_KEY in .env
# Does NOT require: a DB connection (standalone script)
# ------------------------------------------------------------------------------

library(comtradr)
library(tidyverse)
library(glue)

if (!exists("COMTRADE_CSV_DIR"))  stop("Source config.R first or run via run_pipeline.R")
if (!exists("COMTRADE_API_KEY"))  stop("COMTRADE_API_KEY not set -- check .env")
if (COMTRADE_API_KEY == "")       stop("COMTRADE_API_KEY is empty -- check .env")
if (!exists("COMTRADE_MANIFEST")) COMTRADE_MANIFEST <- "data/comtrade_download_manifest.csv"
if (!exists("COMTRADE_WORLD_DIR")) COMTRADE_WORLD_DIR <- "data/comtrade_world"
if (!exists("COMTRADE_WORLD_MANIFEST"))
  COMTRADE_WORLD_MANIFEST <- "data/comtrade_world_manifest.csv"
if (!exists("COMTRADE_EXT_CSV_DIR"))   COMTRADE_EXT_CSV_DIR   <- "data/comtrade_csv_ext"
if (!exists("COMTRADE_EXT_MANIFEST"))  COMTRADE_EXT_MANIFEST  <- "data/comtrade_ext_manifest.csv"
if (!exists("COMTRADE_EXT_WORLD_DIR")) COMTRADE_EXT_WORLD_DIR <- "data/comtrade_world_ext"

set_primary_comtrade_key(COMTRADE_API_KEY)

# -- Config --------------------------------------------------------------------

COMMODITY_CHUNK_SIZE <- 87     # codes per API call (2 chunks for 174 codes)
API_RATE_LIMIT       <- 500    # free tier: 500 calls per day
DAILY_CALL_BUDGET    <- 480    # stop below the limit so no country-year is left half-done
MAX_ROWS_PER_CALL    <- 1e5    # free-tier row cap; a call returning this many is truncated
REFRESH_RECENT_YEARS <- 2L     # the current and previous calendar year are refreshed...
REFRESH_AFTER_DAYS   <- 30L    # ...once their last download is older than this
STOP_HTTP_STATUS     <- c(401L, 403L, 429L)   # key rejected / quota spent: stop, resume later
WORLD_REPORTERS_PER_CALL <- 20L  # World-only calls: ~1,000-character URL, <= 41,760 rows
REQUESTS_PER_SECOND  <- 10 / 60  # comtradr's default throttle: one call every 6 seconds
MAX_RETRY_WAIT_S     <- 60       # retry a rate limit only when asked to wait this long or less
REQUEST_TIMEOUT_S    <- 300      # a call taking longer fails (status error, retried next run)

# -- Helpers -------------------------------------------------------------------

leading_zero_cmd_codes <- function(x) {
  x_char <- as.character(x)
  ifelse(nchar(x_char) %% 2 == 1, paste0("0", x_char), x_char)
}

chunk_commodity_codes <- function(codes, chunk_size = COMMODITY_CHUNK_SIZE) {
  split(codes, ceiling(seq_along(codes) / chunk_size))
}

csv_path <- function(iso3c, year) {
  file.path(COMTRADE_CSV_DIR, paste0(iso3c, "_", year, ".csv"))
}

# World totals (partner "World") live in their own folder, never in the
# country-year CSVs, so partner coverage can be measured (D-014)
world_path <- function(iso3c, year) {
  file.path(COMTRADE_WORLD_DIR, paste0(iso3c, "_", year, ".csv"))
}

# -- Country list --------------------------------------------------------------
# Derived from the pipeline country frame.
# Excludes Kosovo (XKX) and Taiwan (TWN) -- not recognized by Comtrade.
# Adds Hong Kong (HKG) as a separate reporter.

use_iso3c <- read_csv(COUNTRY_FRAME_CSV,
                      col_types = cols(.default = "c"),
                      show_col_types = FALSE) |>
  distinct(iso3c, .keep_all = TRUE) |>       # drop duplicate SRB
  filter(!(iso3c %in% c("XKX", "TWN"))) |>
  add_row(iso3c = "HKG") |>
  arrange(iso3c) |>
  pull(iso3c)

# -- Commodity codes -----------------------------------------------------------

all_cmd_code <- read_csv(COMMODITY_CODES_CSV,
                         col_types = cols(.default = "c"),
                         show_col_types = FALSE)

# Code sets (D-027): "core" = the original 174 codes, downloaded from 2010;
# "ext" = codes added in November 2026, downloaded from 2015 into their own
# folders and manifest. A code file without the column is all core.
if (!"code_set" %in% names(all_cmd_code)) all_cmd_code$code_set <- "core"
all_cmd_code$code_set <- coalesce(na_if(all_cmd_code$code_set, ""), "core")
codes_in_set <- function(set)
  leading_zero_cmd_codes(all_cmd_code$cmd_code[all_cmd_code$code_set == set])

commodity_codes <- codes_in_set("core")

message(sprintf("  Countries: %d | Commodity codes: %d core, %d ext | Commodities: %d",
                length(use_iso3c),
                length(commodity_codes),
                length(codes_in_set("ext")),
                length(unique(all_cmd_code$commodity_name))))

# Where each code set lives, fixed when the script is sourced. with_code_set()
# points the download functions at one set for the length of a call.
CODE_SETS <- list(
  core = list(csv_dir = COMTRADE_CSV_DIR, manifest = COMTRADE_MANIFEST,
              world_dir = COMTRADE_WORLD_DIR, codes = codes_in_set("core")),
  ext  = list(csv_dir = COMTRADE_EXT_CSV_DIR, manifest = COMTRADE_EXT_MANIFEST,
              world_dir = COMTRADE_EXT_WORLD_DIR, codes = codes_in_set("ext"))
)

# Every manifest that logs API calls: one daily budget covers them all
CALL_MANIFESTS <- c(COMTRADE_MANIFEST, COMTRADE_EXT_MANIFEST, COMTRADE_WORLD_MANIFEST)

with_code_set <- function(set, expr) {
  cfg <- CODE_SETS[[set]]
  if (is.null(cfg)) stop("Unknown code set: ", set)
  env  <- environment(with_code_set)          # where the script's settings live
  keys <- c(COMTRADE_CSV_DIR = "csv_dir", COMTRADE_MANIFEST = "manifest",
            COMTRADE_WORLD_DIR = "world_dir", commodity_codes = "codes")
  old  <- mget(names(keys), envir = env)
  on.exit(for (k in names(keys)) assign(k, old[[k]], envir = env), add = TRUE)
  for (k in names(keys)) assign(k, cfg[[keys[[k]]]], envir = env)
  expr                                        # evaluated only now, inside the set
}

# -- API request ---------------------------------------------------------------

# comtradr's internal steps, used below exactly as ct_get_data() uses them
COMTRADR_STEPS <- c("ct_check_params", "ct_build_request", "ct_process_response",
                    "comtrade_error_body")
missing_steps  <- COMTRADR_STEPS[!vapply(COMTRADR_STEPS, exists, logical(1),
                                         envir = asNamespace("comtradr"), inherits = FALSE)]
if (length(missing_steps) > 0)
  stop("comtradr ", packageVersion("comtradr"), " lacks ",
       paste(missing_steps, collapse = ", "), "; comtrade_request() expects comtradr 1.0.x")

# The only function that talks to Comtrade (tests substitute a stub).
# Same parameters, URL and processing as ct_get_data(); only the perform step
# is ours (perform_comtrade). partner = "everything" keeps partnerCode out of
# the URL (see header); partner = "World" asks for World totals only. `iso3c`
# may hold several reporters.
comtrade_request <- function(iso3c, year, codes, partner = "everything") {
  params <- comtradr:::ct_check_params(
    type = "goods", frequency = "M", commodity_classification = "HS",
    commodity_code = codes, flow_direction = c("export", "import"),
    reporter = iso3c, partner = partner,
    start_date = sprintf("%d-01", year), end_date = sprintf("%d-12", year),
    mode_of_transport = "TOTAL modes of transport", partner_2 = "World",
    customs_code = "C00", update = FALSE, verbose = FALSE,
    extra_params = NULL, bulk = FALSE)
  req <- comtradr:::ct_build_request(params,
                                     primary_token = comtradr::get_primary_comtrade_key(),
                                     verbose = FALSE, bulk = FALSE)
  comtradr:::ct_process_response(perform_comtrade(req), verbose = FALSE,
                                 tidy_cols = TRUE, bulk = FALSE)
}

# Seconds Comtrade asks us to wait (Retry-After); NA when it doesn't say
retry_after_s <- function(resp) {
  suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After", default = NA)))
}

# Error text: comtradr's parser, then the gateway's own `message` field, which
# is where "Out of call volume quota. Quota will be replenished in ..." arrives
comtrade_error_message <- function(resp) {
  msg <- tryCatch(comtradr:::comtrade_error_body(resp), error = function(e) NULL)
  if (length(msg) > 0) return(msg)
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.list(body) && is.character(body$message)) body$message else NULL
}

# Retry a rate limit (429) or unavailable server (503) only when the wait asked
# for is short; a spent quota (403, or a 429 asking for hours) is not retried
comtrade_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 503L) &&
    isTRUE(coalesce(retry_after_s(resp), 0) <= MAX_RETRY_WAIT_S)
}

# Perform a request: throttled like comtradr; a short wait is retried, anything
# else raised at once (fetch_chunk turns 401/403/429 into a clean stop)
perform_comtrade <- function(req, rate = REQUESTS_PER_SECOND) {
  if (!is.null(rate)) req <- httr2::req_throttle(req, rate = rate)
  req |>
    httr2::req_error(body = comtrade_error_message) |>
    httr2::req_retry(max_tries = 3, is_transient = comtrade_is_transient,
                     after = retry_after_s) |>
    httr2::req_timeout(REQUEST_TIMEOUT_S) |>
    httr2::req_perform()
}

# HTTP status of a failed request: httr2 condition fields, else the message text
http_status_of <- function(e) {
  if (is.numeric(e$status)) return(as.integer(e$status))
  if (!is.null(e$resp))     return(as.integer(httr2::resp_status(e$resp)))
  m <- regmatches(conditionMessage(e), regexpr("HTTP [0-9]{3}", conditionMessage(e)))
  if (length(m) == 1) as.integer(substring(m, 6)) else NA_integer_
}

# Condition that ends the whole run (quota spent, key rejected, call budget)
comtrade_stop <- function(msg) {
  structure(class = c("comtrade_stop", "error", "condition"),
            list(message = msg, call = NULL))
}

# Fetch one commodity chunk for a country-year (or, with partner = "World", for
# a batch of reporters). Halves the chunk and retries on HTTP 414 or when the
# row cap is reached. Returns list(ok, data, world, status, msg): `data` is a
# list of data frames already restricted to `partners`, `world` a list of the
# World-total rows that came with them.
fetch_chunk <- function(iso3c, year, codes, partners, state, partner = "everything") {
  if (state$calls >= state$budget)
    stop(comtrade_stop(sprintf("call budget of %d reached", state$budget)))
  state$calls <- state$calls + 1L

  res <- tryCatch(comtrade_request(iso3c, year, codes, partner = partner),
                  error = function(e) e)
  Sys.sleep(runif(1, 1, 3))   # respectful API usage

  who <- if (length(iso3c) > 1) sprintf("%d reporters", length(iso3c)) else iso3c
  split_and_retry <- function(why) {
    cat(glue("  {who} {year}: {why} with {length(codes)} codes -- splitting\n\n"))
    half <- ceiling(length(codes) / 2)
    out  <- lapply(list(codes[seq_len(half)], codes[-seq_len(half)]),
                   \(part) fetch_chunk(iso3c, year, part, partners, state, partner))
    failed <- Filter(\(o) !o$ok, out)
    if (length(failed) > 0) return(failed[[1]])
    list(ok = TRUE, data = c(out[[1]]$data, out[[2]]$data),
         world = c(out[[1]]$world, out[[2]]$world),
         status = 200L, msg = NA_character_)
  }

  if (inherits(res, "error")) {
    status <- http_status_of(res)
    if (!is.na(status) && status %in% STOP_HTTP_STATUS)
      stop(comtrade_stop(sprintf("HTTP %d from Comtrade -- %s", status, conditionMessage(res))))
    if (identical(status, 414L) && length(codes) > 1)
      return(split_and_retry("HTTP 414"))
    return(list(ok = FALSE, data = list(), world = list(), status = status,
                msg = conditionMessage(res)))
  }

  # Comtrade's "no data" answer arrives as data.frame(count = 0)
  if (!"partner_iso" %in% names(res))
    return(list(ok = TRUE, data = list(), world = list(), status = 200L,
                msg = NA_character_))

  if (nrow(res) >= MAX_ROWS_PER_CALL) {
    if (length(codes) > 1)
      return(split_and_retry(sprintf("%d rows (row cap)", nrow(res))))
    return(list(ok = FALSE, data = list(), world = list(), status = 200L,
                msg = "a single commodity code still reaches the row cap"))
  }

  # World totals (partner code 0, "W00") are kept apart; the partner data keep
  # the same set as earlier years: frame countries other than the reporter
  is_world <- res$partner_iso %in% "W00"
  if ("partner_code" %in% names(res)) is_world <- is_world | res$partner_code %in% 0
  list(ok = TRUE, data = list(filter(res, partner_iso %in% partners)),
       world = list(res[is_world, , drop = FALSE]),
       status = 200L, msg = NA_character_)
}

# Write the downloaded frames as one CSV once every call has succeeded:
# temp file first, then rename over the target. Columns follow the first frame.
write_country_year <- function(frames, out_file) {
  cols <- names(frames[[1]])
  tmp  <- paste0(out_file, ".tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  for (i in seq_along(frames)) {
    d <- frames[[i]]
    for (m in setdiff(cols, names(d))) d[[m]] <- NA
    write_csv(d[cols], tmp, append = i > 1, col_names = i == 1)
  }
  if (!file.rename(tmp, out_file) && !file.copy(tmp, out_file, overwrite = TRUE))
    stop("could not write ", out_file)
}

# Download every chunk of one country-year; returns its manifest row.
download_country_year <- function(iso3c, year, codes, state,
                                  prev_status = NA_character_,
                                  chunk_size = COMMODITY_CHUNK_SIZE) {
  partners <- setdiff(use_iso3c, iso3c)
  calls0   <- state$calls
  frames   <- list()
  worlds   <- list()

  for (chunk in chunk_commodity_codes(codes, chunk_size)) {
    res <- fetch_chunk(iso3c, year, chunk, partners, state)
    if (!res$ok)   # nothing is written; an existing file stays as it was
      return(manifest_row(iso3c, year, "error", calls = state$calls - calls0,
                          http_status = res$status, message = res$msg))
    frames <- c(frames, res$data)
    worlds <- c(worlds, res$world)
  }

  worlds <- Filter(\(d) nrow(d) > 0, worlds)
  if (length(worlds) > 0) {
    dir.create(COMTRADE_WORLD_DIR, showWarnings = FALSE, recursive = TRUE)
    write_country_year(worlds, world_path(iso3c, year))
  }

  frames   <- Filter(\(d) nrow(d) > 0, frames)
  n_rows   <- sum(vapply(frames, nrow, integer(1)))
  out_file <- csv_path(iso3c, year)

  if (n_rows == 0) {
    if (identical(prev_status, "ok"))   # never replace data with nothing
      return(manifest_row(iso3c, year, "ok", calls = state$calls - calls0,
                          http_status = 200L,
                          message = "refresh returned no rows; previous file kept"))
    # Empty marker files from failed attempts of the old script are dropped
    if (file.exists(out_file) && file.size(out_file) == 0) unlink(out_file)
    return(manifest_row(iso3c, year, "no_data", n_rows = 0L,
                        calls = state$calls - calls0, http_status = 200L))
  }

  write_country_year(frames, out_file)
  months <- sort(unique(as.integer(unlist(lapply(frames, `[[`, "ref_month")))))
  manifest_row(iso3c, year, "ok", n_rows = n_rows, n_months = length(months),
               max_month = max(months), calls = state$calls - calls0,
               http_status = 200L)
}

# -- Manifest ------------------------------------------------------------------

manifest_row <- function(iso3c, year, status, n_rows = NA, n_months = NA,
                         max_month = NA, calls = NA, http_status = NA,
                         message = NA, source = "download",
                         downloaded_at = Sys.time()) {
  tibble(
    iso3c         = iso3c,
    year          = as.integer(year),
    status        = status,
    n_rows        = as.integer(n_rows),
    n_months      = as.integer(n_months),
    max_month     = as.integer(max_month),
    calls         = as.integer(calls),
    http_status   = as.integer(http_status),
    message       = substr(gsub("[\r\n]+", " ", as.character(message)), 1, 300),
    downloaded_at = downloaded_at,
    source        = source
  )
}

read_manifest <- function(path = COMTRADE_MANIFEST) {
  if (!file.exists(path)) return(manifest_row(character(), integer(), character())[0, ])
  read_csv(path, show_col_types = FALSE,
           col_types = cols(iso3c = "c", year = "i", status = "c",
                            n_rows = "i", n_months = "i", max_month = "i",
                            calls = "i", http_status = "i", message = "c",
                            downloaded_at = col_datetime(), source = "c"))
}

write_manifest <- function(manifest, path = COMTRADE_MANIFEST) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  write_csv(arrange(manifest, year, iso3c), tmp)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
}

upsert_manifest <- function(manifest, rows) {
  bind_rows(anti_join(manifest, rows, by = c("iso3c", "year")), rows)
}

# API calls already logged today (UTC), across all runs
calls_used_today <- function(manifest, now = Sys.time()) {
  today <- format(now, "%Y-%m-%d", tz = "UTC")
  logged_today <- !is.na(manifest$downloaded_at) &
    format(manifest$downloaded_at, "%Y-%m-%d", tz = "UTC") == today
  as.integer(sum(manifest$calls[logged_today], na.rm = TRUE))
}

# The other manifests' rows, for the shared budget (the current manifest is
# passed in memory by the caller)
logged_elsewhere <- function(current_path) {
  bind_rows(lapply(setdiff(unique(CALL_MANIFESTS), current_path), read_manifest))
}

# Codes per call for a country: split ahead of time when its largest earlier
# download in this code set would not fit under the row cap, instead of
# spending a call to find out
chunk_size_for <- function(iso3c, manifest, codes) {
  prev <- manifest$n_rows[manifest$iso3c == iso3c & manifest$status == "ok"]
  prev <- suppressWarnings(max(prev, na.rm = TRUE))
  if (!is.finite(prev)) return(as.integer(COMMODITY_CHUNK_SIZE))
  pieces <- max(1, ceiling(prev / (0.9 * MAX_ROWS_PER_CALL)))
  as.integer(min(COMMODITY_CHUNK_SIZE, ceiling(length(codes) / pieces)))
}

# Status of a CSV written before the manifest existed
classify_existing_csv <- function(path) {
  if (file.size(path) == 0) return("error")              # marker from a failed attempt
  first <- readLines(path, n = 1, warn = FALSE)
  if (length(first) == 0) return("error")
  if (startsWith(first, "count")) return("no_data")      # Comtrade had no data
  if (grepl("(^|,)(ref_year|yr)(,|$)", first)) return("ok")
  "error"                                                # header-less or unreadable
}

# Add CSVs already on disk that the manifest doesn't know about yet
bootstrap_manifest <- function(manifest) {
  files <- list.files(COMTRADE_CSV_DIR, pattern = "^[A-Z]{3}_[0-9]{4}\\.csv$",
                      full.names = TRUE)
  key   <- str_match(basename(files), "^([A-Z]{3})_([0-9]{4})\\.csv$")
  new   <- !paste(key[, 2], key[, 3]) %in% paste(manifest$iso3c, manifest$year)
  if (!any(new)) return(manifest)

  files  <- files[new]
  key    <- key[new, , drop = FALSE]
  status <- vapply(files, classify_existing_csv, character(1), USE.NAMES = FALSE)
  rows   <- manifest_row(key[, 2], key[, 3], status,
                         message = if_else(status == "error",
                                           "empty or unreadable file from an earlier attempt",
                                           NA_character_),
                         source = "bootstrap", downloaded_at = file.mtime(files))
  message(sprintf("  Manifest: added %d existing files (%s)", nrow(rows),
                  paste(names(table(status)), table(status), sep = " = ", collapse = ", ")))
  bind_rows(manifest, rows)
}

# Country-years to download now, with the reason for each.
# refresh_before: also re-download anything last downloaded before this date
# (a one-off repair that stays done once the new download is logged).
plan_downloads <- function(countries, years, manifest, force = FALSE,
                           retry_no_data = FALSE, refresh_before = NULL,
                           now = Sys.time()) {
  recent_from <- as.integer(format(now, "%Y")) - REFRESH_RECENT_YEARS + 1L
  cutoff <- if (is.null(refresh_before)) as.POSIXct(NA) else
    as.POSIXct(as.character(refresh_before), tz = "UTC")
  expand_grid(iso3c = countries, year = as.integer(years)) |>
    left_join(select(manifest, iso3c, year, status, downloaded_at),
              by = c("iso3c", "year")) |>
    mutate(
      file_missing = !file.exists(csv_path(iso3c, year)),
      age_days     = as.numeric(difftime(now, downloaded_at, units = "days")),
      reason = case_when(
        force                                               ~ "forced",
        is.na(status)                                       ~ "not attempted",
        status == "error"                                   ~ "retry after error",
        status == "interrupted"                             ~ "resume interrupted",
        status == "ok" & file_missing                       ~ "file missing",
        !is.na(cutoff) & downloaded_at < cutoff             ~ "downloaded before cutoff",
        year >= recent_from & age_days > REFRESH_AFTER_DAYS ~ "refresh recent year",
        status == "no_data" & retry_no_data                 ~ "re-check no data",
        TRUE                                                ~ NA_character_
      )
    ) |>
    filter(!is.na(reason))
}

# -- Main ----------------------------------------------------------------------

run_comtrade_download <- function(years, countries = NULL, force = FALSE,
                                  retry_no_data = FALSE, refresh_before = NULL,
                                  call_budget = DAILY_CALL_BUDGET,
                                  dry_run = FALSE) {
  if (is.null(countries)) countries <- use_iso3c
  unknown <- setdiff(countries, use_iso3c)
  if (length(unknown) > 0)
    stop("Not in the country frame: ", paste(unknown, collapse = ", "))

  dir.create(COMTRADE_CSV_DIR, showWarnings = FALSE, recursive = TRUE)
  manifest <- bootstrap_manifest(read_manifest())
  if (!dry_run) write_manifest(manifest)

  todo     <- plan_downloads(countries, years, manifest, force, retry_no_data,
                             refresh_before)
  todo$chunk_size <- vapply(todo$iso3c, chunk_size_for, integer(1),
                            manifest = manifest, codes = commodity_codes)
  n_calls  <- sum(ceiling(length(commodity_codes) / todo$chunk_size))

  # The budget is per UTC day, shared by every run that day: calls already
  # logged in any manifest today count against it
  used_today <- calls_used_today(bind_rows(manifest, logged_elsewhere(COMTRADE_MANIFEST)))
  budget     <- max(0L, as.integer(call_budget) - used_today)

  cat(glue("\n=== COMTRADE DOWNLOAD ===\n",
           "Years: {years_label(years)} | Countries: {length(countries)} | ",
           "Codes: {length(commodity_codes)} | ",
           "To download: {nrow(todo)} country-years (~{n_calls} calls) | ",
           "Budget left today: {budget} of {call_budget} calls\n\n"))
  if (nrow(todo) > 0) print(count(todo, year, reason), n = Inf)
  if (dry_run || nrow(todo) == 0) return(invisible(todo))
  if (budget == 0) {
    cat("Today's call budget is spent; rerun tomorrow (UTC) to resume.\n")
    return(invisible(manifest))
  }

  state <- new.env()
  state$calls  <- 0L
  state$budget <- budget
  stopped <- NULL

  for (i in seq_len(nrow(todo))) {
    iso <- todo$iso3c[i]
    yr  <- todo$year[i]
    cat(glue("Downloading {iso} {yr} ({todo$reason[i]}):\n\n"))

    calls_before <- state$calls
    row <- tryCatch(
      download_country_year(iso, yr, commodity_codes, state,
                            prev_status = todo$status[i],
                            chunk_size = chunk_size_for(iso, manifest, commodity_codes)),
      comtrade_stop = function(e) e
    )
    if (inherits(row, "comtrade_stop")) {
      stopped <- conditionMessage(row)
      # Log the calls this country-year already used, so the daily budget
      # stays exact; the next run resumes it
      if (state$calls > calls_before) {
        manifest <- upsert_manifest(manifest, manifest_row(
          iso, yr, "interrupted", calls = state$calls - calls_before,
          message = paste("stopped:", stopped)))
        write_manifest(manifest)
      }
      break
    }

    note <- if (is.na(row$message)) "" else paste0(" | ", row$message)
    cat(glue("  {row$status}: {coalesce(row$n_rows, 0L)} rows | ",
             "{coalesce(row$n_months, 0L)} months | {row$calls} calls{note}\n\n"))
    manifest <- upsert_manifest(manifest, row)
    write_manifest(manifest)
  }

  remaining <- plan_downloads(countries, years, manifest, retry_no_data = retry_no_data,
                              refresh_before = refresh_before)
  cat(glue("\n=== DOWNLOAD SUMMARY: {state$calls} calls this run | ",
           "{nrow(remaining)} country-years still to download ===\n\n"))
  manifest |>
    filter(iso3c %in% countries, year %in% years) |>
    count(year, status) |>
    pivot_wider(names_from = status, values_from = n, values_fill = 0L) |>
    print(n = Inf)

  errors <- filter(manifest, iso3c %in% countries, year %in% years, status == "error")
  if (nrow(errors) > 0) print(select(errors, iso3c, year, http_status, message), n = Inf)
  if (!is.null(stopped))
    cat(glue("\nStopped early: {stopped}\nRerun the same command to resume.\n\n"))

  invisible(manifest)
}

# The extension codes (D-027): the same rules as run_comtrade_download(), with
# their own CSV folder, manifest and World folder; the daily budget is shared.
run_comtrade_ext_download <- function(years, ...) {
  with_code_set("ext", run_comtrade_download(years, ...))
}

# -- World totals for files downloaded without them (D-014, D-023) -------------

# "2010-2023, 2026" rather than fifteen years in a row
years_label <- function(years) {
  y    <- sort(unique(as.integer(years)))
  runs <- split(y, cumsum(c(1L, diff(y) != 1L)))
  paste(vapply(runs, \(r) if (length(r) > 1) paste0(r[1], "-", r[length(r)]) else
    as.character(r), character(1)), collapse = ", ")
}

# Country-years with partner data but no World file yet. A World "no_data"
# answer is final; error / interrupted batches are planned again.
plan_world_downloads <- function(countries, years, manifest, world_manifest) {
  no_world <- world_manifest |> filter(status == "no_data") |> select(iso3c, year)
  manifest |>
    filter(iso3c %in% countries, year %in% as.integer(years), status == "ok") |>
    select(iso3c, year) |>
    anti_join(no_world, by = c("iso3c", "year")) |>
    filter(!file.exists(world_path(iso3c, year))) |>
    arrange(year, iso3c)
}

# World totals for a batch of reporters in one year; returns a manifest row per
# reporter. Calls are logged on the batch's first reporter.
download_world_batch <- function(reporters, year, codes, state) {
  calls0   <- state$calls
  calls_of <- function() c(state$calls - calls0, rep(0L, length(reporters) - 1L))
  worlds   <- list()
  for (chunk in chunk_commodity_codes(codes)) {
    res <- fetch_chunk(reporters, year, chunk, character(0), state, partner = "World")
    if (!res$ok)   # nothing is written for any reporter in the batch
      return(manifest_row(reporters, year, "error", calls = calls_of(),
                          http_status = res$status, message = res$msg, source = "world"))
    worlds <- c(worlds, res$world)
  }
  worlds <- Filter(\(d) nrow(d) > 0, worlds)
  got    <- unique(unlist(lapply(worlds, `[[`, "reporter_iso")))
  extra  <- setdiff(got, reporters)
  if (length(extra) > 0)
    message("  World rows for reporters not asked for: ", paste(extra, collapse = ", "))

  calls <- calls_of()
  bind_rows(lapply(seq_along(reporters), \(i) {
    r <- reporters[i]
    frames <- Filter(\(d) nrow(d) > 0,
                     lapply(worlds, \(d) d[d$reporter_iso %in% r, , drop = FALSE]))
    if (length(frames) == 0)
      return(manifest_row(r, year, "no_data", n_rows = 0L, calls = calls[i],
                          http_status = 200L, source = "world"))
    dir.create(COMTRADE_WORLD_DIR, showWarnings = FALSE, recursive = TRUE)
    write_country_year(frames, world_path(r, year))
    months <- sort(unique(as.integer(unlist(lapply(frames, `[[`, "ref_month")))))
    manifest_row(r, year, "ok", n_rows = sum(vapply(frames, nrow, integer(1))),
                 n_months = length(months), max_month = max(months),
                 calls = calls[i], http_status = 200L, source = "world")
  }))
}

run_comtrade_world_download <- function(years, countries = NULL,
                                        reporters_per_call = WORLD_REPORTERS_PER_CALL,
                                        call_budget = DAILY_CALL_BUDGET,
                                        dry_run = FALSE) {
  if (is.null(countries)) countries <- use_iso3c
  manifest       <- bootstrap_manifest(read_manifest())
  world_manifest <- read_manifest(COMTRADE_WORLD_MANIFEST)
  todo <- plan_world_downloads(countries, years, manifest, world_manifest) |>
    group_by(year) |>
    mutate(batch = ceiling(row_number() / reporters_per_call)) |>
    ungroup()
  batches  <- distinct(todo, year, batch)
  n_chunks <- length(chunk_commodity_codes(commodity_codes))

  used_today <- calls_used_today(bind_rows(world_manifest, logged_elsewhere(COMTRADE_WORLD_MANIFEST)))
  budget     <- max(0L, as.integer(call_budget) - used_today)

  cat(glue("\n=== COMTRADE WORLD TOTALS ===\n",
           "Years: {years_label(years)} | To fetch: {nrow(todo)} ",
           "country-years in {nrow(batches)} batches (~{nrow(batches) * n_chunks} calls) | ",
           "Budget left today: {budget} of {call_budget} calls\n\n"))
  if (nrow(todo) > 0) print(count(todo, year, name = "country_years"), n = Inf)
  if (dry_run || nrow(todo) == 0) return(invisible(todo))
  if (budget == 0) {
    cat("Today's call budget is spent; rerun tomorrow (UTC) to resume.\n")
    return(invisible(world_manifest))
  }

  state <- new.env()
  state$calls  <- 0L
  state$budget <- budget
  stopped <- NULL

  for (i in seq_len(nrow(batches))) {
    yr   <- batches$year[i]
    reps <- todo$iso3c[todo$year == yr & todo$batch == batches$batch[i]]
    cat(glue("World totals {yr}, batch {batches$batch[i]}: {length(reps)} reporters ",
             "({reps[1]} to {reps[length(reps)]})\n\n"))

    calls_before <- state$calls
    rows <- tryCatch(download_world_batch(reps, yr, commodity_codes, state),
                     comtrade_stop = function(e) e)
    if (inherits(rows, "comtrade_stop")) {
      stopped <- conditionMessage(rows)
      if (state$calls > calls_before) {
        world_manifest <- upsert_manifest(world_manifest, manifest_row(
          reps, yr, "interrupted",
          calls = c(state$calls - calls_before, rep(0L, length(reps) - 1L)),
          message = paste("stopped:", stopped), source = "world"))
        write_manifest(world_manifest, COMTRADE_WORLD_MANIFEST)
      }
      break
    }
    cat(glue("  {sum(rows$status == 'ok')} with World rows | ",
             "{sum(rows$status == 'no_data')} without | ",
             "{sum(rows$status == 'error')} failed | {sum(rows$calls)} calls\n\n"))
    world_manifest <- upsert_manifest(world_manifest, rows)
    write_manifest(world_manifest, COMTRADE_WORLD_MANIFEST)
  }

  remaining <- plan_world_downloads(countries, years, manifest, world_manifest)
  cat(glue("\n=== WORLD TOTALS SUMMARY: {state$calls} calls this run | ",
           "{nrow(remaining)} country-years still to fetch ===\n\n"))
  errors <- filter(world_manifest, year %in% years, status == "error")
  if (nrow(errors) > 0) print(select(errors, iso3c, year, http_status, message), n = Inf)
  if (!is.null(stopped))
    cat(glue("\nStopped early: {stopped}\nRerun the same command to resume.\n\n"))
  invisible(world_manifest)
}

# One-off check (2 calls): a World-only request returns the same World totals
# as the World rows kept by the main download for that country-year.
# Returns TRUE (match), FALSE (differ) or NA (not checked: nothing to compare,
# or no budget). The result is logged once and reused on later runs.
check_world_totals <- function(iso3c, year, call_budget = DAILY_CALL_BUDGET) {
  world_manifest <- read_manifest(COMTRADE_WORLD_MANIFEST)
  done <- world_manifest[world_manifest$iso3c == iso3c & world_manifest$year == year &
                           world_manifest$source %in% "check", ]
  if (nrow(done) > 0) {
    cat("World totals check (", iso3c, year, "):", done$message[1], "\n")
    return(identical(done$status[1], "ok"))
  }
  main_file <- world_path(iso3c, year)
  if (!file.exists(main_file)) {
    cat("World totals check skipped:", iso3c, year,
        "has no World file from the main download yet.\n")
    return(NA)
  }
  budget <- as.integer(call_budget) -
    calls_used_today(bind_rows(world_manifest, logged_elsewhere(COMTRADE_WORLD_MANIFEST)))
  if (budget < length(chunk_commodity_codes(commodity_codes))) {
    cat("World totals check skipped: not enough of today's call budget left.\n")
    return(NA)
  }

  state <- new.env()
  state$calls  <- 0L
  state$budget <- budget
  frames <- list()
  for (chunk in chunk_commodity_codes(commodity_codes)) {
    res <- tryCatch(fetch_chunk(iso3c, year, chunk, character(0), state, partner = "World"),
                    comtrade_stop = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    if (!res$ok) {
      cat("World totals check failed:", res$msg, "\n")
      return(NA)
    }
    frames <- c(frames, res$world)
  }

  key_cols <- function(d) tibble(flow_code = as.character(d$flow_code),
                                 cmd_code  = as.character(d$cmd_code),
                                 ref_month = as.integer(d$ref_month),
                                 value     = as.numeric(d$primary_value))
  main  <- key_cols(read_csv(main_file, col_types = cols(.default = "c"),
                             show_col_types = FALSE))
  alone <- bind_rows(lapply(Filter(\(d) nrow(d) > 0, frames), key_cols))
  cmp   <- full_join(main, alone, by = c("flow_code", "cmd_code", "ref_month"),
                     suffix = c("_main", "_world"))
  unmatched <- sum(is.na(cmp$value_main) | is.na(cmp$value_world))
  max_diff  <- suppressWarnings(max(abs(cmp$value_main - cmp$value_world), na.rm = TRUE))
  same      <- nrow(alone) > 0 && unmatched == 0 && max_diff == 0
  msg <- sprintf("%s: %d World rows from the main download, %d from World-only; %d unmatched; largest value difference %s",
                 if (same) "match" else "DIFFER", nrow(main), nrow(alone), unmatched,
                 format(max_diff))
  cat("World totals check (", iso3c, year, "):", msg, "\n")

  write_manifest(upsert_manifest(world_manifest, manifest_row(
    iso3c, year, if (same) "ok" else "error", n_rows = nrow(alone), calls = state$calls,
    http_status = 200L, message = msg, source = "check")), COMTRADE_WORLD_MANIFEST)
  same
}

# Old entry point, kept so existing habits get a pointer instead of a puzzle
download_comtrade_data <- function(...) {
  stop("download_comtrade_data() was replaced by run_comtrade_download(), e.g.\n",
       "  run_comtrade_download(2026)\n",
       "See the header of R/ingest_comtrade_download.R.", call. = FALSE)
}

# -- Run -----------------------------------------------------------------------
# The download sequence now lives in run_comtrade_update.R (repo root), which
# sources this file for its functions. These were the calls run from here:
# run_comtrade_download(2026)                      # ~346 calls
# run_comtrade_download(2017)                      # backfill, ~154 calls
# run_comtrade_download(2025)                      # refresh, ~346 calls (next day)
