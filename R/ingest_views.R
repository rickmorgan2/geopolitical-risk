# ingest_views.R
# ------------------------------------------------------------------------------
# ViEWS country-month forecasts, every archived run (D-019): an input to the
# conflict forecasts and the benchmark they must beat.
#
# Source: https://api.viewsforecasting.org/  (public, no key)
#   /                      list of runs
#   /{run}/cm/sb           36 monthly forecasts for ~191 countries
# Runs of the fatalities families start in December 2021 (fatalities001), then
# fatalities002 (2023-04) and fatalities003 (2025-10). ViEWS publishes values
# for state-based violence only: the ns and os endpoints return no forecasts.
#
# Field names changed across families:
#   fatalities002/003  main_mean (fatalities), main_mean_ln, main_dich
#   fatalities001      sc_cm_sb_main (log scale), sc_cm_sb_dich_main
# Standardized here to pred_mean / pred_ln / pred_dich. For fatalities001,
# pred_mean = expm1(pred_ln) and pred_mean_derived = TRUE: treating
# sc_cm_sb_main as log(1 + fatalities) is an assumption to verify.
#
# Output: bronze.views_cm_sb  run x country x forecast month, with the step
#   (1 = first month after the run's data) and `preferred` = the run kept when
#   several runs share a forecast window (re-runs such as *_2022_00, *_t02).
# Raw responses are cached in VIEWS_RAW_DIR; published runs never change.
# Expects: `con` from run_pipeline.R.
# ------------------------------------------------------------------------------

library(DBI)
library(dplyr)
library(purrr)
library(httr2)
library(jsonlite)

if (!exists("VIEWS_RAW_DIR")) VIEWS_RAW_DIR <- "data/raw_views"
VIEWS_API <- "https://api.viewsforecasting.org"

# -- API -----------------------------------------------------------------------

views_get <- function(path) {
  request(paste0(VIEWS_API, "/", path)) |>
    req_user_agent("pressure-points research pipeline") |>
    req_retry(max_tries = 3) |>
    req_timeout(120) |>
    req_perform() |>
    resp_body_string()
}

list_views_runs <- function() {
  runs <- fromJSON(views_get(""))$runs
  sort(grep("^fatalities[0-9]+_", runs, value = TRUE))
}

# Raw JSON for one run, from the cache when present
fetch_views_run <- function(run, type = "sb") {
  path <- file.path(VIEWS_RAW_DIR, sprintf("%s_cm_%s.json", run, type))
  if (!file.exists(path) || file.size(path) == 0) {
    dir.create(VIEWS_RAW_DIR, showWarnings = FALSE, recursive = TRUE)
    body <- views_get(sprintf("%s/cm/%s?pagesize=10000", run, type))
    tmp  <- paste0(path, ".part")
    writeLines(body, tmp, useBytes = TRUE)
    if (!file.rename(tmp, path)) { file.copy(tmp, path, overwrite = TRUE); unlink(tmp) }
    Sys.sleep(0.5)   # be gentle with the API
  }
  fromJSON(path, simplifyVector = TRUE)
}

# -- Standardizing -------------------------------------------------------------

# One run's response -> standardized long rows
parse_views_run <- function(resp, run, type = "sb") {
  if (!is.null(resp$page_count) && resp$page_count > 1)
    stop(run, ": response is paginated (", resp$page_count, " pages); raise pagesize")
  d <- as_tibble(resp$data)
  if (nrow(d) == 0) return(NULL)
  main_col <- sprintf("sc_cm_%s_main", type)
  dich_col <- sprintf("sc_cm_%s_dich_main", type)
  if ("main_mean" %in% names(d)) {
    pred <- tibble(pred_mean = d$main_mean, pred_ln = d$main_mean_ln,
                   pred_dich = d$main_dich, pred_mean_derived = FALSE)
  } else if (main_col %in% names(d)) {
    pred <- tibble(pred_mean = expm1(d[[main_col]]), pred_ln = d[[main_col]],
                   pred_dich = if (dich_col %in% names(d)) d[[dich_col]] else NA_real_,
                   pred_mean_derived = TRUE)
  } else {
    return(NULL)   # endpoint without forecasts (ViEWS ns / os)
  }
  first_month <- if (!is.null(resp$start_date)) resp$start_date else min(d$month_id)
  bind_cols(
    tibble(run = run, family = sub("_.*$", "", run), type = type,
           gwcode = as.integer(d$gwcode), isoab = d$isoab,
           month_id = as.integer(d$month_id),
           year = as.integer(d$year), month = as.integer(d$month),
           step = as.integer(d$month_id - first_month + 1L),
           first_month_id = as.integer(first_month)),
    pred
  )
}

# When several runs forecast the same window, keep the latest-named one
mark_preferred <- function(df) {
  keep <- df |> distinct(run, first_month_id) |>
    group_by(first_month_id) |>
    summarise(run = max(run), .groups = "drop") |>
    mutate(preferred = TRUE)
  df |> left_join(keep, by = c("run", "first_month_id")) |>
    mutate(preferred = coalesce(preferred, FALSE))
}

map_views_countries <- function(df, frame) {
  frame <- frame |> transmute(gwcode = as.integer(gwcode), iso_frame = iso3c) |>
    filter(!is.na(gwcode)) |> distinct(gwcode, .keep_all = TRUE)
  df |> left_join(frame, by = "gwcode") |>
    mutate(country_code = coalesce(iso_frame, isoab)) |>
    select(-iso_frame)
}

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name), df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

# -- Run -----------------------------------------------------------------------

run_views_ingest <- function(con) {
  runs <- list_views_runs()
  message(sprintf("--- ViEWS: %d fatalities runs (%s to %s) ---",
                  length(runs), runs[1], runs[length(runs)]))
  frame <- dbGetQuery(con, "SELECT gwcode, iso3c FROM bronze.country_frame")
  out <- map(runs, \(run) {
    tryCatch(parse_views_run(fetch_views_run(run), run),
             error = function(e) { message("  ", run, ": ", conditionMessage(e)); NULL })
  }) |> compact() |> list_rbind() |>
    map_views_countries(frame) |>
    mark_preferred() |>
    mutate(forecast_date = as.Date(sprintf("%d-%02d-01", year, month)))
  write_bronze(out, "views_cm_sb", con)
  message(sprintf("  %d runs kept as preferred; forecasts for %d countries; steps %d-%d",
                  n_distinct(out$run[out$preferred]), n_distinct(out$country_code),
                  min(out$step), max(out$step)))
  invisible(out)
}

run_views_ingest(con)
