# Shared helpers for the Comtrade download and build tests.
# Run all tests from the repo root:  Rscript -e 'testthat::test_dir("tests/testthat")'
# Nothing here touches the network, data/, or the real API quota.

repo_file <- function(...) normalizePath(testthat::test_path("..", "..", ...), mustWork = TRUE)

# Evaluate quietly; returns the value of `expr`
quietly <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(expr))
  invisible(out)
}

# Source R/ingest_comtrade_download.R into its own environment, pointed at temp
# dirs and a small commodity list. The API stub is installed by the caller.
load_download_script <- function(csv_dir, manifest, n_codes = 10, n_ext = 0) {
  codes_csv <- tempfile(fileext = ".csv")
  codes <- data.frame(cmd_code = sprintf("10%02d", seq_len(n_codes)),
                      commodity_name = "wheat")
  if (n_ext > 0)    # a code file with the code_set column: core plus extension codes
    codes <- rbind(cbind(codes, code_set = "core"),
                   data.frame(cmd_code = sprintf("27%02d", seq_len(n_ext)),
                              commodity_name = "fuel", code_set = "ext"))
  utils::write.csv(codes, codes_csv, row.names = FALSE)
  env <- new.env(parent = globalenv())
  env$COMTRADE_CSV_DIR    <- csv_dir
  env$COMTRADE_MANIFEST   <- manifest
  env$COMTRADE_WORLD_DIR  <- file.path(dirname(csv_dir), "world")
  env$COMTRADE_WORLD_MANIFEST <- file.path(dirname(csv_dir), "world_manifest.csv")
  env$COMTRADE_EXT_CSV_DIR   <- file.path(dirname(csv_dir), "csv_ext")
  env$COMTRADE_EXT_MANIFEST  <- file.path(dirname(csv_dir), "ext_manifest.csv")
  env$COMTRADE_EXT_WORLD_DIR <- file.path(dirname(csv_dir), "world_ext")
  env$COMTRADE_API_KEY    <- "test-key-not-real"
  env$COUNTRY_FRAME_CSV   <- repo_file("country_code_frame.csv")
  env$COMMODITY_CODES_CSV <- codes_csv
  # Evaluate the script's definitions but never a top-level download call:
  # those would hit the real API.
  exprs <- parse(repo_file("R", "ingest_comtrade_download.R"), keep.source = FALSE)
  run_calls <- c("run_comtrade_download", "run_comtrade_ext_download",
                 "run_comtrade_world_download", "check_world_totals")
  is_run_call <- function(e) is.call(e) && as.character(e[[1]])[1] %in% run_calls
  quietly(for (e in exprs) if (!is_run_call(e)) eval(e, envir = env))
  env$Sys.sleep <- function(...) invisible(NULL)   # no waiting in tests
  env$real_comtrade_request <- env$comtrade_request  # kept for the URL test
  env$comtrade_request <- function(...) stop("no API stub installed for this test")
  env
}

# Comtrade-shaped rows (the columns the pipeline reads, in API order)
fake_rows <- function(reporter, year, codes, partners, months = 1:12) {
  g <- expand.grid(ref_month = months, cmd_code = codes, partner_iso = partners,
                   flow_desc = c("Export", "Import"), stringsAsFactors = FALSE)
  data.frame(type_code = "C", freq_code = "M", ref_year = year,
             ref_month = g$ref_month, reporter_iso = reporter,
             flow_code = ifelse(g$flow_desc == "Export", "X", "M"),
             flow_desc = g$flow_desc,
             partner_code = ifelse(g$partner_iso == "W00", 0L, 100L),
             partner_iso = g$partner_iso,
             cmd_code = g$cmd_code, primary_value = 1000,
             stringsAsFactors = FALSE)
}

# httr2-style HTTP error condition, as comtradr raises it
http_error <- function(status, text = "Error") {
  rlang::abort(sprintf("HTTP %d %s.", status, text),
               class = c(paste0("httr2_http_", status), "httr2_http", "httr2_error"),
               status = status)
}

# Load only the function definitions and constants from R/ingest_comtrade_build.R
# (skips reading the crosswalk and the run block at the bottom of the script).
load_build_functions <- function() {
  env   <- new.env(parent = globalenv())
  exprs <- parse(repo_file("R", "ingest_comtrade_build.R"), keep.source = FALSE)
  keep  <- "^(library\\((tidyverse|DBI|duckdb|arrow|glue)\\)|CIF_FOB_OUTLIER_BOUNDS <-|leading_zero_cmd_codes <-|build_[a-z_]+ <-|run_comtrade_build <-)"
  for (e in exprs) {
    if (grepl(keep, paste(deparse(e)[1], collapse = "")))
      suppressPackageStartupMessages(eval(e, envir = env))
  }
  env
}

# One country-year CSV in the current Comtrade column layout. `rows` may carry
# a cmd_code column; without it every row is wheat (1001).
write_trade_csv <- function(dir, reporter, year, rows) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  df <- data.frame(
    type_code = "C", ref_year = year, ref_month = rows$mon,
    reporter_iso = reporter, reporter_desc = paste("Country", reporter),
    flow_desc = rows$flow_desc, partner_iso = rows$partner,
    partner_desc = paste("Country", rows$partner),
    cmd_code = if (is.null(rows$cmd_code)) "1001" else rows$cmd_code, cmd_desc = "Wheat",
    qty_unit_code = 8L, qty = 10, is_qty_estimated = FALSE, net_wgt = 10,
    cifvalue = NA, fobvalue = NA, primary_value = rows$value)
  path <- file.path(dir, sprintf("%s_%d.csv", reporter, year))
  readr::write_csv(df, path)
  path
}
