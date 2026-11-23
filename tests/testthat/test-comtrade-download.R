# Tests for R/ingest_comtrade_download.R with a stubbed Comtrade API.

this_year <- as.integer(format(Sys.Date(), "%Y"))

setup_download <- function(n_codes = 10, chunk_size = NULL, n_ext = 0) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  csv_dir  <- file.path(dir, "csv")
  dir.create(csv_dir)
  env <- load_download_script(csv_dir, file.path(dir, "manifest.csv"), n_codes, n_ext)
  if (!is.null(chunk_size)) env$COMMODITY_CHUNK_SIZE <- chunk_size
  env$calls <- list()
  env
}

# Install a stub: `handler(iso3c, year, codes[, partner])` returns a data frame
# or raises. Handlers that take `partner` (or ...) also receive it.
stub_api <- function(env, handler) {
  takes_partner <- any(c("partner", "...") %in% names(formals(handler)))
  env$comtrade_request <- function(iso3c, year, codes, partner = "everything") {
    env$calls[[length(env$calls) + 1]] <- list(iso3c = iso3c, year = year,
                                               n = length(codes), partner = partner)
    if (takes_partner) handler(iso3c, year, codes, partner = partner)
    else handler(iso3c, year, codes)
  }
}

# A file with partner data, as an old download left it
write_old_csv <- function(env, iso3c, year, days_old = 200) {
  path <- file.path(env$COMTRADE_CSV_DIR, sprintf("%s_%d.csv", iso3c, year))
  writeLines(c("type_code,ref_year,ref_month,partner_iso", "C,1,1,ZAF"), path)
  Sys.setFileTime(path, Sys.time() - days_old * 86400)
  path
}

# A response the way Comtrade's gateway sends it
gateway_response <- function(status, retry_after = NULL, message = NULL) {
  headers <- list(`Content-Type` = "application/json")
  if (!is.null(retry_after)) headers$`Retry-After` <- as.character(retry_after)
  body <- if (is.null(message)) "{}" else
    sprintf('{"statusCode": %d, "message": "%s"}', status, message)
  httr2::response(status_code = status, headers = headers, body = charToRaw(body))
}

test_that("HTTP status is read from httr2 conditions and from plain messages", {
  env <- setup_download()
  e1 <- tryCatch(http_error(414, "URI Too Long"), error = function(e) e)
  e2 <- tryCatch(stop("HTTP 500 Internal Server Error."), error = function(e) e)
  e3 <- tryCatch(stop("timeout"), error = function(e) e)
  expect_identical(env$http_status_of(e1), 414L)
  expect_identical(env$http_status_of(e2), 500L)
  expect_identical(env$http_status_of(e3), NA_integer_)
})

test_that("comtradr still has the internal steps comtrade_request() uses, with these arguments", {
  ns <- asNamespace("comtradr")
  expect_true(all(c("type", "frequency", "commodity_classification", "commodity_code",
                    "flow_direction", "reporter", "partner", "start_date", "end_date",
                    "mode_of_transport", "partner_2", "customs_code", "update",
                    "verbose", "extra_params", "bulk") %in% names(formals(ns$ct_check_params))))
  expect_true(all(c("params", "primary_token", "verbose", "bulk") %in%
                    names(formals(ns$ct_build_request))))
  expect_true(all(c("resp", "verbose", "tidy_cols", "bulk") %in%
                    names(formals(ns$ct_process_response))))
})

test_that("requests leave the partner list out of the URL; World-only asks for partner 0", {
  env <- setup_download()
  urls <- character()
  env$perform_comtrade <- function(req, ...) { urls <<- c(urls, req$url); stop("captured") }
  try(env$real_comtrade_request("USA", 2024, c("1001", "1003")), silent = TRUE)
  try(env$real_comtrade_request(c("USA", "DEU"), 2024, "1001", partner = "World"), silent = TRUE)
  expect_length(urls, 2)
  expect_false(grepl("partnerCode", urls[1]))
  expect_match(urls[1], "reporterCode=842")
  expect_match(urls[1], "cmdCode=1001(,|%2C)1003")
  expect_match(urls[2], "partnerCode=0(&|$)")
  reporters <- strsplit(utils::URLdecode(sub(".*reporterCode=([^&]*).*", "\\1", urls[2])), ",")[[1]]
  expect_true(all(c("842", "276") %in% reporters))   # comtradr adds historical codes too
  expect_false(grepl("test-key-not-real", paste(urls, collapse = " ")))   # key is a header
})

test_that("only short waits are retried; a spent quota is not waited out", {
  env <- setup_download()
  expect_true(env$comtrade_is_transient(gateway_response(429L, retry_after = 1)))
  expect_true(env$comtrade_is_transient(gateway_response(429L, retry_after = 60)))
  expect_true(env$comtrade_is_transient(gateway_response(503L)))           # backoff
  expect_false(env$comtrade_is_transient(gateway_response(429L, retry_after = 20000)))
  expect_false(env$comtrade_is_transient(gateway_response(403L, retry_after = 20000)))
  expect_false(env$comtrade_is_transient(gateway_response(403L)))
  expect_false(env$comtrade_is_transient(gateway_response(414L)))
  expect_equal(env$retry_after_s(gateway_response(429L, retry_after = 7)), 7)
  expect_true(is.na(env$retry_after_s(gateway_response(429L))))
})

test_that("a quota response becomes a clean stop that keeps the gateway's message", {
  env <- setup_download()
  req <- httr2::request("https://comtradeapi.un.org/data/v1/get/C/M/HS")
  quota <- gateway_response(403L, retry_after = 20000,
    message = "Out of call volume quota. Quota will be replenished in 05:33:20.")
  e <- tryCatch(httr2::with_mocked_responses(\(req) quota, env$perform_comtrade(req, rate = NULL)),
                error = function(e) e)
  expect_identical(env$http_status_of(e), 403L)
  expect_match(conditionMessage(e), "replenished in 05:33:20")

  # and through the downloader: the run stops, logging the country-year as interrupted
  env$comtrade_request <- function(...) httr2::with_mocked_responses(\(req) quota,
    env$perform_comtrade(httr2::request("https://comtradeapi.un.org/x"), rate = NULL))
  out <- utils::capture.output(env$run_comtrade_download(this_year, countries = c("AFG", "AGO")))
  m <- env$read_manifest()
  expect_equal(m$status, "interrupted")
  expect_equal(m$iso3c, "AFG")                       # AGO never started
  expect_true(any(grepl("Stopped early: HTTP 403", out)))
})

test_that("only the historical partner set is kept: no World, self, or areas", {
  env <- setup_download()
  stub_api(env, \(iso3c, year, codes)
    fake_rows(iso3c, year, codes, c("W00", "AGO", "ZAF", "NAM", "S19", "_X")))
  quietly(env$run_comtrade_download(this_year, countries = "AGO"))

  out <- readr::read_csv(file.path(env$COMTRADE_CSV_DIR, sprintf("AGO_%d.csv", this_year)),
                         show_col_types = FALSE)
  expect_setequal(unique(out$partner_iso), c("ZAF", "NAM"))
  expect_equal(nrow(out), 10 * 12 * 2 * 2)     # codes x months x partners x flows
  m <- env$read_manifest()
  expect_equal(m$status, "ok")
  expect_equal(m$n_rows, nrow(out))
  expect_equal(m$n_months, 12L)
})

test_that("World totals go to their own file, never into the country-year CSV", {
  env <- setup_download(n_codes = 10)
  stub_api(env, \(iso3c, year, codes) {
    if (length(codes) > 5) http_error(414, "URI Too Long")    # forces a split
    fake_rows(iso3c, year, codes, c("W00", "ZAF", "S19"), months = 1:2)
  })
  quietly(env$run_comtrade_download(this_year, countries = "AGO"))

  main  <- readr::read_csv(file.path(env$COMTRADE_CSV_DIR, sprintf("AGO_%d.csv", this_year)),
                           show_col_types = FALSE)
  world <- readr::read_csv(file.path(env$COMTRADE_WORLD_DIR, sprintf("AGO_%d.csv", this_year)),
                           show_col_types = FALSE)
  expect_equal(unique(main$partner_iso), "ZAF")
  expect_equal(unique(world$partner_iso), "W00")
  expect_equal(nrow(world), 10L * 2L * 2L)                    # both halves kept
  expect_equal(sum(startsWith(readLines(file.path(env$COMTRADE_WORLD_DIR,
                                                   sprintf("AGO_%d.csv", this_year))),
                              "type_code")), 1L)
})

test_that("no World file is written when Comtrade has no data", {
  env <- setup_download()
  stub_api(env, \(iso3c, year, codes) data.frame(count = 0))
  quietly(env$run_comtrade_download(this_year, countries = "AFG"))
  expect_false(file.exists(file.path(env$COMTRADE_WORLD_DIR, sprintf("AFG_%d.csv", this_year))))
})

test_that("a re-download replaces the existing file in one piece", {
  env <- setup_download()
  months_now <- 1:3
  stub_api(env, \(iso3c, year, codes) fake_rows(iso3c, year, codes, "ZAF", months = months_now))
  quietly(env$run_comtrade_download(this_year, countries = "AGO"))
  path <- file.path(env$COMTRADE_CSV_DIR, sprintf("AGO_%d.csv", this_year))
  expect_equal(length(readLines(path)) - 1L, 10L * 3L * 2L)

  months_now <- 1:9                                   # Comtrade has added months
  quietly(env$run_comtrade_download(this_year, countries = "AGO", force = TRUE))
  out <- readr::read_csv(path, show_col_types = FALSE)
  expect_equal(sort(unique(out$ref_month)), 1:9)
  expect_equal(nrow(out), 10L * 9L * 2L)
  expect_equal(env$read_manifest()$max_month, 9L)
  expect_false(file.exists(paste0(path, ".tmp")))
})

test_that("Comtrade's 'no data' answer is recorded as no_data and nothing is written", {
  env <- setup_download()
  stub_api(env, \(iso3c, year, codes) data.frame(count = 0))
  # an empty marker left by the old script's failed attempt
  marker <- file.path(env$COMTRADE_CSV_DIR, sprintf("AFG_%d.csv", this_year))
  file.create(marker)
  quietly(env$run_comtrade_download(this_year, countries = "AFG"))

  expect_false(file.exists(marker))
  m <- env$read_manifest()
  expect_equal(m$status, "no_data")
  expect_equal(m$n_rows, 0L)
})

test_that("HTTP 414 halves the commodity chunk and still writes one complete file", {
  env <- setup_download(n_codes = 10)
  stub_api(env, \(iso3c, year, codes) {
    if (length(codes) > 5) http_error(414, "URI Too Long")
    fake_rows(iso3c, year, codes, "ZAF", months = 1:3)
  })
  quietly(env$run_comtrade_download(this_year, countries = "ALB"))

  expect_equal(vapply(env$calls, `[[`, integer(1), "n"), c(10L, 5L, 5L))
  path <- file.path(env$COMTRADE_CSV_DIR, sprintf("ALB_%d.csv", this_year))
  lines <- readLines(path)
  expect_equal(sum(startsWith(lines, "type_code")), 1L)   # exactly one header
  expect_equal(length(lines) - 1L, 10L * 3L * 2L)
  m <- env$read_manifest()
  expect_equal(c(m$status, m$calls), c("ok", "3"))
})

test_that("a call that reaches the row cap is split, not accepted truncated", {
  env <- setup_download(n_codes = 10)
  env$MAX_ROWS_PER_CALL <- 100
  stub_api(env, \(iso3c, year, codes) fake_rows(iso3c, year, codes, "ZAF", months = 1:4))
  # 10 codes x 4 months x 2 flows = 80 rows: under the cap of 100, one call
  quietly(env$run_comtrade_download(this_year, countries = "ALB"))
  expect_length(env$calls, 1)

  env2 <- setup_download(n_codes = 10)
  env2$MAX_ROWS_PER_CALL <- 50
  stub_api(env2, \(iso3c, year, codes) fake_rows(iso3c, year, codes, "ZAF", months = 1:4))
  quietly(env2$run_comtrade_download(this_year, countries = "ALB"))
  expect_equal(vapply(env2$calls, `[[`, integer(1), "n"), c(10L, 5L, 5L))
  expect_equal(env2$read_manifest()$n_rows, 80L)
})

test_that("a failed chunk writes nothing, keeps the old file, and records error", {
  env <- setup_download(n_codes = 10, chunk_size = 5)
  path <- file.path(env$COMTRADE_CSV_DIR, sprintf("ARE_%d.csv", this_year))
  writeLines(c("type_code,ref_year,ref_month,partner_iso", "C,2026,1,ZAF"), path)
  before <- readLines(path)
  stub_api(env, \(iso3c, year, codes) {
    if (length(env$calls) == 2) stop("HTTP 500 Internal Server Error.")
    fake_rows(iso3c, year, codes, "ZAF")
  })
  quietly(env$run_comtrade_download(this_year, countries = "ARE", force = TRUE))

  expect_identical(readLines(path), before)
  expect_false(file.exists(paste0(path, ".tmp")))
  m <- env$read_manifest()
  expect_equal(m$status, "error")
  expect_equal(m$http_status, 500L)
})

test_that("a quota error stops the run and the next run resumes where it stopped", {
  env <- setup_download(n_codes = 10, chunk_size = 5)
  quota_spent <- TRUE
  stub_api(env, \(iso3c, year, codes) {
    if (iso3c == "ARG" && quota_spent) http_error(429, "Too Many Requests")
    fake_rows(iso3c, year, codes, "ZAF", months = 1:2)
  })
  countries <- c("AFG", "AGO", "ARG", "ARM")
  quietly(env$run_comtrade_download(this_year, countries = countries))

  m <- env$read_manifest()
  status <- setNames(m$status, m$iso3c)
  expect_equal(status[c("AFG", "AGO", "ARG")],
               c(AFG = "ok", AGO = "ok", ARG = "interrupted"))   # ARM never started
  expect_false("ARM" %in% m$iso3c)
  expect_false(any(file.exists(file.path(env$COMTRADE_CSV_DIR,
                                         sprintf("%s_%d.csv", c("ARG", "ARM"), this_year)))))

  quota_spent <- FALSE
  env$calls <- list()
  quietly(env$run_comtrade_download(this_year, countries = countries))
  expect_setequal(vapply(env$calls, `[[`, character(1), "iso3c"), c("ARG", "ARM"))
  expect_setequal(env$read_manifest()$iso3c, countries)
})

test_that("the call budget stops before a country-year is left half-done", {
  env <- setup_download(n_codes = 10, chunk_size = 5)    # 2 calls per country-year
  stub_api(env, \(iso3c, year, codes) fake_rows(iso3c, year, codes, "ZAF", months = 1))
  quietly(env$run_comtrade_download(this_year, countries = c("AFG", "AGO"), call_budget = 3))

  expect_length(env$calls, 3)
  m <- env$read_manifest()
  expect_equal(m$status[m$iso3c == "AFG"], "ok")
  expect_equal(m$status[m$iso3c == "AGO"], "interrupted")   # its one call is logged
  expect_equal(m$calls[m$iso3c == "AGO"], 1L)
  expect_false(file.exists(file.path(env$COMTRADE_CSV_DIR, sprintf("AGO_%d.csv", this_year))))
})

test_that("runs on the same day share one call budget", {
  env <- setup_download(n_codes = 10, chunk_size = 5)    # 2 calls per country-year
  stub_api(env, \(iso3c, year, codes) fake_rows(iso3c, year, codes, "ZAF", months = 1))
  quietly(env$run_comtrade_download(this_year, countries = "AFG", call_budget = 3))
  expect_length(env$calls, 2)

  quietly(env$run_comtrade_download(this_year, countries = "AGO", call_budget = 3))
  expect_length(env$calls, 3)                             # only 1 call was left
  expect_false(file.exists(file.path(env$COMTRADE_CSV_DIR, sprintf("AGO_%d.csv", this_year))))

  quietly(env$run_comtrade_download(this_year, countries = "AGO", call_budget = 3))
  expect_length(env$calls, 3)                             # budget spent: no call at all
})

test_that("bootstrap classifies existing files; closed years are skipped, recent ones refreshed", {
  env <- setup_download()
  d <- env$COMTRADE_CSV_DIR
  closed <- this_year - 3L
  recent <- this_year - 1L
  writeLines(c("type_code,ref_year,ref_month", "C,1,1"), file.path(d, sprintf("ARE_%d.csv", closed)))
  writeLines(c("count", "0", "0"),                       file.path(d, sprintf("ARG_%d.csv", closed)))
  file.create(file.path(d, sprintf("ARM_%d.csv", closed)))                         # empty marker
  aus <- file.path(d, sprintf("AUS_%d.csv", recent))
  writeLines(c("type_code,ref_year,ref_month", "C,1,1"), aus)
  Sys.setFileTime(aus, Sys.time() - 60 * 86400)                                   # 60 days old

  m <- env$bootstrap_manifest(env$read_manifest())
  status <- setNames(m$status, m$iso3c)
  expect_equal(status[c("ARE", "ARG", "ARM", "AUS")],
               c(ARE = "ok", ARG = "no_data", ARM = "error", AUS = "ok"))

  plan <- env$plan_downloads(c("ARE", "ARG", "ARM", "AUS"), c(closed, recent), m)
  reasons <- setNames(plan$reason, paste(plan$iso3c, plan$year))
  expect_equal(reasons[[sprintf("ARM %d", closed)]], "retry after error")
  expect_equal(reasons[[sprintf("AUS %d", recent)]], "refresh recent year")
  expect_false(sprintf("ARE %d", closed) %in% names(reasons))    # closed + ok: skip
  expect_false(sprintf("ARG %d", closed) %in% names(reasons))    # closed + no_data: skip

  plan2 <- env$plan_downloads("ARG", closed, m, retry_no_data = TRUE)
  expect_equal(plan2$reason, "re-check no data")
})

test_that("a refresh that returns no rows keeps the previous file", {
  env <- setup_download()
  recent <- this_year - 1L
  path <- file.path(env$COMTRADE_CSV_DIR, sprintf("AUS_%d.csv", recent))
  writeLines(c("type_code,ref_year,ref_month,partner_iso", "C,1,1,ZAF"), path)
  Sys.setFileTime(path, Sys.time() - 60 * 86400)
  before <- readLines(path)
  stub_api(env, \(iso3c, year, codes) data.frame(count = 0))
  quietly(env$run_comtrade_download(recent, countries = "AUS"))

  expect_identical(readLines(path), before)
  m <- env$read_manifest()
  expect_equal(m$status, "ok")
  expect_match(m$message, "previous file kept")
})

test_that("dry_run plans without calling the API or writing a manifest", {
  env <- setup_download()
  stub_api(env, \(...) stop("should not be called"))
  plan <- quietly(env$run_comtrade_download(this_year, countries = c("AFG", "AGO"), dry_run = TRUE))
  expect_equal(nrow(plan), 2L)
  expect_length(env$calls, 0)
  expect_false(file.exists(env$COMTRADE_MANIFEST))
})

test_that("refresh_before re-downloads an older file once, then leaves it", {
  env <- setup_download()
  closed <- this_year - 3L
  write_old_csv(env, "THA", closed)
  stub_api(env, \(iso3c, year, codes) fake_rows(iso3c, year, codes, "ZAF"))
  cutoff <- format(Sys.Date() - 1, "%Y-%m-%d")

  quietly(env$run_comtrade_download(closed, countries = "THA"))
  expect_length(env$calls, 0)                         # closed year, ok: normally skipped
  quietly(env$run_comtrade_download(closed, countries = "THA", refresh_before = cutoff))
  expect_length(env$calls, 1)
  expect_equal(env$read_manifest()$n_rows, 10L * 12L * 2L)
  quietly(env$run_comtrade_download(closed, countries = "THA", refresh_before = cutoff))
  expect_length(env$calls, 1)                         # done: not downloaded again
})

test_that("World-only batches write a World file per reporter and log each call once", {
  env <- setup_download(n_codes = 10, chunk_size = 5)  # 2 calls per batch
  closed <- this_year - 3L
  for (r in c("AFG", "ALB", "ARE")) write_old_csv(env, r, closed)
  writeLines(c("count", "0", "0"), file.path(env$COMTRADE_CSV_DIR, sprintf("AGO_%d.csv", closed)))
  stub_api(env, \(iso3c, year, codes, partner) {
    stopifnot(partner == "World")
    do.call(rbind, lapply(setdiff(iso3c, "ARE"),       # ARE has no World rows
                          \(r) fake_rows(r, year, codes, "W00", months = 1:2)))
  })
  countries <- c("AFG", "AGO", "ALB", "ARE")
  quietly(env$run_comtrade_world_download(closed, countries = countries, reporters_per_call = 2))

  expect_equal(lapply(env$calls, `[[`, "iso3c"),
               list(c("AFG", "ALB"), c("AFG", "ALB"), "ARE", "ARE"))  # AGO: no partner data
  world <- \(r) file.path(env$COMTRADE_WORLD_DIR, sprintf("%s_%d.csv", r, closed))
  afg <- readr::read_csv(world("AFG"), show_col_types = FALSE)
  expect_equal(unique(afg$reporter_iso), "AFG")
  expect_equal(nrow(afg), 10L * 2L * 2L)               # codes x months x flows
  expect_true(file.exists(world("ALB")))
  expect_false(file.exists(world("ARE")))
  wm <- env$read_manifest(env$COMTRADE_WORLD_MANIFEST)
  status <- setNames(wm$status, wm$iso3c)
  expect_equal(status[c("AFG", "ALB", "ARE")], c(AFG = "ok", ALB = "ok", ARE = "no_data"))
  expect_equal(sum(wm$calls), 4L)                       # each call counted once
  expect_identical(readLines(file.path(env$COMTRADE_CSV_DIR, sprintf("AFG_%d.csv", closed))),
                   c("type_code,ref_year,ref_month,partner_iso", "C,1,1,ZAF"))  # untouched

  quietly(env$run_comtrade_world_download(closed, countries = countries, reporters_per_call = 2))
  expect_length(env$calls, 4)                           # nothing left to fetch

  # the World calls count against the same daily budget
  stub_api(env, \(iso3c, year, codes) fake_rows(iso3c, year, codes, "ZAF"))
  quietly(env$run_comtrade_download(this_year, countries = "AFG", call_budget = 5))
  expect_length(env$calls, 5)
  expect_equal(env$read_manifest()$status[env$read_manifest()$year == this_year], "interrupted")
})

test_that("check_world_totals compares World-only with the main download's World rows, once", {
  env <- setup_download(n_codes = 10, chunk_size = 5)
  stub_api(env, \(iso3c, year, codes, partner) {
    partners <- if (partner == "World") "W00" else c("W00", "ZAF")
    fake_rows(iso3c, year, codes, partners, months = 1:3)
  })
  quietly(env$run_comtrade_download(this_year, countries = "PHL"))   # writes its World file
  expect_length(env$calls, 2)

  ok <- quietly(env$check_world_totals("PHL", this_year))
  expect_true(ok)
  expect_length(env$calls, 4)
  expect_equal(vapply(env$calls[3:4], `[[`, character(1), "partner"), c("World", "World"))
  wm <- env$read_manifest(env$COMTRADE_WORLD_MANIFEST)
  expect_equal(wm$source, "check")
  expect_match(wm$message, "^match")

  expect_true(quietly(env$check_world_totals("PHL", this_year)))     # reused, no calls
  expect_length(env$calls, 4)
  expect_true(is.na(quietly(env$check_world_totals("AFG", this_year))))  # nothing to compare
})

test_that("the old entry point explains the new one", {
  env <- setup_download()
  expect_error(env$download_comtrade_data(), "run_comtrade_download")
})

# -- Extension code set (D-027) -------------------------------------------------

test_that("a code file without code_set is all core; with it, core and ext are kept apart", {
  env <- setup_download(n_codes = 10)
  expect_length(env$codes_in_set("core"), 10)
  expect_length(env$codes_in_set("ext"), 0)

  env2 <- setup_download(n_codes = 10, n_ext = 4)
  expect_equal(env2$commodity_codes, env2$codes_in_set("core"))
  expect_length(env2$commodity_codes, 10)
  expect_equal(env2$codes_in_set("ext"), sprintf("27%02d", 1:4))
})

test_that("extension downloads use their own folders and manifest and leave core untouched", {
  env <- setup_download(n_codes = 10, n_ext = 4)
  stub_api(env, \(iso3c, year, codes) fake_rows(iso3c, year, codes, c("ZAF", "W00"), months = 1:2))
  quietly(env$run_comtrade_ext_download(this_year, countries = "AFG"))

  expect_length(env$calls, 1)
  expect_equal(env$calls[[1]]$n, 4L)                      # only the extension codes
  ext_csv <- file.path(env$COMTRADE_EXT_CSV_DIR, sprintf("AFG_%d.csv", this_year))
  expect_true(file.exists(ext_csv))
  expect_setequal(unique(readr::read_csv(ext_csv, show_col_types = FALSE)$cmd_code), 2701:2704)
  expect_true(file.exists(file.path(env$COMTRADE_EXT_WORLD_DIR, sprintf("AFG_%d.csv", this_year))))
  expect_equal(env$read_manifest(env$COMTRADE_EXT_MANIFEST)$status, "ok")

  # Core is untouched, and the settings are restored after the call
  expect_false(file.exists(env$COMTRADE_MANIFEST))
  expect_length(list.files(env$COMTRADE_CSV_DIR), 0)
  expect_equal(env$COMTRADE_CSV_DIR, env$CODE_SETS$core$csv_dir)
  expect_length(env$commodity_codes, 10)
})

test_that("settings are restored even when an extension run fails", {
  env <- setup_download(n_codes = 10, n_ext = 4)
  expect_error(env$run_comtrade_ext_download(this_year, countries = "XXX"), "Not in the country frame")
  expect_equal(env$COMTRADE_MANIFEST, env$CODE_SETS$core$manifest)
  expect_length(env$commodity_codes, 10)
})

test_that("core and extension runs on the same day share one call budget", {
  env <- setup_download(n_codes = 10, chunk_size = 5, n_ext = 4)   # core: 2 calls, ext: 1
  stub_api(env, \(iso3c, year, codes) fake_rows(iso3c, year, codes, "ZAF", months = 1))
  quietly(env$run_comtrade_download(this_year, countries = "AFG", call_budget = 3))
  expect_length(env$calls, 2)
  quietly(env$run_comtrade_ext_download(this_year, countries = c("AFG", "AGO"), call_budget = 3))
  expect_length(env$calls, 3)                             # one call was left for the extension
  m <- env$read_manifest(env$COMTRADE_EXT_MANIFEST)
  expect_equal(m$status[m$iso3c == "AFG"], "ok")
  expect_false("AGO" %in% m$iso3c)
})

test_that("a country whose earlier download filled the row cap is split ahead of time", {
  env <- setup_download(n_codes = 10)
  m <- env$manifest_row(c("ALB", "DEU"), this_year - 1, "ok", n_rows = c(500L, 250000L))
  expect_equal(env$chunk_size_for("AGO", m, env$commodity_codes), env$COMMODITY_CHUNK_SIZE)  # no history
  expect_equal(env$chunk_size_for("ALB", m, env$commodity_codes), 10L)        # all 10 in one call
  # 250,000 rows need ceiling(250000 / 90000) = 3 calls: 55 codes -> 19 per call
  expect_equal(env$chunk_size_for("DEU", m, sprintf("%04d", 1:55)), 19L)
})
