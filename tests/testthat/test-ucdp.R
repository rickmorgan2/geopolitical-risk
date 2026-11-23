# Tests for R/ingest_ucdp.R (offline: no downloads, no database).

ucdp <- load_script("ingest_ucdp.R", skip_calls = "run_ucdp_ingest")

ucdp_row <- function(...) {
  v <- setNames(rep("", length(ucdp$UCDP_COLUMNS)), ucdp$UCDP_COLUMNS)
  vals <- c(...)
  v[names(vals)] <- vals
  v
}

test_that("file names follow UCDP's patterns", {
  expect_equal(ucdp$ged_zip_name("26.1"), "ged261-csv.zip")
  expect_equal(ucdp$candidate_release_name(2026, 9), "GEDEvent_v26_0_9.csv")
  expect_equal(ucdp$candidate_ytd_name(2026, 9), "GEDEvent_v26_01_26_09.csv")
})

test_that("files with and without a header row read the same", {
  dir <- withr::local_tempdir()
  r <- ucdp_row(id = "1", relid = "UKR-2024-1-1", year = "2024", code_status = "Clear",
                type_of_violence = "1", country = "Ukraine", country_id = "369",
                date_start = "2024-03-05 00:00:00.000", date_end = "2024-03-06 00:00:00.000",
                best = "10", low = "8", high = "12", source_headline = "Shelling, Kharkiv")
  with_header <- file.path(dir, "a.csv")
  utils::write.csv(as.data.frame(t(r)), with_header, row.names = FALSE)
  no_header <- file.path(dir, "b.csv")
  writeLines(paste0('"', r, '"', collapse = ","), no_header)

  a <- ucdp$read_ucdp_csv(with_header)
  b <- ucdp$read_ucdp_csv(no_header)
  expect_equal(a, b)
  expect_equal(names(a), ucdp$KEEP_COLUMNS)
  expect_identical(a$best, 10L)
  expect_identical(a$type_of_violence, 1L)
  expect_equal(a$date_start, as.Date("2024-03-05"))
})

test_that("countries map through the frame's gwcode, then countrycode", {
  frame <- tibble::tibble(gwcode = c("369", "365"), iso3c = c("UKR", "RUS"))
  out <- ucdp$map_countries(tibble::tibble(country_id = c(369L, 365L, 2L, 9999L)), frame)
  expect_equal(out$country_code, c("UKR", "RUS", "USA", NA))
})

test_that("events aggregate to zero-filled country-months by date_start and type", {
  events <- tibble::tibble(
    country_code     = c("UKR", "UKR", "UKR", "RUS", "FRA"),
    date_start       = as.Date(c("2024-01-10", "2024-01-20", "2024-03-01", "2024-02-01", "2024-02-02")),
    type_of_violence = c(1L, 1L, 3L, 2L, 1L),
    best = c(5L, 7L, 2L, 1L, 9L), low = c(4L, 6L, 2L, 1L, 9L), high = c(6L, 9L, 3L, 2L, 9L))
  cm <- ucdp$aggregate_country_month(events, c("UKR", "RUS", "SYR"),
                                     "2024-01-01", "2024-03-01", "ged", "26.1")
  pick <- function(cc, m, t) cm[cm$country_code == cc & cm$month == m & cm$type == t, ]

  expect_equal(nrow(cm), 3 * 3 * 3)                        # countries x months x types
  expect_equal(pick("UKR", 1, "sb")$fatalities_best, 12L)
  expect_equal(pick("UKR", 1, "sb")$fatalities_high, 15L)
  expect_equal(pick("UKR", 1, "sb")$n_events, 2L)
  expect_equal(pick("UKR", 3, "os")$fatalities_best, 2L)
  expect_equal(pick("RUS", 2, "ns")$fatalities_best, 1L)
  expect_equal(pick("UKR", 2, "sb")$fatalities_best, 0L)   # a true zero
  expect_true(all(cm$fatalities_best[cm$country_code == "SYR"] == 0L))
  expect_false("FRA" %in% cm$country_code)                 # outside the country list
})

test_that("the Candidate state: year-to-date file wins inside its window", {
  releases <- tibble::tibble(
    id = c("a", "a", "b", "c"), release_year = 2026L, release_month = c(1L, 2L, 1L, 1L),
    date_start = as.Date(c("2026-01-05", "2026-01-05", "2026-01-09", "2025-12-30")),
    best = c(1L, 5L, 3L, 4L))
  ytd <- tibble::tibble(id = c("a", "d"), date_start = as.Date(c("2026-01-05", "2026-01-20")),
                        best = c(6L, 2L), window_year = 2026L, window_month = 1L)

  st <- ucdp$latest_candidate_state(releases, ytd)
  expect_setequal(st$id, c("a", "c", "d"))                 # "b" was dropped in the revision
  expect_equal(st$best[st$id == "a"], 6L)                  # revised value
  expect_equal(st$best[st$id == "c"], 4L)                  # outside the window: release value

  st2 <- ucdp$latest_candidate_state(releases, NULL)
  expect_setequal(st2$id, c("a", "b", "c"))
  expect_equal(st2$best[st2$id == "a"], 5L)                # latest release wins
})
