# Tests for R/ingest_views.R (offline: no API calls, no database).

views <- load_script("ingest_views.R", skip_calls = "run_views_ingest")

resp_new <- list(page_count = 1, start_date = 562, data = data.frame(
  country_id = 1:2, month_id = c(562, 563), name = c("Guyana", "Guyana"),
  gwcode = c(110, 110), isoab = "GUY", year = 2026, month = c(10, 11),
  main_mean = c(2.5, 0.1), main_dich = c(0.2, 0.01), main_mean_ln = c(1.25, 0.09)))

resp_old <- list(page_count = 1, start_date = 505, data = data.frame(
  country_id = 1, month_id = 507, name = "Guyana", gwcode = 110, isoab = "GUY",
  year = 2022, month = 3, sc_cm_sb_main = log(1 + 4), sc_cm_sb_dich_main = 0.3,
  sc_cm_sb_surrogate_pop = 0.1))

test_that("both field layouts standardize to pred_mean / pred_ln / pred_dich", {
  a <- views$parse_views_run(resp_new, "fatalities003_2026_09_t01")
  expect_equal(a$pred_mean, c(2.5, 0.1))
  expect_equal(a$pred_dich, c(0.2, 0.01))
  expect_equal(a$step, c(1L, 2L))
  expect_false(any(a$pred_mean_derived))
  expect_equal(unique(a$family), "fatalities003")

  b <- views$parse_views_run(resp_old, "fatalities001_2021_12_t01")
  expect_equal(b$pred_mean, 4)                    # expm1 of the log-scale value
  expect_equal(b$step, 3L)                        # month 507 in a window starting at 505
  expect_true(b$pred_mean_derived)
})

test_that("endpoints without forecasts (ns, os) yield nothing", {
  empty <- list(page_count = 1, start_date = 505, data = data.frame(
    country_id = 1, month_id = 505, name = "Guyana", gwcode = 110, isoab = "GUY",
    year = 2022, month = 1))
  expect_null(views$parse_views_run(empty, "fatalities001_2021_12_t01", type = "os"))
})

test_that("a paginated response is refused rather than silently truncated", {
  expect_error(views$parse_views_run(modifyList(resp_new, list(page_count = 3)), "x"),
               "paginated")
})

test_that("re-runs of the same window keep only the latest-named run as preferred", {
  df <- tibble::tibble(run = c("fatalities001_2021_12_t01", "fatalities001_2022_00_t01",
                               "fatalities002_2023_09_t01", "fatalities002_2023_09_t02"),
                       first_month_id = c(505L, 505L, 526L, 526L))
  out <- views$mark_preferred(df)
  expect_equal(out$preferred, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("countries map by gwcode, with ViEWS's ISO code as fallback", {
  frame <- tibble::tibble(gwcode = "110", iso3c = "GUY")
  df <- tibble::tibble(gwcode = c(110L, 999L), isoab = c("GUY", "XXA"))
  expect_equal(views$map_views_countries(df, frame)$country_code, c("GUY", "XXA"))
})
