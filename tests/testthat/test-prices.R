# Tests for the price step's helpers: R/deflator.R, R/pink_sheet.R and
# R/price_helpers.R. Offline: no network, no data/ folder.

source(repo_file("R", "deflator.R"), local = TRUE)
source(repo_file("R", "pink_sheet.R"), local = TRUE)
source(repo_file("R", "price_helpers.R"), local = TRUE)

test_that("monthly MUV has no January step, averages its annual value, and flags months past the data", {
  annual <- data.frame(year = 2022:2025, muv = c(90, 100, 110, 121))
  m <- muv_monthly(annual, 2022:2026, base_year = 2024)

  # 2024 = 100 on the annual value; the monthly mean lands within 0.5%
  expect_lt(abs(mean(m$muv_idx_2024[m$yr == 2024]) - 100), 0.5)
  # No step at the turn of the year: Dec -> Jan moves like any other month
  ch   <- diff(log(m$muv_idx_2024[m$yr %in% 2023:2024]))
  jan  <- ch[12]                                   # Dec 2023 -> Jan 2024
  expect_lt(abs(jan - median(ch)), 1e-3)
  # Past the last mid-year point (mid-2025) the level holds and is provisional
  expect_true(all(m$deflator_provisional[m$yr == 2026]))
  expect_false(any(m$deflator_provisional[m$yr == 2024]))
  expect_equal(length(unique(round(m$muv_idx_2024[m$yr == 2026], 8))), 1)
  expect_equal(m$muv_idx_2024[m$yr == 2026][1], 121 / 110 * 100)
})

test_that("rounded Pink Sheet values take the archived full precision; revisions do not", {
  cur <- data.frame(yr = 2015L, mon = 1:4, series_name = "Sugar, world",
                    price_wb = c(0.34, 174.7, 0.35, 48.1))
  old <- data.frame(yr = 2015L, mon = 1:4, series_name = "Sugar, world",
                    price_wb = c(0.338188708, 174.7073104, 0.338188708, 48.07))
  s <- splice_pink_sheet(cur, list(old))
  expect_equal(s$price_wb, c(0.338188708, 174.7073104, 0.35, 48.07))
  expect_equal(s$precision, c(rep("archived full precision", 2), "current file",
                              "archived full precision"))
})

test_that("unit values are medians over self-reported, measured quantities", {
  root <- withr::local_tempdir()
  d <- data.frame(
    iso3c = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF"), yr = 2025L, mon = 1L,
    cmd_code = "0904", flow_desc = "Import", partner_iso3c = "ZZZ",
    primary_value = c(100, 200, 300, 9999, 9999, 400),
    qty_comtrade = c(10, 10, 10, 1, 1, 10), qty_unit_code = 8L,
    is_qty_estimated = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    mirrored = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  arrow::write_dataset(d, root, partitioning = c("iso3c", "yr"))
  uv <- compute_unit_values(file.path(root, "**", "*.parquet"), "0904")
  # DDD (estimated) and EEE (mirrored) are left out: median of 10, 20, 30, 40
  expect_equal(uv$uv_price, 25)
  expect_equal(uv$uv_n, 4L)
  expect_equal(uv$uv_unit_code, 8L)
  expect_equal(nrow(compute_unit_values(file.path(root, "**", "*.parquet"), "0904", min_obs = 5L)), 0)
})

test_that("a discontinued series continues with its fallback, scaled to the overlap", {
  prices <- data.frame(cmd_code = "1003", yr = 2020L, mon = 1:8, price_wb = 100,
                       price_unit = "mt", price_origin = "WB")
  fred <- data.frame(pcps_code = "PBARL", yr = 2020L, mon = 1:12, value = 200)
  out <- apply_price_fallbacks(prices, data.frame(cmd_code = "1003", price_fallback = "FRED:PBARL"),
                               fred, uv = data.frame())
  added <- out[out$mon > 8, ]
  expect_equal(added$mon, 9:12)
  expect_equal(added$price_wb, rep(100, 4))           # 200 x the overlap ratio 0.5
  expect_equal(unique(added$price_origin), "FRED fallback")
})

test_that("enrichment prices coffee by exporter, uses unit values, and deflates with MUV", {
  d <- data.frame(
    iso3c = c("BRA", "DEU", "DEU", "DEU", "DEU"), yr = 2024L, mon = 6L,
    flow_desc = c("Export", "Import", "Import", "Import", "Import"),
    partner_iso3c = c("DEU", "BRA", "VNM", "FRA", "IND"),
    cmd_code = c("0901", "0901", "0901", "0901", "0904"),
    primary_value = c(500, 500, 500, 500, 1000), qty_comtrade = 100, qty_unit_code = 8L,
    net_wgt_comtrade = 100, mirrored = FALSE)
  coffee <- data.frame(yr = 2024L, mon = 6L, price_arabica = 5, price_robusta = 3,
                       price_avg_coffee = 4)
  uv  <- data.frame(cmd_code = "0904", yr = 2024L, mon = 6L, uv_unit_code = 8L, uv_price = 9)
  muv <- data.frame(yr = 2024L, mon = 6L, muv_idx_2024 = 125, deflator_provisional = FALSE)
  conv <- data.frame(qty_unit_code = 8L, price_unit = "kg", conv_factor = 1)
  prices <- data.frame(cmd_code = character(), yr = integer(), mon = integer(),
                       price_wb = numeric(), price_unit = character(), price_origin = character())

  e <- enrich_trade(d, prices, coffee, "0901", uv, muv, conv, arabica = "BRA", robusta = "VNM")
  expect_equal(e$price_wb, c(5, 5, 3, 4, 9))         # BRA export, BRA- and VNM-sourced imports, other
  expect_equal(e$price_origin[5], "UV")
  expect_equal(e$global_value_wb[5], 900)            # 100 kg x 9
  expect_equal(e$constant_value_2024, d$primary_value / 125 * 100)
  expect_false(any(c("cpi_std_2024", "use_uv") %in% names(e)))
})
