# Tests for run_comtrade_build() in R/ingest_comtrade_build.R on a tiny
# synthetic world: AAA and BBB report, CCC and DDD never do (mirror-only).

build_env <- load_build_functions()

commodity_table <- data.frame(cmd_code = "1001", commodity_name = "wheat",
                              category = "Food", subcategory = "Cereals",
                              parent_code = NA_character_)

rows_of <- function(mon, flow_desc, partner, value, cmd_code = "1001")
  data.frame(mon = mon, flow_desc = flow_desc, partner = partner, value = value,
             cmd_code = cmd_code)

new_dirs <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dirs <- list(csv = file.path(root, "csv"), ext = file.path(root, "csv_ext"),
               parquet = file.path(root, "parquet"), summary = file.path(root, "summary"))
  dir.create(dirs$csv)
  dirs
}

run_build <- function(dirs, force = FALSE, codes = commodity_table) {
  quietly(build_env$run_comtrade_build(
    csv_dirs = c(dirs$csv, dirs$ext), parquet_dir = dirs$parquet, summary_dir = dirs$summary,
    commodity_type_data = codes, force_update = force))
}

read_parquet_rows <- function(dir) {
  arrow::open_dataset(dir) |> dplyr::collect() |>
    dplyr::mutate(yr = as.integer(as.character(yr)))
}

test_that("mirrors fill only country-months without self-reported data, and refresh correctly", {
  dirs <- new_dirs()

  # AAA reports Jan-Mar: exports to BBB (100) and CCC (200), DDD in Jan (70);
  # imports from BBB (50). BBB reports Jan-Feb only.
  write_trade_csv(dirs$csv, "AAA", 2025, rbind(
    rows_of(1:3, "Export", "BBB", 100),
    rows_of(1:3, "Export", "CCC", 200),
    rows_of(1,   "Export", "DDD", 70),
    rows_of(1:3, "Import", "BBB", 50)))
  write_trade_csv(dirs$csv, "BBB", 2025, rbind(
    rows_of(1:2, "Export", "AAA", 55),
    rows_of(1:2, "Import", "AAA", 110)))
  # Non-data files the build must ignore
  writeLines(c("count", "0", "0"), file.path(dirs$csv, "CCC_2025.csv"))
  file.create(file.path(dirs$csv, "DDD_2025.csv"))

  run_build(dirs)
  p <- read_parquet_rows(dirs$parquet)

  # BBB: Jan-Feb self-reported, March mirrored from AAA (CIF/FOB ratio 110/100)
  bbb <- dplyr::filter(p, iso3c == "BBB")
  expect_equal(sum(!bbb$mirrored), 4)
  expect_equal(sort(unique(bbb$mon[bbb$mirrored])), 3)
  expect_equal(bbb$primary_value[bbb$mirrored & bbb$flow_desc == "Import"], 110)
  # Values stay nominal: the price step deflates them (D-025)
  expect_false("constant_value_2024" %in% names(p))
  # CCC and DDD exist only as mirrors
  expect_true(all(dplyr::filter(p, iso3c %in% c("CCC", "DDD"))$mirrored))
  expect_equal(nrow(dplyr::filter(p, iso3c == "CCC")), 3)
  expect_equal(nrow(dplyr::filter(p, iso3c == "DDD")), 1)

  # Refresh: BBB now reports March and its exports to CCC; AAA's revised file
  # drops DDD. Newer CSVs must be reprocessed and every 2025 mirror rebuilt.
  Sys.sleep(1.1)
  write_trade_csv(dirs$csv, "AAA", 2025, rbind(
    rows_of(1:3, "Export", "BBB", 100),
    rows_of(1:3, "Export", "CCC", 200),
    rows_of(1:3, "Import", "BBB", 50)))
  write_trade_csv(dirs$csv, "BBB", 2025, rbind(
    rows_of(1:3, "Export", "AAA", 55),
    rows_of(1:3, "Import", "AAA", 110),
    rows_of(1:3, "Export", "CCC", 30)))

  run_build(dirs)
  p2 <- read_parquet_rows(dirs$parquet)

  bbb2 <- dplyr::filter(p2, iso3c == "BBB")
  expect_false(any(bbb2$mirrored))                       # March is self-reported now
  expect_equal(nrow(bbb2), 9)
  ccc2 <- dplyr::filter(p2, iso3c == "CCC")
  expect_true(all(ccc2$mirrored))
  expect_equal(nrow(ccc2), 6)                            # AAA + BBB reports, no duplicates
  expect_setequal(ccc2$partner_iso3c, c("AAA", "BBB"))
  expect_equal(nrow(dplyr::filter(p2, iso3c == "DDD")), 0)   # emptied partition removed
  expect_false(dir.exists(file.path(dirs$parquet, "iso3c=DDD", "yr=2025")))
  # Nothing is left behind in staging or the trash folder
  expect_false(dir.exists(paste0(dirs$parquet, "_staging")))
  expect_false(dir.exists(file.path(dirname(dirs$parquet), "_trash_parquet")))
  expect_false(file.exists(file.path(dirs$summary, "mirror_pending.txt")))
})

test_that("an unchanged CSV is not reprocessed", {
  dirs <- new_dirs()
  write_trade_csv(dirs$csv, "AAA", 2025, rows_of(1:2, "Export", "BBB", 100))
  write_trade_csv(dirs$csv, "BBB", 2025, rows_of(1:2, "Import", "AAA", 110))
  run_build(dirs)
  first <- file.mtime(list.files(dirs$parquet, recursive = TRUE, full.names = TRUE))

  Sys.sleep(1.1)
  res <- run_build(dirs)
  expect_equal(res$message, "No updates needed")
  expect_equal(file.mtime(list.files(dirs$parquet, recursive = TRUE, full.names = TRUE)), first)
})

test_that("a country that reports only one flow in a month gets the other flow mirrored", {
  dirs <- new_dirs()
  # AAA reports only its exports in January; BBB reports exports to AAA
  write_trade_csv(dirs$csv, "AAA", 2025, rows_of(1, "Export", "BBB", 100))
  write_trade_csv(dirs$csv, "BBB", 2025, rbind(rows_of(1, "Export", "AAA", 40),
                                               rows_of(1, "Import", "AAA", 110)))
  run_build(dirs)
  aaa <- dplyr::filter(read_parquet_rows(dirs$parquet), iso3c == "AAA")
  expect_equal(sum(!aaa$mirrored & aaa$flow_desc == "Export"), 1)
  expect_equal(sum(aaa$mirrored & aaa$flow_desc == "Import"), 1)   # from BBB's exports
  expect_equal(sum(aaa$mirrored & aaa$flow_desc == "Export"), 0)   # its own exports stay
})

test_that("core and extension CSVs of a country-year are read together", {
  dirs  <- new_dirs()
  codes <- rbind(commodity_table,
                 data.frame(cmd_code = "2710", commodity_name = "refined petroleum products",
                            category = "Energy", subcategory = "Energy", parent_code = NA))
  write_trade_csv(dirs$csv, "AAA", 2025, rows_of(1, "Export", "BBB", 100))
  write_trade_csv(dirs$ext, "AAA", 2025, rows_of(1, "Import", "BBB", 300, cmd_code = "2710"))
  run_build(dirs, codes = codes)
  aaa <- dplyr::filter(read_parquet_rows(dirs$parquet), iso3c == "AAA", !mirrored)
  expect_setequal(aaa$cmd_code, c("1001", "2710"))

  # A newer extension file alone triggers reprocessing of that country-year
  Sys.sleep(1.1)
  write_trade_csv(dirs$ext, "AAA", 2025, rows_of(1, "Import", "BBB", 333, cmd_code = "2710"))
  run_build(dirs, codes = codes)
  aaa2 <- dplyr::filter(read_parquet_rows(dirs$parquet), iso3c == "AAA", !mirrored)
  expect_equal(aaa2$primary_value[aaa2$cmd_code == "2710"], 333)
  expect_equal(aaa2$primary_value[aaa2$cmd_code == "1001"], 100)
})

test_that("a parent code becomes the remainder after its downloaded children", {
  dirs  <- new_dirs()
  codes <- data.frame(cmd_code = c("2836", "283691"),
                      commodity_name = c("carbonates", "lithium carbonate"),
                      category = "Metals and Minerals", subcategory = "Element",
                      parent_code = c(NA, "2836"))
  write_trade_csv(dirs$csv, "AAA", 2025, rbind(
    rows_of(1, "Export", "BBB", 100, "2836"), rows_of(1, "Export", "BBB", 30, "283691"),
    rows_of(2, "Export", "BBB", 20, "2836"),  rows_of(2, "Export", "BBB", 30, "283691"),
    rows_of(3, "Export", "BBB", 50, "2836")))
  run_build(dirs, codes = codes)
  a <- dplyr::filter(read_parquet_rows(dirs$parquet), iso3c == "AAA")
  v <- function(m, code) a$primary_value[a$mon == m & a$cmd_code == code]
  expect_equal(v(1, "2836"), 70)                 # 100 - 30
  expect_equal(v(1, "283691"), 30)               # the child is unchanged
  expect_length(v(2, "2836"), 0)                 # 20 - 30 < 0: set to zero and dropped
  expect_equal(v(3, "2836"), 50)                 # no child that month: parent as reported
  expect_equal(a$net_wgt_comtrade[a$mon == 1 & a$cmd_code == "2836"], 0)   # 10 kg - 10 kg
})

test_that("a country-year that turns into 'no data' loses its self-reported partition", {
  dirs <- new_dirs()
  write_trade_csv(dirs$csv, "AAA", 2025, rows_of(1, "Export", "BBB", 100))
  write_trade_csv(dirs$csv, "BBB", 2025, rows_of(1, "Export", "AAA", 40))
  run_build(dirs)
  expect_true(any(!dplyr::filter(read_parquet_rows(dirs$parquet), iso3c == "BBB")$mirrored))

  writeLines(c("count", "0", "0"), file.path(dirs$csv, "BBB_2025.csv"))
  run_build(dirs)
  bbb <- dplyr::filter(read_parquet_rows(dirs$parquet), iso3c == "BBB")
  expect_true(nrow(bbb) > 0 && all(bbb$mirrored))   # only mirrors from AAA remain
})

test_that("staged partitions replace existing ones and leave nothing behind", {
  root <- withr::local_tempdir()
  target  <- file.path(root, "parquet"); staging <- file.path(root, "staging")
  old <- data.frame(iso3c = "AAA", yr = 2025, x = 1)
  arrow::write_dataset(old, target, partitioning = c("iso3c", "yr"))
  arrow::write_dataset(data.frame(iso3c = c("AAA", "BBB"), yr = 2025, x = c(2, 3)),
                       staging, partitioning = c("iso3c", "yr"))
  expect_equal(build_env$build_swap_partitions(staging, target), 2L)
  got <- arrow::open_dataset(target) |> dplyr::collect()
  expect_equal(sort(got$x), c(2, 3))
  expect_false(dir.exists(staging))
  expect_false(dir.exists(file.path(root, "_trash_parquet")))
})
