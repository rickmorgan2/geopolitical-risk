# run_comtrade_update.R
# ------------------------------------------------------------------------------
# Comtrade update, step by step: downloads -> World totals -> parquet -> prices
# -> bronze -> silver. Run from the repo root (open the .Rproj).
#
# How to use it
#   Source the whole file once a day until Step 7 reports nothing left; or run
#   it a step at a time (select a step, Ctrl+Enter). Every download step
#   resumes where the last one stopped. All of them share one budget of
#   DAILY_CALL_BUDGET calls per UTC day: once it is spent, each later step says
#   so and does nothing, and the next day's run carries on. Steps 8-11 (build)
#   run only once Step 7 finds nothing left to download.
#
#   PREVIEW_ONLY <- TRUE; source("run_comtrade_update.R")   # plan only, no calls
#
# Order agreed 2026-11-21 (LOG.md, D-023), ~950 calls over 2-3 days:
#   Day 1  rest of 2026, 2017 backfill, THA 2018 + BIH 2019 repairs,
#          World totals for 2010-2023 and 2026
#   Day 2  2025 refresh
#   Day 3  2024 re-checks, then World totals for 2024
# Done 2026-11-23. Then (D-027): the extension codes for 2013-2026, ~2,450+
# calls over about five days (Step 6b). Large reporters may need 2-3 calls.
#
# Before the first run: no other R session may be downloading. Two runs at
# once overwrite each other's manifest.
# ------------------------------------------------------------------------------

library(DBI)
library(duckdb)

if (!exists("PREVIEW_ONLY"))    PREVIEW_ONLY    <- FALSE  # TRUE: every step only shows its plan
if (!exists("BUILD_WHEN_DONE")) BUILD_WHEN_DONE <- TRUE   # FALSE: stop after the downloads

# -- Step 0: setup -------------------------------------------------------------

if (file.exists(".env")) readRenviron(".env")   # API key; never printed
source("config.R")
source("R/ingest_comtrade_download.R")          # defines the functions; downloads nothing

# The repair steps (3 and 6) re-download files last fetched before this date.
# Once a file is re-downloaded it is newer than the cutoff, so it is not
# fetched again on later days.
REPAIR_CUTOFF <- "2026-11-21"

# 2024 was still filling in at the March 2026 download: 81 countries had no
# data, and these 7 had fewer than 12 self-reported months.
SHORT_2024 <- c("AGO", "COL", "CPV", "KAZ", "KWT", "MAR", "MWI")
recheck_2024 <- function() {
  no_data <- read_manifest() |> filter(year == 2024, status == "no_data") |> pull(iso3c)
  unique(c(no_data, SHORT_2024))
}

# -- Step 1: finish 2026 (the 2026-11-20 run stopped at the quota after PER) --

run_comtrade_download(2026, dry_run = PREVIEW_ONLY)

# -- Step 2: 2017 backfill (77 countries never downloaded) --------------------

run_comtrade_download(2017, dry_run = PREVIEW_ONLY)

# -- Step 3: repairs: files missing their second commodity chunk --------------

run_comtrade_download(2018, countries = "THA", refresh_before = REPAIR_CUTOFF,
                      dry_run = PREVIEW_ONLY)
run_comtrade_download(2019, countries = "BIH", refresh_before = REPAIR_CUTOFF,
                      dry_run = PREVIEW_ONLY)

# -- Step 4: World totals for files downloaded without them -------------------
# First a one-off check (2 calls): a World-only request must return the same
# World totals that the main download kept for PHL 2026. World-only downloads
# run only after it passes; the result is saved and reused on later days.

world_check <- if (PREVIEW_ONLY) NA else check_world_totals("PHL", 2026)
if (PREVIEW_ONLY || isTRUE(world_check)) {
  run_comtrade_world_download(c(2010:2023, 2026), dry_run = PREVIEW_ONLY)
} else {
  message("Step 4 waits: the World totals check has not passed (see above).")
}

# -- Step 5: 2025 refresh (the March download had 77 reporters, 62 of them
# short of 12 months) -----------------------------------------------------------

run_comtrade_download(2025, dry_run = PREVIEW_ONLY)

# -- Step 6: 2024 re-checks, then World totals for the rest of 2024 -----------

run_comtrade_download(2024, countries = recheck_2024(), refresh_before = REPAIR_CUTOFF,
                      dry_run = PREVIEW_ONLY)
if (PREVIEW_ONLY || isTRUE(world_check)) {
  run_comtrade_world_download(2024, dry_run = PREVIEW_ONLY)
}

# -- Step 6b: extension codes, 2013 on (D-027) ----------------------------------
# The 55 codes added in November 2026 (code_set "ext" in the code file), into
# their own folders and manifest. Their World rows come with them. 2013-2014
# are burn-in for rolling windows only.

EXT_YEARS <- BURN_IN_START_YR:as.integer(format(Sys.Date(), "%Y"))
run_comtrade_ext_download(EXT_YEARS, dry_run = PREVIEW_ONLY)

# -- Step 7: anything left? -----------------------------------------------------

manifest       <- bootstrap_manifest(read_manifest())
world_manifest <- read_manifest(COMTRADE_WORLD_MANIFEST)
left <- bind_rows(
  plan_downloads(use_iso3c, 2026, manifest),
  plan_downloads(use_iso3c, 2017, manifest),
  plan_downloads("THA", 2018, manifest, refresh_before = REPAIR_CUTOFF),
  plan_downloads("BIH", 2019, manifest, refresh_before = REPAIR_CUTOFF),
  plan_downloads(use_iso3c, 2025, manifest),
  plan_downloads(recheck_2024(), 2024, manifest, refresh_before = REPAIR_CUTOFF),
  plan_world_downloads(use_iso3c, c(2010:2024, 2026), manifest, world_manifest) |>
    mutate(reason = "World totals"),
  with_code_set("ext", plan_downloads(use_iso3c, EXT_YEARS, bootstrap_manifest(read_manifest()))) |>
    mutate(reason = paste("extension:", reason))
)
cat("\n=== STILL TO DOWNLOAD ===\n")
if (nrow(left) > 0) print(count(left, year, reason), n = Inf) else cat("Nothing: downloads complete.\n")

# -- Steps 8-11: build, once the downloads are complete -------------------------

if (PREVIEW_ONLY || !BUILD_WHEN_DONE) {
  message("Build steps skipped (", if (PREVIEW_ONLY) "preview" else "BUILD_WHEN_DONE = FALSE", ").")
} else if (nrow(left) > 0) {
  message(sprintf("Build waits: %d country-years still to download. Source this file again tomorrow (UTC).",
                  nrow(left)))
} else {
  # Step 8: CSVs -> parquet (CIF/FOB adjustment, mirrors rebuilt for touched years)
  source("R/ingest_comtrade_build.R")

  con <- dbConnect(duckdb(), DB_PATH)
  # Step 9: Pink Sheet and IMF prices, joined into the parquet
  source("R/ingest_price_series.R")
  # Step 10: parquet -> bronze tables
  source("R/ingest_comtrade.R")
  # Step 11: silver trade indicators
  source("R/compute_silver_comtrade.R")
  dbDisconnect(con, shutdown = TRUE)

  message("Comtrade update complete: ", format(Sys.time()))
}
