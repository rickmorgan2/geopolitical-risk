# ingest_comtrade_build.R
# ------------------------------------------------------------------------------
# Step 0b: Convert Comtrade CSVs to hive-partitioned parquet.
#
# Reads the country-year CSVs of both code sets (core in COMTRADE_CSV_DIR,
# extension codes in COMTRADE_EXT_CSV_DIR; D-027), standardizes columns, turns
# parent codes into remainders (parent minus its downloaded children; D-029),
# computes CIF/FOB ratios from matched bilateral pairs, mirrors missing
# country-month flows from partners' reports, and writes the result
# partitioned by iso3c and year.
#
# Values stay nominal (primary_value). The price step, ingest_price_series.R,
# deflates them with the World Bank MUV index (D-025).
#
# Output layout: COMTRADE_PARQUET/iso3c={ISO3}/yr={YEAR}/*.parquet
#
# Safety: new partitions are written to a staging folder and swapped in one
# partition at a time, so a crash never leaves a half-written partition. The
# years whose mirrors still need rebuilding are kept in a marker file until the
# mirroring step has finished, so an interrupted build finishes on the next run.
#
# Uses an in-memory DuckDB (separate from georisk.duckdb) for SQL operations.
# Run after: ingest_comtrade_download.R. Run before: ingest_price_series.R.
# ------------------------------------------------------------------------------

library(tidyverse)
library(DBI)
library(duckdb)
library(arrow)
library(glue)

if (!exists("CROSSWALK_CSV")) stop("Source config.R first or run via run_pipeline.R")

# -- Config --------------------------------------------------------------------

CIF_FOB_OUTLIER_BOUNDS <- c(1.01, 1.50)   # plausible CIF/FOB ratio range
if (!exists("FORCE_UPDATE")) FORCE_UPDATE <- FALSE   # TRUE = rebuild every partition
if (!exists("COMTRADE_EXT_CSV_DIR")) COMTRADE_EXT_CSV_DIR <- "data/comtrade_csv_ext"

# -- Helpers -------------------------------------------------------------------

leading_zero_cmd_codes <- function(x) {
  x_char <- as.character(x)
  ifelse(nchar(x_char) %% 2 == 1, paste0("0", x_char), x_char)
}

# Data files only, judged by header: Comtrade's "no data" answers ("count"
# files) and empty markers from failed downloads are skipped.
build_is_data_csv <- function(f) {
  if (file.size(f) == 0) return(FALSE)
  first <- readLines(f, n = 1, warn = FALSE)
  length(first) == 1 && grepl("(^|,)(ref_year|yr)(,|$)", first)
}

# Comtrade's TRUE/FALSE flags as logical; NA where the column is missing
build_flag <- function(x) {
  if (is.null(x)) return(NA)
  ifelse(is.na(x) | x == "", NA, x %in% c("TRUE", "True", "true", "1"))
}

# One CSV, every column read as text (no type guessing), in the pipeline's
# column names. Handles the two historical layouts.
build_read_csv <- function(file_path) {
  raw <- read_csv(file_path, col_types = cols(.default = "c"), show_col_types = FALSE,
                  progress = FALSE)
  if (nrow(raw) == 0) return(NULL)
  if ("yr" %in% names(raw)) {
    clean <- raw |>
      transmute(yr, mon, iso3c, country, flow_desc, partner_iso3c, partner_country,
                cmd_code, cmd_desc_comtrade, qty_unit_code,
                qty_comtrade, net_wgt_comtrade, cifvalue, fobvalue, primary_value)
  } else if ("ref_year" %in% names(raw)) {
    clean <- raw |>
      transmute(yr = ref_year, mon = ref_month,
                iso3c = reporter_iso, country = reporter_desc,
                flow_desc, partner_iso3c = partner_iso, partner_country = partner_desc,
                cmd_code, cmd_desc_comtrade = cmd_desc, qty_unit_code,
                qty_comtrade = qty, net_wgt_comtrade = net_wgt,
                cifvalue, fobvalue, primary_value)
  } else {
    message("  Warning: unrecognized columns in ", basename(file_path))
    return(NULL)
  }
  clean |>
    mutate(
      across(c(yr, mon, qty_comtrade, net_wgt_comtrade, cifvalue, fobvalue, primary_value),
             as.numeric),
      qty_unit_code        = as.integer(qty_unit_code),
      cmd_code             = leading_zero_cmd_codes(sub("[.]0+$", "", cmd_code)),
      is_qty_estimated     = build_flag(raw[["is_qty_estimated"]]),
      is_net_wgt_estimated = build_flag(raw[["is_net_wgt_estimated"]]),
      mirrored             = FALSE
    )
}

# All files of one country-year (core and extension). If a code appears in
# more than one file, the newest file's rows are kept.
build_read_key <- function(paths) {
  frames <- map(paths, \(p) {
    d <- tryCatch(build_read_csv(p), error = function(e) {
      message("  Error in ", basename(p), ": ", conditionMessage(e)); NULL })
    if (!is.null(d)) d$.mtime <- as.numeric(file.mtime(p))
    d
  }) |> compact()
  if (length(frames) == 0) return(NULL)
  list_rbind(frames) |>
    group_by(cmd_code) |>
    filter(.mtime == max(.mtime)) |>
    ungroup() |>
    select(-.mtime)
}

# Parent codes become remainders: parent minus the sum of its downloaded
# children, per reporter, partner, flow and month (D-029). Values and weights
# are additive; a quantity is only when the units match. Negative remainders
# (children reported above the parent) are set to zero and dropped.
build_apply_remainders <- function(df, parents) {
  if (is.null(df) || nrow(df) == 0 || nrow(parents) == 0) return(df)
  key  <- c("iso3c", "yr", "mon", "flow_desc", "partner_iso3c")
  kids <- df |>
    inner_join(parents, by = c("cmd_code" = "child")) |>
    group_by(across(all_of(c(key, "parent")))) |>
    summarise(kid_value = sum(primary_value, na.rm = TRUE),
              kid_wgt   = sum(net_wgt_comtrade),
              kid_unit  = if (n_distinct(qty_unit_code) == 1) first(qty_unit_code) else NA_integer_,
              kid_qty   = sum(qty_comtrade), .groups = "drop")
  if (nrow(kids) == 0) return(df)
  out <- df |>
    left_join(kids, by = c(key, "cmd_code" = "parent")) |>
    mutate(
      is_rest          = !is.na(kid_value),
      net_rest         = primary_value - kid_value,
      primary_value    = if_else(is_rest, pmax(net_rest, 0), primary_value),
      net_wgt_comtrade = if_else(is_rest, pmax(net_wgt_comtrade - kid_wgt, 0), net_wgt_comtrade),
      qty_comtrade     = if_else(is_rest,
                                 if_else(!is.na(kid_unit) & qty_unit_code == kid_unit,
                                         pmax(qty_comtrade - kid_qty, 0), NA_real_),
                                 qty_comtrade),
      cifvalue         = if_else(is_rest, NA_real_, cifvalue),
      fobvalue         = if_else(is_rest, NA_real_, fobvalue)
    )
  n_neg <- sum(out$is_rest & out$net_rest < 0, na.rm = TRUE)
  out <- out |>
    filter(!(is_rest & primary_value <= 0)) |>
    select(-kid_value, -kid_wgt, -kid_unit, -kid_qty, -is_rest, -net_rest)
  attr(out, "n_negative") <- n_neg
  out
}

# Swap staged partitions into the dataset one at a time. An existing partition
# is moved aside (outside the dataset) first and deleted once the new one is
# in place.
build_swap_partitions <- function(staging_dir, target_dir) {
  if (!dir.exists(staging_dir)) return(0L)
  parts <- list.dirs(staging_dir, recursive = TRUE, full.names = FALSE)
  parts <- parts[grepl("^iso3c=[^/]+/yr=[0-9]+$", parts)]
  trash <- file.path(dirname(target_dir), paste0("_trash_", basename(target_dir)))
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  for (p in parts) {
    src  <- file.path(staging_dir, p)
    dest <- file.path(target_dir, p)
    old  <- file.path(trash, p)
    dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
    if (dir.exists(dest)) {
      dir.create(dirname(old), showWarnings = FALSE, recursive = TRUE)
      unlink(old, recursive = TRUE)
      if (!file.rename(dest, old)) stop("could not move aside ", dest)
    }
    if (!file.rename(src, dest)) {
      if (dir.exists(old)) file.rename(old, dest)
      stop("could not move ", src, " into place")
    }
  }
  unlink(c(staging_dir, trash), recursive = TRUE)
  length(parts)
}

# Partitions on disk, as iso3c / yr pairs
build_partition_keys <- function(parquet_dir) {
  if (!dir.exists(parquet_dir)) return(tibble(iso3c = character(), yr = integer()))
  d <- list.dirs(parquet_dir, recursive = TRUE, full.names = FALSE)
  m <- str_match(d, "^iso3c=([^/]+)/yr=([0-9]+)$")
  tibble(iso3c = m[, 2], yr = as.integer(m[, 3])) |> filter(!is.na(iso3c))
}

# Write a batch through the staging folder
build_write <- function(df, parquet_dir) {
  if (is.null(df) || nrow(df) == 0) return(0L)
  staging <- paste0(parquet_dir, "_staging")
  unlink(staging, recursive = TRUE)
  write_dataset(df, staging, partitioning = c("iso3c", "yr"), format = "parquet",
                compression = "zstd", existing_data_behavior = "overwrite")
  build_swap_partitions(staging, parquet_dir)
}

# Active crosswalk codes with their names, groups and parent codes (D-029)
build_load_codes <- function(path) {
  x <- read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    filter(in_data == "TRUE")
  if (!"parent_code" %in% names(x)) x$parent_code <- NA_character_
  x |> mutate(cmd_code    = leading_zero_cmd_codes(cmd_code),
              parent_code = if_else(is.na(parent_code) | parent_code == "", NA_character_,
                                    leading_zero_cmd_codes(parent_code)))
}

# -- Reference data ------------------------------------------------------------

all_cmd_code <- build_load_codes(CROSSWALK_CSV)

# -- Pipeline function ---------------------------------------------------------

run_comtrade_build <- function(csv_dirs     = c(COMTRADE_CSV_DIR, COMTRADE_EXT_CSV_DIR),
                               parquet_dir  = COMTRADE_PARQUET,
                               summary_dir  = COMTRADE_SUMMARY_DIR,
                               commodity_type_data = all_cmd_code,
                               force_update = FORCE_UPDATE) {

  pipeline_start <- Sys.time()

  cmd_codes <- commodity_type_data |>
    mutate(cmd_code = as.character(cmd_code))
  if (!"parent_code" %in% names(cmd_codes)) cmd_codes$parent_code <- NA_character_
  parents <- cmd_codes |>
    filter(!is.na(parent_code), parent_code %in% cmd_code) |>
    transmute(child = cmd_code, parent = parent_code)

  con <- dbConnect(duckdb(), dbdir = ":memory:")
  dbExecute(con, "SET memory_limit = '16GB'")
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dir.create(summary_dir, showWarnings = FALSE, recursive = TRUE)
  pending_file <- file.path(summary_dir, "mirror_pending.txt")

  ## Step 1: identify files to process -----------------------------------------

  message("--- Comtrade build: step 1 -- scanning CSV files ---")

  files <- map_dfr(csv_dirs[dir.exists(csv_dirs)], \(d)
    tibble(path = list.files(d, pattern = "^[A-Z]{3}_[0-9]{4}\\.csv$", full.names = TRUE)))
  files <- files |>
    filter(map_lgl(path, build_is_data_csv)) |>
    mutate(key   = str_remove(basename(path), "\\.csv$"),
           iso3c = str_sub(key, 1, 3), yr = as.integer(str_sub(key, 5, 8)),
           mtime = as.numeric(file.mtime(path)))
  if (nrow(files) == 0) stop("No CSV files found in ", paste(csv_dirs, collapse = ", "))

  keys  <- files |> group_by(key, iso3c, yr) |> summarise(mtime = max(mtime), .groups = "drop")
  parts <- build_partition_keys(parquet_dir)

  if (force_update) {
    todo <- keys
    message(sprintf("  Force update: processing all %d country-years", nrow(todo)))
  } else {
    # (Re)process a country-year when its partition is missing or older than
    # any of its CSVs (e.g. after a refresh or a new extension download)
    part_mtime <- map_dbl(seq_len(nrow(keys)), \(i) {
      f <- list.files(file.path(parquet_dir, paste0("iso3c=", keys$iso3c[i]),
                                paste0("yr=", keys$yr[i])), full.names = TRUE)
      if (length(f) == 0) NA_real_ else as.numeric(max(file.mtime(f)))
    })
    todo <- keys[is.na(part_mtime) | keys$mtime > part_mtime, ]
    message(sprintf("  New/updated: %d | Already processed: %d", nrow(todo), nrow(keys) - nrow(todo)))
  }

  # Country-years that hold self-reported rows but no longer have a data CSV
  # (a refresh returned "no data"): their partitions are removed
  dropped <- parts |>
    anti_join(keys, by = c("iso3c", "yr")) |>
    filter(map2_lgl(iso3c, yr, \(i, y) {
      d <- file.path(parquet_dir, paste0("iso3c=", i), paste0("yr=", y))
      n <- tryCatch(open_dataset(d) |> filter(!mirrored) |> count() |> collect() |> pull(n),
                    error = function(e) 0L)
      length(n) > 0 && n > 0
    }))
  if (nrow(dropped) > 0) {
    message(sprintf("  Removing %d partitions whose country-year no longer has data", nrow(dropped)))
    walk2(dropped$iso3c, dropped$yr, \(i, y)
      unlink(file.path(parquet_dir, paste0("iso3c=", i), paste0("yr=", y)), recursive = TRUE))
  }

  pending <- if (file.exists(pending_file)) as.integer(readLines(pending_file)) else integer()
  years_touched <- if (force_update) sort(unique(keys$yr)) else
    sort(unique(c(todo$yr, dropped$yr, pending)))

  if (nrow(todo) == 0 && length(years_touched) == 0) {
    message("  No new files to process.")
    return(invisible(list(message = "No updates needed")))
  }
  writeLines(as.character(years_touched), pending_file)

  ## Step 2: CSVs to parquet, one year at a time --------------------------------

  message("--- Comtrade build: step 2 -- CSV to parquet ---")

  n_rows <- 0; n_negative <- 0
  for (y in sort(unique(todo$yr))) {
    yk <- todo |> filter(yr == y)
    batch <- map(yk$key, \(k) {
      d <- build_read_key(files$path[files$key == k])
      d <- build_apply_remainders(d, parents)
      n_negative <<- n_negative + coalesce(attr(d, "n_negative"), 0L)
      d
    }) |> compact() |> list_rbind()
    if (is.null(batch) || nrow(batch) == 0) next

    batch <- batch |>
      mutate(qty_unit_price = if_else(qty_comtrade > 0, primary_value / qty_comtrade, NA_real_)) |>
      left_join(cmd_codes |> select(cmd_code, commodity_name, category, subcategory),
                by = "cmd_code") |>
      filter(!is.na(commodity_name))   # active crosswalk codes only (D-029)

    build_write(batch, parquet_dir)
    n_rows <- n_rows + nrow(batch)
    message(sprintf("  %d: %d country-years, %s rows", y, nrow(yk), format(nrow(batch), big.mark = ",")))
  }
  message(sprintf("  Wrote %s rows | remainders below zero set to zero and dropped: %d",
                  format(n_rows, big.mark = ","), n_negative))

  ## Step 3: CIF/FOB ratios (recomputed on every build) -------------------------

  message("--- Comtrade build: step 3 -- CIF/FOB ratios ---")

  glob <- gsub("\\\\", "/", file.path(parquet_dir, "**", "*.parquet"))
  dbExecute(con, glue("
    CREATE OR REPLACE VIEW trade_data AS
    SELECT * FROM read_parquet('{glob}', hive_partitioning = true, union_by_name = true)
    WHERE NOT COALESCE(mirrored, FALSE) AND primary_value > 0 AND net_wgt_comtrade > 0
  "))

  ratios <- dbGetQuery(con, glue("
    WITH exports AS (
      SELECT iso3c, partner_iso3c, cmd_code, yr, mon, AVG(primary_value) AS export_fob
      FROM trade_data WHERE flow_desc = 'Export'
      GROUP BY iso3c, partner_iso3c, cmd_code, yr, mon
    ),
    imports AS (
      SELECT iso3c, partner_iso3c, cmd_code, yr, mon, AVG(primary_value) AS import_cif
      FROM trade_data WHERE flow_desc = 'Import'
      GROUP BY iso3c, partner_iso3c, cmd_code, yr, mon
    ),
    bilateral_pairs AS (
      SELECT e.cmd_code, i.import_cif / e.export_fob AS cif_fob_ratio
      FROM exports e
      INNER JOIN imports i ON (
        e.iso3c = i.partner_iso3c AND e.partner_iso3c = i.iso3c AND
        e.cmd_code = i.cmd_code AND e.yr = i.yr AND e.mon = i.mon)
      WHERE e.export_fob > 0 AND i.import_cif > 0
        AND i.import_cif / e.export_fob
            BETWEEN {CIF_FOB_OUTLIER_BOUNDS[1]} AND {CIF_FOB_OUTLIER_BOUNDS[2]}
    )
    SELECT cmd_code, COUNT(*) AS n_observations, MEDIAN(cif_fob_ratio) AS cif_fob_ratio
    FROM bilateral_pairs GROUP BY cmd_code
  ")) |> as_tibble()

  all_codes      <- dbGetQuery(con, "SELECT DISTINCT cmd_code FROM trade_data")$cmd_code
  overall_median <- median(ratios$cif_fob_ratio, na.rm = TRUE)
  if (is.na(overall_median)) overall_median <- 1.10

  cif_fob_ratios <- tibble(cmd_code = all_codes) |>
    left_join(ratios, by = "cmd_code") |>
    mutate(cif_fob_ratio  = coalesce(cif_fob_ratio, overall_median),
           n_observations = replace_na(n_observations, 0L))
  write_parquet(cif_fob_ratios, file.path(summary_dir, "cif_fob_ratios.parquet"))
  message(sprintf("  Ratios for %d commodities (overall median: %.4f)",
                  nrow(cif_fob_ratios), overall_median))

  ## Step 4: mirror missing flows ----------------------------------------------
  # A country-month-flow with no self-reported rows is filled from its
  # partners' reports (mirrored = TRUE). Coverage is judged per flow: a country
  # that reports only its imports in a month gets mirrored exports. The mirrors
  # of every year this build touched are recomputed from scratch: they improve
  # as partners' data arrive and disappear once the country reports itself.

  message("--- Comtrade build: step 4 -- mirroring missing flows ---")

  dbWriteTable(con, "cif_fob_ratios", cif_fob_ratios, temporary = TRUE, overwrite = TRUE)
  year_filter <- glue("WHERE yr IN ({paste(years_touched, collapse = ', ')})")

  dbExecute(con, glue("
    CREATE OR REPLACE VIEW existing_data AS
    SELECT * FROM read_parquet('{glob}', hive_partitioning = true, union_by_name = true)
    {year_filter}
  "))
  dbExecute(con, "CREATE OR REPLACE VIEW self_data AS
                  SELECT * FROM existing_data WHERE NOT COALESCE(mirrored, FALSE)")

  coverage <- dbGetQuery(con, "SELECT DISTINCT iso3c, yr, mon, flow_desc FROM self_data") |>
    as_tibble()
  all_countries <- dbGetQuery(con, "
    SELECT DISTINCT iso3c FROM self_data UNION SELECT DISTINCT partner_iso3c FROM self_data
  ")$iso3c

  missing <- expand_grid(iso3c = all_countries,
                         coverage |> distinct(yr, mon),
                         flow_desc = c("Export", "Import")) |>
    anti_join(coverage, by = c("iso3c", "yr", "mon", "flow_desc"))
  message(sprintf("  Years: %s | %d country-month flows without self-reported data",
                  paste(years_touched, collapse = ", "), nrow(missing)))
  dbWriteTable(con, "missing_coverage", missing, temporary = TRUE, overwrite = TRUE)

  mirrored_data <- dbGetQuery(con, "
    SELECT
      e.partner_iso3c AS iso3c, e.yr, e.mon,
      e.partner_country AS country,
      CASE WHEN e.flow_desc = 'Export' THEN 'Import' ELSE 'Export' END AS flow_desc,
      e.iso3c AS partner_iso3c, e.country AS partner_country,
      e.cmd_code, e.commodity_name, e.category, e.subcategory, e.qty_unit_code,
      CASE WHEN e.flow_desc = 'Export' THEN e.primary_value * COALESCE(r.cif_fob_ratio, 1.10)
           ELSE e.primary_value / COALESCE(r.cif_fob_ratio, 1.10) END AS primary_value,
      e.cifvalue, e.fobvalue, e.cmd_desc_comtrade,
      e.qty_comtrade, e.net_wgt_comtrade, e.qty_unit_price,
      e.is_qty_estimated, e.is_net_wgt_estimated,
      TRUE AS mirrored
    FROM self_data e
    LEFT JOIN cif_fob_ratios r ON e.cmd_code = r.cmd_code
    SEMI JOIN missing_coverage m
      ON m.iso3c = e.partner_iso3c AND m.yr = e.yr AND m.mon = e.mon
     AND m.flow_desc = CASE WHEN e.flow_desc = 'Export' THEN 'Import' ELSE 'Export' END
  ") |> as_tibble()

  # Partitions to rewrite: those gaining mirrors and those holding old ones
  old_mirror_parts <- dbGetQuery(con, "
    SELECT DISTINCT iso3c, yr FROM existing_data WHERE COALESCE(mirrored, FALSE)
  ") |> as_tibble()
  affected <- bind_rows(old_mirror_parts, distinct(mirrored_data, iso3c, yr)) |> distinct()

  if (nrow(affected) == 0) {
    message("  No missing flows to mirror")
  } else {
    dbWriteTable(con, "affected_partitions", affected, temporary = TRUE, overwrite = TRUE)
    self_affected <- dbGetQuery(con, "
      SELECT e.* FROM self_data e
      INNER JOIN affected_partitions a ON e.iso3c = a.iso3c AND e.yr = a.yr
    ") |> as_tibble()
    for (v in c("self_data", "existing_data", "trade_data"))
      dbExecute(con, paste("DROP VIEW IF EXISTS", v))

    rewrite <- bind_rows(self_affected, mirrored_data)

    # Partitions that held only old mirrors and get nothing back are removed
    emptied <- anti_join(affected, distinct(rewrite, iso3c, yr), by = c("iso3c", "yr"))
    walk2(emptied$iso3c, emptied$yr, \(iso, yr)
      unlink(file.path(parquet_dir, paste0("iso3c=", iso), paste0("yr=", yr)), recursive = TRUE))

    build_write(rewrite, parquet_dir)
    message(sprintf("  Mirrored %s records into %d partitions (%d emptied)",
                    format(nrow(mirrored_data), big.mark = ","),
                    nrow(affected) - nrow(emptied), nrow(emptied)))
  }
  unlink(pending_file)

  elapsed <- round(as.numeric(Sys.time() - pipeline_start, units = "mins"), 1)
  message(sprintf("--- Comtrade build complete in %.1f min ---", elapsed))

  invisible(list(files_processed = nrow(todo), years = years_touched, total_time_min = elapsed))
}

# -- Run -----------------------------------------------------------------------

run_comtrade_build(
  csv_dirs     = c(COMTRADE_CSV_DIR, COMTRADE_EXT_CSV_DIR),
  parquet_dir  = COMTRADE_PARQUET,
  summary_dir  = COMTRADE_SUMMARY_DIR,
  force_update = FORCE_UPDATE   # TRUE = full reprocess; FALSE = incremental
)

# -- Verification --------------------------------------------------------------

check_parquet <- arrow::open_dataset(
  file.path(COMTRADE_PARQUET, "iso3c=USA", "yr=2024")
) |> collect()

message(sprintf("Verification -- USA 2024: %d rows | %d mirrored",
                nrow(check_parquet), sum(check_parquet$mirrored)))
