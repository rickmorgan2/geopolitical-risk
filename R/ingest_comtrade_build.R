# ingest_comtrade_build.R
# ------------------------------------------------------------------------------
# Step 0b: Convert Comtrade CSVs to hive-partitioned parquet.
#
# Reads country-year CSVs from COMTRADE_CSV_DIR, standardizes columns,
# adjusts to constant 2024 USD via IMF CPI, computes CIF/FOB ratios from
# matched bilateral pairs, mirrors missing country-month flows, and writes
# the final dataset partitioned by iso3c and year.
#
# Output layout: COMTRADE_PARQUET/iso3c={ISO3}/yr={YEAR}/*.parquet
#
# Uses an in-memory DuckDB (separate from econrisk.duckdb) for SQL operations.
# All parquet I/O uses the arrow package.
#
# Run after: ingest_comtrade_download.R
# Run before: ingest_comtrade.R (which reads the parquet into bronze tables)
#
# Does NOT require: a DB connection to econrisk.duckdb (standalone script)
# ------------------------------------------------------------------------------

library(tidyverse)
library(DBI)
library(duckdb)
library(arrow)
library(glue)
library(rsdmx)

if (!exists("CROSSWALK_CSV")) stop("Source config.R first or run via run_pipeline.R")

# -- Config --------------------------------------------------------------------

CIF_FOB_OUTLIER_BOUNDS <- c(1.01, 1.50)   # plausible CIF/FOB ratio range

# -- Helpers -------------------------------------------------------------------

leading_zero_cmd_codes <- function(x) {
  x_char <- as.character(x)
  ifelse(nchar(x_char) %% 2 == 1, paste0("0", x_char), x_char)
}

# -- Reference data ------------------------------------------------------------

use_iso3c <- read_csv(COUNTRY_FRAME_CSV,
                      col_types = cols(.default = "c"),
                      show_col_types = FALSE) |>
  distinct(iso3c, .keep_all = TRUE) |>
  filter(!(iso3c %in% c("XKX", "TWN"))) |>
  add_row(iso3c = "HKG") |>
  arrange(iso3c) |>
  pull(iso3c)

all_cmd_code <- read_csv(CROSSWALK_CSV,
                         col_types = cols(.default = "c"),
                         show_col_types = FALSE) |>
  select(cmd_code, commodity_name, pink_sheet, category, subcategory,
         price_source, price_name, in_data) |>        # drop blank trailing cols
  filter(in_data == "TRUE") |>                        # active codes only; wood products excluded
  mutate(across(matches("cate|name|source"), as.factor),  # pink_sheet handled explicitly below
         cmd_code   = leading_zero_cmd_codes(cmd_code),
         pink_sheet = as.integer(pink_sheet),             # read as character; convert directly
         in_data    = as.logical(in_data))

# All active crosswalk cmd_codes (in_data == TRUE); passed into run_comtrade_build
# as commodity_type_data for the category/subcategory join and cmd_code filter.

# -- IMF CPI (constant 2024 USD) -----------------------------------------------

message("--- Comtrade build: fetching IMF CPI ---")

imf_cpi_url <- paste0(
  "https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.STA/CPI/%2B/",
  "USA.CPI._T.IX.M?dimensionAtObservation=TIME_PERIOD&attributes=dsd",
  "&measures=all&includeHistory=false"
)

us_cpi_data <- rsdmx::readSDMX(imf_cpi_url) |>
  as.data.frame() |>
  select(yr = TIME_PERIOD, imf_cpi = OBS_VALUE) |>
  mutate(
    mon        = as.numeric(str_extract(yr, "(M)([0-9]{2})", group = 2)),
    yr         = as.numeric(str_extract(yr, "[0-9]{4}")),
    us_imf_cpi = as.numeric(imf_cpi)
  ) |>
  filter(yr >= START_YR) |>
  select(yr, mon, us_imf_cpi)

base_cpi <- us_cpi_data |>
  filter(yr == 2024) |>
  pull(us_imf_cpi) |>
  mean()

cpi_std <- us_cpi_data |>
  mutate(cpi_std_2024 = (us_imf_cpi / base_cpi) * 100) |>
  select(yr, mon, cpi_std_2024)

# -- Pipeline function ---------------------------------------------------------

run_comtrade_build <- function(csv_dir     = COMTRADE_CSV_DIR,
                               parquet_dir = COMTRADE_PARQUET,
                               summary_dir = COMTRADE_SUMMARY_DIR,
                               iso3c       = use_iso3c,
                               commodity_type_data = all_cmd_code,cpi         = cpi_std,
                               force_update = FORCE_UPDATE) {

  pipeline_start <- Sys.time()

  cmd_codes <- commodity_type_data |>
    mutate(cmd_code = as.character(cmd_code))

  con <- dbConnect(duckdb(), dbdir = ":memory:")
  dbExecute(con, "SET memory_limit = '16GB'")
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  ## Step 1: identify files to process -----------------------------------------

  message("--- Comtrade build: step 1 -- scanning CSV files ---")

  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE) |>
    keep(~ file.size(.x) > 8000)   # skip empty/error files

  if (length(csv_files) == 0) stop("No CSV files found in ", csv_dir)

  existing_parquet <- character()
  if (dir.exists(parquet_dir) && !force_update) {
    existing_dirs    <- list.dirs(parquet_dir, recursive = FALSE)
    existing_parquet <- basename(existing_dirs) |>
      str_remove("iso3c=") |>
      map(~ {
        year_dirs <- list.dirs(
          file.path(parquet_dir, paste0("iso3c=", .x)),
          recursive = FALSE
        )
        paste(.x, str_remove(basename(year_dirs), "yr="), sep = "_")
      }) |>
      unlist()
  }

  csv_keys <- basename(csv_files) |> str_remove("\\.csv$")

  if (force_update) {
    files_to_process <- csv_files
    message(sprintf("  Force update: processing all %d files", length(files_to_process)))
  } else {
    needs_processing <- !(csv_keys %in% existing_parquet)
    files_to_process <- csv_files[needs_processing]
    message(sprintf("  New/updated: %d | Already processed: %d",
                    sum(needs_processing), sum(!needs_processing)))
  }

  if (length(files_to_process) == 0) {
    message("  No new files to process.")
    return(invisible(list(message = "No updates needed")))
  }

  ## Step 2: CSVs to parquet ---------------------------------------------------

  message("--- Comtrade build: step 2 -- CSV to parquet ---")

  all_new_data <- map(files_to_process, function(file_path) {
    tryCatch({
      raw <- read_csv(file_path, show_col_types = FALSE)
      if (nrow(raw) == 0) return(NULL)

      # Standardize across two historical column formats
      if ("yr" %in% names(raw)) {
        clean <- raw |>
          select(yr, mon, iso3c, country, flow_desc,
                 partner_iso3c, partner_country,
                 cmd_code, cmd_desc_comtrade,
                 qty_unit_code,
                 qty_comtrade, net_wgt_comtrade,
                 cifvalue, fobvalue, primary_value)
      } else if ("ref_year" %in% names(raw)) {
        clean <- raw |>
          select(yr = ref_year, mon = ref_month,
                 iso3c = reporter_iso, country = reporter_desc,
                 flow_desc, partner_iso3c = partner_iso,
                 partner_country = partner_desc,
                 cmd_code, cmd_desc_comtrade = cmd_desc,
                 qty_unit_code,
                 qty_comtrade = qty, net_wgt_comtrade = net_wgt,
                 cifvalue, fobvalue, primary_value)
      } else {
        message("  Warning: unrecognized columns in ", basename(file_path))
        return(NULL)
      }

      clean |>
        mutate(
          across(c(yr, mon, qty_comtrade, net_wgt_comtrade,
                   cifvalue, fobvalue, primary_value), as.numeric),
          qty_unit_code = as.integer(qty_unit_code),
          cmd_code  = leading_zero_cmd_codes(cmd_code),
          mirrored  = FALSE
        )

    }, error = function(e) {
      message("  Error in ", basename(file_path), ": ", e$message)
      NULL
    })
  }, .progress = TRUE) |>
    compact() |>
    list_rbind()

  if (is.null(all_new_data) || nrow(all_new_data) == 0)
    stop("No valid data found in CSV files.")

  all_new_data <- all_new_data |>
    left_join(cpi, by = c("yr", "mon")) |>
    mutate(
      constant_value_2024 = case_when(
        !is.na(primary_value) & !is.na(cpi_std_2024) ~
          primary_value / cpi_std_2024 * 100,
        TRUE ~ primary_value
      ),
      qty_unit_price = if_else(qty_comtrade > 0,
                               primary_value / qty_comtrade,
                               NA_real_)
    ) |>
    left_join(cmd_codes |>
                select(cmd_code, commodity_name, category, subcategory),
              by = "cmd_code") |>
    filter(!is.na(commodity_name))   # drops cmd_codes not in crosswalk (in_data == TRUE)

  dir.create(parquet_dir, showWarnings = FALSE, recursive = TRUE)
  write_dataset(
    all_new_data,
    parquet_dir,
    partitioning             = c("iso3c", "yr"),
    format                   = "parquet",
    compression              = "zstd",
    existing_data_behavior   = "delete_matching"
  )

  message(sprintf("  Wrote %d rows to %s", nrow(all_new_data), parquet_dir))

  ## Step 3: CIF/FOB ratios ----------------------------------------------------

  message("--- Comtrade build: step 3 -- CIF/FOB ratios ---")

  ratios_file <- file.path(summary_dir, "cif_fob_ratios.parquet")

  if (file.exists(ratios_file) && !force_update) {
    message("  Using cached CIF/FOB ratios")
    cif_fob_ratios <- read_parquet(ratios_file)
  } else {
    dbExecute(con, glue("
      CREATE VIEW trade_data AS
      SELECT * FROM read_parquet('{parquet_dir}/**/*.parquet',
                                  hive_partitioning = true)
      WHERE mirrored = FALSE AND primary_value > 0 AND net_wgt_comtrade > 0
    "))

    ratios <- dbGetQuery(con, glue("
      WITH exports AS (
        SELECT iso3c, partner_iso3c, cmd_code, yr, mon,
               AVG(primary_value) AS export_fob
        FROM trade_data WHERE flow_desc = 'Export'
        GROUP BY iso3c, partner_iso3c, cmd_code, yr, mon
      ),
      imports AS (
        SELECT iso3c, partner_iso3c, cmd_code, yr, mon,
               AVG(primary_value) AS import_cif
        FROM trade_data WHERE flow_desc = 'Import'
        GROUP BY iso3c, partner_iso3c, cmd_code, yr, mon
      ),
      bilateral_pairs AS (
        SELECT e.cmd_code,
               i.import_cif / e.export_fob AS cif_fob_ratio
        FROM exports e
        INNER JOIN imports i ON (
          e.iso3c = i.partner_iso3c AND
          e.partner_iso3c = i.iso3c AND
          e.cmd_code = i.cmd_code AND
          e.yr = i.yr AND e.mon = i.mon
        )
        WHERE e.export_fob > 0 AND i.import_cif > 0
          AND i.import_cif / e.export_fob
              BETWEEN {CIF_FOB_OUTLIER_BOUNDS[1]} AND {CIF_FOB_OUTLIER_BOUNDS[2]}
      )
      SELECT cmd_code,
             COUNT(*) AS n_observations,
             MEDIAN(cif_fob_ratio) AS cif_fob_ratio
      FROM bilateral_pairs
      GROUP BY cmd_code
    ")) |> as_tibble()

    all_codes <- dbGetQuery(con,
      "SELECT DISTINCT cmd_code FROM trade_data")$cmd_code

    overall_median <- median(ratios$cif_fob_ratio, na.rm = TRUE)
    if (is.na(overall_median)) overall_median <- 1.10

    cif_fob_ratios <- tibble(cmd_code = all_codes) |>
      left_join(ratios, by = "cmd_code") |>
      mutate(
        cif_fob_ratio  = coalesce(cif_fob_ratio, overall_median),
        n_observations = replace_na(n_observations, 0L)
      )

    dir.create(summary_dir, showWarnings = FALSE, recursive = TRUE)
    write_parquet(cif_fob_ratios, ratios_file)

    message(sprintf("  Ratios for %d commodities (overall median: %.4f)",
                    nrow(cif_fob_ratios), overall_median))
  }

  ## Step 4: mirror missing flows ----------------------------------------------

  message("--- Comtrade build: step 4 -- mirroring missing flows ---")

  dbWriteTable(con, "cif_fob_ratios", cif_fob_ratios, temporary = TRUE)

  dbExecute(con, glue("
    CREATE OR REPLACE VIEW existing_data AS
    SELECT * FROM read_parquet('{parquet_dir}/**/*.parquet',
                                hive_partitioning = true)
  "))

  coverage     <- dbGetQuery(con, "SELECT DISTINCT iso3c, yr, mon FROM existing_data") |>
    as_tibble()
  all_countries <- dbGetQuery(con, "
    SELECT DISTINCT iso3c FROM existing_data
    UNION
    SELECT DISTINCT partner_iso3c FROM existing_data
  ")$iso3c

  missing <- expand_grid(
    iso3c = all_countries,
    coverage |> select(yr, mon) |> distinct()
  ) |>
    anti_join(coverage, by = c("iso3c", "yr", "mon"))

  if (nrow(missing) > 0) {
    message(sprintf("  Found %d missing country-month combinations", nrow(missing)))

    dbWriteTable(con, "missing_coverage", missing, temporary = TRUE)

    dbExecute(con, glue("
      CREATE TEMP TABLE mirrored_data AS
      SELECT
        e.partner_iso3c AS iso3c, e.yr, e.mon,
        e.partner_country AS country,
        CASE WHEN e.flow_desc = 'Export' THEN 'Import' ELSE 'Export' END AS flow_desc,
        e.iso3c AS partner_iso3c,
        e.country AS partner_country,
        e.cmd_code, e.commodity_name, e.category, e.subcategory,
        e.qty_unit_code,
        CASE
          WHEN e.flow_desc = 'Export' THEN
            e.primary_value * COALESCE(r.cif_fob_ratio, 1.10)
          ELSE e.primary_value / COALESCE(r.cif_fob_ratio, 1.10)
        END AS primary_value,
        CASE
          WHEN e.flow_desc = 'Export' THEN
            e.constant_value_2024 * COALESCE(r.cif_fob_ratio, 1.10)
          ELSE e.constant_value_2024 / COALESCE(r.cif_fob_ratio, 1.10)
        END AS constant_value_2024,
        e.cifvalue, e.fobvalue, e.cmd_desc_comtrade,
        e.qty_comtrade, e.net_wgt_comtrade, e.qty_unit_price,
        TRUE AS mirrored
      FROM existing_data e
      LEFT JOIN cif_fob_ratios r ON e.cmd_code = r.cmd_code
      SEMI JOIN (SELECT * FROM missing_coverage) m
        ON m.iso3c = e.partner_iso3c
        AND m.yr = e.yr AND m.mon = e.mon
    "))

    mirrored_n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM mirrored_data")$n
    message(sprintf("  Created %d mirrored records", mirrored_n))

    mirrored_data      <- dbGetQuery(con, "SELECT * FROM mirrored_data") |> as_tibble()
    affected_partitions <- mirrored_data |> distinct(iso3c, yr)
    dbWriteTable(con, "affected_partitions", affected_partitions, temporary = TRUE)

    existing_affected <- dbGetQuery(con, "
      SELECT e.*
      FROM existing_data e
      INNER JOIN affected_partitions a ON e.iso3c = a.iso3c AND e.yr = a.yr
    ") |> as_tibble()

    write_dataset(
      bind_rows(existing_affected, mirrored_data),
      parquet_dir,
      partitioning           = c("iso3c", "yr"),
      format                 = "parquet",
      compression            = "zstd",
      existing_data_behavior = "delete_matching"
    )

    message(sprintf("  Mirrored %d records across %d partitions",
                    mirrored_n, nrow(affected_partitions)))
  } else {
    message("  No missing flows to mirror")
  }

  elapsed <- round(as.numeric(Sys.time() - pipeline_start, units = "mins"), 1)
  message(sprintf("--- Comtrade build complete in %.1f min ---", elapsed))

  invisible(list(
    files_processed = length(files_to_process),
    total_time_min  = elapsed
  ))
}

# -- Run -----------------------------------------------------------------------

run_comtrade_build(
  csv_dir      = COMTRADE_CSV_DIR,
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
