# ingest_comtrade_download.R
# ------------------------------------------------------------------------------
# Step 0a: Download monthly bilateral trade CSVs from the UN Comtrade API.
#
# Collects trade data for 174 HS commodity codes (WB Pink Sheet + US critical
# minerals) across 173 countries, one country-year at a time.
#
# Output: one CSV per country-year in COMTRADE_CSV_DIR
#   e.g. data/comtrade_csv/USA_2024.csv
#
# Run ingest_comtrade_build.R afterward to convert CSVs to parquet.
#
# API limits (free tier): 500 calls/day.
# 173 countries x 2 chunks = 346 calls per year of data.
# Full history (2015-2025) takes ~11 days on the free tier.
# skip_existing = TRUE lets you resume across sessions.
#
# Requires: COMTRADE_API_KEY in .env
# Does NOT require: a DB connection (standalone script)
# ------------------------------------------------------------------------------

library(comtradr)
library(tidyverse)
library(glue)

# -- Helpers -------------------------------------------------------------------

leading_zero_cmd_codes <- function(x) {
  x_char <- as.character(x)
  ifelse(nchar(x_char) %% 2 == 1, paste0("0", x_char), x_char)
}

if (!exists("COMTRADE_CSV_DIR"))    stop("Source config.R first or run via run_pipeline.R")
if (!exists("COMTRADE_API_KEY"))    stop("COMTRADE_API_KEY not set -- check .env")
if (COMTRADE_API_KEY == "")         stop("COMTRADE_API_KEY is empty -- check .env")

set_primary_comtrade_key(COMTRADE_API_KEY)

# -- Config --------------------------------------------------------------------

COMMODITY_CHUNK_SIZE <- 87    # codes per API call (2 chunks for 174 codes)
API_RATE_LIMIT       <- 500   # free tier: 500 calls per day
MAX_ROWS_PER_CALL    <- 1e5   # Comtrade hard limit per call

# -- Country list --------------------------------------------------------------
# Derived from the pipeline country frame.
# Excludes Kosovo (XKX) and Taiwan (TWN) -- not recognized by Comtrade.
# Adds Hong Kong (HKG) as a separate reporter.

use_iso3c <- read_csv(COUNTRY_FRAME_CSV,
                      col_types = cols(.default = "c"),
                      show_col_types = FALSE) |>
  distinct(iso3c, .keep_all = TRUE) |>       # drop duplicate SRB
  filter(!(iso3c %in% c("XKX", "TWN"))) |>
  add_row(iso3c = "HKG") |>
  arrange(iso3c) |>
  pull(iso3c)

# -- Commodity codes -----------------------------------------------------------

all_cmd_code <- read_csv(COMMODITY_CODES_CSV,
                         col_types = cols(.default = "c"),
                         show_col_types = FALSE)

commodity_codes <- leading_zero_cmd_codes(all_cmd_code$cmd_code)

message(sprintf("  Countries: %d | Commodity codes: %d | Commodities: %d",
                length(use_iso3c),
                length(commodity_codes),
                length(unique(all_cmd_code$commodity_name))))

# -- Helpers -------------------------------------------------------------------

leading_zero_cmd_codes <- function(x) {
  x_char <- as.character(x)
  ifelse(nchar(x_char) %% 2 == 1, paste0("0", x_char), x_char)
}

chunk_commodity_codes <- function(codes, chunk_size = COMMODITY_CHUNK_SIZE) {
  split(codes, ceiling(seq_along(codes) / chunk_size))
}

# Download one country-year-chunk; append subsequent chunks to the same file
comtrade_dat_fun <- function(iso3c, all_iso3c, commodity_codes_chunk,
                              start_yrmon, end_yrmon,
                              output_dir = COMTRADE_CSV_DIR,
                              append = FALSE) {

  year_str  <- str_extract(start_yrmon, "20[0-9]{2}")
  out_file  <- file.path(output_dir, paste0(iso3c, "_", year_str, ".csv"))

  tryCatch({
    dat <- comtradr::ct_get_data(
      reporter        = iso3c,
      partner         = all_iso3c[!(all_iso3c %in% iso3c)],
      commodity_code  = commodity_codes_chunk,
      start_date      = start_yrmon,
      end_date        = end_yrmon,
      frequency       = "M",
      flow_direction  = c("export", "import")
    )

    Sys.sleep(runif(1, 1, 3))   # respectful API usage

    if (append && file.exists(out_file)) {
      write_csv(dat, out_file, append = TRUE)
      cat(glue("  Appended {nrow(dat)} rows to {basename(out_file)}\n\n"))
    } else {
      write_csv(dat, out_file)
      cat(glue("  Saved {nrow(dat)} rows to {basename(out_file)}\n\n"))
    }

    return(TRUE)

  }, error = function(e) {
    cat(glue("  Error: {iso3c} {start_yrmon} chunk -- {e$message}\n\n"))
    if (!file.exists(out_file)) write_csv(tibble(), out_file)  # mark attempted
    return(FALSE)
  })
}

# Main loop: countries x years x commodity chunks
download_comtrade_data <- function(iso3c_vec, commodity_codes,
                                   start_yrmon_vec, end_yrmon_vec,
                                   output_dir     = COMTRADE_CSV_DIR,
                                   chunk_size     = COMMODITY_CHUNK_SIZE,
                                   skip_existing  = TRUE) {

  commodity_chunks <- chunk_commodity_codes(commodity_codes, chunk_size)
  total_calls <- length(iso3c_vec) * length(start_yrmon_vec) * length(commodity_chunks)

  cat(glue("\n=== COMTRADE DOWNLOAD ===\n",
           "Countries: {length(iso3c_vec)} | ",
           "Years: {length(start_yrmon_vec)} | ",
           "Chunks: {length(commodity_chunks)} | ",
           "Est. calls: {total_calls}\n\n"))

  if (total_calls > API_RATE_LIMIT)
    cat(glue("WARNING: {total_calls} calls exceeds daily limit ({API_RATE_LIMIT}). ",
             "Requires ~{ceiling(total_calls / API_RATE_LIMIT)} days.\n\n"))

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  n_success <- 0L; n_fail <- 0L; n_skip <- 0L

  for (iso3c in iso3c_vec) {
    for (m in seq_along(start_yrmon_vec)) {
      year_str <- str_extract(start_yrmon_vec[m], "20[0-9]{2}")
      out_file <- file.path(output_dir, paste0(iso3c, "_", year_str, ".csv"))

      if (skip_existing && file.exists(out_file) &&
          file.info(out_file)$size / 1024^2 > 0.008) {
        cat(glue("Skipping {basename(out_file)} (exists)\n\n"))
        n_skip <- n_skip + 1L
        next
      }

      cat(glue("Downloading {iso3c} {year_str}:\n\n"))

      for (chunk_idx in seq_along(commodity_chunks)) {
        ok <- comtrade_dat_fun(
          iso3c                = iso3c,
          all_iso3c            = iso3c_vec,
          commodity_codes_chunk = commodity_chunks[[chunk_idx]],
          start_yrmon          = start_yrmon_vec[m],
          end_yrmon            = end_yrmon_vec[m],
          output_dir           = output_dir,
          append               = chunk_idx > 1
        )
        if (ok) n_success <- n_success + 1L else n_fail <- n_fail + 1L
      }
    }
  }

  cat(glue("\n=== DOWNLOAD COMPLETE ===\n",
           "Successful: {n_success} | Failed: {n_fail} | Skipped: {n_skip}\n\n"))

  invisible(list(successful = n_success, failed = n_fail, skipped = n_skip))
}

# -- Run -----------------------------------------------------------------------
# Edit the year index to select which year(s) to download.
# Years are listed newest-first; [1] = 2025, [2] = 2024, ..., [11] = 2015.
# Completed: 2016-2025 (update this comment as years are finished)

years_newest_first <- 2025:2015

start_yrmon_vec <- paste0(years_newest_first, "-01")
end_yrmon_vec   <- paste0(years_newest_first, "-12")

# Download a single year (change the index to run a different year)
year_idx <- 1   # 1 = 2025

download_comtrade_data(
  iso3c_vec      = use_iso3c,
  commodity_codes = commodity_codes,
  start_yrmon_vec = start_yrmon_vec[year_idx],
  end_yrmon_vec   = end_yrmon_vec[year_idx],
  output_dir     = COMTRADE_CSV_DIR,
  chunk_size     = COMMODITY_CHUNK_SIZE,
  skip_existing  = TRUE
)

# To download all years sequentially (multi-day run):
# for (i in seq_along(start_yrmon_vec)) {
#   download_comtrade_data(
#     iso3c_vec       = use_iso3c,
#     commodity_codes = commodity_codes,
#     start_yrmon_vec = start_yrmon_vec[i],
#     end_yrmon_vec   = end_yrmon_vec[i],
#     output_dir      = COMTRADE_CSV_DIR,
#     chunk_size      = COMMODITY_CHUNK_SIZE,
#     skip_existing   = TRUE
#   )
# }
