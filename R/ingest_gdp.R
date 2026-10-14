# ingest_gdp.R
# ------------------------------------------------------------------------------
# Bronze-layer ingestion of GDP data from local CSV files.
# Not used in any Bayesian model -- context/normalization data for dashboard.
#
# -- Source --------------------------------------------------------------------
#   CSV files at GDP_RAW_PATH (data/gdp_raw/)
#   Key: gwcode -> joined to bronze.country_frame for iso3c
#
# -- Columns -------------------------------------------------------------------
#   gdp_const_usd             NY.GDP.MKTP.KD    constant USD
#   gdp_growth_pct            NY.GDP.MKTP.KD.ZG growth %
#   gdp_per_capita_const_usd  NY.GDP.PCAP.KD    per capita constant USD
#   gdp_per_capita_growth_pct NY.GDP.PCAP.KD.ZG per capita growth %
#
# -- Output --------------------------------------------------------------------
#   bronze.gdp_annual   iso3c, year, gdp_const_usd, gdp_growth_pct,
#                       gdp_per_capita_const_usd, gdp_per_capita_growth_pct
#
# Expects: `con` from run_pipeline.R
# ------------------------------------------------------------------------------

library(DBI)
library(dplyr)
library(readr)
library(purrr)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

GDP_RAW_PATH <- "data/gdp_raw"

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

message("--- GDP: loading from ", GDP_RAW_PATH, " ---")

gdp_files <- list.files(GDP_RAW_PATH, pattern = "\\.csv$",
                        full.names = TRUE, recursive = TRUE)

if (length(gdp_files) == 0)
  stop("No CSV files found in GDP_RAW_PATH: ", GDP_RAW_PATH)

country_frame <- dbGetQuery(con,
  "SELECT gwcode, iso3c FROM bronze.country_frame"
) |> as_tibble() |>
  mutate(gwcode = as.integer(gwcode))

gdp_raw <- read_csv(
  gdp_files, col_types = cols(.default = "d"), show_col_types = FALSE
  ) |>
  mutate(gwcode = as.integer(gwcode),
         year   = as.integer(year)) |> 
  filter(year >= START_YR) 

gdp_annual <- gdp_raw |>
  inner_join(country_frame, by = "gwcode") |>
  transmute(
    iso3c,
    year,
    gdp_const_usd             = NY.GDP.MKTP.KD,
    gdp_growth_pct            = NY.GDP.MKTP.KD.ZG,
    gdp_per_capita_const_usd  = NY.GDP.PCAP.KD,
    gdp_per_capita_growth_pct = NY.GDP.PCAP.KD.ZG
  ) |>
  arrange(iso3c, year)


write_bronze(gdp_annual, "gdp_annual", con)

message("--- GDP ingest complete ---")
