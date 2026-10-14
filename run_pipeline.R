# run_pipeline.R
# ------------------------------------------------------------------------------
# Top-level orchestrator. Run from the repo root:
#
#   Rscript run_pipeline.R
#   # or interactively: setwd("econ-risk-pipeline"); source("run_pipeline.R")
#
# All source() paths are relative to the repo root.
# config.R is sourced first -- all downstream scripts inherit its values.
#
# Comtrade download (step 0) is commented out by default.
# Uncomment only when refreshing raw trade data from the UN Comtrade API.
# ------------------------------------------------------------------------------

# install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
# cmdstanr::install_cmdstan()
# 
# cmdstanr::check_cmdstan_toolchain()
# cmdstanr::cmdstan_version()

# start_time <- Sys.time() 

library(DBI)
library(duckdb)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

options(error = NULL)   # clear any stale error handler (e.g. recover from .Rprofile)

# -- Load .env if present (API keys, etc.) -------------------------------------

if (file.exists(".env")) readRenviron(".env")

# -- Config (must come first) --------------------------------------------------

source("config.R")

# -- DB connection -------------------------------------------------------------

dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)
con <- dbConnect(duckdb(), DB_PATH)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

# FORCE_UPDATE <- TRUE

# BAYES_ALGO <- "sampling"
# BAYES_ALGO <- "pathfinder"

# -- Country frame -------------------------------------------------------------
# Inner-joined in silver transforms to filter to 175 sovereign states.
# iso3c is the universal country key across all tables.

COUNTRY_FRAME <- read_csv(
  COUNTRY_FRAME_CSV,
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  distinct(iso3c, .keep_all = TRUE)   # drop duplicate SRB row

dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
dbWriteTable(con, DBI::Id(schema = "bronze", table = "country_frame"),
             COUNTRY_FRAME, overwrite = TRUE)

message(sprintf("  [bronze.country_frame] %d countries loaded", nrow(COUNTRY_FRAME)))

# -- Step 0: Comtrade download and parquet build (one-off) --------------------
# Only uncomment when rebuilding the parquet from raw CSVs.
# Free API tier: 500 calls/day, ~346 per year (173 countries x 2 chunks).
# Full history (2010-2025) takes ~11 days. skip_existing = TRUE lets you resume.

# Requires: COMTRADE_API_KEY in .env
# Step 0a: downloads CSVs to data/comtrade_csv/
# Step 0b: converts CSVs to parquet, applies CIF/FOB correction, mirrors gaps

# # FORCE_UPDATE <- TRUE 
# # source("R/ingest_comtrade_download.R")   # step 0a: download CSVs
# source("R/ingest_comtrade_build.R")
# # FORCE_UPDATE <- FALSE  # reset
# arrow::open_dataset(COMTRADE_PARQUET)$schema$names

# -- Bronze ingest -------------------------------------------------------------
#  
# source("R/ingest_gdp.R") 
# source("R/ingest_population.R")
# source("R/ingest_imf.R")
# source("R/ingest_wgi.R")
# source("R/ingest_wto.R")
# source("R/ingest_usgs.R")
# source("R/ingest_worldbank.R")
# 
# source("R/ingest_price_series.R")
# source("R/ingest_comtrade.R")
# 
# # -- Silver transforms ---------------------------------------------------------
# 
# source("R/compute_silver_comtrade.R")
# source("R/compute_silver_usgs.R")
# source("R/transform_silver.R")
# 
# # -- Gold: Bayesian models -----------------------------------------------------
# # 
# 
# start_time <- Sys.time()
# source("R/models/compute_bayes_d1_macro.R")
# 
# source("R/models/compute_bayes_d2_annual.R")
# source("R/models/compute_bayes_d3_annual.R")
# # # 
# source("R/models/compute_bayes_d2_monthly.R")
# source("R/models/compute_bayes_d3_monthly.R")
# # 
# source("R/models/compute_d4_network.R")

# Sys.time() - start_time  ## 8.5 hours on 3/14 run. First complete run. Conservative time. some config hiccups

# -- Gold: context, tiers, changes ---------------------------------------------
# 
# source("R/compute_context_layer.R")
# source("R/compute_tiers.R")
# source("R/compute_changes.R")  
# 
# # -- Gold: narratives ----------------------------------------------------------
# 
source("R/compute_narratives-4-14.R")
# 
# message("Pipeline run complete: ", Sys.time())

source("R/view_table.R")
list_tables()
narratives <- view_table("gold.narratives", max_rows = 700000) |> 
  mutate(narrative = str_replace_all(narrative,"enters 2025", "enters 2026"))
 # write_csv(narratives, "shiny/check.csv") 
# |>
#   # filter(year > 2014)
#   mutate(across(contains("_score"), ~round(.x, 2)))

write_gold(narratives, "narratives", con)

unique(check$commodity_group)


con <- dbConnect(duckdb(), dbdir = "data/econrisk_2015.duckdb", read_only = TRUE)

dbGetQuery(con, "
  SELECT table_schema, table_name, column_name, data_type
  FROM information_schema.columns
  WHERE table_schema IN ('bronze', 'gold', 'silver')
  ORDER BY table_schema, table_name, ordinal_position
") |> as.data.frame() |> 
  # filter(str_detect(table_name, "d[1-4]_", negate = T)) |> 
  print(max = 9999) |> 
  write_csv("check.csv")



dbDisconnect(con, shutdown = TRUE)


# dplyr::filter(year == 2023) |> 
#   arrange(cat1_score)
# write_csv(check, "check.csv")
# #   filter(indicator_code== "GGXCNL_NGDP" & value < -20) |> 
# #   select(iso3c, year, value) |> 
# #   arrange(iso3c, year)
# # write_csv(check, "GGXCNL_NGDP_outliers.csv")
# # # view_table("silver.comtrade_import_groups_ts")
# # # df <- view_table("silver.comtrade_export_groups_monthly", max_rows = 5000)
# 
# 
# dbDisconnect(con, shutdown = TRUE)

narratives_wide <- narratives |>
  pivot_wider(names_from = dimension, values_from = narrative)

narratives <- dbGetQuery(con, "
  SELECT country_code, country_name, year, dimension, narrative
  FROM gold.narratives
  ORDER BY country_code, dimension
") |> as_tibble()

write_csv(narratives, "check_narratives_long.csv")



d2_monthly <- dbGetQuery(con, "
  SELECT *
  FROM gold.d2_monthly_posterior
  ORDER BY country_code, year, month
") |> as_tibble() |> 
  select(country_name, year, month, risk_score, everything())

head(d2_monthly, n = 20)
write_csv(d2_monthly, "check_d2m.csv")

d3_monthly <- dbGetQuery(con, "
  SELECT *
  FROM gold.d3_monthly_posterior
  ORDER BY country_code, year, month
") |> as_tibble()|> 
  select(country_name, year, month, risk_score, everything())

head(d3_monthly, n = 20)
write_csv(d3_monthly, "check_d3m.csv")



# source("R/models/compute_monthly_standin.R")     # demo stand-in
