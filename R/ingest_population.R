# ingest_population.R
# ------------------------------------------------------------------------------
# Bronze-layer ingestion of population data from US Census IDB API.
# Not used in any Bayesian model -- context/normalization data for dashboard.
#
# -- Source --------------------------------------------------------------------
#   US Census Bureau International Data Base (IDB)
#   https://api.census.gov/data/timeseries/idb/1year
#   Requires CENSUS_API_KEY in environment or .env file.
#
# -- Coverage ------------------------------------------------------------------
#   Total population (SEX=0, all ages summed) for each year in CENSUS_YEARS.
#   One API call per year; results stacked.
#   Joins on iso2c (GENC 2-letter code) -> iso3c via bronze.country_frame.
#
# -- Output --------------------------------------------------------------------
#   bronze.population_annual   iso3c, year, population
#
# Expects: `con` from run_pipeline.R; CENSUS_API_KEY in environment or .env
# ------------------------------------------------------------------------------

library(DBI)
library(dplyr)
library(jsonlite)
library(httr)
library(purrr)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# Adjust range as needed; upper bound is last completed year
CENSUS_YEARS <- seq(2013L, as.integer(format(Sys.Date(), "%Y")) - 1L)

CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")
if (CENSUS_API_KEY == "")
  stop("CENSUS_API_KEY not set -- add to .env file")

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

country_frame <- dbGetQuery(con,
  "SELECT iso2c, iso3c FROM bronze.country_frame"
) |> as_tibble()

fetch_census_pop <- function(year, api_key) {
  url <- paste0(
    "https://api.census.gov/data/timeseries/idb/1year",
    "?get=NAME,GENC,POP&YR=", year,
    "&AGE=0:100&SEX=0",         # SEX=0 = total; sum across ages below
    "&for=genc+standard+countries+and+areas",
    "&key=", api_key
  )

  resp <- tryCatch(
    httr::GET(url),
    error = function(e) {
      warning(sprintf("Census IDB %d: request failed -- %s", year, conditionMessage(e)))
      return(NULL)
    }
  )
  if (is.null(resp)) return(NULL)

  # Census API sometimes returns 200 with an HTML error page
  content_type <- httr::headers(resp)[["content-type"]]
  body_text    <- httr::content(resp, "text", encoding = "UTF-8")

  if (httr::http_error(resp) || !grepl("json", content_type, ignore.case = TRUE)) {
    warning(sprintf("Census IDB %d: unexpected response (content-type: %s) -- %s",
                    year, content_type,
                    substr(body_text, 1, 300)))
    return(NULL)
  }

  raw <- tryCatch(
    fromJSON(body_text),
    error = function(e) {
      warning(sprintf("Census IDB %d: JSON parse failed -- %s", year, conditionMessage(e)))
      return(NULL)
    }
  )
  if (is.null(raw)) return(NULL)

  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  names(df) <- df[1, ]
  df <- df[-1, ]

  df |>
    transmute(
      iso2c = GENC,
      year  = as.integer(YR),
      age   = as.integer(AGE),
      pop   = as.numeric(POP)
    ) |>
    group_by(iso2c, year) |>
    summarise(population = sum(pop, na.rm = TRUE), .groups = "drop")
}

message(sprintf("--- Population: fetching %d years (%d-%d) from Census IDB ---",
                length(CENSUS_YEARS), min(CENSUS_YEARS), max(CENSUS_YEARS)))

pop_raw <- map(CENSUS_YEARS, fetch_census_pop, api_key = CENSUS_API_KEY,
               .progress = TRUE) |>
  compact() |>
  list_rbind()

if (nrow(pop_raw) == 0)
  stop("No data returned from Census IDB -- check CENSUS_API_KEY and network access")

pop_annual <- pop_raw |>
  inner_join(country_frame, by = "iso2c") |>
  select(iso3c, year, population) |>
  filter(!is.na(iso3c)) |>
  arrange(iso3c, year)

message(sprintf("  %d country-years | %d countries | years %d-%d",
                nrow(pop_annual), n_distinct(pop_annual$iso3c),
                min(pop_annual$year), max(pop_annual$year)))

write_bronze(pop_annual, "population_annual", con)

message("--- Population ingest complete ---")
