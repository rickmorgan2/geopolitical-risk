# ingest_usgs.R
#
# Bronze-layer ingest: USGS Mineral Commodity Summaries (MCS)
# World mine production by country and world reserve totals for critical minerals.
#
# -- Data source ----------------------------------------------------------------
#   USGS National Minerals Information Center, published annually (~January)
#   Individual commodity CSV files retrieved via the sbtools R package.
#   https://www.sciencebase.gov/catalog/
#
# -- Discovery strategy -------------------------------------------------------
#   Uses sbtools::query_sb_text() to find MCS Data Release parent items by year.
#   No hardcoded ScienceBase IDs — new MCS editions are picked up automatically.
#   item_list_children() navigates to individual commodity items.
#   item_list_files()    retrieves CSV download URLs.
#   item_file_download() pulls each world production CSV.
#
# -- What each MCS edition provides -------------------------------------------
#   World production CSV: country-level mine production for 2 years
#     (prior year = actuals; current year = estimates)
#   Reserves: world total only in CSV; country-level is PDF-only (future phase)
#
# -- Time series strategy ------------------------------------------------------
#   Stacking MCS 2020-2025 gives production back to ~2018.
#   MCS_YEAR_RANGE controls which editions to fetch.
#
# -- Mineral crosswalk notes ---------------------------------------------------
#   Not all comtrade critical_mineral commodities have a USGS MCS equivalent:
#     - cermets, ferrocerium          → manufactured products, not in MCS
#     - borotellurate                 → mapped to tellurium
#     - cerium                        → mapped to rare earths (aggregate)
#     - ferromanganese, arsenic sulfide, magnesium sulfate → parent commodity
#   ~22 of 33 comtrade critical minerals map to a distinct USGS commodity.
#
# -- Output tables -------------------------------------------------------------
#   bronze.usgs_production   country-mineral-year, mine production qty + unit
#   bronze.usgs_reserves     mineral-edition, world total reserves qty + unit
#
# Expects: `con` (DBI connection to georisk.duckdb) from run_pipeline.R

library(DBI)
library(sbtools)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(glue)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# -- Config ---------------------------------------------------------------------

RAW_DIR <- "data/raw_usgs"
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

MCS_YEAR_RANGE <- 2022:2026  # editions to fetch; extend to go further back

# -- Mineral crosswalk notes ---------------------------------------------------
#   Not all comtrade critical_mineral commodities have a USGS MCS equivalent:
#     - cermets, ferrocerium          → manufactured products, not in MCS
#     - borotellurate                 → mapped to tellurium
#     - cerium                        → mapped to rare earths (aggregate)
#     - ferromanganese, arsenic sulfide, magnesium sulfate → parent commodity
#   ~22 of 33 comtrade critical minerals map to a distinct USGS commodity.
#
# -- Output tables -------------------------------------------------------------
#   bronze.usgs_production   country-mineral-year, mine production qty + unit
#   bronze.usgs_reserves     mineral-edition, world total reserves qty + unit
#
# Expects: `con` (DBI connection to georisk.duckdb) from run_pipeline.R

# -- Mineral crosswalk ---------------------------------------------------------
# comtrade commodity_name → USGS MCS commodity name as it appears in ScienceBase
# NA = no direct USGS MCS equivalent; excluded from download
MINERAL_CROSSWALK <- tribble(
  ~comtrade_name,       ~usgs_name,
  "aluminum",           "ALUMINUM",
  "antimony",           "ANTIMONY",
  "arsenic sulfide",    "ARSENIC",
  "barium",             "BARITE",
  "beryllium",          "BERYLLIUM",
  "bismuth",            "BISMUTH",
  "borotellurate",      "TELLURIUM",
  "cerium",             "RARE EARTHS",
  "cermets",            NA_character_,      # manufactured product
  "chromium",           "CHROMIUM",
  "cobalt",             "COBALT",
  "copper",             "COPPER",
  "ferrocerium",        NA_character_,      # manufactured product
  "ferromanganese",     "MANGANESE",
  "fluorine",           "FLUORSPAR",
  "fluorspar",          "FLUORSPAR",
  "gold",               "GOLD",
  "graphite",           "GRAPHITE",
  "lead",               "LEAD",
  "lithium",            "LITHIUM",
  "magnesite",          "MAGNESIUM COMPOUNDS",
  "magnesium",          "MAGNESIUM METAL",
  "magnesium sulfate",  "MAGNESIUM COMPOUNDS",
  "manganese",          "MANGANESE",
  "nickel",             "NICKEL",
  "platinum",           "PLATINUM-GROUP METALS",
  "silver",             "SILVER",
  "tantalum",           "NIOBIUM AND TANTALUM",
  "tin",                "TIN",
  "titanium",           "TITANIUM",
  "tungsten",           "TUNGSTEN",
  "zinc",               "ZINC",
  "zirconium",          "ZIRCONIUM AND HAFNIUM"
)

# Unique USGS commodity names to fetch (deduplicated, NAs dropped)
usgs_targets <- MINERAL_CROSSWALK |>
  filter(!is.na(usgs_name)) |>
  distinct(usgs_name) |>
  pull(usgs_name)

# -- ScienceBase discovery via sbtools -----------------------------------------

# Find MCS Data Release parent items for each target year.
# Title pattern: "Mineral Commodity Summaries {YEAR} Data Release"
# query_sb_text() searches across title + description fields.
find_mcs_edition <- function(year) {
  query_text <- glue("Mineral Commodity Summaries {year} Data Release")
  message(sprintf("  Searching ScienceBase: '%s'", query_text))

  results <- tryCatch(
    query_sb_text(query_text, limit = 100),
    error = \(e) { warning(glue("SB query failed [{year}]: {e$message}")); NULL }
  )
  if (is.null(results) || length(results) == 0) return(NULL)

  # Title formats by edition:
  #   2022+:     "Mineral Commodity Summaries 2022 Data Release"
  #   2020-2021: "U.S. Geological Survey Mineral Commodity Summaries 2020 Data Release"
  # Individual commodity items look like: "Mineral Commodity Summaries 2024 - COBALT Data Release"
  # Exclude those by requiring the year to be immediately followed by " Data Release"
  # (no dash + commodity name in between).
  matched <- keep(results, \(item) {
    str_detect(
      str_to_lower(item$title),
      glue("mineral commodity summar.*{year}\\s+data release")
    )
  })

  if (length(matched) == 0) {
    warning(glue("  No matching MCS {year} parent item found on ScienceBase"))
    return(NULL)
  }

  # Return the first match — should only be one per year
  item <- matched[[1]]
  message(sprintf("  Found: '%s' [%s]", item$title, item$id))
  item
}

# Get commodity child items from an edition's parent sbitem.
# Returns a tibble of item_id + title for children that look like
# individual commodity data releases (not the aggregate stats items).
get_commodity_children <- function(edition_item) {
  children <- tryCatch(
    item_list_children(edition_item$id),
    error = \(e) { warning(glue("item_list_children failed: {e$message}")); NULL }
  )
  if (is.null(children) || length(children) == 0) return(NULL)

  tibble(
    item_id = map_chr(children, "id"),
    title   = map_chr(children, "title")
  ) |>
    # Keep only individual commodity items; drop aggregate/overview items
    filter(
      !str_detect(str_to_lower(title),
                  "salient|industry trend|statistic|overview|appendix|revision")
    )
}

# Get world-production CSV file URL for a commodity item.
# item_list_files() returns a data.frame: fname, size, url, facet
get_world_production_url <- function(item_id) {
  files <- tryCatch(
    item_list_files(item_id),
    error = \(e) { warning(glue("item_list_files failed [{item_id}]: {e$message}")); NULL }
  )
  if (is.null(files) || nrow(files) == 0) return(NULL)

  # Keep only CSVs; prefer "worldproduction" in the name over US salient stats
  csv_files <- files |>
    filter(str_detect(str_to_lower(fname), "\\.csv$")) |>
    filter(!str_detect(str_to_lower(fname), "salient|us-|_us_|usstat|trend|industry"))

  if (nrow(csv_files) == 0) return(NULL)

  # If multiple CSVs remain, prefer the one with "world" in the name
  world_csv <- csv_files |>
    arrange(desc(str_detect(str_to_lower(fname), "world"))) |>
    slice(1)

  world_csv$url
}

# Download a CSV to local cache using sbtools item_file_download().
# Falls back to direct URL download if item_file_download not applicable.
download_csv_cached <- function(item_id, fname, url, dest_dir) {
  dest_path <- file.path(dest_dir, fname)
  if (file.exists(dest_path)) return(dest_path)   # use cache

  result <- tryCatch(
    item_file_download(item_id, names = fname, destinations = dest_path),
    error = \(e) { warning(glue("item_file_download failed: {e$message}")); NULL }
  )

  if (!is.null(result) && file.exists(dest_path)) return(dest_path)

  # Fallback: direct URL download
  tryCatch({
    download.file(url, dest_path, mode = "wb", quiet = TRUE)
    dest_path
  }, error = \(e) {
    warning(glue("Direct download failed [{fname}]: {e$message}"))
    NULL
  })
}

# -- Parse world production CSV ------------------------------------------------
# Actual USGS CSV column structure (observed):
#   Source, Country, Type, Prod_t_est_2020, Prod_t_est_2021   (MCS 2022)
#   Source, Country, Type, Prod_t_2021, Prod_t_est_2022,
#     Prod_notes, Reserves_t, Reserves_notes                   (MCS 2023+)
#
# Key differences from naive assumptions:
#   - Year columns are Prod_t_YYYY or Prod_t_est_YYYY, not bare YYYY
#   - is_estimate flag comes from "_est_" in column name, not "e" suffix on value
#   - MCS 2023+ includes Reserves_t per country — country-level reserves!
#   - Unit is in the Type column values, not in column headers

parse_world_production <- function(csv_path, usgs_commodity, mcs_edition) {
  raw <- tryCatch(
    read_csv(csv_path, col_types = cols(.default = "c"), show_col_types = FALSE),
    error = \(e) { warning(glue("Parse failed [{csv_path}]: {e$message}")); NULL }
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # Drop unnamed trailing columns (e.g. ...9, ...10) that appear in some editions
  raw <- raw |> select(-matches("^\\.\\.\\.\\d+$"))

  # Country column: always "Country" in observed files; keep fallback
  country_col <- names(raw)[str_detect(str_to_lower(names(raw)), "^country")][1]
  if (is.na(country_col)) {
    warning(glue("Cannot identify country column in {basename(csv_path)}"))
    return(NULL)
  }

  # Year columns: Prod_t_YYYY, Prod_kt_YYYY, Prod_t_est_YYYY, Prod_kt_Est_YYYY etc.
  # Unit (t vs kt) embedded in column name; captured for the unit field below.
  year_cols <- names(raw)[str_detect(names(raw), regex("^Prod_[a-z]+_.*20[0-9]{2}", ignore_case = TRUE))]

  if (length(year_cols) == 0) {
    warning(glue("No year columns found in {basename(csv_path)}"))
    return(NULL)
  }

  # Unit: extract from the Type column (e.g. "metric tons of contained cobalt")
  # Use the most common non-NA value
  # Guard: Type column absent in some per-commodity CSVs
  unit <- if ("Type" %in% names(raw)) {
    raw$Type |> na.omit() |> (\(x) names(sort(table(x), decreasing = TRUE))[1])()
  } else NULL
  if (is.null(unit) || length(unit) == 0) unit <- "see source"

  # Exclude world/total summary rows from production data
  prod_rows <- raw |>
    filter(
      !is.na(.data[[country_col]]),
      .data[[country_col]] != "",
      !str_detect(.data[[country_col]],
                  regex("world|total|^note|^source|^unit|^data", ignore_case = TRUE))
    )

  # Pivot production year columns to long format
  prod_long <- prod_rows |>
    select(country_raw = all_of(country_col), all_of(year_cols)) |>
    pivot_longer(
      cols      = all_of(year_cols),
      names_to  = "year_raw",
      values_to = "value_raw"
    ) |>
    mutate(
      year        = as.integer(str_extract(year_raw, "20[0-9]{2}")),
      is_estimate = str_detect(year_raw, regex("_est_", ignore_case = TRUE)),
      value_clean = str_remove_all(value_raw %||% "", "[,\\s]"),
      value_clean = na_if(value_clean, "W"),         # W = withheld
      value_clean = na_if(value_clean, "—"),
      value_clean = na_if(value_clean, "--"),
      value_clean = na_if(value_clean, "-"),
      production  = suppressWarnings(as.numeric(value_clean)),
      unit           = unit,
      usgs_commodity = usgs_commodity,
      mcs_edition    = as.integer(mcs_edition)
    ) |>
    filter(!is.na(country_raw), !is.na(year)) |>
    select(country_raw, usgs_commodity, mcs_edition, year, production, unit, is_estimate)

  # Country-level reserves — present in MCS 2023+ as Reserves_t column
  # This is a significant upgrade over world totals only
  reserves <- NULL
  reserves_col <- names(raw)[str_detect(names(raw), regex("^Reserves_[a-z]+$", ignore_case = TRUE))][1]

  if (!is.na(reserves_col)) {
    reserves <- prod_rows |>
      select(country_raw = all_of(country_col), reserves_raw = all_of(reserves_col)) |>
      mutate(
        reserves_clean = str_remove_all(reserves_raw %||% "", "[,\\s]"),
        reserves_clean = na_if(reserves_clean, "W"),
        reserves_clean = na_if(reserves_clean, "—"),
        reserves_clean = na_if(reserves_clean, "--"),
        reserves_clean = na_if(reserves_clean, "-"),
        reserves       = suppressWarnings(as.numeric(reserves_clean)),
        usgs_commodity = usgs_commodity,
        mcs_edition    = as.integer(mcs_edition),
        unit           = unit
      ) |>
      filter(!is.na(country_raw), !is.na(reserves)) |>
      select(country_raw, usgs_commodity, mcs_edition, reserves, unit)
  }

  list(production = prod_long, reserves = reserves)
}

# -- Parse MCS 2025 consolidated CSV -------------------------------------------
# Format: single wide file covering all commodities
# Columns: SOURCE, COMMODITY, COUNTRY, TYPE, UNIT_MEAS,
#          PROD_2023, PROD_EST_ 2024 (note space), PROD_NOTES,
#          CAP_2023, CAP_EST_ 2024, CAP_NOTES, RESERVES_2024, RESERVE_NOTES

parse_mcs2025_consolidated <- function(csv_path, usgs_targets, mcs_edition) {
  raw <- tryCatch(
    read_csv(csv_path, col_types = cols(.default = "c"), show_col_types = FALSE),
    error = \(e) { warning(glue("Parse failed [{csv_path}]: {e$message}")); NULL }
  )
  if (is.null(raw) || nrow(raw) == 0) return(list(production = NULL, reserves = NULL))

  # Normalize column names — strip internal spaces (e.g. "PROD_EST_ 2024")
  names(raw) <- str_replace_all(names(raw), "\\s+", "_")

  # Filter to target commodities (case-insensitive match against COMMODITY column)
  raw <- raw |>
    filter(map_lgl(COMMODITY, \(c) any(str_detect(str_to_upper(c), usgs_targets))))

  if (nrow(raw) == 0) return(list(production = NULL, reserves = NULL))

  # Production year columns: PROD_2023, PROD_EST_2024 etc.
  year_cols <- names(raw)[str_detect(names(raw), regex("^PROD(_EST)?_20[0-9]{2}$", ignore_case = TRUE))]

  production <- NULL
  if (length(year_cols) > 0) {
    production <- raw |>
      select(country_raw = COUNTRY, COMMODITY, unit = UNIT_MEAS, all_of(year_cols)) |>
      pivot_longer(all_of(year_cols), names_to = "year_raw", values_to = "value_raw") |>
      mutate(
        usgs_commodity = map_chr(COMMODITY, \(c) {
          hit <- usgs_targets[str_detect(str_to_upper(c), usgs_targets)]
          if (length(hit) == 0) NA_character_ else hit[1]
        }),
        year        = as.integer(str_extract(year_raw, "20[0-9]{2}")),
        is_estimate = str_detect(year_raw, regex("EST", ignore_case = TRUE)),
        value_clean = str_remove_all(value_raw %||% "", "[,\\s]"),
        value_clean = na_if(value_clean, "NA"),
        production  = suppressWarnings(as.numeric(value_clean)),
        mcs_edition = as.integer(mcs_edition)
      ) |>
      filter(!is.na(usgs_commodity), !is.na(country_raw), !is.na(year)) |>
      select(country_raw, usgs_commodity, mcs_edition, year, production, unit, is_estimate)
  }

  # Reserves: RESERVES_2024 column — country-level
  res_col <- names(raw)[str_detect(names(raw), regex("^RESERVES?_20[0-9]{2}$", ignore_case = TRUE))][1]
  reserves <- NULL
  if (!is.na(res_col)) {
    res_year <- as.integer(str_extract(res_col, "20[0-9]{2}"))
    reserves <- raw |>
      select(country_raw = COUNTRY, COMMODITY, unit = UNIT_MEAS, reserves_raw = all_of(res_col)) |>
      mutate(
        usgs_commodity = map_chr(COMMODITY, \(c) {
          hit <- usgs_targets[str_detect(str_to_upper(c), usgs_targets)]
          if (length(hit) == 0) NA_character_ else hit[1]
        }),
        reserves    = suppressWarnings(as.numeric(str_remove_all(reserves_raw %||% "", "[,\\s]"))),
        mcs_edition = as.integer(mcs_edition)
      ) |>
      filter(!is.na(usgs_commodity), !is.na(country_raw), !is.na(reserves)) |>
      select(country_raw, usgs_commodity, mcs_edition, reserves, unit)
  }

  list(production = production, reserves = reserves)
}

# -- Parse MCS 2026 consolidated CSV -------------------------------------------
# Format: single long file covering all commodities
# Key columns: Section, Commodity, Country, Statistics, Unit, Year, Value
# World production rows: Section contains "World" and Statistics == "Production"
# World reserves rows:   Section contains "World" and Statistics == "Reserves"

parse_mcs2026_consolidated <- function(csv_path, usgs_targets, mcs_edition) {
  raw <- tryCatch(
    read_csv(csv_path, col_types = cols(.default = "c"), show_col_types = FALSE),
    error = \(e) { warning(glue("Parse failed [{csv_path}]: {e$message}")); NULL }
  )
  if (is.null(raw) || nrow(raw) == 0) return(list(production = NULL, reserves = NULL))

  # Filter to world-level rows (exclude US salient stats)
  world_rows <- raw |>
    filter(str_detect(Section, regex("world", ignore_case = TRUE))) |>
    filter(map_lgl(Commodity, \(c) any(str_detect(str_to_upper(c), usgs_targets)))) |>
    mutate(
      usgs_commodity = map_chr(Commodity, \(c) {
        hit <- usgs_targets[str_detect(str_to_upper(c), usgs_targets)]
        if (length(hit) == 0) NA_character_ else hit[1]
      }),
      year        = suppressWarnings(as.integer(Year)),
      value_clean = str_remove_all(Value %||% "", "[,\\s]"),
      value_num   = suppressWarnings(as.numeric(value_clean)),
      mcs_edition = as.integer(mcs_edition)
    ) |>
    filter(!is.na(usgs_commodity), !is.na(Country), !is.na(year), !is.na(value_num))

  production <- world_rows |>
    filter(str_detect(str_to_lower(Statistics), "^production")) |>
    transmute(
      country_raw    = Country,
      usgs_commodity,
      mcs_edition,
      year,
      production     = value_num,
      unit           = Unit,
      is_estimate    = FALSE   # 2026 format doesn't flag estimates explicitly
    )

  reserves <- world_rows |>
    filter(str_detect(str_to_lower(Statistics), "^reserves?")) |>
    # Keep most recent year per country-commodity (may have multiple years)
    group_by(Country, usgs_commodity) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(
      country_raw    = Country,
      usgs_commodity,
      mcs_edition,
      reserves       = value_num,
      unit           = Unit
    )

  list(
    production = if (nrow(production) > 0) production else NULL,
    reserves   = if (nrow(reserves)   > 0) reserves   else NULL
  )
}


# USGS uses natural-language country names, often differing from ISO standards.
# This covers the most common USGS naming quirks for mineral-producing countries.
# Extend as needed when spot-checking output.
USGS_COUNTRY_MAP <- c(
  "United States"                     = "USA",
  "China"                             = "CHN",
  "Congo (Kinshasa)"                  = "COD",
  "Congo, Democratic Republic of the" = "COD",
  "Russia"                            = "RUS",
  "Australia"                         = "AUS",
  "Chile"                             = "CHL",
  "Peru"                              = "PER",
  "South Africa"                      = "ZAF",
  "Indonesia"                         = "IDN",
  "Philippines"                       = "PHL",
  "Brazil"                            = "BRA",
  "India"                             = "IND",
  "Canada"                            = "CAN",
  "Kazakhstan"                        = "KAZ",
  "Mexico"                            = "MEX",
  "Papua New Guinea"                  = "PNG",
  "Argentina"                         = "ARG",
  "Bolivia"                           = "BOL",
  "Zimbabwe"                          = "ZWE",
  "Zambia"                            = "ZMB",
  "Ghana"                             = "GHA",
  "Turkey"                            = "TUR",
  "Türkiye"                           = "TUR",
  "Korea, Republic of"                = "KOR",
  "Korea, South"                      = "KOR",
  "Iran"                              = "IRN",
  "Myanmar"                           = "MMR",
  "Burma"                             = "MMR",
  "Morocco"                           = "MAR",
  "Portugal"                          = "PRT",
  "Spain"                             = "ESP",
  "Germany"                           = "DEU",
  "United Kingdom"                    = "GBR",
  "Japan"                             = "JPN",
  "France"                            = "FRA",
  "Finland"                           = "FIN",
  "Norway"                            = "NOR",
  "Sweden"                            = "SWE",
  "Mozambique"                        = "MOZ",
  "Madagascar"                        = "MDG",
  "Mali"                              = "MLI",
  "Namibia"                           = "NAM",
  "Tanzania"                          = "TZA",
  "Ethiopia"                          = "ETH",
  "Nigeria"                           = "NGA",
  "Sudan"                             = "SDN",
  "Thailand"                          = "THA",
  "Vietnam"                           = "VNM",
  "Viet Nam"                          = "VNM",
  "Malaysia"                          = "MYS",
  "Laos"                              = "LAO",
  "Pakistan"                          = "PAK",
  "Kyrgyzstan"                        = "KGZ",
  "Tajikistan"                        = "TJK",
  "Uzbekistan"                        = "UZB",
  "Ukraine"                           = "UKR",
  "Serbia"                            = "SRB",
  "Czech Republic"                    = "CZE",
  "Czechia"                           = "CZE",
  "Slovakia"                          = "SVK",
  "Hungary"                           = "HUN",
  "Austria"                           = "AUT",
  "Belgium"                           = "BEL",
  "Netherlands"                       = "NLD",
  "Switzerland"                       = "CHE",
  "Poland"                            = "POL",
  "Romania"                           = "ROU",
  "Bulgaria"                          = "BGR",
  "Greece"                            = "GRC",
  "New Caledonia"                     = NA_character_,  # territory, not sovereign
  "United Arab Emirates"              = "ARE",
  "Saudi Arabia"                      = "SAU",
  "Egypt"                             = "EGY",
  "Algeria"                           = "DZA",
  "Tunisia"                           = "TUN",
  "Senegal"                           = "SEN",
  "Cameroon"                          = "CMR",
  "Gabon"                             = "GAB",
  "Botswana"                          = "BWA",
  "Burkina Faso"                      = "BFA",
  "Côte d'Ivoire"                     = "CIV",
  "Cote d'Ivoire"                     = "CIV",
  "Côte d’Ivoire"             = "CIV",  # right-single-quote variant
  "C\xf4te d\x92Ivoire"              = "CIV",  # windows-1252 mojibake
  "C�TE d�IVOIRE"            = "CIV",  # replacement-char mojibake
  "C�TE d�ivoire"             = "CIV",
  "Colombia"                          = "COL",
  "Venezuela"                         = "VEN",
  "Ecuador"                           = "ECU",
  "Cuba"                              = "CUB",
  "Guatemala"                         = "GTM",
  "Honduras"                          = "HND",
  "Nicaragua"                         = "NIC",
  "Dominican Republic"                = "DOM",
  "Greenland"                         = NA_character_,  # territory, not sovereign
  # MCS 2026 additions
  "New Caledonia (overseas territory of France)" = NA_character_,  # territory, not sovereign
  "Korea, North"                      = "PRK",
  "Rwanda"                            = "RWA",
  "Russia (recoverable)"              = "RUS",  # footnote artifact
  "Russiae"                           = "RUS",  # typo/footnote artifact
  "Mongolia"                          = "MNG",
  "Bahrain"                           = "BHR",
  "Iceland"                           = "ISL",
  "Uganda"                            = "UGA",
  "Sri Lanka"                         = "LKA",
  "Israel"                            = "ISR",
  "Kenya"                             = "KEN",
  "Sierra Leone"                      = "SLE",
  "Other countries"                   = NA_character_,
  "Other"                             = NA_character_
)

# -- Main ingest ---------------------------------------------------------------

message(glue("--- USGS MCS: fetching {length(MCS_YEAR_RANGE)} editions via sbtools ---"))
message(glue("  Targeting {length(usgs_targets)} commodities"))
message("  Excluded (no MCS equivalent): ",
        paste(filter(MINERAL_CROSSWALK, is.na(usgs_name))$comtrade_name, collapse = ", "))

all_production <- list()
all_reserves   <- list()

for (yr in MCS_YEAR_RANGE) {

  edition_item <- find_mcs_edition(yr)
  if (is.null(edition_item)) next

  children_raw <- tryCatch(
    item_list_children(edition_item$id),
    error = \(e) { warning(glue("item_list_children failed [{yr}]: {e$message}")); NULL }
  )
  if (is.null(children_raw) || length(children_raw) == 0) {
    warning(glue("  MCS {yr}: no children found"))
    next
  }

  # Format changed in 2025: per-commodity CSVs → single consolidated file
  fmt <- if (yr <= 2024) "per_commodity" else "consolidated"
  message(glue("  MCS {yr}: format = {fmt}"))

  # -- Per-commodity format (MCS 2022-2024) ----------------------------------
  if (fmt == "per_commodity") {

    children <- tibble(
      item_id = map_chr(children_raw, "id"),
      title   = map_chr(children_raw, "title")
    ) |>
      filter(!str_detect(str_to_lower(title),
                         "salient|industry trend|statistic|overview|appendix|revision")) |>
      mutate(
        matched_commodity = map_chr(title, \(t) {
          hit <- usgs_targets[str_detect(str_to_upper(t), str_to_upper(usgs_targets))]
          if (length(hit) == 0) NA_character_ else hit[1]
        })
      ) |>
      filter(!is.na(matched_commodity))

    message(glue("  MCS {yr}: matched {nrow(children)} commodity items"))

    for (i in seq_len(nrow(children))) {
      row     <- children[i, ]
      mineral <- row$matched_commodity
      iid     <- row$item_id

      files <- tryCatch(
        item_list_files(iid),
        error = \(e) { warning(glue("item_list_files failed [{iid}]: {e$message}")); NULL }
      )
      if (is.null(files) || nrow(files) == 0) next

      csv_files <- files |>
        filter(str_detect(str_to_lower(fname), "\\.csv$")) |>
        filter(!str_detect(str_to_lower(fname), "salient|us-|_us_|usstat|trend|industry"))

      if (nrow(csv_files) == 0) next

      target_file <- csv_files |>
        arrange(desc(str_detect(str_to_lower(fname), "world"))) |>
        slice(1)

      local_path <- download_csv_cached(iid, target_file$fname, target_file$url, RAW_DIR)
      if (is.null(local_path)) next

      parsed <- parse_world_production(local_path, mineral, yr)
      if (!is.null(parsed$production)) all_production[[glue("{yr}_{mineral}")]] <- parsed$production
      if (!is.null(parsed$reserves))   all_reserves[[glue("{yr}_{mineral}")]]   <- parsed$reserves

      Sys.sleep(0.3)
    }

  # -- Consolidated format (MCS 2025+) ---------------------------------------
  } else {

    # Find the child item containing world production/reserves data
    world_item <- keep(children_raw, \(ch) {
      str_detect(str_to_lower(ch$title),
                 "world production|world.*data|commodity.*data|commodity.*salient")
    })

    if (length(world_item) == 0) {
      warning(glue("  MCS {yr}: cannot identify world data child item"))
      next
    }
    world_item <- world_item[[length(world_item)]]  # take last if multiple

    files <- tryCatch(
      item_list_files(world_item$id),
      error = \(e) { warning(glue("item_list_files failed [{world_item$id}]: {e$message}")); NULL }
    )
    if (is.null(files) || nrow(files) == 0) next

    csv_file <- files |>
      filter(str_detect(str_to_lower(fname), "\\.csv$")) |>
      slice(1)

    if (nrow(csv_file) == 0) next

    dest_path <- file.path(RAW_DIR, glue("mcs{yr}_consolidated_{csv_file$fname}"))
    local_path <- download_csv_cached(world_item$id, csv_file$fname, csv_file$url, RAW_DIR)
    if (is.null(local_path)) next

    parsed <- if (yr == 2025) {
      parse_mcs2025_consolidated(local_path, usgs_targets, yr)
    } else {
      parse_mcs2026_consolidated(local_path, usgs_targets, yr)  # 2026+ long format
    }
    if (!is.null(parsed$production)) all_production[[glue("{yr}_consolidated")]] <- parsed$production
    if (!is.null(parsed$reserves))   all_reserves[[glue("{yr}_consolidated")]]   <- parsed$reserves
  }
}

# -- Assemble and clean production table ---------------------------------------

if (length(all_production) == 0) stop("No production data retrieved — check ScienceBase connectivity")

production_raw <- list_rbind(all_production)

# Normalize country_raw before ISO3 lookup:
#   iconv repairs mojibake variants (e.g. garbled Cote d'Ivoire from MCS 2026)
normalize_country <- function(x) iconv(x, from = "latin1", to = "UTF-8", sub = "byte") |> str_trim()

# Apply ISO3 crosswalk; flag unknowns rather than silently dropping
production_clean <- production_raw |>
  mutate(
    country_raw_norm = normalize_country(country_raw),
    iso3c = coalesce(USGS_COUNTRY_MAP[country_raw], USGS_COUNTRY_MAP[country_raw_norm]),
    iso3c_missing = is.na(iso3c) & !str_detect(
      str_to_lower(country_raw),
      "other|total|world|undistrib"
    )
  )

# Report unmapped country names — add to USGS_COUNTRY_MAP as needed
unmapped <- production_clean |>
  filter(iso3c_missing) |>
  distinct(country_raw) |>
  pull(country_raw)

if (length(unmapped) > 0) {
  message("  Unmapped country names (add to USGS_COUNTRY_MAP if needed):")
  walk(unmapped, ~ message(glue("    \"{.x}\" = NA_character_,")))
}

# Join back comtrade commodity names (one USGS commodity may map to multiple)
production_final <- production_clean |>
  filter(!is.na(iso3c)) |>          # drop world totals and unknowns
  left_join(
    MINERAL_CROSSWALK |> filter(!is.na(usgs_name)),
    by = c("usgs_commodity" = "usgs_name"),
    relationship = "many-to-many"
  ) |>
  select(
    iso3c,
    country_raw,                     # original USGS name retained for reference
    usgs_commodity,
    comtrade_commodity = comtrade_name,
    mcs_edition,
    year,
    production,
    unit,
    is_estimate
  ) |>
  mutate(pull_ts = Sys.time()) |>
  # When multiple MCS editions report the same country-commodity-year,
  # prefer the most recent edition (most revised data)
  group_by(iso3c, usgs_commodity, year) |>
  slice_max(mcs_edition, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(usgs_commodity, iso3c, year)

# -- Assemble reserves table ---------------------------------------------------
# MCS 2023+ includes country-level Reserves_t in the CSV.
# Earlier editions return NULL for reserves — those years simply won't appear.

reserves_final <- list_rbind(all_reserves) |>
  mutate(
    iso3c = USGS_COUNTRY_MAP[country_raw],
    iso3c_missing = is.na(iso3c) & !str_detect(
      str_to_lower(country_raw), "other|total|world|undistrib"
    )
  ) |>
  filter(!is.na(iso3c)) |>
  left_join(
    MINERAL_CROSSWALK |> filter(!is.na(usgs_name)) |> distinct(usgs_name, comtrade_name),
    by = c("usgs_commodity" = "usgs_name"),
    relationship = "many-to-many"
  ) |>
  rename(comtrade_commodity = comtrade_name) |>
  # Keep most recent edition's reserves estimate per country-commodity
  group_by(iso3c, usgs_commodity) |>
  slice_max(mcs_edition, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(pull_ts = Sys.time()) |>
  arrange(usgs_commodity, iso3c)

# -- Write to bronze -----------------------------------------------------------


# -- Reconcile USGS iso3c to country spine ------------------------------------
if (!exists("COUNTRY_FRAME")) {
  COUNTRY_FRAME <- dbReadTable(con, DBI::Id(schema = "bronze", table = "country_frame"))
}
valid_iso3c <- COUNTRY_FRAME$iso3c


dropped_prod <- sort(unique(production_final$iso3c[!production_final$iso3c %in% valid_iso3c]))
if (length(dropped_prod) > 0)
  message(sprintf("  [USGS production] %d iso3c(s) not in frame (dropped): %s",
                  length(dropped_prod), paste(dropped_prod, collapse = ", ")))
production_final <- production_final |> filter(iso3c %in% valid_iso3c)

dropped_res <- sort(unique(reserves_final$iso3c[!reserves_final$iso3c %in% valid_iso3c]))
if (length(dropped_res) > 0)
  message(sprintf("  [USGS reserves] %d iso3c(s) not in frame (dropped): %s",
                  length(dropped_res), paste(dropped_res, collapse = ", ")))
reserves_final <- reserves_final |> filter(iso3c %in% valid_iso3c)

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

write_bronze(production_final, "usgs_production", con)
write_bronze(reserves_final,   "usgs_reserves",   con)

# -- Summary -------------------------------------------------------------------

message("--- USGS MCS ingest complete ---")
message(sprintf("  Production: %d rows | %d minerals | %d countries | years %d-%d",
                nrow(production_final),
                n_distinct(production_final$usgs_commodity),
                n_distinct(production_final$iso3c),
                min(production_final$year, na.rm = TRUE),
                max(production_final$year, na.rm = TRUE)))
message(sprintf("  Reserves (country-level, MCS 2023+): %d rows | %d minerals | %d countries",
                nrow(reserves_final),
                n_distinct(reserves_final$usgs_commodity),
                n_distinct(reserves_final$iso3c)))
message(sprintf("  Raw CSVs cached: %s", RAW_DIR))
