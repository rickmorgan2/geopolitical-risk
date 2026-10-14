# ingest_price_series.R
#
# Bronze-layer ingestion of commodity price series from two sources:
#   - World Bank Pink Sheet (Excel, manual download -- see Data Notes below)
#   - IMF Primary Commodity Price System (PCPS) via SDMX 3.0 API
#
# Also fetches US CPI (IMF IFS) for price deflation.
#
# After ingestion, enriches the Comtrade parquet in-place with price_wb,
# qty_wb, global_value_wb, and global_value_wb_real.
#
# -- Price source routing ------------------------------------------------------
#   price_source in crosswalk determines which source feeds each cmd_code:
#     "WB"  -> Pink Sheet Excel column header matched via price_name
#     "IMF" -> PCPS SDMX commodity code in price_name (e.g. PORK for swine)
#
#   average_price == 1: price_name follows "Average: Series A; Series B" format.
#   The individual component series are extracted and their prices averaged.
#
#   Coffee (cmd_code 0901) is a special case: price_name is a routing note,
#   not a series name. Both "Coffee, Arabica" and "Coffee, Robusta" are fetched
#   from the Pink Sheet. The price assigned to each trade record is determined
#   by the exporting country's assumed production type (see ARABICA_ISO3 /
#   ROBUSTA_ISO3 below). Countries not in either list receive the simple average
#   of the two varieties. This is documented in indicator_reference.md.
#
# -- Data notes (manual sources) -----------------------------------------------
#   World Bank Pink Sheet
#     File:     CMO-Historical-Data-Monthly.xlsx
#     Source:   https://www.worldbank.org/en/research/commodity-markets
#     Updated:  Monthly, typically the first week of the following month.
#               Current file covers through 2026M02.
#     Action:   Download manually and replace the file at PINK_SHEET_XLSX.
#               No pipeline code changes required unless sheet structure changes.
#
#   comtrade_pinksheet_crosswalk.csv
#     Source:   Maintained manually in the repository.
#     Updated:  As needed when commodity coverage changes (new HS codes,
#               source switches, or in_data status changes). No fixed schedule.
#               Wood products (in_data == FALSE) are excluded from all pipeline
#               runs; HS codes for timber change too frequently for reliable
#               tracking. The swine price (IMF PCPS PORK series) is the only
#               IMF-sourced commodity in the active crosswalk as of 2026.
#
# -- Bronze output tables -------------------------------------------------------
#   bronze.us_cpi_monthly      yr, mon, cpi_idx_2024
#   bronze.pink_sheet_prices   yr, mon, series_name, price_wb, unit
#   bronze.imf_pcps_prices     yr, mon, pcps_code, price_usd
#   bronze.price_lookup        cmd_code, yr, mon, price_wb, price_unit
#
# -- Parquet columns added/replaced --------------------------------------------
#   price_wb, price_unit, commodity_name_price, qty_source,
#   qty_wb, global_value_wb, global_value_wb_real
#
# -- Pipeline position ---------------------------------------------------------
#   After:  ingest_comtrade_build.R
#   Before: ingest_comtrade.R
#
# Expects: con, PINK_SHEET_XLSX, CROSSWALK_CSV, COMTRADE_PARQUET, START_YR

library(DBI)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(purrr)
library(httr)
library(rsdmx)
library(arrow)

if (!exists("con"))              stop("No DB connection. Run via run_pipeline.R")
if (!exists("PINK_SHEET_XLSX"))  stop("PINK_SHEET_XLSX not set in config.R")
if (!exists("CROSSWALK_CSV"))    stop("CROSSWALK_CSV not set in config.R")
if (!exists("COMTRADE_PARQUET")) stop("COMTRADE_PARQUET not set in config.R")
if (!exists("START_YR"))         stop("START_YR not set in config.R")

# -- Helpers -------------------------------------------------------------------

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

# Normalize strings for matching between Excel headers and crosswalk CSV.
# Strips non-breaking spaces, smart quotes, en/em dashes, and other
# non-ASCII characters, then collapses runs of whitespace and trims ends.
clean_str <- function(x) {
  x |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "") |>  # transliterate; drop unconvertible chars rather than returning NA
    str_replace_all("\u00a0",  " ") |> # non-breaking space -> space
    str_replace_all("\u2013",  "-") |> # en dash -> hyphen
    str_replace_all("\u2014",  "-") |> # em dash -> hyphen
    str_replace_all("[\u2018\u2019]", "'") |>  # smart single quotes -> apostrophe
    str_replace_all("[\u201c\u201d]", '"') |>  # smart double quotes -> straight
    str_replace_all("[^[:print:]]", "") |>     # remove remaining non-printable
    str_replace_all("\\*", "") |>             # remove asterisks
    str_squish()                               # collapse whitespace, trim
}

# Parse "Average: Series A; Series B; Series C" -> c("Series A", "Series B", "Series C")
# Returns the input string unchanged for non-average price_name values.
parse_avg_series <- function(price_name) {
  if (!str_starts(price_name, "Average:")) return(price_name)
  str_remove(price_name, "^Average:\\s*") |>
    str_split(";\\s*") |>
    pluck(1) |>
    str_trim()
}

# -- 1. US CPI (IMF IFS): bronze.us_cpi_monthly --------------------------------

message("--- Price series: fetching US CPI (IMF IFS) ---")

imf_cpi_url <- paste0(
  "https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.STA/CPI/%2B/",
  "USA.CPI._T.IX.M?dimensionAtObservation=TIME_PERIOD&attributes=dsd",
  "&measures=all&includeHistory=false"
)

us_cpi_raw <- rsdmx::readSDMX(imf_cpi_url) |>
  as.data.frame() |>
  select(period = TIME_PERIOD, cpi_raw = OBS_VALUE) |>
  mutate(
    yr      = as.integer(str_extract(period, "^[0-9]{4}")),
    mon     = as.integer(str_extract(period, "(?<=M)[0-9]{2}")),
    cpi_raw = as.numeric(cpi_raw)
  ) |>
  filter(yr >= START_YR) |>
  select(yr, mon, cpi_raw)

base_cpi <- us_cpi_raw |>
  filter(yr == 2024) |>
  pull(cpi_raw) |>
  mean(na.rm = TRUE)

us_cpi <- us_cpi_raw |>
  mutate(cpi_idx_2024 = cpi_raw / base_cpi * 100) |>  # 2024 avg = 100
  select(yr, mon, cpi_idx_2024)

message(sprintf("  CPI: %d months (%d-%d)", nrow(us_cpi),
                min(us_cpi$yr), max(us_cpi$yr)))
write_bronze(us_cpi, "us_cpi_monthly", con)

# -- 2. Load crosswalk ---------------------------------------------------------

message("--- Price series: loading crosswalk ---")

xwalk <- read.csv(CROSSWALK_CSV) |>
  filter(in_data == TRUE | in_data == "TRUE") |>
  mutate(
    cmd_code      = as.character(cmd_code),
    cmd_code      = ifelse(nchar(cmd_code) %% 2 == 1, paste0("0", cmd_code), cmd_code),
    average_price = suppressWarnings(as.integer(average_price))
  )

xwalk_wb  <- xwalk |> filter(price_source == "WB")
xwalk_imf <- xwalk |> filter(price_source == "IMF")

message(sprintf("  Crosswalk: %d WB cmd_codes | %d IMF cmd_codes",
                n_distinct(xwalk_wb$cmd_code), n_distinct(xwalk_imf$cmd_code)))

# Build series-to-cmd_code lookup for WB joins:
#   - Direct:   price_name is the exact Pink Sheet column header
#   - Average:  "Average: X; Y" expanded to individual component series names
#   - Coffee:   both varieties always fetched; country routing applied in step 6
series_lookup <- bind_rows(
  # Direct (non-average, non-coffee)
  xwalk_wb |>
    filter(commodity_name != "coffee",
           is.na(average_price) | average_price != 1) |>
    transmute(cmd_code, commodity_name,
              series_name = price_name,
              price_unit  = qty_unit_price),

  # Average: expand "Average: X; Y" to one row per component
  xwalk_wb |>
    filter(commodity_name != "coffee", average_price == 1) |>
    rowwise() |>
    mutate(series_name = list(parse_avg_series(price_name))) |>
    unnest_longer(series_name) |>
    transmute(cmd_code, commodity_name,
              series_name,
              price_unit = qty_unit_price),

  # Coffee: inject both varieties; price_name routing note is ignored here
  xwalk_wb |>
    filter(commodity_name == "coffee") |>
    distinct(cmd_code, commodity_name, price_unit = qty_unit_price) |>
    cross_join(tibble(series_name = c("Coffee, Arabica", "Coffee, Robusta")))

) |>
  distinct(cmd_code, commodity_name, series_name, price_unit) |>
  mutate(series_name = clean_str(series_name))  # normalize to match Excel headers

# -- 3. Read WB Pink Sheet: bronze.pink_sheet_prices ---------------------------

message("--- Price series: reading WB Pink Sheet Excel ---")

if (!file.exists(PINK_SHEET_XLSX))
  stop("Pink Sheet file not found: ", PINK_SHEET_XLSX)

header_names <- suppressMessages(
  read_excel(PINK_SHEET_XLSX, sheet = "Monthly Prices",
             skip = 4, n_max = 1, col_names = FALSE)
) |> unlist(use.names = FALSE)

header_units <- suppressMessages(
  read_excel(PINK_SHEET_XLSX, sheet = "Monthly Prices",
             skip = 5, n_max = 1, col_names = FALSE)
) |> unlist(use.names = FALSE) |>
  str_remove_all("[()]") |>
  clean_str()

# Normalize headers to plain ASCII before any matching or joins
header_names <- clean_str(header_names)

raw <- suppressMessages(
  read_excel(PINK_SHEET_XLSX, sheet = "Monthly Prices",
             skip = 6, col_names = FALSE,
             na = c("", "...", "\u2026"))
)

# Align names by position: col 1 = period string, cols 2:N = commodity series.
# Using seq_len(ncol-1) avoids off-by-one errors from NA trimming in header row.
n_commodity_cols <- ncol(raw) - 1
names(raw)       <- c("period", header_names[seq_len(n_commodity_cols)])

unit_lookup <- tibble(
  series_name = header_names,
  unit        = header_units
) |> filter(!is.na(series_name))

pink_sheet_prices <- raw |>
  filter(!is.na(period), str_detect(period, "^[0-9]{4}M")) |>
  mutate(
    yr  = as.integer(str_extract(period, "^[0-9]{4}")),
    mon = as.integer(str_extract(period, "(?<=M)[0-9]{2}"))
  ) |>
  select(-period) |>
  pivot_longer(cols = -c(yr, mon),
               names_to  = "series_name",
               values_to = "price_wb") |>
  filter(!is.na(price_wb), is.finite(price_wb)) |>
  left_join(unit_lookup, by = "series_name") |>
  semi_join(series_lookup, by = "series_name") |>  # retain crosswalk series only
  filter(yr >= START_YR) |>
  arrange(series_name, yr, mon) |>
  select(yr, mon, series_name, price_wb, unit)

message(sprintf("  Pink Sheet: %s rows | %d series | %d-%d",
                format(nrow(pink_sheet_prices), big.mark = ","),
                n_distinct(pink_sheet_prices$series_name),
                min(pink_sheet_prices$yr), max(pink_sheet_prices$yr)))
write_bronze(pink_sheet_prices, "pink_sheet_prices", con)

# Validation: warn on crosswalk series not found in Pink Sheet headers
missing_series <- series_lookup |>
  filter(commodity_name != "coffee") |>
  anti_join(pink_sheet_prices, by = "series_name") |>
  distinct(series_name)

if (nrow(missing_series) > 0)
  warning(sprintf(
    "[CHECK] %d crosswalk series not found in Pink Sheet headers:\n  %s",
    nrow(missing_series),
    paste(missing_series$series_name, collapse = "\n  ")
  ))

# -- 4. Fetch IMF PCPS prices via FRED: bronze.imf_pcps_prices -----------------
#
# The IMF PCPS SDMX API is unreliable (endpoint changes, column name shifts).
# FRED redistributes the same IMF benchmark prices as plain CSV with no auth.
# Map crosswalk price_name codes -> FRED series IDs here; add rows as needed.
# Units from FRED match original PCPS units (US cents/lb for swine).
# Divide by 100 to convert cents -> USD, consistent with prior PCPS parsing.

message("--- Price series: fetching IMF PCPS prices via FRED ---")

# price_name in crosswalk -> FRED series ID
FRED_PCPS_MAP <- c(
  PPORK = "PPORKUSDM"   # swine, 51-52% lean hogs, US cents/lb
)

pcps_codes <- xwalk_imf |>
  distinct(price_name) |>
  filter(price_name %in% names(FRED_PCPS_MAP)) |>
  pull(price_name)

bad_imf <- xwalk_imf |>
  filter(!price_name %in% names(FRED_PCPS_MAP)) |>
  distinct(cmd_code, price_name)

if (nrow(bad_imf) > 0)
  warning(sprintf(
    "[CHECK] %d IMF cmd_codes have no FRED mapping (add to FRED_PCPS_MAP):\n  %s",
    nrow(bad_imf),
    paste(bad_imf$cmd_code, bad_imf$price_name, sep = " -> ", collapse = "\n  ")
  ))

if (length(pcps_codes) == 0) stop("No FRED-mapped IMF series found in crosswalk.")

imf_pcps_raw <- map(pcps_codes, \(code) {
  fred_id <- FRED_PCPS_MAP[[code]]
  resp    <- tryCatch(
    httr::GET(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", fred_id),
              httr::timeout(60)),
    error = function(e) stop("FRED fetch failed [", fred_id, "]: ", e$message)
  )
  httr::stop_for_status(resp)
  read.csv(text = httr::content(resp, as = "text", encoding = "UTF-8"),
           stringsAsFactors = FALSE) |>
    rename(period = observation_date, value = all_of(fred_id)) |>
    mutate(
      pcps_code = code,
      yr        = as.integer(str_extract(period, "^[0-9]{4}")),
      mon       = as.integer(str_extract(period, "(?<=-)[0-9]{2}(?=-)")),
      value     = as.numeric(value) / 100   # cents -> USD
    ) |>
    filter(yr >= START_YR, !is.na(value)) |>
    select(yr, mon, pcps_code, value)
}) |> list_rbind()

message(sprintf("  PCPS (FRED): %s rows | %d series | %d-%d",
                format(nrow(imf_pcps_raw), big.mark = ","),
                n_distinct(imf_pcps_raw$pcps_code),
                min(imf_pcps_raw$yr), max(imf_pcps_raw$yr)))
write_bronze(imf_pcps_raw |> rename(price_usd = value),
             "imf_pcps_prices", con)

# -- 5. Build unified price lookup: bronze.price_lookup ------------------------
#
# One row per (cmd_code, yr, mon).
# Average-series commodities are averaged across their component series here.
# Coffee is kept variety-level for routing; a pre-computed average is also
# retained for countries not assigned to either variety.

# WB non-coffee: join Pink Sheet on series_name, then average components
wb_prices <- pink_sheet_prices |>
  inner_join(
    series_lookup |> filter(commodity_name != "coffee"),
    by = "series_name",
    relationship = "many-to-many"  # multiple cmd_codes share the same series name
  ) |>
  group_by(cmd_code, yr, mon, price_unit) |>
  summarise(price_wb = mean(price_wb, na.rm = TRUE), .groups = "drop")

# Coffee: keep both varieties wide for per-record routing in step 6
coffee_cmd <- series_lookup |>
  filter(commodity_name == "coffee") |>
  distinct(cmd_code) |>
  pull()

coffee_wide <- pink_sheet_prices |>
  filter(series_name %in% c("Coffee, Arabica", "Coffee, Robusta")) |>
  mutate(variety = if_else(str_detect(series_name, "Arabica"), "arabica", "robusta")) |>
  select(yr, mon, variety, price_wb) |>
  pivot_wider(names_from = variety, values_from = price_wb,
              names_prefix = "price_") |>
  mutate(price_avg_coffee = (price_arabica + price_robusta) / 2)

# IMF PCPS (swine): join PCPS results to cmd_codes via price_name = SDMX code
imf_prices <- xwalk_imf |>
  filter(price_name %in% pcps_codes) |>
  distinct(cmd_code, price_name, price_unit = qty_unit_price) |>
  inner_join(imf_pcps_raw, by = c("price_name" = "pcps_code"),
             relationship = "many-to-many") |>  # multiple cmd_codes per PCPS series
  group_by(cmd_code, yr, mon, price_unit) |>
  summarise(price_wb = mean(value, na.rm = TRUE), .groups = "drop")

# Unified lookup (excludes coffee, which is routed per-record in step 8)
price_lookup <- bind_rows(wb_prices, imf_prices) |>
  arrange(cmd_code, yr, mon)

write_bronze(price_lookup, "price_lookup", con)
message(sprintf("  Price lookup: %s rows | %d cmd_codes",
                format(nrow(price_lookup), big.mark = ","),
                n_distinct(price_lookup$cmd_code)))

# Validation: all active cmd_codes have price coverage
no_price <- xwalk |>
  anti_join(price_lookup, by = "cmd_code") |>
  filter(commodity_name != "coffee") |>
  distinct(cmd_code, commodity_name, price_source)

if (nrow(no_price) > 0)
  warning(sprintf("[CHECK] %d cmd_codes have no price coverage:\n  %s",
                  nrow(no_price),
                  paste(no_price$cmd_code, no_price$commodity_name,
                        sep = " (", collapse = ")\n  ")))

# -- 6. Coffee country routing constants ---------------------------------------
#
# Arabica: highland varieties; dominant in Ethiopia, East Africa, Latin America.
# Robusta: lowland, higher yield; dominant in Vietnam, West/Central Africa, SE Asia.
# Countries not in either list receive the simple mean of arabica and robusta prices.
# Routing is based on ICO production data and commodity trade literature.
# Update these lists if production geography shifts materially.

ARABICA_ISO3 <- c(
  "BRA", "COL", "ETH", "HND", "MEX", "GTM", "PER", "CRI", "PAN", "NIC",
  "KEN", "TZA", "RWA", "BDI", "SLV", "BOL", "ECU", "JAM", "PNG"
)

ROBUSTA_ISO3 <- c(
  "VNM", "CIV", "IDN", "CMR", "TGO", "GHA", "UGA", "COG", "GAB", "LAO",
  "KHM", "PHL", "THA", "MDG"
)

# -- 7. Unit conversion table --------------------------------------------------
#
# (qty_unit_code from Comtrade, price_unit from crosswalk) -> conv_factor
# global_value_wb = (qty_comtrade * conv_factor) * price_wb_usd
# NA conv_factor -> NA global_value_wb; no silent wrong values.

unit_conv <- tribble(
  ~qty_unit_code, ~price_unit,    ~conv_factor,
  8L,             "kg",           1.0,          # kg -> kg
  15L,            "kg",           0.001,        # g -> kg
  21L,            "kg",           1000.0,       # 1000 kg -> kg
  8L,             "mt",           0.001,        # kg -> mt
  15L,            "mt",           1e-6,         # g -> mt
  21L,            "mt",           1.0,          # 1000 kg -> mt
  8L,             "t",            0.001,        # kg -> t (alias for mt)
  15L,            "t",            1e-6,         # g -> t
  21L,            "t",            1.0,          # 1000 kg -> t
  21L,            "dmtu",         1.0,          # 1000 kg -> dmtu (~1 mt dry)
  8L,             "dmtu",         0.001,        # kg -> dmtu
  8L,             "troy oz",      32.1507,      # kg -> troy oz
  15L,            "troy oz",      0.0321507,    # g -> troy oz
  21L,            "troy oz",      32150.7,      # 1000 kg -> troy oz
  19L,            "bbl",          1.0,          # bbl -> bbl
  8L,             "bbl",          0.00733,      # kg -> bbl (crude ~136.4 kg/bbl)
  21L,            "bbl",          7.33,         # 1000 kg -> bbl
  12L,            "bbl",          6.2898,       # m3 -> bbl (petroleum)
  17L,            "mmbtu",        35.315,       # 1000 m3 -> mmbtu
  12L,            "mmbtu",        0.035315,     # m3 -> mmbtu (natural gas; not LNG liquid)
  18L,            "mmbtu",        947.817,      # TJ -> mmbtu
  20L,            "mmbtu",        0.0215,       # 1000 L LNG -> mmbtu
  8L,             "mmbtu",        0.04607,      # kg LNG -> mmbtu
  8L,             "lbs",          2.20462       # kg -> lbs (swine PPORK series)
) |> mutate(qty_unit_code = as.integer(qty_unit_code))

# -- 8. Enrich Comtrade parquet ------------------------------------------------

message("--- Price series: enriching Comtrade parquet ---")

price_lookup_arrow <- arrow::as_arrow_table(
  price_lookup |> mutate(yr = as.integer(yr), mon = as.integer(mon))
)
cpi_arrow <- arrow::as_arrow_table(
  us_cpi |> mutate(yr = as.integer(yr), mon = as.integer(mon))
)

enriched_cols <- c("price_wb", "price_unit", "commodity_name_price",
                   "qty_source", "qty_wb", "global_value_wb",
                   "global_value_wb_real", "cpi_idx_2024", "conv_factor")

rows_before <- arrow::open_dataset(COMTRADE_PARQUET) |>
  summarise(n = n()) |>
  collect() |>
  pull(n)

enriched <- arrow::open_dataset(COMTRADE_PARQUET) |>
  mutate(yr = as.integer(yr), mon = as.integer(mon)) |>
  select(-any_of(enriched_cols)) |>
  left_join(price_lookup_arrow, by = c("cmd_code", "yr", "mon")) |>
  left_join(cpi_arrow,          by = c("yr", "mon")) |>
  collect() |>
  # Coffee routing: join wide prices then assign variety by exporting country
  left_join(coffee_wide, by = c("yr", "mon")) |>
  mutate(
    price_wb = case_when(
      cmd_code == coffee_cmd & iso3c %in% ARABICA_ISO3 ~ price_arabica,
      cmd_code == coffee_cmd & iso3c %in% ROBUSTA_ISO3 ~ price_robusta,
      cmd_code == coffee_cmd                           ~ price_avg_coffee,
      TRUE                                             ~ price_wb
    ),
    price_unit = if_else(cmd_code == coffee_cmd & is.na(price_unit), "kg", price_unit)
  ) |>
  select(-price_arabica, -price_robusta, -price_avg_coffee) |>
  # Qty resolution: fall back to net_wgt when primary qty unit unavailable
  mutate(
    qty_for_conv   = if_else(qty_unit_code == -1L & !is.na(net_wgt_comtrade),
                             net_wgt_comtrade, qty_comtrade),
    qty_unit_final = as.integer(if_else(
      qty_unit_code == -1L & !is.na(net_wgt_comtrade), 8L, qty_unit_code)),
    qty_source = case_when(
      qty_unit_code != -1L & !is.na(qty_comtrade)     ~ "primary",
      qty_unit_code == -1L & !is.na(net_wgt_comtrade) ~ "net_wgt_fallback",
      TRUE                                             ~ "none"
    )
  ) |>
  left_join(unit_conv |> rename(qty_unit_final = qty_unit_code),
            by = c("qty_unit_final", "price_unit")) |>
  mutate(
    qty_wb               = qty_for_conv * conv_factor,
    price_wb_usd         = price_wb,  # all prices in USD; cents/sheet removed (plywood excluded)
    global_value_wb      = qty_wb * price_wb_usd,
    global_value_wb_real = global_value_wb / cpi_idx_2024 * 100  # 2024 USD
  ) |>
  select(-conv_factor, -price_wb_usd, -qty_for_conv, -qty_unit_final, -cpi_idx_2024)

# -- 9. Write and validate -----------------------------------------------------

tryCatch(dbExecute(con, "DROP VIEW IF EXISTS existing_data"), error = function(e) NULL)
gc()

arrow::write_dataset(
  enriched,
  COMTRADE_PARQUET,
  partitioning           = c("iso3c", "yr"),
  format                 = "parquet",
  compression            = "zstd",
  existing_data_behavior = "delete_matching"
)

# Row count invariant
rows_after <- arrow::open_dataset(COMTRADE_PARQUET) |>
  summarise(n = n()) |>
  collect() |>
  pull(n)

if (rows_before != rows_after)
  warning(sprintf("[CHECK] Row count changed after enrichment: %d -> %d",
                  rows_before, rows_after))

# Coverage spot check: USA 2024
check_enrich <- arrow::open_dataset(
  file.path(COMTRADE_PARQUET, "iso3c=USA", "yr=2024")
) |> collect()

message(sprintf(
  "  Enrichment -- USA 2024: price_wb %.1f%% | qty_wb %.1f%% | global_value_wb %.1f%%",
  mean(!is.na(check_enrich$price_wb))        * 100,
  mean(!is.na(check_enrich$qty_wb))          * 100,
  mean(!is.na(check_enrich$global_value_wb)) * 100
))

# Flag commodities with low qty_wb coverage
check_enrich |>
  filter(!is.na(price_wb)) |>
  group_by(commodity_name) |>
  summarise(pct_qty_wb = round(mean(!is.na(qty_wb)) * 100, 1), .groups = "drop") |>
  filter(pct_qty_wb < 50) |>
  (\(x) {
    if (nrow(x) > 0)
      warning("[CHECK] Low qty_wb coverage (<50%): ",
              paste(x$commodity_name, x$pct_qty_wb, sep = " ", collapse = "%, "), "%")
  })()

# Coffee routing spot check: BRA (arabica) vs VNM (robusta) must differ
coffee_check <- enriched |>
  filter(cmd_code == coffee_cmd, yr == 2024, mon == 6,
         iso3c %in% c("BRA", "VNM")) |>
  distinct(iso3c, price_wb)

if (nrow(coffee_check) == 2 &&
    coffee_check$price_wb[coffee_check$iso3c == "BRA"] ==
    coffee_check$price_wb[coffee_check$iso3c == "VNM"])
  warning("[CHECK] Coffee routing failed: BRA and VNM have identical prices in Jun 2024")

message("--- Price series: complete ---")
