# ingest_price_series.R
#
# Commodity prices and the trade-value deflator, then the Comtrade parquet
# enriched with them.
#
# -- Prices, in tiers (D-029) --------------------------------------------------
#   1. World Bank Pink Sheet (monthly Excel file; price_source "WB")
#   2. IMF primary commodity prices via FRED (price_source "IMF")
#   3. Comtrade world unit values, built from our own data (price_source "UV"):
#      the median value per unit across reporters, estimated quantities left out
#   Codes with price_source "none" (broad headings, manufactures, arms) are
#   counted for exposure only and get no price.
#   A series the Pink Sheet stopped publishing is continued by the crosswalk's
#   price_fallback ("FRED:<code>" or "UV"), scaled to it over their overlap.
#
#   average_price == 1: price_name is "Average: Series A; Series B" and the
#   component prices are averaged. Coffee (0901) is priced by the exporting
#   country's variety (ARABICA_ISO3 / ROBUSTA_ISO3 below); others get the mean.
#
# -- Deflator (D-025) ----------------------------------------------------------
#   World Bank MUV index from the annual Pink Sheet file, monthly, 2024 = 100
#   (R/deflator.R). constant_value_2024 = primary_value / muv_idx_2024 * 100.
#
# -- Data notes (manual sources) -----------------------------------------------
#   Pink Sheet monthly and annual files: download_pink_sheet() in R/pink_sheet.R
#   fetches both and keeps the previous monthly file as
#   CMO-Historical-Data-Monthly_<date>.xlsx. Older full-precision files are
#   spliced in where the current file only rounded a value.
#   comtrade_price_crosswalk.csv: maintained in the repository. Plywood
#   (4408, 4412) stays excluded (in_data FALSE).
#
# -- Bronze output tables -------------------------------------------------------
#   bronze.muv_monthly           yr, mon, muv_idx_2024, deflator_provisional
#   bronze.pink_sheet_prices     yr, mon, series_name, price_wb, unit, precision
#   bronze.imf_pcps_prices       yr, mon, pcps_code, price_usd
#   bronze.comtrade_unit_values  cmd_code, yr, mon, uv_unit_code, uv_price, uv_n
#   bronze.price_lookup          cmd_code, yr, mon, price_wb, price_unit, price_origin
#
# -- Parquet columns added/replaced --------------------------------------------
#   price_wb, price_unit, price_origin, qty_source, qty_wb, global_value_wb,
#   global_value_wb_real, muv_idx_2024, deflator_provisional, constant_value_2024
#   The enriched dataset is written to a staging folder, checked (same rows,
#   same partitions) and only then swapped in place of the old one.
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
library(arrow)

source("R/deflator.R")
source("R/pink_sheet.R")
source("R/price_helpers.R")

if (!exists("con"))              stop("No DB connection. Run via run_pipeline.R")
if (!exists("PINK_SHEET_XLSX"))  stop("PINK_SHEET_XLSX not set in config.R")
if (!exists("CROSSWALK_CSV"))    stop("CROSSWALK_CSV not set in config.R")
if (!exists("COMTRADE_PARQUET")) stop("COMTRADE_PARQUET not set in config.R")
if (!exists("START_YR"))         stop("START_YR not set in config.R")
if (!exists("PINK_SHEET_ANNUAL_XLSX"))
  PINK_SHEET_ANNUAL_XLSX <- file.path(dirname(PINK_SHEET_XLSX), "CMO-Historical-Data-Annual.xlsx")
if (!exists("FRED_CACHE_DIR")) FRED_CACHE_DIR <- "data/raw_fred"

PRICE_FIRST_YR <- 2010L   # prices and the deflator cover every year in the parquet

# -- Helpers -------------------------------------------------------------------

write_bronze <- function(df, table_name, con) {
  df <- mutate(df, pull_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS bronze")
  dbWriteTable(con, DBI::Id(schema = "bronze", table = table_name), df, overwrite = TRUE)
  message(sprintf("  [bronze.%s] %d rows written", table_name, nrow(df)))
}

# Parse "Average: Series A; Series B; Series C" -> c("Series A", "Series B", "Series C")
parse_avg_series <- function(price_name) {
  if (!str_starts(price_name, "Average:")) return(price_name)
  str_remove(price_name, "^Average:\\s*") |> str_split(";\\s*") |> pluck(1) |> str_trim()
}

pad_code <- function(x) { x <- as.character(x); ifelse(nchar(x) %% 2 == 1, paste0("0", x), x) }

# -- 1. Deflator: MUV (D-025) --------------------------------------------------

message("--- Price series: MUV deflator ---")
muv_annual <- load_muv_annual(PINK_SHEET_ANNUAL_XLSX)
muv <- muv_monthly(muv_annual, PRICE_FIRST_YR:as.integer(format(Sys.Date(), "%Y")))
message(sprintf("  MUV annual %d-%d; monthly from %d; provisional after %d-06",
                min(muv_annual$year), max(muv_annual$year), PRICE_FIRST_YR, max(muv_annual$year)))
write_bronze(muv, "muv_monthly", con)
dbExecute(con, "DROP TABLE IF EXISTS bronze.us_cpi_monthly")   # replaced by bronze.muv_monthly

# -- 2. Crosswalk --------------------------------------------------------------

message("--- Price series: loading crosswalk ---")
xwalk <- read.csv(CROSSWALK_CSV, colClasses = "character") |>
  filter(in_data == "TRUE") |>
  mutate(cmd_code = pad_code(cmd_code), average_price = suppressWarnings(as.integer(average_price)))
if (!"price_fallback" %in% names(xwalk)) xwalk$price_fallback <- ""

xwalk_wb  <- xwalk |> filter(price_source == "WB")
xwalk_imf <- xwalk |> filter(price_source == "IMF")
uv_codes  <- xwalk$cmd_code[xwalk$price_source == "UV" | xwalk$price_fallback == "UV"]
message(sprintf("  Crosswalk: %d WB | %d IMF | %d UV | %d exposure only",
                nrow(xwalk_wb), nrow(xwalk_imf), sum(xwalk$price_source == "UV"),
                sum(xwalk$price_source == "none")))

series_lookup <- bind_rows(
  xwalk_wb |>
    filter(commodity_name != "coffee", is.na(average_price) | average_price != 1) |>
    transmute(cmd_code, commodity_name, series_name = price_name, price_unit = qty_unit_price),
  xwalk_wb |>
    filter(commodity_name != "coffee", average_price == 1) |>
    rowwise() |> mutate(series_name = list(parse_avg_series(price_name))) |>
    unnest_longer(series_name) |>
    transmute(cmd_code, commodity_name, series_name, price_unit = qty_unit_price),
  xwalk_wb |>
    filter(commodity_name == "coffee") |>
    distinct(cmd_code, commodity_name, price_unit = qty_unit_price) |>
    cross_join(tibble(series_name = c("Coffee, Arabica", "Coffee, Robusta")))
) |>
  distinct(cmd_code, commodity_name, series_name, price_unit) |>
  mutate(series_name = clean_str(series_name))

# -- 3. Pink Sheet, with full precision spliced in ------------------------------

message("--- Price series: reading WB Pink Sheet Excel ---")
ps       <- read_pink_sheet(PINK_SHEET_XLSX)
archives <- sort(list.files(dirname(PINK_SHEET_XLSX), full.names = TRUE,
                            pattern = "^CMO-Historical-Data-Monthly_.*\\.xlsx$"), decreasing = TRUE)
spliced  <- splice_pink_sheet(ps$prices, map(archives, \(f) read_pink_sheet(f)$prices))

pink_sheet_prices <- spliced |>
  left_join(ps$units, by = "series_name") |>
  semi_join(series_lookup, by = "series_name") |>
  filter(yr >= PRICE_FIRST_YR) |>
  arrange(series_name, yr, mon) |>
  select(yr, mon, series_name, price_wb, unit, precision)

message(sprintf("  Pink Sheet: %s rows | %d series | %d-%d | through %s | %s cells from archived full precision",
                format(nrow(pink_sheet_prices), big.mark = ","),
                n_distinct(pink_sheet_prices$series_name),
                min(pink_sheet_prices$yr), max(pink_sheet_prices$yr),
                with(filter(pink_sheet_prices, yr == max(yr)), sprintf("%d-%02d", max(yr), max(mon))),
                format(sum(pink_sheet_prices$precision != "current file"), big.mark = ",")))
write_bronze(pink_sheet_prices, "pink_sheet_prices", con)

missing_series <- series_lookup |>
  filter(commodity_name != "coffee") |>
  anti_join(pink_sheet_prices, by = "series_name") |>
  distinct(series_name)
if (nrow(missing_series) > 0)
  warning(sprintf("[CHECK] %d crosswalk series not found in Pink Sheet headers:\n  %s",
                  nrow(missing_series), paste(missing_series$series_name, collapse = "\n  ")))

# -- 4. IMF primary commodity prices via FRED -------------------------------------

message("--- Price series: IMF prices via FRED ---")
# price_name in the crosswalk -> FRED series, divisor to US$, unit (check the
# unit against the FRED title when adding a series)
FRED_SERIES <- tribble(
  ~price_name, ~fred_id,      ~divisor, ~unit,
  "PPORK",     "PPORKUSDM",   100,      "lbs",   # swine, US cents per pound
  "PURAN",     "PURANUSDM",   1,        "lbs",   # uranium, US$ per pound
  "POLVOIL",   "POLVOILUSDM", 1,        "mt",    # olive oil, US$ per metric ton
  "PWOOLC",    "PWOOLCUSDM",  100,      "kg",    # wool, coarse, US cents per kg
  "PWOOLF",    "PWOOLFUSDM",  100,      "kg",    # wool, fine, US cents per kg
  "PHIDE",     "PHIDEUSDM",   100,      "lbs",   # hides, US cents per pound
  "PBARL",     "PBARLUSDM",   1,        "mt"     # barley, US$ per metric ton (fallback)
)
imf_names <- unique(c(unlist(str_split(xwalk_imf$price_name, ";\\s*")),
                      sub("^FRED:", "", xwalk$price_fallback[startsWith(xwalk$price_fallback, "FRED:")])))
unknown <- setdiff(imf_names, FRED_SERIES$price_name)
if (length(unknown) > 0)
  warning("[CHECK] IMF price names with no FRED mapping: ", paste(unknown, collapse = ", "))

imf_pcps_raw <- FRED_SERIES |>
  filter(price_name %in% imf_names) |>
  pmap(\(price_name, fred_id, divisor, unit)
    fetch_fred_series(fred_id, divisor, FRED_CACHE_DIR) |> mutate(pcps_code = price_name)) |>
  list_rbind() |>
  filter(yr >= PRICE_FIRST_YR)
message(sprintf("  FRED: %s rows | %d series", format(nrow(imf_pcps_raw), big.mark = ","),
                n_distinct(imf_pcps_raw$pcps_code)))
write_bronze(imf_pcps_raw |> rename(price_usd = value), "imf_pcps_prices", con)

# -- 5. Comtrade world unit values (UV tier) -----------------------------------

message("--- Price series: Comtrade unit values ---")
parquet_glob <- file.path(COMTRADE_PARQUET, "**", "*.parquet")
uv_prices <- compute_unit_values(parquet_glob, uv_codes)
message(sprintf("  Unit values: %s code-months | %d codes", format(nrow(uv_prices), big.mark = ","),
                n_distinct(uv_prices$cmd_code)))
write_bronze(uv_prices, "comtrade_unit_values", con)

# -- 6. Unified price lookup ----------------------------------------------------

wb_prices <- pink_sheet_prices |>
  inner_join(series_lookup |> filter(commodity_name != "coffee"),
             by = "series_name", relationship = "many-to-many") |>
  group_by(cmd_code, yr, mon, price_unit) |>
  summarise(price_wb = mean(price_wb, na.rm = TRUE), .groups = "drop") |>
  mutate(price_origin = "WB")

coffee_cmd <- series_lookup |> filter(commodity_name == "coffee") |> distinct(cmd_code) |> pull()
coffee_wide <- pink_sheet_prices |>
  filter(series_name %in% c("Coffee, Arabica", "Coffee, Robusta")) |>
  mutate(variety = if_else(str_detect(series_name, "Arabica"), "arabica", "robusta")) |>
  select(yr, mon, variety, price_wb) |>
  pivot_wider(names_from = variety, values_from = price_wb, names_prefix = "price_") |>
  mutate(price_avg_coffee = (price_arabica + price_robusta) / 2)

imf_prices <- xwalk_imf |>
  distinct(cmd_code, price_name, price_unit = qty_unit_price) |>
  separate_rows(price_name, sep = ";\\s*") |>
  inner_join(imf_pcps_raw, by = c("price_name" = "pcps_code"), relationship = "many-to-many") |>
  group_by(cmd_code, yr, mon, price_unit) |>
  summarise(price_wb = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(price_origin = "IMF")

fallbacks <- xwalk |> filter(price_fallback != "") |> distinct(cmd_code, price_fallback)
price_lookup <- bind_rows(wb_prices, imf_prices) |>
  apply_price_fallbacks(fallbacks, imf_pcps_raw, uv_prices) |>
  arrange(cmd_code, yr, mon)
write_bronze(price_lookup, "price_lookup", con)
message(sprintf("  Price lookup: %s rows | %d codes | fallback rows: %d",
                format(nrow(price_lookup), big.mark = ","), n_distinct(price_lookup$cmd_code),
                sum(grepl("fallback", price_lookup$price_origin))))

no_price <- xwalk |>
  filter(price_source %in% c("WB", "IMF"), commodity_name != "coffee") |>
  anti_join(price_lookup, by = "cmd_code") |>
  distinct(cmd_code, commodity_name, price_source)
if (nrow(no_price) > 0)
  warning(sprintf("[CHECK] %d priced codes have no price at all:\n  %s", nrow(no_price),
                  paste(no_price$cmd_code, no_price$commodity_name, sep = " (", collapse = ")\n  ")))

# -- 7. Coffee routing and unit conversion ---------------------------------------
#
# Arabica: highland varieties; dominant in Ethiopia, East Africa, Latin America.
# Robusta: lowland, higher yield; dominant in Vietnam, West/Central Africa, SE Asia.
# Countries in neither list get the mean of the two. Prices follow the exporter.

ARABICA_ISO3 <- c("BRA", "COL", "ETH", "HND", "MEX", "GTM", "PER", "CRI", "PAN", "NIC",
                  "KEN", "TZA", "RWA", "BDI", "SLV", "BOL", "ECU", "JAM", "PNG")
ROBUSTA_ISO3 <- c("VNM", "CIV", "IDN", "CMR", "TGO", "GHA", "UGA", "COG", "GAB", "LAO",
                  "KHM", "PHL", "THA", "MDG")

# (qty_unit_code from Comtrade, price_unit from crosswalk) -> conv_factor
# global_value_wb = (qty_comtrade * conv_factor) * price_wb; no row = NA value
unit_conv <- tribble(
  ~qty_unit_code, ~price_unit,    ~conv_factor,
  8L,  "kg",          1.0,       # kg -> kg
  15L, "kg",          0.001,     # g -> kg
  21L, "kg",          1000.0,    # 1000 kg -> kg
  8L,  "mt",          0.001,     # kg -> mt
  15L, "mt",          1e-6,      # g -> mt
  21L, "mt",          1.0,       # 1000 kg -> mt
  8L,  "t",           0.001,     # kg -> t (alias for mt)
  15L, "t",           1e-6,      # g -> t
  21L, "t",           1.0,       # 1000 kg -> t
  21L, "dmtu",        1.0,       # 1000 kg -> dmtu (~1 mt dry)
  8L,  "dmtu",        0.001,     # kg -> dmtu
  8L,  "troy oz",     32.1507,   # kg -> troy oz
  15L, "troy oz",     0.0321507, # g -> troy oz
  21L, "troy oz",     32150.7,   # 1000 kg -> troy oz
  19L, "bbl",         1.0,       # bbl -> bbl
  8L,  "bbl",         0.00733,   # kg -> bbl (crude ~136.4 kg/bbl)
  21L, "bbl",         7.33,      # 1000 kg -> bbl
  12L, "bbl",         6.2898,    # m3 -> bbl (petroleum)
  17L, "mmbtu",       35.315,    # 1000 m3 -> mmbtu
  12L, "mmbtu",       0.035315,  # m3 -> mmbtu (natural gas; not LNG liquid)
  18L, "mmbtu",       947.817,   # TJ -> mmbtu
  20L, "mmbtu",       0.0215,    # 1000 L LNG -> mmbtu
  8L,  "mmbtu",       0.04607,   # kg LNG -> mmbtu
  8L,  "lbs",         2.20462,   # kg -> lbs
  12L, "cubic meter", 1.0        # m3 -> m3 (logs, sawnwood)
) |> mutate(qty_unit_code = as.integer(qty_unit_code))

# -- 8. Pink Sheet up to date? -------------------------------------------------

last_trade <- open_dataset(COMTRADE_PARQUET) |>
  filter(!mirrored) |>
  summarise(t = max(as.integer(as.character(yr)) * 12L + mon)) |>
  collect() |> pull(t)
last_ps <- max(pink_sheet_prices$yr * 12L + pink_sheet_prices$mon)
if (last_ps < last_trade - 1)
  warning(sprintf("[CHECK] The Pink Sheet ends %d-%02d but Comtrade runs to %d-%02d: download the current file (download_pink_sheet())",
                  (last_ps - 1) %/% 12, (last_ps - 1) %% 12 + 1, (last_trade - 1) %/% 12, (last_trade - 1) %% 12 + 1))

# -- 9. Enrich the parquet: one country at a time, through a staging folder ------

message("--- Price series: enriching Comtrade parquet ---")

staging <- paste0(COMTRADE_PARQUET, "_staging")
unlink(staging, recursive = TRUE)
ds <- open_dataset(COMTRADE_PARQUET, unify_schemas = TRUE)
rows_before  <- ds |> summarise(n = n()) |> collect() |> pull(n)
parts_before <- length(list.files(COMTRADE_PARQUET, pattern = "\\.parquet$", recursive = TRUE))
isos <- sub("^iso3c=", "", list.files(COMTRADE_PARQUET, pattern = "^iso3c="))

for (iso in isos) {
  d <- ds |> filter(iso3c == iso) |> collect()
  if (nrow(d) == 0) next
  d <- enrich_trade(d, price_lookup, coffee_wide, coffee_cmd, uv_prices, muv, unit_conv,
                    ARABICA_ISO3, ROBUSTA_ISO3)
  write_dataset(d, staging, partitioning = c("iso3c", "yr"), format = "parquet",
                compression = "zstd", existing_data_behavior = "overwrite")
}
rm(ds, d); invisible(gc())

# -- 10. Check, then swap in -----------------------------------------------------

rows_after  <- open_dataset(staging) |> summarise(n = n()) |> collect() |> pull(n)
parts_after <- length(list.dirs(staging, recursive = TRUE)[grepl("yr=", basename(list.dirs(staging, recursive = TRUE)))])
parts_src   <- length(list.dirs(COMTRADE_PARQUET, recursive = TRUE)[grepl("yr=", basename(list.dirs(COMTRADE_PARQUET, recursive = TRUE)))])
if (rows_before != rows_after || parts_after != parts_src)
  stop(sprintf("Enrichment check failed (rows %d -> %d, partitions %d -> %d); the parquet is unchanged and the staging folder is kept for inspection: %s",
               rows_before, rows_after, parts_src, parts_after, staging))
invisible(gc())
prev <- paste0(COMTRADE_PARQUET, "_prev")
unlink(prev, recursive = TRUE)
if (!file.rename(COMTRADE_PARQUET, prev)) stop("Could not move the old parquet aside; nothing changed")
if (!file.rename(staging, COMTRADE_PARQUET)) {
  file.rename(prev, COMTRADE_PARQUET)
  stop("Could not move the enriched parquet into place; the old parquet was restored")
}
unlink(prev, recursive = TRUE)
message(sprintf("  Enriched %s rows in %d partitions", format(rows_after, big.mark = ","), parts_after))

# -- 11. Spot checks ------------------------------------------------------------

check_enrich <- open_dataset(file.path(COMTRADE_PARQUET, "iso3c=USA", "yr=2024")) |> collect()
message(sprintf(
  "  Enrichment -- USA 2024: price_wb %.1f%% | qty_wb %.1f%% | global_value_wb %.1f%% | deflated %.1f%%",
  mean(!is.na(check_enrich$price_wb)) * 100, mean(!is.na(check_enrich$qty_wb)) * 100,
  mean(!is.na(check_enrich$global_value_wb)) * 100,
  mean(!is.na(check_enrich$constant_value_2024)) * 100))

coffee_check <- open_dataset(COMTRADE_PARQUET) |>
  filter(cmd_code %in% coffee_cmd, yr == 2024, mon == 6, flow_desc == "Export",
         iso3c %in% c("BRA", "VNM")) |>
  distinct(iso3c, price_wb) |> collect()
if (nrow(coffee_check) == 2 && length(unique(coffee_check$price_wb)) == 1)
  warning("[CHECK] Coffee routing failed: BRA and VNM exports have identical prices in Jun 2024")

message("--- Price series: complete ---")
