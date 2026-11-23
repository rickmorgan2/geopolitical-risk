# price_helpers.R
# ------------------------------------------------------------------------------
# Functions used by ingest_price_series.R (kept here so the tests can load them
# without running the price step).
#
#   fetch_fred_series()     one IMF PCPS series from FRED, cached on disk
#   compute_unit_values()   Comtrade world unit values: the price tier for codes
#                           with no market quote (D-029)
#   apply_price_fallbacks() continue a discontinued series with another one,
#                           scaled to it over their overlap
#   enrich_trade()          prices, quantities in price units, MUV deflation
# ------------------------------------------------------------------------------

library(dplyr)
library(purrr)

# One FRED series as yr, mon, value (US$ per unit after `divisor`). A
# successful download refreshes the cache; if FRED cannot be reached, the
# cached copy is used with a warning.
fetch_fred_series <- function(fred_id, divisor = 1, cache_dir = "data/raw_fred") {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache <- file.path(cache_dir, paste0(fred_id, ".csv"))
  resp  <- tryCatch(httr::GET(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", fred_id),
                              httr::timeout(60)),
                    error = function(e) NULL)
  txt <- if (!is.null(resp) && httr::status_code(resp) == 200)
    httr::content(resp, as = "text", encoding = "UTF-8") else NULL
  if (!is.null(txt) && grepl(fred_id, substr(txt, 1, 200), fixed = TRUE)) {
    writeLines(txt, cache)
  } else if (file.exists(cache)) {
    warning("[CHECK] FRED unreachable for ", fred_id, "; using the cached copy from ",
            format(file.mtime(cache), "%Y-%m-%d"))
    txt <- paste(readLines(cache, warn = FALSE), collapse = "\n")
  } else {
    stop("FRED fetch failed for ", fred_id, " and no cached copy exists")
  }
  x <- read.csv(text = txt, stringsAsFactors = FALSE)
  names(x) <- c("date", "value")
  x$value <- suppressWarnings(as.numeric(x$value))
  x <- x[!is.na(x$value), ]
  data.frame(yr = as.integer(substr(x$date, 1, 4)), mon = as.integer(substr(x$date, 6, 7)),
             value = x$value / divisor)
}

# Median unit value across reporter-flows per code and month, in the code's
# main quantity unit. Self-reported rows only; quantities Comtrade estimated
# are left out (D-015). At least `min_obs` reporter-flows per month.
compute_unit_values <- function(glob, codes, min_obs = 3L) {
  if (length(codes) == 0) return(data.frame(cmd_code = character(), yr = integer(), mon = integer(),
                                            uv_unit_code = integer(), uv_price = numeric(),
                                            uv_n = integer()))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "uv_codes", data.frame(cmd_code = unique(codes)), temporary = TRUE)
  DBI::dbGetQuery(con, sprintf("
    WITH r AS (
      SELECT t.cmd_code, CAST(t.yr AS INTEGER) AS yr, CAST(t.mon AS INTEGER) AS mon,
             t.iso3c, t.flow_desc, CAST(t.qty_unit_code AS INTEGER) AS qty_unit_code,
             SUM(t.primary_value) AS v, SUM(t.qty_comtrade) AS q
      FROM read_parquet('%s', hive_partitioning = true, union_by_name = true) t
      SEMI JOIN uv_codes c ON t.cmd_code = c.cmd_code
      WHERE NOT COALESCE(t.mirrored, FALSE) AND t.primary_value > 0 AND t.qty_comtrade > 0
        AND NOT COALESCE(t.is_qty_estimated, FALSE) AND t.qty_unit_code > 0
      GROUP BY ALL
    ),
    main_unit AS (
      SELECT cmd_code, qty_unit_code FROM (
        SELECT cmd_code, qty_unit_code,
               ROW_NUMBER() OVER (PARTITION BY cmd_code ORDER BY COUNT(*) DESC, qty_unit_code) AS rk
        FROM r GROUP BY cmd_code, qty_unit_code)
      WHERE rk = 1
    )
    SELECT r.cmd_code, r.yr, r.mon, r.qty_unit_code AS uv_unit_code,
           MEDIAN(r.v / r.q) AS uv_price, CAST(COUNT(*) AS INTEGER) AS uv_n
    FROM r JOIN main_unit u USING (cmd_code, qty_unit_code)
    GROUP BY 1, 2, 3, 4
    HAVING COUNT(*) >= %d
    ORDER BY 1, 2, 3", gsub("\\\\", "/", glob), as.integer(min_obs)))
}

# After a code's own price series ends, continue it with a fallback series
# ("FRED:<code>" or "UV") scaled by the median ratio of the two over the last
# `window` months they share. Needs at least 6 shared months.
apply_price_fallbacks <- function(prices, fallbacks, fred, uv, window = 36L) {
  if (is.null(fallbacks) || nrow(fallbacks) == 0) return(prices)
  add <- map(seq_len(nrow(fallbacks)), \(i) {
    code <- fallbacks$cmd_code[i]; fb <- fallbacks$price_fallback[i]
    p <- prices |> filter(cmd_code == code, !is.na(price_wb)) |> mutate(t = yr * 12L + mon)
    if (nrow(p) == 0) return(NULL)
    last <- max(p$t)
    s <- if (startsWith(fb, "FRED:")) {
      fred |> filter(pcps_code == sub("^FRED:", "", fb)) |> transmute(yr, mon, fb_price = value)
    } else {
      uv |> filter(cmd_code == code) |> transmute(yr, mon, fb_price = uv_price)
    }
    if (nrow(s) == 0) return(NULL)
    s <- s |> mutate(t = yr * 12L + mon)
    ov <- inner_join(p, s, by = c("yr", "mon", "t")) |> filter(t > last - window, fb_price > 0)
    if (nrow(ov) < 6) return(NULL)
    ratio <- median(ov$price_wb / ov$fb_price)
    s |> filter(t > last) |>
      transmute(cmd_code = code, yr, mon, price_wb = fb_price * ratio,
                price_unit = p$price_unit[1],
                price_origin = paste(if (startsWith(fb, "FRED:")) "FRED" else "UV", "fallback"))
  }) |> compact() |> list_rbind()
  bind_rows(prices, add)
}

ENRICHED_COLS <- c("price_wb", "price_unit", "price_origin", "commodity_name_price", "qty_source",
                   "qty_wb", "global_value_wb", "global_value_wb_real", "cpi_idx_2024",
                   "cpi_std_2024", "conv_factor", "constant_value_2024", "muv_idx_2024",
                   "deflator_provisional")

# Prices, quantities in price units, world values and MUV deflation for one
# batch of trade rows.
#   prices   cmd_code, yr, mon, price_wb, price_unit, price_origin (WB, IMF, fallbacks)
#   coffee   yr, mon, price_arabica, price_robusta, price_avg_coffee
#   uv       cmd_code, yr, mon, uv_unit_code, uv_price (unit-value tier)
#   muv      yr, mon, muv_idx_2024, deflator_provisional
# Coffee follows the exporting country's variety: the reporter on an export
# row, the partner on an import row.
enrich_trade <- function(d, prices, coffee, coffee_codes, uv, muv, unit_conv,
                         arabica, robusta) {
  d <- d |>
    select(-any_of(ENRICHED_COLS)) |>
    mutate(yr = as.integer(as.character(yr)), mon = as.integer(mon),
           qty_unit_code = as.integer(qty_unit_code)) |>
    left_join(prices |> select(cmd_code, yr, mon, price_wb, price_unit, price_origin),
              by = c("cmd_code", "yr", "mon")) |>
    left_join(uv |> select(cmd_code, yr, mon, uv_unit_code, uv_price),
              by = c("cmd_code", "yr", "mon")) |>
    mutate(use_uv       = is.na(price_wb) & !is.na(uv_price),
           price_wb     = if_else(use_uv, uv_price, price_wb),
           price_unit   = if_else(use_uv, "uv", price_unit),
           price_origin = if_else(use_uv, "UV", price_origin)) |>
    left_join(coffee, by = c("yr", "mon")) |>
    mutate(
      exporter     = if_else(flow_desc == "Export", iso3c, partner_iso3c),
      is_coffee    = cmd_code %in% coffee_codes,
      price_wb     = case_when(is_coffee & exporter %in% arabica ~ price_arabica,
                               is_coffee & exporter %in% robusta ~ price_robusta,
                               is_coffee                         ~ price_avg_coffee,
                               TRUE                              ~ price_wb),
      price_unit   = if_else(is_coffee, "kg", price_unit),
      price_origin = if_else(is_coffee & !is.na(price_wb), "WB", price_origin)
    ) |>
    select(-any_of(c("price_arabica", "price_robusta", "price_avg_coffee", "exporter", "is_coffee"))) |>
    mutate(
      qty_for_conv   = if_else(qty_unit_code == -1L & !is.na(net_wgt_comtrade),
                               net_wgt_comtrade, qty_comtrade),
      qty_unit_final = as.integer(if_else(qty_unit_code == -1L & !is.na(net_wgt_comtrade),
                                          8L, qty_unit_code)),
      qty_source = case_when(
        qty_unit_code != -1L & !is.na(qty_comtrade)     ~ "primary",
        qty_unit_code == -1L & !is.na(net_wgt_comtrade) ~ "net_wgt_fallback",
        TRUE                                             ~ "none")
    ) |>
    left_join(unit_conv |> rename(qty_unit_final = qty_unit_code),
              by = c("qty_unit_final", "price_unit")) |>
    mutate(conv_factor     = if_else(use_uv, if_else(qty_unit_final == uv_unit_code, 1, NA_real_),
                                     conv_factor),
           qty_wb          = qty_for_conv * conv_factor,
           global_value_wb = qty_wb * price_wb) |>
    left_join(muv, by = c("yr", "mon")) |>
    mutate(constant_value_2024  = primary_value / muv_idx_2024 * 100,     # constant 2024 US$ (D-025)
           global_value_wb_real = global_value_wb / muv_idx_2024 * 100) |>
    select(-conv_factor, -qty_for_conv, -qty_unit_final, -use_uv, -uv_unit_code, -uv_price)
  d
}
