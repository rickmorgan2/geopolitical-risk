# pink_sheet.R
# ------------------------------------------------------------------------------
# World Bank Pink Sheet helpers, used by ingest_price_series.R.
#
#   read_pink_sheet(path)        monthly prices, long: yr, mon, series_name, price_wb
#   splice_pink_sheet(cur, arch) keep full precision where a newer file only rounded
#   download_pink_sheet()        fetch the current monthly and annual files (manual)
#
# From 2026 the World Bank publishes the monthly file rounded to one or two
# decimals (world sugar 0.338188708 became 0.34). Rounding noise reaches about
# 1.6% for low-priced series, enough to add noise to monthly volatility. Older
# full-precision files are kept as CMO-Historical-Data-Monthly_<date>.xlsx;
# where the new value is exactly an older value rounded, the older value is
# used. Any other difference is a genuine revision and the new value wins.
# ------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Normalize strings for matching between Excel headers and the crosswalk:
# non-ASCII characters transliterated, smart quotes and dashes straightened,
# asterisks removed, whitespace collapsed.
clean_str <- function(x) {
  x |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "") |>
    str_replace_all(" ", " ") |>
    str_replace_all("[–—]", "-") |>
    str_replace_all("[‘’]", "'") |>
    str_replace_all("[“”]", '"') |>
    str_replace_all("[^[:print:]]", "") |>
    str_replace_all("\\*", "") |>
    str_squish()
}

read_pink_sheet <- function(path, sheet = "Monthly Prices") {
  if (!file.exists(path)) stop("Pink Sheet file not found: ", path)
  header_names <- suppressMessages(read_excel(path, sheet = sheet, skip = 4, n_max = 1,
                                              col_names = FALSE)) |>
    unlist(use.names = FALSE) |> clean_str()
  header_units <- suppressMessages(read_excel(path, sheet = sheet, skip = 5, n_max = 1,
                                              col_names = FALSE)) |>
    unlist(use.names = FALSE) |> str_remove_all("[()]") |> clean_str()
  raw <- suppressMessages(read_excel(path, sheet = sheet, skip = 6, col_names = FALSE,
                                     na = c("", "...", "…")))
  n <- ncol(raw) - 1
  names(raw) <- c("period", header_names[seq_len(n)])
  prices <- raw |>
    filter(!is.na(period), str_detect(period, "^[0-9]{4}M")) |>
    mutate(yr  = as.integer(str_sub(period, 1, 4)),
           mon = as.integer(str_extract(period, "(?<=M)[0-9]{2}"))) |>
    select(-period) |>
    pivot_longer(-c(yr, mon), names_to = "series_name", values_to = "price_wb",
                 values_transform = list(price_wb = \(v) suppressWarnings(as.numeric(v)))) |>
    filter(!is.na(price_wb), is.finite(price_wb))
  list(prices = prices,
       units  = tibble(series_name = header_names, unit = header_units[seq_along(header_names)]) |>
         filter(!is.na(series_name)))
}

# TRUE where `new` is exactly `old` rounded to 0-4 decimals
is_rounded_from <- function(new, old) {
  tol <- 1e-9 * pmax(1, abs(new))
  Reduce(`|`, lapply(0:4, \(d) !is.na(old) & abs(new - round(old, d)) <= tol))
}

# Keep full precision from older files (newest archive first) where the current
# file only rounded the value. Returns the prices with a `precision` column.
splice_pink_sheet <- function(current, archives) {
  out <- current |> mutate(precision = "current file")
  for (a in archives) {
    old <- a |> select(yr, mon, series_name, old = price_wb)
    out <- out |>
      left_join(old, by = c("yr", "mon", "series_name")) |>
      mutate(use_old   = precision == "current file" & is_rounded_from(price_wb, old) &
                         abs(price_wb - old) > 0,
             price_wb  = if_else(use_old, old, price_wb),
             precision = if_else(use_old, "archived full precision", precision)) |>
      select(-old, -use_old)
  }
  out
}

# Download the current monthly and annual files from the World Bank's
# commodity-markets page. The previous monthly file is kept as
# CMO-Historical-Data-Monthly_<its date>.xlsx for the precision splice.
download_pink_sheet <- function(raw_dir = dirname(PINK_SHEET_XLSX),
                                page = "https://www.worldbank.org/en/research/commodity-markets") {
  resp <- httr::GET(page, httr::timeout(60), httr::user_agent("research-pipeline"))
  httr::stop_for_status(resp)
  html  <- httr::content(resp, as = "text", encoding = "UTF-8")
  links <- unique(str_extract_all(html, "https?://[^\"' ]+CMO-Historical-Data-(Monthly|Annual)\\.xlsx")[[1]])
  if (length(links) == 0) stop("No Pink Sheet links found on ", page)

  tmp <- tempfile("pink_sheet_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  out <- character()
  for (u in links) {
    name <- basename(u)
    f    <- file.path(tmp, name)
    r    <- httr::GET(u, httr::timeout(120), httr::write_disk(f, overwrite = TRUE),
                      httr::user_agent("research-pipeline"))
    httr::stop_for_status(r)
    sig <- readBin(f, "raw", 2)
    if (!identical(sig, as.raw(c(0x50, 0x4b)))) stop(name, " is not an Excel file")
    if (!any(grepl("Prices|Indices", excel_sheets(f)))) stop(name, " has no price sheets")

    dest <- file.path(raw_dir, name)
    if (file.exists(dest) && unname(tools::md5sum(dest)) == unname(tools::md5sum(f))) {
      message("  ", name, ": unchanged"); next
    }
    if (file.exists(dest) && grepl("Monthly", name)) {
      keep <- file.path(raw_dir, sub("\\.xlsx$", paste0("_", format(file.mtime(dest), "%Y-%m-%d"),
                                                         ".xlsx"), name))
      if (!file.exists(keep)) file.copy(dest, keep, copy.date = TRUE)
      message("  previous monthly file kept as ", basename(keep))
    }
    file.copy(f, dest, overwrite = TRUE)
    message("  ", name, ": updated")
    out <- c(out, dest)
  }
  invisible(out)
}
