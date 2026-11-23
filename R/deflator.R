# deflator.R
# ------------------------------------------------------------------------------
# The World Bank Manufactures Unit Value index (MUV) as the deflator for trade
# values (D-025).
#
# Source: CMO-Historical-Data-Annual.xlsx (PINK_SHEET_ANNUAL_XLSX), sheet
# "Annual Indices (Real)", column "Deflator MUV Index", 2010 = 100. MUV is the
# US-dollar unit value of manufactures exported by 15 major economies; the
# World Bank uses it for its constant-dollar commodity prices. It is annual.
#
# Monthly values: each annual value sits at mid-year (between June and July)
# and months are interpolated log-linearly between mid-points, so there is no
# step at January. Months after the last mid-point hold its level and are
# marked provisional until the next annual value is published. The index is
# rebased so that the 2024 annual value = 100 ("constant 2024 US$").
# ------------------------------------------------------------------------------

library(readxl)

# Annual MUV, 2010 = 100: year, muv
load_muv_annual <- function(path, sheet = "Annual Indices (Real)") {
  x <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE))
  hit <- which(apply(as.matrix(x[seq_len(min(15, nrow(x))), ]), c(1, 2),
                     \(v) grepl("MUV", v, ignore.case = TRUE)), arr.ind = TRUE)
  if (nrow(hit) == 0) stop("No MUV column found in sheet '", sheet, "' of ", path)
  years <- suppressWarnings(as.integer(x[[1]]))
  muv   <- suppressWarnings(as.numeric(x[[hit[1, "col"]]]))
  out   <- data.frame(year = years, muv = muv)
  out   <- out[!is.na(out$year) & !is.na(out$muv) & out$year >= 1900, ]
  if (nrow(out) < 2) stop("MUV column has fewer than two years in ", path)
  out[order(out$year), ]
}

# Monthly index (base_year = 100) for the given years, with the provisional flag
muv_monthly <- function(annual, years, base_year = 2024L) {
  if (!base_year %in% annual$year) stop("MUV has no value for base year ", base_year)
  t_pts <- annual$year + 0.5                       # mid-year of each annual value
  grid  <- expand.grid(mon = 1:12, yr = as.integer(years))[, c("yr", "mon")]
  t_mon <- grid$yr + (grid$mon - 0.5) / 12         # mid-month
  logv  <- stats::approx(t_pts, log(annual$muv), xout = t_mon, rule = 2)$y
  base  <- annual$muv[annual$year == base_year]
  data.frame(yr = grid$yr, mon = grid$mon,
             muv_idx_2024 = exp(logv) / base * 100,
             deflator_provisional = t_mon > max(t_pts))
}
