# compute_silver_comtrade.R
#
# Silver-layer trade indicators derived from the Comtrade monthly parquet.
# Computes rolling-window metrics separately for five commodity groups so that
# concentration, volatility, and risk signals are group-specific rather than
# diluted across all traded goods.
#
# -- Seasonality treatment ---------------------------------------------------
#   All volatility, VaR, and shock metrics are computed on year-over-year (YoY)
#   changes of 12-month rolling sums rather than month-over-month changes of raw
#   monthly values. This eliminates the need for seasonal decomposition by
#   construction: each observation compares an equivalent trailing window to the
#   same window one year prior.
#
#   r12[t]   = sum of value_usd in months t-11 through t   (rolling 12m sum)
#   yoy[t]   = (r12[t] / r12[t-12] - 1) * 100             (YoY % change)
#
#   CV columns report the rolling standard deviation of yoy (not SD/mean) because
#   the mean YoY change is close to zero for stable series, making the ratio
#   unstable. SD of annual growth rates is a standard economic volatility measure.
#   Units: percentage points.
#
# -- Mirror share ------------------------------------------------------------
#   `mirror_share` joins from bronze.comtrade_mirror_share. It reflects the
#   share of trade value in that country-month-flow that was estimated from
#   partner-reported flows rather than self-reported. Surfaced on the dashboard
#   as a data-quality indicator alongside every figure.
#
# -- Commodity groups --------------------------------------------------------
#   pink_sheet       World Bank Pink Sheet commodities
#   food             Food category (Cereals, Meat, Oils and Meals, etc.)
#   energy           Energy category (petroleum, coal, natural gas, LNG)
#   cereal           Cereal subcategory (subset of food)
#   fertilizer       Fertilizers category (DAP, urea, potash -- food production inputs)
#
# Critical mineral data is sourced from USGS only (compute_silver_usgs.R).
# Comtrade critical mineral flows are too sparse at monthly level to be reliable.
#
# -- Output tables -----------------------------------------------------------
#
#   silver.comtrade_export_groups_monthly  country-year-month-group
#   silver.comtrade_import_groups_monthly  country-year-month-group
#     Full monthly time series of rolling metrics. Use for dashboard charts.
#
#   silver.comtrade_export_groups_ts       country-year-group
#   silver.comtrade_import_groups_ts       country-year-group
#     December snapshot of rolling metrics; annual sum for value_usd.
#     Use for gold scoring and cross-country comparison.
#
#   silver.d2_import_hhi_monthly           country-year-month
#   silver.d2_import_hhi_annual            country-year
#     Pre-pivoted supplier diversity indicators for D2 Bayesian models.
#     diversity = 1 - supplier_hhi for food, energy, cereal, fertilizer groups.
#
#   silver.d3_export_fragility_monthly     country-year-month
#   silver.d3_export_fragility_annual      country-year
#     Pre-pivoted export fragility indicators for D3 Bayesian models.
#     diversity_partner, diversity_commodity, export_independence from pink_sheet.
#
#   silver.monthly_disruption_d2           country-year-month
#     Six indicators feeding compute_bayes_d2_monthly.R:
#       food_vol_anomaly_z       food import volume anomaly (winsorized +/-Z_CAP)
#       food_price_z             food import basket price anomaly (winsorized)
#       energy_price_z           energy import basket price anomaly (winsorized)
#       food_cv_basket_wtd       Morgan-Reinhardt food import price volatility
#       energy_cv_basket_wtd     Morgan-Reinhardt energy import price volatility
#       fertilizer_cv_basket_wtd Morgan-Reinhardt fertilizer import price volatility
#
#   silver.monthly_disruption_d3           country-year-month
#     Three indicators feeding compute_bayes_d3_monthly.R:
#       export_vol_anomaly_z     pink_sheet export volume anomaly (winsorized)
#       export_basket_price_z    export basket price anomaly (winsorized)
#       cv_basket_wtd            Morgan-Reinhardt export price volatility
#
#   silver.bilateral_category_shares       country-partner-year
#     Annual bilateral import/export shares by category for D4 partner detail:
#       food_import_share, fertilizer_import_share, energy_import_share, export_share
#
# -- Shared group columns ----------------------------------------------------
#   value_usd              group trade value (monthly; annual = sum)
#   value_share            group as % of country total trade
#   mirror_share           share of value estimated via partner-reported flows
#   cv_unweighted          rolling SD of group-total YoY % changes
#   cv_weighted            commodity-weighted SD of per-commodity YoY % changes
#   cv_basket_wtd          Morgan-Reinhardt weighted basket price volatility: SD of
#                          3-month smoothed monthly log price first differences,
#                          12-month window lagged 1 month, weighted by lagged value shares
#   cv_basket_wb_share     share of basket weight covered by WB Pink prices
#   commodity_hhi          HHI of 12m rolling commodity shares within group
#   n_active_commodities   commodities with positive 12m rolling share
#   top_commodity_share    largest commodity share
#   top3_commodity_share   top-3 cumulative share
#   var_05                 5th-pct of rolling YoY distribution
#   var_10                 10th-pct of rolling YoY distribution
#   cvar_05                mean of YoY observations <= var_05 (expected shortfall)
#   max_drawdown           min peak-to-trough of r12 in trailing VAR_WINDOW months
#   current_drawdown       current r12 vs trailing VAR_WINDOW peak
#   neg_return_freq        % months where YoY change was negative
#   [export] partner_hhi / partner_hhi_delta3 / n_partners / top_partner_share
#   [import] supplier_hhi / supplier_hhi_delta3 / n_suppliers / top_supplier_share
#   [import] price_avg_usd / price_yoy_pct / price_vol_cv
#   [monthly] shock              TRUE if this month's YoY < baseline mean - 2 SD
#   [annual]  shock_months_yr / shock_flag
#
# -- Annual-only columns -----------------------------------------------------
#   trend_slope / trend_pct      OLS slope of annual value, trailing 5 years
#   mirror_share_annual          annual average mirror share
#
# -- Run position in pipeline ------------------------------------------------
#   After:  ingest_price_series.R (enriches parquet with price_wb)
#           ingest_comtrade.R     (writes bronze.cmd_meta, bronze.comtrade_mirror_share)
#   Before: compute_bayes_d2_annual.R, compute_bayes_d2_monthly.R,
#           compute_bayes_d3_annual.R, compute_bayes_d3_monthly.R,
#           compute_d4_network.R
#
# Expects: `con` (DBI connection to georisk.duckdb) from run_pipeline.R

library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(stringr)
library(glue)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# -- Config ------------------------------------------------------------------
# START_YR and TS_START_YR inherited from config.R via run_pipeline.R.
# Guards below catch standalone sourcing.
if (!exists("START_YR"))    START_YR    <- 2010L
if (!exists("TS_START_YR")) TS_START_YR <- 2010L
if (!exists("CMD_MIN_USD")) CMD_MIN_USD <- 1e5
if (!exists("YOY_CAP"))     YOY_CAP     <- 500

COMTRADE_PARQUET <- normalizePath(
  file.path("..", "commodity-project", "data", "output", "comtrade_parquet"),
  mustWork = FALSE
) |> str_replace_all("\\\\", "/")

WORLD_CODES     <- c("WLD", "_X", "W00")   # excluded from bilateral HHI
ROLL_SUM_WINDOW <- 12L    # trailing months for rolling sum (r12)
WEIGHT_WINDOW   <- 24L    # lagged weight lookback for value share weights
MR_SMOOTH_WIN   <- 3L     # M&R: months to smooth log price before differencing
MR_VOL_WIN      <- 12L    # M&R: SD window (t-13 to t-1, lagged 1 month)
YOY_WINDOW      <- 24L    # YoY observations for vol/VaR/shock statistics
VAR_WINDOW      <- 12L    # months for drawdown window (on r12 levels)
TREND_YEARS     <- 5L     # years: OLS slope and cross-year price CV
SHOCK_MIN_USD   <- 1e6    # min r12 (USD) before a month can be flagged as shock

# -- Disruption indicator config (monthly_disruption_d2 / d3) ----------------
RUPTURE_THRESH  <- 0.10   # min trailing 12m share to qualify as "major supplier"
BASELINE_MIN_N  <- 3L     # min prior same-month obs required for anomaly z-score
Z_CAP           <- 5      # basket price z-scores winsorized at +/- this value
MIN_VALUE_SHARE <- 0.001  # group value_share floor; concentration metrics nulled below this;
                           #   diversity metrics set to 1.0 (no concentration) not NA
VOL_WINDOW      <- 6L     # rolling window for export revenue/price CV

GROUPS <- list(
  pink_sheet  = list(where_r   = \(d) filter(d, pink_sheet == 1),
                     where_sql = "COALESCE(m.pink_sheet, 0) = 1"),
  food        = list(where_r   = \(d) filter(d, category == "Food"),
                     where_sql = "t.category = 'Food'"),
  energy      = list(where_r   = \(d) filter(d, category == "Energy"),
                     where_sql = "t.category = 'Energy'"),
  cereal      = list(where_r   = \(d) filter(d, category == "Food", subcategory == "Cereals"),
                     where_sql = "t.category = 'Food' AND t.subcategory = 'Cereals'"),
  fertilizer  = list(where_r   = \(d) filter(d, category == "Fertilizers"),
                     where_sql = "t.category = 'Fertilizers'")
)

wc_sql <- paste(sprintf("'%s'", WORLD_CODES), collapse = ", ")

# -- Helpers -----------------------------------------------------------------

# Lagged same-month robust z-score (median / normalized MAD).
# Requires df sorted by country_code, month, year (ascending).
# out_col receives NA when fewer than min_n prior observations exist or MAD == 0.
# constant = 1.4826 scales MAD to match SD for a normal distribution.
add_robust_z <- function(df, value_col, out_col, min_n = BASELINE_MIN_N) {
  df |>
    arrange(country_code, month, year) |>
    group_by(country_code, month) |>
    mutate(!!out_col := map_dbl(seq_along(.data[[value_col]]), \(i) {
      hist <- .data[[value_col]][seq_len(i - 1)]
      hist <- hist[!is.na(hist)]
      if (length(hist) < min_n) return(NA_real_)
      m <- median(hist)
      s <- mad(hist, constant = 1.4826)
      if (s == 0) return(NA_real_)
      (.data[[value_col]][i] - m) / s
    })) |>
    ungroup()
}

write_silver <- function(df, table_name, con) {
  df <- mutate(df, transform_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS silver")
  dbWriteTable(con, DBI::Id(schema = "silver", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [silver.%s] %d rows written", table_name, nrow(df)))
}


# Rolling SD of YoY changes; NA when fewer than `window` observations available.
# Input yoy series must be pre-sanitized (no Inf/-Inf) before calling;
# sd(na.rm=TRUE) handles NA but not Inf.
roll_vol_yoy <- function(yoy, window = YOY_WINDOW) {
  rollapply(yoy, width = window,
            FUN = function(v) sd(v, na.rm = TRUE),
            fill = NA, align = "right", partial = FALSE)
}

# Rolling empirical quantile of YoY changes (VaR)
roll_var_yoy <- function(yoy, window = YOY_WINDOW, prob) {
  rollapply(yoy, width = window,
            FUN = function(v) quantile(v, probs = prob, na.rm = TRUE),
            fill = NA, align = "right", partial = 8)   # require ~8 obs for quantile
}

# Rolling CVaR: mean of YoY obs at or below the VaR threshold
roll_cvar_yoy <- function(yoy, window = YOY_WINDOW, prob) {
  rollapply(yoy, width = window,
            FUN = function(v) {
              thr <- quantile(v, probs = prob, na.rm = TRUE)
              mean(v[v <= thr], na.rm = TRUE)
            },
            fill = NA, align = "right", partial = 8)
}

# Shock detection on YoY series.
# Flags months where yoy[i] < mean(baseline_yoy) - 2*SD(baseline_yoy).
# Guards: (1) series shorter than window -> no flags; (2) r12 < SHOCK_MIN_USD -> no flag
roll_shock_yoy <- function(r12, yoy, window = YOY_WINDOW, min_mean = SHOCK_MIN_USD) {
  n   <- length(yoy)
  out <- rep(FALSE, n)
  if (n <= window) return(out)
  for (i in (window + 1L):n) {
    if (is.na(r12[i]) || r12[i] < min_mean || is.na(yoy[i])) next
    base <- yoy[(i - window):(i - 1L)]
    bm   <- mean(base, na.rm = TRUE)
    bs   <- sd(base,   na.rm = TRUE)
    if (!is.na(bs) && bs > 0)
      out[i] <- yoy[i] < bm - 2 * bs
  }
  out
}

# CV of a rolling window (SD / |mean|); NA when mean is near zero.
# Used for export revenue and basket price volatility metrics.
roll_cv <- function(x, k = VOL_WINDOW) {
  rollapply(x, width = k,
            FUN = function(v) {
              m <- mean(v, na.rm = TRUE)
              if (is.na(m) || abs(m) < 1e-9) return(NA_real_)
              sd(v, na.rm = TRUE) / abs(m)
            },
            fill = NA, align = "right", partial = FALSE)
}

# -- Core computation --------------------------------------------------------

# Build complete country-commodity-month grid for one group, compute all
# rolling metrics on YoY changes of 12m rolling sums (seasonal-neutral).
# Returns monthly-grain tibble (country_code, year, month, ...).
compute_group_monthly <- function(monthly_grp, wb_pv = NULL) {
  active <- monthly_grp |>
    filter(value_usd >= CMD_MIN_USD) |>
    distinct(country_code, commodity_name)

  if (nrow(active) == 0) return(tibble())

  max_yr <- max(monthly_grp$year)

  # Complete grid: gap months get 0
  complete <- active |>
    cross_join(expand_grid(year = START_YR:max_yr, month = 1:12)) |>
    left_join(monthly_grp, by = c("country_code", "commodity_name", "year", "month")) |>
    mutate(value_usd = replace_na(value_usd, 0)) |>
    arrange(country_code, year, month, commodity_name)

  # Country-month group totals
  totals <- complete |>
    group_by(country_code, year, month) |>
    summarise(total_usd = sum(value_usd, na.rm = TRUE), .groups = "drop") |>
    arrange(country_code, year, month)

  # 12m rolling sum per commodity; used for HHI and weighted vol
  r12_cmd <- complete |>
    arrange(country_code, commodity_name, year, month) |>
    group_by(country_code, commodity_name) |>
    mutate(r12_cmd = rollsum(value_usd, k = ROLL_SUM_WINDOW, fill = NA, align = "right")) |>
    ungroup()

  # 12m rolling sum and YoY change for group totals
  r12_totals <- totals |>
    arrange(country_code, year, month) |>  # must be sorted before rollsum
    group_by(country_code) |>
    mutate(
      r12     = rollsum(total_usd, k = ROLL_SUM_WINDOW, fill = NA, align = "right"),
      # Same CMD_MIN_USD guard as yoy_cmd: near-zero lags produce huge finite YoY
      yoy_pct = if_else(
        lag(r12, ROLL_SUM_WINDOW) >= CMD_MIN_USD,
        (r12 / lag(r12, ROLL_SUM_WINDOW) - 1) * 100,
        NA_real_
      ),
      yoy_pct = pmax(-YOY_CAP, pmin(YOY_CAP, yoy_pct)) # cap before rolling SD
    ) |>
    ungroup()

  # Unweighted vol: rolling SD of group-total YoY changes
  vol_unw <- r12_totals |>
    arrange(country_code, year, month) |>  # must be sorted before rollapply
    group_by(country_code) |>
    mutate(cv_unweighted = roll_vol_yoy(yoy_pct)) |>
    ungroup()

  # Commodity-weighted vol: weighted avg of per-commodity YoY SDs
  # Weights are lagged 24m rolling sums to avoid look-ahead bias
  cv_wtd <- r12_cmd |>
    arrange(country_code, commodity_name, year, month) |>
    group_by(country_code, commodity_name) |>
    mutate(
      # Guard: denominator must meet minimum trade threshold.
      # is.finite() alone is insufficient - a near-zero lag (float artifact)
      # produces astronomically large but finite YoY values (e.g. 4e18%)
      # that pass is.finite() and corrupt cv_weighted downstream.
      yoy_cmd  = if_else(
        lag(r12_cmd, ROLL_SUM_WINDOW) >= CMD_MIN_USD,
        (r12_cmd / lag(r12_cmd, ROLL_SUM_WINDOW) - 1) * 100,
        NA_real_
      ),
      yoy_cmd  = pmax(-YOY_CAP, pmin(YOY_CAP, yoy_cmd)), # cap before SD
      cmd_vol  = roll_vol_yoy(yoy_cmd),
      hist_sum = lag(rollsum(value_usd, k = WEIGHT_WINDOW, fill = NA, align = "right"), 1)
    ) |>
    ungroup() |>
    group_by(country_code, year, month) |>
    mutate(
      total_hist = sum(hist_sum, na.rm = TRUE),
      weight     = if_else(total_hist > 0, replace_na(hist_sum, 0) / total_hist, 0)
    ) |>
    summarise(
      # if_else(is.finite()) guards against any large-finite cmd_vol slipping through
      cv_weighted = sum(weight * if_else(is.finite(cmd_vol), cmd_vol, 0), na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(cv_weighted = if_else(cv_weighted == 0, NA_real_, cv_weighted))

  # Weighted basket price volatility (Morgan-Reinhardt 2016 method)
  # Uses WB Pink world prices (wb_pv) where available; falls back to Comtrade
  # implied unit prices for commodities without a Pink Sheet series.
  # Price vol is commodity-level from Pink Sheet (country-invariant); weights
  # are country-specific lagged 24m value shares -- same as cv_weighted above.
  cv_basket <- r12_cmd |>
    # Join pre-computed world price vol if provided; NA where no Pink Sheet match
    (\(.) if (!is.null(wb_pv))
        left_join(., wb_pv, by = c("commodity_name", "year", "month"))
      else
        mutate(., price_vol_wb = NA_real_))() |>
    mutate(
      # Comtrade fallback: implied unit price when value and qty both present
      unit_price_ct = if_else(
        value_usd > 0 & !is.na(qty_comtrade) & qty_comtrade > 0,
        value_usd / qty_comtrade, NA_real_
      )
    ) |>
    arrange(country_code, commodity_name, year, month) |>
    group_by(country_code, commodity_name) |>
    mutate(
      # Comtrade fallback: 3-month smoothed first difference of log unit price (M&R adaptation)
      ln_price_ct    = if_else(!is.na(unit_price_ct), log(unit_price_ct), NA_real_),
      ln_price_ct_sm = rollmean(ln_price_ct, k = MR_SMOOTH_WIN, fill = NA, align = "right"),
      dlnp_ct        = ln_price_ct_sm - lag(ln_price_ct_sm, 1L),
      dlnp_ct        = pmax(-3, pmin(3, dlnp_ct)),
      price_vol_ct   = lag(
        rollapply(dlnp_ct, width = MR_VOL_WIN,
                  FUN = function(v) sd(v, na.rm = TRUE),
                  fill = NA, align = "right", partial = FALSE),
        1L),
      # WB Pink takes precedence; Comtrade unit price is the fallback
      price_vol    = coalesce(price_vol_wb, price_vol_ct),
      hist_sum     = lag(rollsum(value_usd, k = WEIGHT_WINDOW, fill = NA, align = "right"), 1)
    ) |>
    ungroup() |>
    group_by(country_code, year, month) |>
    mutate(
      total_hist = sum(hist_sum, na.rm = TRUE),
      weight     = if_else(total_hist > 0, replace_na(hist_sum, 0) / total_hist, 0)
    ) |>
    summarise(
      cv_basket_wtd = sum(weight * if_else(is.finite(price_vol), price_vol, 0), na.rm = TRUE),
      # share of basket weight covered by WB Pink prices (vs Comtrade unit price fallback)
      cv_basket_wb_share = sum(
        weight * if_else(!is.na(price_vol_wb) & is.finite(price_vol_wb), 1, 0),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      cv_basket_wtd      = if_else(cv_basket_wtd == 0, NA_real_, cv_basket_wtd),
      cv_basket_wb_share = if_else(is.na(cv_basket_wtd), NA_real_, cv_basket_wb_share)
    )

  # Commodity HHI: shares of 12m rolling sums within group (shares in %)
  cmd_hhi <- r12_cmd |>
    group_by(country_code, year, month) |>
    mutate(
      rolling_total = sum(r12_cmd, na.rm = TRUE),
      cmd_share     = if_else(rolling_total > 0,
                              replace_na(r12_cmd, 0) / rolling_total * 100, 0)
    ) |>
    arrange(desc(cmd_share)) |>
    summarise(
      commodity_hhi        = sum(cmd_share^2, na.rm = TRUE),
      n_active_commodities = as.integer(sum(cmd_share > 0)),
      top_commodity_share  = first(cmd_share),
      top3_commodity_share = sum(head(cmd_share, 3), na.rm = TRUE),
      .groups = "drop"
    )

  # VaR, drawdown, neg_return_freq, shock -- all on YoY / r12
  risk <- vol_unw |>
    arrange(country_code, year, month) |>  # must precede group_by for rolling fns
    group_by(country_code) |>
    mutate(
      var_05           = roll_var_yoy(yoy_pct, prob = 0.05),
      var_10           = roll_var_yoy(yoy_pct, prob = 0.10),
      cvar_05          = roll_cvar_yoy(yoy_pct, prob = 0.05),
      rolling_peak     = rollmax(r12, k = VAR_WINDOW, fill = NA, align = "right"),
      current_drawdown = (r12 / rolling_peak - 1) * 100,        # on smoothed r12 series
      max_drawdown     = rollapply(
        current_drawdown, width = VAR_WINDOW,
        FUN = function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
        fill = NA, align = "right", partial = 6),
      neg_return_freq  = rollapply(
        yoy_pct, width = YOY_WINDOW,
        FUN = function(x) mean(x < 0, na.rm = TRUE) * 100,      # % months YoY negative
        fill = NA, align = "right", partial = 6),
      shock            = roll_shock_yoy(r12, yoy_pct)
    ) |>
    ungroup()

  # Join all monthly metrics; preserve monthly grain
  vol_unw |>
    select(country_code, year, month, value_usd = total_usd, cv_unweighted) |>
    left_join(cv_wtd,    by = c("country_code", "year", "month")) |>
    left_join(cv_basket, by = c("country_code", "year", "month")) |>
    left_join(cmd_hhi, by = c("country_code", "year", "month")) |>
    left_join(
      risk |> select(country_code, year, month,
                     var_05, var_10, cvar_05,
                     max_drawdown, current_drawdown, neg_return_freq,
                     shock),
      by = c("country_code", "year", "month")
    )
}

# Collapse monthly to annual: value_usd = sum; rolling metrics = December snapshot
monthly_to_annual <- function(monthly) {
  monthly |>
    group_by(country_code, year) |>
    summarise(
      value_usd            = sum(value_usd, na.rm = TRUE),
      cv_unweighted        = cv_unweighted[month == 12][1],
      cv_weighted          = cv_weighted[month == 12][1],
      cv_basket_wtd        = cv_basket_wtd[month == 12][1],
      cv_basket_wb_share   = cv_basket_wb_share[month == 12][1],
      commodity_hhi        = commodity_hhi[month == 12][1],
      n_active_commodities = n_active_commodities[month == 12][1],
      top_commodity_share  = top_commodity_share[month == 12][1],
      top3_commodity_share = top3_commodity_share[month == 12][1],
      var_05               = var_05[month == 12][1],
      var_10               = var_10[month == 12][1],
      cvar_05              = cvar_05[month == 12][1],
      max_drawdown         = max_drawdown[month == 12][1],
      current_drawdown     = current_drawdown[month == 12][1],
      neg_return_freq      = neg_return_freq[month == 12][1],
      .groups = "drop"
    ) |>
    filter(value_usd > 0)
}

# OLS trend appended to annual table
# OLS slope of value_usd ~ year over a trailing TREND_YEARS window.
# Computed per country-year (local OLS); requires >= 3 observations.
fit_ols <- function(x, y) {
  b <- coef(lm(y ~ x))[["x"]]
  list(slope = b, slope_pct = b / abs(mean(y)) * 100)
}

add_ols_trend <- function(annual) {
  annual |>
    arrange(country_code, year) |>
    group_by(country_code) |>
    mutate(
      trend_slope = map_dbl(row_number(), \(i) {
        start <- max(1L, i - TREND_YEARS + 1L)
        mask  <- !is.na(value_usd[start:i])
        if (sum(mask) < 3) return(NA_real_)
        fit_ols(year[start:i][mask], value_usd[start:i][mask])$slope
      }),
      trend_pct = map_dbl(row_number(), \(i) {
        start <- max(1L, i - TREND_YEARS + 1L)
        mask  <- !is.na(value_usd[start:i])
        if (sum(mask) < 3) return(NA_real_)
        fit_ols(year[start:i][mask], value_usd[start:i][mask])$slope_pct
      })
    ) |>
    ungroup()
}

# Bilateral HHI computed annually in DuckDB, broadcast to monthly spine
bilateral_hhi_monthly <- function(flow, where_sql, month_spine) {
  annual_hhi <- dbGetQuery(con, glue("
    WITH flows AS (
      SELECT t.iso3c AS country_code, t.yr AS year, t.partner_iso3c AS partner,
             SUM(t.constant_value_2024) AS value_usd
      FROM comtrade_raw t
      LEFT JOIN bronze.cmd_meta m ON t.cmd_code = m.cmd_code
      WHERE t.flow_desc = '{flow}' AND t.constant_value_2024 > 0
        AND t.partner_iso3c NOT IN ({wc_sql})
        AND {where_sql}
      GROUP BY t.iso3c, t.yr, t.partner_iso3c
    ),
    totals AS (
      SELECT country_code, year, SUM(value_usd) AS total
      FROM flows GROUP BY country_code, year
    )
    SELECT f.country_code, f.year,
      SUM(POWER(f.value_usd / t.total, 2)) AS bilateral_hhi,
      COUNT(DISTINCT f.partner)             AS n_partners,
      MAX(f.value_usd / t.total)            AS top_partner_share
    FROM flows f JOIN totals t USING (country_code, year)
    GROUP BY f.country_code, f.year
  ")) |>
    as_tibble() |>
    arrange(country_code, year) |>
    group_by(country_code) |>
    mutate(bilateral_hhi_delta3 = bilateral_hhi - lag(bilateral_hhi, 3)) |>
    ungroup()

  month_spine |> left_join(annual_hhi, by = c("country_code", "year"))
}

# Import unit prices: annual averages broadcast to monthly spine
import_price_monthly <- function(where_sql, month_spine) {
  annual_price <- dbGetQuery(con, glue("
    SELECT t.iso3c AS country_code, t.yr AS year, t.mon AS month,
      SUM(t.constant_value_2024) / NULLIF(SUM(t.qty_comtrade), 0) AS unit_price
    FROM comtrade_raw t
    LEFT JOIN bronze.cmd_meta m ON t.cmd_code = m.cmd_code
    WHERE t.flow_desc = 'Import' AND t.constant_value_2024 > 0
      AND t.qty_comtrade > 0 AND {where_sql}
    GROUP BY t.iso3c, t.yr, t.mon
  ")) |>
    as_tibble() |>
    group_by(country_code, year) |>
    summarise(
      price_avg_usd  = mean(unit_price, na.rm = TRUE),
      n_price_months = sum(!is.na(unit_price)),
      .groups = "drop"
    ) |>
    filter(n_price_months >= 4) |>
    arrange(country_code, year) |>
    group_by(country_code) |>
    mutate(
      price_yoy_pct = (price_avg_usd / lag(price_avg_usd) - 1) * 100,
      price_vol_cv  = map_dbl(row_number(), \(i) {
        start <- max(1L, i - TREND_YEARS + 1L)
        vals  <- price_avg_usd[start:i]
        if (sum(!is.na(vals)) < 3) return(NA_real_)
        m <- mean(vals, na.rm = TRUE)
        if (abs(m) < 1e-9) return(NA_real_)
        sd(vals, na.rm = TRUE) / abs(m)
      })
    ) |>
    ungroup() |>
    select(country_code, year, price_avg_usd, price_yoy_pct, price_vol_cv)

  month_spine |> left_join(annual_price, by = c("country_code", "year"))
}


# Setup: re-register parquet view

message("--- Comtrade silver: registering parquet view ---")

glob <- paste0(COMTRADE_PARQUET, "/**/*.parquet")
if (!file.exists(COMTRADE_PARQUET))
  stop("Comtrade parquet not found: ", COMTRADE_PARQUET)

dbExecute(con, glue("
  CREATE OR REPLACE VIEW comtrade_raw AS
  SELECT * FROM read_parquet('{glob}', hive_partitioning = TRUE, union_by_name = TRUE)
  WHERE yr >= {START_YR}
"))
message(sprintf("  %s rows available",
  format(dbGetQuery(con, "SELECT COUNT(*) AS n FROM comtrade_raw")$n, big.mark = ",")))


# Pre-compute WB Pink world price volatility (commodity-month level)
# price_wb sourced from bronze.price_lookup (written by ingest_price_series.R).
# Pulled here as a distinct commodity-month series -- country-invariant, so
# computed once and passed into compute_group_monthly as the wb_pv argument.

message("--- Comtrade silver: pre-computing WB Pink world price volatility ---")

wb_price_vol <- dbGetQuery(con, "
  SELECT m.commodity_name,
         p.yr AS year, p.mon AS month,
         AVG(p.price_wb) AS price_wb
  FROM bronze.price_lookup p
  JOIN bronze.cmd_meta m USING (cmd_code)
  WHERE p.price_wb IS NOT NULL AND p.price_wb > 0
  GROUP BY m.commodity_name, p.yr, p.mon
") |>
  as_tibble() |>
  arrange(commodity_name, year, month) |>
  group_by(commodity_name) |>
  mutate(
    ln_price     = log(price_wb),
    # 3-month moving average of log price to reduce reporting noise (M&R adaptation)
    ln_price_sm  = rollmean(ln_price, k = MR_SMOOTH_WIN, fill = NA, align = "right"),
    # Monthly first difference of smoothed log price
    dlnp         = ln_price_sm - lag(ln_price_sm, 1L),
    dlnp         = pmax(-3, pmin(3, dlnp)),
    # SD over prior 12 months, lagged 1 month so current month excluded (M&R: t-13 to t-1)
    price_vol_wb = lag(
      rollapply(dlnp, width = MR_VOL_WIN,
                FUN = function(v) sd(v, na.rm = TRUE),
                fill = NA, align = "right", partial = FALSE),
      1L)
  ) |>
  ungroup() |>
  filter(!is.na(price_vol_wb)) |>
  select(year, month, commodity_name, price_vol_wb)

message(sprintf("  WB Pink price vol: %d commodity-months | %d series",
  nrow(wb_price_vol), n_distinct(wb_price_vol$commodity_name)))

# Load mirror share from bronze (computed by ingest_comtrade.R)
# Joined to monthly outputs as a data-quality transparency field

message("--- Comtrade silver: loading mirror share ---")

mirror_export <- dbGetQuery(con, "
  SELECT country_code, year, month, mirror_share
  FROM bronze.comtrade_mirror_share
  WHERE flow_desc = 'Export'
") |> as_tibble()

mirror_import <- dbGetQuery(con, "
  SELECT country_code, year, month, mirror_share
  FROM bronze.comtrade_mirror_share
  WHERE flow_desc = 'Import'
") |> as_tibble()

message(sprintf("  mirror export: %d country-months | mirror import: %d country-months",
  nrow(mirror_export), nrow(mirror_import)))


# Single data pull with group flags attached
# MAX() handles commodity_names spanning cmd_codes with differing flag values


message("--- Comtrade silver: pulling monthly commodity data (all groups) ---")

monthly_all <- dbGetQuery(con, "
  SELECT
    t.iso3c AS country_code, t.yr AS year, t.mon AS month,
    t.flow_desc, t.commodity_name, t.category, t.subcategory,
    COALESCE(MAX(m.pink_sheet), 0)       AS pink_sheet,
    COALESCE(MAX(m.critical_mineral), 0) AS critical_mineral,
    SUM(t.constant_value_2024)           AS value_usd,
    SUM(t.qty_comtrade)                  AS qty_comtrade   -- for implicit unit price
  FROM comtrade_raw t
  LEFT JOIN bronze.cmd_meta m ON t.cmd_code = m.cmd_code
  WHERE t.constant_value_2024 > 0 AND t.commodity_name IS NOT NULL
  GROUP BY t.iso3c, t.yr, t.mon, t.flow_desc, t.commodity_name, t.category, t.subcategory
") |> as_tibble()

message(sprintf("  %s country-commodity-month records | %d countries",
  format(nrow(monthly_all), big.mark = ","), n_distinct(monthly_all$country_code)))

exports_raw <- monthly_all |> filter(flow_desc == "Export") |> select(-flow_desc)
imports_raw <- monthly_all |> filter(flow_desc == "Import") |> select(-flow_desc)

# Grand totals for value_share denominators
monthly_export_total <- exports_raw |>
  group_by(country_code, year, month) |>
  summarise(grand_usd = sum(value_usd, na.rm = TRUE), .groups = "drop")

monthly_import_total <- imports_raw |>
  group_by(country_code, year, month) |>
  summarise(grand_usd = sum(value_usd, na.rm = TRUE), .groups = "drop")

annual_export_total <- monthly_export_total |>
  group_by(country_code, year) |>
  summarise(grand_usd = sum(grand_usd, na.rm = TRUE), .groups = "drop")

annual_import_total <- monthly_import_total |>
  group_by(country_code, year) |>
  summarise(grand_usd = sum(grand_usd, na.rm = TRUE), .groups = "drop")


# SILVER -- export groups


message("--- Comtrade silver: export groups ---")

export_monthly_list <- list()
export_annual_list  <- list()

for (grp in names(GROUPS)) {
  g <- GROUPS[[grp]]
  message(sprintf("  [%s] exports...", grp))

  monthly_grp <- g$where_r(exports_raw) |>
    select(country_code, year, month, commodity_name, value_usd, qty_comtrade)

  monthly <- compute_group_monthly(monthly_grp, wb_pv = wb_price_vol)
  if (nrow(monthly) == 0) next

  month_spine <- monthly |> distinct(country_code, year, month)

  phhi <- bilateral_hhi_monthly("Export", g$where_sql, month_spine) |>
    rename(partner_hhi        = bilateral_hhi,
           partner_hhi_delta3 = bilateral_hhi_delta3)

  annual_shock <- monthly |>
    group_by(country_code, year) |>
    summarise(
      shock_months_yr = as.integer(sum(shock, na.rm = TRUE)),
      shock_flag      = as.integer(any(shock, na.rm = TRUE)),
      .groups = "drop"
    )

  # Monthly: join partner HHI, mirror share, grand total
  export_monthly_list[[grp]] <- monthly |>
    left_join(phhi,                 by = c("country_code", "year", "month")) |>
    left_join(mirror_export,        by = c("country_code", "year", "month")) |>
    left_join(monthly_export_total, by = c("country_code", "year", "month")) |>
    mutate(value_share = value_usd / grand_usd, commodity_group = grp) |>
      mutate(
    commodity_hhi = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(commodity_hhi)),
    n_active_commodities = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(n_active_commodities)),
    top_commodity_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top_commodity_share)),
    top3_commodity_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top3_commodity_share)),
    partner_hhi = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(partner_hhi)),
    partner_hhi_delta3 = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(partner_hhi_delta3)),
    n_partners = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(n_partners)),
    top_partner_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top_partner_share))
  ) |>
    select(-grand_usd)

  # Annual: Dec snapshot for rolling metrics, annual sum for value
  annual <- monthly_to_annual(monthly) |> add_ols_trend()

  annual_phhi <- phhi |>
    filter(month == 12) |>
    select(country_code, year,
           partner_hhi, partner_hhi_delta3, n_partners, top_partner_share)

  # Mirror share: annual average (mean of monthly values for the year)
  annual_mirror_exp <- mirror_export |>
    group_by(country_code, year) |>
    summarise(mirror_share_annual = mean(mirror_share, na.rm = TRUE), .groups = "drop")

  export_annual_list[[grp]] <- annual |>
    left_join(annual_phhi,         by = c("country_code", "year")) |>
    left_join(annual_shock,        by = c("country_code", "year")) |>
    left_join(annual_mirror_exp,   by = c("country_code", "year")) |>
    left_join(annual_export_total, by = c("country_code", "year")) |>
    mutate(value_share = value_usd / grand_usd, commodity_group = grp) |>
      mutate(
    commodity_hhi = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(commodity_hhi)),
    n_active_commodities = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(n_active_commodities)),
    top_commodity_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top_commodity_share)),
    top3_commodity_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top3_commodity_share)),
    partner_hhi = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(partner_hhi)),
    partner_hhi_delta3 = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(partner_hhi_delta3)),
    n_partners = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(n_partners)),
    top_partner_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top_partner_share))
  ) |>
    select(-grand_usd)
}

# Column ordering
col_monthly_exp <- c(
  "country_code", "year", "month", "commodity_group",
  "value_usd", "value_share", "mirror_share",
  "cv_unweighted", "cv_weighted", "cv_basket_wtd", "cv_basket_wb_share",
  "commodity_hhi", "n_active_commodities", "top_commodity_share", "top3_commodity_share",
  "var_05", "var_10", "cvar_05",
  "max_drawdown", "current_drawdown", "neg_return_freq",
  "partner_hhi", "partner_hhi_delta3", "n_partners", "top_partner_share",
  "shock"
)
col_annual_exp <- c(
  "country_code", "year", "commodity_group",
  "value_usd", "value_share", "mirror_share_annual",
  "cv_unweighted", "cv_weighted", "cv_basket_wtd", "cv_basket_wb_share",
  "commodity_hhi", "n_active_commodities", "top_commodity_share", "top3_commodity_share",
  "trend_slope", "trend_pct",
  "var_05", "var_10", "cvar_05",
  "max_drawdown", "current_drawdown", "neg_return_freq",
  "partner_hhi", "partner_hhi_delta3", "n_partners", "top_partner_share",
  "shock_months_yr", "shock_flag"
)

comtrade_export_groups_monthly <- bind_rows(export_monthly_list) |>
  filter(year >= TS_START_YR) |>
  select(all_of(col_monthly_exp))

comtrade_export_groups_ts <- bind_rows(export_annual_list) |>
  filter(year >= TS_START_YR) |>
  select(all_of(col_annual_exp))

write_silver(comtrade_export_groups_monthly, "comtrade_export_groups_monthly", con)
write_silver(comtrade_export_groups_ts,      "comtrade_export_groups_ts",      con)


# SILVER -- import groups


message("--- Comtrade silver: import groups ---")

import_monthly_list <- list()
import_annual_list  <- list()

for (grp in names(GROUPS)) {
  g <- GROUPS[[grp]]
  message(sprintf("  [%s] imports...", grp))

  monthly_grp <- g$where_r(imports_raw) |>
    select(country_code, year, month, commodity_name, value_usd, qty_comtrade)

  monthly <- compute_group_monthly(monthly_grp, wb_pv = wb_price_vol)
  if (nrow(monthly) == 0) next

  month_spine <- monthly |> distinct(country_code, year, month)

  shhi <- bilateral_hhi_monthly("Import", g$where_sql, month_spine) |>
    rename(supplier_hhi        = bilateral_hhi,
           supplier_hhi_delta3 = bilateral_hhi_delta3,
           n_suppliers         = n_partners,
           top_supplier_share  = top_partner_share)

  price <- import_price_monthly(g$where_sql, month_spine)

  annual_shock <- monthly |>
    group_by(country_code, year) |>
    summarise(
      shock_months_yr = as.integer(sum(shock, na.rm = TRUE)),
      shock_flag      = as.integer(any(shock, na.rm = TRUE)),
      .groups = "drop"
    )

  # Monthly: join supplier HHI, price, mirror share, grand total
  import_monthly_list[[grp]] <- monthly |>
    left_join(shhi,                 by = c("country_code", "year", "month")) |>
    left_join(price,                by = c("country_code", "year", "month")) |>
    left_join(mirror_import,        by = c("country_code", "year", "month")) |>
    left_join(monthly_import_total, by = c("country_code", "year", "month")) |>
    mutate(value_share = value_usd / grand_usd, commodity_group = grp) |>
      mutate(
    commodity_hhi = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(commodity_hhi)),
    n_active_commodities = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(n_active_commodities)),
    top_commodity_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top_commodity_share)),
    top3_commodity_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top3_commodity_share)),
    supplier_hhi = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(supplier_hhi)),
    supplier_hhi_delta3 = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(supplier_hhi_delta3)),
    n_suppliers = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(n_suppliers)),
    top_supplier_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top_supplier_share))
  ) |>
    select(-grand_usd)

  # Annual
  annual <- monthly_to_annual(monthly) |> add_ols_trend()

  annual_shhi <- shhi |>
    filter(month == 12) |>
    select(country_code, year,
           supplier_hhi, supplier_hhi_delta3, n_suppliers, top_supplier_share)

  annual_price <- price |>
    filter(month == 12) |>
    select(country_code, year, price_avg_usd, price_yoy_pct, price_vol_cv)

  annual_mirror_imp <- mirror_import |>
    group_by(country_code, year) |>
    summarise(mirror_share_annual = mean(mirror_share, na.rm = TRUE), .groups = "drop")

  import_annual_list[[grp]] <- annual |>
    left_join(annual_shhi,         by = c("country_code", "year")) |>
    left_join(annual_price,        by = c("country_code", "year")) |>
    left_join(annual_shock,        by = c("country_code", "year")) |>
    left_join(annual_mirror_imp,   by = c("country_code", "year")) |>
    left_join(annual_import_total, by = c("country_code", "year")) |>
    mutate(value_share = value_usd / grand_usd, commodity_group = grp) |>
      mutate(
    commodity_hhi = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(commodity_hhi)),
    n_active_commodities = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(n_active_commodities)),
    top_commodity_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top_commodity_share)),
    top3_commodity_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top3_commodity_share)),
    supplier_hhi = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(supplier_hhi)),
    supplier_hhi_delta3 = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(supplier_hhi_delta3)),
    n_suppliers = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(n_suppliers)),
    top_supplier_share = if_else(value_share < MIN_VALUE_SHARE, NA_real_, as.double(top_supplier_share))
  ) |>
    select(-grand_usd)
}

col_monthly_imp <- c(
  "country_code", "year", "month", "commodity_group",
  "value_usd", "value_share", "mirror_share",
  "cv_unweighted", "cv_weighted", "cv_basket_wtd", "cv_basket_wb_share",
  "commodity_hhi", "n_active_commodities", "top_commodity_share", "top3_commodity_share",
  "var_05", "var_10", "cvar_05",
  "max_drawdown", "current_drawdown", "neg_return_freq",
  "supplier_hhi", "supplier_hhi_delta3", "n_suppliers", "top_supplier_share",
  "price_avg_usd", "price_yoy_pct", "price_vol_cv",
  "shock"
)
col_annual_imp <- c(
  "country_code", "year", "commodity_group",
  "value_usd", "value_share", "mirror_share_annual",
  "cv_unweighted", "cv_weighted", "cv_basket_wtd", "cv_basket_wb_share",
  "commodity_hhi", "n_active_commodities", "top_commodity_share", "top3_commodity_share",
  "trend_slope", "trend_pct",
  "var_05", "var_10", "cvar_05",
  "max_drawdown", "current_drawdown", "neg_return_freq",
  "supplier_hhi", "supplier_hhi_delta3", "n_suppliers", "top_supplier_share",
  "price_avg_usd", "price_yoy_pct", "price_vol_cv",
  "shock_months_yr", "shock_flag"
)

comtrade_import_groups_monthly <- bind_rows(import_monthly_list) |>
  filter(year >= TS_START_YR) |>
  select(all_of(col_monthly_imp))

comtrade_import_groups_ts <- bind_rows(import_annual_list) |>
  filter(year >= TS_START_YR) |>
  select(all_of(col_annual_imp))

write_silver(comtrade_import_groups_monthly, "comtrade_import_groups_monthly", con)
write_silver(comtrade_import_groups_ts,      "comtrade_import_groups_ts",      con)

==
# D2 IMPORT DIVERSITY TABLES
# Pre-pivoted, direction-corrected wide tables for the D2 Bayesian model.
# Computed here from already-built import group outputs so D2 scripts do
# a single clean pull with no transformation work.
#
# diversity = 1 - supplier_hhi  (higher = more diversified = more resilient)
# supplier_hhi from bilateral_hhi_monthly() is in [0, 1] (proportions squared).
#
# silver.d2_import_hhi_monthly  country-year-month  food + energy + cereal + fertilizer
# silver.d2_import_hii_annual   country-year        food + energy + cereal + fertilizer
==

message("--- Comtrade silver: building D2 import diversity tables ---")

# Pivot both supplier_hhi and value_share to distinguish two NA types:
#   value_share NA          -> no Comtrade record -> genuinely unknown -> NA
#   value_share < threshold -> trivial trade exists -> no concentration -> 1.0
# Countries with at least one record across all years for each group
has_energy_history <- comtrade_import_groups_ts |>
  filter(commodity_group == "energy", !is.na(value_share)) |>
  distinct(country_code) |>
  mutate(has_energy = TRUE)
has_food_history <- comtrade_import_groups_ts |>
  filter(commodity_group == "food", !is.na(value_share)) |>
  distinct(country_code) |>
  mutate(has_food = TRUE)
has_cereal_history <- comtrade_import_groups_ts |>
  filter(commodity_group == "cereal", !is.na(value_share)) |>
  distinct(country_code) |>
  mutate(has_cereal = TRUE)
has_fertilizer_history <- comtrade_import_groups_ts |>
  filter(commodity_group == "fertilizer", !is.na(value_share)) |>
  distinct(country_code) |>
  mutate(has_fertilizer = TRUE)

# history flags computed once above -- used by both monthly and annual blocks
d2_import_hhi_monthly <- comtrade_import_groups_monthly |>
  filter(commodity_group %in% c("food", "energy", "cereal", "fertilizer")) |>
  select(country_code, year, month, commodity_group, supplier_hhi, value_share) |>
  pivot_wider(names_from = commodity_group,
              values_from = c(supplier_hhi, value_share)) |>
  left_join(has_energy_history,     by = "country_code") |>
  left_join(has_food_history,       by = "country_code") |>
  left_join(has_cereal_history,     by = "country_code") |>
  left_join(has_fertilizer_history, by = "country_code") |>
  mutate(
    diversity_food = case_when(
      value_share_food   < MIN_VALUE_SHARE ~ 1.0,
      is.na(value_share_food) & is.na(has_food)    ~ 1.0,
      is.na(value_share_food)              ~ NA_real_,
      is.na(supplier_hhi_food)             ~ NA_real_,
      TRUE                                 ~ 1 - supplier_hhi_food
    ),
    diversity_energy = case_when(
      value_share_energy < MIN_VALUE_SHARE ~ 1.0,
      is.na(value_share_energy) & is.na(has_energy)  ~ 1.0,
      is.na(value_share_energy)            ~ NA_real_,
      is.na(supplier_hhi_energy)           ~ NA_real_,
      TRUE                                 ~ 1 - supplier_hhi_energy
    ),
    diversity_cereal = case_when(
      value_share_cereal < MIN_VALUE_SHARE ~ 1.0,
      is.na(value_share_cereal) & is.na(has_cereal)  ~ 1.0,
      is.na(value_share_cereal)            ~ NA_real_,
      is.na(supplier_hhi_cereal)           ~ NA_real_,
      TRUE                                 ~ 1 - supplier_hhi_cereal
    ),
    diversity_fertilizer = case_when(
      value_share_fertilizer < MIN_VALUE_SHARE ~ 1.0,
      is.na(value_share_fertilizer) & is.na(has_fertilizer) ~ 1.0,
      is.na(value_share_fertilizer)            ~ NA_real_,
      is.na(supplier_hhi_fertilizer)           ~ NA_real_,
      TRUE                                     ~ 1 - supplier_hhi_fertilizer
    )
  ) |>
  select(country_code, year, month,
         diversity_food, diversity_energy, diversity_cereal, diversity_fertilizer)

write_silver(d2_import_hhi_monthly, "d2_import_hhi_monthly", con)

# Annual: December snapshot of monthly rolling HHI for all four groups.

d2_import_hhi_annual <- comtrade_import_groups_ts |>
  filter(commodity_group %in% c("food", "energy", "cereal", "fertilizer")) |>
  select(country_code, year, commodity_group, supplier_hhi, value_share) |>
  pivot_wider(names_from = commodity_group,
              values_from = c(supplier_hhi, value_share)) |>
  left_join(has_energy_history,     by = "country_code") |>
  left_join(has_food_history,       by = "country_code") |>
  left_join(has_cereal_history,     by = "country_code") |>
  left_join(has_fertilizer_history, by = "country_code") |>
  mutate(
    diversity_food = case_when(
      value_share_food   < MIN_VALUE_SHARE ~ 1.0,          # trivial trade
      is.na(value_share_food) & is.na(has_food)    ~ 1.0,  # never reported
      is.na(value_share_food)              ~ NA_real_,     # gap in history
      is.na(supplier_hhi_food)             ~ NA_real_,     # bilateral gap
      TRUE                                 ~ 1 - supplier_hhi_food
    ),
    diversity_energy = case_when(
      value_share_energy < MIN_VALUE_SHARE ~ 1.0,
      is.na(value_share_energy) & is.na(has_energy)  ~ 1.0,
      is.na(value_share_energy)            ~ NA_real_,
      is.na(supplier_hhi_energy)           ~ NA_real_,
      TRUE                                 ~ 1 - supplier_hhi_energy
    ),
    diversity_cereal = case_when(
      value_share_cereal < MIN_VALUE_SHARE ~ 1.0,
      is.na(value_share_cereal) & is.na(has_cereal)  ~ 1.0,
      is.na(value_share_cereal)            ~ NA_real_,
      is.na(supplier_hhi_cereal)           ~ NA_real_,
      TRUE                                 ~ 1 - supplier_hhi_cereal
    ),
    diversity_fertilizer = case_when(
      value_share_fertilizer < MIN_VALUE_SHARE ~ 1.0,
      is.na(value_share_fertilizer) & is.na(has_fertilizer) ~ 1.0,
      is.na(value_share_fertilizer)            ~ NA_real_,
      is.na(supplier_hhi_fertilizer)           ~ NA_real_,
      TRUE                                     ~ 1 - supplier_hhi_fertilizer
    )
  ) |>
  select(country_code, year,
         diversity_food, diversity_energy, diversity_cereal, diversity_fertilizer)

write_silver(d2_import_hhi_annual, "d2_import_hhi_annual", con)

==
# D3 EXPORT FRAGILITY TABLES
# Pre-pivoted, direction-corrected wide tables for the D3 Bayesian model.
# All three indicators derived from the pink_sheet export group -- the broadest
# commodity lens for export revenue vulnerability.
#
# diversity_partner   = 1 - partner_hhi    (higher = buyers more diversified)
# diversity_commodity = 1 - commodity_hhi  (higher = export basket more diversified)
# export_independence = 1 - value_share    (higher = less reliant on pink-sheet exports)
#
# silver.d3_export_fragility_monthly  country-year-month
# silver.d3_export_fragility_annual   country-year
==

message("--- Comtrade silver: building D3 export fragility tables ---")

d3_export_fragility_monthly <- comtrade_export_groups_monthly |>
  filter(commodity_group == "pink_sheet") |>
  select(country_code, year, month,
         partner_hhi, commodity_hhi, value_share) |>
  mutate(
    # partner_hhi / commodity_hhi nulled upstream when value_share < MIN_VALUE_SHARE.
    # Trivial pink_sheet exports = no concentration risk = 1.0, not unknown.
    # Genuine gap (no bilateral data despite non-trivial value_share) stays NA.
    diversity_partner   = case_when(
      is.na(value_share)            ~ 1.0,
      value_share < MIN_VALUE_SHARE ~ 1.0,
      is.na(partner_hhi)            ~ NA_real_,
      TRUE                          ~ 1 - partner_hhi
    ),
    diversity_commodity = case_when(
      is.na(value_share)            ~ 1.0,
      value_share < MIN_VALUE_SHARE ~ 1.0,
      is.na(commodity_hhi)          ~ NA_real_,
      TRUE                          ~ 1 - commodity_hhi
    ),
    export_independence = if_else(is.na(value_share), 1.0, 1 - value_share)
  ) |>
  select(country_code, year, month,
         diversity_partner, diversity_commodity, export_independence)

write_silver(d3_export_fragility_monthly, "d3_export_fragility_monthly", con)

d3_export_fragility_annual <- comtrade_export_groups_ts |>
  filter(commodity_group == "pink_sheet") |>
  select(country_code, year,
         partner_hhi, commodity_hhi, value_share) |>
  mutate(
    diversity_partner   = case_when(
      is.na(value_share)            ~ 1.0,
      value_share < MIN_VALUE_SHARE ~ 1.0,
      is.na(partner_hhi)            ~ NA_real_,
      TRUE                          ~ 1 - partner_hhi
    ),
    diversity_commodity = case_when(
      is.na(value_share)            ~ 1.0,
      value_share < MIN_VALUE_SHARE ~ 1.0,
      is.na(commodity_hhi)          ~ NA_real_,
      TRUE                          ~ 1 - commodity_hhi
    ),
    export_independence = if_else(is.na(value_share), 1.0, 1 - value_share)
  ) |>
  select(country_code, year,
         diversity_partner, diversity_commodity, export_independence)

write_silver(d3_export_fragility_annual, "d3_export_fragility_annual", con)

==
# MONTHLY DISRUPTION INDICATORS -- D2 (food imports) + D3 (pink_sheet exports)
#
# These feed compute_bayes_d2_monthly.R and compute_bayes_d3_monthly.R.
# HHI is retained in the group tables above for annual models and the
# context layer; disruption indicators are the monthly signal layer.
#
# Same-month seasonal baseline: DuckDB window function over all prior years
# for the same calendar month, with 1-year lag enforced via ROWS...1 PRECEDING.
# This is equivalent to same_month_z() in R but runs entirely in DuckDB.
#
# silver.monthly_disruption_d2  country-year-month
#   import_vol_anomaly_z       mean/SD z-score of food import value vs prior-year same months (winsorized +/-Z_CAP)
#   import_vol_anomaly_z_r     robust z-score: same baseline, median/MAD instead of mean/SD
#   import_basket_price_z      mean/SD z-score of WB-price-weighted food import basket vs prior years (winsorized)
#   import_basket_price_z_r    robust z-score: same basket, median/MAD baseline
#   partner_hhi_shift_3mo      3-month change in bilateral supplier HHI (acute shift)
#   partner_hhi_shift_6mo      6-month change in bilateral supplier HHI (structural drift)
#   n_major_suppliers          partners with trailing 12m share > 10%
#   rupture_count              major suppliers with zero flows this month
#   rupture_share              rupture_count / n_major_suppliers (0-1)
#
# silver.monthly_disruption_d3  country-year-month
#   export_vol_anomaly_z       mean/SD z-score of pink_sheet export value vs prior years (winsorized +/-Z_CAP)
#   export_vol_anomaly_z_r     robust z-score: same baseline, median/MAD instead of mean/SD
#   export_basket_price_z      mean/SD z-score of WB-price-weighted export basket vs prior years (winsorized)
#   export_basket_price_z_r    robust z-score: same basket, median/MAD baseline
#   cv_basket_wtd              Morgan-Reinhardt weighted basket price vol: SD of 3-month
#                              smoothed monthly log price first differences, 12-month window
#                              lagged 1 month, weighted by lagged export/import value shares
==

message("--- Comtrade silver: monthly disruption indicators ---")

==
# D2 -- IMPORT DISRUPTION (food + energy, six indicators)
#
# Replaces prior single-category approach with separate food and energy signals:
#   food_vol_anomaly_z       z-score of food import value vs same-month baseline
#   food_price_z             z-score of WB-price-weighted food import basket
#   energy_price_z           z-score of WB-price-weighted energy import basket
#   food_cv_basket_wtd       Morgan-Reinhardt weighted price vol -- food imports
#   energy_cv_basket_wtd     Morgan-Reinhardt weighted price vol -- energy imports
#   fertilizer_cv_basket_wtd Morgan-Reinhardt weighted price vol -- fertilizer imports
#
# Vol/price z-scores use same-month seasonal baseline; winsorized at +/-Z_CAP.
# Basket volatility pulled from pre-computed import_monthly_list.
==

# Helper: volume anomaly z-score for a given category SQL filter
vol_anomaly_sql <- function(category_sql, output_col) {
  dbGetQuery(con, glue("
    WITH monthly AS (
      SELECT t.iso3c AS country_code, t.yr AS year, t.mon AS month,
             SUM(t.constant_value_2024) AS value_usd
      FROM comtrade_raw t
      LEFT JOIN bronze.cmd_meta m ON t.cmd_code = m.cmd_code
      WHERE t.flow_desc = 'Import' AND t.constant_value_2024 > 0
        AND {category_sql}
      GROUP BY t.iso3c, t.yr, t.mon
    ),
    stats AS (
      SELECT country_code, year, month, value_usd,
        AVG(value_usd)         OVER (PARTITION BY country_code, month
                                     ORDER BY year
                                     ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS baseline_mean,
        STDDEV_SAMP(value_usd) OVER (PARTITION BY country_code, month
                                     ORDER BY year
                                     ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS baseline_sd,
        COUNT(value_usd)       OVER (PARTITION BY country_code, month
                                     ORDER BY year
                                     ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS n_prior
      FROM monthly
    )
    SELECT country_code, year, month,
      CASE WHEN n_prior >= {BASELINE_MIN_N} AND baseline_sd > 0
           THEN (value_usd - baseline_mean) / baseline_sd
           ELSE NULL END AS {output_col}
    FROM stats
    ORDER BY country_code, year, month
  ")) |>
    as_tibble() |>
    mutate(across(all_of(output_col), ~ pmax(-Z_CAP, pmin(Z_CAP, .x))))
}

# Helper: WB-price-weighted basket price z-score for a given category SQL filter
basket_price_sql <- function(category_sql, output_col) {
  dbGetQuery(con, glue("
    WITH commodity_monthly AS (
      SELECT t.iso3c AS country_code, t.yr AS year, t.mon AS month,
             m.commodity_name,
             SUM(t.constant_value_2024) AS value_usd
      FROM comtrade_raw t
      LEFT JOIN bronze.cmd_meta m ON t.cmd_code = m.cmd_code
      WHERE t.flow_desc = 'Import' AND t.constant_value_2024 > 0
        AND {category_sql}
        AND t.constant_value_2024 >= {CMD_MIN_USD} / 12.0
      GROUP BY t.iso3c, t.yr, t.mon, m.commodity_name
    ),
    wb_prices AS (
      SELECT m.commodity_name, p.yr AS year, p.mon AS month,
             AVG(p.price_wb) AS price_wb
      FROM bronze.price_lookup p
      JOIN bronze.cmd_meta m USING (cmd_code)
      WHERE p.price_wb IS NOT NULL AND p.price_wb > 0
      GROUP BY m.commodity_name, p.yr, p.mon
    ),
    r12_shares AS (
      SELECT c.country_code, c.commodity_name, c.year, c.month,
             w.price_wb,
        SUM(c.value_usd) OVER (PARTITION BY c.country_code, c.commodity_name
                               ORDER BY c.year * 12 + c.month
                               ROWS BETWEEN 11 PRECEDING AND CURRENT ROW) AS r12_value
      FROM commodity_monthly c
      LEFT JOIN wb_prices w ON c.commodity_name = w.commodity_name
                            AND c.year = w.year AND c.month = w.month
    ),
    country_r12 AS (
      SELECT country_code, year, month, SUM(r12_value) AS r12_total
      FROM r12_shares GROUP BY country_code, year, month
    ),
    basket AS (
      SELECT r.country_code, r.year, r.month,
        SUM(r.price_wb * r.r12_value / NULLIF(c.r12_total, 0)) AS basket_price
      FROM r12_shares r JOIN country_r12 c USING (country_code, year, month)
      WHERE r.price_wb IS NOT NULL
      GROUP BY r.country_code, r.year, r.month
    ),
    stats AS (
      SELECT country_code, year, month, basket_price,
        AVG(basket_price)         OVER (PARTITION BY country_code, month
                                        ORDER BY year
                                        ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS baseline_mean,
        STDDEV_SAMP(basket_price) OVER (PARTITION BY country_code, month
                                        ORDER BY year
                                        ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS baseline_sd,
        COUNT(basket_price)       OVER (PARTITION BY country_code, month
                                        ORDER BY year
                                        ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS n_prior
      FROM basket
    )
    SELECT country_code, year, month,
      CASE WHEN n_prior >= {BASELINE_MIN_N} AND baseline_sd > 0
           THEN (basket_price - baseline_mean) / baseline_sd
           ELSE NULL END AS {output_col}
    FROM stats
    ORDER BY country_code, year, month
  ")) |>
    as_tibble() |>
    mutate(across(all_of(output_col), ~ pmax(-Z_CAP, pmin(Z_CAP, .x))))
}

# -- D2.1/D2.2 Food volume and price anomalies ---------------------------------
message("  [D2] food import volume anomaly...")
food_vol_anom <- vol_anomaly_sql("t.category = 'Food'", "food_vol_anomaly_z")

message("  [D2] food import basket price shock...")
food_price_z  <- basket_price_sql("t.category = 'Food'", "food_price_z")

# -- D2.3 Energy price anomaly -------------------------------------------------
message("  [D2] energy import basket price shock...")
energy_price_z  <- basket_price_sql("t.category = 'Energy'", "energy_price_z")

# -- D2.5/D2.6 Basket price volatility (Morgan-Reinhardt) ----------------------
# cv_basket_wtd already computed by compute_group_monthly for food and fertilizer.
# Pull directly from import_monthly_list -- no recomputation needed.
message("  [D2] food and fertilizer basket price volatility...")

food_cv <- import_monthly_list[["food"]] |>
  select(country_code, year, month, food_cv_basket_wtd = cv_basket_wtd)

energy_cv <- import_monthly_list[["energy"]] |>
  select(country_code, year, month, energy_cv_basket_wtd = cv_basket_wtd)

fertilizer_cv <- import_monthly_list[["fertilizer"]] |>
  select(country_code, year, month, fertilizer_cv_basket_wtd = cv_basket_wtd)

# -- D2 join and write ---------------------------------------------------------

spine_d2 <- bind_rows(
  food_vol_anom |> select(country_code, year, month),
  food_price_z  |> select(country_code, year, month),
  energy_price_z |> select(country_code, year, month),
  food_cv        |> select(country_code, year, month),
  energy_cv      |> select(country_code, year, month),
  fertilizer_cv  |> select(country_code, year, month)
) |> distinct()

monthly_disruption_d2 <- spine_d2 |>
  left_join(food_vol_anom,  by = c("country_code", "year", "month")) |>
  left_join(food_price_z,   by = c("country_code", "year", "month")) |>
  left_join(energy_price_z, by = c("country_code", "year", "month")) |>
  left_join(food_cv,        by = c("country_code", "year", "month")) |>
  left_join(energy_cv,      by = c("country_code", "year", "month")) |>
  left_join(fertilizer_cv,  by = c("country_code", "year", "month")) |>
  filter(year >= TS_START_YR) |>
  arrange(country_code, year, month)

write_silver(monthly_disruption_d2, "monthly_disruption_d2", con)

==
# D3 -- PINK SHEET EXPORT DISRUPTION
==

# -- D3.1 Volume anomaly -------------------------------------------------------

message("  [D3] pink_sheet export volume anomaly...")

vol_anom_d3 <- dbGetQuery(con, glue("
  WITH monthly AS (
    SELECT t.iso3c AS country_code, t.yr AS year, t.mon AS month,
           SUM(t.constant_value_2024) AS value_usd
    FROM comtrade_raw t
    LEFT JOIN bronze.cmd_meta m ON t.cmd_code = m.cmd_code
    WHERE t.flow_desc = 'Export' AND t.constant_value_2024 > 0
      AND COALESCE(m.pink_sheet, 0) = 1
    GROUP BY t.iso3c, t.yr, t.mon
  ),
  stats AS (
    SELECT country_code, year, month, value_usd,
      AVG(value_usd)    OVER (PARTITION BY country_code, month
                              ORDER BY year
                              ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS baseline_mean,
      STDDEV_SAMP(value_usd) OVER (PARTITION BY country_code, month
                              ORDER BY year
                              ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS baseline_sd,
      COUNT(value_usd)  OVER (PARTITION BY country_code, month
                              ORDER BY year
                              ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS n_prior
    FROM monthly
  )
  SELECT country_code, year, month, value_usd,
    CASE WHEN n_prior >= {BASELINE_MIN_N} AND baseline_sd > 0
         THEN (value_usd - baseline_mean) / baseline_sd
         ELSE NULL END AS export_vol_anomaly_z
  FROM stats
  ORDER BY country_code, year, month
")) |>
  as_tibble() |>
  mutate(export_vol_anomaly_z = pmax(-Z_CAP, pmin(Z_CAP, export_vol_anomaly_z))) |>
  add_robust_z("value_usd", "export_vol_anomaly_z_r") |>
  select(-value_usd)

# -- D3.2 Export basket price shock --------------------------------------------

message("  [D3] pink_sheet export basket price shock...")

basket_price_d3 <- dbGetQuery(con, glue("
  -- Uses WB Pink Sheet world prices (price_wb) weighted by each country's
  -- rolling 12m export shares. Avoids Comtrade implied unit price noise
  -- (value/qty) which produces extreme z-scores for sparse exporters.
  WITH commodity_monthly AS (
    SELECT t.iso3c AS country_code, t.yr AS year, t.mon AS month,
           m.commodity_name,
           SUM(t.constant_value_2024) AS value_usd
    FROM comtrade_raw t
    LEFT JOIN bronze.cmd_meta m ON t.cmd_code = m.cmd_code
    WHERE t.flow_desc = 'Export' AND t.constant_value_2024 > 0
      AND COALESCE(m.pink_sheet, 0) = 1
      AND t.constant_value_2024 >= {CMD_MIN_USD} / 12.0
    GROUP BY t.iso3c, t.yr, t.mon, m.commodity_name
  ),
  -- WB world price per commodity-month (country-invariant)
  wb_prices AS (
    SELECT m.commodity_name, p.yr AS year, p.mon AS month,
           AVG(p.price_wb) AS price_wb
    FROM bronze.price_lookup p
    JOIN bronze.cmd_meta m USING (cmd_code)
    WHERE p.price_wb IS NOT NULL AND p.price_wb > 0
    GROUP BY m.commodity_name, p.yr, p.mon
  ),
  r12_shares AS (
    SELECT c.country_code, c.commodity_name, c.year, c.month,
           w.price_wb,
      SUM(c.value_usd) OVER (PARTITION BY c.country_code, c.commodity_name
                             ORDER BY c.year * 12 + c.month
                             ROWS BETWEEN 11 PRECEDING AND CURRENT ROW) AS r12_value
    FROM commodity_monthly c
    LEFT JOIN wb_prices w ON c.commodity_name = w.commodity_name
                          AND c.year = w.year AND c.month = w.month
  ),
  country_r12 AS (
    SELECT country_code, year, month, SUM(r12_value) AS r12_total
    FROM r12_shares GROUP BY country_code, year, month
  ),
  basket AS (
    SELECT r.country_code, r.year, r.month,
      SUM(r.price_wb * r.r12_value / NULLIF(c.r12_total, 0)) AS basket_price
    FROM r12_shares r JOIN country_r12 c USING (country_code, year, month)
    WHERE r.price_wb IS NOT NULL
    GROUP BY r.country_code, r.year, r.month
  ),
  stats AS (
    SELECT country_code, year, month, basket_price,
      AVG(basket_price)    OVER (PARTITION BY country_code, month
                                 ORDER BY year
                                 ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS baseline_mean,
      STDDEV_SAMP(basket_price) OVER (PARTITION BY country_code, month
                                 ORDER BY year
                                 ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS baseline_sd,
      COUNT(basket_price)  OVER (PARTITION BY country_code, month
                                 ORDER BY year
                                 ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS n_prior
    FROM basket
  )
  SELECT country_code, year, month, basket_price,
    CASE WHEN n_prior >= {BASELINE_MIN_N} AND baseline_sd > 0
         THEN (basket_price - baseline_mean) / baseline_sd
         ELSE NULL END AS export_basket_price_z
  FROM stats
  ORDER BY country_code, year, month
")) |>
  as_tibble() |>
  mutate(export_basket_price_z = pmax(-Z_CAP, pmin(Z_CAP, export_basket_price_z))) |>
  add_robust_z("basket_price", "export_basket_price_z_r") |>
  select(-basket_price)

# -- D3.3 Basket price volatility (Morgan-Reinhardt weighted SD) ---------------
# Pull cv_basket_wtd from pink_sheet export groups already computed above.
# Replaces the 6m rolling CV measures (export_revenue_cv_6mo, export_price_cv_6mo)
# with the theoretically grounded 12m log-price-change volatility measure.

message("  [D3] basket price volatility (cv_basket_wtd from pink_sheet exports)...")

cv_basket_d3 <- export_monthly_list[["pink_sheet"]] |>
  select(country_code, year, month, cv_basket_wtd)

# -- D3 join and write ---------------------------------------------------------

spine_d3 <- bind_rows(
  vol_anom_d3    |> select(country_code, year, month),
  basket_price_d3 |> select(country_code, year, month),
  cv_basket_d3   |> select(country_code, year, month)
) |> distinct()

monthly_disruption_d3 <- spine_d3 |>
  left_join(vol_anom_d3,     by = c("country_code", "year", "month")) |>
  left_join(basket_price_d3, by = c("country_code", "year", "month")) |>
  left_join(cv_basket_d3,    by = c("country_code", "year", "month")) |>
  filter(year >= TS_START_YR) |>
  arrange(country_code, year, month)

write_silver(monthly_disruption_d3, "monthly_disruption_d3", con)

# -- Cleanup -----------------------------------------------------------------

dbExecute(con, "DROP VIEW IF EXISTS comtrade_raw")

# -- Summary -----------------------------------------------------------------

recent_yr <- max(comtrade_export_groups_ts$year, na.rm = TRUE)

message("--- Comtrade silver complete ---")
for (tbl in list(
  list(name = "comtrade_export_groups_monthly", df = comtrade_export_groups_monthly),
  list(name = "comtrade_export_groups_ts",      df = comtrade_export_groups_ts),
  list(name = "comtrade_import_groups_monthly", df = comtrade_import_groups_monthly),
  list(name = "comtrade_import_groups_ts",      df = comtrade_import_groups_ts),
  list(name = "monthly_disruption_d2",          df = monthly_disruption_d2),
  list(name = "monthly_disruption_d3",          df = monthly_disruption_d3)
)) {
  message(sprintf("  %-42s %d rows | %d countries | years %d-%d",
    tbl$name, nrow(tbl$df), n_distinct(tbl$df$country_code),
    min(tbl$df$year, na.rm = TRUE), max(tbl$df$year, na.rm = TRUE)))
}

# Mirror share summary: flag countries with high mirroring
high_mirror <- comtrade_export_groups_ts |>
  filter(year == recent_yr, commodity_group == "pink_sheet",
         !is.na(mirror_share_annual), mirror_share_annual > 0.5) |>
  arrange(desc(mirror_share_annual)) |>
  select(country_code, mirror_share_annual) |>
  head(10)
message(sprintf("  High mirror share (>50%%) in %d, pink_sheet exports:", recent_yr))
print(high_mirror)

# Top export concentrators per group (annual)
walk(names(GROUPS), \(grp) {
  top <- comtrade_export_groups_ts |>
    filter(year == recent_yr, commodity_group == grp, !is.na(commodity_hhi)) |>
    arrange(desc(commodity_hhi)) |>
    select(country_code, commodity_hhi, value_share, cv_weighted) |>
    head(5)
  message(sprintf("  Top %s export concentrators (%d):", grp, recent_yr))
  print(top)
})

# Import shocks this year
shock_summary <- comtrade_import_groups_ts |>
  filter(year == recent_yr, shock_flag == 1L) |>
  count(commodity_group, name = "n_countries") |>
  arrange(desc(n_countries))
message(sprintf("  Import shocks active in %d:", recent_yr))
print(shock_summary)

# Disruption signal check: confirm indicators have meaningful variation
report_z <- function(prefix, col, v) {
  nn <- mean(!is.na(v)) * 100
  if (endsWith(col, "_r")) {
    # Robust z: median |z| and p95 are more informative than SD
    med_abs <- median(abs(v), na.rm = TRUE)
    p95     <- quantile(abs(v), 0.95, na.rm = TRUE)
    message(sprintf("  %s %-32s non-null: %.0f%% | med|z|: %.3f | p95|z|: %.3f",
      prefix, col, nn, med_abs, p95))
  } else {
    message(sprintf("  %s %-32s non-null: %.0f%% | SD: %.3f",
      prefix, col, nn, sd(v, na.rm = TRUE)))
  }
}

for (col in c("food_vol_anomaly_z", "food_price_z",
              "energy_price_z",
              "food_cv_basket_wtd", "energy_cv_basket_wtd", "fertilizer_cv_basket_wtd")) {
  report_z("D2", col, monthly_disruption_d2[[col]])
}
for (col in c("export_vol_anomaly_z", "export_vol_anomaly_z_r",
              "export_basket_price_z", "export_basket_price_z_r",
              "cv_basket_wtd")) {
  report_z("D3", col, monthly_disruption_d3[[col]])
}
