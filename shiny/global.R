library(shiny)
library(dplyr)
library(tidyr)
library(duckdb)
library(DBI)
library(echarts4r)
library(DT)
library(stringr)
library(tibble)
library(countrycode)

# Null-coalescing operator: returns `a` if non-null, non-empty, and non-NA; otherwise `b`
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# DB connection (read-only)
db_path <- file.path("data", "georisk.duckdb")
drv     <- duckdb(dbdir = db_path, read_only = TRUE)
con     <- dbConnect(drv)

# Tier palette + ordering
tier_colors <- c(
  Critical = "#8B2635",
  Elevated = "#C17B5A",
  Guarded  = "#C4A563",
  Moderate = "#5D8FA3",
  Low      = "#859C72"
)
tier_order <- names(tier_colors)

# Score to hex color by threshold
score_color <- function(x) {
  dplyr::case_when(
    is.na(x)  ~ "#aaaaaa",
    x >= 0.75 ~ "#8B2635",
    x >= 0.65 ~ "#C17B5A",
    x >= 0.50 ~ "#C4A563",
    x >= 0.30 ~ "#5D8FA3",
    TRUE      ~ "#859C72"
  )
}

# Dimension lookup table
dim_meta <- tibble(
  id        = c("D1",                      "D2",                        "D3",                "D4"),
  label     = c("Macro-Fiscal Stability",  "Strategic Import Reliance", "Export Fragility",  "Network Contagion Risk"),
  tab_id    = c("macrofiscal",             "importreliance",            "exportfragility",   "network"),
  color     = c("#2B4C6F",                "#4A7C8C",                   "#859C72",           "#6A7381"),
  score_col = c("d1_score",               "d2_score",                  "d3_score",          "d4_score")
)

# Context threshold flags
is_flagged <- function(key, val) {
  thresholds <- list(
    debt_service_ratio      = list(warn = "above", thresh = 20),
    reserves_months_imports = list(warn = "below", thresh = 3),
    ext_debt_to_reserves    = list(warn = "above", thresh = 1.0)
  )
  cfg <- thresholds[[key]]
  if (is.null(cfg) || is.null(val) || length(val) == 0 || is.na(val)) return(FALSE)
  if (cfg$warn == "below") return(val < cfg$thresh)
  if (cfg$warn == "above") return(val > cfg$thresh)
  FALSE
}

# Number formatters
fmt_big <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("N/A")
  if (abs(x) >= 1e12) return(paste0(round(x / 1e12, 1), "T"))
  if (abs(x) >= 1e9)  return(paste0(round(x / 1e9,  1), "B"))
  if (abs(x) >= 1e6)  return(paste0(round(x / 1e6,  1), "M"))
  format(round(x), big.mark = ",")
}

fmt1 <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "N/A" else sprintf("%.1f", x)
fmt2 <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "N/A" else sprintf("%.2f", x)

# Latest non-NA value from a df column
latest_val <- function(df, col) {
  if (nrow(df) == 0 || !col %in% names(df)) return(NA_real_)
  v <- df[[col]][!is.na(df[[col]])]
  if (length(v) == 0) NA_real_ else tail(v, 1)
}

# HTML: tier badge span
tier_badge_html <- function(tier) {
  col <- tier_colors[as.character(tier)] %||% "#6A7381"
  sprintf('<span class="tier-badge" style="background:%s">%s</span>',
          col, toupper(as.character(tier) %||% "UNKNOWN"))
}

# HTML: context KPI card
kpi_card <- function(label, val_str, flagged = FALSE, sub = NULL) {
  div(class = paste("ctx-card", if (flagged) "ctx-flagged" else ""),
    div(class = "ctx-value",
        val_str,
        if (flagged) tags$i(class = "fas fa-exclamation-triangle ctx-warn-icon")),
    div(class = "ctx-label", label),
    if (!is.null(sub)) div(class = "ctx-sub", sub)
  )
}

# HTML: source attribution link
src_tag <- function(label, url) {
  tags$span(class = "chart-src",
    "Source: ",
    tags$a(href = url, target = "_blank", rel = "noopener", label)
  )
}

# HTML: interpretation callout box
interp_box <- function(...) div(class = "interp-box", ...)

# Inline SVG sparkline — always width:100%, no JS timing dependency
# type = "line" (area+line) or "bar" (positive/negative split)
svg_spark <- function(values, color = "#5D8FA3", type = "line", height = 52) {
  values <- values[!is.na(values)]
  if (length(values) < 2) return(tags$div(style = sprintf("height:%dpx", height)))

  n      <- length(values)
  mn     <- min(values); mx <- max(values)
  pad    <- 4
  h_in   <- height - 2 * pad
  vw     <- 100  # viewBox width units

  # Normalize values to y pixel positions (top = 0)
  y_norm <- if (mn == mx) rep(height / 2, n) else
    pad + h_in * (1 - (values - mn) / (mx - mn))
  xs <- seq(0, vw, length.out = n)

  body <- if (type == "bar") {
    zero_y <- if (mn >= 0) height - pad
               else if (mx <= 0) pad
               else pad + h_in * (1 - (0 - mn) / (mx - mn))
    bw <- max(1.5, (vw / n) * 0.65)

    # Zero reference line when series crosses zero
    ref <- if (mn < 0 && mx > 0)
      sprintf('<line x1="0" y1="%.2f" x2="%d" y2="%.2f" stroke="#dde4ec" stroke-width="0.8"/>', zero_y, vw, zero_y)
    else ""

    bars <- paste(vapply(seq_len(n), function(i) {
      y1  <- min(y_norm[i], zero_y)
      bh  <- max(abs(y_norm[i] - zero_y), 1.5)
      col <- if (values[i] >= 0) color else "#B8795E"
      sprintf('<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s" rx="1"/>',
              xs[i] - bw / 2, y1, bw, bh, col)
    }, character(1)), collapse = "")

    paste0(ref, bars)
  } else {
    pts  <- paste(sprintf("%.2f,%.2f", xs, y_norm), collapse = " ")
    area <- sprintf("0,%.2f %s %.2f,%.2f", as.numeric(height), pts, as.numeric(vw), as.numeric(height))
    paste0(
      sprintf('<polygon points="%s" fill="%s" fill-opacity="0.18" stroke="none"/>', area, color),
      sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>', pts, color)
    )
  }

  HTML(sprintf(
    '<svg viewBox="0 0 %d %d" width="100%%" height="%dpx" preserveAspectRatio="none" style="display:block;margin-top:2px">%s</svg>',
    vw, height, height, body
  ))
}

# Source URL lookup
urls <- list(
  imf_weo    = "https://www.imf.org/en/Publications/WEO",
  wb_wdi     = "https://databank.worldbank.org/source/world-development-indicators",
  comtrade   = "https://comtradeplus.un.org",
  pink_sheet = "https://www.worldbank.org/en/research/commodity-markets",
  fred       = "https://fred.stlouisfed.org",
  usgs       = "https://www.usgs.gov/centers/national-minerals-information-center",
  census     = "https://www.census.gov/programs-surveys/international-programs/about/idb.html",
  wto        = "https://www.wto.org/english/res_e/reser_e/tariff_profiles_e.htm"
)

# Preload latest tier per country (used for country selector + score cards)
latest_tiers <- dbGetQuery(con, "
  SELECT r.country_code,
         cf.country_name,
         cf.iso2c,
         r.year,
         r.tier,
         r.mean_score,
         r.d1_score, r.d2_score, r.d3_score, r.d4_score,
         r.region, r.income_group,
         r.sig_1yr
  FROM gold.risk_tiers r
  JOIN bronze.country_frame cf ON cf.iso3c = r.country_code
") |>
  filter(!is.na(tier)) |>
  group_by(country_code) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  mutate(tier = factor(tier, levels = tier_order))

country_choices <- setNames(latest_tiers$country_code, latest_tiers$country_name)

# KPI indicator descriptions — shown in the shared detail panel when a card is clicked
kpi_descriptions <- list(
  gdp_growth = list(
    title = "GDP Growth %",
    text  = paste0(
      "Source: IMF World Economic Outlook. Annual percentage change in real GDP, adjusted for inflation. ",
      "It is the broadest single measure of macroeconomic momentum, capturing ",
      "whether an economy is expanding, stagnant, or contracting in real terms. ",
      "Sustained negative growth over two consecutive quarters typically signals ",
      "a recession. Persistently high growth alongside high inflation can indicate ",
      "an overheating economy at risk of a hard landing. Structural growth rates ",
      "vary significantly across income groups and development stages, so ",
      "cross-country comparison should account for peer context."
    )
  ),
  current_acct = list(
    title = "Current Account % GDP",
    text  = paste0(
      "Source: IMF World Economic Outlook (World Bank WDI for historical fill). Records a country's net transactions in goods, services, primary income, ",
      "and secondary income with the rest of the world, expressed as a share of GDP. ",
      "A surplus means the country earns more from abroad than it spends, ",
      "accumulating net foreign assets; a deficit means it is spending more than it earns ",
      "and building external liabilities that must be financed. Persistent deficits above ",
      "3-4% of GDP are often flagged as vulnerability signals, particularly when financed ",
      "by short-term capital flows rather than stable FDI. Large surpluses can also signal ",
      "structural imbalances — domestic demand suppression or currency undervaluation."
    )
  ),
  fiscal_bal = list(
    title = "Fiscal Balance % GDP",
    text  = paste0(
      "Source: IMF World Economic Outlook. General government net lending or borrowing as a share of GDP, capturing the ",
      "difference between total government revenues and expenditures. A deficit means ",
      "the government spends more than it collects and must borrow; a surplus means it ",
      "is saving. The fiscal balance directly drives debt accumulation over time. ",
      "Persistent structural deficits — those not explained by the business cycle — ",
      "erode fiscal space and can constrain the capacity to respond to future shocks ",
      "without additional borrowing or tax increases. IMF forecasts extend several years ",
      "forward and reflect expected consolidation or expansion paths."
    )
  ),
  inflation = list(
    title = "Inflation %",
    text  = paste0(
      "Source: IMF World Economic Outlook. Annual percentage change in the consumer price index, measuring the rate at ",
      "which the general price level of goods and services rises over time. Moderate ",
      "and stable inflation (typically targeting 2% in advanced economies) is consistent ",
      "with healthy economic activity. High or volatile inflation erodes household ",
      "purchasing power, distorts price signals, and complicates investment planning. ",
      "Hyperinflation can be destabilizing and is often associated with fiscal ",
      "imbalances or monetary financing of deficits. Deflation signals weak aggregate ",
      "demand and can entrench a debt-deflation spiral if expectations become unanchored."
    )
  ),
  gross_debt = list(
    title = "Govt Gross Debt % GDP",
    text  = paste0(
      "Source: IMF World Economic Outlook. Total outstanding government liabilities — including bonds, loans, and other ",
      "instruments — as a share of the economy. Higher debt levels reduce the fiscal ",
      "space available to respond to downturns without triggering market concerns ",
      "about sustainability, and typically raise sovereign borrowing costs over time. ",
      "There is no universally applicable safe threshold; sustainability depends on the ",
      "interplay of growth rates, real interest rates, the primary balance, and currency ",
      "denomination of debt. Countries with reserve currency status or deep domestic ",
      "capital markets can sustain higher debt levels than those relying on external financing."
    )
  ),
  debt_svc = list(
    title = "Debt Service % Exports",
    text  = paste0(
      "Source: World Bank World Development Indicators. Total scheduled principal and interest payments on external debt as a share of ",
      "export earnings. This ratio measures the proportion of foreign exchange income ",
      "consumed by debt repayment obligations. When this ratio is high, a country has ",
      "limited foreign currency available for imports, reserve accumulation, and other ",
      "external obligations. Ratios above 20% are a standard early-warning threshold ",
      "used by the World Bank and IMF for debt distress assessments. The ratio is ",
      "particularly sensitive to exchange rate movements and rollover risk, as sharp ",
      "depreciations or loss of market access can cause rapid deterioration."
    )
  ),
  ext_debt = list(
    title = "Ext. Debt / Reserves",
    text  = paste0(
      "Source: World Bank World Development Indicators. Short-term external debt outstanding relative to total international reserves. ",
      "This ratio — known as the Guidotti-Greenspan rule — captures whether a country ",
      "holds enough reserves to cover its near-term external debt obligations without ",
      "relying on new borrowing or asset sales. When the ratio exceeds 1.0x, reserves ",
      "are insufficient to cover short-term external liabilities if rollover access is ",
      "disrupted — a key early-warning indicator of balance-of-payments stress and ",
      "potential currency crisis. Reserves act as self-insurance against sudden stops; ",
      "their adequacy also depends on exchange rate regime and capital account openness."
    )
  ),
  trade_gdp = list(
    title = "Trade % GDP",
    text  = paste0(
      "Source: World Bank World Development Indicators. Total exports and imports of goods and services as a share of GDP, measuring ",
      "the degree to which an economy is integrated with global markets. Higher trade ",
      "openness means that external shocks — commodity price swings, trading partner ",
      "recessions, supply chain disruptions, or tariff changes — transmit more rapidly ",
      "and forcefully into domestic conditions. It also reflects depth of integration ",
      "into global supply chains and dependence on external demand. Very high trade ",
      "openness amplifies both upside gains from global growth and downside exposure ",
      "to external volatility. Small open economies typically have ratios exceeding 100%."
    )
  ),
  net_fdi = list(
    title = "Net FDI % GDP",
    text  = paste0(
      "Source: World Bank World Development Indicators. Net foreign direct investment inflows — gross inflows minus outflows — as a ",
      "share of GDP. FDI represents long-term cross-border investment in productive ",
      "assets and is considered more stable and beneficial than portfolio flows. ",
      "Sustained positive inflows signal investor confidence, bring productive capital, ",
      "technology transfer, and employment, and reduce reliance on debt financing. ",
      "Persistent near-zero or negative net FDI may reflect deteriorating investment ",
      "conditions, regulatory uncertainty, or structural competitiveness concerns. ",
      "The direction of FDI flows is a leading signal of medium-term growth prospects ",
      "that complements debt and current account indicators."
    )
  )
)
