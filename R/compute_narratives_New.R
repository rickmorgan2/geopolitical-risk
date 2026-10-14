# compute_narratives.R
#
# Narrative layer: LLM-generated country-dimension risk summaries.
#
# Produces five narratives per country-year that are displayed alongside 
# the charts and tables visible in each dashboard section:
#
#   D1  Macro-Fiscal Stability
#         - Reads from the same data behind the fiscal time-series chart
#           (current account %, fiscal balance %, gross debt %) and the
#           context-flag panel (reserves, debt service, external debt ratio).
#
#   D2  Strategic Import Reliance
#         - Addresses both chart areas: the structural supplier diversity
#           trend (food/energy/cereal/fertilizer HHI, left) and the real-
#           time anomaly signals (volume shortfall, price spike, right).
#         - Names specific commodities from the top-5 import table (bottom).
#
#   D3  Export Fragility
#         - Mirror structure: partner and commodity concentration trend
#           (left) and export revenue anomaly signals (right).
#         - Names top-5 export commodities; notes mineral leverage where set.
#
#   D4  Network Contagion
#         - Explains the ranked partner bar chart (composite risk x trade
#           weight) and the category exposure table below it (food/energy/
#           fertilizer/export channel shares per partner).
#
#   OVW Overview -- 5-paragraph synthesis across all four dimensions.
#
# Output
#   gold.narratives  country-year-dimension, narrative text + audit fields
#
# API 
#   Calls Claude (Anthropic) by default; configurable via DIM_CONFIG.
#   Set ANTHROPIC_API_KEY in environment before running.
#   Rate-limited via Sys.sleep between calls.
#
# Expects: `con` (DBI connection to econrisk.duckdb) from run_pipeline.R

library(DBI)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(httr)
library(jsonlite)

if (!exists("con")) stop("No DB connection. Run via run_pipeline.R")

# CONFIG -----------------------------------------------------------------------

# Cap at max year with actual dimensional data -- risk_tiers can include
# WEO forecast years (e.g. 2030) with no real indicator coverage.
NARRATIVE_YEAR <- dbGetQuery(con, "
  SELECT MIN(max_yr) AS yr FROM (
    SELECT MAX(year) AS max_yr FROM gold.risk_tiers WHERE year IS NOT NULL
    UNION ALL
    SELECT MAX(year) AS max_yr FROM gold.d1_macro_posterior WHERE risk_score IS NOT NULL
    UNION ALL
    SELECT MAX(year) AS max_yr FROM gold.d2_annual_posterior WHERE risk_score IS NOT NULL
  )
")$yr |> as.integer()
if (is.na(NARRATIVE_YEAR)) stop("Cannot determine NARRATIVE_YEAR: gold.risk_tiers is empty.")
message(sprintf("  [narratives] NARRATIVE_YEAR = %d (derived from gold.risk_tiers)", NARRATIVE_YEAR))

NARRATIVE_TEMP  <- 0.2   # low temperature for factual, reproducible output
NARRATIVE_SLEEP <- 0.5   # seconds between API calls

# Per-dimension API and model config.
# D1-D4 are one paragraph (300 tokens); OVW is the longer synthesis.
# Mix platforms freely -- e.g. cheaper model for D1-D4, stronger for OVW.
DIM_CONFIG <- list(
  D1  = list(api = "claude", model = "claude-haiku-4-5-20251001", max_tokens = 300L),
  D2  = list(api = "claude", model = "claude-haiku-4-5-20251001", max_tokens = 300L),
  D3  = list(api = "claude", model = "claude-haiku-4-5-20251001", max_tokens = 300L),
  D4  = list(api = "claude", model = "claude-haiku-4-5-20251001", max_tokens = 300L),
  OVW = list(api = "claude", model = "claude-sonnet-4-6",         max_tokens = 1200L)
)

# Example: mix platforms
# DIM_CONFIG <- list(
#   D1  = list(api = "openai", model = "gpt-4.1-mini", max_tokens = 600L),
#   D2  = list(api = "openai", model = "gpt-4.1-mini", max_tokens = 600L),
#   D3  = list(api = "openai", model = "gpt-4.1-mini", max_tokens = 600L),
#   D4  = list(api = "openai", model = "gpt-4.1-mini", max_tokens = 600L),
#   OVW = list(api = "claude", model = "claude-sonnet-4-6", max_tokens = 1600L)
# )

# Client context -- swap to change the analytical lens across runs
CONTEXT_STATEMENT <- paste(
  "The client is a professional risk analyst seeking concise,",
  "actionable intelligence on economic and trade vulnerabilities.",
  "Focus on operational and investment risk implications.",
  "They work for a risk management firm."
)

# HELPERS ----------------------------------------------------------------------

write_gold <- function(df, table_name, con) {
  df <- mutate(df, score_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS gold")
  dbWriteTable(con, DBI::Id(schema = "gold", table = table_name),
               df, overwrite = TRUE)
  message(sprintf("  [gold.%s] %d rows written", table_name, nrow(df)))
}

# Format a numeric value for prompt display -- NA becomes "not available"
fmt <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("not available")
  formatC(round(x, digits), format = "f", digits = digits)
}

fmt_pct <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("not available")
  paste0(formatC(round(x, 1), format = "f", digits = 1), "%")
}

# Claude API call
call_claude <- function(system_msg, user_msg,
                        model      = "claude-haiku-4-5-20251001",
                        max_tokens = 300L,
                        temperature = NARRATIVE_TEMP) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (identical(api_key, "")) stop("ANTHROPIC_API_KEY not set")

  resp <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ),
    body = jsonlite::toJSON(auto_unbox = TRUE, list(
      model       = model,
      max_tokens  = max_tokens,
      temperature = temperature,
      system      = system_msg,
      messages    = list(list(role = "user", content = user_msg))
    ))
  )

  if (httr::status_code(resp) >= 300)
    stop("Claude API error: ", httr::status_code(resp), " ",
         httr::content(resp, as = "text", encoding = "UTF-8"))

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  parsed$content[[1]]$text
}

# OpenAI fallback
call_openai <- function(system_msg, user_msg,
                        model       = "gpt-4.1-mini",
                        max_tokens  = 300L,
                        temperature = NARRATIVE_TEMP) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (identical(api_key, "")) stop("OPENAI_API_KEY not set")

  resp <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      Authorization  = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(auto_unbox = TRUE, list(
      model       = model,
      max_tokens  = max_tokens,
      temperature = temperature,
      messages    = list(
        list(role = "system", content = system_msg),
        list(role = "user",   content = user_msg)
      )
    ))
  )

  if (httr::status_code(resp) >= 300)
    stop("OpenAI API error: ", httr::status_code(resp), " ",
         httr::content(resp, as = "text", encoding = "UTF-8"))

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  parsed$choices[[1]]$message$content
}

# Dispatch to the correct API based on DIM_CONFIG
call_api <- function(system_msg, user_msg, dim,
                     max_tokens = DIM_CONFIG[[dim]]$max_tokens) {
  cfg <- DIM_CONFIG[[dim]]
  if (cfg$api == "claude") {
    call_claude(system_msg, user_msg, model = cfg$model, max_tokens = max_tokens)
  } else {
    call_openai(system_msg, user_msg, model = cfg$model, max_tokens = max_tokens)
  }
}

SYSTEM_MSG <- glue(
  "You are a Ph.D.-trained economist and geopolitical risk analyst specializing ",
  "in sovereign risk, trade vulnerability, and macroeconomic fragility. ",
  "Your role is to transform quantitative risk data into clear, analytical prose ",
  "suitable for professional risk intelligence briefings. {CONTEXT_STATEMENT}\n\n",
  "Your audience does not share your background. Use plain language, but don't dumb it down.\n",
  "Key principles:\n",
  "- Cite specific scores, time periods, and trends\n",
  "- Use precise language; avoid technical jargon from economics and political science\n",
  "- Avoid hedging or vague generalizations\n",
  "- Write in flowing prose -- no bullet points or lists\n",
  "- Structure analysis logically: current state -> trend -> risk implication\n",
  "- Avoid the word 'robust'\n",
  "- Treat price volatility (CV measures) as the primary structural risk signal; ",
  "z-score anomalies capture a current shock, but chronic price volatility makes ",
  "supply planning and contracting unreliable even in the absence of a shock. ",
  "When both are present, lead with what the volatility tells you.\n",
  "- DO NOT USE 'Dimension [1-4]' in any of the text. Refer to the dimension by its name."
)

# PULL DATA --------------------------------------------------------------------

message("--- Narratives: pulling gold posteriors and context ---")

# Annual composite scores with pre-computed change labels
d1 <- dbGetQuery(con, glue("
  SELECT country_code, year,
    risk_score, risk_score_q05, risk_score_q95,
    chg_1yr, sig_1yr, chg_3yr, sig_3yr, chg_5yr, sig_5yr,
    region, income_group, region_mean, income_mean,
    vs_region, vs_income,
    peer1_code, peer1_score, peer2_code, peer2_score
  FROM gold.d1_macro_posterior
  WHERE year = {NARRATIVE_YEAR}
")) |> as_tibble()

d4 <- dbGetQuery(con, glue("
  SELECT country_code, year,
    d4_score, d4_score_q05, d4_score_q95, n_partners,
    chg_1yr, sig_1yr, chg_3yr, sig_3yr, chg_5yr, sig_5yr,
    region, income_group, region_mean, income_mean,
    vs_region, vs_income,
    peer1_code, peer1_score, peer2_code, peer2_score
  FROM gold.d4_annual
  WHERE year = {NARRATIVE_YEAR}
")) |> as_tibble()

# Header card: tier badge, composite score, 1yr tier change arrow
# Used in the overview narrative and to name partner tiers in the D4 prompt
# (D4 bar chart colors each bar by the partner's tier -- narrative should match)
tiers <- dbGetQuery(con, glue("
  SELECT country_code, year,
    tier, composite_score, tier_change_1yr
  FROM gold.risk_tiers
  WHERE year = {NARRATIVE_YEAR}
")) |> as_tibble()

# D1: the three time-series lines + WEO projections shown in the left chart
macro <- dbGetQuery(con, glue("
  SELECT country_code, year,
    gdp_growth_pct, inflation_cpi_pct, current_account_pct_gdp,
    domestic_credit_pct_gdp, net_fdi_pct_gdp,
    reserves_months_imports, ext_debt_to_reserves, debt_service_ratio
  FROM silver.macro_derived
  WHERE year BETWEEN {NARRATIVE_YEAR - 3L} AND {NARRATIVE_YEAR}
")) |> as_tibble()

# WEO: fiscal balance and gross debt -- historical + near-term projections
# (projections rendered as dashed lines on the dashboard chart)
weo <- dbGetQuery(con, glue("
  SELECT iso3c AS country_code, year,
    MAX(CASE WHEN indicator_code = 'GGXCNL_NGDP' THEN value END) AS fiscal_balance_pct_gdp,
    MAX(CASE WHEN indicator_code = 'GGXWDG_NGDP' THEN value END) AS govt_gross_debt_pct_gdp
  FROM bronze.imf_weo_annual
  WHERE year BETWEEN {NARRATIVE_YEAR - 3L} AND {NARRATIVE_YEAR + 2L}
  GROUP BY iso3c, year
")) |> as_tibble()

# D2: monthly supplier diversity by commodity group (structural trend chart)
# diversity = 1 - supplier_hhi; higher = more diversified sourcing
d2_hhi_monthly <- dbGetQuery(con, "
  SELECT country_code, year, month,
    diversity_food, diversity_energy, diversity_cereal, diversity_fertilizer
  FROM silver.d2_import_hhi_monthly
  ORDER BY country_code, year, month
") |> as_tibble()

# D2: real-time anomaly indicators (right chart)
d2_monthly_raw <- dbGetQuery(con, "
  SELECT country_code, year, month,
    food_vol_anomaly_z,
    food_price_z,
    food_cv_basket_wtd,
    energy_cv_basket_wtd,
    fertilizer_cv_basket_wtd
  FROM silver.monthly_disruption_d2
  ORDER BY country_code, year, month
") |> as_tibble()

# D3: monthly partner and commodity HHI (structural fragility chart)
# diversity_partner = 1 - partner_hhi; diversity_commodity = 1 - commodity_hhi
d3_hhi_monthly <- dbGetQuery(con, "
  SELECT country_code, year, month,
    diversity_partner, diversity_commodity
  FROM silver.d3_export_fragility_monthly
  ORDER BY country_code, year, month
") |> as_tibble()

# D3: real-time export revenue anomaly indicators (right chart)
d3_monthly_raw <- dbGetQuery(con, "
  SELECT country_code, year, month,
    export_vol_anomaly_z,
    export_basket_price_z,
    cv_basket_wtd
  FROM silver.monthly_disruption_d3
  ORDER BY country_code, year, month
") |> as_tibble()

# D2/D3: monthly composite posteriors with peer context and change labels
d2_monthly <- dbGetQuery(con, "
  SELECT country_code, year, month,
    risk_score, risk_score_q05, risk_score_q95,
    chg_1mo, sig_1mo, chg_3mo, sig_3mo, chg_6mo, sig_6mo,
    chg_9mo, sig_9mo, chg_12mo, sig_12mo,
    region, income_group, region_mean, income_mean,
    vs_region, vs_income,
    peer1_code, peer1_score, peer2_code, peer2_score
  FROM gold.d2_monthly_posterior
") |> as_tibble()

d3_monthly <- dbGetQuery(con, "
  SELECT country_code, year, month,
    risk_score, risk_score_q05, risk_score_q95,
    chg_1mo, sig_1mo, chg_3mo, sig_3mo, chg_6mo, sig_6mo,
    chg_9mo, sig_9mo, chg_12mo, sig_12mo,
    region, income_group, region_mean, income_mean,
    vs_region, vs_income,
    peer1_code, peer1_score, peer2_code, peer2_score
  FROM gold.d3_monthly_posterior
") |> as_tibble()

d4_monthly <- dbGetQuery(con, "
  SELECT country_code, year, month,
    d4_score, d4_score_q05, d4_score_q95,
    chg_1mo, sig_1mo, chg_3mo, sig_3mo, chg_6mo, sig_6mo,
    chg_9mo, sig_9mo, chg_12mo, sig_12mo,
    region, income_group, region_mean, income_mean,
    vs_region, vs_income,
    peer1_code, peer1_score, peer2_code, peer2_score
  FROM gold.d4_monthly
") |> as_tibble()

# D2/D3 bottom table: top 5 import and export commodities by trade value
top5 <- dbGetQuery(con, glue("
  SELECT iso3c AS country_code, yr AS year,
    flow_desc, commodity_name, category, trade_value_usd, rank
  FROM bronze.comtrade_top5_commodities
  WHERE yr = {NARRATIVE_YEAR}
  ORDER BY iso3c, flow_desc, rank
")) |> as_tibble()

# D2 optional bottom panel: net food trade position by subcategory
# positive net_trade_usd = net exporter; negative = net importer
food_net_trade <- dbGetQuery(con, glue("
  SELECT iso3c AS country_code, yr AS year,
    food_subcategory, net_trade_usd, export_value_usd, import_value_usd
  FROM bronze.comtrade_food_net_trade
  WHERE yr = {NARRATIVE_YEAR}
  ORDER BY iso3c, net_trade_usd
")) |> as_tibble()

# Trade structure: food and energy as share of total imports/exports
# Provides structural context for how exposed total trade is to volatile categories
commodity_exposure <- dbGetQuery(con, glue("
  SELECT iso3c AS country_code, yr AS year, flow_desc,
    energy_share, food_share, top_commodity_name, top_commodity_share
  FROM bronze.comtrade_commodity_exposure
  WHERE yr = {NARRATIVE_YEAR}
")) |> as_tibble()

# D4: top partners with category exposure columns (bar chart + detail table)
# food_import_share / fertilizer_import_share / energy_import_share / export_share
# reflect the channel through which each partner creates risk
d4_partners <- dbGetQuery(con, glue("
  SELECT country_code, partner_code, partner_name, weight_norm,
    food_import_share, fertilizer_import_share,
    energy_import_share, export_share,
    d1_score, d2_score, d3_score, composite
  FROM gold.d4_partner_detail
  WHERE year = {NARRATIVE_YEAR} AND month = 12
")) |>
  as_tibble() |>
  group_by(country_code) |>
  slice_max(weight_norm, n = 5, with_ties = FALSE) |>
  ungroup() |>
  left_join(
    tiers |> select(country_code, tier) |> rename(partner_code = country_code, partner_tier = tier),
    by = "partner_code"
  )

# Context flag indicators (right panel in D1; mineral callout in D3)
ctx <- dbGetQuery(con, glue("
  SELECT country_code, year,
    reserves_months_imports, reserves_low,
    ext_debt_to_reserves, ext_debt_high,
    debt_service_ratio, debt_service_high,
    regulatory_quality, control_of_corruption,
    governance_score, governance_weak,
    major_producer, n_dominant_minerals,
    dominant_minerals_str, max_strategic_hhi
  FROM gold.context_indicators
  WHERE year = {NARRATIVE_YEAR}
")) |> as_tibble()

# Country metadata with region and income group
has_wb_meta <- nrow(dbGetQuery(con,
  "SELECT 1 FROM information_schema.tables
   WHERE table_schema = 'bronze' AND table_name = 'wb_countries_raw'")) > 0

if (has_wb_meta) {
  country_names <- dbGetQuery(con, "
    SELECT cf.iso3c AS country_code, cf.country_name,
           wb.region, wb.income_level AS income_group
    FROM bronze.country_frame cf
    LEFT JOIN bronze.wb_countries_raw wb ON cf.iso3c = wb.iso3c
  ") |> as_tibble()
} else {
  message("  WARNING: bronze.wb_countries_raw not found -- region/income_group will be NA.")
  country_names <- dbGetQuery(
    con, "SELECT iso3c AS country_code, country_name FROM bronze.country_frame"
  ) |>
    as_tibble() |>
    mutate(region = NA_character_, income_group = NA_character_)
}

# Countries with a scored D1 row drive the narrative loop
scored_countries <- d1$country_code

message(sprintf("  %d countries to narrate for %d",
                length(scored_countries), NARRATIVE_YEAR))

# CHANGE CONTEXT HELPERS

# Score + pre-computed change labels for a single country-year (annual)
score_context <- function(df, country, score_col = "risk_score", yr = NARRATIVE_YEAR) {
  row <- df |> filter(country_code == country, year == yr)
  get <- function(col) if (nrow(row) > 0 && col %in% names(row)) row[[col]][1] else NA
  list(
    value   = get(score_col),
    chg_1yr = get("sig_1yr"),
    chg_3yr = get("sig_3yr"),
    chg_5yr = get("sig_5yr")
  )
}

# Score + change labels from the latest monthly row
score_context_monthly <- function(df, country, score_col = "risk_score") {
  row <- df |>
    filter(country_code == country) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  get <- function(col) if (nrow(row) > 0 && col %in% names(row)) row[[col]][1] else NA
  list(
    value    = get(score_col),
    chg_3mo  = get("sig_3mo"),
    chg_6mo  = get("sig_6mo"),
    chg_9mo  = get("sig_9mo"),
    chg_12mo = get("sig_12mo")
  )
}

# Pre-computed peer/group context block for prompt injection
fmt_peer_block_precomp <- function(row, score_label = "score") {
  p1_name <- country_names$country_name[
    country_names$country_code == (row$peer1_code[1] %||% "")][1]
  p2_name <- country_names$country_name[
    country_names$country_code == (row$peer2_code[1] %||% "")][1]

  p1_line <- if (!is.na(row$peer1_code[1]) && length(p1_name) > 0 && !is.na(p1_name))
    sprintf("  %s: %s=%s", p1_name, score_label, fmt(row$peer1_score[1]))
  else "  not available"
  p2_line <- if (!is.na(row$peer2_code[1]) && length(p2_name) > 0 && !is.na(p2_name))
    sprintf("  %s: %s=%s", p2_name, score_label, fmt(row$peer2_score[1]))
  else ""

  glue(
    "PEER COMPARISON:\n",
    "  Countries with similar score:\n",
    "{p1_line}\n",
    "{p2_line}\n",
    "  Regional peers ({row$region[1] %||% 'N/A'}): ",
    "mean {score_label} = {fmt(row$region_mean[1])}, ",
    "this country {fmt(row$vs_region[1], 3)} vs mean\n",
    "  Income group peers ({row$income_group[1] %||% 'N/A'}): ",
    "mean {score_label} = {fmt(row$income_mean[1])}, ",
    "this country {fmt(row$vs_income[1], 3)} vs mean"
  )
}

# Monthly trend block for the overview prompt
monthly_trend_block <- function(monthly_df, country, score_col = "risk_score",
                                dim_label = "D?") {
  row <- monthly_df |>
    filter(country_code == country) |>
    arrange(year, month) |>
    slice_tail(n = 1)

  if (nrow(row) == 0) return(glue("  {dim_label}: monthly data not available"))

  get <- function(col) if (col %in% names(row)) row[[col]][1] else NA
  v  <- get(score_col)
  mo <- sprintf("%d-%02d", row$year[1], row$month[1])

  lkbks <- list(
    c("sig_1mo",  "1 month"),
    c("sig_3mo",  "3 months"),
    c("sig_6mo",  "6 months"),
    c("sig_9mo",  "9 months"),
    c("sig_12mo", "12 months")
  )

  lines <- glue("  {dim_label} current ({mo}): {fmt(v)}")
  for (x in lkbks) {
    lbl <- get(x[[1]])
    if (!is.na(lbl)) lines <- c(lines, glue("    vs {x[[2]]} ago: {lbl}"))
  }
  paste(lines, collapse = "\n")
}

# Latest diversity score + 3-month delta for a single group column
# Returns a formatted string: "0.42 (vs 3mo ago: +0.05 = improving)"
fmt_diversity_trend <- function(hhi_df, country, col) {
  rows <- hhi_df |>
    filter(country_code == country) |>
    arrange(year, month)

  n <- nrow(rows)
  if (n == 0 || !(col %in% names(rows))) return("not available")

  latest <- rows[[col]][n]
  if (is.na(latest)) return("not available")

  # 3-month delta where available
  lag_val <- if (n >= 4) rows[[col]][n - 3] else NA_real_
  delta_str <- if (!is.na(lag_val) && !is.na(latest)) {
    d <- latest - lag_val
    sign_str <- if (d > 0) "+" else ""
    dir <- if (d > 0.01) "improving" else if (d < -0.01) "deteriorating" else "stable"
    sprintf(" (3-month delta: %s%s = %s)", sign_str, fmt(d, 3), dir)
  } else ""

  paste0(fmt(latest, 3), delta_str)
}

# Top-N commodity lines for prompt injection (D2 imports or D3 exports)
fmt_top5_block <- function(country, flow = "Import") {
  rows <- top5 |>
    filter(country_code == country, flow_desc == flow) |>
    arrange(rank)

  if (nrow(rows) == 0) return("  not available")

  pmap_chr(rows, function(commodity_name, category, trade_value_usd, rank, ...) {
    sprintf("  %d. %s (%s) -- $%sM",
            rank, commodity_name, category,
            formatC(round(trade_value_usd / 1e6, 0), format = "f", digits = 0, big.mark = ","))
  }) |> paste(collapse = "\n")
}

# Food net trade position by subcategory (D2 optional bottom panel)
# Shows which food categories the country relies on imports for
fmt_food_net_trade <- function(country) {
  rows <- food_net_trade |>
    filter(country_code == country) |>
    arrange(net_trade_usd)

  if (nrow(rows) == 0) return("  not available")

  pmap_chr(rows, function(food_subcategory, net_trade_usd, ...) {
    dir <- if (net_trade_usd < 0) "net importer" else "net exporter"
    sprintf("  %s: %s ($%sM net)",
            food_subcategory, dir,
            formatC(abs(round(net_trade_usd / 1e6, 0)), format = "f", digits = 0, big.mark = ","))
  }) |> paste(collapse = "\n")
}

# Food and energy as share of total trade (structural exposure context)
# Returns a formatted single line: "food X%, energy Y%, top commodity: Z (W%)"
fmt_trade_structure <- function(country, flow = "Import") {
  row <- commodity_exposure |>
    filter(country_code == country, flow_desc == flow)

  if (nrow(row) == 0) return("not available")

  sprintf("food %.0f%%, energy %.0f%%, single largest commodity: %s (%.0f%% of %ss)",
          row$food_share[1] * 100,
          row$energy_share[1] * 100,
          row$top_commodity_name[1] %||% "unknown",
          row$top_commodity_share[1] * 100,
          tolower(flow))
}

# PROMPT BUILDERS

build_prompt_d1 <- function(country_code, country_name, region, income_group) {
  s      <- score_context(d1, country_code, "risk_score")
  m_now  <- macro |> filter(country_code == !!country_code, year == NARRATIVE_YEAR)
  w_now  <- weo   |> filter(country_code == !!country_code, year == NARRATIVE_YEAR)
  w_fwd  <- weo   |> filter(country_code == !!country_code, year == NARRATIVE_YEAR + 2L)
  cx     <- ctx   |> filter(country_code == !!country_code)

  get_m <- function(d, col) if (nrow(d) > 0 && col %in% names(d)) d[[col]][1] else NA_real_

  fis_3yago <- fmt_pct(
    (weo |> filter(country_code == !!country_code, year == NARRATIVE_YEAR - 3L))$fiscal_balance_pct_gdp[1]
  )

  # WEO projection context (dashed lines on the dashboard chart)
  fwd_block <- ""
  if (nrow(w_fwd) > 0) {
    fwd_block <- glue(
      "\nIMF PROJECTIONS ({NARRATIVE_YEAR + 2L}, shown as dashed lines on chart):\n",
      "  Fiscal balance (projected): {fmt_pct(w_fwd$fiscal_balance_pct_gdp[1])} of GDP\n",
      "  Gross debt (projected):     {fmt_pct(w_fwd$govt_gross_debt_pct_gdp[1])} of GDP"
    )
  }

  # Context flag panel (green/amber/red indicators shown to the right of the chart)
  ctx_block <- ""
  if (nrow(cx) > 0) {
    parts <- c()
    if (!is.na(cx$reserves_months_imports))
      parts <- c(parts, glue(
        "FX reserves: {fmt(cx$reserves_months_imports, 1)} months of import cover",
        "{if (cx$reserves_low) ' [FLAG: below 3-month threshold]' else ''}"
      ))
    if (!is.na(cx$ext_debt_to_reserves))
      parts <- c(parts, glue(
        "External debt to reserves: {fmt(cx$ext_debt_to_reserves, 2)}x",
        "{if (cx$ext_debt_high) ' [FLAG: elevated]' else ''}"
      ))
    if (!is.na(cx$debt_service_ratio))
      parts <- c(parts, glue(
        "Debt service ratio: {fmt_pct(cx$debt_service_ratio * 100)}",
        "{if (cx$debt_service_high) ' [FLAG: above 20% threshold]' else ''}"
      ))
    if (!is.na(cx$governance_score))
      parts <- c(parts, glue(
        "WGI governance composite: {fmt(cx$governance_score, 2)}",
        "{if (cx$governance_weak) ' [FLAG: weak institutions]' else ''}"
      ))
    if (length(parts) > 0)
      ctx_block <- paste0("\nCONTEXT FLAG PANEL (amber/red indicators):\n",
                          paste(parts, collapse = "\n"))
  }

  glue("
TASK: Write exactly ONE paragraph (3-4 sentences) contextualizing the macro-fiscal
risk profile for {country_name}. The dashboard shows a time-series chart of current
account %, fiscal balance %, and gross debt % (historical lines with IMF projection
dashed lines), alongside a context-flag panel. Your paragraph should explain what
those chart lines and flags mean together for this country's near-term fiscal stability.

RISK SCORE ({NARRATIVE_YEAR}):
  Macro-fiscal risk: {fmt(s$value)} [0=low risk, 1=high risk]
  1-year change: {s$chg_1yr} | 3-year: {s$chg_3yr} | 5-year: {s$chg_5yr}
  90% CI: [{fmt(d1 |> filter(country_code==!!country_code, year==NARRATIVE_YEAR) |> pull(risk_score_q05))},
           {fmt(d1 |> filter(country_code==!!country_code, year==NARRATIVE_YEAR) |> pull(risk_score_q95))}]

TIME-SERIES INDICATORS ({NARRATIVE_YEAR}):
  Current account balance: {fmt_pct(get_m(m_now, 'current_account_pct_gdp'))} of GDP
  Fiscal balance:          {fmt_pct(get_m(w_now, 'fiscal_balance_pct_gdp'))} of GDP  (3 years ago: {fis_3yago})
  Gross government debt:   {fmt_pct(get_m(w_now, 'govt_gross_debt_pct_gdp'))} of GDP
  GDP growth:              {fmt_pct(get_m(m_now, 'gdp_growth_pct'))}
  Inflation (CPI):         {fmt_pct(get_m(m_now, 'inflation_cpi_pct'))}
  Net FDI:                 {fmt_pct(get_m(m_now, 'net_fdi_pct_gdp'))} of GDP
{fwd_block}
{ctx_block}

Country: {country_name} | Region: {region} | Income group: {income_group}

{fmt_peer_block_precomp(d1 |> filter(country_code == !!country_code, year == NARRATIVE_YEAR), 'macro-fiscal score')}

REQUIREMENTS:
- Open with the overall risk score and its directional trend
- Describe the trajectory of the three chart lines in plain terms -- is debt rising,
  is the fiscal deficit narrowing, is the current account under pressure?
- Where context flags are active (amber/red), state the implication clearly
- Note whether IMF projections show the trajectory improving or worsening
- Anchor relative to regional or income-group peers where instructive
- ONE paragraph, 3-4 sentences -- flowing prose only
- OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

build_prompt_d2 <- function(country_code, country_name, region, income_group) {
  # Monthly composite -- primary score signal
  latest_m <- d2_monthly |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  mo_ref <- if (nrow(latest_m) > 0)
    sprintf("%d-%02d", latest_m$year[1], latest_m$month[1]) else "N/A"
  get_m <- function(col) if (nrow(latest_m) > 0 && col %in% names(latest_m))
    latest_m[[col]][1] else NA

  # Latest disruption indicators (anomaly chart, right panel)
  raw <- d2_monthly_raw |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  get_r <- function(col) if (nrow(raw) > 0 && col %in% names(raw)) raw[[col]][1] else NA

  # Supplier diversity trends (HHI chart, left panel)
  div_food  <- fmt_diversity_trend(d2_hhi_monthly, country_code, "diversity_food")
  div_enrg  <- fmt_diversity_trend(d2_hhi_monthly, country_code, "diversity_energy")
  div_cerl  <- fmt_diversity_trend(d2_hhi_monthly, country_code, "diversity_cereal")
  div_fert  <- fmt_diversity_trend(d2_hhi_monthly, country_code, "diversity_fertilizer")

  # Trade structure: how much of total imports are food / energy
  import_structure <- fmt_trade_structure(country_code, "Import")

  pc_row <- latest_m

  glue("
TASK: Write exactly ONE paragraph (3-4 sentences) explaining {country_name}'s
import reliance risk. The dashboard shows a structural supplier diversity trend
(food/energy/cereal/fertilizer HHI, left), a real-time anomaly chart (volume shortfall
and price spikes, right), and a top-5 import commodity table. The key analytical
question: is persistent price volatility across import baskets -- not just a single
price spike -- creating a chronic sourcing risk that concentration makes harder to
manage?

MONTHLY COMPOSITE SCORE (latest: {mo_ref}) [0=low risk, 1=high risk]:
  Import reliance score: {fmt(get_m('risk_score'))}
  vs 1 month ago:   {get_m('sig_1mo') %||% 'not available'}
  vs 3 months ago:  {get_m('sig_3mo') %||% 'not available'}
  vs 6 months ago:  {get_m('sig_6mo') %||% 'not available'}
  vs 12 months ago: {get_m('sig_12mo') %||% 'not available'}

IMPORT STRUCTURE ({NARRATIVE_YEAR}):
  {import_structure}
  [Food and energy shares indicate structural dependence on volatile commodity categories]

PRICE VOLATILITY -- PRIMARY CHRONIC RISK SIGNALS (right chart, latest: {mo_ref}):
  Food basket price CV (rolling SD of log price changes, weighted by import share):
    {fmt(get_r('food_cv_basket_wtd'), 3)}
    [above 0.30 = chronic instability; persistent volatility strains fiscal and food budgets
     regardless of whether a directional price shock is occurring at this moment]
  Energy basket price CV (rolling):
    {fmt(get_r('energy_cv_basket_wtd'), 3)}
    [above 0.30 = chronic energy cost instability; compounds import bill pressure]
  Fertilizer basket price CV (rolling):
    {fmt(get_r('fertilizer_cv_basket_wtd'), 3)}
    [above 0.30 = input cost volatility affecting domestic food production costs]

DIRECTIONAL SHOCK SIGNALS (right chart -- acute events compounding the volatility baseline):
  Food import volume anomaly (z-score): {fmt(get_r('food_vol_anomaly_z'), 2)}
    [negative = import shortfall vs seasonal baseline; below -1.5 = acute disruption]
  Food basket price anomaly (z-score):  {fmt(get_r('food_price_z'), 2)}
    [positive = price spike above baseline; above +1.5 = acute pressure on top of volatility]

SUPPLIER DIVERSITY (left chart -- 1=fully diversified, 0=single-source):
  Food suppliers:       {div_food}
  Energy suppliers:     {div_enrg}
  Cereal suppliers:     {div_cerl}
  Fertilizer suppliers: {div_fert}
  [Low diversity = concentrated sourcing = reduced ability to substitute when prices spike]

TOP 5 IMPORT COMMODITIES ({NARRATIVE_YEAR}, bottom table):
{fmt_top5_block(country_code, 'Import')}

FOOD NET TRADE POSITION ({NARRATIVE_YEAR}, optional bottom panel):
{fmt_food_net_trade(country_code)}
  [Net importer subcategories = structural food import dependence, amplifying price volatility exposure]

{fmt_peer_block_precomp(pc_row, 'import reliance score')}
Country: {country_name} | Region: {region} | Income group: {income_group}

REQUIREMENTS:
- Lead with the composite score and its trajectory
- The volatility (CV) measures are a primary chronic risk signal: sustained
  price volatility across the import basket is a structural vulnerability, distinct from
  and more persistent than a single price spike. 
- Use the directional z-scores to describe whether a shock is currently compounding that
  volatility baseline
- Connect supplier concentration (low diversity) to the CV measures: a concentrated sourcing
  base removes the substitution options that would otherwise buffer price volatility
- Where the country is a net food importer in specific subcategories, note that structural
  dependence amplifies its exposure to food price instability
- Highlight the leading area of concern. If there are multiple high risk indicators, describe 
  the compound risk, naming the indicators.   
- Reference specific commodities from the top-5 table to ground the analysis
- ONE paragraph, 3-4 sentences -- flowing prose only
- OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

build_prompt_d3 <- function(country_code, country_name, region, income_group) {
  # Monthly composite -- primary score signal
  latest_m <- d3_monthly |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  mo_ref <- if (nrow(latest_m) > 0)
    sprintf("%d-%02d", latest_m$year[1], latest_m$month[1]) else "N/A"
  get_m <- function(col) if (nrow(latest_m) > 0 && col %in% names(latest_m))
    latest_m[[col]][1] else NA

  # Latest disruption indicators (revenue anomaly chart, right panel)
  raw <- d3_monthly_raw |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  get_r <- function(col) if (nrow(raw) > 0 && col %in% names(raw)) raw[[col]][1] else NA

  # Export concentration trends (left panel)
  div_part <- fmt_diversity_trend(d3_hhi_monthly, country_code, "diversity_partner")
  div_cmd  <- fmt_diversity_trend(d3_hhi_monthly, country_code, "diversity_commodity")

  # Export trade structure: how volatile-category-exposed are total exports
  export_structure <- fmt_trade_structure(country_code, "Export")

  cx <- ctx |> filter(country_code == !!country_code)

  # Critical mineral callout (shown when major_producer flag is set)
  cm_block <- ""
  if (nrow(cx) > 0 && isTRUE(cx$major_producer)) {
    cm_block <- glue(
      "\nCRITICAL MINERAL PRODUCER FLAG (dashboard callout):\n",
      "  Dominant minerals (>5% world output): {cx$dominant_minerals_str}\n",
      "  Highest global supply HHI among these: {fmt(cx$max_strategic_hhi, 2)}\n",
      "  IMPORTANT: High export concentration in critical minerals where this country ",
      "holds global market power represents leverage, not fragility. ",
      "The narrative must reflect this distinction."
    )
  }

  pc_row <- latest_m

  glue("
TASK: Write exactly ONE paragraph (3-4 sentences) explaining {country_name}'s
export fragility risk. The dashboard shows a structural concentration trend (partner
and commodity HHI over time, left), an export revenue anomaly chart (right), and a
top-5 export commodity table. The key analytical question: is persistent price
volatility across the export basket -- not just a single price shock -- creating a
chronic revenue instability that narrow buyer/product concentration makes worse?

MONTHLY COMPOSITE SCORE (latest: {mo_ref}) [0=low risk, 1=high risk]:
  Export fragility score: {fmt(get_m('risk_score'))}
  vs 1 month ago:   {get_m('sig_1mo') %||% 'not available'}
  vs 3 months ago:  {get_m('sig_3mo') %||% 'not available'}
  vs 6 months ago:  {get_m('sig_6mo') %||% 'not available'}
  vs 12 months ago: {get_m('sig_12mo') %||% 'not available'}

EXPORT STRUCTURE ({NARRATIVE_YEAR}):
  {export_structure}
  [High food/energy share = large fraction of export revenue in volatile commodity categories]

PRICE VOLATILITY -- PRIMARY CHRONIC RISK SIGNAL (right chart, latest: {mo_ref}):
  Weighted basket price CV (rolling SD of log price changes, weighted by export share):
    {fmt(get_r('cv_basket_wtd'), 3)}
    [above 0.30 = chronic revenue instability; above 0.40 = structural concern;
     high CV means fiscal and external revenue are persistently unpredictable,
     making forward planning and debt servicing unreliable even without a shock]

DIRECTIONAL SHOCK SIGNALS (right chart -- acute events compounding the volatility baseline):
  Export basket price anomaly (z-score): {fmt(get_r('export_basket_price_z'), 2)}
    [negative = revenue price shock; below -1.5 = acute pressure;
     positive = windfall; direction matters as much as magnitude]
  Export volume anomaly (z-score):       {fmt(get_r('export_vol_anomaly_z'), 2)}
    [negative = volume shortfall vs baseline; below -1.5 = compounds revenue pressure]

EXPORT CONCENTRATION TRENDS (left chart -- 1=fully diversified, 0=single-buyer/product):
  Partner diversity (buyer concentration):    {div_part}
  Commodity diversity (basket concentration): {div_cmd}
  [Low diversity = few buyers and few products = price volatility hits the full revenue base]
{cm_block}

TOP 5 EXPORT COMMODITIES ({NARRATIVE_YEAR}, bottom table):
{fmt_top5_block(country_code, 'Export')}

{fmt_peer_block_precomp(pc_row, 'export fragility score')}
Country: {country_name} | Region: {region} | Income group: {income_group}

REQUIREMENTS:
- Lead with the composite score and its trend
- The weighted basket price CV is a the primary chronic risk signal: high CV
  means export revenue is structurally unpredictable, independent of whether a price
  shock is occurring right now -- this is the condition that creates fiscal fragility
- Use the directional z-scores to show whether a shock is currently compounding the
  volatility baseline: a negative price anomaly on top of high CV is a double exposure
- Connect export concentration (buyer and commodity HHI) to volatility: narrow
  concentration means there are no diversified revenue streams to buffer price swings
- Highlight the leading area of concern. If there are multiple high risk indicators, describe 
  the compound risk, naming the indicators.   
- Reference the top export commodities to name which products drive the volatility
- If the critical mineral flag is set, note that concentration may reflect market leverage
- ONE paragraph, 3-4 sentences -- flowing prose only
- OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

build_prompt_d4 <- function(country_code, country_name, region, income_group) {
  s        <- score_context(d4, country_code, "d4_score")
  partners <- d4_partners |> filter(country_code == !!country_code)

  if (nrow(partners) == 0) return(NA_character_)

  # Latest monthly row for near-term signals and peer context
  latest_m <- d4_monthly |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  mo_ref <- if (nrow(latest_m) > 0)
    sprintf("%d-%02d", latest_m$year[1], latest_m$month[1]) else "N/A"
  get_m <- function(col) if (nrow(latest_m) > 0 && col %in% names(latest_m))
    latest_m[[col]][1] else NA

  # Bar chart rows: partner name + composite risk + trade weight (colored by tier)
  # Category exposure table rows: which channel each partner represents
  partner_lines <- pmap_chr(partners, function(partner_name, weight_norm,
                                               food_import_share, fertilizer_import_share,
                                               energy_import_share, export_share,
                                               d1_score, d2_score,
                                               d3_score, composite,
                                               partner_tier = NA_integer_, ...) {
    tier_str <- if (!is.na(partner_tier)) sprintf("Tier %d", partner_tier) else "tier N/A"
    exposure <- c(
      if (!is.na(food_import_share)        && food_import_share > 0.05)
        sprintf("food %.0f%%",        food_import_share * 100),
      if (!is.na(fertilizer_import_share)  && fertilizer_import_share > 0.05)
        sprintf("fertilizer %.0f%%",  fertilizer_import_share * 100),
      if (!is.na(energy_import_share)      && energy_import_share > 0.05)
        sprintf("energy %.0f%%",      energy_import_share * 100),
      if (!is.na(export_share)             && export_share > 0.05)
        sprintf("export destination %.0f%%", export_share * 100)
    )
    exposure_str <- if (length(exposure) > 0)
      paste0(" | channels: ", paste(exposure, collapse = ", "))
    else ""
    sprintf("%s (%s | trade weight: %.1f%% | composite risk: %s | D1: %s | D3: %s%s)",
            partner_name, tier_str, weight_norm * 100,
            fmt(composite), fmt(d1_score), fmt(d3_score), exposure_str)
  })

  glue("
TASK: Write exactly ONE paragraph (3-4 sentences) explaining {country_name}'s
network contagion risk. The dashboard shows a horizontal bar chart of top import
partners ranked by composite risk score, weighted by trade share, and colored by
risk tier (Tier 1=green through Tier 5=red), plus a category exposure table below.
Your paragraph should explain what this network exposure means: which partners combine
a high tier/risk score with a large trade weight, and which supply-chain channels are
most implicated.

ANNUAL SCORE ({NARRATIVE_YEAR}):
  Network contagion score: {fmt(s$value)} [0=low risk, 1=high risk; trade-weighted partner risk]
  1-year change: {s$chg_1yr} | 3-year: {s$chg_3yr} | 5-year: {s$chg_5yr}
  Partners scored: {(d4 |> filter(country_code==!!country_code, year==NARRATIVE_YEAR))$n_partners[1]}

MONTHLY TRAJECTORY (latest: {mo_ref}):
  vs 1 month ago:   {get_m('sig_1mo') %||% 'not available'}
  vs 3 months ago:  {get_m('sig_3mo') %||% 'not available'}
  vs 6 months ago:  {get_m('sig_6mo') %||% 'not available'}
  vs 12 months ago: {get_m('sig_12mo') %||% 'not available'}

TOP PARTNERS (bar chart colored by tier + category exposure table below):
{paste(partner_lines, collapse = '\n')}
[Tiers 1-5: 1=lowest risk (green bar), 5=highest risk (red bar)]
[composite = trade-weighted average of macro-fiscal, import reliance, export fragility risk]
[channel shares shown only where > 5% of bilateral trade value]

Country: {country_name} | Region: {region} | Income group: {income_group}

{fmt_peer_block_precomp(latest_m, 'network contagion score')}

REQUIREMENTS:
- Open by explaining that this score measures how much of this country's supply-chain
  exposure flows through fragile partners -- not its own internal risk
- Name the partners with both high composite scores and large trade weights (those
  are the bars in the chart that should concern the analyst)
- Use the category exposure data to explain which channels create the risk:
  a food-heavy bilateral with a fragile partner is a food security contagion vector;
  an energy-heavy one is an energy security exposure
- Flag where a high-weight partner also carries elevated macro-fiscal (D1) or
  export fragility (D3) risk specifically, as these can transmit via price or
  supply disruption
- ONE paragraph, 3-4 sentences -- flowing prose only
- OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

build_prompt_overview <- function(country_code, country_name, region, income_group,
                                  d1_txt, d2_txt, d3_txt, d4_txt) {
  s1 <- score_context(d1, country_code, "risk_score")
  s2 <- score_context_monthly(d2_monthly, country_code, "risk_score")
  s3 <- score_context_monthly(d3_monthly, country_code, "risk_score")
  s4 <- score_context(d4, country_code, "d4_score")

  # Tier from header card -- the primary summary label on the dashboard
  tr <- tiers |> filter(country_code == !!country_code)
  tier_val    <- if (nrow(tr) > 0) tr$tier[1]            else NA_integer_
  comp_score  <- if (nrow(tr) > 0) tr$composite_score[1] else NA_real_
  tier_chg    <- if (nrow(tr) > 0) tr$tier_change_1yr[1] else NA_integer_
  tier_line   <- if (!is.na(tier_val)) {
    chg_str <- if (!is.na(tier_chg) && tier_chg != 0)
      sprintf(" (%+d tier vs prior year)", tier_chg) else " (tier unchanged)"
    sprintf("Tier %d | composite score: %s%s", tier_val, fmt(comp_score, 3), chg_str)
  } else "not available"

  cx <- ctx |> filter(country_code == !!country_code)

  flags <- character(0)
  if (nrow(cx) > 0) {
    if (isTRUE(cx$reserves_low))
      flags <- c(flags, "FX reserves below 3-month threshold")
    if (isTRUE(cx$ext_debt_high))
      flags <- c(flags, "external debt elevated relative to reserves")
    if (isTRUE(cx$debt_service_high))
      flags <- c(flags, "debt service ratio above 20%")
    if (isTRUE(cx$governance_weak))
      flags <- c(flags, "weak governance institutions (WGI < -1)")
    if (isTRUE(cx$major_producer))
      flags <- c(flags, glue("major critical mineral producer ({cx$dominant_minerals_str})"))
  }
  flags_block <- if (length(flags) > 0)
    paste0("\nACTIVE ALERT FLAGS:\n", paste("-", flags, collapse = "\n"))
  else
    "\nNo alert flags active."

  m_d2 <- monthly_trend_block(d2_monthly, country_code, dim_label = "Import Reliance")
  m_d3 <- monthly_trend_block(d3_monthly, country_code, dim_label = "Export Fragility")
  m_d4 <- monthly_trend_block(d4_monthly, country_code,
                               score_col = "d4_score",
                               dim_label = "Network Contagion")

  glue("
TASK: Write a comprehensive overview narrative for {country_name}'s economic and
trade risk profile for {NARRATIVE_YEAR}. This is the headline narrative shown at the
top of the country dashboard card, above all four dimension sections. Write 5 paragraphs
in flowing analytical prose integrating annual trends and recent monthly movements.

HEADER CARD SUMMARY (what the analyst sees first):
  {tier_line}
  [Tiers run 1 (lowest risk) to 5 (highest risk); composite score aggregates all four dimensions]

DIMENSIONAL SCORES [0=low risk, 1=high risk]:
  Macro-Fiscal (annual, {NARRATIVE_YEAR}):  {fmt(s1$value)} | 1yr: {s1$chg_1yr} | 3yr: {s1$chg_3yr} | 5yr: {s1$chg_5yr}
  Import Reliance (monthly latest):  {fmt(s2$value)} | 3mo: {s2$chg_3mo} | 6mo: {s2$chg_6mo} | 9mo: {s2$chg_9mo} | 12mo: {s2$chg_12mo}
  Export Fragility (monthly latest): {fmt(s3$value)} | 3mo: {s3$chg_3mo} | 6mo: {s3$chg_6mo} | 9mo: {s3$chg_9mo} | 12mo: {s3$chg_12mo}
  Network Contagion (annual, {NARRATIVE_YEAR}): {fmt(s4$value)} | 1yr: {s4$chg_1yr} | 3yr: {s4$chg_3yr} | 5yr: {s4$chg_5yr}

MONTHLY SIGNALS:
{m_d2}
{m_d3}
{m_d4}

Country: {country_name} | Region: {region} | Income group: {income_group}
{flags_block}

DIMENSIONAL SUMMARIES (use as source material -- do not reproduce verbatim):
[Macro-Fiscal]   {d1_txt}
[Import Reliance] {d2_txt}
[Export Fragility] {d3_txt}
[Network Contagion] {d4_txt}

REQUIREMENTS:
1. Opening paragraph: state the tier and what it means, name the most elevated
   dimension(s), and explain what they mean in combination for this country's
   near-term vulnerability; note if the tier changed in the past year
2. Two body paragraphs covering the four dimensions: draw on the summaries to explain
   primary drivers concisely; do not simply re-state the summaries
3. Monthly trend paragraph: most significant recent movements across import reliance,
   export fragility, and network contagion -- are risks accelerating, stabilizing,
   or reversing? Note where monthly trajectory diverges from the annual trend
4. Synthesis paragraph: cross-dimensional dynamics, active alert flags, and practical
   risk implication for an analyst or portfolio manager; where import or export price
   volatility (CV measures) is elevated, note that the risk is chronic instability
   rather than a single shock -- this distinction matters for how the analyst should
   interpret the data
5. Cite specific scores and time references throughout
6. Flowing prose only, no headers or bullet points
7. OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

# MAIN LOOP

message(glue("--- Narratives: generating for {length(scored_countries)} countries ---"))

results <- list()
for (cc in scored_countries) {
  meta <- country_names |> filter(country_code == cc)
  if (nrow(meta) == 0) next
  cname  <- meta$country_name[1]
  region <- meta$region[1]
  income <- meta$income_group[1]

  message(sprintf("  %s (%s)", cname, cc))
  start <- Sys.time()

  tryCatch({
    p1 <- build_prompt_d1(cc, cname, region, income)
    t1 <- call_api(SYSTEM_MSG, p1, dim = "D1")
    Sys.sleep(NARRATIVE_SLEEP)

    p2 <- build_prompt_d2(cc, cname, region, income)
    t2 <- call_api(SYSTEM_MSG, p2, dim = "D2")
    Sys.sleep(NARRATIVE_SLEEP)

    p3 <- build_prompt_d3(cc, cname, region, income)
    t3 <- call_api(SYSTEM_MSG, p3, dim = "D3")
    Sys.sleep(NARRATIVE_SLEEP)

    p4 <- build_prompt_d4(cc, cname, region, income)
    if (!is.na(p4)) {
      t4 <- call_api(SYSTEM_MSG, p4, dim = "D4")
      Sys.sleep(NARRATIVE_SLEEP)
    } else {
      t4 <- "Insufficient trade partner data to generate network contagion narrative."
    }

    p_ovw <- build_prompt_overview(cc, cname, region, income, t1, t2, t3, t4)
    t_ovw <- call_api(SYSTEM_MSG, p_ovw, dim = "OVW")

    elapsed <- round(as.numeric(difftime(Sys.time(), start, units = "secs")), 1)

    dims <- tibble(
      dimension = c("D1", "D2", "D3", "D4", "OVW"),
      prompt    = c(p1, p2, p3, p4 %||% NA_character_, p_ovw),
      narrative = c(t1, t2, t3, t4, t_ovw)
    )

    results[[cc]] <- dims |>
      mutate(
        country_code  = cc,
        country_name  = cname,
        year          = NARRATIVE_YEAR,
        model         = map_chr(dimension, \(d) DIM_CONFIG[[d]]$model),
        context       = CONTEXT_STATEMENT,
        elapsed_secs  = elapsed,
        generated_at  = Sys.time()
      )

  }, error = function(e) {
    message(sprintf("    ERROR for %s: %s", cc, e$message))
  })
}

# WRITE

if (length(results) > 0) {
  narratives <- bind_rows(results) |>
    select(country_code, country_name, year, dimension,
           narrative, model, context, prompt,
           elapsed_secs, generated_at)

  write_gold(narratives, "narratives", con)

  message("--- Narratives complete ---")
  message(sprintf("  %d narratives | %d countries | models per DIM_CONFIG",
                  nrow(narratives), n_distinct(narratives$country_code)))
  message(sprintf("  Mean elapsed per country: %.1f sec",
                  sum(narratives$elapsed_secs, na.rm = TRUE) /
                    n_distinct(narratives$country_code)))
} else {
  warning("No narratives generated -- check API key and gold table availability")
}
