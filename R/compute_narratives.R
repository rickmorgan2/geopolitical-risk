# compute_narratives.R
#
# Narrative layer: LLM-generated country-dimension risk summaries.
#
# Produces five narratives per country-year:
#   D1  Macro-fiscal risk      -- index trajectory + underlying indicators
#   D2  Import reliance        -- index trajectory + underlying indicators
#   D3  Export fragility       -- index trajectory + underlying indicators
#   D4  Network contagion      -- index trajectory + top partner breakdown
#   OVW Overview               -- longer synthesis across all four dimensions,
#                                 primary drivers, context flags that fired
#
# Dimensional narratives (D1-D4):
#   - Open with the index risk score and its trend
#   - Discuss the underlying indicators driving that score and how they changed
#   - Integrate context indicators where available (see mapping below)
#
# Context indicator mapping:
#   D1: reserves_months_imports, ext_debt_to_reserves, debt_service_ratio,
#       regulatory_quality, control_of_corruption
#   D2: no critical mineral import context -- Comtrade cm data dropped;
#       USGS production data is authoritative source for critical minerals
#   D3: major_producer flag, dominant_minerals_str, max_strategic_hhi
#       (leverage caveat when export concentration is in a critical mineral)
#   D4: no additional context -- top partner scores are the context
#
# Overview narrative:
#   - Longer format (5-7 paragraphs)
#   - One paragraph per dimension summarizing primary drivers
#   - Synthesis paragraph integrating cross-dimensional dynamics
#   - Notes context flags that fired at the country level
#
# -- Output -------------------------------------------------------------------
#   gold.narratives  country-year-dimension, narrative text + audit fields
#
# -- API ----------------------------------------------------------------------
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

# ==============================================================================
# CONFIG
# ==============================================================================

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

# -- Per-dimension API and model config ---------------------------------------
# Set api to "claude" or "openai" per dimension.
# D1-D4 are shorter (2 paragraphs); OVW is the longer overview.
# Mix platforms freely -- e.g. use a cheaper model for D1-D4, stronger for OVW.

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

# ==============================================================================
# HELPERS
# ==============================================================================

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
  "- Avoid the word 'robust'", 
  "- DO NOT USE 'Dimension [1-4]' in any of the text. Refer to the dimension by its name."
)

# ==============================================================================
# PULL DATA
# ==============================================================================

message("--- Narratives: pulling gold posteriors and context ---")

# Annual index scores -- current narrative year only;
# chg/sig pre-computed by compute_changes.R
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

d2 <- dbGetQuery(con, glue("
  SELECT country_code, year,
    risk_score, risk_score_q05, risk_score_q95,
    chg_1yr, sig_1yr, chg_3yr, sig_3yr, chg_5yr, sig_5yr
  FROM gold.d2_annual_posterior
  WHERE year = {NARRATIVE_YEAR}
")) |> as_tibble()

d3 <- dbGetQuery(con, glue("
  SELECT country_code, year,
    risk_score, risk_score_q05, risk_score_q95,
    chg_1yr, sig_1yr, chg_3yr, sig_3yr, chg_5yr, sig_5yr
  FROM gold.d3_annual_posterior
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

# D1 underlying indicators: macro-fiscal from silver + IMF WEO
macro <- dbGetQuery(con, glue("
  SELECT country_code, year,
    gdp_growth_pct, inflation_cpi_pct, current_account_pct_gdp,
    domestic_credit_pct_gdp, net_fdi_pct_gdp,
    reserves_months_imports, ext_debt_to_reserves, debt_service_ratio
  FROM silver.macro_derived
  WHERE year BETWEEN {NARRATIVE_YEAR - 3L} AND {NARRATIVE_YEAR}
")) |> as_tibble()

weo <- dbGetQuery(con, glue("
  SELECT iso3c AS country_code, year,
    MAX(CASE WHEN indicator_code = 'GGXCNL_NGDP' THEN value END) AS fiscal_balance_pct_gdp,
    MAX(CASE WHEN indicator_code = 'GGXWDG_NGDP' THEN value END) AS govt_gross_debt_pct_gdp
  FROM bronze.imf_weo_annual
  WHERE year BETWEEN {NARRATIVE_YEAR - 3L} AND {NARRATIVE_YEAR}
  GROUP BY iso3c, year
")) |> as_tibble()

# D2 underlying indicators: import diversity (annual structural context)
d2_ind <- dbGetQuery(con, glue("
  SELECT country_code, year,
    diversity_food, diversity_energy, diversity_cereal
  FROM silver.d2_import_hhi_annual
  WHERE year BETWEEN {NARRATIVE_YEAR - 3L} AND {NARRATIVE_YEAR}
")) |> as_tibble()

# D3 underlying indicators: export fragility (annual structural context)
d3_ind <- dbGetQuery(con, glue("
  SELECT country_code, year,
    diversity_partner, diversity_commodity, export_independence
  FROM silver.d3_export_fragility_annual
  WHERE year BETWEEN {NARRATIVE_YEAR - 3L} AND {NARRATIVE_YEAR}
")) |> as_tibble()

# D2 raw monthly disruption indicators -- columns reflect post-refactor schema
d2_monthly_raw <- dbGetQuery(con, "
  SELECT country_code, year, month,
    food_vol_anomaly_z,
    food_price_z,
    food_cv_basket_wtd,
    energy_cv_basket_wtd
  FROM silver.monthly_disruption_d2
  ORDER BY country_code, year, month
") |> as_tibble()

# D3 raw monthly disruption indicators
d3_monthly_raw <- dbGetQuery(con, "
  SELECT country_code, year, month,
    export_vol_anomaly_z,
    export_basket_price_z,
    cv_basket_wtd
  FROM silver.monthly_disruption_d3
  ORDER BY country_code, year, month
") |> as_tibble()

# D4 top 5 partners by trade weight for the narrative year
d4_partners <- dbGetQuery(con, glue("
  SELECT country_code, partner_code, partner_name, weight_norm,
    d1_score, d2_score, d3_score, composite
  FROM gold.d4_partner_detail
  WHERE year = {NARRATIVE_YEAR} AND month = 12
")) |>
  as_tibble() |>
  group_by(country_code) |>
  slice_max(weight_norm, n = 5, with_ties = FALSE) |>
  ungroup()

# Monthly composite scores with pre-computed change labels
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

# Context indicators for the narrative year
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

# ==============================================================================
# CHANGE CONTEXT HELPERS
# ==============================================================================

# Return score + pre-computed change labels for a single country-year (annual).
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

# Return score + pre-computed change labels from the latest monthly row.
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

# Format pre-computed peer/group context block for prompt injection.
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

# Monthly trend block for overview prompt: latest score + lookback change labels.
monthly_trend_block <- function(monthly_df, country, score_col = "risk_score",
                                dim_label = "D?") {
  row <- monthly_df |>
    filter(country_code == country) |>
    arrange(year, month) |>
    slice_tail(n = 1)

  if (nrow(row) == 0) return(glue("  {dim_label}: monthly data not available"))

  get <- function(col) if (col %in% names(row)) row[[col]][1] else NA
  v   <- get(score_col)
  mo  <- sprintf("%d-%02d", row$year[1], row$month[1])

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

# ==============================================================================
# PROMPT BUILDERS
# ==============================================================================

build_prompt_d1 <- function(country_code, country_name, region, income_group) {
  s      <- score_context(d1, country_code, "risk_score")
  m_now  <- macro |> filter(country_code == !!country_code, year == NARRATIVE_YEAR)
  m_lag1 <- macro |> filter(country_code == !!country_code, year == NARRATIVE_YEAR - 1L)
  w_now  <- weo   |> filter(country_code == !!country_code, year == NARRATIVE_YEAR)
  cx     <- ctx   |> filter(country_code == !!country_code)

  get_m <- function(d, col) if (nrow(d) > 0 && col %in% names(d)) d[[col]][1] else NA_real_

  fis_3yago <- fmt_pct(
    (weo |> filter(country_code == !!country_code, year == NARRATIVE_YEAR - 3L))$fiscal_balance_pct_gdp[1]
  )

  ctx_block <- ""
  if (nrow(cx) > 0) {
    parts <- c()
    if (!is.na(cx$reserves_months_imports))
      parts <- c(parts, glue(
        "FX reserves: {fmt(cx$reserves_months_imports, 1)} months of import cover",
        "{if (cx$reserves_low) ' [BELOW 3-MONTH THRESHOLD]' else ''}"
      ))
    if (!is.na(cx$ext_debt_to_reserves))
      parts <- c(parts, glue(
        "External debt to reserves ratio: {fmt(cx$ext_debt_to_reserves, 2)}x",
        "{if (cx$ext_debt_high) ' [ELEVATED]' else ''}"
      ))
    if (!is.na(cx$debt_service_ratio))
      parts <- c(parts, glue(
        "Debt service ratio: {fmt_pct(cx$debt_service_ratio * 100)}",
        "{if (cx$debt_service_high) ' [ABOVE 20% THRESHOLD]' else ''}"
      ))
    if (!is.na(cx$governance_score))
      parts <- c(parts, glue(
        "WGI governance composite: {fmt(cx$governance_score, 2)}",
        "{if (cx$governance_weak) ' [WEAK INSTITUTIONS FLAG]' else ''}"
      ))
    if (length(parts) > 0)
      ctx_block <- paste0("\nCONTEXT INDICATORS:\n", paste(parts, collapse = "\n"))
  }

  glue("
TASK: Write exactly ONE paragraphs (3-4 sentences) analyzing {country_name}'s
Dimension 1 (Macro-Fiscal Risk) score. The paragraph should address the index
score, its trend, and overall risk level, and it should explain which
underlying indicators are driving that score and how they have changed.

INDEX SCORE ({NARRATIVE_YEAR}):
  D1 risk score: {fmt(s$value)} [0=low risk, 1=high risk]
  1-year change: {s$chg_1yr}
  3-year change: {s$chg_3yr}
  5-year change: {s$chg_5yr}
  90% CI: [{fmt(d1 |> filter(country_code==!!country_code, year==NARRATIVE_YEAR) |> pull(risk_score_q05))},
           {fmt(d1 |> filter(country_code==!!country_code, year==NARRATIVE_YEAR) |> pull(risk_score_q95))}]

UNDERLYING INDICATORS ({NARRATIVE_YEAR}):
  GDP growth: {fmt_pct(get_m(m_now, 'gdp_growth_pct'))}
  Inflation (CPI): {fmt_pct(get_m(m_now, 'inflation_cpi_pct'))}
  Current account balance: {fmt_pct(get_m(m_now, 'current_account_pct_gdp'))} of GDP
  Fiscal balance: {fmt_pct(get_m(w_now, 'fiscal_balance_pct_gdp'))} of GDP  (3 years ago: {fis_3yago})
  Gross govt debt: {fmt_pct(get_m(w_now, 'govt_gross_debt_pct_gdp'))} of GDP
  Net FDI: {fmt_pct(get_m(m_now, 'net_fdi_pct_gdp'))} of GDP
{ctx_block}

Country: {country_name} | Region: {region} | Income group: {income_group}

{fmt_peer_block_precomp(d1 |> filter(country_code == !!country_code, year == NARRATIVE_YEAR), 'D1 score')}

REQUIREMENTS:
- Open with the D1 risk score and what it signals at the country level
- Name specific indicators that are most elevated or deteriorating
- Anchor the country's score relative to regional or income-group peers where instructive
- If context indicators breach warning thresholds, flag the implication clearly
- ONE paragraph, 3-4 sentences
- Flowing prose only, no bullet points
- OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

build_prompt_d2 <- function(country_code, country_name, region, income_group) {
  # Monthly composite -- primary signal
  latest_m <- d2_monthly |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  mo_ref <- if (nrow(latest_m) > 0)
    sprintf("%d-%02d", latest_m$year[1], latest_m$month[1]) else "N/A"
  get_m <- function(col) if (nrow(latest_m) > 0 && col %in% names(latest_m))
    latest_m[[col]][1] else NA

  # Latest raw disruption indicators (post-refactor column names)
  raw   <- d2_monthly_raw |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  get_r <- function(col) if (nrow(raw) > 0 && col %in% names(raw)) raw[[col]][1] else NA

  # Pre-computed peer context from latest monthly row
  pc_row <- d2_monthly |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)

  glue("
TASK: Write exactly ONE paragraphs (3-4 sentences) analyzing {country_name}'s
Dimension 2 (Import Reliance) risk. This is a MONTHLY, near-term signal -- emphasize
what is happening right now and over recent months. The first paragraph covers the
current monthly composite score and its recent trajectory. The second explains
which specific disruption indicators are elevated and what they signal operationally.

MONTHLY COMPOSITE SCORE (latest: {mo_ref}) [0=low risk, 1=high risk]:
  D2 risk score: {fmt(get_m('risk_score'))}
  vs 1 month ago:   {get_m('sig_1mo') %||% 'not available'}
  vs 3 months ago:  {get_m('sig_3mo') %||% 'not available'}
  vs 6 months ago:  {get_m('sig_6mo') %||% 'not available'}
  vs 12 months ago: {get_m('sig_12mo') %||% 'not available'}

MONTHLY DISRUPTION INDICATORS (latest: {mo_ref}):
  Food import volume anomaly (z-score):  {fmt(get_r('food_vol_anomaly_z'), 2)}
    [negative = food import shortfall vs seasonal baseline; below -1.5 = acute disruption]
  Food basket price anomaly (z-score):   {fmt(get_r('food_price_z'), 2)}
    [positive = food price spike above baseline; above +1.5 = acute price pressure]
  Food basket price CV (rolling):        {fmt(get_r('food_cv_basket_wtd'), 3)}
    [coefficient of variation across food import commodity prices, weighted by value share;
     above 0.30 = elevated price instability]
  Energy basket price CV (rolling):      {fmt(get_r('energy_cv_basket_wtd'), 3)}
    [coefficient of variation across energy import commodity prices, weighted by value share;
     above 0.30 = elevated energy cost instability]

{fmt_peer_block_precomp(pc_row, 'D2 monthly score')}
Country: {country_name} | Region: {region} | Income group: {income_group}

REQUIREMENTS:
- Lead with the current monthly composite score and its recent trajectory (1mo, 3mo, 12mo signals)
- Explain what each elevated disruption indicator signals operationally: a negative food
  volume anomaly means import shortfalls; high CV scores signal chronic price instability
  even absent a directional shock; distinguish food from energy pressure
- Anchor the score relative to regional or income-group peers
- ONE paragraphs, 3-4 sentences each -- flowing prose only
- OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

build_prompt_d3 <- function(country_code, country_name, region, income_group) {
  # Monthly composite -- primary signal
  latest_m <- d3_monthly |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  mo_ref <- if (nrow(latest_m) > 0)
    sprintf("%d-%02d", latest_m$year[1], latest_m$month[1]) else "N/A"
  get_m <- function(col) if (nrow(latest_m) > 0 && col %in% names(latest_m))
    latest_m[[col]][1] else NA

  # Latest raw disruption indicators
  raw   <- d3_monthly_raw |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  get_r <- function(col) if (nrow(raw) > 0 && col %in% names(raw)) raw[[col]][1] else NA

  cx <- ctx |> filter(country_code == !!country_code)

  # Critical mineral producer context -- leverage caveat for export concentration
  cm_block <- ""
  if (nrow(cx) > 0 && isTRUE(cx$major_producer)) {
    cm_block <- glue(
      "\nCRITICAL MINERAL PRODUCER FLAG:\n",
      "  Dominant minerals (>5% world output): {cx$dominant_minerals_str}\n",
      "  Highest global supply HHI among these: {fmt(cx$max_strategic_hhi, 2)}\n",
      "  IMPORTANT: High export concentration in critical minerals where this country ",
      "holds global market power represents leverage, not fragility. ",
      "The narrative must reflect this distinction."
    )
  }

  # Pre-computed peer context from latest monthly row
  pc_row <- d3_monthly |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)

  glue("
TASK: Write exactly ONE paragraphs (3-4 sentences) analyzing {country_name}'s
Dimension 3 (Export Fragility) risk. This is a MONTHLY, near-term signal -- emphasize
what is happening right now and over recent months. The first paragraph covers the
current monthly composite score and its price volatility signals. The second covers
volume disruption and structural context.

MONTHLY COMPOSITE SCORE (latest: {mo_ref}) [0=low risk, 1=high risk]:
  D3 risk score: {fmt(get_m('risk_score'))}
  vs 1 month ago:   {get_m('sig_1mo') %||% 'not available'}
  vs 3 months ago:  {get_m('sig_3mo') %||% 'not available'}
  vs 6 months ago:  {get_m('sig_6mo') %||% 'not available'}
  vs 12 months ago: {get_m('sig_12mo') %||% 'not available'}

PRICE VOLATILITY INDICATORS (latest: {mo_ref}) [PRIMARY SIGNALS]:
  Export basket price anomaly (z-score): {fmt(get_r('export_basket_price_z'), 2)}
    [signed deviation from rolling baseline; below -1.5 = acute revenue pressure,
     above +1.5 = price windfall; direction matters as much as magnitude]
  Weighted basket price CV (rolling):    {fmt(get_r('cv_basket_wtd'), 3)}
    [coefficient of variation across export commodity prices, weighted by value share;
     above 0.30 = elevated revenue instability; persistent CV > 0.40 = structural concern]

VOLUME DISRUPTION (latest: {mo_ref}):
  Export volume anomaly (z-score): {fmt(get_r('export_vol_anomaly_z'), 2)}
    [negative = export shortfall vs seasonal baseline; below -1.5 = acute disruption]
{cm_block}

{fmt_peer_block_precomp(pc_row, 'D3 monthly score')}
Country: {country_name} | Region: {region} | Income group: {income_group}

REQUIREMENTS:
- Lead with the composite score trajectory, then immediately address the price volatility
  indicators -- these are the primary signal for near-term export revenue risk
- Distinguish between price level shocks (export_basket_price_z) and price instability
  (cv_basket_wtd): a negative price anomaly is a revenue shock; a high CV is chronic
  unpredictability even without a directional shock
- Discuss export volume anomaly as a compounding or mitigating factor in the second paragraph
- If the critical mineral producer flag is set, note that concentration may reflect leverage
- Anchor the score relative to regional or income-group peers
- ONE paragraph, 3-4 sentences -- flowing prose only
- OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

build_prompt_d4 <- function(country_code, country_name, region, income_group) {
  s        <- score_context(d4, country_code, "d4_score")
  partners <- d4_partners |> filter(country_code == !!country_code)

  if (nrow(partners) == 0) return(NA_character_)

  # Latest monthly row for near-term change signals and peer context
  latest_m <- d4_monthly |>
    filter(country_code == !!country_code) |>
    arrange(year, month) |>
    slice_tail(n = 1)
  mo_ref <- if (nrow(latest_m) > 0)
    sprintf("%d-%02d", latest_m$year[1], latest_m$month[1]) else "N/A"
  get_m <- function(col) if (nrow(latest_m) > 0 && col %in% names(latest_m))
    latest_m[[col]][1] else NA

  partner_lines <- pmap_chr(partners, function(partner_name, weight_norm,
                                               d1_score, d2_score,
                                               d3_score, composite, ...) {
    sprintf("%s (weight: %.1f%% | D1: %s | D2: %s | D3: %s | composite: %s)",
            partner_name, weight_norm * 100,
            fmt(d1_score), fmt(d2_score), fmt(d3_score), fmt(composite))
  })

  glue("
TASK: Write exactly ONE paragraphs (3-4 sentences) analyzing {country_name}'s
Dimension 4 (Network Contagion Risk) score. The first paragraph addresses the index
score and its recent monthly trajectory. The second discusses which partners are
driving the score.

ANNUAL INDEX SCORE ({NARRATIVE_YEAR}):
  D4 risk score: {fmt(s$value)} [0=low risk, 1=high risk; trade-weighted avg of partner risk]
  1-year change: {s$chg_1yr}
  3-year change: {s$chg_3yr}
  5-year change: {s$chg_5yr}
  Partners scored: {(d4 |> filter(country_code==!!country_code, year==NARRATIVE_YEAR))$n_partners[1]}

MONTHLY TRAJECTORY (latest: {mo_ref}):
  vs 1 month ago:   {get_m('sig_1mo') %||% 'not available'}
  vs 3 months ago:  {get_m('sig_3mo') %||% 'not available'}
  vs 6 months ago:  {get_m('sig_6mo') %||% 'not available'}
  vs 12 months ago: {get_m('sig_12mo') %||% 'not available'}

TOP TRADING PARTNERS (ranked by import share, critical commodity scope):
{paste(partner_lines, collapse = '\n')}
[D1=macro-fiscal, D2=import reliance, D3=export fragility; composite=weighted avg]

Country: {country_name} | Region: {region} | Income group: {income_group}

{fmt_peer_block_precomp(latest_m, 'D4 monthly score')}

REQUIREMENTS:
- Explain that D4 measures how much of this country's supply-chain exposure flows
  through fragile partners, not its own internal risk
- Lead with the annual score but note whether the monthly trajectory is
  accelerating, stabilizing, or reversing that trend
- Name the partners with the highest composite scores and largest trade weights
- Flag if a high-weight partner also has elevated D1 or D3 risk specifically
- Anchor the D4 score relative to regional or income-group peers where instructive
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

  m_d2 <- monthly_trend_block(d2_monthly, country_code, dim_label = "D2 Import Reliance")
  m_d3 <- monthly_trend_block(d3_monthly, country_code, dim_label = "D3 Export Fragility")
  m_d4 <- monthly_trend_block(d4_monthly, country_code,
                               score_col = "d4_score",
                               dim_label = "D4 Network Contagion")

  glue("
TASK: Write a comprehensive overview narrative for {country_name}'s economic and
trade risk profile for {NARRATIVE_YEAR}. This is the headline narrative for the
country dashboard card. Write 5 paragraphs in flowing analytical prose, integrating
both annual trends and recent monthly movements.

DIMENSIONAL SCORES [0=low risk, 1=high risk]:
  D1 Macro-Fiscal (annual, {NARRATIVE_YEAR}): {fmt(s1$value)} | 1yr: {s1$chg_1yr} | 3yr: {s1$chg_3yr} | 5yr: {s1$chg_5yr}
  D2 Import Reliance (monthly):  {fmt(s2$value)} | 3mo: {s2$chg_3mo} | 6mo: {s2$chg_6mo} | 9mo: {s2$chg_9mo} | 12mo: {s2$chg_12mo}
  D3 Export Fragility (monthly): {fmt(s3$value)} | 3mo: {s3$chg_3mo} | 6mo: {s3$chg_6mo} | 9mo: {s3$chg_9mo} | 12mo: {s3$chg_12mo}
  D4 Network Contagion (annual): {fmt(s4$value)} | 1yr: {s4$chg_1yr} | 3yr: {s4$chg_3yr} | 5yr: {s4$chg_5yr}

MONTHLY SIGNALS (D2 and D3 are monthly composite scores):
{m_d2}
{m_d3}
{m_d4}

Country: {country_name} | Region: {region} | Income group: {income_group}
{flags_block}

DIMENSIONAL SUMMARIES (use as source material -- do not reproduce verbatim):
[D1] {d1_txt}
[D2] {d2_txt}
[D3] {d3_txt}
[D4] {d4_txt}

REQUIREMENTS:
1. Open paragraph: overall risk profile, most elevated dimension(s), and
   what they mean in combination for this country
2. Two paragraphs highlighing the four dimension: explain the annual score and its
   primary drivers concisely, drawing on the summaries above
3. Monthly trend paragraph: describe the most significant recent monthly movements
   across D2/D3/D4 -- are risks accelerating, stabilizing, or reversing?
   Note where monthly trajectory diverges from the annual trend
4. Final synthesis paragraph: cross-dimensional dynamics, active alert flags,
   and practical risk implication for an analyst or investor
5. Cite specific scores and time references throughout
6. Flowing prose only, no headers or bullet points
7. OUTPUT FORMAT: Return only the paragraph text, no preamble or labels
")
}

# ==============================================================================
# MAIN LOOP
# ==============================================================================

message(glue("--- Narratives: generating for {length(scored_countries)} countries ---"))

results <- list()
# cc <- "BLR"
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
      t4 <- "Insufficient trade partner data to generate D4 narrative."
    }
    
    p_ovw <- build_prompt_overview(cc, cname, region, income, t1, t2, t3, t4)
    t_ovw <- call_api(SYSTEM_MSG, p_ovw, dim = "OVW")
    # Sys.sleep(NARRATIVE_SLEEP)
    # nchar(t_ovw)
    
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

# ==============================================================================
# WRITE
# ==============================================================================

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
