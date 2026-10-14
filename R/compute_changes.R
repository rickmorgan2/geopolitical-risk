# compute_changes.R

## SETUP ####
# Computes lookback change signals, peer comparisons, and group benchmarks for
# all gold index tables and writes the columns back in place.
#
# Annual tables (D1, D2, D3, D4, risk_tiers):
#   lookbacks: 1, 3, 5 years
#   new columns per lookback N: chg_Nyr (numeric delta), sig_Nyr (label)
#
# Monthly tables (D2, D3, D4):
#   lookbacks: 1, 3, 6, 9, 12 months
#   new columns per lookback N: chg_Nmo (numeric delta), sig_Nmo (label)
#
# Peer / group context (all scored tables):
#   region_mean     -- mean score for country's WB region in that time period
#   income_mean     -- mean score for country's WB income group in that time period
#   vs_region       -- country score minus region_mean (signed; positive = above mean)
#   vs_income       -- country score minus income_mean
#   peer1_code      -- nearest-score country (excluding self)
#   peer1_score     -- that country's score
#   peer2_code      -- second nearest-score country
#   peer2_score     -- that country's score
#   region          -- WB region label (joined from bronze.wb_countries_raw)
#   income_group    -- WB income group label
#
# Change label (three levels, based on CI non-overlap):
#   "substantial"  -- CI intervals do not overlap
#   "marginal"     -- partial overlap
#   "none"         -- full overlap (change within model uncertainty)
#
# For risk_tiers (no CI): label is based solely on tier_num delta.
#   "substantial"  -- tier_num changed by >= 2
#   "marginal"     -- tier_num changed by 1
#   "none"         -- no change

if (!exists("con")) stop("No DB connection. Source via run_pipeline.R")

library(dplyr)
library(tidyr)
library(DBI)

message("--- Changes: computing lookback signals across gold tables ---")

# HELPERS ####

# Change label: substantial if the current point estimate falls outside the
# 90% CI of the comparison period. Marginal if directional but within that CI.
# Note: q05_now / q95_now are carried through for future refinement but are
# not used in the classification at this stage.
change_label <- function(score_now, score_then,
                         q05_now,   q05_then,
                         q95_now,   q95_then) {
  if (any(is.na(c(score_now, score_then, q05_then, q95_then))))
    return(NA_character_)
  if (score_now > q95_then || score_now < q05_then) return("substantial")
  if (score_now > score_then) return("marginal")
  if (score_now < score_then) return("marginal")
  return("none")
}

tier_change_label <- function(tier_now, tier_then) {
  if (is.na(tier_now) || is.na(tier_then)) return(NA_character_)
  delta <- abs(tier_now - tier_then)
  if (delta >= 2L) return("substantial")
  if (delta == 1L) return("marginal")
  return("none")
}

add_annual_changes <- function(df, score_col, q05_col, q95_col,
                                lookbacks = c(1L, 3L, 5L)) {
  df <- df |> arrange(country_code, year)

  for (n in lookbacks) {
    df <- df |>
      group_by(country_code) |>
      mutate(
        !!paste0(".lag_score_", n) := lag(.data[[score_col]], n),
        !!paste0(".lag_q05_",   n) := lag(.data[[q05_col]],   n),
        !!paste0(".lag_q95_",   n) := lag(.data[[q95_col]],   n)
      ) |>
      ungroup()
  }

  for (n in lookbacks) {
    suffix <- paste0(n, "yr")
    df <- df |>
      mutate(
        !!paste0("chg_", suffix) := round(
          .data[[score_col]] - .data[[paste0(".lag_score_", n)]], 4),
        !!paste0("sig_", suffix) := mapply(
          change_label,
          score_now  = .data[[score_col]],
          score_then = .data[[paste0(".lag_score_", n)]],
          q05_now    = .data[[q05_col]],
          q05_then   = .data[[paste0(".lag_q05_",   n)]],
          q95_now    = .data[[q95_col]],
          q95_then   = .data[[paste0(".lag_q95_",   n)]],
          SIMPLIFY   = TRUE
        )
      )
  }
  df |> select(-starts_with(".lag_"))
}

# Monthly lookbacks include 9mo for narrative layer
add_monthly_changes <- function(df, score_col, q05_col, q95_col,
                                 lookbacks = c(1L, 3L, 6L, 9L, 12L)) {
  df <- df |> arrange(country_code, year, month)

  for (n in lookbacks) {
    df <- df |>
      group_by(country_code) |>
      mutate(
        !!paste0(".lag_score_", n) := lag(.data[[score_col]], n),
        !!paste0(".lag_q05_",   n) := lag(.data[[q05_col]],   n),
        !!paste0(".lag_q95_",   n) := lag(.data[[q95_col]],   n)
      ) |>
      ungroup()
  }

  for (n in lookbacks) {
    suffix <- paste0(n, "mo")
    df <- df |>
      mutate(
        !!paste0("chg_", suffix) := round(
          .data[[score_col]] - .data[[paste0(".lag_score_", n)]], 4),
        !!paste0("sig_", suffix) := mapply(
          change_label,
          score_now  = .data[[score_col]],
          score_then = .data[[paste0(".lag_score_", n)]],
          q05_now    = .data[[q05_col]],
          q05_then   = .data[[paste0(".lag_q05_",   n)]],
          q95_now    = .data[[q95_col]],
          q95_then   = .data[[paste0(".lag_q95_",   n)]],
          SIMPLIFY   = TRUE
        )
      )
  }
  df |> select(-starts_with(".lag_"))
}

# Add peer comparisons and group benchmarks to a scored table.
#
# time_cols: character vector of time grouping columns, e.g. c("year") for
#            annual tables or c("year", "month") for monthly tables. Peers
#            and group means are computed within each time slice.
# score_col: column containing the risk score to compare.
# country_meta: tibble(country_code, region, income_group) from wb_countries_raw.
#
# Join map:
#   df -> country_meta          ON  df.country_code = country_meta.country_code
#                                   (LEFT: keeps all scored rows; adds region, income_group)
#
#   df -> region_means          ON  df.[time_cols] = region_means.[time_cols]
#                               AND df.region      = region_means.region
#                                   (LEFT: NA when region is unknown)
#
#   df -> income_means          ON  df.[time_cols]   = income_means.[time_cols]
#                               AND df.income_group  = income_means.income_group
#                                   (LEFT: NA when income_group is unknown)
#
#   slim (self) -> slim (peers) ON  slim.[time_cols] = peers.[time_cols]
#                                   (INNER: same time slice only; self excluded by filter)
#                               -> peers_wide joined back to df
#                               ON  df.[time_cols]   = peers_wide.[time_cols]
#                               AND df.country_code  = peers_wide.country_code
#                                   (LEFT: NA when fewer than 2 scored peers exist)
#
# Output columns added:
#   region, income_group         -- from country_meta
#   region_mean, income_mean     -- group score averages for this time slice
#   vs_region, vs_income         -- signed delta: country score minus group mean
#   peer1_code, peer1_score      -- nearest-score country (excluding self)
#   peer2_code, peer2_score      -- second nearest-score country
add_peer_context <- function(df, score_col, time_cols, country_meta) {

  ### 1. country_code -> region, income_group ----------------------------------
  # Drop any peer/group columns from prior runs before joining -- gold tables
  # are read with SELECT * so they carry these columns on re-runs, which would
  # produce .x/.y suffixes and break downstream group_by calls.
  peer_cols <- c("region", "income_group", "region_mean", "income_mean",
                 "vs_region", "vs_income",
                 "peer1_code", "peer1_score", "peer2_code", "peer2_score")
  df <- df |> select(-any_of(peer_cols))

  # LEFT join: all scored rows retained; unmatched get NA region/income_group.
  df <- df |>
    left_join(
      country_meta |> select(country_code, region, income_group),
      by = "country_code",
      relationship = "many-to-one"   # many time-period rows per country -> one metadata row
    )

  ### 2. Group means: aggregate within time slice x group ----------------------
  grp_time <- syms(time_cols)

  # region_means keyed on (time_cols, region)
  region_means <- df |>
    filter(!is.na(region), !is.na(.data[[score_col]])) |>
    group_by(!!!grp_time, region) |>
    summarise(region_mean = mean(.data[[score_col]], na.rm = TRUE),
              .groups = "drop")

  # income_means keyed on (time_cols, income_group)
  income_means <- df |>
    filter(!is.na(income_group), !is.na(.data[[score_col]])) |>
    group_by(!!!grp_time, income_group) |>
    summarise(income_mean = mean(.data[[score_col]], na.rm = TRUE),
              .groups = "drop")

  # LEFT join means back: match on time slice + group label
  df <- df |>
    left_join(region_means, by = c(time_cols, "region"),
              relationship = "many-to-one") |>  # many countries per region/time -> one mean
    left_join(income_means, by = c(time_cols, "income_group"),
              relationship = "many-to-one") |>  # many countries per income group/time -> one mean
    mutate(
      vs_region = round(.data[[score_col]] - region_mean, 4),
      vs_income = round(.data[[score_col]] - income_mean, 4)
    )

  ### 3. Nearest-score peers: self-join within each time slice -----------------
  # slim: one row per (time_cols, country_code) with score only
  slim <- df |>
    select(all_of(c(time_cols, "country_code")), .score = !!sym(score_col)) |>
    filter(!is.na(.score))

  # INNER self-join on time_cols: every country paired with every other country
  # in the same time slice; self-pairs removed by filter(country_code != peer_code)
  peers_long <- slim |>
    inner_join(
      slim |> rename(peer_code = country_code, peer_score = .score),
      by = time_cols,
      relationship = "many-to-many"  # each country pairs with every other country in the time slice
    ) |>
    filter(country_code != peer_code) |>
    mutate(dist = abs(.score - peer_score)) |>
    arrange(across(all_of(c(time_cols, "country_code"))), dist) |>
    group_by(across(all_of(c(time_cols, "country_code")))) |>
    slice_head(n = 2) |>
    mutate(peer_rank = row_number()) |>
    ungroup() |>
    select(all_of(c(time_cols, "country_code")), peer_rank, peer_code, peer_score)

  # Pivot to wide: one row per (time_cols, country_code) with peer1/peer2 columns
  peers_wide <- peers_long |>
    pivot_wider(
      id_cols     = all_of(c(time_cols, "country_code")),
      names_from  = peer_rank,
      values_from = c(peer_code, peer_score),
      names_glue  = "peer{peer_rank}_{.value}"
    ) |>
    rename(
      peer1_code  = peer1_peer_code,
      peer1_score = peer1_peer_score,
      peer2_code  = peer2_peer_code,
      peer2_score = peer2_peer_score
    )

  # LEFT join peers back: match on (time_cols, country_code)
  # NA when fewer than 2 scored peers exist in that time slice
  df |> left_join(peers_wide, by = c(time_cols, "country_code"),
                  relationship = "one-to-one")  # one peers_wide row per country/time slice
}

# Write a data frame back to a gold table (overwrite)
write_gold <- function(df, table_name) {
  df <- mutate(df, score_ts = Sys.time())
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS gold")
  dbWriteTable(con, DBI::Id(schema = "gold", table = table_name),
               df, overwrite = TRUE)
  chg_cols  <- sum(grepl("^(chg|sig)_", names(df)))
  peer_cols <- sum(grepl("^(peer|vs_|region_mean|income_mean)", names(df)))
  message(sprintf("  Written: gold.%s (%d rows, %d change cols, %d peer/group cols)",
                  table_name, nrow(df), chg_cols, peer_cols))
}

# COUNTRY METADATA ####

# Pull region and income_group from wb_countries_raw.
# Falls back to NAs so all downstream joins still work if WB meta not ingested.
has_wb_meta <- nrow(dbGetQuery(con,
  "SELECT 1 FROM information_schema.tables
   WHERE table_schema = 'bronze' AND table_name = 'wb_countries_raw'")) > 0

if (has_wb_meta) {
  country_meta <- dbGetQuery(con, "
    SELECT cf.iso3c AS country_code,
           wb.region, wb.income_level AS income_group
    FROM bronze.country_frame cf
    LEFT JOIN bronze.wb_countries_raw wb ON cf.iso3c = wb.iso3c
  ") |> as_tibble()
} else {
  message("  WARNING: bronze.wb_countries_raw not found -- peer/group context will be NA.")
  country_meta <- dbGetQuery(con,
    "SELECT iso3c AS country_code FROM bronze.country_frame"
  ) |>
    as_tibble() |>
    mutate(region = NA_character_, income_group = NA_character_)
}

message(sprintf("  Country metadata: %d countries, %d with region, %d with income_group",
  nrow(country_meta),
  sum(!is.na(country_meta$region)),
  sum(!is.na(country_meta$income_group))))

## ANNUAL TABLES ####

message("--- Changes: annual tables ---")

### D1 ####
d1 <- dbGetQuery(con, "SELECT * FROM gold.d1_macro_posterior") |> as_tibble()

d1 |>
  add_annual_changes("risk_score", "risk_score_q05", "risk_score_q95") |>
  add_peer_context("risk_score", c("year"), country_meta) |>
  write_gold("d1_macro_posterior")

### D2 annual ####
d2a <- dbGetQuery(con, "SELECT * FROM gold.d2_annual_posterior") |> as_tibble()

d2a |>
  add_annual_changes("risk_score", "risk_score_q05", "risk_score_q95") |>
  add_peer_context("risk_score", c("year"), country_meta) |>
  write_gold("d2_annual_posterior")

### D3 annual ####
d3a <- dbGetQuery(con, "SELECT * FROM gold.d3_annual_posterior") |> as_tibble()

d3a |>
  add_annual_changes("risk_score", "risk_score_q05", "risk_score_q95") |>
  add_peer_context("risk_score", c("year"), country_meta) |>
  write_gold("d3_annual_posterior")

### D4 annual ####
d4a <- dbGetQuery(con, "SELECT * FROM gold.d4_annual") |> as_tibble()

d4a |>
  add_annual_changes("d4_score", "d4_score_q05", "d4_score_q95") |>
  add_peer_context("d4_score", c("year"), country_meta) |>
  write_gold("d4_annual")

### Risk tiers (no CI -- use tier_num delta) ####
tiers <- dbGetQuery(con, "SELECT * FROM gold.risk_tiers") |>
  as_tibble() |>
  arrange(country_code, year)

for (n in c(1L, 3L, 5L)) {
  suffix <- paste0(n, "yr")
  tiers <- tiers |>
    group_by(country_code) |>
    mutate(
      !!paste0("chg_", suffix) := tier_num - lag(tier_num, n),
      !!paste0("sig_", suffix) := mapply(
        tier_change_label,
        tier_now  = tier_num,
        tier_then = lag(tier_num, n),
        SIMPLIFY  = TRUE
      )
    ) |>
    ungroup()
}

# Peer context on tier_num (ordinal -- nearest tier peers)
tiers |>
  add_peer_context("tier_num", c("year"), country_meta) |>
  write_gold("risk_tiers")

## MONTHLY TABLES ####

message("--- Changes: monthly tables ---")

### D2 monthly ####
d2m <- dbGetQuery(con, "SELECT * FROM gold.d2_monthly_posterior") |> as_tibble()

d2m |>
  add_monthly_changes("risk_score", "risk_score_q05", "risk_score_q95") |>
  add_peer_context("risk_score", c("year", "month"), country_meta) |>
  write_gold("d2_monthly_posterior")

### D3 monthly ####
d3m <- dbGetQuery(con, "SELECT * FROM gold.d3_monthly_posterior") |> as_tibble()

d3m |>
  add_monthly_changes("risk_score", "risk_score_q05", "risk_score_q95") |>
  add_peer_context("risk_score", c("year", "month"), country_meta) |>
  write_gold("d3_monthly_posterior")

### D4 monthly ####
d4m <- dbGetQuery(con, "SELECT * FROM gold.d4_monthly") |> as_tibble()

d4m |>
  add_monthly_changes("d4_score", "d4_score_q05", "d4_score_q95") |>
  add_peer_context("d4_score", c("year", "month"), country_meta) |>
  write_gold("d4_monthly")

## SUMMARY ####

message("--- Changes: complete ---")
message("  Annual change columns:  chg/sig_1yr, _3yr, _5yr")
message("  Monthly change columns: chg/sig_1mo, _3mo, _6mo, _9mo, _12mo")
message("  Peer/group columns:     region, income_group, region_mean, income_mean,")
message("                          vs_region, vs_income,")
message("                          peer1_code, peer1_score, peer2_code, peer2_score")
message("  Tables updated: d1_macro_posterior, d2_annual_posterior, d3_annual_posterior,")
message("                  d4_annual, risk_tiers, d2_monthly_posterior,")
message("                  d3_monthly_posterior, d4_monthly")
