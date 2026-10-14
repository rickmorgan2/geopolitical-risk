# config.R
# ------------------------------------------------------------------------------
# Central configuration for econ-risk-pipeline.
# Sourced first by run_pipeline.R -- all scripts inherit these values via the
# shared environment created by source().
#
# API keys are read from environment variables, not stored here.
# Set them in a .env file or your shell profile:
#
#   ANTHROPIC_API_KEY=sk-ant-...
#   OPENAI_API_KEY=sk-...
#   COMTRADE_API_KEY=...
#
# Load with: readRenviron(".env")  -- called automatically in run_pipeline.R
# ------------------------------------------------------------------------------

# -- Paths ---------------------------------------------------------------------

DB_PATH               <- "data/econrisk_2015.duckdb"
COMTRADE_CSV_DIR      <- "data/comtrade_csv"         # intermediate CSVs from download step
COMTRADE_PARQUET      <- "data/comtrade_parquet"     # hive-partitioned final parquet
COMTRADE_SUMMARY_DIR  <- "data/comtrade_summaries"   # CIF/FOB ratio cache
COMMODITY_CODES_CSV   <- "data/all-commodity-codes.csv"
CROSSWALK_CSV         <- "data/comtrade_price_crosswalk.csv"
COMTRADE_QTY_CODES_CSV <- "data/comtrade_qty_codes.csv"
PINK_SHEET_XLSX       <- "data/raw_pink_sheet_price/CMO-Historical-Data-Monthly.xlsx"
COUNTRY_FRAME_CSV     <- "country_code_frame.csv"
MODEL_CACHE_DIR       <- "R/models/cache/"            # brms .rds cache files

# -- Data window ---------------------------------------------------------------
# 2010 allows rolling-window burn-in (24m weighted vol, 12m YoY) before the
# first silver output year. TS_START_YR filters what gets written to silver
# tables; currently matches START_YR so the full history is preserved.
START_YR            <- 2010L
TS_START_YR         <- 2010L
M_TS_START_YR       <- 2015L    # monthly series start: earlier years have sparse Comtrade coverage

# -- Comtrade quality thresholds -----------------------------------------------

CMD_MIN_USD         <- 1e5     # minimum annual trade value to enter rolling calcs
YOY_CAP             <- 500     # YoY % change winsorized at this value (pp)

# -- Bayesian model settings ---------------------------------------------------

# "pathfinder" = fast approximation (development / routine updates)
# "sampling"   = full MCMC (quarterly / publication-quality runs)
BAYES_ALGO          <- "sampling"

BAYES_CHAINS        <- 4L
BAYES_ITER          <- 2500L
BAYES_WARMUP        <- 1000L
BAYES_SEED          <- 42L
BAYES_CORES         <- parallel::detectCores() - 1L
BAYES_BACKEND       <- "cmdstanr"   # or "rstan"

# -- Tier classification -------------------------------------------------------

TRIGGER_SINGLE      <- 0.975   # single-dim score that immediately triggers Critical (top 2.5%)
FLAG_BOOST          <- TRUE    # boost one tier when context flags are active
TIER_YEAR           <- NULL    # set to e.g. 2024L to restrict tier output to one year

# -- Narrative layer -----------------------------------------------------------

# API keys read from environment -- never hardcode here
ANTHROPIC_API_KEY   <- Sys.getenv("ANTHROPIC_API_KEY")
OPENAI_API_KEY      <- Sys.getenv("OPENAI_API_KEY")
COMTRADE_API_KEY    <- Sys.getenv("COMTRADE_API_KEY")
# COMTRADE_API_KEY    <- Sys.getenv("COMTRADE_API_KEY_alt")

NARRATIVE_YEAR      <- NULL    # set dynamically in compute_narratives.R from max year in gold.risk_tiers

# Per-dimension model config (api, model, max_tokens)
DIM_CONFIG <- list(
  d1  = list(api = "anthropic", model = "claude-haiku-4-5-20251001",   max_tokens = 600),
  d2  = list(api = "anthropic", model = "claude-haiku-4-5-20251001",   max_tokens = 600),
  d3  = list(api = "anthropic", model = "claude-haiku-4-5-20251001",   max_tokens = 600),
  d4  = list(api = "anthropic", model = "claude-haiku-4-5-20251001",   max_tokens = 600),
  ovw = list(api = "anthropic", model = "claude-sonnet-4-6", max_tokens = 1600)
)

# -- Validation ----------------------------------------------------------------

if (ANTHROPIC_API_KEY == "" && any(sapply(DIM_CONFIG, `[[`, "api") == "anthropic")) {
  message("  WARNING: ANTHROPIC_API_KEY not set -- narrative layer will fail.")
}
if (OPENAI_API_KEY == "" && any(sapply(DIM_CONFIG, `[[`, "api") == "openai")) {
  message("  WARNING: OPENAI_API_KEY not set -- narrative layer will fail.")
}

message(sprintf(
  "  [config] BAYES_ALGO=%s | START_YR=%d | TIER_YEAR=%s | NARRATIVE_YEAR=derived at runtime",
  BAYES_ALGO, START_YR,
  ifelse(is.null(TIER_YEAR), "all", as.character(TIER_YEAR))
))

naCountFun <- function(dat, exclude_year){
  dat %>%
    dplyr::filter(year < exclude_year) %>%
    sapply(function(x) sum(is.na(x))) %>%
    sort()
}
# naCountFun(pink_sheet_prices |> rename(year = yr), 2026)
# str(enriched)
# 