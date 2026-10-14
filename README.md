# Economic Risk Data Pipeline

base directory: econ-risk-pipeline

The economic risk data pipeline produces all of the indicators and indices needed for use in a Shiny dashboard.

I used the R `{brms}` package to draft a set of Bayesian factor analysis models to calculate risk estimates and uncertainty bounds around three dimensions of economic risk macro, import reliance, and export fragility. I then calculated a network contagion risk score for each country, derived form the risk profiles of the country's trading partners. I used DuckDB for SQL-layer computation and standard R packages for data management and wrangling. All pipeline artifacts are stored in a hive-partitioned DuckDB database.

The pipeline ingests data from multiple public sources, transforms it through a Medallion architecture (Bronze -> Silver -> Gold), and produces scored, tiered, and narrated country risk profiles. It builds on a long chain of fits and starts around the idea that price volatility in a country's import and export baskets, triggered by geopolitical events and disruptions in international markets increases political instability and the risk of civil unrest and conflict. 

I have a lot of experience working with the data sources pulled into this project. Indeed, many of the ingest scripts have origins in other projects that I've done over the years. I unified them for this framework using Anthropic AI's Claude web application. I didn't use Claude Code. Instead, I used Claude (Sonnet 4.6) for code review and refactoring inefficient code. This was very helpful – especially with the SQL code. Claude saved me here. The raw input is 24,000,000 rows. I tried to use `{arrow}` and `{DT}`, but they kept failing. SQL was the necessary move, but I didn't have the skill level needed. Claude plussed me up. 

Helpful, but very dangerous. I know these data. I know what "right" looks like. There were many occasions where Claude was wrong with the math or produced duplicate code (among other quarks). It also had a tendency to resurface elements of a script that had been purposefully removed by me, the human, who knows what their doing. But, here is not where I debate GenAI, but to remind  future me and others who might stumble on this to always check the work of a tool that can't be held accountable for mistakes. 

---

## What it produces

Each pipeline run writes a fully updated `econrisk_2015.duckdb` containing:

| Layer | What is in it |
|---|---|
| **Bronze** | Raw ingested tables from IMF WEO, IMF PCPS, World Bank WDI/WGI, WTO, UN Comtrade, USGS, World Bank Pink Sheet, US Census IDB, GDP/population local files |
| **Silver** | Derived indicators: rolling HHIs, volatility metrics, network weights, monthly disruption signals, commodity price enrichment |
| **Gold** | Bayesian posterior scores (D1-D4), risk tiers, change signals, narratives |

The four risk dimensions scored are:

- **D1 Macro-Fiscal Stability** - reserve adequacy, debt burden, currency stress
- **D2 Strategic Import Reliance** - food and commodity import concentration and vulnerability
- **D3 Export Fragility** - commodity export dependence and price exposure
- **D4 Network Contagion Risk** - trade-weighted partner risk propagation

---

## Repository structure

```
econ-risk-pipeline/
|-- indicator_guide.md          # full indicator reference
|-- config.R                          # all configuration -- edit here first
|-- run_pipeline.R                    # top-level orchestrator
|-- country_code_frame.csv            # 175 sovereign states with iso3c keys
|-- comtrade_pinksheet_crosswalk.csv  # maps HS codes to Pink Sheet/IMF PCPS price series
|-- comtrade_qty_codes.csv            # Comtrade quantity unit code reference
|
|-- R/
|   |-- ingest_comtrade_download.R  # step 0a: download CSVs from Comtrade API
|   |-- ingest_comtrade_build.R     # step 0b: CSVs to parquet, CIF/FOB correction, mirror gaps
|   |-- ingest_price_series.R       # step 0c: Pink Sheet + IMF PCPS prices; enriches parquet in-place
|   |-- ingest_comtrade.R           # bronze: read enriched parquet into DuckDB
|   |-- ingest_imf.R                # bronze: IMF WEO via API
|   |-- ingest_worldbank.R          # bronze: World Bank WDI via wbstats
|   |-- ingest_wgi.R                # bronze: World Governance Indicators
|   |-- ingest_wto.R                # bronze: WTO tariff data
|   |-- ingest_usgs.R               # bronze: USGS Mineral Commodity Summaries
|   |-- ingest_gdp.R                # bronze: GDP from local CSV files (context/normalization)
|   |-- ingest_population.R         # bronze: population from US Census Bureau IDB API
|   |
|   |-- compute_silver_comtrade.R   # silver: trade concentration + monthly disruption indicators
|   |-- transform_silver.R          # silver: macro and trade derived ratios
|   |-- compute_silver_usgs.R       # silver: mineral supply concentration
|   |-- compute_context_layer.R     # context: sparse indicators as annotations
|   |-- compute_tiers.R             # gold: 5-tier risk classification
|   |-- compute_changes.R           # gold: lookback change signals (chg_*/sig_*)
|   |-- compute_narratives.R        # gold: LLM-generated country narratives
|   |
|   |-- models/
|       |-- compute_bayes_d1_macro.R
|       |-- compute_bayes_d2_annual.R
|       |-- compute_bayes_d2_monthly.R
|       |-- compute_bayes_d3_annual.R
|       |-- compute_bayes_d3_monthly.R
|       |-- compute_d4_network.R
|       |-- cache/                  # brms model cache -- gitignored
|
|-- data/                           # gitignored -- must be created locally
    |-- econrisk_2015.duckdb
    |-- comtrade_csv/               # intermediate CSVs from download step
    |-- comtrade_parquet/           # hive-partitioned: iso3c={}/yr={}/ (price-enriched)
    |-- comtrade_summaries/         # CIF/FOB ratio cache
    |-- all-commodity-codes.csv
    |-- raw_usgs/                   # USGS MCS CSVs cached from ScienceBase (auto-created)
    |-- raw_wgi/                    # manually downloaded WGI Excel file (see ingest_wgi.R)
    |-- raw_wto/                    # WTO tariff ZIPs cached from WTO website (auto-created)
    |-- raw_pink_sheet_price/       # manually downloaded World Bank Pink Sheet Excel file
    |-- gdp_raw/                    # manually obtained GDP CSV files keyed on gwcode
    
```

---

## Prerequisites

### R packages

Install all dependencies at once:

```r
install.packages(c(
  # infrastructure
  "DBI", "duckdb", "dplyr", "tidyr", "readr", "purrr", "stringr", "glue",
  "lubridate",
  # data ingestion
  "httr2", "jsonlite", "wbstats", "countrycode", "comtradr", "rsdmx",
  # parquet I/O
  "arrow",
  # time series and rolling
  "zoo",
  # Bayesian models
  "brms", "cmdstanr",
  # network
  "igraph",
  # documentation
  "rmarkdown", "knitr", "kableExtra"
))
```

Install CmdStan (required by brms):

```r
cmdstanr::install_cmdstan()
```

### API access

| Service | Required for | How to obtain |
|---|---|---|
| **UN Comtrade** | `ingest_comtrade_download.R` (step 0a only) | [comtradeplus.un.org](https://comtradeplus.un.org) -- free tier (500 calls/day) |
| **US Census Bureau IDB** | `ingest_population.R` | [api.census.gov](https://api.census.gov/data/key_signup.html) -- free |
| **Anthropic** | `compute_narratives.R` | [console.anthropic.com](https://console.anthropic.com) |
| **OpenAI** | `compute_narratives.R` (if any dimension routed to OpenAI in `config.R`) | [platform.openai.com](https://platform.openai.com) |

IMF, World Bank, WGI, WTO, and USGS are all public APIs requiring no key.

---

## Configuration

All pipeline settings live in `config.R`. The most commonly changed options:

```r
# Bayesian algorithm: "pathfinder" (fast) or "sampling" (full MCMC)
BAYES_ALGO    <- "sampling"

# Parallel chains -- defaults to all cores minus one
BAYES_CORES   <- parallel::detectCores() - 1L

# Stan backend: "cmdstanr" (recommended) or "rstan"
BAYES_BACKEND <- "cmdstanr"

# Year for tier output (NULL = all years in the posterior tables)
TIER_YEAR     <- NULL

# Narrative year is derived automatically at runtime from the most recent
# scored year in gold.risk_tiers -- no manual update needed.
```

Use `"pathfinder"` for routine updates and development. Switch to `"sampling"` for quarterly publication-quality runs. The brms model cache (`R/models/cache/`) means full MCMC only re-runs when data or model specification changes.

### API keys

Create a `.env` file in the repo root (ensure `.env` is in `.gitignore`):

```
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_API_KEY=sk-...
COMTRADE_API_KEY=...
CENSUS_API_KEY=...
```

`run_pipeline.R` calls `readRenviron(".env")` automatically if the file exists.

---

## Running the pipeline

### First-time setup

1. Clone the repo and set it as your working directory.

2. Create the `data/` directory:

```bash
mkdir -p data
```

3. Obtain the Comtrade parquet data. Two options:

   **Option A -- Download fresh** (requires free Comtrade API key): Uncomment the step 0 lines in `run_pipeline.R`:
   ```r
   source("R/ingest_comtrade_download.R")   # step 0a: download CSVs
   source("R/ingest_comtrade_build.R")      # step 0b: build parquet
   source("R/ingest_price_series.R")        # step 0c: enrich parquet with prices
   ```
   Step 0a writes one CSV per country-year to `data/comtrade_csv/`.
   Step 0b converts those CSVs to hive-partitioned parquet in `data/comtrade_parquet/`, applies CIF/FOB adjustment, and mirrors missing bilateral flows.
   Step 0c joins Pink Sheet and IMF PCPS commodity prices into the parquet in-place.

   Free API tier: 500 calls/day. 173 countries x 2 commodity chunks = 346 calls per year of data. Full history (2010-present) takes approximately 11 days.
   `skip_existing = TRUE` in `ingest_comtrade_download.R` lets you resume across sessions without re-downloading completed files.

   **Option B -- Copy existing data** from a prior run or shared storage:
   ```bash
   cp -r /path/to/comtrade_parquet  data/comtrade_parquet
   cp    /path/to/all-commodity-codes.csv  data/all-commodity-codes.csv
   ```
   Steps 0b and 0c are not needed if you copy a fully built and price-enriched parquet directory.

4. Copy `country_code_frame.csv`, `comtrade_pinksheet_crosswalk.csv`, and `comtrade_qty_codes.csv` to the repo root if not already present.

5. Place manual data files (see "What collaborators need to supply" below).

6. Create and populate `.env` with your API keys.

### Full pipeline run

```r
setwd("econ-risk-pipeline")  # or open the .Rproj file
source("run_pipeline.R")
```

Runtime breakdown (approximate, on a modern laptop):

| Stage | Full MCMC |
|---|---|
| Bronze ingest |  10-20 min |
| Silver transforms | 5-10 min |
| D1 macro model | ~20 min |
| D2 annual | ~30 min |
| D2 monthly | ~6 hr |
| D3 annual | ~30 min |
| D3 monthly | ~6 hr  |
| D4 network | 2 min |
| Tiers + changes | < 1 min |
| Narratives (175 countries) |  15-30 min |

Full MCMC model run takes approximately 14 hours. brms file cache is helpful for resampling -- completed models are not re-run unless data or model specification changes.

Use `BAYES_ALGO <- "pathfinder"` in `config.R` for development updates.

### Partial runs

Each script is self-contained once `con` exists. To re-run only the narrative layer after editing prompt text:

```r
source("config.R")
con <- DBI::dbConnect(duckdb::duckdb(), DB_PATH)
source("R/compute_narratives.R")
DBI::dbDisconnect(con, shutdown = TRUE)
```

---

## Pipeline execution order

The diagram below shows the dependency chain enforced by `run_pipeline.R`. An arrow means "must complete before."

```
[.env + config.R]
        |
        v
[country_code_frame.csv] --> [bronze.country_frame]
        |
        |-- Step 0a: ingest_comtrade_download.R  (optional -- downloads CSVs)
        |-- Step 0b: ingest_comtrade_build.R     (optional -- CSVs to parquet)
        |-- Step 0c: ingest_price_series.R       (optional -- enriches parquet with prices)
        |
        v
[Bronze ingest]
  ingest_imf.R
  ingest_worldbank.R
  ingest_wgi.R
  ingest_wto.R
  ingest_usgs.R
  ingest_gdp.R
  ingest_population.R
  ingest_comtrade.R    <-- reads price-enriched data/comtrade_parquet/
        |
        v
[Silver transforms]
  compute_silver_comtrade.R   <-- group metrics + monthly disruption indicators
  transform_silver.R
  compute_silver_usgs.R
        |
        v
[Gold: Bayesian models]
  compute_bayes_d1_macro.R
  compute_bayes_d2_annual.R  \
  compute_bayes_d2_monthly.R  |-- independent, can run in any order
  compute_bayes_d3_annual.R   |
  compute_bayes_d3_monthly.R /
  compute_d4_network.R
        |
        v
[Gold: scoring and annotation]
  compute_context_layer.R
  compute_tiers.R
  compute_changes.R          <-- requires all gold model tables
        |
        v
[Gold: narratives]
  compute_narratives.R       <-- reads pre-computed changes
```

---

## Data sources

| Source | Data | Update frequency | Access |
|---|---|---|---|
| IMF WEO | GDP, inflation, current account, debt | Twice yearly (Apr/Oct) | Public API |
| IMF PCPS | Primary commodity prices | Monthly | Public SDMX API |
| World Bank WDI | Reserves, exchange rates, poverty | Annual (July) | Public API via `wbstats` |
| World Bank WGI | Governance indicators | Annual | Public Excel download |
| World Bank Pink Sheet | Commodity price series | Monthly | Manual Excel download |
| WTO | Tariff schedules, trade policy | Annual | Public API |
| UN Comtrade | Bilateral trade flows by HS code | Monthly | Subscription API |
| USGS MCS | Critical mineral production and reserves | Annual (January) | Public PDF/Excel |
| US Census Bureau IDB | Country population | Annual | Public API |

### Coverage notes

- Comtrade: 173 countries, 174 HS codes, 2010-present for burn-in; monthly series from 2015 forward
- USGS: ~50 mineral commodities, country-level mine production
- WGI: 2024 is the latest available release; sourced from manual Excel download until the `wbstats` API carries the current year
- World Bank WDI: tariff indicators discontinued July 2025; see `ingest_wto.R` for current sourcing
- World Bank Pink Sheet: download `CMO-Historical-Data-Monthly.xlsx` from [worldbank.org/en/research/commodity-markets](https://www.worldbank.org/en/research/commodity-markets) and place at `data/raw_pink_sheet_price/`

---

## What collaborators need to supply

The following are excluded from the repository and must be obtained or created locally:

| Item | Why excluded | Where to get it |
|---|---|---|
| `data/econrisk_2015.duckdb` | Too large; regenerated by pipeline | Run `run_pipeline.R` |
| `data/comtrade_csv/` | Too large; intermediate download files | Run `ingest_comtrade_download.R` or skip if copying parquet directly |
| `data/comtrade_parquet/` | Too large; ~10-50 GB depending on history | Run `ingest_comtrade_build.R` + `ingest_price_series.R`, or copy from shared storage |
| `data/comtrade_summaries/` | Regenerated by build step | Run `ingest_comtrade_build.R` |
| `data/all-commodity-codes.csv` | Companion to parquet; provided on request | Contact repo maintainer |
| `R/models/cache/` | Regenerated by brms on first model run | Run pipeline |
| `data/raw_usgs/` | Auto-created; ScienceBase CSVs cached by ingest script | Run `ingest_usgs.R` (auto-created on first run) |
| `data/raw_wgi/` | Manually downloaded Excel file; not auto-retrieved | Download from [World Bank WGI](https://www.worldbank.org/en/publication/worldwide-governance-indicators) and place in this folder |
| `data/raw_wto/` | Auto-created; WTO tariff ZIPs cached by ingest script | Run `ingest_wto.R` (auto-created on first run) |
| `data/raw_pink_sheet_price/` | Manually downloaded Excel file; not auto-retrieved | Download `CMO-Historical-Data-Monthly.xlsx` from [worldbank.org/en/research/commodity-markets](https://www.worldbank.org/en/research/commodity-markets) |
| `data/gdp_raw/` | Local CSV files keyed on gwcode; not auto-retrieved | Contact repo maintainer |
| `.env` | Contains private API keys | Create from template above |

---

## Risk tier definitions

| Tier | Label | Meaning |
|---|---|---|
| 5 | Critical | Extreme multi-dimensional stress or single-dimension score >= 0.75 |
| 4 | Elevated | Broad-based pressure across dimensions |
| 3 | Guarded | Moderate and rising risk; warrants monitoring |
| 2 | Moderate | Limited stress; isolated vulnerabilities |
| 1 | Low | No meaningful stress signals |

Context flags (food import dependence, landlocked status, mineral dependence, etc.) can boost a country one tier when active.

---

## License

Private -- for authorized collaborators only.
