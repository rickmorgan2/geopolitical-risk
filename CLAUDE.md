# CLAUDE.md

Guidance for Claude Code (and anyone new) working in this repository.

## What this is

An R pipeline that scores ~175 countries on four dimensions of economic risk and feeds a Shiny dashboard. The project is **Pressure Points — Economic Shocks and Conflict Forecasts** (DECISIONS.md D-024).

- **D1 Macro-fiscal stability**: IMF WEO / World Bank WDI indicators.
- **D2 Strategic import reliance**: food, energy and fertilizer import concentration and price disruption, from UN Comtrade.
- **D3 Export fragility**: commodity export concentration and price exposure, from UN Comtrade.
- **D4 Network contagion**: trade-weighted partner risk.

Scores come from Bayesian models (brms / Stan), then go through tiers, change signals and LLM-written narratives.

The headline output is the extra risk of organized violence that external economic shocks add, per country-month, with a credible interval and split by channel: food, fuel, fertilizer and export prices (D-034). Underneath sits a forecast of UCDP organized violence, 1–36 months ahead, benchmarked against ViEWS (D-018 to D-022). It is the engine that computes the extra risk and the check that the estimate is believable. D2 and D3 measure exposure; D1 measures buffers. The plan is in the design doc (https://claude.ai/artifact/NENMJC2hgoaRMhU16RCQP1); no forecast modeling starts until the owner has reviewed it.

The core idea, from the owner's earlier research (the Morgan–Reinhardt commodity paper at the repo root): price volatility in a country's import and export baskets raises the risk of instability.

## Read first

- `LOG.md`: dated work log. Start with the latest entry.
- `DECISIONS.md`: what was decided and why, with status.
- End a session with `/close-up-shop` (`.claude/skills/close-up-shop/`). It adds the LOG entry, including the open threads, and keeps DECISIONS.md and this file current. It never commits.
- The approved remediation plan is summarized in the 2026-11-20 LOG entry. Known defects (model structure, monthly aggregation, dashboard charts) are listed there. Don't assume current outputs are correct.

## Layout and data flow

```
config.R                   all paths and parameters -- single source of truth
run_pipeline.R             orchestrator (sources the scripts below in order)
R/ingest_*.R               bronze: raw sources -> DuckDB bronze.* tables
  ingest_comtrade_download.R  step 0a: Comtrade API -> data/comtrade_csv*/ (core, ext) + manifests
  ingest_comtrade_build.R     step 0b: CSVs -> data/comtrade_parquet (remainders, CIF/FOB, mirroring; nominal values)
  ingest_price_series.R       step 0c: prices in tiers (Pink Sheet, IMF via FRED, Comtrade unit values) and MUV deflation, joined into the parquet
R/deflator.R, R/pink_sheet.R, R/price_helpers.R   helpers for step 0c: MUV index; Pink Sheet reading, precision splice and download_pink_sheet(); FRED cache, unit values, fallbacks, enrichment
R/compute_silver_*.R, R/transform_silver.R   silver: derived indicators
R/models/*.R               gold: Bayesian models (D1-D3) and D4 network
R/compute_context_layer.R, compute_tiers.R, compute_changes.R, compute_narratives.R
shiny/                     dashboard (global.R, ui.R, server.R, www/)
tests/testthat/            offline tests (no network, no data/)
```

The dashboard is being rethought (D-039). Its replacement will be a self-hosted, free public app, not Shiny on shinyapps.io (D-037). A scenario-tool prototype was built and archived on 2026-11-25. It lives in `archive/scenario/` (gitignored; see its README). Don't restore or extend it unless the owner asks.

The DuckDB file is `data/georisk.duckdb` (`DB_PATH` in `config.R`), with schemas `bronze`, `silver` and `gold`. `data/` is gitignored and large (~17 GB with the Comtrade CSVs). The app reads a separate copy at `shiny/data/georisk.duckdb`.

## Running things

- **Tests:** `Rscript -e 'testthat::test_dir("tests/testthat")'` from the repo root.
- **Pipeline:** open the `.Rproj` (or `setwd` to the repo root), then `source("run_pipeline.R")`. Most steps in `run_pipeline.R` are currently commented toggles. A single script runs once `config.R` is sourced and `con` exists (see README "Partial runs").
- **Comtrade download:** `source("run_comtrade_update.R")` runs the agreed sequence step by step (D-023). Source it once a day; it resumes and shares one daily budget. Single calls: `source("R/ingest_comtrade_download.R")`, then `run_comtrade_download(<year>)` or `run_comtrade_world_download(<years>)`.
  - Two code sets (D-027), set by `code_set` in `data/all-commodity-codes.csv`: `core` (174 codes, from 2010) and `ext` (55 codes, from 2013 with 2013–2014 as burn-in only, in their own folders and manifest via `run_comtrade_ext_download()`). Don't edit `run_comtrade_update.R` while a background run is using it: Rscript reads the script as it goes.
  - Use `dry_run = TRUE` (or `PREVIEW_ONLY <- TRUE` before sourcing the step script) to preview without calling the API.
  - Only one R session may download at a time: two runs overwrite each other's manifest.
  - The free tier allows 500 calls/day and 100k rows per call.
  - Never list partner countries in the request: URLs around 2,000 characters get HTTP 414. See D-003.
- **Comtrade build and prices:** `run_comtrade_update.R` runs the build steps once downloads are complete. For a full rebuild, set `FORCE_UPDATE <- TRUE` before sourcing `R/ingest_comtrade_build.R`.
  - Before a rebuild, refresh the Pink Sheet with `download_pink_sheet()` (source `config.R` and `R/pink_sheet.R`). The previous monthly file is kept for the precision splice.
  - Prices come from `price_source` in `data/comtrade_price_crosswalk.csv` (D-029). Codes marked `none`, and broad headings, count for exposure only. Manufactured codes (`manufactured` = 1: chips, magnets, arms, explosives) stay out of commodity totals, shares and baskets and have their own table, `bronze.comtrade_manufactured_trade` (D-032).
  - Both the build and the price step write to a staging folder and swap it in. A failed check leaves the parquet unchanged.
- **Models:** use `BAYES_ALGO = "pathfinder"` for development and `"sampling"` for publication. A full MCMC run took about 8.5 hours. The brms cache lives in `R/models/cache/` (gitignored).
- **App:** `shiny::runApp("shiny")`.

## Conventions

- **Country key:** `iso3c` in bronze, `country_code` in silver and gold. Time keys are `year` and `month`; Comtrade raw uses `yr` and `mon`.
- **Writing tables:** use `write_silver()` and `write_gold()`, which overwrite whole tables and stamp `transform_ts` / `score_ts`.
- **Paths and parameters:** take them from `config.R`; never hard-code a path in a script.
- **Start year:** 2015 for every output and model fit (`START_YR`, D-027). Rolling windows may read 2013–2014 only as burn-in.
- **Spelling:** American English in all project text: docs, comments, logs, the design doc. Never respell code identifiers or source data values; `summarise()` and Pink Sheet series names stay as they are.
- **Secrets:** API keys live in `.env`, which is gitignored. Never print, read out, or edit `.env`.
- **Database inspection:** query a scratch copy of the DuckDB file in read-only mode, so you never lock or alter the live database.
- **Maps:** Crimea is part of Ukraine on every map, checked by a test. It is the only disputed territory that gets special handling (D-038).
- **Dashboard style:** light theme on a gray page with white panels; no dark backgrounds.
- **Data use:** credit every source; anything a public app ships holds derived values only, never Comtrade records or vendor prices (D-037).

## Working rules from the owner

- The owner knows these data and what "right" looks like. Verify the math, and show evidence (queries, numbers) for claims about the data or models.
- Never bring back code the owner removed. Check LOG.md, DECISIONS.md, git history and `archive/` first, and ask if unsure.
- Retire files by moving them to `archive/<date>_<topic>/`, keeping their repo paths; never delete them (D-031). Keep bulk edits (sed, find) out of every `archive/` folder.
- Plan before changing anything non-trivial, and get approval.
- Do not commit or push unless explicitly asked.
- Legacy organization branding is gone from the files and from git history (D-001, rewritten and force-pushed 2026-11-23). Do not add organization names, logos or branding. CSS color tokens are `--brand-*`. The project name is in D-024.
