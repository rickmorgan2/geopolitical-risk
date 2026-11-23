# Decisions

One entry per decision.

| Status | Meaning |
|---|---|
| **Accepted** | Agreed with the owner |
| **Proposed** | Recommended, awaiting confirmation |
| **Open** | To be settled later |
| **Superseded** | Replaced by a later decision |

Newest entries at the bottom. Reference them by ID (e.g. D-003) in code comments, LOG.md, and commit messages.

---

## D-001 — Remove legacy organization branding
- **Date:** 2026-11-20
- **Status:** Accepted. Done 2026-11-23 apart from the `.env` key name. History rewritten in a fresh clone, verified, and force-pushed to GitHub `main` (`d886405`) on the owner's go-ahead; this repo re-synced. The backup mirror of the original history was deleted afterward at the owner's request. Tracked files done and committed 2026-11-23: a "Pressure Points" text wordmark replaces the logo; the logo and legacy CSS files are archived (D-031); UI text, the indicator guide and the CSS header are renamed; the legacy-named CSS tokens are `--brand-*` and its classes are `app-main`, `text-brand-*` and `bg-brand-*`; two unused legacy-named helpers in `global.R` and the commented key line in `config.R` are deleted. Still to do: renaming the old key in `.env` (owner).
- **Context:** The repo and dashboard carry a former organization's name and logo.
- **Decision:**
  - Remove every reference from tracked files: UI text, logo, CSS token and class names, unused helpers, docs.
  - Rewrite git history so past commits are clean too.
  - The gitignored `docs/` and `archive/` folders stay untouched.
  - New files and commit messages say "legacy branding" and never repeat the name.
- **Consequences:**
  - The history rewrite needs a force-push to GitHub. Anyone with a clone must re-clone.
  - A mirror backup is taken first.
  - The force-push happens only after the owner's explicit go-ahead.

## D-002 — Dashboard name
- **Date:** 2026-11-20
- **Status:** Superseded by D-024.
- **Decision:** "Undercurrent — Country Economic Risk Monitor", as a text wordmark with no logo image. Alternates considered: Tremor, Strata, Fault Line.
- **Consequences:** The current color palette stays, under neutral `--brand-*` token names.

## D-003 — Comtrade download design
- **Date:** 2026-11-20
- **Status:** Accepted. Implemented in `R/ingest_comtrade_download.R`, committed 2026-11-23.
- **Context:**
  - Requests listed all ~172 partners explicitly, making URLs about 2,000 characters.
  - Comtrade began rejecting these with HTTP 414. The same URL had worked in March 2026.
  - The 8 KB resume rule re-requested 978 valid "no data" answers on every run.
  - Partial or header-less files could be left behind.
  - Recent years were never refreshed.
- **Decision:**
  - Request `partner = "everything"` (about 890 characters), then keep the historical partner set in R (country frame minus the reporter).
  - Halve the commodity chunk on HTTP 414 or at the 100k-row cap.
  - Write each country-year in one piece (temp file + rename) once every call succeeds.
  - Record outcomes in `data/comtrade_download_manifest.csv`.
  - Skip closed years once `ok` or `no_data`. Refresh the current and previous year after 30 days.
  - Stop cleanly at 480 calls a day, or on HTTP 401/403/429.
- **Consequences:**
  - The entry point is `run_comtrade_download(years, ...)`.
  - The old `download_comtrade_data()` now only points to the new function.

## D-004 — Mirrored flows are rebuilt from self-reported data
- **Date:** 2026-11-20
- **Status:** Accepted. Implemented in `R/ingest_comtrade_build.R`, committed 2026-11-23.
- **Context:**
  - The build processed only missing partitions, so refreshed CSVs were ignored.
  - It mirrored only country-months with no rows at all, so old mirrors were never updated.
- **Decision:**
  - Reprocess any CSV newer than its parquet partition.
  - Judge what is "missing" from self-reported rows only.
  - Recompute all mirrored rows for every year a build touches.
  - Delete partitions left empty.
- **Consequences:** After a backfill or refresh, partner-filled data improve automatically.

## D-005 — One parquet path
- **Date:** 2026-11-20
- **Status:** Accepted. Implemented, committed 2026-11-23.
- **Decision:** `compute_silver_comtrade.R` reads `COMTRADE_PARQUET` from `config.R` (`data/comtrade_parquet`) instead of a hard-coded sibling-project path.
- **Context:** On 2026-11-20 the two copies were identical (28 columns, 19,660,961 rows), so outputs don't change. Without this, new downloads would never reach the silver layer.

## D-006 — Bayesian model engines
- **Date:** 2026-11-20
- **Status:** Accepted for the engine choice. Model details are Proposed (plan Phase 4).
- **Context:**
  - The current models estimate no latent variable.
  - Fitted values are additive country + global-time effects.
  - As a result, country rankings never change and monthly series step every January.
- **Decision:**
  - **Annual models (D1, D2, D3):** brms non-linear one-factor measurement models (indicator loadings, Student-t errors) with a country-year latent term.
  - **Monthly models (D2, D3):** custom Stan via cmdstanr, using a dynamic factor model with a per-country AR(1) latent state and a smooth global path.
- **Consequences:** Stays within the latent-variable paradigm, and makes it real.

## D-007 — The dashboard plots data, models supply scores
- **Date:** 2026-11-20
- **Status:** Proposed.
- **Decision:** Chart observed silver indicators. Use model output only for scores and latent estimates, with credible bands.

## D-008 — New models run side by side
- **Date:** 2026-11-20
- **Status:** Proposed.
- **Decision:** Write v2 outputs to `gold.*_v2` tables until validated. The dashboard switches when the owner accepts them.

## D-009 — Stale files are deleted, not archived
- **Date:** 2026-11-20
- **Status:** Superseded by D-031 (owner, 2026-11-23): retired files are archived, not deleted.
- **Decision:** Delete old Shiny copies, unused CSS, superseded scripts and scratch outputs from tracked files. They remain recoverable from the pre-rewrite backup mirror.

## D-010 — WEO projections stay out of model fitting
- **Date:** 2026-11-20
- **Status:** Proposed.
- **Decision:** Fit macro models on actual years only. Projections can feed a separate outlook view.

## D-011 — Risk-score scale
- **Date:** 2026-11-20
- **Status:** Superseded by D-018. Headline outputs are now fatality forecast distributions; latent scores are intermediate inputs.
- **Recommendation:** Compute risk = Φ(θ) per posterior draw on the model's identified scale, with no renormalization on each run.

## D-012 — Measurement error reflects data quality
- **Date:** 2026-11-20
- **Status:** Accepted. Planned for Phase 4.
- **Context:**
  - The most recent months, and every month of non-reporting countries, rest on mirror data.
  - Mirror data understate totals. Fully mirrored country-months average z = −1.22 / −1.59 (Nov/Dec 2025), against +0.83 / +0.45 when self-reported.
- **Decision:** In the v2 measurement models, each cell's error scale grows with its mirror share and provisional status:

      sigma[c,t,k] = sigma_k * exp(gamma_m * mirror_share + gamma_p * provisional)

  `gamma_m` and `gamma_p` are estimated and constrained to be ≥ 0. Annual brms models get the same through a `sigma` formula, for D2 and D3.
- **Consequences:**
  - Incomplete months barely move the latent state.
  - Mirror-only countries get wider, honest credible intervals.

## D-013 — Provisional months
- **Date:** 2026-11-20
- **Status:** Accepted. Simple version in Phase 2, full flag in Phase 3.
- **Decision:**
  - A country-month is *provisional* when any of these holds:
    - it is mostly mirrored;
    - it falls within the reporting-lag window;
    - its partner coverage is low.
  - The flag clears when self-reported data arrive.
  - The dashboard marks or holds back provisional months.
  - Until the full flag exists, months with an import mirror share above 0.5 are shaded.
- **Context:** At the 2026-03-03 download, only 16 of 173 reporters had December 2025, and 96 had no 2025 data at all.

## D-014 — Keep Comtrade World totals and measure partner coverage
- **Date:** 2026-11-20
- **Status:** Accepted. World totals are kept by every download and were fetched for all earlier files (D-023). The partner-coverage measure is planned for Phase 3.
- **Decision:**
  - Save the World rows that `partner = "everything"` already returns to `data/comtrade_world/`, before the partner filter.
  - Build `bronze.comtrade_partner_coverage`: kept partners ÷ World, per country, month, flow and commodity group.
- **Context:** The partner filter drops "Areas, nes", bunkers, free zones and Taiwan ("Other Asia, nes"), so totals and HHIs are understated by an unknown amount.
- **Consequences:**
  - No extra API calls.
  - Downloads made before this change would have to be repeated to recover their World rows.

## D-015 — Unit values ignore Comtrade-estimated quantities
- **Date:** 2026-11-20
- **Status:** Accepted. Applied 2026-11-23 to the Comtrade unit-value prices (D-029); the rest is planned for Phase 3.
- **Decision:**
  - Carry `is_qty_estimated` and `is_net_wgt_estimated` into the parquet; the build currently drops them.
  - Exclude estimated quantities from unit values: the basket-volatility fallback and the price enrichment's quantities.
- **Context:** In 2024, 82% of Egypt's quantities and 26% of the US's are Comtrade estimates; 22% of Nigeria's are missing.

## D-016 — Reporter-specific valuation in mirroring
- **Date:** 2026-11-20
- **Status:** Accepted. Planned for Phase 3.
- **Decision:**
  - When a mirrored flow comes from a partner that reports FOB values, use them directly.
  - Otherwise convert with a CIF/FOB ratio estimated by commodity *and year*. This replaces the single median ratio (default 1.10).
- **Context:**
  - 2024 imports: Canada and South Africa report FOB only; Germany and India CIF only.
  - Freight costs, and with them CIF/FOB ratios, spiked in 2021–22.

## D-017 — HS edition concordance for tracked codes
- **Date:** 2026-11-20
- **Status:** Accepted. Planned for Phase 3.
- **Decision:**
  - Check the 174 tracked codes against the HS2012 → HS2017 → HS2022 concordances.
  - Map or drop the codes that changed.
  - Test for January breaks at commodity level in the switch years.
- **Context:** Reporters switch HS editions in a January: HS2017 mostly in 2017–18, HS2022 mostly in 2022. Codes that changed can break at those Januaries, independently of the model artifacts in D-006.

## D-018 — Forecast target: UCDP organized violence
- **Date:** 2026-11-20
- **Status:** Accepted.
- **Decision:** Forecast UCDP fatalities per country-month for all three violence types: state-based, non-state and one-sided.
  - Each forecast is a full predictive distribution. The probabilities of 1+, 25+ and 100+ deaths are derived from it.
  - Horizons run from 1 to 36 months.
  - Grid-level forecasts are out of scope for now.
- **Context:** ACLED's license rules it out. Unrest, coups, sovereign distress and food crises are deferred. Design doc: https://claude.ai/artifact/NENMJC2hgoaRMhU16RCQP1
- **Consequences:**
  - Tiers and composite risk scores stop being headline outputs (see D-011).
  - Outcome data come from final UCDP GED plus the monthly UCDP Candidate releases.

## D-019 — ViEWS as input and benchmark
- **Date:** 2026-11-20
- **Status:** Accepted.
- **Decision:**
  - ViEWS's forecast for the same violence type and horizon enters the model with an estimated weight. A history-only fallback covers months without it.
  - Each forecast records which ViEWS run fed it.
  - Success is measured against three nested models: (1) history only, (2) history plus ViEWS, (3) the full model. The go/no-go test is (3) beating (2).

## D-020 — Forecast model family
- **Date:** 2026-11-20
- **Status:** Accepted for the design; details may change during the build.
- **Decision:**
  - **Model:** Bayesian hurdle negative-binomial models in brms, one per violence type and horizon block (1–3, 4–6, 7–12, 13–24, 25–36 months). The horizon enters as a smooth term within each block, giving 15 models.
  - **Predictors:**
    - conflict history (decayed time since violence, recent fatalities, neighbors, trading partners);
    - ViEWS;
    - shock × exposure × buffer channels;
    - V-Dem conditions.
  - **Estimation:**
    - Latent inputs enter with measurement error (`me()`).
    - Regularized horseshoe priors on the interactions.
    - Candidate counts are modeled as noisy measurements of final GED counts.
- **Alternative kept open:** a dynamic latent-state count model in Stan, once the channels with signal are known.

## D-021 — Forecast validation protocol
- **Date:** 2026-11-20
- **Status:** Accepted (owner, 2026-11-23), thresholds included. They are a soft gate: every backtest reports them, but the build is completed whether or not our model beats ViEWS.
- **Decision:**
  - **Backtest design:**
    - Rolling monthly origins over the period where archived ViEWS runs exist.
    - Information sets rebuilt from archived Candidate releases and ViEWS runs.
    - Each input lagged by its usual publication delay.
    - 2023–2025 held out.
  - **Scores:**
    - CRPS and log score;
    - interval score and coverage;
    - Brier score and precision-recall for 25+ deaths;
    - onsets reported separately.
  - **Proposed go/no-go:**
    - CRPS skill above zero over history plus ViEWS at horizons 1–12, for at least two of the three types;
    - 80% intervals covering 75–85% of outcomes.

## D-022 — ViEWS coverage and backtest windows
- **Date:** 2026-11-20
- **Status:** Accepted (owner, 2026-11-23), with the development window moved for the 2015 start (D-027): development origins 2016-01 to 2021-11, ViEWS-anchored 2021-12 to 2024-06, final test from 2024-07.
- **Context:** The ViEWS API archive was checked on 2026-11-20.
  - **Runs:** 63 runs (58 distinct forecast windows), fatalities001 → 002 → 003, from December 2021 to September 2026.
  - **Coverage:** forecasts exist for **state-based violence only**. The ns and os endpoints return no values.
  - **`main_dich`:** checked to be P(25+ state-based deaths). Its mean is 0.092–0.131 against an observed 0.091–0.110. fatalities003 over-predicts.
- **Decision:**
  - ViEWS's state-based forecast enters the models for all three violence types. For ns and os, "history plus ViEWS" uses that cross-type forecast.
  - **Development origins:** 2012-01 to 2021-11, comparing history-only with the full model. Outcome history comes from final GED lagged one month, because Candidate vintages start in 2021.
  - **ViEWS-anchored development:** origins 2021-12 to 2024-06.
  - **Final test:** origins from 2024-07 on are held out. This replaces "hold out 2023–2025" in D-021.

## D-023 — Comtrade refresh scope and World totals
- **Date:** 2026-11-20 (approved by the owner the same day)
- **Status:** Accepted. It extends D-003, D-014 and D-015.
- **Context:**
  - The 2026 run stopped after PER when the daily quota ran out. comtradr waits out any `Retry-After` header, so the R session slept for hours with nothing logged.
  - World totals (D-014) only started being saved after that run, so none of the existing files have them.
  - Checks on 2026-11-20:
    - All 2010–2025 files date from 3–10 March 2026.
    - THA 2018 and BIH 2019 have no rows from the second commodity chunk; both countries do in their other years. This is the old script's partial-file defect.
    - 2024 had 92 reporters (107–117 in earlier years), and 7 of them were short of 12 months.
    - 2025 had 77 reporters, 62 of them short of 12 months.
- **Decision:**
  - **No full re-download.** It would cost ~5,900 calls (~13 days); the closed years are one consistent March 2026 snapshot.
  - **World totals for existing files:** partner = World only, 20 reporters × 87 codes per call (URL ~1,006 characters; at most 41,760 rows). About 12 calls per year; only country-years with self-reported data are asked.
  - **Re-downloads:**
    - the rest of 2026 and the 2017 backfill;
    - THA 2018 and BIH 2019 (repairs);
    - all of 2025;
    - 2024 for the 81 no-data countries and the 7 short ones.
  - **One-off repairs** use `refresh_before`: re-download what was last fetched before a date, once.
  - **Quota handling:** requests are performed by our own step, built on comtradr's request-building and response-processing functions. Rate-limit waits up to 60 s are retried; a spent quota stops the run at once, logged as `interrupted`.
  - **Check first:** before the World-only downloads, a World-only call for PHL 2026 must match the World rows kept by the main download.
  - **Sequence:** `run_comtrade_update.R`, about 960 calls over 2–3 days.
- **Consequences:**
  - The World totals of closed years come from a November 2026 snapshot, while their partner data come from March 2026. For closed years the difference should be small.
  - `comtrade_request()` relies on comtradr's internal functions. A test fails if a comtradr update changes them.

## D-024 — Dashboard name: Pressure Points
- **Date:** 2026-11-21
- **Status:** Accepted for now; the owner may revisit it. Supersedes D-002. Applied 2026-11-23 to the app (text wordmark, About panel), the indicator guide and the stylesheet; committed 2026-11-23.
- **Context:**
  - The owner dropped "Undercurrent".
  - Since D-018, the headline output is a forecast of organized violence. Economic shocks, exposure and governance buffers (D-020) are its inputs, so "Country Economic Risk Monitor" no longer describes the tool.
- **Decision:** "Pressure Points — Economic Shocks and Conflict Forecasts", as a text wordmark with no logo image. Alternates considered: Tensile, Fault Lines, Stress Test, Breaking Point, Flashpoint.
- **Consequences:**
  - As under D-002, the current color palette stays, under neutral `--brand-*` token names.
  - Phase 1 uses this name in the app header and the About panel.
  - Trademarks and domains have not been checked.

## D-025 — Deflate trade values with the World Bank MUV index
- **Date:** 2026-11-23
- **Status:** Accepted (owner). Implemented 2026-11-23 in `R/deflator.R` and `R/ingest_price_series.R`, committed 2026-11-23; the full rebuild that day removed the October 2025 spike.
- **Context:**
  - Trade values were deflated with US CPI from the IMF. US consumer prices are a poor deflator for international goods trade.
  - October 2025 has no US CPI value, so 109,623 self-reported rows fell back to nominal dollars: about +3.5% against September (103.54) and November (103.33).
- **Decision:**
  - Deflate with the World Bank Manufactures Unit Value index (MUV). It is the unit value, in US dollars, of manufactures exported by 15 major economies, and the deflator the World Bank uses for constant-dollar commodity prices.
  - **Source:** sheet "Annual Indices (Real)", column "Deflator MUV Index" (2010 = 100), in `CMO-Historical-Data-Annual.xlsx`. It is annual, 1960–2025, as of the September 2026 update.
  - **Used for:** `constant_value_2024` (rebased so that 2024 = 100) and `global_value_wb_real`.
- **Consequences:**
  - The build no longer fetches IMF CPI.
  - MUV is annual, so monthly values need interpolation, and months after the latest annual value are provisional. The plan sets out how.

## D-026 — Governance from V-Dem, not WGI
- **Date:** 2026-11-23
- **Status:** Accepted (owner). V-Dem ingest and variable choice are deferred to a later phase, after the economic build is settled (owner, 2026-11-23). WGI was removed from the pipeline on 2026-11-23 (owner): `ingest_wgi.R` and `data/raw_wgi/` are archived; WGI is out of the context layer, the tier flag boost, the narrative prompts and the docs; `bronze.wb_wgi_raw` is dropped and the gold context, tier and change tables were rebuilt without it. Committed 2026-11-23.
- **Decision:**
  - The forecast's governance and political inputs come from V-Dem, not the World Bank Worldwide Governance Indicators (WGI).
  - V-Dem is ingested as country-year data, with its posterior uncertainty, and lagged by its March release date.
- **Consequences:**
  - No new code uses WGI.
  - `R/ingest_wgi.R` and the older scoring scripts that read it (`compute_context_layer.R`, `compute_narratives.R`, `ingest_worldbank.R`) stay as they are until that code is retired under its own plan. Nothing is removed without the owner's approval.

## D-027 — Project start year 2015, and an extension code set
- **Date:** 2026-11-23
- **Status:** Accepted (owner). Amended 2026-11-23: the extension codes are also downloaded for 2013-2014 (346 calls), used only as burn-in for rolling windows. Downloader support is implemented; the extension download started 2026-11-23.
- **Decision:**
  - **2015 is the start year for the whole project:** the first year of every output and of model fitting. In `config.R`, `START_YR`, `TS_START_YR` and `M_TS_START_YR` are all 2015.
  - **Burn-in:** rolling windows need two years of earlier data. Silver will read 2013–2014 for that and write nothing before 2015 (build-chain work).
  - **Extension code set:** 55 new HS codes, downloaded for 2015 on. They are `code_set = "ext"` in `data/all-commodity-codes.csv` and go to their own folders and manifest (`COMTRADE_EXT_*` in `config.R`). The original 174 codes are `core`.
  - **The 55 codes cover:**
    - energy (2710, LPG, electricity, nuclear fuel);
    - fertilizers (ammonia, sulfur, 3102, 3104, 3105, phosphoric acid);
    - food staples (millet, maize meal, pulses, cassava, potatoes, milk powder, fish, vegetable oils, oilseeds, live cattle, sheep and goats, vanilla);
    - gum arabic;
    - wood (downloaded but inactive while wood stays excluded);
    - critical minerals (lithium carbonate, rare-earth metals, high-purity silicon, minor metals, molybdenum, artificial graphite);
    - gems and gold ores, jewelry;
    - semiconductors and magnets;
    - pharmaceuticals and vaccines;
    - arms, armored vehicles, drones and explosives.
  - **New columns in the code file:** `parent_code` (for the build's remainder rule), `conflict_mineral`, `broad_heading` and `note` (honest descriptions where the old label named only part of a heading).
  - **The downloader:**
    - `run_comtrade_ext_download(years)` handles the extension codes;
    - one daily budget covers every manifest;
    - a country whose earlier download in a code set would overflow the 100k-row cap is split ahead of time.
- **Consequences:**
  - **Cost:** about 2,100 calls (2,076 country-years, more where big reporters need splitting), about five days.
  - **The build:** it must read both CSV folders (build-chain work).
  - **D-022:** its development origins (2012-01 on) no longer fit. Origins should start around 2016-01, once a year of 2015 features exists. It needs revising before approval.
  - **Burn-in for the extension codes:** they have none before 2015, so their rolling measures start in 2017 unless 2013–2014 are downloaded too (346 calls).

## D-028 — A ViEWS mirror baseline for the forecast comparison
- **Date:** 2026-11-23
- **Status:** Superseded: withdrawn by the owner on 2026-11-23. The original plan stands (D-019, D-021): ViEWS is an input and the benchmark. The owner will discuss adding our indicators to ViEWS's own models with the ViEWS team.
- **Context:**
  - The owner wants to compare our forecasts with ViEWS's while holding the model family fixed.
  - ViEWS's country-month models are open source (`views-platform/views-models`, MIT): Darts-based "stepshifter" models (random forest, LightGBM, XGBoost, hurdle) that take inputs from ViEWS's `viewser` querysets. Stepshifter documents point and binary predictions.
- **Decision:**
  - The comparison adds a ViEWS mirror:
    - their model code and configuration, run on their inputs;
    - first checked against their published forecasts for the same origins (a replication gate);
    - then the same models with our indicators added.
  - **The nested set becomes:** history only; history plus ViEWS; ViEWS mirror; ViEWS mirror plus our indicators; our Bayesian model.
  - **What the gap shows:** the gap between the two mirrors measures what our indicators add; the gap to our model measures what the model adds.
- **Consequences:**
  - **Runtime:** a pinned-commit Python sidecar writes its forecasts to DuckDB.
  - **Inputs:** `viewser` access, their queryset definitions and, ideally, archived inputs come from the ViEWS team (the owner will ask).
  - **Scoring:** CRPS needs predictive distributions. If the mirror gives point predictions only, they get an error distribution from calibration-period residuals, and the plan says so.

## D-029 — Commodity coverage and pricing in the build
- **Date:** 2026-11-23
- **Status:** Accepted (owner). Implemented 2026-11-23 in the crosswalk, `R/ingest_comtrade_build.R` and `R/ingest_price_series.R`, committed 2026-11-23. The owner confirmed the fallbacks after the rebuild: barley from FRED, sorghum and shrimp from Comtrade unit values. The USGS and Kimberley checks are still to do.
- **Decision:**
  - **Coverage:** every downloaded code is switched on except plywood (4408, 4412), which stays excluded. That brings in the 64 codes the build used to drop and the 55 extension codes, including charcoal, logs and sawnwood.
  - **No double counting:** a code whose children are also downloaded becomes a remainder (parent minus children), using `parent_code` in the code file.
  - **Broad headings** (`broad_heading` = 1: chapters 47 and 72 and chemical headings such as 2834 and 2804) count for exposure only. They are kept out of trade totals, shares and price baskets.
  - **Price tiers:**
    1. World Bank Pink Sheet;
    2. IMF primary commodity prices via FRED;
    3. Comtrade world unit values (the median across reporters, estimated quantities excluded where flagged).
    USGS annual prices and Kimberley Process values are checks, not inputs.
  - **Cobalt and lithium** are priced from Comtrade unit values.
  - **Fertilizers:** 3103 is relabeled phosphatic fertilizers (superphosphates) and priced with TSP. DAP is covered by 3105. 283523, labeled TSP, is trisodium phosphate (merged into 283529 in HS2007) and is retired: `in_data` FALSE, kept in the download list like plywood.

## D-030 — Remediation scope after the change of direction
- **Date:** 2026-11-23
- **Status:** Accepted (owner).
- **Decision:**
  - **Kept as planned:**
    - Phase 1: legacy branding removal and history rewrite, plus repo hygiene. It now also covers retiring the older scoring code that reads WGI (D-026).
    - Phase 3: the silver monthly fixes.
  - **To be re-planned as the forecast project takes shape:** Phases 2 (dashboard), 4 (latent models), 5 (downstream) and 6 (docs).
- **Context:** the headline output is now a conflict forecast (D-018). The dashboard will be redesigned later, keeping its design elements and figures.

## D-031 — Retired files go to the root archive, not the bin
- **Date:** 2026-11-23
- **Status:** Accepted (owner). Supersedes D-009. First batch moved 2026-11-23, committed 2026-11-23.
- **Decision:**
  - Files retired from the repo are moved into the gitignored root `archive/`, in a dated subfolder that keeps their repo paths (for example `archive/2026-11-23_repo_cleanup/shiny/old/ui.R`). They are not deleted.
  - The owner commits the removal from the tracked tree.
- **Consequences:**
  - A local copy survives even for files the Phase 1 history rewrite will purge from git.
  - D-001 still applies: the archive is not scrubbed of legacy branding.
