# Work log

Dated entries, newest first. Decisions live in DECISIONS.md; this file records what was done and found.

---

## 2026-11-23

### Repo cleanup: stale files archived, WGI removed, legacy branding removed (uncommitted; D-001, D-009, D-024, D-026, D-031)

**Task:** Phase 1 hygiene. Move files that nothing uses out of the tracked tree, remove WGI from the pipeline, remove the legacy branding from the working tree, and drop the scratch code from `run_pipeline.R`.

**What changed**
- **Archived to `archive/2026-11-23_repo_cleanup/`** (gitignored; repo paths kept; D-031): 20 tracked files plus `data/raw_wgi/`, 21 files in all. Git shows the 20 as deletions.

  | Files | Why |
  |---|---|
  | `R/compute_narratives.R` (old), `R/compute_narratives_New.R` | The pipeline ran `compute_narratives-4-14.R`; 748 and 44 lines differ from it |
  | `shiny/old/` (3), `shiny/old_server.R`, `old_1server.R`, `old_ui.R` | Old app copies; the app loads only `global.R`, `ui.R`, `server.R`, `www/style.css` |
  | 4 CSS files in `shiny/www/` (two legacy branding), root `style.css` | Not loaded by the app |
  | `shiny/check.csv` (5.5 MB), `shiny/files.zip`, `R/brms.pdf` | Scratch output; the brms manual |
  | `shiny/wireframe_v2.txt`, `wireframe_v5.txt`, the legacy logo | Owner's call; the logo is replaced by a wordmark |
  | `R/ingest_wgi.R`, `data/raw_wgi/` (the 2025 Excel file) | WGI removed (D-026) |

- **Renamed** `R/compute_narratives-4-14.R` to `R/compute_narratives.R`, the name the README already used; `run_pipeline.R` sources it.
- **WGI removed (D-026):**
  - `compute_context_layer.R`: the WGI block, join and summary lines are gone; blocks renumbered 1–5. `gold.context_indicators` loses `regulatory_quality`, `control_of_corruption`, `governance_score`, `governance_available` and `governance_weak`.
  - `compute_tiers.R`: the flag boost counts reserves, external debt and debt service only.
  - `compute_narratives.R`: the WGI line in the context panel and the weak-governance flag are gone.
  - The `ingest_worldbank.R` header note, the `run_pipeline.R` toggle, and the README and indicator-guide WGI passages are gone.
  - **Live database** (backup first: `data/old/georisk_before_wgi_removal_2026-11-23.duckdb`, 157 MB): reran `compute_context_layer.R`, `compute_tiers.R` and `compute_changes.R`, and dropped `bronze.wb_wgi_raw`. No WGI table or column is left.

  | Tier effect (3,623 country-years) | Count |
  |---|---|
  | Drop one tier (weak governance was their only flag) | 198 |
  | Unchanged | 2,486 |
  | Too few dimensions for a tier, before and after | 939 |
  | Drops per year, 2020–2024 (of 173 scored) | 11, 13, 15, 13, 15 |
  | Drops in 2025 | 0 |

  A scratch run beforehand gave the same counts. D1 and D2 monthly score sums are identical before and after, so `compute_changes.R` rewrote the posterior tables without changing them.
- **Legacy branding (D-001, D-024):**
  - The topbar logo is a "Pressure Points" text wordmark (`.topbar-wordmark`: Oswald 600, the existing primary color).
  - The "About" panel, the indicator guide's title and overview, and the stylesheet header say Pressure Points.
  - The legacy-named CSS color tokens are `--brand-*` (73 uses), the main-area class is `app-main`, and the utility classes are `text-brand-*` and `bg-brand-*`. Colors are unchanged.
  - Deleted two unused legacy-named helpers in `global.R` (a palette vector, an echarts grid wrapper) and the commented key line in `config.R`.
  - A search for the old name across tracked and untracked files, outside `data/`, `docs/` and the archives, finds nothing.
- **`run_pipeline.R`:** the scratch code after the narratives step (old lines 130–209) is deleted (owner). It included the "enters 2025" → "enters 2026" rewrite written back to `gold.narratives`, a reference to an undefined `check`, and CSV dumps.
- **Checks:** every edited script parses; tests 50, expectations 223, 0 failures. The app was not launched.
- **Docs:** D-031 added; D-009 superseded; D-001, D-024 and D-026 statuses updated. CLAUDE.md: the plain project name, the `--brand-*` tokens, the archive rule, `archive/` among the places to check before bringing code back, and the narratives layout line. The close-up-shop skill now only keeps the session record out of `docs/` and `archive/`.

**Why / decisions**
- **Archive, not delete (D-031, owner):** retired files go to the root `archive/` in a dated subfolder. This replaces D-009, which proposed deleting them. The owner lifted the earlier "leave `archive/` untouched" rule for this.
- **WGI removed surgically:** only the WGI parts went. The context layer, tiers and narratives otherwise work as before.
- **Held back:** `R/compute_silver_comtrade_3monrollsum.R` is unused, but it is the only implementation of the Morgan–Reinhardt 3-month price smoothing (`MR_SMOOTH_WIN = 3`). It goes after the Phase 3 smoothing decision.
- **The old database name stays** in the 49 `R/archive/` files changed by this morning's rename (owner's choice).
- **Narratives not regenerated:** that costs API calls, and only 2 of 835 mention governance.

**What failed**
- **Legacy names in the notes:** the first LOG and DECISIONS wording of the branding pass quoted the old token and helper names. Reworded; describe them as "legacy-named" instead.

**Open threads**
- **Extension download:** running. At 22:11 UTC it was at 376 of about 2,420 country-years (262 ok, 114 no data; 32 countries). Today's run stops at its 478-call budget.
  - Source `run_comtrade_update.R` once per UTC day until done, around November 28. It includes 2013–2014.
  - Its build steps then run incrementally and pick up the new extension CSVs.
- **Still to do (D-029):** the USGS and Kimberley price checks.
- **Forecast plan:**
  - The owner reviews the design doc first; no modeling before that.
  - Then the UCDP Candidate and ViEWS refresh routines and the country-month panel.
  - V-Dem is deferred (D-026).
  - Owner to ask the ViEWS team for pre-2021 forecasts and 2016–2021 backcasts.
- **Remediation:**
  - Phase 1, nearly done. Still to do:
    - **History rewrite (D-001):** needs today's work committed. Then a mirror backup, `git filter-repo` in a fresh clone (not installed yet), verification across every commit, and the force-push only on the owner's go-ahead at that moment.
    - **`.env`:** the owner renames or removes the old-branded Comtrade key name.
    - The 3-month-smoothing script, after the Phase 3 decision.
  - Phase 3 (the silver fixes) not started; it includes the 3-month smoothing decision.
  - The other phases will be re-planned (D-030).
- **Dashboard copy** (`shiny/data/georisk.duckdb`) still has the WGI table and columns and the old tiers until it is next copied from the pipeline database. The app doesn't read the WGI columns.
- **Narratives:** 2 of 835 stored narratives (ETH overview, HTI D1) mention governance in passing, from WGI-era prompts. They change at the next narratives run.
- **Dashboard redesign:** later, when the owner asks.
- **Commit:** the owner commits at the end of today. Everything since `20aa706` is uncommitted.

**Notes for picking this up**
- **`run_pipeline.R` still sources `compute_narratives.R` uncommented,** so sourcing it calls the LLM APIs. The other steps are commented toggles.
- **Git line endings:** with `* text=auto`, git warns that several LF working files will become CRLF on checkout. That's harmless; the repo stores LF.
- **Two CSS tokens are used but never defined** (`--border-light`, `--text-muted`), already so at `20aa706`.

### Build-chain rework, extension codes, full rebuild (uncommitted; D-021, D-022, D-025 to D-030)

**Task:** carry out the approved build-chain plan:
- the MUV deflator;
- the Pink Sheet fixes;
- every downloaded commodity, priced in tiers;
- safer writes.

Then start the extension-code download and rebuild everything.

**What changed**
- **Decisions recorded:**
  - D-021: thresholds are a soft gate.
  - D-022: approved, development origins 2016-01 to 2021-11.
  - D-025: MUV deflator.
  - D-026: V-Dem instead of WGI, deferred.
  - D-027: start year 2015, plus the extension codes, downloaded from 2013 as burn-in.
  - D-028: ViEWS mirror baseline, proposed and then withdrawn.
  - D-029: coverage and pricing.
  - D-030: remediation scope.
- **Code file** (`data/all-commodity-codes.csv`): 174 → 229 codes, 55 of them in the new `ext` set. New columns: `code_set`, `parent_code`, `conflict_mineral` (19 codes), `broad_heading` (14) and `note` (25 honest descriptions). The old file is kept as `_2026-11-23_backup.csv`.
- **Price crosswalk:** 227 active codes, every downloaded code except plywood: 109 Pink Sheet, 10 IMF via FRED, 83 Comtrade unit values, 25 exposure only. New columns: `price_fallback`, `parent_code`, `broad_heading`, `conflict_mineral`. 3103 is relabeled phosphatic fertilizers and priced with TSP. A backup is kept.
- **Downloader:**
  - `with_code_set()` and `run_comtrade_ext_download()` keep the extension codes in their own folders and manifest;
  - one daily budget covers every manifest;
  - `chunk_size_for()` splits a big reporter's request ahead of time, from its earlier downloads.
- **Build** (`R/ingest_comtrade_build.R`, rewritten):
  - reads both CSV folders, every column as text;
  - parent codes become remainders;
  - mirroring is decided per flow;
  - CIF/FOB ratios are recomputed on every build;
  - a drop-out guard removes partitions whose country-year no longer has data;
  - staged partition swaps;
  - a `mirror_pending.txt` marker so an interrupted build finishes its mirrors;
  - yearly batches;
  - the estimated-quantity flags are kept;
  - values stay nominal.
- **Price step** (`R/ingest_price_series.R`, rewritten), with new helpers in `R/deflator.R`, `R/pink_sheet.R` (including `download_pink_sheet()`) and `R/price_helpers.R`:
  - MUV deflation;
  - the Pink Sheet precision splice and staleness check;
  - FRED with a local cache (`data/raw_fred`) and the right unit for each series;
  - the unit-value tier;
  - fallbacks for discontinued series;
  - coffee priced by exporter;
  - per-country enrichment into a staging folder, swapped in only after the row and partition counts match.
  - `bronze.us_cpi_monthly` is dropped; `bronze.muv_monthly` and `bronze.comtrade_unit_values` are new.
- **Bronze and silver:** both read the parquet by column name and leave broad headings out. Silver reads 2013–2014 as burn-in (`BURN_IN_START_YR`).
- **Config:**
  - `START_YR` and `TS_START_YR` are 2015, `BURN_IN_START_YR` is 2013;
  - new entries `COMTRADE_EXT_*`, `PINK_SHEET_ANNUAL_XLSX`, `FRED_CACHE_DIR`.
- **Pink Sheet:** the current monthly file (through 2026M10) and the annual file are downloaded. The old monthly file is kept as `CMO-Historical-Data-Monthly_2026-03-10.xlsx`.
- **Other changes:**
  - The dashboard's copy is renamed `shiny/data/georisk.duckdb`.
  - The design doc is updated: name, data-source status, a "Data integration" section, the soft gate, D-022, V-Dem deferred.
  - American spelling is a standing rule (CLAUDE.md, memory); 8 fixes.
- **Not logged before, from 2026-11-21:** the Pressure Points name (D-024), and the close-up-shop skill adapted to this repo.
- **Tests:** 223 expectations, 0 failures (157 this morning).

**Full rebuild** (`FORCE_UPDATE = TRUE`, no API calls, 22.8 min; the build alone took 10.6)

| Check | Before (this morning's backup) | After |
|---|---|---|
| Parquet rows / partitions / codes | 19,660,961 / 2,689 / 108 | 33,698,179 / 2,939 / 226 |
| Constant / nominal, Sep–Nov 2025 | October 3.5% high (no CPI) | Flat 0.9914; provisional from July 2025 |
| MUV Dec→Jan change vs other months | n/a | 0.33% vs 0.30% average |
| 2026 rows priced (codes with a price tier) | Mar–Sep 2–3% | 99–100% every month |
| Coffee, DEU imports, June 2024 | All at the average | From BRA 5.476 (arabica), VNM 4.504 (robusta), FRA 4.990 (average) |
| World sugar 2015M01 | 0.338188708 | 0.3381887 (9,339 cells kept at full precision) |

- **Mirrors:** 3,961,155 rows in 1,248 partitions.
- **Remainders:** 1,617 negative remainders set to zero. US 2024 chapter-72 imports are $24.1bn after removing 7201–7205 ($7.8bn).
- **Price fallbacks:** barley from FRED from 2020-09; sorghum (2020-09) and shrimp (2023-11) from unit values. The owner confirmed all three after the rebuild.

**Why / decisions**
- **MUV over US CPI (D-025):** the owner found it easier to justify.
  - Monthly values come from log-linear interpolation between mid-year points, so there's no January step.
  - Months after mid-2025 hold the last level and are flagged provisional, rather than using World Bank forecasts.
- **Precision splice:** from 2026 the Pink Sheet rounds to one or two decimals (rounding noise up to about 1.6% for sugar). The archived value is used only where the new value is exactly the old one rounded. Any other difference counts as a revision.
- **Barley:** continued with FRED barley; the owner confirmed this after the rebuild. Over 2010–2020 its level ratio is 0.98 and the levels correlate at 0.97, though monthly changes correlate at only 0.47.
- **Extension codes:** a separate code set costs about 2,450 calls for 2013–2026. Adding the codes to the core set would mean re-downloading everything, about 5,900+ calls.
- **Broad headings are exposure only:** chapter 72 alone carried $431bn of 2023 imports and would swamp every share.
- **`critical_mineral` stays 0 in `bronze.cmd_meta`,** as before: mineral flows are tracked through USGS.
- **283523 retired (owner, after the rebuild):**
  - It is trisodium phosphate (HS 1992–2002), not triple superphosphate fertilizer. HS2007 merged it into 283529 (other phosphates).
  - Only HS2002 filers still report it: 7 reporters over 2010–2018 (PHL the most), at most $0.3M a year self-reported, none after 2018.
  - It had been in the fertilizer basket, priced with the Pink Sheet TSP series, a different product. Triple superphosphate is covered by 3103.
  - The crosswalk now marks it `in_data` FALSE and unpriced. It stays in the core download list, like plywood, so the download calls don't change.
- **Backup deleted** (owner): `data/backup_pre_build_2026-11-23/` (1.4 GB) is gone. The 3103 label in the code file now matches the crosswalk (it still read DAP).

**What failed**
- **curl to FRED:** from Git Bash it timed out (HTTP 000 after 60 s), while R's `httr` reached FRED instantly. Use R for FRED.
- **Shrimp from FRED:** rejected as a substitute. Level ratio 0.63, levels correlate at 0.69, monthly changes at 0.03: a different product.
- **sed:** `sed -i` in Git Bash strips CR from CRLF files, and `grep -c $'\r$'` miscounts. Use `perl -pi` or the Edit tool, and count line endings with `tr -cd '\r'`.
- **Inline R:** `Rscript -e` with pipes or quotes, and `Rscript /dev/stdin`, fail in this shell. Write a scratch script instead.
- **CIF/FOB comparison:** the March ratio cache was overwritten by the rebuild without a copy, so old and new ratios can't be compared. The new overall median is 1.139.
- **ViEWS mirror (D-028):** proposed, then withdrawn by the owner.
- **Database rename reached the gitignored archives:** `grep -rl ... R/ | xargs sed -i` also rewrote 49 files in `R/archive/` and `R/models/archive/` (72 occurrences of the old database file name, and CRLF stripped to LF). Never committed, so git can't restore them; found 2026-11-23 evening. Exclude archive folders from bulk edits.

**Open threads**
- **Extension download:** running. At 21:33 UTC it was at 298 of about 2,420 country-years (204 ok, 94 no data; 25 countries). Today's run stops at its 478-call budget.
  - Source `run_comtrade_update.R` once per UTC day until done, around November 28. It now includes 2013–2014.
  - Its build steps then run incrementally and pick up the new extension CSVs.
- **Still to do (D-029):** the USGS and Kimberley price checks.
- **Forecast plan:**
  - The owner reviews the design doc first; no modeling before that.
  - Then the UCDP Candidate and ViEWS refresh routines and the country-month panel.
  - V-Dem is deferred (D-026).
  - Owner to ask the ViEWS team for pre-2021 forecasts and 2016–2021 backcasts.
- **Remediation, not started:**
  - Phase 1: branding removal, history rewrite, retiring the WGI-using scoring code.
  - Phase 3: the silver fixes.
  - The other phases will be re-planned (D-030).
- **Dashboard redesign:** later, when the owner asks.
- **Commit:** the owner commits at the end of the day. Everything since `20aa706` is uncommitted.

**Notes for picking this up**
- **The price step must follow every build:** it computes `constant_value_2024`, which bronze and silver read.
- **After a successful run** there's no `comtrade_parquet_staging`, `comtrade_parquet_prev` or `_trash_comtrade_parquet` folder. After a failed check the staging folder is kept for inspection, and the parquet is unchanged.
- **FRED outage:** the cached copy in `data/raw_fred` is used, with a `[CHECK]` warning.
- **Months from July 2025** carry `deflator_provisional = TRUE` until the World Bank publishes MUV for 2026.
- **283523 rows** (136, 2010–2018) stay in the parquet until those partitions are rebuilt. Bronze and silver join only active crosswalk codes, so they drop them at the next `ingest_comtrade.R` run; no forced rebuild is needed.

### Comtrade update, Day 3 and the build; build-chain review; database renamed (uncommitted; D-023)

**Task:** finish the downloads, run the build (parquet → prices → bronze → silver), review the build-chain scripts, and rename the database.

**What changed**
- **Downloads finished:** the last 5 World totals for 2024 (2 calls). Nothing is left to download.
- **Backup first:** the DuckDB and the parquet were copied to `data/backup_pre_build_2026-11-23/` (1.4 GB).
- **Build:**
  - 242 changed CSVs rebuilt; mirrors recomputed for 2017–2019 and 2024–2026 (1,035,557 rows in 497 partitions).
  - Timing: build 1.2 min; prices and bronze about 2 min; silver done by 17:12 UTC.
  - The price step's row-count check held.

| Check | Before | After |
|---|---|---|
| Parquet rows / partitions | 19,660,961 / 2,689 | 21,692,476 / 2,939 |
| 2017 countries with own data / any data | 63 / 96 | 121 / 173 |
| 2025 import value from mirrors, Nov / Dec | 0.43 / 0.51 | 0.19 / 0.19 |

- **Database renamed:**
  - `data/econrisk_2015.duckdb` copied to `data/georisk.duckdb` (byte-identical) and made `DB_PATH`.
  - `run_pipeline.R` now uses `DB_PATH` instead of a hard-coded path.
  - Docs and script headers updated.
  - The old file and the dashboard's copy (`shiny/data/econrisk_2015.duckdb`) were left in place.

**Review findings** (build, price and bronze scripts; nothing fixed yet, waiting on the owner)
- **October 2025 has no US CPI.** 109,623 self-reported rows fall back to nominal values: about +3.5% against September (103.54) and November (103.33).
- **Coffee import prices are routed by the importer, not the exporter.** In 2024, 38,458 import rows ($49.1bn) get the average of the two varieties.
- **The CIF/FOB ratio cache dates from 2026-03-16.** It is only refreshed under `FORCE_UPDATE`.
- **The Pink Sheet file runs only through 2026M02.** 2026 rows are 93% priced in Jan–Feb but 2–3% in Mar–Sep.
- **Robustness:**
  - The price step and mirror step rewrite partitions in place, with no staging folder.
  - `ingest_comtrade.R` reads the parquet without `union_by_name`.
  - Mirroring coverage is decided per country-month, not per flow (306 months, 2 of them since 2024).
- **Checked and fine:**
  - readr type guessing (no values lost in 4 large files);
  - the direction of the CIF/FOB adjustment;
  - incremental pick-up of changed CSVs.

**Why / decisions**
- The owner questioned deflating everything with US CPI. Recommended instead:
  - nominal USD for shares, weights and prices;
  - the goods' own prices to turn values into volumes;
  - import bill ÷ export earnings or ÷ GDP for burden;
  - the World Bank MUV index for real USD levels, if needed.
- This is a methodology change: not yet a decision; to become D-025 once the owner chooses.

**Open threads**
- Owner approvals:
  - the build-chain fixes;
  - the deflator approach (D-025);
  - whether to rename the dashboard's database copy;
  - D-022 backtest windows.
- Owner actions:
  - download the current Pink Sheet file;
  - remove the legacy-branded key line in `.env`;
  - delete `data/econrisk_2015.duckdb` and the backup folder when satisfied.
- After the fixes: a full rebuild (`FORCE_UPDATE = TRUE`, no API calls).
- Paused: the forecast build (Candidate → final revision model, backtest harness, ViEWS baseline) and remediation Phases 1–6.
- Uncommitted: everything since `20aa706`, on `fix/comtrade-download`.

---

## 2026-11-21

### Comtrade update, Day 2 (`run_comtrade_update.R`, uncommitted; D-023)

**The run** started at 00:05 UTC on 2026-11-22 (19:05 local on 2026-11-21) and finished at 01:49 UTC. It used the full budget of 480 calls, with no errors.

| Step | Result | Calls |
|---|---|---|
| World totals 2014, retry | The 20 that failed with HTTP 500 on Day 1 (CIV to GBR) all came through. World totals for 2010–2023 and 2026 are now complete. | 2 |
| 2025 refresh | All 173 refreshed: 92 ok, 81 no_data. | 294 |
| 2024 re-checks | 88 re-checked. | 176 |
| World totals 2024 | 4 of 5 batches done (80 country-years). Batch 5 (5 reporters, UKR to ZWE) is waiting for budget. | 8 |

**2025: March download vs. now** (self-reported months)
- **Reporters:** 77 → 92.
- **With all 12 months:** 15 → 85.
- **Short:** the 7 reporters still short have 1 to 11 months.
- **Dropped out:** none of the March reporters.

**2024 re-checks**
- **Newly reported:** 5 of the 81 no-data countries now have data: BHR, GHA, KHM, OMN, YEM. 2024 is now 97 ok, 76 no_data.
- **The 7 short ones:** AGO, COL, KWT and MAR now have all 12 months. CPV still has 4, and KAZ and MWI still have 2.

**Left:** World totals for the 5 reporters in 2024 batch 5 (2 calls), then the build (Steps 8–11). Nothing ran on 2026-11-22 UTC.

### Comtrade update, Day 1 (`run_comtrade_update.R`, uncommitted; D-023)

**The run** used the full budget of 480 calls, ended at 06:00 UTC with exit code 0, and made no 414s or quota stops.

| Step | Result | Calls |
|---|---|---|
| 1. Rest of 2026 | 46 done: 18 with data, 28 with none. 2026 is now 78 ok, 95 no_data. Latest month among the 18: 3 to 9; none yet has 12. | 92 |
| 2. 2017 backfill | 77 done: 58 with data (54 with all 12 months), 19 with none (e.g. IRN, IRQ). 2017 is now 121 ok, 52 no_data: all 173 countries are settled. | 154 |
| 3. Repairs | THA 2018: 33,534 rows, 12 months. BIH 2019: 11,056 rows, 12 months. Both chunks are present now. | 4 |
| PHL World check | Match: 1,557 World rows from the main download and 1,557 from World-only, 0 unmatched, largest value difference 0. | 2 |
| 4. World totals 2010–2023, 2026 | 1,582 of 1,602 written to `data/comtrade_world/`. One batch failed with HTTP 500 (2014, 20 reporters CIV to GBR, logged as `error`). | 175 |
| 5. 2025 refresh | 26 of 173 done (AFG to BWA): 14 with data, 12 with none. CAF was cut off at the budget and logged as `interrupted`. | 53 |
| 6. 2024 | Not started: the budget was spent. | 0 |

**What the 2025 refresh changed.** Self-reported months in the March parquet compared with the new files:
- Of the 14 with data, 13 now have all 12 months. In March only 4 of them did (AUS, BIH, BRA, BTN).
- Examples: ARG 9 → 12, AZE 9 → 12, BGR 9 → 12, BEL 11 → 12.
- AGO and BDI are new reporters: no 2025 data in March, now 12 and 11 months.
- Row counts are not comparable between the CSV and the parquet, because the build collapses rows. AUS 2023, which is unchanged, has 23,220 CSV rows and 15,454 parquet rows.

**Left:** 347 country-years, about 482 calls (294 + 176 + 10 + 2). That is just over one day's budget of 480, so the last World totals batch for 2024 runs on Day 3.
- the rest of 2025: 146 to refresh plus CAF;
- the 2024 re-checks: 88;
- World totals: 92 for 2024, and the 20 failed in 2014.

The build waits until nothing is left. **Next:** source the script again after 00:00 UTC.

---

## 2026-11-20

### Comtrade: why the 2026 run stalled, refresh plan, World totals (uncommitted; D-023)

**The stall**
- The owner's run logged PER at 21:31 UTC, then nothing. The daily quota was spent (the owner had made an earlier run that day).
- comtradr retries any 403/429 that carries `Retry-After` and sleeps for that many seconds, which can be hours. The R session (VS Code terminal) was still alive and asleep at 00:07 UTC, with no open connection.
- No World files exist: that session had loaded the script before Step 1b.

**Checks on the existing files** (parquet, self-reported rows)
- **Download dates:** all 2010–2025 files date from 3–10 March 2026.
- **Missing chunks:** THA 2018 and BIH 2019 have no rows from commodity chunk 2; both countries do in their other years. LSO 2010/2017, SDN 2010 and TGO 2010 are sparse but have both chunks.
- **Reporters and missing months:**

  | Year | Reporters | Short of 12 months |
  |---|---|---|
  | 2020 | 117 | 4 |
  | 2021 | 112 | 0 |
  | 2022 | 114 | 4 |
  | 2023 | 107 | 0 |
  | 2024 | 92 | 7 (AGO, COL, CPV, KAZ, KWT, MAR, MWI) |
  | 2025 | 77 | 62 |

- **World-only URLs** (built offline):

  | Request | URL length (characters) |
  |---|---|
  | Today's request: 1 reporter, 87 codes, all partners | 895 |
  | World only: 20 reporters, 87 codes | 1,006 |
  | World only: 20 reporters, 174 codes | 1,651 (too close to the limit) |

- **Historical codes:** comtradr adds them to some reporters, e.g. DEU = 280 and 276, USA = 842 and 841.
- **2026 files:** `reporter_iso` matches the file name in all 60.

**Changes**
- `R/ingest_comtrade_download.R`:
  - `comtrade_request()` builds and processes requests with comtradr's own functions, but performs them itself (`perform_comtrade`). Waits up to 60 s are retried; a spent quota stops the run at once and keeps the gateway's "replenished in" message.
  - It now accepts several reporters and `partner = "World"`.
  - New `run_comtrade_world_download()`, `plan_world_downloads()`, `download_world_batch()`, and `check_world_totals()` (a one-off check, 2 calls).
  - New `refresh_before` argument for one-off repairs.
  - The daily budget now counts both manifests.
  - The run block moved to the new step script.
- `config.R`: added `COMTRADE_WORLD_MANIFEST`.
- New `run_comtrade_update.R`, the step-by-step update:
  1. the rest of 2026
  2. the 2017 backfill
  3. THA and BIH repairs
  4. the PHL check, then World totals for 2010–2023 and 2026
  5. the 2025 refresh
  6. the 2024 re-checks, then World totals for 2024
  7. a list of what's left
  8. Steps 8–11, the build (parquet, then prices, then bronze, then silver), run only once nothing is left.
- **Tests:** 157 expectations, all passing (110 before). New tests cover the URL check, the retry rule, a quota stop through the downloader, World batches, the check, `refresh_before`, and the shared budget.

**Preview** (`PREVIEW_ONLY = TRUE`, with a dummy key; nothing called, nothing written)

| Step | Country-years | Calls |
|---|---|---|
| 2026 rest | 46 | ~92 |
| 2017 backfill | 77 | ~154 |
| THA 2018, BIH 2019 | 2 | 4 |
| World totals 2010–2023, 2026 | 1,604, in 89 batches | ~178 |
| 2025 refresh | 173 | ~346 |
| 2024 re-checks | 88 | ~176 |
| World totals 2024 | 92 | ~10 |

That's about 960 calls. Day 1 is about 428, and 2025 starts with what's left of that day.

**Next:** the owner stops the sleeping session; then Day 1 runs through `run_comtrade_update.R`.

### Forecast build started: outcome data in, download hardening (uncommitted)

**Direction**
- **Decisions:** outcome = UCDP organized violence, all three types; full fatality distributions; horizons 1–36 months; country-month (D-018 to D-021). ACLED is out (license).
- **Design doc** updated: https://claude.ai/artifact/NENMJC2hgoaRMhU16RCQP1

**Comtrade download, first production run (by the owner)**
- 127 country-years for 2026 (60 with data, 67 no data), 254 calls, all HTTP 200.
- No 414s, no splits. The partner filter was verified: no World, self or aggregate-area rows.

**Download script follow-ups (D-014)**
- World totals now go to `data/comtrade_world/`.
- The call budget is per UTC day and shared across runs, counted from the manifest.
- A country-year cut off mid-way is logged as `interrupted`, with its calls, and resumed on the next run.

**Test fix**
- The owner's run block now downloads on `source()`, so tests load script definitions only.
- That fixed a hang: tests had been sending real API calls with a fake key and hitting rate-limit waits.

**UCDP ingest: new `R/ingest_ucdp.R`**
- **Access:** public file downloads. The UCDP API now requires a token.
- **GED 26.1:** 417,968 events, 1989–2025.
- **Candidate:** 69 monthly releases (2021-01 to 2026-09), 124,277 events, plus the revised 2026 year-to-date file (16,633 events).
- **Release structure:** each monthly release holds that month's events (~98%) plus late additions. The year-to-date file adds 1,936 events and drops 807 compared with the monthly releases combined.
- **Output:** `silver.ucdp_country_month`, 236,466 rows (174 countries × 453 months × 3 types).
- **Sanity checks:**
  - 2022 state-based deaths: ETH 163k, UKR 99k.
  - No jump at the switch from GED to Candidate (Dec 2025 → Jan 2026).
  - Any state-based violence occurs in 17.4% of country-months; 25+ deaths in 9.1%.

**ViEWS ingest: new `R/ingest_views.R`**
- **Archive:** 63 runs from December 2021 to September 2026; 58 preferred after removing re-runs. Output: `bronze.views_cm_sb`, 433,188 rows.
- **Coverage:** forecasts for state-based violence only (D-022).
- **`main_dich` = P(25+ deaths).**
  - Its average (0.092–0.105) matches the observed share of 25+ months (0.091–0.104).
  - Scored as P(25+), its Brier score is 0.018–0.034; scored as P(any violence), it is about 0.06–0.09.
  - fatalities003 over-predicts (0.131 against 0.110).
- **Ranking:** Spearman rank correlation of `pred_mean` with realized deaths is 0.63–0.66.

**Tests:** 110 expectations, all passing, in about 9 seconds.

**Next**
1. Candidate → final revision model.
2. Backtest harness with the history-only baseline.
3. ViEWS-anchored baseline, then Gate 2.
4. Shock and latent features.

### Comtrade reporting limitations (read-only checks of `data/comtrade_csv` and the parquet)

These checks led to decisions D-012 – D-017, added to the plan as Step 1b and new items in Phases 2–4.

**How mirroring is decided**
- The empty "no data" CSVs never drove mirroring. The build mirrors any country-month that has no self-reported rows, and skips the empty files.
- Cross-check for 2010–2025:

  | CSV folder | Parquet | Country-years |
  |---|---|---|
  | "no data" file | mirrored only | 976 |
  | "no data" file | nothing at all | 2 |
  | data file | self-reported only | 1,570 |
  | data file | self-reported plus mirrored months | 143 (62 in 2025) |
  | never downloaded | nothing at all | 77 (all 2017) |

- So the 2017 gap is entirely empty, which is why silver shows zeros for 2017.

**Reporting lags**
- At the 2026-03-03 download, the last month available in 2025 was December for only 16 of 173 reporters; 96 had no 2025 data at all.
- China has no self-reported 2025 data.

**Non-reporters**
- No monthly data at all for 2019–2025: IRN, VEN, SYR, LBY, SDN, AFG, ETH, BGD, IRQ.
- Reporting stopped: RUS from 2022, YEM from 2020, KEN from 2024.

**Valuation (2024 imports)**
- CAN and ZAF report FOB only; DEU and IND CIF only; USA, AUS and BRA report both.

**Quantities (2024)**
- Share of quantities that are Comtrade estimates: EGY 82%, USA 26%.
- Share of quantities missing or zero: NGA 22%.

**HS editions**
- HS2017 was adopted mostly in 2017–18 (H5: 50 reporters in 2017, 105 in 2018).
- HS2022 was adopted mostly in 2022 (H6: 95 reporters).

**China January/February**
- Comtrade has separate months in 2022–2024, so there is no merge artifact.

### Comtrade download fix (plan Step 1)

**Status:** done and tested; nothing committed. Changes sit on local branch `fix/comtrade-download`.

**Cause of the HTTP 414 error**
- Each request listed all ~172 partner countries. The `partnerCode` parameter alone was 1,102 characters, making a URL of about 1,990–2,000 characters.
- That URL is the same for 2025 and 2026, and the same under R 4.4.3 (comtradr 1.0.3.9000) and R 4.6.0 (comtradr 1.0.5).
- It worked for the 2025 download in March 2026, so Comtrade has since lowered its limit.
- Probes with an invalid key returned 401 at every length, so the limit is enforced only after authentication.
- With `partner = "everything"` the URL is 887 characters (1,532 for all 174 codes in one call).

**Other defects found in the downloader**
- **Resume rule:** "under 8 KB" was treated as unfinished. But 978 of 2,694 files are Comtrade's valid "no data" answer (`count / 0 / 0`), and every rerun requested them again. This likely drained the quota and left 2017 incomplete: 96 of 173 countries, only 63 of them with data.
- **Broken files:** a failed first chunk left a header-less file; a failed second chunk left a partial file that was never retried.
- **No refresh:** the 2024–25 files date from 2026-03-03. Ukraine 2025 stops at September; India and Egypt stop at October.
- **Unused row cap:** the cap constant was never checked. The largest chunk so far is 67,705 rows (Netherlands 2023).

**Changes made**
- `R/ingest_comtrade_download.R`, rewritten per D-003: `run_comtrade_download()`, manifest, adaptive splitting, all-or-nothing file writes, refresh policy, call budget. The owner's 2026 setting is kept as the documented first run.
- `R/ingest_comtrade_build.R`, per D-004:
  - Files are selected by header rather than size.
  - A CSV newer than its partition is reprocessed.
  - Mirrors are rebuilt from self-reported rows for each touched year.
  - Emptied partitions are removed.
  - `union_by_name` is used when reading the parquet.
  - A `FORCE_UPDATE` default is added.
- `R/compute_silver_comtrade.R` reads `COMTRADE_PARQUET` from `config.R` (D-005). Both copies were checked to be identical: 28 columns, 19,660,961 rows.
- `config.R` gains `COMTRADE_MANIFEST`. The README step 0a instructions are updated.
- New offline tests in `tests/testthat/`, using a stubbed API and a synthetic parquet. All 63 expectations pass.

**Dry run against the real CSV folder** (no calls, nothing written)
- The manifest bootstrap finds 1,713 `ok` and 978 `no_data` files.
- To download: 2017 → 77 country-years (not attempted); 2025 → 173 (refresh); 2026 → 173 (not attempted).
- 2024 with `retry_no_data = TRUE` → 81.
- The three empty 2026 marker files from the failed attempt had already been removed.

**Next — the owner runs these (needs the API key):**
1. `run_comtrade_download(2026, countries = "AFG")`: smoke test, 2 calls.
2. All of 2026 (~346 calls).
3. 2017 backfill (~154 calls).
4. 2025 refresh (~346 calls, next day).
5. `ingest_comtrade_build.R` → `ingest_price_series.R` → `ingest_comtrade.R` → `compute_silver_comtrade.R`.

### Repo review (read-only)

Evidence comes from queries against a scratch copy of `shiny/data/econrisk_2015.duckdb`.

**January steps in the monthly "Price Volatility & Anomaly Signals" chart**
- The chart plots model fitted values (`gold.d2_monthly_indicator_ts.fit_mean`, in standardized units under a "CV" label), not data.
- The D2 monthly model is `y ~ 1 + (1|country) + (1|year) + (1|month)`, so its fits move only through global year and month effects.
- Silver data are smooth across January: Ukraine's food basket volatility goes 0.0476 → 0.0479 (Dec-23 → Jan-24) and 0.0771 → 0.0783 (Dec-24 → Jan-25).
- Over the same period the model fit steps 0.262 → 0.062 → −0.033.

**All five Bayesian models (D1, D2a, D3a, D2m, D3m)**
- **No latent variable:** theta is the equal-weight mean of each indicator's fitted values.
- **Purely additive:** fitted values are country + global time; the variance left after removing those effects is about 1e-17.
- **Frozen rankings:** the Spearman rank correlation between consecutive periods is 1.00000.
- **Artifact change signals:** "substantial" one-month changes fire for 84% of country-months in January and 0% in other months.
- **Wrong intervals:** the composite interval is the average of indicator quantiles, and columns named q05/q95 hold the 2.5%/97.5% quantiles.
- **Data handling:** carried-forward values are fitted as observed data; WEO projections (2026–2030) are fitted as data; the score is rescaled on every run.
- **Example:** Ukraine's D1 score is lower in 2022 (0.821) than in 2020 (0.930).

**Silver monthly layer**
- Unreported months are zero-filled. 2017 is missing for 78 of 173 countries, producing −100% drawdowns and inflated volatility through 2019.
- Supplier HHI and import unit prices are annual values repeated across 12 months, so they step every January.
- The basket price z-score adds raw prices in different units.
- `ROWS` windows are used on sparse rows.
- Expanding baselines cause z-scores to drift upward over time.
- Morgan–Reinhardt basket volatility: weights aren't renormalized, flat price series create jumps, and the documented smoothing isn't implemented.
- The energy basket has no HS 2710 (refined products).
- **End-of-series mirror bias:** the average import mirror share rises from 0.56 to 0.91 across 2025. Fully mirrored months average z = −1.22 / −1.59 in Nov/Dec, vs +0.83 / +0.45 when self-reported. Ukraine's Nov/Dec-2025 "shortfall" is mostly this artifact.

**Downstream**
- D4 inherits the January steps, and its intervals come from averaged quantiles.
- `compute_changes.R` lags by row, not by date.
- The Ukraine D2 narrative says "substantial increase from 0.273". 0.273 is the income-group mean; the data show a 0.254 *decrease* from 0.603. The prompt passed only "substantial", with no direction or value.
- Score cards use D2 annual while narratives quote D2 monthly.

**Hygiene**
- `run_pipeline.R` contains ad-hoc code and is not runnable end to end.
- There are three narrative scripts.
- There are stale Shiny/CSS copies.
- No tests existed.

**Plan approved:** Step 1 (download fix), then Phases 0–6:
- **0** — scaffolding
- **1** — legacy branding removal and hygiene
- **2** — charts show data
- **3** — silver fixes
- **4** — latent models v2
- **5** — downstream
- **6** — docs
