# Pressure Points – Economic Risk Indicator Reference Guide

## Table of Contents

- [Overview](#overview)
- [Raw Data Layer](#raw-data-layer)
  - [UN Comtrade – Bilateral Trade Parquet](#un-comtrade-bilateral-trade-parquet)
  - [Commodity Price Data](#commodity-price-data)
  - [IMF World Economic Outlook](#imf-world-economic-outlook)
  - [World Bank WDI](#world-bank-wdi)
  - [WTO Tariff Data](#wto-tariff-data)
  - [USGS Mineral Commodity Summaries](#usgs-mineral-commodity-summaries)
  - [GDP and Population Supplements](#gdp-and-population-supplements)
  - [Bronze Table Inventory](#bronze-table-inventory)
- [Macro-Fiscal Stability](#macro-fiscal-stability)
- [Strategic Import Reliance](#strategic-import-reliance)
  - [Annual Model](#annual-model)
  - [Monthly Level Model](#monthly-level-model)
- [Export Fragility](#export-fragility)
  - [Annual Model](#annual-model-1)
  - [Monthly Level Model](#monthly-level-model-1)
- [Network Contagion Risk](#network-contagion-risk)
- [Context Indicators](#context-indicators)
- [Risk Tier Classification](#risk-tier-classification)

---

## Overview

This reference guide covers the data inputs, derived indicators, and model outputs for an economic risk data pipeline, which feeds the Pressure Points interactive dashboard. This project originally started as a qualification requirement for my Ph.D. at Emory. Working with one of my advisers, that paper looked at the relationship between the volatility of a country's export revenue (through trade data) and the likelihood of civil conflict. A version of that paper is in the `econ-risk-pipeline` root folder. This project builds from that prior work. 

The pipeline scores 175 countries across four risk dimensions: macro-fiscal stability, strategic import reliance, export fragility, and network contagion risk. It combines them into tiered country risk profiles with Bayesian credible intervals. These profiles feed directly into the Pressure Points Shiny dashboard, the primary analytical interface for end users. The dashboard supports country-level deep dives, cross-country comparison, time series tracking, and narrative summaries generated from the scored outputs. Scores are updated on a quarterly publication cadence using full MCMC sampling.

**How to use this guide.** The Raw Data Layer section documents every bronze-layer source table: what was pulled, at what grain, and with what schema. The risk dimension sections that follow document how those inputs are transformed into model-ready indicators and ultimately into scored outputs. Each indicator entry covers its source table, construction logic, and how its orientation relates to the final risk score.

All risk scores are on [0, 1] where higher values indicate greater risk. Credible intervals are 95% posterior intervals unless noted otherwise.

---

## Raw Data Layer

This section documents the raw data source ingested by the pipeline. All raw data lands in the `bronze` schema of `georisk.duckdb` before any transformation. The bronze layer is the authoritative, unmodified record of what was pulled from each source and when.

The universal country key across all tables is `iso3c` (ISO 3166-1 alpha-3). All ingest scripts filter to the 175 countries defined in `bronze.country_frame`.

---

### UN Comtrade – Bilateral Trade Parquet

**Script:** `ingest_comtrade_download.R` (step 0a), `ingest_comtrade_build.R` (step 0b), `ingest_price_series.R` (step 0c, price enrichment), `ingest_comtrade.R` (bronze aggregation)

**Location:** `data/comtrade_parquet/` – hive-partitioned by `iso3c={ISO3}/yr={YEAR}/`

**Coverage:** 173 countries, 174 HS codes, 90 commodities, 2010-present, monthly level

The Comtrade parquet is the most data-rich input in the pipeline. It contains monthly bilateral import and export flows for a curated set of HS codes spanning food, energy, fertilizer, and critical minerals. All monetary values are expressed in constant 2024 USD (deflated by US CPI from the IMF IFS API). Step 0b applies a CIF/FOB adjustment to harmonize reported values across reporter-partner pairs and fills gaps via mirroring – when a reporter's self-reported flow is missing, the corresponding partner's reported flow is substituted and flagged.

Step 0c enriches the parquet in-place with commodity price data from the World Bank Pink Sheet and IMF PCPS (via FRED), adding price, quantity, and global-value columns used in volatility and price-shock calculations.

**Parquet columns**

| Column | Type | Description |
|--------|------|-------------|
| `yr` | integer | Reference year |
| `mon` | integer | Reference month (1-12) |
| `iso3c` | character | Reporter ISO3 (hive partition key) |
| `country` | character | Reporter name |
| `flow_desc` | character | "Import" or "Export" |
| `partner_iso3c` | character | Partner country ISO3 |
| `partner_country` | character | Partner country name |
| `cmd_code` | character | HS commodity code |
| `cmd_desc_comtrade` | character | Comtrade commodity description |
| `commodity_name` | character | Standardized name from crosswalk |
| `category` | character | Commodity group (food, energy, fertilizer, critical_mineral, pink_sheet) |
| `subcategory` | character | Commodity subgroup (e.g., cereal, petroleum) |
| `qty_unit_code` | integer | Comtrade quantity unit code (see `comtrade_qty_codes.csv`) |
| `qty_comtrade` | numeric | Quantity in Comtrade-reported units |
| `net_wgt_comtrade` | numeric | Net weight in kg (reporter-stated) |
| `cifvalue` | numeric | CIF value, original USD |
| `fobvalue` | numeric | FOB value, original USD |
| `primary_value` | numeric | Best available value (CIF for imports, FOB for exports), original USD |
| `constant_value_2024` | numeric | `primary_value` deflated to constant 2024 USD via US CPI (see deflation methodology note below) |
| `mirrored` | logical | TRUE if flow estimated from partner-reported data |
| `price_wb` | numeric | World Bank Pink Sheet or IMF PCPS commodity price (USD per unit) |
| `price_unit` | character | Price unit (e.g., "\$/mt", "\$/bbl") |
| `commodity_name_price` | character | Price series name as labeled in source |
| `qty_source` | character | Source of quantity used for price join ("comtrade" or "wb") |
| `qty_wb` | numeric | Quantity derived from global trade value / price |
| `global_value_wb` | numeric | Global trade value at Pink Sheet price, current USD |
| `global_value_wb_real` | numeric | `global_value_wb` deflated to constant 2024 USD |

**Commodity classification**

The crosswalk file `comtrade_pinksheet_crosswalk.csv` maps each HS `cmd_code` to a commodity name, category, subcategory, and price series. The `in_data` flag marks which codes are active; wood products are excluded due to HS code instability. Coffee receives variety-level price routing (arabica vs. robusta) based on the exporting country's production profile.

**Bronze aggregation tables**

`ingest_comtrade.R` reads the enriched parquet and writes the following bronze summary tables:

| Table | Description |
|-------|-------------|
| `bronze.comtrade_food_net_trade` | Annual net trade position by food subcategory (cereals, meat, oils, etc.) |
| `bronze.comtrade_cereal_hhi` | Cereal import source HHI – concentration of staple grain sourcing by partner |
| `bronze.comtrade_food_price_volatility` | Intra-year CV of monthly food import unit prices |
| `bronze.comtrade_export_hhi` | Export commodity HHI restricted to Pink Sheet commodities |
| `bronze.comtrade_top5_commodities` | Top 5 export and import commodities by value, long format with rank |
| `bronze.comtrade_commodity_exposure` | Critical mineral, energy, and food shares of total trade by flow direction |
| `bronze.comtrade_mirror_share` | Monthly share of trade value estimated via mirroring; surfaced as a dashboard data-quality indicator |

**Deflation methodology note**

Trade values are currently deflated using the US CPI (2024 average = 100), sourced from the IMF IFS API. While standard and widely used convention that places all flows in a common numeration for cross-country comparison, its limitation is that it conflates real volume changes with movements in the USD itself rather than tracking changes in the international price level of traded goods.

The more theoretically appropriate deflator for international trade values is the World Bank Manufactured Unit Value (MUV) index, which tracks export prices of manufactured goods from major economies and is the standard deflator used in World Bank commodity market analysis -- including the Pink Sheet data already ingested by this pipeline. This slipped my mind. 

Switching to MUV is relatively low-friction, given the existing infrastructure, but at this point I need a minimum viable product so, this is a candidate for a future methodology update, particularly if the platform publishes results for an external audience where deflator choice is likely to draw scrutiny.

**Coverage notes**

Monthly Comtrade coverage is sparser before 2015. The pipeline ingests from 2010 (controlled by `START_YR` in `config.R`) to allow rolling-window burn-in, but monthly silver series start at 2015 (`M_TS_START_YR`). Flows below the `CMD_MIN_USD` threshold (currently $100K annual equivalent) are excluded from rolling calculations to suppress noise from infrequent or trivial trade.

---

### Commodity Price Data

**Scripts:** `ingest_price_series.R`

Commodity prices are ingested from two sources and unified into a single lookup table used throughout the pipeline.

#### World Bank Pink Sheet

**Source:** `CMO-Historical-Data-Monthly.xlsx` – manually downloaded from [worldbank.org/en/research/commodity-markets](https://www.worldbank.org/en/research/commodity-markets) and placed at `data/raw_pink_sheet_price/`. Updated monthly, typically the first week of the following month.

**Bronze table:** `bronze.pink_sheet_prices`

| Column | Description |
|--------|-------------|
| `yr` | Year |
| `mon` | Month (1-12) |
| `series_name` | Pink Sheet column header (e.g., "Crude oil, average", "Wheat, US HRW") |
| `price_wb` | Nominal commodity price |
| `unit` | Price unit from the Excel header (e.g., "\$/bbl", "\$/mt") |

Coverage spans all commodity series present in the active crosswalk: energy (crude oil, natural gas, coal), food (wheat, rice, maize, soybeans, sugar, palm oil, beef, poultry, fish, coffee arabica/robusta, tea, cocoa, bananas, oranges), fertilizers (DAP, urea, potash), and metals/minerals linked to Pink Sheet series. Only series that appear in the crosswalk with `in_data == TRUE` are retained in the bronze table.

#### IMF Primary Commodity Price System (PCPS)

**Source:** Redistributed via FRED (Federal Reserve Bank of St. Louis). The IMF SDMX endpoint is unreliable for bulk access; FRED provides the same benchmark prices as plain CSV with no authentication required.

**Bronze table:** `bronze.imf_pcps_prices`

| Column | Description |
|--------|-------------|
| `yr` | Year |
| `mon` | Month (1-12) |
| `pcps_code` | IMF PCPS commodity code (e.g., "PPORK") |
| `price_usd` | Price in USD (converted from cents where applicable) |

Currently active PCPS series: `PPORK` (swine, 51-52% lean hogs). Additional IMF-sourced series can be added to the `FRED_PCPS_MAP` in `ingest_price_series.R` as needed.

#### US CPI Deflator

**Source:** IMF IFS API (SDMX 3.0), USA CPI all-items monthly index.

**Bronze table:** `bronze.us_cpi_monthly`

| Column | Description |
|--------|-------------|
| `yr` | Year |
| `mon` | Month (1-12) |
| `cpi_idx_2024` | US CPI index, 2024 annual average = 100 |

Used to deflate both Pink Sheet prices and Comtrade trade values to constant 2024 USD.

#### Unified Price Lookup

**Bronze table:** `bronze.price_lookup`

One row per `cmd_code` × `yr` × `mon`. Average-series commodities (where `average_price == 1` in the crosswalk) are averaged across their component Pink Sheet series. This table is the join target when attaching prices to any Comtrade-derived indicator.

| Column | Description |
|--------|-------------|
| `cmd_code` | HS commodity code |
| `yr` | Year |
| `mon` | Month |
| `price_wb` | Price in USD per unit (averaged across components where applicable) |
| `price_unit` | Price unit |

---

### IMF World Economic Outlook

**Script:** `ingest_imf.R`

**Source:** IMF WEO DataMapper API (`imf.org/external/datamapper/api/v1`)

**Bronze table:** `bronze.imf_weo_annual`

Annual series with five-year forward projections. These indicators are not available from the World Bank WDI at WEO's coverage depth or forward-projection horizon.

| Indicator code | Label | Notes |
|----------------|-------|-------|
| `BCA_NGDPD` | `current_account_pct_gdp` | Current account balance as % of GDP |
| `GGXWDG_NGDP` | `govt_gross_debt_pct_gdp` | General government gross debt as % of GDP |
| `GGXCNL_NGDP` | `fiscal_balance_pct_gdp` | Fiscal balance (net lending/borrowing) as % of GDP |
| `PCPIPCH` | `inflation_cpi_pct` | CPI inflation, annual % change |
| `NGDP_RPCH` | `gdp_growth_pct` | Real GDP growth, annual % change |

**Table schema:** long format, one row per `iso3c` × `year` × `indicator_code`. Columns: `iso3c`, `imf_country_code`, `indicator_code`, `indicator_label`, `year`, `value`, `pull_ts`.

**Coverage:** 175-country spine; includes both observed and forecasts (through the WEO projection horizon). Aggregates and territories outside the spine are dropped at ingest.

**Update frequency:** April and October (biannual WEO release).

**Note on IFS monthly:** The IMF IFS monthly CPI SDMX endpoint currently returns an empty dataset for all tested key combinations. `bronze.imf_ifs_monthly` is written as an empty placeholder so downstream scripts do not error. Monthly CPI candidates for a future phase include IMF IFS via a paid API key or FRED.

---

### World Bank WDI

**Script:** `ingest_worldbank.R`

**Source:** World Bank WDI API via the `wbstats` R package

**Bronze tables:** `bronze.wb_wdi_raw`, `bronze.wb_countries_raw`

`bronze.wb_wdi_raw` is in long format: one row per `country_code` × `period` × `indicator_id`. Columns: `country_code` (iso3c), `country_name`, `indicator_id`, `indicator_label`, `period`, `value`, `pull_ts`.

The full set of ingested indicators is organized into six functional groups:

**Group 1: Macro-financial stability**

| WDI code | Label | Notes |
|----------|-------|-------|
| `FI.RES.XGLD.CD` | `reserves_excl_gold_usd` | International reserves excluding gold |
| `FI.RES.TOTL.CD` | `total_reserves_usd` | Total reserves including gold |
| `FI.RES.TOTL.MO` | `reserves_months_imports` | Months of import cover – precomputed by WB |
| `BM.GSR.GNFS.CD` | `imports_goods_services_usd` | Total imports of goods and services |
| `DT.DOD.DSTC.CD` | `ext_debt_shortterm_usd` | Short-term external debt outstanding |
| `DT.TDS.DECT.CD` | `debt_service_total_usd` | Total debt service (principal + interest) |

**Group 2: Trade and capital flows**

| WDI code | Label | Notes |
|----------|-------|-------|
| `BX.GSR.GNFS.CD` | `exports_goods_services_usd` | Total exports of goods and services |
| `BX.KLT.DINV.CD.WD` | `fdi_inflows_usd` | FDI net inflows |
| `BM.KLT.DINV.CD.WD` | `fdi_outflows_usd` | FDI net outflows |
| `IS.SHP.GCNW.XQ` | `lsci` | Liner Shipping Connectivity Index |

**Group 3: Tariff time series**

| WDI code | Label | Notes |
|----------|-------|-------|
| `TM.TAX.MRCH.WM.AR.ZS` | `tariff_mfn_applied_wtd_avg` | MFN applied tariff, trade-weighted average |
| `TM.TAX.MRCH.BC.ZS` | `tariff_bound_simple_avg` | Bound tariff, simple average |

Note: WDI tariff indicators were discontinued in the July 2025 WDI update. Current tariff sourcing relies on `ingest_wto.R`. The WDI series remain in the bronze table for historical trend analysis where coverage exists.

**Group 4: Growth and real economy**

| WDI code | Label | Notes |
|----------|-------|-------|
| `NY.GDP.MKTP.CD` | `gdp_usd` | Nominal GDP, current USD |
| `NY.GDP.MKTP.KD.ZG` | `gdp_growth_pct` | GDP growth rate |
| `NY.GDP.PCAP.KD` | `gdp_per_capita_const_usd` | GDP per capita, constant USD |
| `NE.GDI.TOTL.ZS` | `capital_formation_pct_gdp` | Gross capital formation as % of GDP |

**Group 5: External, monetary, and fiscal**

| WDI code | Label | Notes |
|----------|-------|-------|
| `BN.CAB.XOKA.GD.ZS` | `current_account_pct_gdp` | Current account balance % GDP (historical; WEO adds forecasts) |
| `NE.TRD.GNFS.ZS` | `trade_pct_gdp` | Trade openness |
| `FP.CPI.TOTL.ZG` | `inflation_cpi_pct` | CPI inflation, annual % |
| `FS.AST.PRVT.GD.ZS` | `domestic_credit_private_pct_gdp` | Domestic credit to private sector % GDP |
| `NE.CON.GOVT.ZS` | `gov_consumption_pct_gdp` | Government consumption % GDP |

**Group 6: Exchange rate**

| WDI code | Label | Notes |
|----------|-------|-------|
| `PA.NUS.FCRF` | `fx_rate_lcu_per_usd` | Official exchange rate, local currency per USD |
| `PX.REX.REER` | `reer_index` | Real effective exchange rate index |

**Group 7: Portfolio flows**

| WDI code | Label | Notes |
|----------|-------|-------|
| `BN.KLT.PTXL.CD` | `net_portfolio_investment_usd` | Net portfolio investment (equity + debt, BoP basis) |

**`bronze.wb_countries_raw`** contains World Bank country metadata: `iso3c`, `iso2c`, `country`, `capital_city`, `longitude`, `latitude`, `region`, `income_level`, `lending_type`. Used as a join reference for WTO country code reconciliation in the silver layer.

**Update frequency:** Annual, typically July.

---

### WTO Tariff Data

**Script:** `ingest_wto.R`

**Source:** WTO Tariff Profiles – stable "daily_update" ZIP endpoint, always current annual edition. No API key required.

Three bronze tables are produced, each supporting a different analytical view:

**`bronze.wto_tariff_summary`**

Cross-sectional snapshot: 153 countries × 22 metrics × 3 product scopes (all goods, agricultural, non-agricultural). One row per country × scope. Columns include bound and applied tariff averages, peak tariff rates, duty-free import shares, and binding coverage.

**`bronze.wto_tariff_sectors`** – importer view

Per-MTN product group breakdown for each country as an importer. Captures the bound-applied gap (legal headroom to raise tariffs) by product group, which is a key signal for supply chain escalation risk. Columns: `iso3c`, `mtb_group`, `bound_avg`, `bound_duty_free_pct`, `bound_max`, `binding_coverage_pct`, `applied_avg`, `applied_duty_free_pct`, `applied_max`, `import_share_pct`, `bound_applied_gap`.

**`bronze.wto_export_partners`** – exporter view

Top 5 import partners for each country in agricultural and non-agricultural product scopes, showing the tariff rates those partners impose on the country's goods. Used to assess export market access risk. Columns: `iso3c`, `scope`, `partner_rank`, `partner_name`, `year`, `bilateral_imports_mn_usd`, `diversification_hs2`, `diversification_hs6`, `mfn_avg_simple`, `mfn_avg_weighted`, `pref_margin_weighted`, `duty_free_imports_pct`.

**`bronze.wto_mpia_raw`**

Binary flag: MPIA (Multi-Party Interim Appeal Arbitration Arrangement) participation. Non-participation means WTO rulings can be frozen on appeal ("appeal into the void"), a recourse-layer risk signal. One row per country.

**Update frequency:** Annual. The WTO Tariff Profiles ZIP is always the current edition; no versioning needed.

---

### USGS Mineral Commodity Summaries

**Script:** `ingest_usgs.R`

**Source:** USGS National Minerals Information Center, ScienceBase repository. Retrieved automatically via `sbtools` – no hardcoded ScienceBase IDs; new annual editions are discovered via text search. Raw CSVs are cached at `data/raw_usgs/`.

**Bronze tables:** `bronze.usgs_production`, `bronze.usgs_reserves`

**`bronze.usgs_production`**

Country-level mine production for each mineral, one row per country-mineral-year.

| Column | Description |
|--------|-------------|
| `iso3c` | Country ISO3 |
| `mineral` | Mineral name (standardized to USGS MCS names) |
| `year` | Production year |
| `production_qty` | Mine production quantity |
| `unit` | Production unit (e.g., "metric tons", "kilograms") |
| `mcs_edition` | USGS MCS edition year this estimate comes from |

Stacking MCS editions 2020-2025 (controlled by `MCS_YEAR_RANGE` in `ingest_usgs.R`) gives production history back to approximately 2018. The most recent year in each edition is an estimate; the prior year is actuals.

**`bronze.usgs_reserves`**

World total reserves by mineral and edition. Country-level reserve data exists only in the PDF version of the MCS; `usgs_reserves` captures world totals for concentration calculations.

| Column | Description |
|--------|-------------|
| `mineral` | Mineral name |
| `mcs_edition` | MCS edition year |
| `reserves_qty` | World total reserves |
| `unit` | Reserve unit |

**Mineral coverage**

Approximately 50 mineral commodities are tracked across editions. Not all Comtrade critical mineral HS codes have a USGS MCS equivalent – manufactured products (cermets, ferrocerium) and some processed forms are absent. Approximately 22 of 33 Comtrade critical mineral codes map to a distinct USGS commodity. Notable mappings: borotellurate --> tellurium; cerium --> rare earths aggregate; ferromanganese --> manganese.

**Update frequency:** Annual, published approximately January each year.

---

### GDP and Population Supplements

These two sources provide context and normalization data used in the dashboard but are not inputs to any Bayesian model.

#### GDP – Local CSV Files

**Script:** `ingest_gdp.R`

**Source:** Local CSV files at `data/gdp_raw/`, keyed on `gwcode` (Gleditsch-Ward country code), joined to `bronze.country_frame` for iso3c.

**Bronze table:** `bronze.gdp_annual`

| Column | WDI code | Description |
|--------|----------|-------------|
| `iso3c` | - | Country ISO3 |
| `year` | - | Year |
| `gdp_const_usd` | `NY.GDP.MKTP.KD` | GDP in constant USD |
| `gdp_growth_pct` | `NY.GDP.MKTP.KD.ZG` | GDP growth % |
| `gdp_per_capita_const_usd` | `NY.GDP.PCAP.KD` | GDP per capita, constant USD |
| `gdp_per_capita_growth_pct` | `NY.GDP.PCAP.KD.ZG` | GDP per capita growth % |

This table supplements the WDI GDP indicators where a consistent long-run constant-USD series is needed. Contact the repo maintainer for the source CSV files.

#### Population – US Census Bureau IDB

**Script:** `ingest_population.R`

**Source:** US Census Bureau International Data Base API (`api.census.gov/data/timeseries/idb/1year`). Requires `CENSUS_API_KEY` in `.env`.

**Bronze table:** `bronze.population_annual`

| Column | Description |
|--------|-------------|
| `iso3c` | Country ISO3 |
| `year` | Year |
| `population` | Total population, all ages, all sexes |

Joins via `iso2c` (GENC 2-letter code) to `bronze.country_frame`. Used as a denominator for per-capita normalization in dashboard displays.

**Update frequency:** Annual.

---

### Bronze Table Inventory

Complete reference of all bronze tables written by the pipeline, in execution order.

| Table | Script | Grain | Description |
|-------|--------|-------|-------------|
| `bronze.country_frame` | `run_pipeline.R` | country | 175 sovereign states with iso3c, gwcode, wbcode |
| `bronze.imf_weo_annual` | `ingest_imf.R` | country-year-indicator | IMF WEO: 5 annual macro series with forecasts |
| `bronze.imf_ifs_monthly` | `ingest_imf.R` | – | Empty placeholder (IFS SDMX endpoint unavailable) |
| `bronze.us_cpi_monthly` | `ingest_price_series.R` | year-month | US CPI index, 2024 = 100; deflation reference |
| `bronze.pink_sheet_prices` | `ingest_price_series.R` | year-month-series | World Bank Pink Sheet commodity prices |
| `bronze.imf_pcps_prices` | `ingest_price_series.R` | year-month-code | IMF PCPS prices via FRED (currently: swine) |
| `bronze.price_lookup` | `ingest_price_series.R` | cmd_code-year-month | Unified commodity price by HS code |
| `bronze.wb_wdi_raw` | `ingest_worldbank.R` | country-year-indicator | World Bank WDI: 25 indicators, long format |
| `bronze.wb_countries_raw` | `ingest_worldbank.R` | country | WB country metadata: region, income level, etc. |
| `bronze.wto_tariff_summary` | `ingest_wto.R` | country-scope | WTO Tariff Profiles cross-sectional summary |
| `bronze.wto_tariff_sectors` | `ingest_wto.R` | country-mtb_group | Bound/applied rates by MTN product group |
| `bronze.wto_export_partners` | `ingest_wto.R` | country-scope-rank | Top 5 export partner tariff rates |
| `bronze.wto_mpia_raw` | `ingest_wto.R` | country | Binary MPIA participation flag |
| `bronze.comtrade_food_net_trade` | `ingest_comtrade.R` | country-year-subcategory | Annual net trade by food subcategory |
| `bronze.comtrade_cereal_hhi` | `ingest_comtrade.R` | country-year | Cereal import source HHI |
| `bronze.comtrade_food_price_volatility` | `ingest_comtrade.R` | country-year | Intra-year CV of food import unit prices |
| `bronze.comtrade_export_hhi` | `ingest_comtrade.R` | country-year | Export commodity HHI (Pink Sheet commodities) |
| `bronze.comtrade_top5_commodities` | `ingest_comtrade.R` | country-year-flow-rank | Top 5 export and import commodities by value |
| `bronze.comtrade_commodity_exposure` | `ingest_comtrade.R` | country-year-flow | Critical mineral / energy / food trade shares |
| `bronze.comtrade_mirror_share` | `ingest_comtrade.R` | country-year-month-flow | Share of trade value estimated via mirroring |
| `bronze.usgs_production` | `ingest_usgs.R` | country-mineral-year | Country mine production by mineral |
| `bronze.usgs_reserves` | `ingest_usgs.R` | mineral-edition | World total reserves by mineral and MCS edition |
| `bronze.gdp_annual` | `ingest_gdp.R` | country-year | GDP and per-capita GDP from local CSV files |
| `bronze.population_annual` | `ingest_population.R` | country-year | Total population from US Census IDB |

---

## Macro-Fiscal Stability

**Model level:** Annual (one score per country-year)
**Gold table:** `gold.d1_macro_posterior`

D1 measures a country's latent capacity to absorb external shocks. The model estimates latent buffer capacity (theta) from three fiscal indicators. Indicators enter the model in their protective orientation – higher values mean a stronger fiscal position. Risk scores are derived as `pnorm(-theta)`, so a country with high buffer capacity receives a low risk score. Debt is negated before entering the model (`-govt_gross_debt_pct_gdp`) so that all three indicators share the same protective orientation.

| Indicator | Source | Construction |
|-----------|--------|--------------|
| `current_account_pct_gdp` | IMF WEO (WDI fallback) | Current account balance as % of GDP |
| `fiscal_balance_pct_gdp` | IMF WEO | General government net lending/borrowing as % of GDP |
| `govt_gross_debt_pct_gdp` | IMF WEO | General government gross debt as % of GDP, negated in model |

---

## Strategic Import Reliance

### Annual Model

**Model level:** Annual
**Gold table:** `gold.d2_annual_posterior`

Captures structural import concentration vulnerability using supplier diversity indicators. Indicators enter the model in their protective orientation – higher values mean a more diversified supplier base. Risk scores are derived as `pnorm(-theta)`, so a highly diversified country receives a low risk score. Diversity is computed as `1 - HHI` so that the scale runs in the same protective direction across all four indicators.

| Indicator | Source | Construction |
|-----------|--------|--------------|
| `diversity_food` | UN Comtrade | 1 - supplier HHI across food import partners |
| `diversity_energy` | UN Comtrade | 1 - supplier HHI across energy import partners |
| `diversity_cereal` | UN Comtrade | 1 - supplier HHI across cereal import partners |
| `diversity_fertilizer` | UN Comtrade | 1 - supplier HHI across fertilizer import partners (DAP, urea, potash) |

### Monthly Level Model

**Model level:** Monthly (one score per country-year-month)
**Gold table:** `gold.d2_monthly_posterior`

Captures realized import disruption signals separately for food and energy. Unlike the annual model, indicators here are pre-oriented so that higher values correspond directly to greater risk before standardization – volume shortfalls are negated, price spikes are positive, and volatility measures are positive by construction. Risk scores are derived as `pnorm(theta)` with no sign flip.

| Indicator | Source | Construction |
|-----------|--------|--------------|
| `food_vol_risk` | UN Comtrade | Food import volume anomaly z-score, negated – a shortfall relative to the same-month seasonal baseline produces a positive value |
| `food_price_risk` | UN Comtrade / Pink Sheet | Food import basket price z-score – a price spike above the seasonal baseline produces a positive value |
| `food_basket_vol` | Pink Sheet / UN Comtrade | `food_cv_basket_wtd`: SD of monthly log price first differences over a 12-month window (lagged 1 month), weighted by lagged food import value shares. Morgan-Reinhardt (2016) specification. |
| `energy_basket_vol` | Pink Sheet / UN Comtrade | `energy_cv_basket_wtd`: same Morgan-Reinhardt construction applied to energy imports |
| `food_input_basket_vol` | Pink Sheet / UN Comtrade | `fertilizer_cv_basket_wtd`: same Morgan-Reinhardt construction applied to fertilizer imports (DAP, urea, potash) |

---

## Export Fragility

### Annual Model

**Model level:** Annual
**Gold table:** `gold.d3_annual_posterior`

Measures structural export concentration and revenue dependence. Like the D2 annual model, indicators enter in their protective orientation – higher values mean greater diversification or independence. Risk scores are derived as `pnorm(-theta)`.

| Indicator | Source | Construction |
|-----------|--------|--------------|
| `diversity_partner` | UN Comtrade | 1 - bilateral export partner HHI |
| `diversity_commodity` | UN Comtrade | 1 - export commodity HHI |
| `export_independence` | UN Comtrade | 1 - Pink Sheet commodity value share of total exports |

### Monthly Level Model

**Model level:** Monthly
**Gold table:** `gold.d3_monthly_posterior`

Like the D2 monthly model, indicators are pre-oriented so that higher values correspond to greater risk. Volume shortfalls and price collapses are negated before entering the model; price volatility is positive by construction. Risk scores are derived as `pnorm(theta)` with no sign flip.

| Indicator | Source | Construction |
|-----------|--------|--------------|
| `export_vol_risk` | UN Comtrade | Export volume anomaly z-score, negated – a shortfall below the seasonal baseline produces a positive value |
| `export_price_risk` | UN Comtrade / Pink Sheet | Export basket price z-score, negated – a price collapse below the seasonal baseline produces a positive value |
| `basket_price_vol_risk` | Pink Sheet / UN Comtrade | `cv_basket_wtd`: SD of monthly log price first differences over a 12-month window (lagged 1 month), weighted by lagged export value shares. Morgan-Reinhardt (2016) specification. |

**Interpretation thresholds for `export_basket_price_z`**

- Below -1.5: Acute export revenue pressure
- Above +1.5: Windfall conditions

**Interpretation thresholds for `cv_basket_wtd`**

- > 0.30: Elevated price instability
- > 0.40: Structural concern

---

## Network Contagion Risk

**Model level:** Monthly and annual
**Gold tables:** `gold.d4_monthly`, `gold.d4_annual`, `gold.d4_partner_detail`

D4 is a deterministic aggregation – no Bayesian model. A country's D4 score is the trade-weighted average of its import partners' composite risk scores (D1 + D2 + D3). A country sourcing critical imports from structurally fragile partners inherits a share of their vulnerability.

Trade weights are restricted to Pink Sheet commodity categories, food, and energy imports. Weights are annual bilateral import shares, normalized within reporter-year. Credible intervals propagate from D1-D3 bounds through the same weighted-average formula.

`gold.d4_partner_detail` provides a full per-partner breakdown for each reporter country, including the partner's individual D1/D2/D3 scores, composite risk score, and normalized trade weight. It also includes four bilateral category share columns that identify the channel through which each partner relationship creates exposure:

| Column | Description |
|--------|-------------|
| `food_import_share` | Share of reporter's total food imports sourced from this partner |
| `fertilizer_import_share` | Share of reporter's total fertilizer imports sourced from this partner |
| `energy_import_share` | Share of reporter's total energy imports sourced from this partner |
| `export_share` | Share of reporter's total exports destined for this partner |

Source: `silver.bilateral_category_shares` (computed in `compute_silver_comtrade.R` before `comtrade_raw` is dropped).

---

## Context Indicators

**Gold table:** `gold.context_indicators`

Context indicators are excluded from the Bayesian models due to high missingness or annual-only availability, but are surfaced as dashboard annotations. Their presence enriches interpretation; their absence does not degrade the modeled risk score.

| Indicator | Source | Threshold / Notes |
|-----------|--------|-------------------|
| `reserves_months_imports` | World Bank WDI | Warn < 3 months |
| `ext_debt_to_reserves` | IMF / WDI | Warn > 1.0 |
| `debt_service_ratio` | World Bank WDI | Warn > 20% |
| `major_producer` | USGS | TRUE = >5% global share in at least one mineral |
| `n_dominant_minerals` | USGS | Count of minerals where global production share > 5% |
| `most_concentrated_mineral` | USGS | Mineral with highest global HHI among minerals where country has >1% share |
| `gdp_const_usd` | World Bank WDI | GDP in constant USD |
| `gdp_per_capita_const_usd` | World Bank WDI | GDP per capita in constant USD |
| `gdp_growth_pct` | World Bank WDI | Annual GDP growth rate (%) |
| `population` | US Census IDB | Total population, all ages |

---

## Risk Tier Classification

**Gold table:** `gold.risk_tiers`

Tiers are assigned annually from the four dimensional scores. Classification proceeds in strict priority order:

1. **Single-dimension trigger:** Any dimension >= 0.975 --> Critical immediately, regardless of other scores.
2. **Pattern rules** (evaluated in order Critical --> Low; both conditions must hold):

| Tier | Mean score AND | n dimensions above threshold |
|------|----------------|------------------------------|
| Critical | >= 0.75 | 3+ dims above 0.70 |
| Elevated | >= 0.60 | 2+ dims above 0.65 |
| Guarded | >= 0.45 | 2+ dims above 0.50 |
| Moderate | >= 0.30 | 1+ dim above 0.40 |
| Low | catch-all | – |

3. **Context flag boost:** Active alert flags (reserves, external debt, debt service) raise a country one tier above its pattern-rule assignment.
