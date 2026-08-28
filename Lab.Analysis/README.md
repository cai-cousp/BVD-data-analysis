# Laboratory Performance & Sample Positivity Analysis (EVD17 / MVE RDC)

An integrated epidemiological and laboratory analytics pipeline for monitoring Ebola Virus Disease (EVD17 / Maladie à Virus Ebola - Bundibugyo) surveillance in the Democratic Republic of the Congo (RDC), focusing on Ituri and Nord-Kivu provinces.

This repository provides end-to-end data processing, sample flow tracking, testing coverage analysis, turnaround time evaluation, spatial prioritization, and automated dynamic reporting.

---

## Table of Contents

- [Overview](#overview)
- [Key Features](#key-features)
- [Tech Stack & Requirements](#tech-stack--requirements)
- [Project Structure](#project-structure)
- [Installation & Setup](#installation--setup)
- [Usage & Entry Points](#usage--entry-points)
  - [1. Main Analysis Pipeline](#1-main-analysis-pipeline)
  - [2. Sample Positivity Wrapper](#2-sample-positivity-wrapper)
  - [3. Quarto Epidemiological Report](#3-quarto-epidemiological-report)
- [Data Inputs & Outputs](#data-inputs--outputs)
  - [Input Data Sources](#input-data-sources)
  - [Generated Outputs](#generated-outputs)
- [Environment Variables & Runtime Options](#environment-variables--runtime-options)
- [Testing & Quality Assurance](#testing--quality-assurance)
- [Scripts & Helpers Reference](#scripts--helpers-reference)
- [License & Acknowledgments](#license--acknowledgments)
- [TODOs & Future Improvements](#todos--future-improvements)

---

## Overview

During the EVD17 outbreak response, real-time tracking of diagnostic laboratory performance is critical for case detection, turnaround bottleneck identification, and resource allocation. This project standardizes raw case notifications and laboratory records to compute key epidemiological indicators across multiple administrative levels (National, Provincial, Health Zone / *Zone de Santé*):

- **Alert-to-Lab Cascade:** Tracking the transition from validated alert notifications to field sample collection, transport, laboratory reception, and diagnostic testing.
- **21-Day Rolling Surveillance Window:** Comparing overall performance against recent (last 21 days) active transmission metrics.
- **Positivity Rates:** Calculating positivity on first diagnostic samples per case (avoiding bias from repeat monitoring samples) with exact binomial (Clopper-Pearson) 95% confidence intervals.
- **Turnaround Times (TAT):** Multi-interval duration tracking (onset $\rightarrow$ collection, collection $\rightarrow$ reception, reception $\rightarrow$ analysis, collection $\rightarrow$ result, onset $\rightarrow$ result) with median, IQR, and target compliance proportions.
- **Spatial Prioritization:** Multi-criteria scoring combining sample volume, positivity, and geographic accessibility to guide laboratory network deployment.

---

## Key Features

- **Standardized Multi-level Aggregations:** Computes metrics at national (`all`), provincial (`prov`), and health zone (`zs`) granularities.
- **Robust Statistical Summaries:** Exact Clopper-Pearson 95% confidence intervals for proportions and rates; median/quartile metrics for skewed delay distributions.
- **Interactive Quarto Reporting:** Generates self-contained HTML reports with embedded high-resolution figures (300 DPI PNG) and Excel export capabilities via base64 data URIs.
- **Modular Pipeline Architecture:** Clean separation of sample-level metrics, individual/case-level metrics, destination lab tracking, and visualization routines.

---

## Tech Stack & Requirements

### Core Requirements
- **R:** Version $\ge 4.1.0$ (R 4.2+ recommended)
- **Quarto CLI:** Version $\ge 1.3$ (required for rendering `.qmd` reports)
- **Google Chrome / Chromium:** Optional, required by `webshot2` for headless snapshot generation of HTML tables.

### Key R Packages
- **Package Manager:** `pacman` (automatically manages and installs dependencies when running in standard mode)
- **Data Wrangling & Manipulation:** `dplyr`, `tidyr`, `purrr`, `lubridate`, `stringr`, `stringi`, `stringdist`, `zoo`, `tidytext`
- **Epidemiology & Statistics:** `incidence`, `forecast`, `DescTools`, `scales`
- **Data Visualization:** `ggplot2`, `ggforce` (parallel sets / alluvial flows), `ggthemes`, `ggrepel`, `ggfittext`, `RColorBrewer`, `wesanderson`
- **Reporting & Office I/O:** `knitr`, `kableExtra`, `openxlsx`, `readxl`, `here`, `webshot2`
- **Testing:** `testthat`

---

## Project Structure

```text
.
├── lab.analysis.R                      # Main analysis & visualization pipeline
├── sample_positivity.R                 # Standalone CLI wrapper for positivity tables
├── rapport_echantillons_positivite.qmd # Quarto report source (French)
├── rapport_echantillons_positivite.html# Rendered self-contained HTML report
├── rapport-echantillons-purl.R         # R code purled from the Quarto document
├── _dl_buttons.html                    # HTML/CSS header assets for download buttons
├── lab_alert_coverage.R.new            # Alert coverage & individual-level indicator helper
├── lab_indicator_prop.R.new            # Sample-level indicators, counts & turnaround helper
├── test_lab_analysis_pipeline.R.new    # Integration smoke test script
├── test-lab_alert_coverage.R.new       # Unit tests for alert coverage & 21-day window
├── test-lab_indicator_prop.R.new       # Unit tests for sample indicators & proportions
├── helpers/                            # Project helper modules (sourced via here::here)
│   ├── paths.R                         # Path resolution & project directories
│   ├── DebutSem.R                      # Epidemiological week date calculations
│   ├── LoadLatestData.R                # Dynamic loader for latest RDS data exports
│   ├── lab_long.R                      # Data shaping to long format & result harmonization
│   ├── lab_indicators.R                # Per-case indicator aggregations
│   ├── lab_indicator_prop.R            # Sample-level indicator & turnaround computations
│   ├── lab_indicator_prop_lab.R        # Destination laboratory aggregations
│   ├── lab_alert_coverage.R            # Alert validation, sampling & testing coverage
│   ├── lab_positivity.R                # First-sample positivity rate calculations
│   ├── lab_plots.R                     # ggplot2 themes and plotting routines
│   ├── lab_weekly.R                    # Weekly epidemiological aggregations
│   └── clean_filename.R                # Filename string sanitization
└── README.md                           # Project documentation
```

---

## Installation & Setup

1. **Clone the repository and set the working directory:**
   ```bash
   git clone <repository_url>
   cd Lab.Analysis
   ```

2. **Install R dependencies:**
   Launch R and install `pacman`, which handles the remaining required packages:
   ```r
   if (!require("pacman", quietly = TRUE)) install.packages("pacman")
   pacman::p_load(
     "openxlsx", "readxl", "incidence", "stringdist", "lubridate",
     "RColorBrewer", "wesanderson", "dplyr", "tidyverse", "stringi",
     "ggforce", "ggfittext", "ggthemes", "ggrepel", "tidytext",
     "zoo", "forecast", "DescTools", "scales", "kableExtra", "here"
   )
   ```

3. **Verify Quarto installation:**
   ```bash
   quarto --version
   ```

---

## Usage & Entry Points

### 1. Main Analysis Pipeline
Executes data extraction, filtering to health zones with confirmed cases, metric computation, figure generation, and Excel exports:
```bash
Rscript lab.analysis.R
```
*Or within R/RStudio:*
```r
source("lab.analysis.R")
```

### 2. Sample Positivity Wrapper
Computes national, provincial, and health zone positivity overviews (overall and weekly by symptom onset date) and exports them to `.rds` and `.xlsx`:
```bash
Rscript sample_positivity.R
```

### 3. Quarto Epidemiological Report
Renders the French-language analytical report into a standalone, interactive HTML document:
```bash
quarto render rapport_echantillons_positivite.qmd
```
*Note: During rendering, the report automatically executes `lab.analysis.R` with `options(lab.analysis.report_mode = TRUE)` to populate memory without triggering redundant file exports.*

---

## Data Inputs & Outputs

### Input Data Sources
The pipeline expects upstream cleaned data artifacts (configured via `helpers/paths.R` and loaded via `load_latest_data()`):

| Data Source | Description | Format | Expected Location |
|---|---|---|---|
| `evd.cleaning` | Cleaned case line list (epidemiological and clinical data) | `.rds` | `data_cleaning_output/evd.cleaning/` |
| `lab.cleaning` | Cleaned laboratory sample line list (dates, PCR results, labs) | `.rds` | `data_cleaning_output/lab.cleaning/` |
| `20250701 RDC_ZS_Accessibilite.xlsx` | *(Optional)* Geographic accessibility and travel time index | `.xlsx` | `data/` or `helpers/` |

### Generated Outputs
Outputs are organized under `OutPut/Lab/<refdate>/` (where `<refdate>` is the latest laboratory analysis date formatted as `DDMon`, e.g., `15Apr`):

- **Data Tables & Workbooks (`.xlsx` & `.rds`):**
  - `lab_analysis_summary_<refdate>.xlsx` (Multi-tab indicators for National, Province, Health Zone, and Destination Labs)
  - `sample_positivity_<refdate>.xlsx` (Overview of 12 positivity tables)
  - `lab_priority_areas_<refdate>.xlsx` (Spatial prioritization score ranking)
- **High-Resolution Figures (300 DPI PNG):**
  - `coverage_labflow_prov_<refdate>.png` (Alert-to-lab cascade by province)
  - `testing_rate_prov_21d_<refdate>.png` (Testing rates: Overall vs last 21 days)
  - `sample_flow_<refdate>.png` (Collected vs Arrived vs Processed volume)
  - `result_breakdown_<refdate>.png` (Positive, Negative, and Other result distribution)
  - `turnaround_boxplot_<refdate>.png` (Distribution of delays across process stages)
  - `positivity_trend_<refdate>.png` & `turnaround_trend_<refdate>.png` (Weekly epidemiological trends)
  - `lab_destination_by_zs_<refdate>.png` (Parallel sets alluvial diagram)

---

## Environment Variables & Runtime Options

| Option / Variable | Type | Default | Description |
|---|---|---|---|
| `lab.analysis.report_mode` | R Option (`logical`) | `FALSE` | When set to `TRUE`, disables file exports (`ggsave`, `write.xlsx`, `saveRDS`) and interactive `View()` calls for in-memory execution inside Quarto reports. |
| `data_cleaning_output` | R Variable | Defined in `helpers/paths.R` | Path pointing to upstream cleaned RDS directory. |
| `project_root` | R Variable | Defined in `helpers/paths.R` | Base directory for project outputs and relative paths. |

---

## Testing & Quality Assurance

The codebase includes automated unit and integration tests using `testthat`:

- **Integration Smoke Test (`test_lab_analysis_pipeline.R`):**
  Validates table schema invariants, column standardization, sample partitioning (`n_positive + n_negative + n_other == total`), first-sample deduplication, and weekly aggregations.
  ```bash
  Rscript test_lab_analysis_pipeline.R.new
  ```

- **Unit Tests (`testthat`):**
  - `test-lab_alert_coverage.R.new`: Tests alert validation rules, sample collection/reception coverage, 21-day window calculations, and handling of missing geography.
  - `test-lab_indicator_prop.R.new`: Tests sample volume counters, turnaround time quantiles, threshold adherence calculations, and first-sample positivity metrics.

To execute tests within R:
```r
testthat::test_file("test-lab_alert_coverage.R.new")
testthat::test_file("test-lab_indicator_prop.R.new")
```

---

## Scripts & Helpers Reference

- **`compute_lab_indicator_prop(long_data, level, positivity_reference_date)`**
  Computes sample-level indicators returning a list of tibbles: `$counts` (volume, results, positivity all/1st/21d), `$turnaround` (medians/IQRs), and `$prop` (threshold compliance).
- **`compute_alert_lab_coverage(lab_data, level, testing_reference_date)`**
  Computes individual-level cascade indicators: validated alerts, sampled alerts, received alerts, and diagnostic testing coverage overall and within a 21-day notification window.
- **`compute_lab_indicator_prop_lab(long_data)`**
  Computes performance and volume metrics stratified by diagnostic laboratory (`lab_destination`).
- **`compute_positivity_overview(lab_data)`**
  Generates the standard 12 positivity summary tables (national, province, health zone $\times$ overall / weekly).
- **`theme_lab()`**
  Standardized clean `ggplot2` theme for publication-ready outbreak visualizations.

---

## License & Acknowledgments

- **Status / License:** Internal outbreak analytics & surveillance tool for the EVD17 Outbreak Response (Cellule d'Analyses Intégrées / Integrated Outbreak Analytics - IOA / CAI).
- **TODO:** Determine formal open source licensing (e.g., MIT, GPL-3.0) if releasing publicly.

---

## TODOs & Future Improvements

- [ ] **Dependency Lockfile:** Implement `renv` lockfile (`renv.lock`) to freeze R package versions across deployment environments.
- [ ] **Automated CI/CD:** Add GitHub Actions / GitLab CI workflows to run test suites on commit.
- [ ] **Automated Accessibility Data Ingestion:** Formalize column mapping for `data/20250701 RDC_ZS_Accessibilite.xlsx` to replace the default static accessibility score.
- [ ] **Database / API Connector:** Support direct ingestion from DHIS2 / SORMAS instances in addition to local RDS exports.
