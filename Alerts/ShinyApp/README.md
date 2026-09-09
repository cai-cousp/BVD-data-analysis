# BVD Alert Thresholds & Trends

A polished, website-style Shiny application for interactive exploration of Ebola Virus Disease (BVD) alert thresholds and trends across health zones in the Democratic Republic of Congo.

> 🌐 **Live GitHub Pages App**: [https://cai-cousp.github.io/BVD-Alert-performance/](https://cai-cousp.github.io/BVD-Alert-performance/)

## Overview

This dashboard provides epidemiologists with interactive surveillance monitoring:

- **Trends** — Longitudinal alert counts, threshold bands, adequacy indices, and model parameter synthesis tables (at aggregate ensemble and individual health zone levels)
- **Interactive notification map** — Leaflet health-zone performance map with three-window hover indicators. Clicking a health zone synchronizes the health-zone section and its map-linked longitudinal tables.
- **Export** — Report downloads (HTML) and data exports (Excel, CSV)

## Data Sources

The app consumes pre-computed outputs from the alert analysis pipeline. It does **not** re-run any computational pipelines.

| File | Source Script | Description |
|------|--------------|-------------|
| `01_thresholds_synthesis.rds` | `R/01_alert_thresholds.R` | Longitudinal threshold estimates per HZ per week (31 columns) |
| `01_intermediate_parameters.rds` | `R/01_alert_thresholds.R` | CFR, detection rates, Rt, SAR, multipliers |
| `02_trends_smooth.rds` | `R/02_alert_trends.R` | Alert counts joined with thresholds and adequacy indices (42 columns) |
| `02_recent_adequacy.xlsx` | `R/02_alert_trends.R` | Per-HZ adequacy summary with trend direction |

Data is loaded automatically from the most recent dated subdirectory in `Alerts/output/`.

## File Structure

```
ShinyApp/
├── app.R                      # Launch script (entry point)
├── ui.R                       # UI definition (website-style layout)
├── server.R                   # Server definition (reactive flow, module init)
├── report_template.qmd        # Parameterized Quarto report template
├── README.md
├── R/
│   ├── global.R               # Data loading, shared constants (sourced once)
│   ├── mod_utils.R            # Filter module, litera theme, helpers
│   ├── mod_trends.R           # Tab 1: Trend charts, adequacy, synthesis tables
│   ├── map_data_helpers.R     # Interactive notification map and selected-zone tables
│   ├── mod_map.R              # Leaflet map module
│   └── mod_export.R           # Tab 2: Export handlers
├── www/
│   └── custom.css             # Website-style responsive styling
└── tests/
    └── testthat/
        ├── test-mod_data_loading.R
        ├── test-map_data_helpers.R
        └── test-mod_trends_tables.R
```

`app.R` is the launch entry point. It sources `ui.R` and `server.R`,
then calls `shinyApp(ui, server)`. Both `ui.R` and `server.R` source their
own dependencies; `global.R` is loaded only once thanks to the
`.bvd_global_loaded` guard flag.

## Dependencies

```r
# Core Shiny
shiny, bslib, bsicons, shinyWidgets

# Data manipulation
dplyr, tidyr, purrr, rlang, readr, readxl, tibble, stringr, slider

# Visualization
plotly, DT, leaflet, sf

# Utilities
here

# Report generation (optional)
quarto, knitr, kableExtra, ggplot2, writexl
```

## Running the App

```r
# From the ShinyApp directory
setwd("Alerts/ShinyApp")
shiny::runApp(".")
```

Or from R console:

```r
shiny::runApp("/path/to/Alerts/ShinyApp")
```

Shiny detects `app.R` at the app root. You can also source `ui.R` and
`server.R` independently for development:

```r
source("ui.R")      # defines `ui`
source("server.R")  # defines `server`
shinyApp(ui, server)
```

## Prerequisites

1. Run `R/01_alert_thresholds.R` to generate threshold synthesis data
2. Run `R/02_alert_trends.R` to generate trends and adequacy data
3. Launch the app — it auto-detects the latest output directory

## Design

- **Theme**: `litera` Bootswatch — clean, publication-quality aesthetic (replaces dashboard-style `flatly`)
- **Layout**: Website-style with `page_navbar(fillable = FALSE)`, horizontal filter bar, stacked cards
- **No sidebar**: Filters are integrated into each tab's content area
- **No value boxes**: Replaced with elegant card-based layout
- **Responsive**: Mobile-friendly with CSS media queries

## Export Features

- **HTML report**: Self-contained parameterized Quarto report
- **Excel export**: Multi-sheet workbook with filtered data and metadata
- **CSV export**: Comma-separated trends data
- **Interactive chart export**: Built-in plotly PNG download per chart
- **Table export**: DT buttons for copy/CSV/Excel per table

## Methodology Reference

See the methods notes for detailed computational approaches:
- `methods_note_alert_thresholds.qmd` — Threshold computation methods
- `methods_note_alert_trends.qmd` — Trend analysis and adequacy methods

## Deployment to GitHub Pages (Shinylive / webR)

The application can be deployed as a **zero-server static web application** on **GitHub Pages** using Posit's [Shinylive for R](https://github.com/posit-dev/r-shinylive), which runs R directly inside the user's browser via **webR (WebAssembly)**.

### Architecture
- **Self-Contained Bundle**: The app loads pre-computed indicators from `ShinyApp/data/` (manifest, synthesis, trends, and lightweight spatial layers).
- **Optimized Map Geometry**: Spatial boundaries for affected provinces and health zones are compressed and simplified down to ~112 KB (from 54+ MB raw data), allowing fast client-side map rendering.
- **Dual-Mode Operation**: When run locally with live analysis pipelines present, `global.R` continues auto-detecting new outputs in `Alerts/output/`. In Shinylive or when deployed statically, it seamlessly falls back to `ShinyApp/data/`.

### 1. Preparing Bundled Data
To refresh the bundled datasets from the latest analysis output:
```bash
Rscript ShinyApp/deploy/prepare_bundled_data.R
```

### 2. Exporting & Previewing Locally
To export the static site and launch an instant local preview server:
```bash
# Export and start preview server at http://127.0.0.1:8080
Rscript ShinyApp/deploy/build_shinylive.R --serve --port 8080
```

### 3. Automated GitHub Actions CI/CD
A GitHub Actions workflow is located at `.github/workflows/deploy-shinylive.yml`.

Every push to `main` involving `Alerts/ShinyApp/**` or `Alerts/output/**` will automatically:
1. Bundle the latest analysis outputs and lightweight map layers.
2. Export the site with `shinylive::export()`.
3. Publish to GitHub Pages via `actions/deploy-pages@v4`.

#### Enabling GitHub Pages in Repository Settings
1. Go to repository **Settings > Pages** on GitHub.
2. Under **Build and deployment > Source**, select **GitHub Actions**.
3. Pushes to `main` (or triggering the workflow manually via `workflow_dispatch`) will publish the app to GitHub Pages: [https://cai-cousp.github.io/BVD-Alert-performance/](https://cai-cousp.github.io/BVD-Alert-performance/).
