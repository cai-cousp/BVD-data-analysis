# BVD Alert Thresholds & Trends

A polished, website-style Shiny application for interactive exploration of Ebola Virus Disease (BVD) alert thresholds and trends across health zones in the Democratic Republic of Congo.

## Overview

This dashboard provides epidemiologists with interactive surveillance monitoring:

- **Trends** — Longitudinal alert counts, threshold bands, adequacy indices, and model parameter synthesis tables (at aggregate ensemble and individual health zone levels)
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
│   └── mod_export.R           # Tab 2: Export handlers
├── www/
│   └── custom.css             # Website-style responsive styling
└── tests/
    └── testthat/
        ├── test-mod_data_loading.R
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
