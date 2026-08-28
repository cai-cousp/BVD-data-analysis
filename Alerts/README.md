# EVD17 Alerts — Thresholds and Trends

R analysis pipeline for the 17th Ebola Virus Disease (EVD17) outbreak response
in the Democratic Republic of the Congo. The project computes weekly
**alert thresholds** and **trend / adequacy metrics** per health zone from
multiple data sources (baseline mortality, the historical Beni alert database,
and the current case line list), and tracks whether each health zone is
generating the expected volume of validated alerts.

> **The Shiny dashboard is a separate project** and lives in its own GitHub
> repository. This repository contains only the analysis pipeline, helpers,
> tests, and documentation.

## Context

The EVD17 outbreak response requires a systematic framework to evaluate
whether the surveillance system is detecting enough alerts. The pipeline
answers three core questions:

1. What is the expected weekly volume of validated alerts per health zone?
2. Are health zones generating that expected volume?
3. Is the alert volume changing over time, and is the surveillance adequate?

See [docs/README.md](docs/README.md) for the consolidated documentation index
(glossaries, methods notes, and plans).

## Repository structure

```
.
├── R/                          # Core analysis scripts
│   ├── 01_alert_thresholds.R
│   ├── 01b_alert_thresholds_windows.R
│   ├── 02_alert_trends.R
│   └── 03_alert_mapping_capacity.R
├── alert_helpers/              # Modular helper functions
├── tests/testthat/             # testthat unit and integration tests
├── trend_viewer.R              # Standalone trend viewer script
└── docs/                       # Consolidated documentation
    ├── README.md
    ├── glossaries/             # Column definitions for output tables
    ├── methods/                # Quarto methods notes (EN + FR)
    └── plans/                  # Historical implementation plans
```

> **Methodology design notes** (project brief, CFR methods, Poisson offset
> model) live locally under `docs/methodology/` and are **not** redistributed
> with this repository. See `.gitignore` for the exclusion pattern.

## Reproducing the analysis

### Required R packages

This project does not yet use `renv`. Install the following packages before
running the scripts or tests:

```r
install.packages(c(
  "tidyverse",   # data manipulation and plotting
  "testthat",    # test framework
  "quarto",      # render .qmd methods notes
  "sf",          # spatial features (for mapping)
  "readxl",      # read .xlsx inputs
  "openxlsx",    # write .xlsx outputs
  "epitools",    # epidemiology helpers
  "trend"        # Mann-Kendall trend tests
))
```

Additional packages may be required by individual scripts; errors will name
the missing package.

### Run the tests

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

### Run the analysis

Each script in `R/` is an entry point. They expect the external data layout
described below. Run them in numerical order:

```bash
Rscript R/01_alert_thresholds.R
Rscript R/01b_alert_thresholds_windows.R
Rscript R/02_alert_trends.R
Rscript R/03_alert_mapping_capacity.R
```

Outputs are written under `output/<YYYY_MM_DD>/` (gitignored; regenerated on
each run).

### Render the methods notes

```bash
quarto render docs/methods/methods_note_alert_thresholds.qmd
quarto render docs/methods/methods_note_alert_trends.qmd
```

Rendered HTML files are gitignored.

## Data dependencies

The scripts read inputs from paths **outside** this repository. These inputs
are not redistributed:

| Path (relative to this project) | Description |
|---|---|
| `../../DataCleaning/data/Output/` | Cleaned EVD case line list (latest snapshot) |
| `../../Maps/health_zones/*.gpkg` | Health-zone GIS shapefiles |
| `../../DataAnalysis/data/Alerts/Alert_Beni.rds` | Historical alerts (Beni 2018–20) |
| `../../DataAnalysis/data/20250701 RDC_ZS_Accessibilité.xlsx` | Health-zone accessibility |
| `../../DataAnalysis/data/PopulationParAge/DRC_ZS_DHIS2_Pop_2024.xlsx` | Population by health zone (DHIS2 2024 projection) |

To run the pipeline you must place these inputs in the expected locations
(see the project brief in the local `docs/methodology/alerts.md` for the full
data specification — note: this file is not redistributed with the public
repository).

## License

[MIT](LICENSE) — see `LICENSE` for the full text.
