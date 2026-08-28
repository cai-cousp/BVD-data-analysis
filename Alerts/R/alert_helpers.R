# Shared helper functions for Alerts analysis
# Purpose: small, well-documented utilities used by the threshold and trend scripts
# Note: keep implementation and comments aligned; avoid changing behavior here without tests


suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(incidence2)
  library(EpiEstim)
})

#' Load the most recent cleaned line list (EVD or contact)
#' @param base_path Character. Path to the Output folder containing dated
#'   directories (and nested cleaning subfolders).
#' @param pattern Character. Regular expression for the target files.
#'   Default: `"evd.clean_Int_.*\\.rds"` (EVD line list). Use
#'   `"contact.clean_Int_.*\\.rds"` for the cleaned contact follow-up data.
load_latest_evd <- function(base_path, pattern = "evd.clean_Int_.*\\.rds") {
  files <- list.files(path = base_path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) {
    rlang::abort(sprintf("No cleaned data file matching pattern '%s' found in %s.", pattern, base_path))
  }
  
  # Find the most recent file based on the numeric timestamp in the filename
  latest_file <- files[which.max(as.numeric(gsub("\\D", "", basename(files))))]

  data <- readRDS(latest_file)

  # Data dictionary migration: the canonical column name for the final case
  # classification is `classification_finale`. Older clean exports used
  # `classification_finale_cas`; rename those here so all downstream helpers
  # can use the canonical name.
  if (!is.null(data) &&
      "classification_finale_cas" %in% names(data) &&
      !"classification_finale" %in% names(data)) {
    names(data)[names(data) == "classification_finale_cas"] <- "classification_finale"
  }

  data
}

# compute_epiweek() (ISO/Monday epiweek) is sourced from
# alert_helpers/compute_epiweek.R and is used only for the EVD10 Beni
# historical benchmark (Approach B), not for the live analysis grid.
# See alert_helpers/window_utils.R for the harmonized grid convention.

#' Expected community deaths per HZ based on CMR
#' @param pop Numeric. Population.
#' @param cmr Numeric. Crude mortality rate per 1000 per year.
#' @param period_days Numeric. Number of days in the period (default 7 for week).
expected_deaths_baseline <- function(pop, cmr, period_days = 7) {
  (pop * (cmr / 1000) / 365) * period_days
}

# Helper functions are sourced directly from alert_helpers/ below.

#' Create dated output dir
#' @param base_dir Character. Base output directory.
create_output_dir <- function(base_dir) {
  today_dir <- file.path(base_dir, format(Sys.Date(), "%Y_%m_%d"))
  if (!dir.exists(today_dir)) {
    dir.create(today_dir, recursive = TRUE)
  }
  today_dir
}

# Project root paths
# here::here() resolves to Alerts/ (the project root, marked by Alerts.Rproj)
evd17_root <- file.path(here::here(), "..", "..")

# Source all helper files from alert_helpers/ in dependency order
source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/window_utils.R"))
source(here::here("alert_helpers/compute_epiweek.R"))
source(here::here("alert_helpers/compute_recent_confirmed_windows_by_hz.R"))
source(here::here("alert_helpers/compute_detection_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/combine_detection_estimates_by_hz.R"))
source(here::here("alert_helpers/compute_alert_multipliers.R"))
source(here::here("alert_helpers/join_thresholds_to_alert_counts.R"))
source(here::here("alert_helpers/build_ensemble_thresholds.R"))
source(here::here("alert_helpers/compute_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/compute_growth_rate_by_hz.R"))
source(here::here("alert_helpers/compute_detection_by_hz.R"))
source(here::here("alert_helpers/compute_cfr_by_hz.R"))
source(here::here("alert_helpers/compute_rt_hz.R"))
source(here::here("alert_helpers/nowcast_epinow2.R"))
source(here::here("alert_helpers/confirmed_incidence.R"))
source(here::here("alert_helpers/nowcast_by_zone.R"))
source(here::here("alert_helpers/load_latest_evd.R"))
source(here::here("alert_helpers/compute_contacts_per_case_by_hz.R"))
source(here::here("alert_helpers/expected_deaths_baseline.R"))
source(here::here("alert_helpers/expected_alerts_from_cases.R"))
source(here::here("alert_helpers/compute_delays_by_hz.R"))
source(here::here("alert_helpers/create_output_dir.R"))
source(here::here("alert_helpers/compute_last_window_projection.R"))
source(here::here("alert_helpers/setup.R"))
