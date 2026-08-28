# =============================================================================
# BVD Alerts Dashboard — Global data loading and shared constants
# =============================================================================
# This file runs once at app startup. It loads pre-computed data from the
# alert threshold and trend analysis pipelines. The app is a pure visualization
# layer — it does NOT re-run EpiEstim, Poisson models, or back-calculation.
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(readr)
library(readxl)
library(sf)
library(leaflet)
library(plotly)
library(DT)
library(shinyWidgets)
library(here)
library(stringr)
library(slider)
library(writexl)

# --- Paths -------------------------------------------------------------------
# Locate the ShinyApp root directory robustly
find_shiny_app_dir <- function() {
  # Check current directory and parents
  for (parent in c(".", "..", "../..", "../../..")) {
    cand <- normalizePath(file.path(getwd(), parent), mustWork = FALSE)
    if (dir.exists(file.path(cand, "R")) && file.exists(file.path(cand, "app.R"))) {
      return(cand)
    }
  }
  # Fallback relative to source file
  tryCatch(
    normalizePath(file.path(dirname(sys.frame(1)$ofile), "..")),
    error = function(e) normalizePath(getwd())
  )
}

app_dir <- find_shiny_app_dir()
root_dir <- dirname(app_dir)
output_base <- file.path(root_dir, "output")
data_folder <- file.path(dirname(dirname(root_dir)), "DataCleaning", "data", "Output")
# Maps are at EVD17/Maps/health_zones/ — three levels up from ShinyApp
map_dir <- file.path(dirname(dirname(root_dir)), "Maps", "health_zones")

message("App directory: ", app_dir)
message("Output base:   ", output_base)
message("Data folder:   ", data_folder)
message("Map directory: ", map_dir)

# --- Source project plotting and helper routines -----------------------------
if (file.exists(file.path(root_dir, "R", "alert_helpers.R"))) {
  source(file.path(root_dir, "R", "alert_helpers.R"))
}
if (file.exists(file.path(root_dir, "R", "alert_plots.R"))) {
  source(file.path(root_dir, "R", "alert_plots.R"))
}

# --- EVD / Output Metadata and Pipeline Synchronization ----------------------

#' Retrieve information about the latest cleaned EVD line list
#' @param data_folder Character path to DataCleaning output directory.
#' @return A list with `path`, `filename`, and `date_stamp`.
get_latest_evd_info <- function(data_folder) {
  if (!dir.exists(data_folder)) {
    return(list(path = NULL, filename = NULL, date_stamp = NULL))
  }
  evd_file <- tryCatch(
    latest_evd_file(data_folder),
    error = function(e) NULL
  )
  if (is.null(evd_file) || length(evd_file) == 0L) {
    return(list(path = NULL, filename = NULL, date_stamp = NULL))
  }
  evd_stamp <- extract_evd_date_stamp(basename(evd_file))
  list(path = evd_file, filename = basename(evd_file), date_stamp = evd_stamp)
}

#' Retrieve information about the latest exported analysis artifacts
#' @param output_base Character path to Alerts output directory.
#' @return A list with `dir`, `synthesis_path`, and `date_stamp`.
get_latest_exported_info <- function(output_base) {
  if (!dir.exists(output_base)) {
    return(list(dir = NULL, synthesis_path = NULL, date_stamp = NULL))
  }
  synthesis_files <- list.files(
    output_base,
    pattern = "^01_thresholds_synthesis.*\\.rds$",
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(synthesis_files) == 0L) {
    return(list(dir = NULL, synthesis_path = NULL, date_stamp = NULL))
  }
  synthesis_files <- sort(synthesis_files, decreasing = TRUE)
  latest_file <- synthesis_files[[1]]
  
  stamp <- extract_evd_date_stamp(basename(latest_file))
  if (is.null(stamp)) {
    tryCatch({
      obj <- readRDS(latest_file)
      stamp <- attr(obj, "evd_file_date")
    }, error = function(e) NULL)
  }
  
  list(dir = dirname(latest_file), synthesis_path = latest_file, date_stamp = stamp)
}

#' Check timestamps and conditionally run R/01_alert_thresholds.R (and trends)
#' @param output_base Path to output directory.
#' @param root_dir Path to Alerts repository root.
#' @param data_folder Path to cleaned EVD line list directory.
#' @param force_rerun Logical. If TRUE, forces pipeline re-run regardless of stamps.
#' @return Logical indicating whether the pipeline was executed.
sync_alert_data <- function(output_base, root_dir, data_folder, force_rerun = FALSE) {
  evd_info <- get_latest_evd_info(data_folder)
  exp_info <- get_latest_exported_info(output_base)
  
  evd_stamp <- evd_info$date_stamp
  exp_stamp <- exp_info$date_stamp
  
  needs_run <- FALSE
  if (force_rerun) {
    needs_run <- TRUE
    message("Force rerun requested.")
  } else if (is.null(exp_info$synthesis_path)) {
    needs_run <- TRUE
    message("No exported analysis found. Running alert pipeline...")
  } else if (!is.null(evd_stamp) && !is.null(exp_stamp) && !identical(evd_stamp, exp_stamp)) {
    needs_run <- TRUE
    message(sprintf(
      "New EVD snapshot detected (%s vs exported %s). Running alert pipeline...",
      evd_stamp, exp_stamp
    ))
  } else if (!is.null(evd_stamp) && is.null(exp_stamp)) {
    needs_run <- TRUE
    message(sprintf("Running alert pipeline for EVD snapshot %s...", evd_stamp))
  } else {
    message(sprintf(
      "Alert analysis is up-to-date with latest EVD snapshot [%s]. Skipping R/01_alert_thresholds.R.",
      dplyr::coalesce(exp_stamp, "current")
    ))
  }
  
  if (needs_run) {
    script_01 <- file.path(root_dir, "R", "01_alert_thresholds.R")
    script_02 <- file.path(root_dir, "R", "02_alert_trends.R")
    
    if (file.exists(script_01)) {
      message("Running R/01_alert_thresholds.R to update thresholds...")
      res_01 <- system2("Rscript", args = c(shQuote(script_01)), stdout = TRUE, stderr = TRUE)
      status_01 <- attr(res_01, "status")
      if (!is.null(status_01) && status_01 != 0) {
        warning("R/01_alert_thresholds.R exited with code ", status_01, ":\n", paste(res_01, collapse = "\n"))
      }
    }
    
    if (file.exists(script_02)) {
      message("Running R/02_alert_trends.R to update trends...")
      res_02 <- system2("Rscript", args = c(shQuote(script_02)), stdout = TRUE, stderr = TRUE)
      status_02 <- attr(res_02, "status")
      if (!is.null(status_02) && status_02 != 0) {
        warning("R/02_alert_trends.R exited with code ", status_02, ":\n", paste(res_02, collapse = "\n"))
      }
    }
  }
  
  invisible(needs_run)
}

# --- Load latest output directory --------------------------------------------
load_latest_data <- function(output_base) {
  output_dirs <- list.dirs(output_base, recursive = FALSE, full.names = TRUE)
  if (length(output_dirs) == 0) {
    stop("No output directories found in: ", output_base)
  }

  # Filter to directories that actually contain the required analysis files
  valid_dirs <- output_dirs[
    (lengths(lapply(output_dirs, function(d) list.files(d, pattern = "^01_thresholds_synthesis.*\\.rds$"))) > 0) &
    (lengths(lapply(output_dirs, function(d) list.files(d, pattern = "^02_trends_smooth.*\\.rds$"))) > 0)
  ]

  if (length(valid_dirs) == 0) {
    valid_dirs <- output_dirs[
      lengths(lapply(output_dirs, function(d) list.files(d, pattern = "^01_thresholds_synthesis.*\\.rds$"))) > 0
    ]
  }

  if (length(valid_dirs) == 0) {
    stop("No valid output directories with threshold/trend files found in: ", output_base)
  }

  latest_dir <- sort(valid_dirs, decreasing = TRUE)[[1]]
  message("Loading data from: ", latest_dir)

  # Threshold synthesis (longitudinal per-HZ per-week)
  synthesis_files <- sort(
    list.files(latest_dir, pattern = "^01_thresholds_synthesis.*\\.rds$", full.names = TRUE),
    decreasing = TRUE
  )
  synthesis_path <- synthesis_files[[1]]
  synthesis <- readRDS(synthesis_path) |>
    tibble::as_tibble()

  if ("week_start" %in% names(synthesis)) {
    synthesis <- synthesis |>
      mutate(week_start = as.Date(.data$week_start))
  }

  # Round numeric columns for display
  synthesis <- synthesis |>
    mutate(across(
      where(is.numeric),
      \(x) if (all(is.na(x))) x else round(x, 3)
    ))

  # Intermediate parameters (list of epidemiological estimates)
  param_files <- sort(
    list.files(latest_dir, pattern = "^01_intermediate_parameters.*\\.rds$", full.names = TRUE),
    decreasing = TRUE
  )
  intermediate_params <- if (length(param_files) > 0) {
    readRDS(param_files[[1]])
  } else {
    list()
  }

  # Trends smooth adeq (alert counts joined with thresholds, adequacy indices)
  trend_files <- sort(
    list.files(latest_dir, pattern = "^02_trends_smooth_adeq.*\\.rds$", full.names = TRUE),
    decreasing = TRUE
  )
  if (length(trend_files) == 0) {
    trend_files <- sort(
      list.files(latest_dir, pattern = "^02_trends_smooth.*\\.rds$", full.names = TRUE),
      decreasing = TRUE
    )
  }
  trends_smooth <- readRDS(trend_files[[1]]) |>
    tibble::as_tibble()

  if ("week_start" %in% names(trends_smooth)) {
    trends_smooth <- trends_smooth |>
      mutate(week_start = as.Date(.data$week_start))
  } else if ("threshold_time_key" %in% names(trends_smooth)) {
    trends_smooth <- trends_smooth |>
      mutate(week_start = as.Date(.data$threshold_time_key))
  }

  trends_smooth_adeq <- trends_smooth

  # Recent adequacy (per-HZ summary)
  adequacy_files <- sort(
    list.files(latest_dir, pattern = "^02_recent_adequacy.*\\.xlsx$", full.names = TRUE),
    decreasing = TRUE
  )
  recent_adequacy <- if (length(adequacy_files) > 0) {
    readxl::read_excel(adequacy_files[[1]]) |>
      tibble::as_tibble()
  } else {
    tibble::tibble()
  }

  list(
    synthesis = synthesis,
    intermediate_params = intermediate_params,
    trends_smooth = trends_smooth,
    trends_smooth_adeq = trends_smooth_adeq,
    recent_adequacy = recent_adequacy,
    data_dir = latest_dir
  )
}

# --- Synchronize data if EVD line list is updated, then load ---
sync_alert_data(output_base, root_dir, data_folder)

data <- load_latest_data(output_base)
synthesis <- data$synthesis
intermediate_params <- data$intermediate_params
trends_smooth <- data$trends_smooth
trends_smooth_adeq <- data$trends_smooth_adeq
recent_adequacy <- data$recent_adequacy

# --- Load map shapefiles -----------------------------------------------------
load_hz_maps <- function(map_dir) {
  if (!dir.exists(map_dir)) {
    warning("Map directory not found: ", map_dir, ". Map tab will be disabled.")
    return(NULL)
  }
  gpkg_files <- list.files(map_dir, pattern = "[.]gpkg$", full.names = TRUE)
  if (length(gpkg_files) == 0) {
    warning("No GPKG files found in: ", map_dir)
    return(NULL)
  }

  maps <- gpkg_files |>
    map(\(f) {
      tryCatch(
        sf::read_sf(f),
        error = function(e) {
          warning("Failed to read: ", f, " — ", conditionMessage(e))
          NULL
        }
      )
    }) |>
    keep(\(x) !is.null(x))

  if (length(maps) == 0) return(NULL)

  # Combine all province shapefiles into one sf object
  combined <- do.call(rbind, maps)

  # Normalize column names: GPKG files use 'zonesante' and 'province'
  name_map <- c(
    "zonesante"              = "zone_sante_notification",
    "ZONE_SANTE"             = "zone_sante_notification",
    "zone_sante_notification" = "zone_sante_notification",
    "province"               = "Province",
    "PROVINCE"               = "Province"
  )

  for (old_name in names(name_map)) {
    new_name <- name_map[[old_name]]
    if (old_name %in% names(combined) && !(new_name %in% names(combined))) {
      combined <- combined |>
        rename(!!new_name := !!old_name)
    }
  }

  # Apply title case to health zone names
  if ("zone_sante_notification" %in% names(combined)) {
    combined <- combined |>
      mutate(zone_sante_notification = str_to_title(.data$zone_sante_notification))
  } else {
    warning("No health zone name column found in shapefiles. Map will not join with data.")
  }

  combined
}

hz_maps <- load_hz_maps(map_dir)

# --- Prepare HZ metadata -----------------------------------------------------
# Combine HZ-level info with latest adequacy for the overview and map tabs
hz_metadata <- trends_smooth |>
  distinct(.data$zone_sante_notification, .data$Province, .data$Population) |>
  left_join(
    recent_adequacy |>
      select(
        "zone_sante_notification", "mean_aai",
        "adequacy_category", "trend_direction"
      ),
    by = "zone_sante_notification"
  ) |>
  mutate(
    zone_sante_notification = str_to_title(.data$zone_sante_notification)
  )

# --- Shared constants --------------------------------------------------------
all_hz <- sort(unique(trends_smooth$zone_sante_notification))
all_provinces <- sort(unique(trends_smooth$Province))
date_range_full <- range(trends_smooth$week_start, na.rm = TRUE)
all_weeks <- sort(unique(trends_smooth$week_start))
all_time_windows <- sort(unique(trends_smooth_adeq$threshold_time_key))
time_window_choices <- stats::setNames(
  all_time_windows,
  format(as.Date(all_time_windows), "%d %b %Y")
)

# Adequacy color palette
adequacy_colors <- c(
  "Under-alerting" = "#DC3545",
  "Adequate"       = "#198754",
  "Over-alerting"  = "#FFC107"
)

# Trend direction icons
trend_icons <- c(
  "Increasing" = "arrow-up",
  "Decreasing" = "arrow-down",
  "Stable"     = "minus",
  "Unknown"    = "question"
)

# Approach colors for threshold comparison
approach_colors <- c(
  "Approach A (CMR)"      = "#0D6EFD",
  "Approach B (Beni)"     = "#FD7E14",
  "Approach C (Case-derived)" = "#198754",
  "Consensus"             = "#6C757D"
)

# --- Health zones without aggregate ensemble ---------------------------------
all_hz_individual <- sort(setdiff(unique(trends_smooth_adeq$zone_sante_notification), "Ensemble de la zone affectée"))

# --- Helper: extract intermediate parameter tables ---------------------------
get_cfr_table <- function() {
  intermediate_params$cfr |>
    tibble::as_tibble() |>
    mutate(zone_sante_notification = str_to_title(.data$zone_sante_notification))
}

get_detection_table <- function() {
  intermediate_params$detection_rates |>
    tibble::as_tibble() |>
    mutate(zone_sante_notification = str_to_title(.data$zone_sante_notification))
}

get_rt_sar_table <- function() {
  intermediate_params$rt_sar |>
    tibble::as_tibble() |>
    mutate(zone_sante_notification = str_to_title(.data$zone_sante_notification))
}

get_multipliers_table <- function() {
  intermediate_params$multipliers |>
    tibble::as_tibble() |>
    mutate(zone_sante_notification = str_to_title(.data$zone_sante_notification))
}

# --- Helper: build Table 1 for Ensemble ---------------------------------------
get_table1_ensemble_df <- function(trends_smooth_adeq) {
  trends_smooth_adeq |>
    dplyr::filter(zone_sante_notification == "Ensemble de la zone affectée") |>
    dplyr::select(
      threshold_time_key,
      case_alerts, death_alerts, total_alerts,
      Alert_case_threshold, Alert_death_threshold,
      case_adequacy, death_adequacy, aai
    ) |>
    dplyr::mutate(
      case_adequacy = round(case_adequacy, 2),
      death_adequacy = round(death_adequacy, 2),
      aai = round(aai, 2)
    ) |>
    dplyr::arrange(dplyr::desc(threshold_time_key))
}

# --- Helper: build Table 2 for Ensemble ---------------------------------------
get_table2_ensemble_df <- function(intermediate_params) {
  ensemble_cdp <- intermediate_params$case_derived_params |>
    dplyr::filter(zone_sante_notification == "Ensemble de la zone affectée") |>
    dplyr::select(
      threshold_time_key, beta_c, beta_d, estimated_true_cases_recent
    )

  recent_ensemble <- intermediate_params$recent_cases |>
    dplyr::mutate(
      n_recent_confirmed_nowcast = dplyr::coalesce(
        n_recent_confirmed_nowcast,
        n_recent_confirmed
      )
    ) |>
    dplyr::summarise(
      n_recent_confirmed = sum(n_recent_confirmed, na.rm = TRUE),
      n_recent_confirmed_nowcast = sum(
        n_recent_confirmed_nowcast,
        na.rm = TRUE
      ),
      .by = threshold_time_key
    )

  ensemble_cdp |>
    dplyr::left_join(recent_ensemble, by = "threshold_time_key") |>
    dplyr::mutate(
      detection_rate_adj = dplyr::if_else(
        estimated_true_cases_recent > 0,
        n_recent_confirmed_nowcast / estimated_true_cases_recent,
        NA_real_
      ),
      detection_rate_adj = dplyr::if_else(
        is.finite(detection_rate_adj),
        detection_rate_adj,
        NA_real_
      )
    ) |>
    dplyr::mutate(
      beta_c = round(beta_c, 3),
      beta_d = round(beta_d, 3),
      detection_rate_adj = round(detection_rate_adj, 3),
      estimated_true_cases_recent = round(estimated_true_cases_recent, 1)
    ) |>
    dplyr::arrange(dplyr::desc(threshold_time_key)) |>
    dplyr::select(
      threshold_time_key,
      beta_c, beta_d,
      n_recent_confirmed, n_recent_confirmed_nowcast,
      detection_rate_adj, estimated_true_cases_recent
    )
}

# --- Helper: build Table 1 for Health Zones (by window) -----------------------
get_table1_hz_df <- function(trends_smooth_adeq, intermediate_params = NULL, time_key = NULL) {
  if (is.null(time_key) || length(time_key) == 0) {
    time_key <- max(trends_smooth_adeq$threshold_time_key, na.rm = TRUE)
  }

  t1 <- trends_smooth_adeq |>
    dplyr::filter(
      threshold_time_key %in% time_key,
      zone_sante_notification != "Ensemble de la zone affectée"
    ) |>
    dplyr::select(
      zone_sante_notification, Province,
      case_alerts, death_alerts, total_alerts,
      Alert_case_threshold, Alert_death_threshold,
      case_adequacy, death_adequacy, aai
    ) |>
    dplyr::mutate(
      case_adequacy = round(case_adequacy, 2),
      death_adequacy = round(death_adequacy, 2),
      aai = round(aai, 2)
    )

  if (is.null(intermediate_params) && exists("intermediate_params", envir = .GlobalEnv)) {
    intermediate_params <- get("intermediate_params", envir = .GlobalEnv)
  }

  if (!is.null(intermediate_params) && "recent_cases" %in% names(intermediate_params)) {
    recent_active_hz <- intermediate_params$recent_cases |>
      dplyr::filter(
        threshold_time_key %in% time_key,
        zone_sante_notification != "Ensemble de la zone affectée"
      ) |>
      dplyr::mutate(
        n_recent_confirmed_nowcast = dplyr::coalesce(
          n_recent_confirmed_nowcast,
          n_recent_confirmed
        )
      ) |>
      dplyr::filter(n_recent_confirmed_nowcast > 0) |>
      dplyr::pull(zone_sante_notification)

    t1 <- t1 |>
      dplyr::filter(zone_sante_notification %in% recent_active_hz)
  }

  # Order by AAI ascending
  t1 |>
    dplyr::arrange(aai, dplyr::desc(total_alerts))
}

# --- Helper: build Table 2 for Health Zones (by window) -----------------------
get_table2_hz_df <- function(intermediate_params, trends_smooth_adeq, time_key = NULL) {
  if (is.null(time_key) || length(time_key) == 0) {
    time_key <- max(trends_smooth_adeq$threshold_time_key, na.rm = TRUE)
  }

  t1_df <- get_table1_hz_df(trends_smooth_adeq, intermediate_params, time_key = time_key)
  trends_hz_set <- t1_df$zone_sante_notification

  beta_tab <- intermediate_params$multipliers |>
    dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
    dplyr::select(
      zone_sante_notification, threshold_time_key,
      beta_c, beta_d
    )
  recent_cases_tab <- intermediate_params$recent_cases |>
    dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
    dplyr::select(
      zone_sante_notification, threshold_time_key,
      n_recent_confirmed, n_recent_confirmed_nowcast
    )
  detection_tab <- combine_detection_estimates_by_hz(
    intermediate_params$detection_backcalc,
    intermediate_params$detection_rates,
    recent_cases = intermediate_params$recent_cases
  ) |>
    dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
    dplyr::select(
      zone_sante_notification, threshold_time_key, detection_rate_adj
    )
  province_tab <- trends_smooth_adeq |>
    dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
    dplyr::select(zone_sante_notification, Province) |>
    dplyr::distinct()

  table2_df <- beta_tab |>
    dplyr::left_join(
      recent_cases_tab,
      by = c("zone_sante_notification", "threshold_time_key")
    ) |>
    dplyr::left_join(detection_tab, by = c("zone_sante_notification", "threshold_time_key")) |>
    dplyr::left_join(province_tab, by = "zone_sante_notification") |>
    dplyr::filter(threshold_time_key %in% time_key) |>
    dplyr::filter(zone_sante_notification %in% trends_hz_set) |>
    dplyr::select(
      zone_sante_notification, Province,
      beta_c, beta_d, n_recent_confirmed, n_recent_confirmed_nowcast,
      detection_rate_adj
    ) |>
    dplyr::mutate(
      n_recent_confirmed_nowcast = dplyr::coalesce(
        n_recent_confirmed_nowcast,
        n_recent_confirmed
      ),
      estimated_true_cases_recent = dplyr::if_else(
        n_recent_confirmed_nowcast == 0,
        0,
        n_recent_confirmed_nowcast / detection_rate_adj
      ),
      estimated_true_cases_recent = dplyr::if_else(
        is.finite(estimated_true_cases_recent),
        estimated_true_cases_recent,
        NA_real_
      )
    ) |>
    dplyr::mutate(
      beta_c = round(beta_c, 3),
      beta_d = round(beta_d, 3),
      detection_rate_adj = round(detection_rate_adj, 3),
      estimated_true_cases_recent = round(estimated_true_cases_recent, 1)
    ) |>
    dplyr::filter(n_recent_confirmed_nowcast > 0)

  # Match row order of Table 1
  hz_order <- t1_df$zone_sante_notification

  table2_df |>
    dplyr::slice(match(hz_order, zone_sante_notification))
}

message("Global data loaded successfully. ",
        length(all_hz), " health zones, ",
        length(all_weeks), " weeks (",
        as.character(date_range_full[1]), " to ",
        as.character(date_range_full[2]), ")")

# Flag to prevent re-sourcing when ui.R and server.R both load global.R
.bvd_global_loaded <- TRUE
