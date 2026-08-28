# Script 2b: Alert Trends Plots (standalone)
# Purpose: render case alert and death alert trend plots with threshold
#   boundaries from the 02_trends_smooth_adeq.rds data produced by 02_alert_trends.R.
#
# This script is independent of 02_alert_trends.R - it only consumes the saved
# RDS, so it can be re-run whenever thresholds or styling change without
# recomputing the trend analysis.
#
# Outputs (written to output/<date>/):
#   - alert_case_trends_thresholds.pdf   : all HZs, faceted (case alerts overview)
#   - alert_case_trends_thresholds.html  : interactive plotly version (case overview)
#   - alert_death_trends_thresholds.pdf  : all HZs, faceted (death alerts overview)
#   - alert_death_trends_thresholds.html : interactive plotly version (death overview)
#   - per_hz/alert_case_trends_{hz}.pdf  : one static chart per HZ (case alerts)
#   - per_hz/alert_case_trends_{hz}.html : one interactive chart per HZ (case alerts)
#   - per_hz/alert_death_trends_{hz}.pdf : one static chart per HZ (death alerts)
#   - per_hz/alert_death_trends_{hz}.html: one interactive chart per HZ (death alerts)
#   - adequacy_stacked.pdf               : stacked case/death adequacy overview
#   - adequacy_stacked.html              : interactive stacked adequacy overview
#   - per_hz/adequacy_stacked_{hz}.pdf   : one stacked adequacy chart per HZ
#   - per_hz/adequacy_stacked_{hz}.html  : one interactive adequacy chart per HZ
#   - alert_trends_tables_last_window.html : kableExtra summary tables (last window)
#   - alert_trends_tables_last_window.xlsx : same tables as an Excel workbook
#   - alert_trends_tables_ensemble_time.html : ensemble time-series tables (kableExtra)
#   - alert_trends_tables_ensemble_time.xlsx : same tables as an Excel workbook
#
# Usage:
#   Rscript R/02b_alert_trends_plots.R
#   Run from an interactive R session and adjust the `hz` vector below to
#   restrict the per-HZ export to a subset of health zones.
#
# Conventions: native pipe `|>`, here::here() paths, .by grouping.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# Load helpers + plotting functions
source(here::here("R/alert_helpers.R"))
source(here::here("R/alert_plots.R"))

# ---------------------------------------------------------------- #
# 1. Configuration
# ---------------------------------------------------------------- #

# Paths
output_dir <- create_output_dir(here::here("output"))
adeq_path <- latest_output_file(here::here("output"), "02_trends_smooth_adeq\\.rds")

if (is.na(adeq_path)) {
  rlang::abort(
    "No '02_trends_smooth_adeq.rds' found under output/. ",
    "Run R/02_alert_trends.R first."
  )
}
adeq_output_dir <- dirname(adeq_path)

# Health zones to render as individual plots.
# - NULL  => defaults to top 6 active health zones (or all if specified)
# - vector => render only those HZs (recommended for targeted inspection)
# Override at run time via the HZ_TARGET env var (comma-separated), e.g.:
#   HZ_TARGET="Bunia,Beni,Butembo" Rscript R/02b_alert_trends_plots.R
hz_env_target <- NULL
if (!is.null(hz_env <- Sys.getenv("HZ_TARGET", ""))) {
  hz_env_target <- if (nzchar(hz_env)) trimws(strsplit(hz_env, ",", fixed = TRUE)[[1L]]) else NULL
}

# Static-plot dimensions (inches) used for the faceted overview and per-HZ PDFs.
overview_width  <- 14
overview_height <- 10
per_hz_width    <- 9
per_hz_height   <- 6

# Whether to colour points by adequacy_category (Under/Adequate/Over-alerting).
colour_by_adequacy <- TRUE

# Whether to render the stacked case/death adequacy bar chart (section 5).
show_adequacy_stacked <- TRUE

# Whether to render the last-window kableExtra summary tables (section 6).
show_trends_tables <- TRUE

# Whether to render the ensemble time-series kableExtra tables (section 6f).
show_ensemble_tables <- TRUE

# ---------------------------------------------------------------- #
# 2. Load data
# ---------------------------------------------------------------- #
message("Loading trend data from ", adeq_path)
trends_smooth_adeq <- readRDS(adeq_path)

trends_smooth_adeq |> names()

if (!is.data.frame(trends_smooth_adeq)) {
  rlang::abort("Loaded object is not a dataframe: ", adeq_path)
}

if (interactive()) {
  print(names(trends_smooth_adeq))
  trends_smooth_adeq |>
    dplyr::filter(zone_sante_notification == "Bunia") |>
    utils::head() |>
    print()
}

# Identify top health zones by alert activity (excluding aggregate ensemble)
top_zs <- trends_smooth_adeq |>
  dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
  dplyr::summarise(
    case_alerts = sum(case_alerts, na.rm = TRUE),
    death_alerts = sum(death_alerts, na.rm = TRUE),
    .by = zone_sante_notification
  ) |>
  dplyr::arrange(dplyr::desc(case_alerts)) |>
  head(6) |>
  dplyr::pull(zone_sante_notification)

# Resolve target HZs for per-HZ export
target_zones <- if (!is.null(hz_env_target)) hz_env_target else top_zs
message("Target health zones for per-HZ export (", length(target_zones), "): ",
        paste(target_zones, collapse = ", "))

# HZ-only data for faceted overview plots
trends_hz_only <- trends_smooth_adeq |>
  dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée")

# ---------------------------------------------------------------- #
# 3. Overview plots (all HZs, faceted) - Case & Death Alerts
# ---------------------------------------------------------------- #

top_hzs <- trends_hz_only |> 
  filter(threshold_time_key >= nth(threshold_time_key, -3)) |>
  summarise(total_alerts =  sum(total_alerts), .by = zone_sante_notification) |>
  arrange(desc(total_alerts)) |>
  top_n(6, total_alerts) |> pull(zone_sante_notification)

# --- 3a. Case Alert Overview (faceted across HZs) ----------------
message("Rendering case alerts overview (faceted) plot...")
p_case_overview <- plot_alert_trends(
  data = trends_hz_only[trends_hz_only$zone_sante_notification %in% top_hzs,],
  metric = "case",
  colour_by_adequacy = colour_by_adequacy
)
save_alert_trend_plot(
  p_case_overview,
  "alert_case_trends_thresholds.pdf",
  output_dir = adeq_output_dir,
  width = overview_width, height = overview_height
)
# Backward-compatibility alias
save_alert_trend_plot(
  p_case_overview,
  "alert_trends_thresholds.pdf",
  output_dir = adeq_output_dir,
  width = overview_width, height = overview_height
)
message("  -> ", file.path(adeq_output_dir, "alert_case_trends_thresholds.pdf"))

if (requireNamespace("plotly", quietly = TRUE)) {
  message("Rendering interactive case alerts overview...")
  ip_case_overview <- plot_alert_trends_interactive(
    data = trends_hz_only[trends_hz_only$zone_sante_notification %in% top_hzs,],
    metric = "case",
    colour_by_adequacy = colour_by_adequacy
  )
  save_alert_trend_plot(
    ip_case_overview,
    "alert_case_trends_thresholds.html",
    output_dir = adeq_output_dir
  )
  save_alert_trend_plot(
    ip_case_overview,
    "alert_trends_thresholds.html",
    output_dir = adeq_output_dir
  )
  message("  -> ", file.path(adeq_output_dir, "alert_case_trends_thresholds.html"))
} else {
  message("Package 'plotly' not available - skipping interactive case overview.")
}

# --- 3b. Death Alert Overview (faceted across HZs) ---------------
message("Rendering death alerts overview (faceted) plot...")
p_death_overview <- plot_alert_trends(
  data = trends_hz_only[trends_hz_only$zone_sante_notification %in% top_hzs,],
  metric = "death",
  colour_by_adequacy = colour_by_adequacy
)
save_alert_trend_plot(
  p_death_overview,
  "alert_death_trends_thresholds.pdf",
  output_dir = adeq_output_dir,
  width = overview_width, height = overview_height
)
message("  -> ", file.path(adeq_output_dir, "alert_death_trends_thresholds.pdf"))

if (requireNamespace("plotly", quietly = TRUE)) {
  message("Rendering interactive death alerts overview...")
  ip_death_overview <- plot_alert_trends_interactive(
    data = trends_hz_only,
    metric = "death",
    colour_by_adequacy = colour_by_adequacy
  )
  save_alert_trend_plot(
    ip_death_overview,
    "alert_death_trends_thresholds.html",
    output_dir = adeq_output_dir
  )
  message("  -> ", file.path(adeq_output_dir, "alert_death_trends_thresholds.html"))
} else {
  message("Package 'plotly' not available - skipping interactive death overview.")
}

# --- 3c. Whole Affected Area Plots (Ensemble de la zone affectée) -
has_ensemble <- any(trends_smooth_adeq$zone_sante_notification == "Ensemble de la zone affectée")

if (has_ensemble) {
  message("Rendering Whole Affected Area (Ensemble) trend plots...")

  # Case alerts: Ensemble
  p_case_ensemble <- plot_alert_trends(
    data = trends_smooth_adeq,
    approach = c("C"),
    hz = "Ensemble de la zone affectée",
    metric = "case",
    colour_by_adequacy = colour_by_adequacy
  )
  save_alert_trend_plot(
    p_case_ensemble,
    "alert_case_trends_ensemble.pdf",
    output_dir = adeq_output_dir,
    width = per_hz_width, height = per_hz_height
  )
  message("  -> ", file.path(adeq_output_dir, "alert_case_trends_ensemble.pdf"))

  if (requireNamespace("plotly", quietly = TRUE)) {
    ip_case_ensemble <- plot_alert_trends_interactive(
      data = trends_smooth_adeq,
      hz = "Ensemble de la zone affectée",
      metric = "case",
      colour_by_adequacy = colour_by_adequacy
    )
    save_alert_trend_plot(
      ip_case_ensemble,
      "alert_case_trends_ensemble.html",
      output_dir = adeq_output_dir
    )
    message("  -> ", file.path(adeq_output_dir, "alert_case_trends_ensemble.html"))
  }

  if (interactive()) {
    trends_smooth_adeq |> View()
  }

  # Death alerts: Ensemble
  p_death_ensemble <- plot_alert_trends(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "death",
    colour_by_adequacy = colour_by_adequacy
  )
  save_alert_trend_plot(
    p_death_ensemble,
    "alert_death_trends_ensemble.pdf",
    output_dir = adeq_output_dir,
    width = per_hz_width, height = per_hz_height
  )
  message("  -> ", file.path(adeq_output_dir, "alert_death_trends_ensemble.pdf"))

  if (requireNamespace("plotly", quietly = TRUE)) {
    ip_death_ensemble <- plot_alert_trends_interactive(
      data = trends_smooth_adeq,
      hz = "Ensemble de la zone affectée",
      metric = "death",
      colour_by_adequacy = colour_by_adequacy
    )
    save_alert_trend_plot(
      ip_death_ensemble,
      "alert_death_trends_ensemble.html",
      output_dir = adeq_output_dir
    )
    message("  -> ", file.path(adeq_output_dir, "alert_death_trends_ensemble.html"))
  }

  # Stacked adequacy: Ensemble
  if (show_adequacy_stacked) {
    p_adeq_ensemble <- plot_adequacy_stacked(
      data = trends_smooth_adeq,
      hz = "Ensemble de la zone affectée"
    )
    save_alert_trend_plot(
      p_adeq_ensemble,
      "adequacy_stacked_ensemble.pdf",
      output_dir = adeq_output_dir,
      width = per_hz_width, height = per_hz_height
    )
    message("  -> ", file.path(adeq_output_dir, "adequacy_stacked_ensemble.pdf"))

    if (requireNamespace("plotly", quietly = TRUE)) {
      ip_adeq_ensemble <- plot_adequacy_stacked_interactive(
        data = trends_smooth_adeq,
        hz = "Ensemble de la zone affectée"
      )
      save_alert_trend_plot(
        ip_adeq_ensemble,
        "adequacy_stacked_ensemble.html",
        output_dir = adeq_output_dir
      )
      message("  -> ", file.path(adeq_output_dir, "adequacy_stacked_ensemble.html"))
    }
  }
}

# ---------------------------------------------------------------- #
# 4. Per-HZ plots - Case & Death Alerts per Health Zone
# ---------------------------------------------------------------- #
per_hz_dir <- file.path(adeq_output_dir, "per_hz")
if (!dir.exists(per_hz_dir)) dir.create(per_hz_dir, recursive = TRUE)

message("Rendering per-HZ plots for ", length(target_zones), " health zone(s)...")

# --- 4a. Case Alert Per-HZ Plots ---------------------------------
message("Rendering per-HZ case alert plots...")
per_hz_case_static <- plot_alert_trends(
  data = trends_smooth_adeq,
  hz = target_zones,
  metric = "case",
  colour_by_adequacy = colour_by_adequacy,
  one_per_hz = TRUE
)
case_static_paths <- save_alert_trend_plot(
  per_hz_case_static,
  "alert_case_trends_{hz}.pdf",
  output_dir = per_hz_dir,
  width = per_hz_width, height = per_hz_height
)
# Backward-compatibility alias
save_alert_trend_plot(
  per_hz_case_static,
  "alert_trends_{hz}.pdf",
  output_dir = per_hz_dir,
  width = per_hz_width, height = per_hz_height
)
message("  -> ", length(case_static_paths), " static case PDFs in ", per_hz_dir)

if (requireNamespace("plotly", quietly = TRUE)) {
  message("Rendering interactive per-HZ case alert plots...")
  per_hz_case_interactive <- plot_alert_trends_interactive(
    data = trends_smooth_adeq,
    hz = target_zones,
    metric = "case",
    colour_by_adequacy = colour_by_adequacy,
    one_per_hz = TRUE
  )
  case_interactive_paths <- save_alert_trend_plot(
    per_hz_case_interactive,
    "alert_case_trends_{hz}.html",
    output_dir = per_hz_dir
  )
  save_alert_trend_plot(
    per_hz_case_interactive,
    "alert_trends_{hz}.html",
    output_dir = per_hz_dir
  )
  message("  -> ", length(case_interactive_paths), " interactive case HTMLs in ", per_hz_dir)
} else {
  message("Package 'plotly' not available - skipping interactive per-HZ case plots.")
}

# --- 4b. Death Alert Per-HZ Plots --------------------------------
message("Rendering per-HZ death alert plots...")
per_hz_death_static <- plot_alert_trends(
  data = trends_smooth_adeq,
  hz = target_zones,
  metric = "death",
  colour_by_adequacy = colour_by_adequacy,
  one_per_hz = TRUE
)
death_static_paths <- save_alert_trend_plot(
  per_hz_death_static,
  "alert_death_trends_{hz}.pdf",
  output_dir = per_hz_dir,
  width = per_hz_width, height = per_hz_height
)
message("  -> ", length(death_static_paths), " static death PDFs in ", per_hz_dir)

if (requireNamespace("plotly", quietly = TRUE)) {
  message("Rendering interactive per-HZ death alert plots...")
  per_hz_death_interactive <- plot_alert_trends_interactive(
    data = trends_smooth_adeq,
    hz = target_zones,
    metric = "death",
    colour_by_adequacy = colour_by_adequacy,
    one_per_hz = TRUE
  )
  death_interactive_paths <- save_alert_trend_plot(
    per_hz_death_interactive,
    "alert_death_trends_{hz}.html",
    output_dir = per_hz_dir
  )
  message("  -> ", length(death_interactive_paths), " interactive death HTMLs in ", per_hz_dir)
} else {
  message("Package 'plotly' not available - skipping interactive per-HZ death plots.")
}

# ---------------------------------------------------------------- #
# 5. Stacked adequacy plot (case + death adequacy bars, hline at y=0.75)
# ---------------------------------------------------------------- #
if (show_adequacy_stacked) {
  message("Rendering stacked adequacy chart (overview)...")
  p_adeq_overview <- plot_adequacy_stacked(
    data = trends_smooth_adeq,
    hz = target_zones
  )
  save_alert_trend_plot(
    p_adeq_overview,
    "adequacy_stacked.pdf",
    output_dir = adeq_output_dir,
    width = overview_width, height = overview_height
  )
  message("  -> ", file.path(adeq_output_dir, "adequacy_stacked.pdf"))

  # Per-HZ static PDFs
  message("Rendering per-HZ stacked adequacy charts...")
  p_adeq_hz <- plot_adequacy_stacked(
    data = trends_smooth_adeq,
    hz = target_zones,
    one_per_hz = TRUE
  )
  adeq_paths <- save_alert_trend_plot(
    p_adeq_hz,
    "adequacy_stacked_{hz}.pdf",
    output_dir = per_hz_dir,
    width = per_hz_width, height = per_hz_height
  )
  message("  -> ", length(adeq_paths), " static PDFs in ", per_hz_dir)

  # Interactive
  if (requireNamespace("plotly", quietly = TRUE)) {
    message("Rendering interactive stacked adequacy chart (overview)...")
    ip_adeq_overview <- plot_adequacy_stacked_interactive(
      data = trends_smooth_adeq,
      hz = target_zones
    )
    save_alert_trend_plot(
      ip_adeq_overview,
      "adequacy_stacked.html",
      output_dir = adeq_output_dir
    )
    message("  -> ", file.path(adeq_output_dir, "adequacy_stacked.html"))

    message("Rendering per-HZ interactive stacked adequacy charts...")
    ip_adeq_hz <- plot_adequacy_stacked_interactive(
      data = trends_smooth_adeq,
      hz = target_zones,
      one_per_hz = TRUE
    )
    ip_adeq_paths <- save_alert_trend_plot(
      ip_adeq_hz,
      "adequacy_stacked_{hz}.html",
      output_dir = per_hz_dir
    )
    message("  -> ", length(ip_adeq_paths), " interactive HTMLs in ", per_hz_dir)
  }
}

# ---------------------------------------------------------------- #
# 6. Summary tables (last time window) - kableExtra HTML + XLSX
# ---------------------------------------------------------------- #
if (show_trends_tables) {
  last_key <- max(trends_smooth_adeq$threshold_time_key, na.rm = TRUE)
  window_label <- format(as.Date(last_key), "%d %b %Y")

  message("Rendering summary tables for last time window (", window_label, ")...")

  # --- 6a. Table 1: alert counts, thresholds and adequacy ---------
  table1_df <- trends_smooth_adeq |>
    dplyr::filter(threshold_time_key == last_key) |>
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

  # --- 6b. Table 2: beta multipliers, recent cases, detection ----
  intermediate_path <- file.path(adeq_output_dir, "01_intermediate_parameters.rds")
  if (!file.exists(intermediate_path)) {
    intermediate_path <- latest_output_file(
      here::here("output"), "01_intermediate_parameters\\.rds"
    )
  }
  if (is.na(intermediate_path)) {
    rlang::abort(
      "No '01_intermediate_parameters.rds' found under output/. ",
      "Run R/01_alert_thresholds.R first."
    )
  }
  intermediate_params <- readRDS(intermediate_path)

  beta_tab <- intermediate_params$multipliers |>
    dplyr::select(
      zone_sante_notification, threshold_time_key,
      beta_c, beta_d
    )
  recent_cases_tab <- intermediate_params$recent_cases |>
    dplyr::select(
      zone_sante_notification, threshold_time_key,
      n_recent_confirmed, n_recent_confirmed_nowcast
    )
  detection_tab <- combine_detection_estimates_by_hz(
    intermediate_params$detection_backcalc,
    intermediate_params$detection_rates,
    recent_cases = intermediate_params$recent_cases
  ) |>
    dplyr::select(
      zone_sante_notification, threshold_time_key, detection_rate_adj
    )
  province_tab <- trends_smooth_adeq |>
    dplyr::select(zone_sante_notification, Province) |>
    dplyr::distinct()

  # Restrict to the HZ set analysed in 02 (keeps both tables aligned).
  trends_hz_set <- table1_df$zone_sante_notification

  table2_df <- beta_tab |>
    dplyr::left_join(
      recent_cases_tab,
      by = c("zone_sante_notification", "threshold_time_key")
    ) |>
    dplyr::left_join(detection_tab, by = c("zone_sante_notification", "threshold_time_key")) |>
    dplyr::left_join(province_tab, by = "zone_sante_notification") |>
    dplyr::filter(threshold_time_key == last_key) |>
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
    )

  # --- 6c. Order rows by aai ascending; ensemble row last ---------
  hz_order <- table1_df |>
    dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
    dplyr::arrange(aai, dplyr::desc(total_alerts)) |>
    dplyr::pull(zone_sante_notification)

  order_by_aai <- function(df) {
    hz_rows <- df |>
      dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
      dplyr::slice(match(hz_order, zone_sante_notification))
    ensemble_rows <- df |>
      dplyr::filter(zone_sante_notification == "Ensemble de la zone affectée")
    dplyr::bind_rows(hz_rows, ensemble_rows)
  }

  table1_df <- order_by_aai(table1_df)
  table2_df <- order_by_aai(table2_df)

  # Intermediate params have no ensemble row; aggregate it from HZs.
  if (!any(table2_df$zone_sante_notification == "Ensemble de la zone affectée")) {
    ensemble_row <- table2_df |>
      dplyr::summarise(
        zone_sante_notification = "Ensemble de la zone affectée",
        Province = "Ensemble",
        beta_c = NA_real_,
        beta_d = NA_real_,
        n_recent_confirmed = sum(n_recent_confirmed, na.rm = TRUE),
        n_recent_confirmed_nowcast = sum(
          n_recent_confirmed_nowcast,
          na.rm = TRUE
        ),
        detection_rate_adj = NA_real_,
        estimated_true_cases_recent = sum(
          estimated_true_cases_recent,
          na.rm = TRUE
        )
      )
      table2_df <- dplyr::bind_rows(table2_df#, ensemble_row
                                    ) |> 
                    filter(n_recent_confirmed_nowcast > 0)
  }

  table1_labels_fr <- c(
    zone_sante_notification = "Zone de santé",
    Province = "Province",
    case_alerts = "Alertes de cas",
    death_alerts = "Alertes de décès",
    total_alerts = "Total des alertes",
    Alert_case_threshold = "Seuil médian : cas",
    Alert_death_threshold = "Seuil médian : décès",
    case_adequacy = "Performance : cas",
    death_adequacy = "Performance : décès",
    aai = "Performance globale (AAI)"
  )
  table2_labels_fr <- c(
    zone_sante_notification = "Zone de santé",
    Province = "Province",
    beta_c = "Coefficient β : cas",
    beta_d = "Coefficient β : décès",
    n_recent_confirmed = "Cas confirmés récents",
    n_recent_confirmed_nowcast = "Cas confirmés récents (nowcast)",
    detection_rate_adj = "Taux de détection combiné",
    estimated_true_cases_recent = "Cas vrais récents estimés"
  )

  # --- 6d. kableExtra HTML export ---------------------------------
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    table_caption <- paste0(
      "Synthèse d'adéquation des alertes - dernière fenêtre de 7 jours se terminant le ",
      window_label
    )

    table1_html <- table1_df |>
      kableExtra::kbl(
        caption = table_caption,
        col.names = unname(table1_labels_fr[names(table1_df)]),
        align = c("l", "l", rep("r", 8))
      ) |>
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE
      )

    table2_html <- table2_df |>
      kableExtra::kbl(
        caption = paste0(table_caption, " (paramètres du modèle)"),
        col.names = unname(table2_labels_fr[names(table2_df)]),
        align = c("l", "l", rep("r", 6))
      ) |>
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE
      )

    tables_html_path <- file.path(
      adeq_output_dir,
      "alert_trends_tables_last_window.html"
    )
    htmltools::save_html(
      htmltools::tagList(
        htmltools::h3("Tableau 1 - Alertes, seuils et performance"),
        table1_html,
        htmltools::h3("Tableau 2 - Paramètres du modèle (β, nowcast, détection)"),
        table2_html
      ),
      file = tables_html_path
    )
    message("  -> ", tables_html_path)
  } else {
    message("Package 'kableExtra' not available - skipping HTML tables.")
  }

  # --- 6e. XLSX export (two sheets) -------------------------------
  if (requireNamespace("writexl", quietly = TRUE)) {
    table1_export <- rlang::set_names(
      table1_df,
      unname(table1_labels_fr[names(table1_df)])
    )
    table2_export <- rlang::set_names(
      table2_df,
      unname(table2_labels_fr[names(table2_df)])
    )
    tables_xlsx_path <- file.path(
      adeq_output_dir,
      "alert_trends_tables_last_window.xlsx"
    )
    writexl::write_xlsx(
      list(
        "Adéquation des alertes" = table1_export,
        "Paramètres du modèle" = table2_export
      ),
      tables_xlsx_path
    )
    message("  -> ", tables_xlsx_path)
  } else {
    message("Package 'writexl' not available - skipping XLSX export.")
  }

  # --- 6f. Ensemble time-series tables (whole affected area, all windows) ---
  if (show_ensemble_tables) {
    message("Rendering ensemble time-series summary tables...")

    # Table 1: alerts / thresholds / adequacy for the ensemble, all windows
    table1_ensemble_df <- trends_smooth_adeq |>
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

    # Table 2: model parameters for the ensemble, all windows.
    #   beta_c / beta_d / estimated_true_cases_recent come from the official
    #   01 ensemble rows (means for beta, sums for true cases over the
    #   55-zone set); recent confirmed counts are summed over HZs; the pooled
    #   detection rate = sum(nowcast) / sum(estimated true cases).
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

    table2_ensemble_df <- ensemble_cdp |>
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
      # Match the column order of the health-zone Table 2 (section 6b)
      dplyr::select(
        threshold_time_key,
        beta_c, beta_d,
        n_recent_confirmed, n_recent_confirmed_nowcast,
        detection_rate_adj, estimated_true_cases_recent
      )

    # French labels (reuse section-6 vectors, prepend the window column)
    table1_ensemble_labels_fr <- c(
      threshold_time_key = "Semaine de notification (début)",
      table1_labels_fr[setdiff(names(table1_ensemble_df), "threshold_time_key")]
    )
    table2_ensemble_labels_fr <- c(
      threshold_time_key = "Semaine de notification (début)",
      table2_labels_fr[setdiff(names(table2_ensemble_df), "threshold_time_key")]
    )

    # HTML export
    if (requireNamespace("kableExtra", quietly = TRUE)) {
      ensemble_caption <- paste0(
        "Synthèse d'adéquation des alertes - Ensemble de la zone affectée, ",
        "toutes fenêtres"
      )

      table1_ensemble_html <- table1_ensemble_df |>
        kableExtra::kbl(
          caption = ensemble_caption,
          col.names = unname(
            table1_ensemble_labels_fr[names(table1_ensemble_df)]
          ),
          align = c("l", rep("r", 8))
        ) |>
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed"),
          full_width = FALSE
        )

      table2_ensemble_html <- table2_ensemble_df |>
        kableExtra::kbl(
          caption = paste0(ensemble_caption, " (paramètres du modèle)"),
          col.names = unname(
            table2_ensemble_labels_fr[names(table2_ensemble_df)]
          ),
          align = c("l", rep("r", 6))
        ) |>
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed"),
          full_width = FALSE
        )

      ensemble_html_path <- file.path(
        adeq_output_dir,
        "alert_trends_tables_ensemble_time.html"
      )
      htmltools::save_html(
        htmltools::tagList(
          htmltools::h3(
            "Tableau 1 - Ensemble de la zone affectée : alertes, seuils et performance"
          ),
          table1_ensemble_html,
          htmltools::h3(
            "Tableau 2 - Ensemble de la zone affectée : paramètres du modèle (β, nowcast, détection)"
          ),
          table2_ensemble_html
        ),
        file = ensemble_html_path
      )
      message("  -> ", ensemble_html_path)
    } else {
      message("Package 'kableExtra' not available - skipping ensemble HTML tables.")
    }

    # XLSX export (two sheets)
    if (requireNamespace("writexl", quietly = TRUE)) {
      table1_ensemble_export <- rlang::set_names(
        table1_ensemble_df,
        unname(table1_ensemble_labels_fr[names(table1_ensemble_df)])
      )
      table2_ensemble_export <- rlang::set_names(
        table2_ensemble_df,
        unname(table2_ensemble_labels_fr[names(table2_ensemble_df)])
      )
      ensemble_xlsx_path <- file.path(
        adeq_output_dir,
        "alert_trends_tables_ensemble_time.xlsx"
      )
      writexl::write_xlsx(
        list(
          "Adéquation (ensemble)" = table1_ensemble_export,
          "Paramètres (ensemble)" = table2_ensemble_export
        ),
        ensemble_xlsx_path
      )
      message("  -> ", ensemble_xlsx_path)
    } else {
      message("Package 'writexl' not available - skipping ensemble XLSX export.")
    }
  }
}

# ---------------------------------------------------------------- #
# 7. Done
# ---------------------------------------------------------------- #
message("\nPlots complete.")
message("  Overview: ", adeq_output_dir)
message("  Per-HZ:   ", per_hz_dir)
