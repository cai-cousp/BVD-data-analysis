# Script 2: Alert Trends Analysis
# Purpose: compute weekly adequacy measures and detect trends using the threshold synthesis
# Inputs: latest cleaned EVD line list; 01_thresholds_synthesis.rds and
#   01_thresholds_ensemble.rds from the same output directory
# Outputs: 02_trends_smooth.rds, 02_trends_smooth_adeq.rds, PDF and xlsx exports


suppressPackageStartupMessages({
  library(zoo)
  library(Kendall)
  library(ggplot2)
})

# Load helpers
source(here::here("R/alert_helpers.R"))

# Load shared DataAnalysis helper(s)
if (!exists("load_latest_data", mode = "function")) {
  source(file.path(evd17_root, "DataAnalysis", "helpers", "LoadLatestData.R"))
}

# normalize_threshold_columns() is now sourced from alert_helpers.R

# 1. Paths
data_folder <- file.path(evd17_root, "DataCleaning", "data", "Output")
output_dir <- create_output_dir(here::here("output"))

# 2. Load Data
message("Loading data...")
latest_evd <- latest_evd_file(data_folder)
evd_snapshot_key <- basename(latest_evd)
evd_file_date <- extract_evd_date_stamp(evd_snapshot_key)

evd <- load_latest_data(
  base_dir = data_folder,
  format = "rds",
  file_pattern = "evd.clean_Int_.*\\.rds"
)

# Locate thresholds file matching the latest EVD timestamp first, with fallbacks
thresholds_path <- file.path(output_dir, sprintf("01_thresholds_synthesis_%s.rds", evd_file_date))
if (!file.exists(thresholds_path)) {
  thresholds_path <- file.path(output_dir, "01_thresholds_synthesis.rds")
}
if (!file.exists(thresholds_path) || is.na(thresholds_path)) {
  thresholds_path <- latest_output_file(
    here::here("output"),
    sprintf("01_thresholds_synthesis_%s\\.rds", evd_file_date)
  )
}
if (is.na(thresholds_path) || !file.exists(thresholds_path)) {
  thresholds_path <- latest_output_file(here::here("output"), "01_thresholds_synthesis.*\\.rds")
}

ensemble_thresholds_path <- file.path(
  dirname(thresholds_path),
  sprintf("01_thresholds_ensemble_%s.rds", evd_file_date)
)
if (!file.exists(ensemble_thresholds_path)) {
  ensemble_thresholds_path <- file.path(
    dirname(thresholds_path),
    "01_thresholds_ensemble.rds"
  )
}

if (is.na(thresholds_path) || !file.exists(thresholds_path)) {
  rlang::abort("Thresholds file not found. Run 01_alert_thresholds.R first.")
}
if (is.na(ensemble_thresholds_path) || !file.exists(ensemble_thresholds_path)) {
  rlang::abort(c(
    "Ensemble thresholds file not found in the threshold output directory.",
    x = ensemble_thresholds_path,
    i = "Run 01_alert_thresholds.R again to create both threshold artifacts."
  ))
}

thresholds_hz <- normalize_threshold_columns(readRDS(thresholds_path))
thresholds_ensemble <- normalize_threshold_columns(
  readRDS(ensemble_thresholds_path)
)

if (interactive()) {
  evd |> 
    mutate(alert_date_debut_symptoms = as.Date(alert_date_debut_symptoms)) |>
    filter(!is.na(alert_date_debut_symptoms))
}

if (any(thresholds_hz$zone_sante_notification == "Ensemble de la zone affectée")) {
  rlang::abort("01_thresholds_synthesis.rds must contain zone de santé rows only.")
}
if (!all(
  thresholds_ensemble$zone_sante_notification == "Ensemble de la zone affectée"
)) {
  rlang::abort("01_thresholds_ensemble.rds must contain ensemble rows only.")
}
if (!setequal(
  unique(thresholds_hz$threshold_time_key),
  unique(thresholds_ensemble$threshold_time_key)
)) {
  rlang::abort("Zone and ensemble thresholds do not share the same time-window grid.")
}

evd_max_date_attr <- attr(thresholds_hz, "evd_max_date")
if (!identical(evd_max_date_attr, attr(thresholds_ensemble, "evd_max_date"))) {
  rlang::abort("Zone and ensemble thresholds were not produced in the same run.")
}
threshold_run_id_attr <- attr(thresholds_hz, "threshold_run_id")
if (is.null(threshold_run_id_attr) ||
    !identical(threshold_run_id_attr, attr(thresholds_ensemble, "threshold_run_id"))) {
  rlang::abort("Zone and ensemble threshold run IDs do not match.")
}

thresholds <- dplyr::bind_rows(thresholds_hz, thresholds_ensemble)
attr(thresholds, "evd_max_date") <- evd_max_date_attr
attr(thresholds, "threshold_run_id") <- threshold_run_id_attr

if (interactive()) {
  thresholds_hz |>
    filter(zone_sante_notification == "Bunia") |>
    View()
  thresholds_ensemble |> View()
}

# 3. Phase 1: Data Preparation
# The analysis grid is the threshold window grid from 01_alert_thresholds.R
# (backward-anchored, non-overlapping 7-day windows). Alerts outside that grid
# are excluded explicitly here so the window join below never drops rows
# silently.
message("Preparing trend data...")
grid_start <- min(thresholds$recent_case_window_start, na.rm = TRUE)
grid_end   <- max(thresholds$recent_case_window_end, na.rm = TRUE)

evd_val <- evd |>
  mutate(date_heure_notification_alerte = as.Date(date_heure_notification_alerte)) |>
  dplyr::filter(alert_conlusion %in% c("validée", "Validée"))

n_alerts_outside_grid <- evd_val |>
  dplyr::filter(date_heure_notification_alerte < grid_start |
                  date_heure_notification_alerte > grid_end) |>
  nrow()

evd_val <- evd_val |>
  dplyr::filter(date_heure_notification_alerte >= grid_start,
                date_heure_notification_alerte <= grid_end)

message("Alert grid: ", format(grid_start), " to ", format(grid_end),
        " (excluded ", n_alerts_outside_grid, " validated alerts outside the grid)")

if (interactive()) {
  names(evd_val)
  unique(evd_val$nature_alerte)
  sort(unique(as.Date(evd_val$date_heure_notification_alerte)))
}

# 4. Phase 2 & 3: Overall & HZ Trends with Thresholds
message("Analyzing trends...")
trends_with_thresholds <- join_thresholds_to_alert_counts(evd_val, thresholds) |>
  dplyr::mutate(
    case_threshold_mid = (case_lower + case_upper) / 2,
    death_threshold_mid = (death_lower + death_upper) / 2,
    case_threshold_mid_B = (alert_case_threshold_lower_B + alert_case_threshold_upper_B) / 2,
    death_threshold_mid_B = (alert_death_threshold_lower_B + alert_death_threshold_upper_B) / 2,
    case_threshold_mid_C = (alert_case_threshold_lower_C + alert_case_threshold_upper_C) / 2,
    death_threshold_mid_C = (alert_death_threshold_lower_C + alert_death_threshold_upper_C) / 2,

    case_adequacy = dplyr::if_else(
      is.finite(case_threshold_mid) & case_threshold_mid > 0,
      case_alerts / case_threshold_mid,
      NA_real_
    ),
    death_adequacy = dplyr::if_else(
      is.finite(death_threshold_mid) & death_threshold_mid > 0,
      death_alerts / death_threshold_mid,
      NA_real_
    ),
    case_adequacy_B = dplyr::if_else(
      is.finite(case_threshold_mid_B) & case_threshold_mid_B > 0,
      case_alerts / case_threshold_mid_B,
      NA_real_
    ),
    death_adequacy_B = dplyr::if_else(
      is.finite(death_threshold_mid_B) & death_threshold_mid_B > 0,
      death_alerts / death_threshold_mid_B,
      NA_real_
    ),
    case_adequacy_C = dplyr::if_else(
      is.finite(case_threshold_mid_C) & case_threshold_mid_C > 0,
      case_alerts / case_threshold_mid_C,
      NA_real_
    ),
    death_adequacy_C = dplyr::if_else(
      is.finite(death_threshold_mid_C) & death_threshold_mid_C > 0,
      death_alerts / death_threshold_mid_C,
      NA_real_
    ),
    threshold_available = !is.na(case_adequacy) | !is.na(death_adequacy),
    threshold_available_B = !is.na(case_adequacy_B) | !is.na(death_adequacy_B),
    threshold_available_C = !is.na(case_adequacy_C) | !is.na(death_adequacy_C),
    aai = rowMeans(dplyr::pick(case_adequacy, death_adequacy), na.rm = TRUE),
    aai = dplyr::if_else(threshold_available, aai, NA_real_),
    aai_B = rowMeans(dplyr::pick(case_adequacy_B, death_adequacy_B), na.rm = TRUE),
    aai_B = dplyr::if_else(threshold_available_B, aai_B, NA_real_),
    aai_C = rowMeans(dplyr::pick(case_adequacy_C, death_adequacy_C), na.rm = TRUE),
    aai_C = dplyr::if_else(threshold_available_C, aai_C, NA_real_)
  )

if (interactive()) {
  trends_with_thresholds |>
    filter(zone_sante_notification == "Bunia") |>
    View()
  trends_with_thresholds |>
    filter(zone_sante_notification == "Ensemble de la zone affectée") |>
    View()
}


trends_smooth <- trends_with_thresholds |>
  dplyr::arrange(zone_sante_notification, week_start) |>
  dplyr::mutate(
    case_alerts_3w = zoo::rollmean(case_alerts, k = 3, fill = NA, align = "right"),
    death_alerts_3w = zoo::rollmean(death_alerts, k = 3, fill = NA, align = "right"),
    .by = zone_sante_notification
  )

hz_trends <- trends_smooth |>
  dplyr::filter(!is.na(aai), n() >= 4, .by = zone_sante_notification) |>
  dplyr::summarise(
    tau = tryCatch(Kendall::MannKendall(total_alerts)$tau[1], error = function(e) NA_real_),
    p_value = tryCatch(Kendall::MannKendall(total_alerts)$sl[1], error = function(e) NA_real_),
    .by = zone_sante_notification
  ) |>
  dplyr::mutate(
    trend_direction = dplyr::case_when(
      is.na(p_value) ~ "Unknown",
      p_value < 0.05 & tau > 0 ~ "Increasing",
      p_value < 0.05 & tau < 0 ~ "Decreasing",
      .default = "Stable"
    )
  )

# 5. Phase 4: Ranking & Adequacy
message("Ranking HZs by Adequacy for more recent alert data...")

# Recent sub-windows are anchored on the latest window END (not the latest
# week_start, which would end the "recent" period one week early) and use
# strict > with 42/21 days to keep exactly the last 6 / 3 completed windows.
last_window_end <- max(trends_smooth$threshold_valid_to, na.rm = TRUE)

hz_trends_recent <- trends_smooth |>
  dplyr::filter(threshold_valid_to > last_window_end - lubridate::days(42)) |>
  dplyr::filter(!is.na(aai), n() >= 3, .by = zone_sante_notification) |>
  dplyr::summarise(
    tau = tryCatch(Kendall::MannKendall(total_alerts)$tau[1], error = function(e) NA_real_),
    p_value = tryCatch(Kendall::MannKendall(total_alerts)$sl[1], error = function(e) NA_real_),
    .by = zone_sante_notification
  ) |>
  dplyr::mutate(
    trend_direction = dplyr::case_when(
      is.na(p_value) ~ "Unknown",
      p_value < 0.05 & tau > 0 ~ "Increasing",
      p_value < 0.05 & tau < 0 ~ "Decreasing",
      .default = "Stable"
    )
  )

# Retrieve or compute reporting delays per HZ x window
delays_path <- file.path(output_dir, sprintf("01_intermediate_parameters_%s.rds", evd_file_date))
if (!file.exists(delays_path)) delays_path <- file.path(output_dir, "01_intermediate_parameters.rds")
intermediate_params <- tryCatch(readRDS(delays_path), error = function(e) NULL)

delays_tab <- if (!is.null(intermediate_params$delays)) {
  intermediate_params$delays
} else {
  compute_delays_by_hz(evd, windows = thresholds, lookback_days = 21L)
}

recent_delays <- delays_tab |>
  dplyr::filter(threshold_valid_to > last_window_end - lubridate::days(21)) |>
  dplyr::summarise(
    mean_delay_days = if (all(is.na(median_delay_used))) NA_real_ else mean(median_delay_used, na.rm = TRUE),
    mean_prop_notif_48h = if (all(is.na(prop_notif_48h_used))) NA_real_ else mean(prop_notif_48h_used, na.rm = TRUE),
    .by = zone_sante_notification
  )

recent_adequacy <- trends_smooth |>
  dplyr::filter(threshold_valid_to > last_window_end - lubridate::days(21)) |>
  dplyr::summarise(
    mean_aai = if (all(is.na(aai))) NA_real_ else mean(aai, na.rm = TRUE),
    .by = zone_sante_notification
  ) |>
  dplyr::mutate(
    adequacy_category = dplyr::case_when(
      mean_aai < 0.75 ~ "Under-alerting",
      mean_aai > 2.0 ~ "Over-alerting",
      .default = "Adequate"
    )
  ) |>
  dplyr::left_join(hz_trends, by = "zone_sante_notification") |>
  dplyr::left_join(recent_delays, by = "zone_sante_notification") |>
  dplyr::mutate(
    performance_category = dplyr::case_when(
      adequacy_category == "Under-alerting" & (is.na(mean_delay_days) | mean_delay_days > 2) ~ "Under-alerting & Delayed",
      adequacy_category == "Under-alerting" & mean_delay_days <= 2 ~ "Under-alerting & Rapid",
      adequacy_category == "Over-alerting" ~ "Over-alerting",
      adequacy_category == "Adequate" & !is.na(mean_delay_days) & mean_delay_days > 2 ~ "Adequate Volume & Delayed",
      .default = "Optimal (Adequate & Rapid)"
    )
  ) |>
  dplyr::arrange(mean_aai)

delay_join_cols <- c("zone_sante_notification", "threshold_time_key")

trends_smooth_adeq <-
  trends_smooth |>
    dplyr::left_join(
      delays_tab |> dplyr::select(
        dplyr::any_of(c(delay_join_cols, "median_delay_used", "prop_notif_48h_used", "median_death_delay_used"))
      ),
      by = delay_join_cols
    ) |>
    dplyr::mutate(
    adequacy_category = dplyr::case_when(
      is.na(aai) ~ NA_character_,
      aai < 0.75 ~ "Under-alerting",
      aai > 2.0 ~ "Over-alerting",
      .default = "Adequate"
    ),
    performance_category = dplyr::case_when(
      is.na(aai) ~ NA_character_,
      aai < 0.75 & (is.na(median_delay_used) | median_delay_used > 2) ~ "Under-alerting & Delayed",
      aai < 0.75 & median_delay_used <= 2 ~ "Under-alerting & Rapid",
      aai > 2.0 ~ "Over-alerting",
      aai >= 0.75 & aai <= 2.0 & !is.na(median_delay_used) & median_delay_used > 2 ~ "Adequate Volume & Delayed",
      .default = "Optimal (Adequate & Rapid)"
    ),
    adequacy_category_B = dplyr::case_when(
      is.na(aai_B) ~ NA_character_,
      aai_B < 0.75 ~ "Under-alerting",
      aai_B > 2.0 ~ "Over-alerting",
      .default = "Adequate"
    ),
    adequacy_category_C = dplyr::case_when(
      is.na(aai_C) ~ NA_character_,
      aai_C < 0.75 ~ "Under-alerting",
      aai_C > 2.0 ~ "Over-alerting",
      .default = "Adequate"
    ),
     aai.3w = ((case_alerts_3w/Alert_case_threshold)+
              (death_alerts_3w/Alert_death_threshold))/2,
     adequacy_category.3w = dplyr::case_when(
      is.na(aai.3w) ~ NA_character_,
      aai.3w < 0.75 ~ "Under-alerting",
      aai.3w > 2.0 ~ "Over-alerting",
      .default = "Adequate"
    ),
     aai.3w_B = ((case_alerts_3w/case_threshold_mid_B)+
              (death_alerts_3w/death_threshold_mid_B))/2,
     adequacy_category.3w_B = dplyr::case_when(
      is.na(aai.3w_B) ~ NA_character_,
      aai.3w_B < 0.75 ~ "Under-alerting",
      aai.3w_B > 2.0 ~ "Over-alerting",
      .default = "Adequate"
    ),
     aai.3w_C = ((case_alerts_3w/case_threshold_mid_C)+
              (death_alerts_3w/death_threshold_mid_C))/2,
     adequacy_category.3w_C = dplyr::case_when(
      is.na(aai.3w_C) ~ NA_character_,
      aai.3w_C < 0.75 ~ "Under-alerting",
      aai.3w_C > 2.0 ~ "Over-alerting",
      .default = "Adequate"
    )
  ) |>
  select(-c(recent_case_window_start:recent_case_anchor_date,week_start,
         case_threshold_mid,death_threshold_mid,case_threshold_mid_B,death_threshold_mid_B,case_threshold_mid_C,death_threshold_mid_C,
         threshold_available,threshold_available_B,threshold_available_C)) |>
  rename(Alert_case_threshold_lower=case_lower,Alert_case_threshold_upper=case_upper,
         Alert_death_threshold_lower = death_lower,Alert_death_threshold_upper = death_upper)|>
  relocate(adequacy_category, performance_category, .after = aai) 

if (interactive()) {
  hz_trends_recent |>
    dplyr::filter(zone_sante_notification == "Bunia") |>
    View()
  recent_adequacy |>
    dplyr::filter(zone_sante_notification == "Bunia") |>
    View()
  trends_smooth |>
    dplyr::filter(zone_sante_notification == "Bunia") |>
    View()
  trends_smooth_adeq |>
    dplyr::filter(zone_sante_notification == "Bunia") |>
    View()
}

# Save
evd_max_date_attr <- attr(thresholds, "evd_max_date")
if (!is.null(evd_max_date_attr)) {
  attr(trends_smooth, "evd_max_date") <- evd_max_date_attr
  attr(trends_smooth_adeq, "evd_max_date") <- evd_max_date_attr
}
attr(trends_smooth, "evd_file_date") <- evd_file_date
attr(trends_smooth_adeq, "evd_file_date") <- evd_file_date
attr(trends_smooth, "evd_snapshot_key") <- evd_snapshot_key
attr(trends_smooth_adeq, "evd_snapshot_key") <- evd_snapshot_key

saveRDS(trends_smooth, file.path(output_dir, sprintf("02_trends_smooth_%s.rds", evd_file_date)))
saveRDS(trends_smooth, file.path(output_dir, "02_trends_smooth.rds"))
saveRDS(trends_smooth_adeq, file.path(output_dir, sprintf("02_trends_smooth_adeq_%s.rds", evd_file_date)))
saveRDS(trends_smooth_adeq, file.path(output_dir, "02_trends_smooth_adeq.rds"))

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(recent_adequacy, file.path(output_dir, sprintf("02_recent_adequacy_%s.xlsx", evd_file_date)))
  writexl::write_xlsx(recent_adequacy, file.path(output_dir, "02_recent_adequacy.xlsx"))
  writexl::write_xlsx(trends_smooth, file.path(output_dir, sprintf("trends_smooth_%s.xlsx", evd_file_date)))
  writexl::write_xlsx(trends_smooth, file.path(output_dir, "trends_smooth.xlsx"))
  writexl::write_xlsx(trends_smooth_adeq, file.path(output_dir, sprintf("trends_smooth_adeq_%s.xlsx", evd_file_date)))
  writexl::write_xlsx(trends_smooth_adeq, file.path(output_dir, "trends_smooth_adeq.xlsx"))
}

pdf(file.path(output_dir, sprintf("02_alert_trends_%s.pdf", evd_file_date)), width = 11, height = 8)
p1 <- ggplot(trends_smooth |> 
                  dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
                  dplyr::summarise(total = sum(total_alerts), .by = week_start),
             aes(x = week_start, y = total)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Overall Validated Alerts over Time", x = "Semaine de notification (date de début)", y = "Total Alerts")
print(p1)

ens_data <- trends_smooth |> dplyr::filter(zone_sante_notification == "Ensemble de la zone affectée")
if (nrow(ens_data) > 0) {
  p_ens <- ggplot(ens_data, aes(x = week_start)) +
    geom_col(aes(y = total_alerts), fill = "steelblue", alpha = 0.7) +
    geom_line(aes(y = case_alerts_3w + tidyr::replace_na(death_alerts_3w, 0)),
              colour = "firebrick", linetype = "dashed", linewidth = 0.8, na.rm = TRUE) +
    theme_minimal() +
    labs(title = "Validated Alert Trends - Ensemble de la zone affectée",
         subtitle = "Total alerts (bars) and combined 3-week rolling mean (dashed red line)",
         x = "Semaine de notification (date de début)", y = "Total Alerts")
  print(p_ens)
}

top_under <- recent_adequacy |>
  dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée") |>
  dplyr::pull(zone_sante_notification) |>
  head(9)

if (length(top_under) > 0) {
  p2 <- ggplot(trends_smooth |> dplyr::filter(zone_sante_notification %in% top_under), 
               aes(x = week_start, y = total_alerts)) +
    geom_col(fill = "coral") +
    facet_wrap(~zone_sante_notification, scales = "free_y") +
    theme_minimal() +
    labs(title = "Trends in Under-alerting HZs", x = "Semaine de notification (date de début)", y = "Alerts")
  print(p2)
}
dev.off()
file.copy(
  file.path(output_dir, sprintf("02_alert_trends_%s.pdf", evd_file_date)),
  file.path(output_dir, "02_alert_trends.pdf"),
  overwrite = TRUE
)

message("Trend analysis complete. Output saved to ", output_dir)
