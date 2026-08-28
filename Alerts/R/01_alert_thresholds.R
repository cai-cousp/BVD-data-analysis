# Script 1: Alert Threshold Estimation
# Purpose: estimate per-HZ weekly alert thresholds by triangulating baseline mortality, Beni benchmark, and case-derived expectations
# Inputs: latest cleaned line list, Beni historical alerts, DHIS2 population file
# Outputs: 01_thresholds_synthesis.rds, 01_thresholds_ensemble.rds,
#   01_intermediate_parameters.rds, xlsx exports

# Approach A: Baseline Mortality
# Approach B: Beni Historical Benchmark
# Approach C: Case-Derived Expectations (from the line list)

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(MASS)
  library(EpiEstim)
})

# Load project-local helpers
source(here::here("R/alert_helpers.R"))

# Load shared DataAnalysis helper(s)
if (!exists("load_latest_data", mode = "function")) {
  source(file.path(evd17_root, "DataAnalysis", "helpers", "LoadLatestData.R"))
}

# 1. Paths
data_folder <- file.path(evd17_root, "DataCleaning", "data", "Output")
beni_path <- file.path(
  evd17_root,
  "DataAnalysis",
  "data",
  "Alerts",
  "Alert_Beni.rds"
)
pop_path <- file.path(
  evd17_root,
  "DataAnalysis",
  "data",
  "PopulationParAge",
  "DRC_ZS_DHIS2_Pop_2024.xlsx"
)
output_dir <- create_output_dir(here::here("output"))

# Population projection parameters (base: 2024 DHIS2)
pop_base_year <- 2024L
pop_growth_rate <- 0.0129 # DRC annual growth rate (~1.29%)

# 2. Load Data
message("Loading data...")
evd <- load_latest_data(
  base_dir = data_folder,
  format = "rds",
  file_pattern = "evd.clean_Int_.*\\.rds"
)

evd |>
  filter(classification_finale == "Cas confirmé") |>
  nrow()

# Cleaned contact follow-up data (wide, one row per contact). Loaded through
# the same "latest file" mechanism because the Output folder is date-stamped.
contacts <- load_latest_data(
  base_dir = data_folder,
  format = "rds",
  file_pattern = "contact.clean_Int_.*\\.rds"
)

# Zone de snaté with at least one confirmed case

conf.zs <-
  evd |>
  filter(classification_finale == "Cas confirmé"|
         lab_resultat_final == "Positif") |>
  count(zone_sante_notification) |> pull(zone_sante_notification)

confirmed_notification_dates <- evd |>
  dplyr::mutate(
    is_confirmed = alert_is_confirmed_case(evd),
    notification_date = as.Date(date_heure_notification_alerte)
  ) |>
  dplyr::filter(
    is_confirmed,
    !is.na(zone_sante_notification),
    !is.na(notification_date)
  ) |>
  dplyr::summarise(
    first_confirmed_notification_date = min(notification_date),
    .by = zone_sante_notification
  )

zones_without_notification_dates <- setdiff(
  conf.zs,
  confirmed_notification_dates$zone_sante_notification
)
if (length(zones_without_notification_dates) > 0L) {
  rlang::abort(c(
    "Affected health zones have no usable confirmed notification date:",
    x = paste(zones_without_notification_dates, collapse = ", ")
  ))
}

# Per-HZ EpiNow2 nowcasts (fitted once on the latest snapshot, cached so the
# daily re-run only refits when the line list changes).
message("Computing per-HZ EpiNow2 nowcasts...")
latest_evd <- latest_evd_file(data_folder)
evd_snapshot_key <- basename(latest_evd)
evd_file_date <- extract_evd_date_stamp(evd_snapshot_key)

nowcasts <- compute_nowcasts_by_zone(
  evd[evd$zone_sante_notification %in% conf.zs, ],
  ref_date = evd_notification_ref_date(evd),
  cache_path = file.path(output_dir, sprintf("05_nowcast_by_zone_%s.rds", evd_file_date)),
  snapshot_key = evd_snapshot_key
)
nowcasts_deaths <- compute_nowcasts_by_zone(
  evd[evd$zone_sante_notification %in% conf.zs, ],
  ref_date = evd_notification_ref_date(evd),
  series = "confirmed_deaths",
  cache_path = file.path(output_dir, sprintf("05_nowcast_by_zone_deaths_%s.rds", evd_file_date)),
  snapshot_key = evd_snapshot_key
)


names(evd)[grepl("num",names(evd), ignore.case = TRUE)]

# DHIS2 tracker "as-of" date — the most recent notification date in the line
# list. This is persisted as an attribute on the synthesis RDS so downstream
# scripts and plots can show it in captions without re-loading the raw data.

evd_max_date0 <- as.Date(evd$date_heure_notification_alerte)

evd_max_date <- tryCatch(
  max(evd_max_date0[which(evd_max_date0 <= Sys.Date())], na.rm = TRUE),
  error = function(e) -Inf
)
if (!is.finite(evd_max_date)) {
  evd_max_date <- Sys.Date()
}

beni <- readRDS(beni_path)
pop <- read_excel(pop_path) |>
  dplyr::filter(
    Province %in% c("Ituri", "Nord Kivu", "Nord-Kivu", "Sud Kivu", "Sud-Kivu")
  ) |>
  dplyr::mutate(Province = stringr::str_replace(Province, "-", " "))

# Project 2024 population to current year using compound growth
current_year <- as.integer(format(Sys.Date(), "%Y"))
years_elapsed <- current_year - pop_base_year
growth_factor <- (1 + pop_growth_rate)^years_elapsed

message(
  "Projecting population from ",
  pop_base_year,
  " to ",
  current_year,
  " (rate = ",
  pop_growth_rate,
  ", factor = ",
  sprintf("%.5f", growth_factor),
  ")"
)

pop <- pop |>
  dplyr::mutate(Population = round(Population * growth_factor, 0))

# Keep the full provincial population for the historical EVD10 benchmark
# lookup; restrict the per-HZ threshold pipeline to affected zones only.
pop_all <- pop
pop <- pop |>
  dplyr::filter(HZ %in% conf.zs)
message(
  "Restricting threshold pipeline to ", nrow(pop),
  " affected health zones (of ", nrow(pop_all), " in population file)"
)

# 3. Phase 1: Baseline Mortality Thresholds (Approach A)
message("Phase 1: Baseline mortality thresholds...")

affect.prov <- unique(evd$province_notification[
  evd$classification_finale == "Cas confirmé"
])

national.cmr <- 8.3 # per 1,000 per year (DRC average, WHO 2025)

cmr_province <- tibble::tibble(
  Province = affect.prov,
  cmr_per_1000_yr = rep(national.cmr, length(affect.prov)) # Fallback
)

baseline_thresholds <- pop |>
  dplyr::left_join(cmr_province, by = "Province") |>
  dplyr::mutate(
    expected_weekly_deaths_cmr = expected_deaths_baseline(
      Population,
      cmr_per_1000_yr,
      7
    ),
    death_threshold_lower_A = expected_weekly_deaths_cmr * 0.9, #qpois(0.50, lambda = expected_weekly_deaths_cmr),
    death_threshold_upper_A = expected_weekly_deaths_cmr * 1.1 #qpois(0.95, lambda = expected_weekly_deaths_cmr)
  ) |>
  dplyr::select(
    zone_sante_notification = HZ,
    Population,
    expected_weekly_deaths_cmr,
    death_threshold_lower_A,
    death_threshold_upper_A
  )

#baseline_thresholds  |> View()

# 4. Phase 2: EVD10 Historical Benchmark — All Health Zones (Approach B)
# Ref: Lekone & Finkenstädt, 2006 — EVD serial interval
message("Phase 2: EVD10 Historical Benchmark (all HZs)...")

evd10 <- beni
evd10_hz_col <- "Zones Sante"

evd10_val <- evd10 |>
  dplyr::filter(Conclusion_finale %in% c("validée", "Validée")) |>
  dplyr::mutate(
    epiweek = compute_epiweek(Date_alerte),
    hz = .data[[evd10_hz_col]]
  )

# Identify all health zones in the dataset
evd10_hzs <- sort(unique(evd10_val$hz))
message("  Found ", length(evd10_hzs), " health zones in EVD10 dataset")

# Per-HZ and per-week positivity
evd10_hz_pos <- evd10_val |>
  dplyr::summarise(
    n_val = dplyr::n(),
    n_sampled = sum(!is.na(lab_result)),
    n_pos = sum(lab_result == "Positif", na.rm = TRUE),
    .by = c(epiweek, hz)
  ) |>
  dplyr::filter(n_sampled > 0) |>
  dplyr::mutate(
    positivity_rate = n_pos / n_sampled
  )

# Optimal weeks per HZ: positivity < 10%
evd10_optimal_weeks <- evd10_hz_pos |>
  dplyr::filter(positivity_rate < 0.10)

# Alert rates per HZ during optimal periods (case vs death)
evd10_optimal_alerts <- evd10_val |>
  dplyr::semi_join(evd10_optimal_weeks, by = c("epiweek", "hz")) |>
  dplyr::summarise(
    n_cases = sum(Statut_initial %in% c("vivant", "Vivant", "VIVANT")),
    n_deaths = sum(Statut_initial %in% c("décédé", "Décédé", "Décédée")),
    n_optimal_weeks = dplyr::n_distinct(epiweek),
    .by = hz
  )

# Join population to compute rates per 100k per week
# HZ names in EVD10 may not match exactly — use title-case matching
pop_lookup <- pop_all |>
  dplyr::mutate(hz_match = stringr::str_to_title(tolower(HZ))) |>
  dplyr::select(hz_match, Population)

evd10_rates <- evd10_optimal_alerts |>
  dplyr::mutate(hz_match = stringr::str_to_title(tolower(hz))) |>
  dplyr::left_join(pop_lookup, by = "hz_match") |>
  dplyr::filter(!is.na(Population), n_optimal_weeks > 0) |>
  dplyr::mutate(
    case_rate_100k_wk = (n_cases / n_optimal_weeks) / Population * 1e5,
    death_rate_100k_wk = (n_deaths / n_optimal_weeks) / Population * 1e5
  )

# Identify best-performing HZs:
# - Minimum 50 validated alerts total
# - Top quartile of case alert rate during optimal periods
evd10_hz_total <- evd10_val |>
  dplyr::count(hz, name = "total_validated")

best_performing_hzs <- evd10_rates |>
  dplyr::left_join(evd10_hz_total, by = "hz") |>
  dplyr::filter(total_validated >= 50) |>
  dplyr::filter(
    case_rate_100k_wk >= quantile(case_rate_100k_wk, 0.75, na.rm = TRUE)
  )

message(
  "  Best-performing HZs (top quartile, ≥50 alerts): ",
  paste(best_performing_hzs$hz, collapse = ", ")
)

# Benchmark rates from best-performing HZs
benchmark_case <- best_performing_hzs$case_rate_100k_wk[which(
  best_performing_hzs$hz == "Beni"
)]
benchmark_case_lower <- qpois(0.25, lambda = benchmark_case) #quantile(benchmark_case, 0.50, na.rm = TRUE)
benchmark_case_upper <- qpois(0.95, lambda = benchmark_case) #quantile(benchmark_case, 0.75, na.rm = TRUE)
benchmark_death <- best_performing_hzs$death_rate_100k_wk[which(
  best_performing_hzs$hz == "Beni"
)] #best_performing_hzs$death_rate_100k_wk
benchmark_death_lower <- qpois(0.25, lambda = benchmark_death) #quantile(benchmark_death, 0.50, na.rm = TRUE)
benchmark_death_upper <- qpois(0.95, lambda = benchmark_death) #quantile(benchmark_death, 0.75, na.rm = TRUE)

# Scale to each target HZ by population
beni_thresholds <- pop |>
  dplyr::mutate(
    case_alerts = benchmark_case * Population / 1e5,
    alert_case_threshold_lower_B = benchmark_case_lower * Population / 1e5,
    alert_case_threshold_upper_B = benchmark_case_upper * Population / 1e5,
    death_alerts = benchmark_death * Population / 1e5,
    alert_death_threshold_lower_B = benchmark_death_lower * Population / 1e5,
    alert_death_threshold_upper_B = benchmark_death_upper * Population / 1e5
  ) |>
  dplyr::select(
    zone_sante_notification = HZ,
    case_alerts,
    death_alerts,
    dplyr::ends_with("_B")
  )

# 5. Phase 3: Case-Derived Expectations (Approach C)
message("Phase 3: Case-Derived Expectations...")

# Restrict the case-derived pipeline to affected health zones. All confirmed
# cases are in conf.zs by construction, so analysis dates are unchanged.
evd_conf <- evd |>
  dplyr::filter(zone_sante_notification %in% conf.zs)
contacts_conf <- contacts |>
  dplyr::filter(zone_sante_notification %in% conf.zs)

# Dual-Clock Grid Anchoring: derive analysis timeline covering active surveillance
# and clinical cases
notif_dates <- alert_resolve_notification_date(evd_conf)
valid_notif_dates <- notif_dates[!is.na(notif_dates) & notif_dates <= Sys.Date()]

confirmed_cases <- evd_conf |>
  dplyr::filter(alert_is_confirmed_case(evd_conf))

case_dates <- dplyr::coalesce(
  as.Date(confirmed_cases$alert_date_debut_symptoms),
  as.Date(confirmed_cases$s2_date_debut_signes_symptomes)
)
valid_case_dates <- case_dates[!is.na(case_dates) & case_dates <= Sys.Date()]

all_timeline_dates <- c(valid_notif_dates, valid_case_dates)
analysis_start_date <- max(min(all_timeline_dates, na.rm = TRUE), as.Date("2026-04-01"))
analysis_end_date <- min(max(all_timeline_dates, na.rm = TRUE), evd_max_date)

# Rolling 7-day recent cases — one row per HZ x time window.
hz_recent_cases <- compute_recent_confirmed_windows_by_hz(
  evd_conf,
  window_days = 7L,
  analysis_start_date = analysis_start_date,
  analysis_end_date = analysis_end_date,
  nowcast = nowcasts
)

window_grid <- hz_recent_cases |>
  dplyr::select(
    threshold_time_key,
    threshold_window_id,
    threshold_window_index,
    threshold_valid_from,
    threshold_valid_to,
    recent_case_window_start,
    recent_case_window_end,
    recent_case_window_days,
    recent_case_anchor_date
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(threshold_valid_from)

# Time-window indicators (trailing 21-day rate windows)
hz_cfr <- compute_cfr_by_hz(
  evd_conf,
  windows = window_grid,
  lookback_days = 21L,
  nowcast_deaths = nowcasts_deaths
)
hz_detection <- compute_detection_by_hz(
  evd_conf,
  windows = window_grid,
  lookback_days = 21L
)

# Back-calculation components (trailing 21-day rate windows)
hz_cfr_backcalc <- compute_cfr_backcalc_by_hz(
  evd_conf,
  windows = window_grid,
  lookback_days = 21L,
  nowcast = nowcasts,
  nowcast_deaths = nowcasts_deaths
)
hz_growth <- compute_growth_rate_by_hz(
  evd_conf,
  windows = window_grid,
  nowcast = nowcasts
)

hz_detection_backcalc <- compute_detection_cfr_backcalc_by_hz(
  evd_conf,
  hz_cfr_backcalc,
  hz_growth,
  windows = window_grid,
  lookback_days = 21L,
  nowcast = nowcasts,
  nowcast_deaths = nowcasts_deaths
)

# Contacts per confirmed case estimated per time window (trailing 21 days)
hz_contacts <- compute_contacts_per_case_by_hz(
  contacts_conf,
  evd_conf,
  windows = window_grid,
  lookback_days = 21L,
  nowcast = nowcasts
)

# Rt per HZ x time window (trailing 21 days), with window contacts ratio
hz_rt <- compute_rt_by_hz(
  evd_conf,
  windows = window_grid,
  nowcast = nowcasts
) |>
  dplyr::left_join(
    hz_contacts |> dplyr::select(zone_sante_notification, threshold_time_key, contacts_per_case_used),
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::mutate(
    sar = dplyr::if_else(
      is.finite(contacts_per_case_used) & contacts_per_case_used > 0,
      rt_used / contacts_per_case_used,
      NA_real_
    )
  )

# Notification timeliness and reporting delays per HZ x time window (trailing 21 days)
hz_delays <- compute_delays_by_hz(
  evd_conf,
  windows = window_grid,
  lookback_days = 21L
)

val_alerts <- evd_conf |> dplyr::filter(alert_conlusion %in% c("validée", "Validée"))

# Combine detection estimates using recent cases
case_derived_thresholds.i <- combine_detection_estimates_by_hz(
  hz_detection_backcalc,
  hz_detection,
  recent_cases = hz_recent_cases
) |>
  dplyr::rename(cfr_backcalc_used = cfr_used)

# Fit Poisson offset multipliers per window (trailing 3-week span) with lag adjustment
hz_multipliers <- compute_alert_multipliers_by_window(
  val_alerts = val_alerts,
  true_cases = case_derived_thresholds.i,
  hz_cfr = hz_cfr,
  evd = evd_conf,
  min_model_weeks = 2L,
  windows = window_grid,
  lookback_weeks = 3L,
  nowcast = nowcasts,
  lag_to_notification = TRUE
)

if (interactive()) {
  hz_multipliers |> filter(zone_sante_notification == "Bunia") |> View()
  case_derived_thresholds.i |> filter(zone_sante_notification == "Bunia") |> View()
  case_derived_thresholds.i |> View()
  evd |> filter(zone_sante_notification == "Bunia") |> View()
}


# Extract CI bounds from multipliers
multiplier_cols <- c(
  "zone_sante_notification", "threshold_time_key",
  "beta_c", "beta_c_low", "beta_c_high",
  "beta_d", "beta_d_low", "beta_d_high"
)

# Bin validated alerts by time window grid to compute per-window pooled multipliers
val_alerts_grid <- bin_dates_by_grid(
  val_alerts |> dplyr::mutate(alert_date = as.Date(date_heure_notification_alerte)),
  windows = window_grid,
  date_col = "alert_date"
) |>
  dplyr::summarise(
    val_alive_alerts = sum(nature_alerte == "Vivant", na.rm = TRUE),
    val_dead_alerts = sum(nature_alerte == "Décédé", na.rm = TRUE),
    .by = c(zone_sante_notification, threshold_time_key)
  )

window_multiplier_totals <- case_derived_thresholds.i |>
  dplyr::left_join(
    hz_cfr |> dplyr::select(zone_sante_notification, threshold_time_key, cfr_used),
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::left_join(
    val_alerts_grid,
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::mutate(
    val_alive_alerts = tidyr::replace_na(val_alive_alerts, 0L),
    val_dead_alerts = tidyr::replace_na(val_dead_alerts, 0L),
    expected_deaths_window = estimated_true_cases_recent * cfr_used
  ) |>
  dplyr::summarise(
    sum_alive_alerts = sum(val_alive_alerts, na.rm = TRUE),
    sum_dead_alerts = sum(val_dead_alerts, na.rm = TRUE),
    sum_true_cases = sum(estimated_true_cases_recent, na.rm = TRUE),
    sum_expected_deaths = sum(expected_deaths_window, na.rm = TRUE),
    .by = threshold_time_key
  ) |>
  dplyr::mutate(
    pooled_beta_c_window = dplyr::if_else(
      sum_true_cases > 0,
      sum_alive_alerts / sum_true_cases,
      NA_real_
    ),
    pooled_beta_d_window = dplyr::if_else(
      sum_expected_deaths > 0,
      sum_dead_alerts / sum_expected_deaths,
      NA_real_
    )
  )

if (interactive()) {
  window_multiplier_totals |> View()
}

case_derived_thresholds <- case_derived_thresholds.i |>
  dplyr::left_join(
    hz_multipliers |> dplyr::select(dplyr::any_of(multiplier_cols)),
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::left_join(
    hz_rt |> dplyr::select(zone_sante_notification, threshold_time_key, sar),
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::left_join(
    hz_contacts |> dplyr::select(zone_sante_notification, threshold_time_key, contacts_per_case_used),
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::left_join(
    window_multiplier_totals |> dplyr::select(threshold_time_key, pooled_beta_c_window, pooled_beta_d_window),
    by = "threshold_time_key"
  ) |>
  dplyr::left_join(
    hz_cfr |> dplyr::select(
      zone_sante_notification,
      threshold_time_key,
      cfr_used
    ),
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::mutate(
    expected_deaths = estimated_true_cases_recent * cfr_used,
    expected_deaths = dplyr::if_else(estimated_true_cases_recent == 0, 0, expected_deaths),
    beta_c = dplyr::coalesce(beta_c, pooled_beta_c_window, 1.0),
    beta_d = dplyr::coalesce(beta_d, pooled_beta_d_window, 1.0),
    beta_c_low = dplyr::coalesce(beta_c_low, beta_c),
    beta_c_high = dplyr::coalesce(beta_c_high, beta_c),
    beta_d_low = dplyr::coalesce(beta_d_low, beta_d),
    beta_d_high = dplyr::coalesce(beta_d_high, beta_d),
    sar = tidyr::replace_na(sar, 0.1),
    contacts_per_case_used = tidyr::replace_na(
      contacts_per_case_used,
      unique(hz_contacts$contacts_per_case_pooled)[1]
    ),
    cfr_used = tidyr::replace_na(cfr_used, mean(hz_cfr$pooled_cfr, na.rm = TRUE)),
    expected_secondary = estimated_true_cases_recent * contacts_per_case_used * sar,
    alert_case_threshold_C = beta_c * estimated_true_cases_recent,
    alert_death_threshold_C = beta_d * expected_deaths,
    # Model-derived uncertainty bands
    alert_case_threshold_lower_C = beta_c_low * estimated_true_cases_recent,
    alert_case_threshold_upper_C = beta_c_high * estimated_true_cases_recent,
    alert_death_threshold_lower_C = beta_d_low * expected_deaths,
    alert_death_threshold_upper_C = beta_d_high * expected_deaths
  ) |>
    dplyr::select(
      zone_sante_notification,
      threshold_time_key,
      threshold_valid_from,
      threshold_valid_to,
      dplyr::ends_with("_C"),
      estimated_true_cases_recent,
      beta_c,
      beta_c_low,
      beta_c_high,
      beta_d,
      beta_d_low,
      beta_d_high,
      detection_rate_adj,
      expected_secondary,
      contacts_per_case_used
    )

if (interactive()) {
  case_derived_thresholds.i |> filter(zone_sante_notification == "Bunia") |> View()
}

# 6. Phase 4: Threshold Synthesis
message("Phase 4: Threshold Synthesis...")

threshold_c_cols <- c(
  "alert_case_threshold_C",
  "alert_case_threshold_lower_C",
  "alert_case_threshold_upper_C",
  "alert_death_threshold_C",
  "alert_death_threshold_lower_C",
  "alert_death_threshold_upper_C"
)

# Build longitudinal synthesis: one row per threshold_time_key + zone_sante_notification
synthesis_hz <- pop |>
  dplyr::select(zone_sante_notification = HZ, Province, Population) |>
  dplyr::left_join(baseline_thresholds, by = c("zone_sante_notification", "Population")) |>
  dplyr::left_join(beni_thresholds, by = "zone_sante_notification") |>
  # Cross join with threshold windows
  tidyr::crossing(
    hz_recent_cases |>
      dplyr::select(
        threshold_time_key,
        threshold_valid_from,
        threshold_valid_to,
        recent_case_window_start,
        recent_case_window_end,
        recent_case_window_days,
        recent_case_anchor_date
      ) |>
      dplyr::distinct()
  ) |>
  # Join case-derived thresholds
  dplyr::left_join(
    case_derived_thresholds |> dplyr::select(
      zone_sante_notification, threshold_time_key,
      dplyr::any_of(threshold_c_cols)
    ),
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::mutate(
    Alert_case_lower = (alert_case_threshold_lower_B + tidyr::replace_na(alert_case_threshold_lower_C, 0)) / 2,
    Alert_case_upper = (alert_case_threshold_upper_B + tidyr::replace_na(alert_case_threshold_upper_C, 0)) / 2,
    Alert_death_lower = (death_threshold_lower_A + alert_death_threshold_lower_B + tidyr::replace_na(alert_death_threshold_lower_C, 0)) / 3,
    Alert_death_upper = (death_threshold_upper_A + alert_death_threshold_upper_B + tidyr::replace_na(alert_death_threshold_upper_C, 0)) / 3,
    Alert_case_threshold = (Alert_case_lower + Alert_case_upper) / 2,
    Alert_death_threshold = (Alert_death_lower + Alert_death_upper) / 2,
    week_start = recent_case_window_start
  ) |>
  dplyr::mutate(dplyr::across(where(is.double), ~round(.x, 0))) |>
  dplyr::select(-c(case_alerts, death_alerts))

# Compute threshold synthesis separately for the whole affected area. The
# ensemble excludes Approach B by design; that benchmark remains zone-specific.
synthesis_ensemble <- build_ensemble_thresholds(
  synthesis_hz,
  confirmed_notification_dates,
  case_derived_params = case_derived_thresholds
)
synthesis <- synthesis_hz

# Guard the two threshold scopes before they are exported as separate artifacts
stopifnot(all(
  synthesis$zone_sante_notification %in% conf.zs
))
stopifnot(
  !any(synthesis$zone_sante_notification == "Ensemble de la zone affectée"),
  all(
    synthesis_ensemble$zone_sante_notification ==
      "Ensemble de la zone affectée"
  ),
  setequal(
    unique(synthesis$threshold_time_key),
    unique(synthesis_ensemble$threshold_time_key)
  )
)

names(synthesis_hz)

thresholds.cols <- names(synthesis)[grepl("threshold|alert", names(synthesis), ignore.case = TRUE)]

synthesis.tab <- synthesis |>
  dplyr::select(Province, zone_sante_notification, Population, expected_weekly_deaths_cmr,
                dplyr::all_of(thresholds.cols))

if (interactive()) {
  synthesis |> filter(zone_sante_notification == "Bunia") |> View()
  synthesis_ensemble |> arrange(threshold_time_key) |> View()
}

case_derived_ensemble <- case_derived_thresholds |>
  dplyr::summarise(
    zone_sante_notification = "Ensemble de la zone affectée",
    threshold_valid_from = min(threshold_valid_from),
    threshold_valid_to = max(threshold_valid_to),
    alert_case_threshold_C = sum(alert_case_threshold_C, na.rm = TRUE),
    alert_case_threshold_lower_C = sum(alert_case_threshold_lower_C, na.rm = TRUE),
    alert_case_threshold_upper_C = sum(alert_case_threshold_upper_C, na.rm = TRUE),
    alert_death_threshold_C = sum(alert_death_threshold_C, na.rm = TRUE),
    alert_death_threshold_lower_C = sum(alert_death_threshold_lower_C, na.rm = TRUE),
    alert_death_threshold_upper_C = sum(alert_death_threshold_upper_C, na.rm = TRUE),
    estimated_true_cases_recent = sum(estimated_true_cases_recent, na.rm = TRUE),
    beta_c = mean(beta_c, na.rm = TRUE),
    beta_c_low = mean(beta_c_low, na.rm = TRUE),
    beta_c_high = mean(beta_c_high, na.rm = TRUE),
    beta_d = mean(beta_d, na.rm = TRUE),
    beta_d_low = mean(beta_d_low, na.rm = TRUE),
    beta_d_high = mean(beta_d_high, na.rm = TRUE),
    expected_secondary = sum(expected_secondary, na.rm = TRUE),
    contacts_per_case_used = mean(contacts_per_case_used, na.rm = TRUE),
    .by = threshold_time_key
  )

case_derived_params_all <- dplyr::bind_rows(case_derived_thresholds, case_derived_ensemble)

names(hz_detection_backcalc)
# Save intermediate parameters as a list
intermediate_params <- list(
  cfr = hz_cfr,
  cfr_backcalc = hz_cfr_backcalc,
  detection_rates = hz_detection,
  detection_backcalc = hz_detection_backcalc,
  growth_rates = hz_growth,
  recent_cases = hz_recent_cases,
  multipliers = hz_multipliers,
  alert_multiplier_weekly_data = attr(hz_multipliers, "weekly_model_data"),
  alert_multiplier_model_summary = attr(hz_multipliers, "model_summary"),
  rt_sar = hz_rt,
  contacts_per_case = hz_contacts,
  delays = hz_delays,
  baseline_cmr = cmr_province,
  evd10_hz_positivity = evd10_hz_pos,
  evd10_hz_rates = evd10_rates,
  evd10_best_performing_hzs = best_performing_hzs,
  case_derived_params = case_derived_params_all,
  evd_max_date = evd_max_date
)

# Save intermediate parameters
attr(intermediate_params, "evd_file_date") <- evd_file_date
attr(intermediate_params, "evd_snapshot_key") <- evd_snapshot_key
saveRDS(intermediate_params, file.path(output_dir, sprintf("01_intermediate_parameters_%s.rds", evd_file_date)))
saveRDS(intermediate_params, file.path(output_dir, "01_intermediate_parameters.rds"))

# Attach the DHIS2 tracker "as-of" date and EVD snapshot stamps so downstream
# consumers (trends, plots, Shiny) can show it in captions without reloading raw line list.
threshold_run_id <- format(Sys.time(), "%Y%m%dT%H%M%OS6")
attr(synthesis, "evd_max_date") <- evd_max_date
attr(synthesis_ensemble, "evd_max_date") <- evd_max_date
attr(synthesis, "evd_file_date") <- evd_file_date
attr(synthesis_ensemble, "evd_file_date") <- evd_file_date
attr(synthesis, "evd_snapshot_key") <- evd_snapshot_key
attr(synthesis_ensemble, "evd_snapshot_key") <- evd_snapshot_key
attr(synthesis, "threshold_run_id") <- threshold_run_id
attr(synthesis_ensemble, "threshold_run_id") <- threshold_run_id

saveRDS(synthesis, file.path(output_dir, sprintf("01_thresholds_synthesis_%s.rds", evd_file_date)))
saveRDS(synthesis, file.path(output_dir, "01_thresholds_synthesis.rds"))
saveRDS(synthesis_ensemble, file.path(output_dir, sprintf("01_thresholds_ensemble_%s.rds", evd_file_date)))
saveRDS(synthesis_ensemble, file.path(output_dir, "01_thresholds_ensemble.rds"))

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(synthesis, file.path(output_dir, sprintf("01_thresholds_synthesis_%s.xlsx", evd_file_date)))
  writexl::write_xlsx(synthesis, file.path(output_dir, "01_thresholds_synthesis.xlsx"))
  writexl::write_xlsx(synthesis.tab, file.path(output_dir, sprintf("synthesis_short_%s.xlsx", evd_file_date)))
  writexl::write_xlsx(synthesis.tab, file.path(output_dir, "synthesis_short.xlsx"))
}

episize_df <- dplyr::select(
  case_derived_thresholds.i,
  threshold_time_key, threshold_window_id,
  zone_sante_notification:detection_rate_backcalc_high,
  n_conf_valid_epilink:n_recent_confirmed, detection_rate_adj,
  estimated_true_cases_recent, estimated_true_cases_global,
  cumulative_confirmed_cases = n_detected
) |>
  dplyr::filter(threshold_time_key >= "2026-05-01")

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(episize_df, file.path(output_dir, sprintf("EpiSize_BDV_%s.xlsx", evd_file_date)))
  writexl::write_xlsx(episize_df, file.path(output_dir, "EpiSize_BDV.xlsx"))
}

message(
  "Threshold estimation complete for ",
  dplyr::n_distinct(synthesis$zone_sante_notification),
  " health zones. Output saved to ", output_dir
)
