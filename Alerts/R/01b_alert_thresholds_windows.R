# Script 1b: Alert Threshold Estimation — Longitudinal (per HZ × time window)
#
# Same three-approach structure as 01_alert_thresholds.R, but Phase 3
# (Case-Derived Expectations) is computed per time window using cumulative
# data from the outbreak start up to each window end.  Phase 4 synthesis
# produces one row per HZ × time window.
#
# Time windows are the same backward-anchored, non-overlapping 7-day windows
# used elsewhere in this project (compute_recent_confirmed_windows_by_hz() /
# build_recent_case_windows()): each row carries threshold_time_key,
# threshold_valid_from/to, and recent_case_window_start/end.  The week-start
# date (week_start) is carried for the trend script and Shiny app.
#
# Approach A: Baseline Mortality (static, per HZ)
# Approach B: Beni Historical Benchmark (static, per HZ)
# Approach C: Case-Derived Expectations (cumulative, per HZ × time window)

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(MASS)
  library(EpiEstim)
})

# Load helpers
source(here::here("R/alert_helpers.R"))

# 1. Paths
data_folder <- file.path(evd17_root, "DataCleaning", "data", "Output")
beni_path   <- file.path(evd17_root, "DataAnalysis", "data", "Alerts", "Alert_Beni.rds")
pop_path    <- file.path(evd17_root, "DataAnalysis", "data", "PopulationParAge", "DRC_ZS_DHIS2_Pop_2024.xlsx")
output_dir  <- create_output_dir(here::here("output"))

# Population projection parameters (base: 2024 DHIS2)
pop_base_year   <- 2024L
pop_growth_rate <- 0.0129   # DRC annual growth rate (~1.29%)

# 2. Load Data
message("Loading data...")
evd  <- load_latest_evd(data_folder)

# Cleaned contact follow-up data (wide, one row per contact). Loaded through
# the same "latest file" mechanism because the Output folder is date-stamped.
contacts <- load_latest_evd(data_folder, pattern = "contact.clean_Int_.*\\.rds")

# Per-HZ EpiNow2 nowcasts (fitted once on the latest snapshot, cached so the
# daily re-run only refits when the line list changes).
message("Computing per-HZ EpiNow2 nowcasts...")
# ref_date defaults to the most recent notification date in the line list
# that is not later than the system date (see evd_notification_ref_date()).
latest_evd <- latest_evd_file(data_folder)
evd_snapshot_key <- basename(latest_evd)
evd_file_date <- extract_evd_date_stamp(evd_snapshot_key)

nowcasts <- compute_nowcasts_by_zone(
  evd,
  cache_path = file.path(output_dir, sprintf("05_nowcast_by_zone_%s.rds", evd_file_date)),
  snapshot_key = evd_snapshot_key
)
nowcasts_deaths <- compute_nowcasts_by_zone(
  evd,
  series = "confirmed_deaths",
  cache_path = file.path(output_dir, sprintf("05_nowcast_by_zone_deaths_%s.rds", evd_file_date)),
  snapshot_key = evd_snapshot_key
)

beni <- readRDS(beni_path)
pop  <- read_excel(pop_path) |>
  dplyr::filter(Province %in% c("Ituri", "Nord Kivu", "Nord-Kivu", "Sud Kivu", "Sud-Kivu")) |>
  dplyr::mutate(Province = stringr::str_replace(Province, "-", " "))

# Project 2024 population to current year using compound growth
current_year    <- as.integer(format(Sys.Date(), "%Y"))
years_elapsed   <- current_year - pop_base_year
growth_factor   <- (1 + pop_growth_rate)^years_elapsed

pop <- pop |>
  dplyr::mutate(Population = round(Population * growth_factor, 0))

# Analysis bounds
analysis_start_date <- as.Date("2026-01-01")

# 3. Phase 1: Baseline Mortality Thresholds (Approach A) — static per HZ
message("Phase 1: Baseline mortality thresholds...")

national.cmr <- 8.3 # per 1,000 per year (DRC average, WHO 2025)
cmr_province <- tibble::tribble(
  ~Province, ~cmr_per_1000_yr,
  "Ituri", national.cmr,
  "Nord Kivu", national.cmr,
  "Sud Kivu", national.cmr
)

baseline_thresholds <- pop |>
  dplyr::left_join(cmr_province, by = "Province") |>
  dplyr::mutate(
    expected_weekly_deaths_cmr  = expected_deaths_baseline(Population, cmr_per_1000_yr, 7),
    death_threshold_lower_A     = expected_weekly_deaths_cmr * 0.9,
    death_threshold_upper_A     = expected_weekly_deaths_cmr * 1.1
  ) |>
  dplyr::select(
    zone_sante_notification = HZ, Population,
    expected_weekly_deaths_cmr, death_threshold_lower_A, death_threshold_upper_A
  )

# 4. Phase 2: EVD10 Historical Benchmark (Approach B) — static per HZ
message("Phase 2: EVD10 Historical Benchmark (all HZs)...")

evd10        <- beni
evd10_hz_col <- "Zones Sante"

evd10_val <- evd10 |>
  dplyr::filter(Conclusion_finale %in% c("validée", "Validée")) |>
  dplyr::mutate(
    epiweek = compute_epiweek(Date_alerte),
    hz      = .data[[evd10_hz_col]]
  )

evd10_hz_pos <- evd10_val |>
  dplyr::summarise(
    n_val     = dplyr::n(),
    n_sampled = sum(!is.na(lab_result)),
    n_pos     = sum(lab_result == "Positif", na.rm = TRUE),
    .by = c(epiweek, hz)
  ) |>
  dplyr::filter(n_sampled > 0) |>
  dplyr::mutate(positivity_rate = n_pos / n_sampled)

evd10_optimal_weeks  <- evd10_hz_pos |> dplyr::filter(positivity_rate < 0.10)

evd10_optimal_alerts <- evd10_val |>
  dplyr::semi_join(evd10_optimal_weeks, by = c("epiweek", "hz")) |>
  dplyr::summarise(
    n_cases         = sum(Statut_initial %in% c("vivant", "Vivant", "VIVANT")),
    n_deaths        = sum(Statut_initial %in% c("décédé", "Décédé", "Décédée")),
    n_optimal_weeks = dplyr::n_distinct(epiweek),
    .by = hz
  )

pop_lookup <- pop |>
  dplyr::mutate(hz_match = stringr::str_to_title(tolower(HZ))) |>
  dplyr::select(hz_match, Population)

evd10_rates <- evd10_optimal_alerts |>
  dplyr::mutate(hz_match = stringr::str_to_title(tolower(hz))) |>
  dplyr::left_join(pop_lookup, by = "hz_match") |>
  dplyr::filter(!is.na(Population), n_optimal_weeks > 0) |>
  dplyr::mutate(
    case_rate_100k_wk  = (n_cases  / n_optimal_weeks) / Population * 1e5,
    death_rate_100k_wk = (n_deaths / n_optimal_weeks) / Population * 1e5
  )

evd10_hz_total <- evd10_val |> dplyr::count(hz, name = "total_validated")

best_performing_hzs <- evd10_rates |>
  dplyr::left_join(evd10_hz_total, by = "hz") |>
  dplyr::filter(total_validated >= 50) |>
  dplyr::filter(case_rate_100k_wk >= quantile(case_rate_100k_wk, 0.75, na.rm = TRUE))

benchmark_case        <- best_performing_hzs$case_rate_100k_wk
benchmark_case_lower  <- quantile(benchmark_case, 0.50, na.rm = TRUE)
benchmark_case_upper  <- quantile(benchmark_case, 0.75, na.rm = TRUE)
benchmark_death       <- best_performing_hzs$death_rate_100k_wk
benchmark_death_lower <- quantile(benchmark_death, 0.50, na.rm = TRUE)
benchmark_death_upper <- quantile(benchmark_death, 0.75, na.rm = TRUE)

beni_thresholds <- pop |>
  dplyr::mutate(
    alert_case_threshold_lower_B  = benchmark_case_lower  * Population / 1e5,
    alert_case_threshold_upper_B  = benchmark_case_upper  * Population / 1e5,
    alert_death_threshold_lower_B = benchmark_death_lower * Population / 1e5,
    alert_death_threshold_upper_B = benchmark_death_upper * Population / 1e5
  ) |>
  dplyr::select(zone_sante_notification = HZ, dplyr::ends_with("_B"))

# 5. Phase 3: Case-Derived Expectations (Approach C) — per HZ × time window
message("Phase 3: Case-Derived Expectations (per time window)...")

# Resolve a single onset date per row for cumulative filtering.
# Fallback is the S2 symptom-onset date, NOT the notification timestamp —
# consistent with the compute_recent_confirmed_*() helper defaults
# (alert_date_debut_symptoms -> s2_date_debut_signes_symptomes).
evd <- evd |>
  dplyr::mutate(
    resolved_date = dplyr::coalesce(
      as.Date(alert_date_debut_symptoms),
      as.Date(s2_date_debut_signes_symptomes)
    )
  )

# Rolling 7-day recent cases — one row per HZ × time window. The distinct
# window metadata doubles as the analysis time grid: the same backward-
# anchored, non-overlapping 7-day windows used everywhere else in this
# project (01_alert_thresholds.R, trends, Shiny).
hz_recent_cases <- compute_recent_confirmed_windows_by_hz(
  evd,
  window_days          = 7L,
  analysis_start_date  = analysis_start_date,
  analysis_end_date    = Sys.Date(),
  nowcast              = nowcasts
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

message("  Generating thresholds for ", nrow(window_grid), " time windows...")

val_alerts <- evd |> dplyr::filter(alert_conlusion %in% c("validée", "Validée"))

# Windowed indicator tables — one row per HZ x time window, estimated on
# cumulative data up to each window end and sharing the same grid as
# hz_recent_cases (threshold_time_key alignment).
message("  Computing windowed indicator tables...")

hz_cfr        <- compute_cfr_by_hz(
  evd,
  windows = window_grid,
  nowcast_deaths = nowcasts_deaths
)
hz_detection  <- compute_detection_by_hz(evd, windows = window_grid)
hz_cfr_bk     <- compute_cfr_backcalc_by_hz(
  evd,
  windows = window_grid,
  nowcast = nowcasts,
  nowcast_deaths = nowcasts_deaths
)
hz_growth     <- compute_growth_rate_by_hz(
  evd,
  windows = window_grid,
  nowcast = nowcasts
)
hz_detection_backcalc <- compute_detection_cfr_backcalc_by_hz(
  evd,
  hz_cfr_bk,
  hz_growth,
  windows = window_grid,
  nowcast = nowcasts,
  nowcast_deaths = nowcasts_deaths
)

# Rt per HZ x time window (trailing window ending at each window end)
hz_rt <- compute_rt_by_hz(evd, windows = window_grid, nowcast = nowcasts)

# Combined detection estimates (backcalc + epilink), keyed by HZ x window and
# carrying the recent-case counts from hz_recent_cases.
combined_all <- combine_detection_estimates_by_hz(
  hz_detection_backcalc,
  hz_detection,
  recent_cases = hz_recent_cases
)

# Per-window assembly loop — contacts, multipliers and threshold derivation.
# The indicator tables themselves are already windowed (computed once above).
case_derived_window <- purrr::map_dfr(
  seq_len(nrow(window_grid)),
  \(i) {
    w_key  <- window_grid$threshold_time_key[i]
    w_to   <- window_grid$threshold_valid_to[i]

    evd_cum <- evd |> dplyr::filter(resolved_date <= w_to)

    # Skip windows with too few cases
    n_cum_conf <- sum(
      alert_is_confirmed_case(evd_cum),
      na.rm = TRUE
    )
    if (n_cum_conf < 3L) {
      return(tibble::tibble())
    }

    # Cumulative contacts per case up to this window end. Skip windows
    # before any contact follow-up exists.
    contacts_cum <- contacts |>
      dplyr::filter(as.Date(enrolement_date) <= w_to)

    hz_contacts <- compute_contacts_per_case_by_hz(
      contacts_cum, evd_cum, error_if_none = FALSE, nowcast = nowcasts
    )
    if (nrow(hz_contacts) == 0L) {
      return(tibble::tibble())
    }

    # Rt for this window, with the window-specific contacts ratio attached
    # to derive SAR.
    hz_rt_window <- hz_rt |>
      dplyr::filter(threshold_time_key == w_key) |>
      dplyr::left_join(
        hz_contacts |>
          dplyr::select(zone_sante_notification, contacts_per_case_used),
        by = "zone_sante_notification",
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        sar = dplyr::if_else(
          is.finite(contacts_per_case_used) & contacts_per_case_used > 0,
          rt_used / contacts_per_case_used,
          NA_real_
        )
      )

    # Combined detection estimates for this window
    combined <- combined_all |>
      dplyr::filter(threshold_time_key == w_key)

    # Alert multipliers for this window.
    # Validated alerts are counted by date_heure_notification_alerte
    # (alert_date_col, unchanged). Symptom-onset resolution inside the
    # helper falls back from alert_date_debut_symptoms to
    # s2_date_debut_signes_symptomes (helper default).
    multipliers <- compute_alert_multipliers_by_window(
      val_alerts      = val_alerts,
      true_cases      = combined,
      hz_cfr          = hz_cfr |>
        dplyr::filter(threshold_time_key == w_key),
      evd             = evd_cum,
      min_model_weeks = 2L,
      use_full_window = TRUE,
      windows         = window_grid,
      nowcast         = nowcasts
    )

    multiplier_cols <- c(
      "zone_sante_notification", "threshold_time_key",
      "beta_c", "beta_c_low", "beta_c_high",
      "beta_d", "beta_d_low", "beta_d_high"
    )

    # Build per-window case-derived thresholds
    combined |>
      dplyr::left_join(
        multipliers |> dplyr::select(dplyr::any_of(multiplier_cols)),
        by = c("zone_sante_notification", "threshold_time_key")
      ) |>
      dplyr::left_join(
        hz_rt_window |>
          dplyr::select(
            zone_sante_notification,
            threshold_time_key,
            sar
          ),
        by = c("zone_sante_notification", "threshold_time_key")
      ) |>
      dplyr::left_join(
        hz_contacts |> dplyr::select(zone_sante_notification, contacts_per_case_used),
        by = "zone_sante_notification",
        relationship = "one-to-one"
      ) |>
      dplyr::rename(cfr_backcalc_used = cfr_used) |>
      dplyr::left_join(
        hz_cfr |>
          dplyr::filter(threshold_time_key == w_key) |>
          dplyr::select(zone_sante_notification, cfr_used),
        by = "zone_sante_notification",
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        expected_deaths = estimated_true_cases_recent * cfr_used,
        pooled_beta_c = sum(val_alerts$nature_alerte == "Vivant", na.rm = TRUE) / sum(estimated_true_cases_recent, na.rm = TRUE),
        pooled_beta_d = sum(val_alerts$nature_alerte == "Décédé", na.rm = TRUE) / sum(expected_deaths, na.rm = TRUE),
        beta_c          = dplyr::if_else(is.na(beta_c), pooled_beta_c, beta_c),
        beta_d          = dplyr::if_else(is.na(beta_d), pooled_beta_d, beta_d),
        beta_c_low      = dplyr::if_else(is.na(beta_c_low), pooled_beta_c, beta_c_low),
        beta_c_high     = dplyr::if_else(is.na(beta_c_high), pooled_beta_c, beta_c_high),
        beta_d_low      = dplyr::if_else(is.na(beta_d_low), pooled_beta_d, beta_d_low),
        beta_d_high     = dplyr::if_else(is.na(beta_d_high), pooled_beta_d, beta_d_high),
        sar             = tidyr::replace_na(sar, 0.1),
        contacts_per_case_used = tidyr::replace_na(
          contacts_per_case_used,
          unique(hz_contacts$contacts_per_case_pooled)
        ),
        cfr_used        = tidyr::replace_na(cfr_used, mean(hz_cfr$pooled_cfr, na.rm = TRUE)),
        expected_secondary = estimated_true_cases_recent * contacts_per_case_used * sar,
        alert_case_threshold_C = beta_c * estimated_true_cases_recent,
        alert_death_threshold_C = beta_d * expected_deaths,
        # Model-derived uncertainty bands
        alert_case_threshold_lower_C = beta_c_low * estimated_true_cases_recent,
        alert_case_threshold_upper_C = beta_c_high * estimated_true_cases_recent,
        alert_death_threshold_lower_C = beta_d_low  * expected_deaths,
        alert_death_threshold_upper_C = beta_d_high * expected_deaths
      )
  }
)

message("  Computed case-derived thresholds for ",
        nrow(case_derived_window), " HZ-window rows")

# 6. Phase 4: Threshold Synthesis (per HZ × time window)
message("Phase 4: Threshold Synthesis...")

threshold_c_cols <- c(
  "alert_case_threshold_C",
  "alert_case_threshold_lower_C",
  "alert_case_threshold_upper_C",
  "alert_death_threshold_C",
  "alert_death_threshold_lower_C",
  "alert_death_threshold_upper_C"
)

synthesis <- pop |>
  dplyr::select(zone_sante_notification = HZ, Province, Population) |>
  dplyr::left_join(baseline_thresholds, by = c("zone_sante_notification", "Population")) |>
  dplyr::left_join(beni_thresholds, by = "zone_sante_notification") |>
  # Cross join with the time-window grid
  dplyr::mutate(join_key = 1L) |>
  dplyr::left_join(
    window_grid |> dplyr::mutate(join_key = 1L),
    by = "join_key"
  ) |>
  dplyr::select(-join_key) |>
  # Join case-derived thresholds
  dplyr::left_join(
    case_derived_window |> dplyr::select(
      zone_sante_notification, threshold_time_key,
      dplyr::any_of(threshold_c_cols)
    ),
    by = c("zone_sante_notification", "threshold_time_key")
  ) |>
  dplyr::mutate(
    Alert_case_lower  = (alert_case_threshold_lower_B  + tidyr::replace_na(alert_case_threshold_lower_C,  0)) / 2,
    Alert_case_upper  = (alert_case_threshold_upper_B  + tidyr::replace_na(alert_case_threshold_upper_C,  0)) / 2,
    Alert_death_lower = (death_threshold_lower_A + alert_death_threshold_lower_B +
                           tidyr::replace_na(alert_death_threshold_lower_C, 0)) / 3,
    Alert_death_upper = (death_threshold_upper_A + alert_death_threshold_upper_B +
                           tidyr::replace_na(alert_death_threshold_upper_C, 0)) / 3,
    Alert_case_threshold  = (Alert_case_lower  + Alert_case_upper)  / 2,
    Alert_death_threshold = (Alert_death_lower + Alert_death_upper) / 2,
    week_start = recent_case_window_start
  ) |>
  dplyr::mutate(dplyr::across(where(is.double), ~round(.x, 0)))

# Save outputs
saveRDS(synthesis, file.path(output_dir, "01b_thresholds_synthesis.rds"))
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(synthesis, file.path(output_dir, "01b_thresholds_synthesis.xlsx"))
}

message("Longitudinal threshold estimation complete. Output saved to ", output_dir)
