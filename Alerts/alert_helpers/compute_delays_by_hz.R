# Compute notification timeliness and reporting delays per health zone
#
# Characterises operational surveillance reactivity by measuring the delay
# from symptom onset (or community death) to alert notification.
# Supports single-estimate or windowed trailing evaluation.

#' Compute reporting delays and timeliness metrics per health zone
#'
#' @param data Dataframe. Cleaned EVD line list.
#' @param windows Dataframe. Optional analysis grid windows.
#' @param lookback_days Integer. Lookback window in days (default 21L).
#' @param notification_col Character. Preferred notification date column.
#' @param case_date_col Character. Preferred onset date column.
#' @param fallback_date_col Character. Fallback onset date column.
#' @param min_cases_hz Integer. Minimum cases required before using zone-specific median (default 5L).
#' @param min_deaths_hz Integer. Minimum deaths required before using zone-specific death median (default 3L).
#'
#' @return Tibble with one row per health zone (and per time window if `windows` is provided).
#' @export
compute_delays_by_hz <- function(data,
                                 windows = NULL,
                                 lookback_days = 21L,
                                 notification_col = "date_heure_notification_alerte",
                                 case_date_col = "alert_date_debut_symptoms",
                                 fallback_date_col = "s2_date_debut_signes_symptomes",
                                 min_cases_hz = 5L,
                                 min_deaths_hz = 3L) {
  alert_required_columns(
    data,
    c("zone_sante_notification", "classification_finale", "lab_resultat_final"),
    "compute_delays_by_hz()"
  )

  if (is.null(windows)) {
    return(delays_by_hz_estimate(
      data = data,
      notification_col = notification_col,
      case_date_col = case_date_col,
      fallback_date_col = fallback_date_col,
      min_cases_hz = min_cases_hz,
      min_deaths_hz = min_deaths_hz
    ))
  }

  windows_meta <- alert_window_grid(windows)
  hz_index <- alert_hz_index(data)
  resolved_notif_date <- alert_resolve_notification_date(data, notification_col = notification_col)
  resolved_case_date <- alert_resolve_case_date(data, case_date_col = case_date_col, fallback_date_col = fallback_date_col)

  # Anchor trailing window on notification date (operational window)
  # with fallback to case date if notification date is missing
  anchor_date <- dplyr::coalesce(resolved_notif_date, resolved_case_date)

  purrr::map_dfr(
    seq_len(nrow(windows_meta)),
    \(i) {
      w <- windows_meta[i, ]
      window_end <- w$recent_case_window_end
      window_start <- if (is.null(lookback_days) || is.na(lookback_days)) {
        as.Date("1900-01-01")
      } else {
        window_end - as.integer(lookback_days) + 1L
      }

      subset <- data |>
        dplyr::mutate(.alert_anchor_date = anchor_date) |>
        dplyr::filter(
          !is.na(.alert_anchor_date),
          .alert_anchor_date >= window_start,
          .alert_anchor_date <= window_end
        ) |>
        dplyr::select(-.alert_anchor_date)

      hz_index |>
        dplyr::left_join(
          delays_by_hz_estimate(
            data = subset,
            notification_col = notification_col,
            case_date_col = case_date_col,
            fallback_date_col = fallback_date_col,
            min_cases_hz = min_cases_hz,
            min_deaths_hz = min_deaths_hz
          ),
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          n_cases_with_delay = tidyr::replace_na(n_cases_with_delay, 0L),
          n_deaths_with_delay = tidyr::replace_na(n_deaths_with_delay, 0L)
        ) |>
        alert_bind_window_metadata(w)
    }
  ) |>
    dplyr::arrange(threshold_valid_from, zone_sante_notification)
}

delays_by_hz_estimate <- function(data,
                                  notification_col = "date_heure_notification_alerte",
                                  case_date_col = "alert_date_debut_symptoms",
                                  fallback_date_col = "s2_date_debut_signes_symptomes",
                                  min_cases_hz = 5L,
                                  min_deaths_hz = 3L) {
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      zone_sante_notification = character(),
      n_cases_with_delay = integer(),
      median_delay_onset_to_notif = double(),
      prop_notif_24h = double(),
      prop_notif_48h = double(),
      pooled_median_delay_onset_to_notif = double(),
      pooled_prop_notif_48h = double(),
      median_delay_used = double(),
      prop_notif_48h_used = double(),
      n_deaths_with_delay = integer(),
      median_delay_death_to_notif = double(),
      pooled_median_delay_death_to_notif = double(),
      median_death_delay_used = double()
    ))
  }

  notif_date <- alert_resolve_notification_date(data, notification_col = notification_col)
  onset_date <- alert_resolve_case_date(data, case_date_col = case_date_col, fallback_date_col = fallback_date_col)
  is_confirmed <- alert_is_confirmed_case(data)
  death_date_cols <- c("s6_date_deces", "date_de_deces")
  death_dates <- list()
  for (col in death_date_cols) {
    if (col %in% names(data)) {
      death_dates[[length(death_dates) + 1L]] <- as.Date(data[[col]])
    }
  }
  death_date <- if (length(death_dates) > 0L) {
    purrr::reduce(death_dates, dplyr::coalesce)
  } else {
    as.Date(rep(NA, nrow(data)))
  }

  is_dead <- if (all(c("classification_finale", "lab_resultat_final", "s6_statut_final_patient",
                       "s5_statut_patient_lors_prelev", "nature_alerte", "date_de_deces", "s6_date_deces") %in% names(data))) {
    alert_is_dead(data)
  } else {
    alert_status <- if ("nature_alerte" %in% names(data)) as.character(data$nature_alerte) else ""
    final_status <- if ("s6_statut_final_patient" %in% names(data)) as.character(data$s6_statut_final_patient) else ""
    alert_status == "Décédé" | final_status == "Décédé" | !is.na(death_date)
  }

  case_delays <- tibble::tibble(
    zone_sante_notification = data$zone_sante_notification,
    is_confirmed = is_confirmed,
    is_dead = is_dead,
    delay_onset = as.numeric(notif_date - onset_date),
    delay_death = as.numeric(notif_date - death_date)
  ) |>
    dplyr::filter(!is.na(zone_sante_notification))

  # Confirmed cases onset delay (valid non-negative delays <= 60 days)
  valid_case_delays <- case_delays |>
    dplyr::filter(
      is_confirmed,
      !is.na(delay_onset),
      delay_onset >= 0,
      delay_onset <= 60
    )

  # Confirmed deaths delay (valid non-negative delays <= 60 days)
  valid_death_delays <- case_delays |>
    dplyr::filter(
      is_confirmed,
      is_dead,
      !is.na(delay_death),
      delay_death >= 0,
      delay_death <= 60
    )

  # Pooled metrics
  pooled_median_onset <- if (nrow(valid_case_delays) > 0L) {
    stats::median(valid_case_delays$delay_onset, na.rm = TRUE)
  } else {
    NA_real_
  }

  pooled_prop_24h <- if (nrow(valid_case_delays) > 0L) {
    mean(valid_case_delays$delay_onset <= 1, na.rm = TRUE)
  } else {
    NA_real_
  }

  pooled_prop_48h <- if (nrow(valid_case_delays) > 0L) {
    mean(valid_case_delays$delay_onset <= 2, na.rm = TRUE)
  } else {
    NA_real_
  }

  pooled_median_death <- if (nrow(valid_death_delays) > 0L) {
    stats::median(valid_death_delays$delay_death, na.rm = TRUE)
  } else {
    NA_real_
  }

  hz_case_summary <- valid_case_delays |>
    dplyr::summarise(
      n_cases_with_delay = dplyr::n(),
      median_delay_onset_to_notif = stats::median(delay_onset, na.rm = TRUE),
      prop_notif_24h = mean(delay_onset <= 1, na.rm = TRUE),
      prop_notif_48h = mean(delay_onset <= 2, na.rm = TRUE),
      .by = zone_sante_notification
    )

  hz_death_summary <- valid_death_delays |>
    dplyr::summarise(
      n_deaths_with_delay = dplyr::n(),
      median_delay_death_to_notif = stats::median(delay_death, na.rm = TRUE),
      .by = zone_sante_notification
    )

  all_hzs <- alert_hz_index(data)

  all_hzs |>
    dplyr::left_join(hz_case_summary, by = "zone_sante_notification") |>
    dplyr::left_join(hz_death_summary, by = "zone_sante_notification") |>
    dplyr::mutate(
      n_cases_with_delay = tidyr::replace_na(n_cases_with_delay, 0L),
      n_deaths_with_delay = tidyr::replace_na(n_deaths_with_delay, 0L),
      pooled_median_delay_onset_to_notif = pooled_median_onset,
      pooled_prop_notif_48h = pooled_prop_48h,
      pooled_median_delay_death_to_notif = pooled_median_death,
      median_delay_used = dplyr::case_when(
        n_cases_with_delay >= min_cases_hz ~ median_delay_onset_to_notif,
        !is.na(pooled_median_onset) ~ pooled_median_onset,
        .default = NA_real_
      ),
      prop_notif_48h_used = dplyr::case_when(
        n_cases_with_delay >= min_cases_hz ~ prop_notif_48h,
        !is.na(pooled_prop_48h) ~ pooled_prop_48h,
        .default = NA_real_
      ),
      median_death_delay_used = dplyr::case_when(
        n_deaths_with_delay >= min_deaths_hz ~ median_delay_death_to_notif,
        !is.na(pooled_median_death) ~ pooled_median_death,
        .default = NA_real_
      )
    )
}
