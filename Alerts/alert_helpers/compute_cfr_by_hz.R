#' Compute CFR per health zone with pooled fallback
#'
#' When `windows` is supplied, CFR is estimated on trailing time-window data
#' (default 21 days up to each window end: resolved onset date in
#' `[window_end - lookback_days + 1, window_end]`), one row per health zone per
#' time window, sharing the window grid used by
#' `compute_recent_confirmed_windows_by_hz()`. Without `windows`, the original
#' single cumulative estimate per health zone is returned.
#'
#' @param data Dataframe. Line list data.
#' @param windows Dataframe. Optional shared time-window grid (distinct window
#'   metadata or a full HZ x window table such as `hz_recent_cases`).
#' @param lookback_days Integer. Number of days in the trailing rate window
#'   (default 21L).
#' @param case_date_col Character. Preferred onset date column.
#' @param fallback_date_col Character. Fallback onset date column.
#' @return Tibble with one row per health zone (and per time window when
#'   `windows` is supplied).
compute_cfr_by_hz <- function(data,
                              windows = NULL,
                              lookback_days = 21L,
                              case_date_col = "alert_date_debut_symptoms",
                              fallback_date_col = "s2_date_debut_signes_symptomes",
                              nowcast_deaths = NULL,
                              ref_date_deaths = NULL,
                              max_delay_deaths = 21L) {
  alert_required_columns(
    data,
    c(
      "zone_sante_notification",
      "classification_finale",
      "lab_resultat_final",
      "s6_statut_final_patient",
      "nature_alerte"
    ),
    "compute_cfr_by_hz()"
  )

  if (is.null(windows)) {
    death_delta <- nowcast_delta_for_data(
      nowcast_deaths,
      data,
      ref_date = ref_date_deaths,
      max_delay = max_delay_deaths,
      case_date_col = case_date_col,
      fallback_date_col = fallback_date_col
    ) |>
      dplyr::rename(
        delta_median_deaths = delta_median,
        delta_low_deaths = delta_low,
        delta_high_deaths = delta_high
      )

    return(
      cfr_by_hz_estimate(data) |>
        dplyr::left_join(
          death_delta,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        cfr_by_hz_finish_nowcast()
    )
  }

  windows_meta <- alert_window_grid(windows)
  hz_index <- alert_hz_index(data)
  resolved_date <- alert_resolve_case_date(
    data,
    case_date_col = case_date_col,
    fallback_date_col = fallback_date_col
  )

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
        dplyr::mutate(.alert_resolved_date = resolved_date) |>
        dplyr::filter(
          !is.na(.alert_resolved_date),
          .alert_resolved_date >= window_start,
          .alert_resolved_date <= window_end
        ) |>
        dplyr::select(-.alert_resolved_date)

      death_delta <- nowcast_delta_for_data(
        nowcast_deaths,
        subset,
        ref_date = ref_date_deaths,
        max_delay = max_delay_deaths,
        case_date_col = case_date_col,
        fallback_date_col = fallback_date_col,
        through_date = window_end
      ) |>
        dplyr::rename(
          delta_median_deaths = delta_median,
          delta_low_deaths = delta_low,
          delta_high_deaths = delta_high
        )

      hz_index |>
        dplyr::left_join(
          cfr_by_hz_estimate(subset),
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          death_delta,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          n_conf = tidyr::replace_na(n_conf, 0L),
          n_deaths = tidyr::replace_na(n_deaths, 0L),
          cfr_hz = dplyr::if_else(
            n_conf > 0,
            n_deaths / n_conf,
            NA_real_
          ),
          pooled_cfr = dplyr::if_else(
            sum(n_conf, na.rm = TRUE) > 0,
            sum(n_deaths, na.rm = TRUE) / sum(n_conf, na.rm = TRUE),
            NA_real_
          ),
          # Use pooled CFR if sparse (< 5 cases in the time window)
          cfr_used = dplyr::if_else(n_conf < 5, pooled_cfr, cfr_hz)
        ) |>
        cfr_by_hz_finish_nowcast() |>
        alert_bind_window_metadata(w)
    }
  ) |>
    dplyr::arrange(threshold_valid_from, zone_sante_notification)
}

#' Single cumulative CFR estimate per health zone
#' @noRd
cfr_by_hz_estimate <- function(data) {
  data |>
    dplyr::filter(classification_finale == "Cas confirmé" | lab_resultat_final == "Positif") |>
    dplyr::filter(s6_statut_final_patient %in% c("Décédé", "Vivant") | nature_alerte %in% c("Décédé", "Vivant")) |>
    dplyr::mutate(
      final_status = dplyr::case_when(
        s6_statut_final_patient == "Décédé" ~ "Décédé",
        nature_alerte == "Décédé" ~ "Décédé",
        s6_statut_final_patient == "Vivant" ~ "Vivant",
        nature_alerte == "Vivant" ~ "Vivant",
        .default = NA_character_
      )
    ) |>
    dplyr::filter(!is.na(final_status)) |>
    dplyr::summarise(
      n_conf = dplyr::n(),
      n_deaths = sum(final_status == "Décédé"),
      .by = zone_sante_notification
    ) |>
    dplyr::mutate(
      cfr_hz = dplyr::if_else(n_conf > 0, n_deaths / n_conf, NA_real_),
      pooled_cfr = sum(n_deaths, na.rm = TRUE) / sum(n_conf, na.rm = TRUE),
      # Use pooled CFR if sparse (< 5 cases)
      cfr_used = dplyr::if_else(n_conf < 5, pooled_cfr, cfr_hz)
    )
}

#' Add confirmed-death nowcast CFR columns
#' @noRd
cfr_by_hz_finish_nowcast <- function(cfr_by_hz) {
  cfr_by_hz |>
    dplyr::mutate(
      n_deaths_nowcast = dplyr::if_else(
        is.finite(.data$delta_median_deaths),
        .data$n_deaths + .data$delta_median_deaths,
        NA_real_
      ),
      cfr_hz_nowcast = dplyr::if_else(
        .data$n_conf > 0,
        .data$n_deaths_nowcast / .data$n_conf,
        NA_real_
      ),
      pooled_cfr_nowcast = dplyr::if_else(
        sum(.data$n_conf, na.rm = TRUE) > 0 &
          any(!is.na(.data$n_deaths_nowcast)),
        sum(.data$n_deaths_nowcast, na.rm = TRUE) /
          sum(.data$n_conf, na.rm = TRUE),
        NA_real_
      ),
      cfr_used_nowcast = dplyr::if_else(
        .data$n_conf < 5,
        .data$pooled_cfr_nowcast,
        .data$cfr_hz_nowcast
      )
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "delta_median_deaths",
        "delta_low_deaths",
        "delta_high_deaths"
      ))
    )
}
