#' Compute Detection Rate per HZ (Epi-link method)
#'
#' Returns the case-detection-rate (CDR) per health zone using the epi-link
#' method: confirmed cases with a recorded epidemiological link to another
#' case, divided by confirmed cases with a valid epi-link field.
#'
#' Sparse health zones (< 5 confirmed cases with a valid epi-link field) fall
#' back to the pooled rate across all health zones. Non-finite or out-of-range
#' values are replaced with `NA_real_`.
#'
#' @param data Dataframe. Line list data.
#' @param windows Dataframe. Optional shared time-window grid (distinct window
#' When `windows` is supplied, the detection rate is estimated on trailing
#' time-window data (default 21 days up to each window end: resolved onset date
#' in `[window_end - lookback_days + 1, window_end]`), one row per health zone
#' per time window.
#'
#' @param data Dataframe. Line list data.
#' @param windows Dataframe. Optional shared time-window grid (distinct window
#'   metadata or a full HZ x window table such as `hz_recent_cases`).
#' @param lookback_days Integer. Number of days in the trailing rate window
#'   (default 21L).
#' @param case_date_col Character. Preferred onset date column.
#' @param fallback_date_col Character. Fallback onset date column.
#' @return Dataframe with one row per health zone (and per time window when
#'   `windows` is supplied) and columns:
#'   `zone_sante_notification`, `n_conf_valid_epilink`, `n_epilink`,
#'   `detection_rate_hz`, `pooled_detection`, `detection_rate_adj`,
#'   `under_detection_rate`.
compute_detection_by_hz <- function(data,
                                    windows = NULL,
                                    lookback_days = 21L,
                                    case_date_col = "alert_date_debut_symptoms",
                                    fallback_date_col = "s2_date_debut_signes_symptomes") {
  if (is.null(windows)) {
    return(detection_by_hz_estimate(data))
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

      hz_index |>
        dplyr::left_join(
          detection_by_hz_estimate(subset),
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          n_conf_valid_epilink = tidyr::replace_na(n_conf_valid_epilink, 0L),
          n_epilink = tidyr::replace_na(n_epilink, 0L),
          detection_rate_hz = dplyr::if_else(
            n_conf_valid_epilink > 0,
            n_epilink / n_conf_valid_epilink,
            NA_real_
          ),
          pooled_detection = dplyr::if_else(
            sum(n_conf_valid_epilink, na.rm = TRUE) > 0,
            sum(n_epilink, na.rm = TRUE) /
              sum(n_conf_valid_epilink, na.rm = TRUE),
            NA_real_
          ),
          # Zones without any valid epi-link field keep an NA rate (same as
          # the single-window behaviour); sparse zones (< 5 in time window) use the pooled.
          detection_rate_adj = dplyr::if_else(
            n_conf_valid_epilink > 0,
            dplyr::if_else(
              n_conf_valid_epilink < 5,
              pooled_detection,
              detection_rate_hz
            ),
            NA_real_
          ),
          under_detection_rate = 1 - detection_rate_adj
        ) |>
        alert_bind_window_metadata(w)
    }
  ) |>
    dplyr::arrange(threshold_valid_from, zone_sante_notification)
}

#' Single cumulative epi-link detection estimate per health zone
#' @noRd
detection_by_hz_estimate <- function(data) {
  conf <- data |> dplyr::filter(classification_finale == "Cas confirmé" | lab_resultat_final == "Positif")

  # --- Epi-link method (known-reported-infector) -----------------------------
  epilink_results <- conf |>
    dplyr::filter(!is.na(alert_lien_epidemiologic)) |>
    dplyr::summarise(
      n_conf_valid_epilink = dplyr::n(),
      n_epilink = sum(alert_lien_epidemiologic == "Oui"),
      .by = zone_sante_notification
    ) |>
    dplyr::mutate(
      detection_rate_hz = n_epilink / n_conf_valid_epilink,
      pooled_detection = sum(n_epilink, na.rm = TRUE) / sum(n_conf_valid_epilink, na.rm = TRUE),
      detection_rate_adj = dplyr::if_else(n_conf_valid_epilink < 5, pooled_detection, detection_rate_hz),
      under_detection_rate = 1 - detection_rate_adj
    )

  epilink_results
}
