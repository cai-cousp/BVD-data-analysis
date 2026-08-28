#' Combine detection-rate estimates by health zone
#'
#' Averages valid detection rates from CFR back-calculation and the epi-link
#' method, then recomputes derived metrics from the averaged rate.
#'
#' @param hz_detection_backcalc Dataframe from
#'   `compute_detection_cfr_backcalc_by_hz()`.
#' @param hz_detection_epilink Dataframe from `compute_detection_by_hz()`.
#' @param recent_cases Optional dataframe from
#'   `compute_recent_confirmed_windows_by_hz()`.
#' @return Tibble with one row per health zone.
combine_detection_estimates_by_hz <- function(hz_detection_backcalc,
                                              hz_detection_epilink,
                                              recent_cases = NULL) {
  alert_required_columns(
    hz_detection_backcalc,
    c("zone_sante_notification", "n_detected", "detection_rate_adj"),
    "combine_detection_estimates_by_hz()"
  )
  alert_required_columns(
    hz_detection_epilink,
    c("zone_sante_notification", "detection_rate_adj"),
    "combine_detection_estimates_by_hz()"
  )
  if (!is.null(recent_cases)) {
    alert_required_columns(
      recent_cases,
      c("zone_sante_notification", "n_recent_confirmed"),
      "combine_detection_estimates_by_hz()"
    )
  }

  backcalc <- hz_detection_backcalc |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::select(
      zone_sante_notification,
      dplyr::any_of("threshold_time_key"),
      n_detected,
      dplyr::any_of(c(
        "n_detected_nowcast",
        "n_detected_nowcast_low",
        "n_detected_nowcast_high"
      )),
      n_observed_deaths,
      dplyr::any_of(c(
        "n_observed_deaths_nowcast",
        "n_observed_deaths_nowcast_low",
        "n_observed_deaths_nowcast_high"
      )),
      cfr_used,
      r_used,
      detection_rate_backcalc_adj = detection_rate_adj,
      under_detection_backcalc = under_detection_rate,
      estimated_true_cases_backcalc = estimated_true_cases,
      estimated_true_cases_backcalc_low = estimated_true_cases_low,
      estimated_true_cases_backcalc_high = estimated_true_cases_high,
      detection_rate_backcalc_low = detection_rate_low,
      detection_rate_backcalc_high = detection_rate_high,
      detection_backcalc_status,
      dplyr::any_of(c(
        "zero_deaths",
        "missing_growth",
        "missing_cfr",
        "detection_gt_one",
        "pooled_cfr_used",
        "pooled_growth_used",
        "predicted_last_count"
      ))
    )

  epilink <- hz_detection_epilink |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::select(
      zone_sante_notification,
      dplyr::any_of("threshold_time_key"),
      n_conf_valid_epilink,
      n_epilink,
      detection_rate_epilink_adj = detection_rate_adj,
      under_detection_epilink = under_detection_rate
    )

  combined <- dplyr::full_join(
    backcalc,
    epilink,
    by = if (all(c("threshold_time_key") %in% names(backcalc)) &&
      all(c("threshold_time_key") %in% names(epilink))) {
      dplyr::join_by(zone_sante_notification, threshold_time_key)
    } else {
      dplyr::join_by(zone_sante_notification)
    },
    relationship = "one-to-one"
  )

  has_recent_cases <- !is.null(recent_cases)

  if (has_recent_cases) {
    recent_metadata_columns <- c(
      "threshold_time_key",
      "threshold_window_id",
      "threshold_window_index",
      "threshold_valid_from",
      "threshold_valid_to",
      "recent_case_window_start",
      "recent_case_window_end",
      "recent_case_window_days",
      "recent_case_anchor_date"
    )
    recent <- recent_cases |>
      dplyr::filter(!is.na(zone_sante_notification)) |>
      dplyr::select(
        zone_sante_notification,
        n_recent_confirmed,
        dplyr::any_of(c(
          "n_recent_confirmed_nowcast",
          "n_recent_confirmed_nowcast_low",
          "n_recent_confirmed_nowcast_high",
          "recent_count_source"
        )),
        dplyr::any_of(recent_metadata_columns)
      )

    combined <- dplyr::full_join(
      combined,
      recent,
      by = if ("threshold_time_key" %in% names(recent) &&
        "threshold_time_key" %in% names(combined)) {
        dplyr::join_by(zone_sante_notification, threshold_time_key)
      } else {
        dplyr::join_by(zone_sante_notification)
      },
      relationship = if ("threshold_time_key" %in% names(recent)) {
        if ("threshold_time_key" %in% names(combined)) {
          "one-to-one"
        } else {
          "one-to-many"
        }
      } else {
        "one-to-one"
      }
    )
  }

  if (!("predicted_last_count" %in% names(combined))) {
    combined$predicted_last_count <- NA_real_
  }
  if (!("n_recent_confirmed_nowcast" %in% names(combined))) {
    combined$n_recent_confirmed_nowcast <- NA_real_
  }
  if (!("n_detected_nowcast" %in% names(combined))) {
    combined$n_detected_nowcast <- NA_integer_
  }
  if (!("n_observed_deaths_nowcast" %in% names(combined))) {
    combined$n_observed_deaths_nowcast <- NA_real_
    combined$n_observed_deaths_nowcast_low <- NA_real_
    combined$n_observed_deaths_nowcast_high <- NA_real_
  }
  if (!("threshold_window_index" %in% names(combined))) {
    combined$is_most_recent_window <- TRUE
  } else {
    combined$is_most_recent_window <- combined |>
      dplyr::mutate(
        is_most_recent_window = .data$threshold_window_index ==
          max(.data$threshold_window_index, na.rm = TRUE),
        .by = zone_sante_notification
      ) |>
      dplyr::pull(is_most_recent_window)
  }

  combined |>
    dplyr::mutate(
      n_detected = dplyr::coalesce(n_detected, n_conf_valid_epilink),
      n_detected_recent = if (.env$has_recent_cases) {
        tidyr::replace_na(n_recent_confirmed, 0L)
      } else {
        n_detected
      },
      projected_recent_cases = dplyr::if_else(
        is_most_recent_window,
        dplyr::coalesce(
          as.double(n_recent_confirmed_nowcast),
          predicted_last_count,
          as.double(n_detected_recent)
        ),
        as.double(n_detected_recent)
      ),
      detection_rate_backcalc_valid = valid_detection_rate(
        detection_rate_backcalc_adj
      ),
      detection_rate_epilink_valid = valid_detection_rate(
        detection_rate_epilink_adj
      ),
      detection_rate_sources_used = rowSums(
        !is.na(dplyr::pick(
          detection_rate_backcalc_valid,
          detection_rate_epilink_valid
        ))
      ),
      detection_rate_adj = rowMeans(
        dplyr::pick(
          detection_rate_backcalc_valid,
          detection_rate_epilink_valid
        ),
        na.rm = TRUE
      ),
      detection_rate_adj = dplyr::if_else(
        is.nan(detection_rate_adj),
        NA_real_,
        detection_rate_adj
      ),
      under_detection_rate = 1 - detection_rate_adj,
      cumulative_confirmed_cases = n_detected,
      cumulative_confirmed_cases_nowcast = dplyr::coalesce(
        n_detected_nowcast,
        n_detected
      ),
      estimated_true_cases_recent = dplyr::if_else(
        projected_recent_cases == 0,
        0,
        projected_recent_cases / detection_rate_adj
      ),
      estimated_true_cases_recent = dplyr::if_else(
        is.finite(estimated_true_cases_recent),
        estimated_true_cases_recent,
        NA_real_
      ),
      estimated_true_cases_global = n_detected / detection_rate_adj,
      estimated_true_cases_global = dplyr::if_else(
        is.finite(estimated_true_cases_global),
        estimated_true_cases_global,
        NA_real_
      ),
      estimated_true_cases_backcalc_recent = dplyr::if_else(
        projected_recent_cases == 0,
        0,
        projected_recent_cases / detection_rate_backcalc_valid
      ),
      estimated_true_cases_backcalc_recent = dplyr::if_else(
        is.finite(estimated_true_cases_backcalc_recent),
        estimated_true_cases_backcalc_recent,
        NA_real_
      ),
      estimated_true_cases_epilink_recent = dplyr::if_else(
        projected_recent_cases == 0,
        0,
        projected_recent_cases / detection_rate_epilink_valid
      ),
      estimated_true_cases_epilink_recent = dplyr::if_else(
        is.finite(estimated_true_cases_epilink_recent),
        estimated_true_cases_epilink_recent,
        NA_real_
      ),
      estimated_true_cases_backcalc_global = n_detected / detection_rate_backcalc_valid,
      estimated_true_cases_backcalc_global = dplyr::if_else(
        is.finite(estimated_true_cases_backcalc_global),
        estimated_true_cases_backcalc_global,
        NA_real_
      ),
      estimated_true_cases_epilink_global = n_detected / detection_rate_epilink_valid,
      estimated_true_cases_epilink_global = dplyr::if_else(
        is.finite(estimated_true_cases_epilink_global),
        estimated_true_cases_epilink_global,
        NA_real_
      ),
      estimated_true_cases_basis = if (.env$has_recent_cases) {
        "recent_detected_cases"
      } else {
        "cumulative_detected_cases"
      },
      detection_rate_source_names = dplyr::case_when(
        !is.na(detection_rate_backcalc_valid) &
          !is.na(detection_rate_epilink_valid) ~
          "backcalc,epilink",
        !is.na(detection_rate_backcalc_valid) ~ "backcalc",
        !is.na(detection_rate_epilink_valid) ~ "epilink",
        .default = NA_character_
      ),
      detection_rate_combined_status = dplyr::case_when(
        detection_rate_sources_used == 2L ~ "averaged",
        detection_rate_sources_used == 1L ~ "single_source",
        .default = "no_valid_detection_rate"
      )
    ) |>
    dplyr::select(
      -detection_rate_backcalc_valid,
      -detection_rate_epilink_valid,
      -is_most_recent_window,
      -projected_recent_cases
    )
}

#' Keep only plausible detection rates for averaging
#' @noRd
valid_detection_rate <- function(rate) {
  dplyr::if_else(
    is.finite(rate) & rate > 0 & rate <= 1,
    rate,
    NA_real_
  )
}
