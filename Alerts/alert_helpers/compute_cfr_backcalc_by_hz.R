#' Compute CFR for death back-calculation by health zone
#'
#' CFR follows the project-specific definition in `Alerts/cfr.methods.md`:
#' eligible deaths with positive or unavailable laboratory result divided by
#' confirmed cases plus eligible deaths that are not already confirmed.
#'
#' @param data Dataframe. EVD line list.
#' @param min_denominator Integer. HZ denominators below this value use the
#'   pooled CFR.
#' @param no_lab_values Character vector treated as no laboratory result
#'   available. Missing values are always treated as no laboratory result.
#' @param conf_level Numeric. Confidence level for exact binomial intervals.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param ref_date Date. Optional explicit nowcast reference date.
#' @param max_delay Integer. Nowcast horizon in days.
#' @param ref_date Date. Optional explicit nowcast reference date.
#' @param max_delay Integer. Nowcast horizon in days.
#' @param windows Dataframe. Optional shared time-window grid (distinct window
#'   metadata or a full HZ x window table such as `hz_recent_cases`). When
#'   supplied, CFR is estimated on trailing time-window data (default 21 days up
#'   to each window end), one row per health zone per time window.
#' @param lookback_days Integer. Number of days in the trailing rate window
#'   (default 21L).
#' @param case_date_col Character. Preferred onset date column.
#' @param fallback_date_col Character. Fallback onset date column.
#' @return Tibble with one row per health zone (and per time window when
#'   `windows` is supplied).
compute_cfr_backcalc_by_hz <- function(data,
                                       min_denominator = 5L,
                                       no_lab_values = c(""),
                                       conf_level = 0.95,
                                       nowcast = NULL,
                                       nowcast_deaths = NULL,
                                       ref_date = NULL,
                                       ref_date_deaths = NULL,
                                       max_delay = 21L,
                                       max_delay_deaths = 21L,
                                       windows = NULL,
                                       lookback_days = 21L,
                                       case_date_col = "alert_date_debut_symptoms",
                                       fallback_date_col = "s2_date_debut_signes_symptomes") {
  alert_required_columns(
    data,
    c(
      "zone_sante_notification",
      "classification_finale",
      "lab_resultat_final",
      "nature_alerte",
      "s6_statut_final_patient"
    ),
    "compute_cfr_backcalc_by_hz()"
  )

  if (is.null(windows)) {
    cfr_by_hz <- cfr_backcalc_counts(data, no_lab_values = no_lab_values)
    nc_delta <- nowcast_delta_for_data(
      nowcast,
      data,
      ref_date = ref_date,
      max_delay = max_delay,
      case_date_col = case_date_col,
      fallback_date_col = fallback_date_col
    )
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
      cfr_by_hz |>
        dplyr::left_join(
          nc_delta,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          death_delta,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        cfr_backcalc_finish(
          min_denominator = min_denominator,
          conf_level = conf_level
        )
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

      counts <- cfr_backcalc_counts(subset, no_lab_values = no_lab_values)
      nc_delta <- nowcast_delta_for_data(
        nowcast,
        subset,
        ref_date = ref_date,
        max_delay = max_delay,
        case_date_col = case_date_col,
        fallback_date_col = fallback_date_col,
        through_date = window_end
      )
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
          counts,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          nc_delta,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          death_delta,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          n_confirmed = tidyr::replace_na(n_confirmed, 0L),
          n_deaths_eligible = tidyr::replace_na(n_deaths_eligible, 0L),
          n_deaths_not_confirmed = tidyr::replace_na(
            n_deaths_not_confirmed,
            0L
          ),
          cfr_denominator = n_confirmed + n_deaths_not_confirmed,
          cfr_hz = alert_valid_ratio(n_deaths_eligible, cfr_denominator)
        ) |>
        cfr_backcalc_finish(
          min_denominator = min_denominator,
          conf_level = conf_level
        ) |>
        alert_bind_window_metadata(w)
    }
  ) |>
    dplyr::arrange(threshold_valid_from, zone_sante_notification)
}

#' Per-zone CFR back-calculation counts for a data subset
#' @noRd
cfr_backcalc_counts <- function(data, no_lab_values) {
  cfr_data <- data |>
    dplyr::mutate(
      is_confirmed = alert_is_confirmed_case(data),
      is_observed_death = alert_is_observed_death(data),
      has_non_negative_lab = alert_has_non_negative_lab(
        data,
        no_lab_values = no_lab_values
      ),
      is_eligible_death = is_observed_death & has_non_negative_lab,
      is_eligible_unconfirmed_death = is_eligible_death & !is_confirmed
    )

  cfr_by_hz <- cfr_data |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::summarise(
      n_confirmed = sum(is_confirmed),
      n_deaths_eligible = sum(is_eligible_death),
      n_deaths_not_confirmed = sum(is_eligible_unconfirmed_death),
      cfr_denominator = n_confirmed + n_deaths_not_confirmed,
      cfr_hz = alert_valid_ratio(n_deaths_eligible, cfr_denominator),
      .by = zone_sante_notification
    )
}

#' Finish CFR back-calculation: nowcast counts, pooled fallback and CIs
#' @noRd
cfr_backcalc_finish <- function(cfr_by_hz,
                                min_denominator,
                                conf_level) {
  pooled_deaths <- sum(cfr_by_hz$n_deaths_eligible, na.rm = TRUE)
  pooled_denominator <- sum(cfr_by_hz$cfr_denominator, na.rm = TRUE)
  pooled_cfr <- alert_valid_ratio(pooled_deaths, pooled_denominator)
  pooled_ci <- alert_binom_ci(
    pooled_deaths,
    pooled_denominator,
    level = conf_level
  )

  cfr_by_hz <- cfr_by_hz |>
    dplyr::mutate(
      n_deaths_eligible_nowcast = dplyr::if_else(
        is.finite(.data$delta_median_deaths),
        .data$n_deaths_eligible + .data$delta_median_deaths,
        NA_real_
      )
    )

  pooled_deaths_nowcast <- if (all(is.na(cfr_by_hz$n_deaths_eligible_nowcast))) {
    NA_real_
  } else {
    sum(cfr_by_hz$n_deaths_eligible_nowcast, na.rm = TRUE)
  }
  pooled_cfr_nowcast <- alert_valid_ratio(
    pooled_deaths_nowcast,
    pooled_denominator
  )
  pooled_ci_nowcast <- alert_binom_ci(
    pooled_deaths_nowcast,
    pooled_denominator,
    level = conf_level
  )

  cfr_by_hz |>
    dplyr::mutate(
      n_confirmed_nowcast = dplyr::if_else(
        is.finite(.data$delta_median),
        as.integer(round(.data$n_confirmed + .data$delta_median)),
        NA_integer_
      ),
      n_confirmed_nowcast_low = dplyr::if_else(
        is.finite(.data$delta_low),
        .data$n_confirmed + .data$delta_low,
        NA_real_
      ),
      n_confirmed_nowcast_high = dplyr::if_else(
        is.finite(.data$delta_high),
        .data$n_confirmed + .data$delta_high,
        NA_real_
      )
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "delta_median",
        "delta_low",
        "delta_high",
        "delta_median_deaths",
        "delta_low_deaths",
        "delta_high_deaths"
      ))
    ) |>
    dplyr::mutate(
      cfr_ci = purrr::map2(
        n_deaths_eligible,
        cfr_denominator,
        alert_binom_ci,
        level = conf_level
      ),
      cfr_low = purrr::map_dbl(cfr_ci, "low"),
      cfr_high = purrr::map_dbl(cfr_ci, "high"),
      pooled_cfr = pooled_cfr,
      pooled_cfr_low = pooled_ci[["low"]],
      pooled_cfr_high = pooled_ci[["high"]],
      cfr_fallback_reason = dplyr::case_when(
        cfr_denominator < min_denominator ~ "sparse_denominator",
        !is.finite(cfr_hz) ~ "non_finite_cfr",
        .default = NA_character_
      ),
      pooled_cfr_used = !is.na(cfr_fallback_reason),
      cfr_used = dplyr::if_else(pooled_cfr_used, pooled_cfr, cfr_hz),
      cfr_low_used = dplyr::if_else(
        pooled_cfr_used,
        pooled_cfr_low,
        cfr_low
      ),
      cfr_high_used = dplyr::if_else(
        pooled_cfr_used,
        pooled_cfr_high,
        cfr_high
      ),
      cfr_hz_nowcast = alert_valid_ratio(
        n_deaths_eligible_nowcast,
        cfr_denominator
      ),
      cfr_ci_nowcast = purrr::map2(
        n_deaths_eligible_nowcast,
        cfr_denominator,
        alert_binom_ci,
        level = conf_level
      ),
      cfr_low_nowcast = purrr::map_dbl(cfr_ci_nowcast, "low"),
      cfr_high_nowcast = purrr::map_dbl(cfr_ci_nowcast, "high"),
      pooled_cfr_low_nowcast = pooled_ci_nowcast[["low"]],
      pooled_cfr_high_nowcast = pooled_ci_nowcast[["high"]],
      cfr_fallback_reason_nowcast = dplyr::case_when(
        cfr_denominator < min_denominator ~ "sparse_denominator",
        !is.finite(cfr_hz_nowcast) ~ "non_finite_cfr",
        .default = NA_character_
      ),
      pooled_cfr_used_nowcast = !is.na(cfr_fallback_reason_nowcast),
      cfr_used_nowcast = dplyr::if_else(
        pooled_cfr_used_nowcast,
        pooled_cfr_nowcast,
        cfr_hz_nowcast
      ),
      cfr_low_used_nowcast = dplyr::if_else(
        pooled_cfr_used_nowcast,
        pooled_cfr_low_nowcast,
        cfr_low_nowcast
      ),
      cfr_high_used_nowcast = dplyr::if_else(
        pooled_cfr_used_nowcast,
        pooled_cfr_high_nowcast,
        cfr_high_nowcast
      )
    ) |>
    dplyr::select(-cfr_ci, -cfr_ci_nowcast)
}
