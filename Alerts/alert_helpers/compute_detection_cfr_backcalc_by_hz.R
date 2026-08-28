#' Compute CFR back-calculation detection rate by health zone
#'
#' Implements `rho_T = N_T * CFR / (D_T * (1 + r / beta)^alpha)`.
#'
#' @param data Dataframe. EVD line list.
#' @param hz_cfr Dataframe from `compute_cfr_backcalc_by_hz()`.
#' @param hz_growth Dataframe from `compute_growth_rate_by_hz()`.
#' @param delay_mean Numeric. Mean onset-to-death delay in days.
#' @param delay_sd Numeric. Standard deviation of onset-to-death delay in days.
#' @param no_lab_values Character vector treated as no laboratory result
#'   available when counting eligible deaths.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param ref_date Date. Optional explicit nowcast reference date.
#' @param max_delay Integer. Nowcast horizon in days.
#' @param windows Dataframe. Optional shared time-window grid (distinct window
#'   metadata or a full HZ x window table such as `hz_recent_cases`). When
#'   supplied, counts are computed on trailing time-window data (default 21 days
#'   up to each window end) and `hz_cfr` / `hz_growth` are joined on
#'   `(zone_sante_notification, threshold_time_key)`.
#' @param lookback_days Integer. Number of days in the trailing rate window
#'   (default 21L).
#' @param case_date_col Character. Preferred onset date column.
#' @param fallback_date_col Character. Fallback onset date column.
#' @return Tibble with one row per health zone (and per time window when
#'   `windows` is supplied).
compute_detection_cfr_backcalc_by_hz <- function(
    data,
    hz_cfr,
    hz_growth,
    delay_mean = 11.37,
    delay_sd = 5.41,
    no_lab_values = c(""),
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
    "compute_detection_cfr_backcalc_by_hz()"
  )

  alpha <- (delay_mean / delay_sd)^2
  beta <- delay_mean / delay_sd^2

  if (is.null(windows)) {
    counts <- detection_backcalc_counts(data, no_lab_values = no_lab_values)
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
    hz_index <- detection_backcalc_hz_index(counts, hz_cfr, hz_growth)

    return(
      hz_index |>
        dplyr::left_join(
          counts,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          hz_cfr,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          hz_growth,
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
        detection_backcalc_finish(alpha = alpha, beta = beta)
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

      counts <- detection_backcalc_counts(
        subset,
        no_lab_values = no_lab_values
      )
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

      window_cfr <- hz_cfr
      if ("threshold_time_key" %in% names(hz_cfr)) {
        window_cfr <- hz_cfr |>
          dplyr::filter(threshold_time_key == w$threshold_time_key)
      }
      window_growth <- hz_growth
      if ("threshold_time_key" %in% names(hz_growth)) {
        window_growth <- hz_growth |>
          dplyr::filter(threshold_time_key == w$threshold_time_key)
      }

      hz_index |>
        dplyr::left_join(
          counts,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          window_cfr,
          by = dplyr::join_by(zone_sante_notification),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          window_growth,
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
        detection_backcalc_finish(alpha = alpha, beta = beta) |>
        alert_bind_window_metadata(w)
    }
  ) |>
    dplyr::arrange(threshold_valid_from, zone_sante_notification)
}

#' Per-zone confirmed / eligible-death counts for a data subset
#' @noRd
detection_backcalc_counts <- function(data, no_lab_values) {
  data |>
    dplyr::mutate(
      is_confirmed = alert_is_confirmed_case(data),
      is_eligible_death = alert_is_observed_death(data) &
        alert_has_non_negative_lab(data, no_lab_values = no_lab_values)
    ) |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::summarise(
      n_detected = sum(is_confirmed),
      n_observed_deaths = sum(is_eligible_death),
      .by = zone_sante_notification
    )
}

#' Union of health zones across counts, CFR and growth inputs
#' @noRd
detection_backcalc_hz_index <- function(counts, hz_cfr, hz_growth) {
  dplyr::bind_rows(
    counts |> dplyr::select(zone_sante_notification),
    hz_cfr |> dplyr::select(zone_sante_notification),
    hz_growth |> dplyr::select(zone_sante_notification)
  ) |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::distinct(zone_sante_notification)
}

#' Finish detection back-calculation: derived rates and status flags
#' @noRd
detection_backcalc_finish <- function(data, alpha, beta) {
  data |>
    dplyr::mutate(
      n_detected = tidyr::replace_na(n_detected, 0L),
      n_observed_deaths = tidyr::replace_na(n_observed_deaths, 0L),
      n_observed_deaths_nowcast = dplyr::if_else(
        is.finite(.data$delta_median_deaths),
        as.integer(round(.data$n_observed_deaths + .data$delta_median_deaths)),
        NA_integer_
      ),
      n_observed_deaths_nowcast_low = dplyr::if_else(
        is.finite(.data$delta_low_deaths),
        .data$n_observed_deaths + .data$delta_low_deaths,
        NA_real_
      ),
      n_observed_deaths_nowcast_high = dplyr::if_else(
        is.finite(.data$delta_high_deaths),
        .data$n_observed_deaths + .data$delta_high_deaths,
        NA_real_
      ),
      death_count = dplyr::coalesce(
        .data$n_observed_deaths_nowcast,
        .data$n_observed_deaths
      ),
      death_count_low = dplyr::coalesce(
        .data$n_observed_deaths_nowcast_low,
        .data$n_observed_deaths
      ),
      death_count_high = dplyr::coalesce(
        .data$n_observed_deaths_nowcast_high,
        .data$n_observed_deaths
      ),
      n_detected_nowcast = dplyr::if_else(
        is.finite(.data$delta_median),
        as.integer(round(.data$n_detected + .data$delta_median)),
        NA_integer_
      ),
      n_detected_nowcast_low = dplyr::if_else(
        is.finite(.data$delta_low),
        .data$n_detected + .data$delta_low,
        NA_real_
      ),
      n_detected_nowcast_high = dplyr::if_else(
        is.finite(.data$delta_high),
        .data$n_detected + .data$delta_high,
        NA_real_
      ),
      delay_shape = alpha,
      delay_rate = beta,
      delay_adjustment = compute_delay_adjustment(r_used, beta, alpha),
      delay_adjustment_low = compute_delay_adjustment(
        dplyr::coalesce(r_low_used, r_used),
        beta,
        alpha
      ),
      delay_adjustment_high = compute_delay_adjustment(
        dplyr::coalesce(r_high_used, r_used),
        beta,
        alpha
      ),
      estimated_true_cases = death_count * delay_adjustment / cfr_used,
      estimated_true_cases_low = death_count_low *
        delay_adjustment_low / cfr_high_used,
      estimated_true_cases_high = death_count_high *
        delay_adjustment_high / cfr_low_used,
      detection_rate_cfr_backcalc = n_detected / estimated_true_cases,
      detection_rate_low = n_detected * cfr_low_used /
        (death_count_high * delay_adjustment_high),
      detection_rate_high = n_detected * cfr_high_used /
        (death_count_low * delay_adjustment_low),
      detection_rate_cfr_backcalc_nowcast =
        n_detected_nowcast / estimated_true_cases,
      detection_rate_nowcast_low =
        n_detected_nowcast_low / estimated_true_cases,
      detection_rate_nowcast_high =
        n_detected_nowcast_high / estimated_true_cases,
      dplyr::across(
        c(
          estimated_true_cases,
          estimated_true_cases_low,
          estimated_true_cases_high,
          detection_rate_cfr_backcalc,
          detection_rate_low,
          detection_rate_high,
          detection_rate_cfr_backcalc_nowcast,
          detection_rate_nowcast_low,
          detection_rate_nowcast_high
        ),
        \(x) dplyr::if_else(is.finite(x), x, NA_real_)
      ),
      detection_rate_adj = detection_rate_cfr_backcalc,
      under_detection_rate = 1 - detection_rate_adj,
      zero_deaths = n_observed_deaths == 0,
      zero_deaths_nowcast = death_count == 0,
      missing_growth = !is.finite(r_used),
      missing_cfr = !is.finite(cfr_used) | cfr_used <= 0,
      invalid_delay_adjustment = !is.finite(delay_adjustment),
      detection_gt_one = detection_rate_cfr_backcalc > 1,
      detection_backcalc_status = dplyr::case_when(
        zero_deaths ~ "zero_deaths",
        missing_cfr ~ "missing_cfr",
        missing_growth ~ "missing_growth_rate",
        invalid_delay_adjustment ~ "invalid_delay_adjustment",
        detection_gt_one ~ "detection_gt_one",
        .default = "estimated"
      )
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "delta_median",
        "delta_low",
        "delta_high",
        "delta_median_deaths",
        "delta_low_deaths",
        "delta_high_deaths",
        "death_count",
        "death_count_low",
        "death_count_high"
      ))
    )
}

#' Compute delay adjustment term for CFR back-calculation
#' @noRd
compute_delay_adjustment <- function(r, beta, alpha) {
  r <- dplyr::if_else(is.finite(r) & r < 0, 0, r)
  adjustment_base <- 1 + r / beta
  adjustment <- adjustment_base^alpha

  dplyr::if_else(
    is.finite(adjustment) & adjustment_base > 0,
    adjustment,
    NA_real_
  )
}
