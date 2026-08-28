#' Compute Case and Death Alert Multipliers (βc and βd)
#'
#' `beta_c` and `beta_d` are estimated within each health zone from
#' HZ-week-level Poisson offset models. Weekly true-case exposure is derived
#' by allocating each HZ's total estimated true cases across weeks in
#' proportion to detected confirmed/positive cases.
#'
#' @param val_alerts Dataframe. Validated alerts.
#' @param true_cases Dataframe. Estimated true cases per HZ.
#' @param hz_cfr Dataframe. Case fatality rates per HZ.
#' @param evd Dataframe. Full EVD line list used to derive weekly exposure.
#' @param alert_date_col Character. Date column for validated alert counts.
#' @param case_date_col Character. Preferred date column for confirmed cases.
#' @param min_model_weeks Integer. Minimum positive-exposure weeks required
#'   before fitting a Poisson offset model.
#' @param confidence_level Numeric. Confidence level for model intervals.
#' @param windows Dataframe. Optional analysis grid (backward-anchored 7-day
#'   windows, as produced by `build_recent_case_windows()`). When provided,
#'   the weekly model data is binned onto these grid windows instead of
#'   Monday calendar weeks, keeping the model time axis aligned with the
#'   threshold grid. When `NULL`, windows are derived from `true_cases` when
#'   it carries window columns; otherwise Monday calendar weeks are used as a
#'   legacy fallback.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param ref_date Date. Optional explicit nowcast reference date.
#' @param max_delay Integer. Nowcast horizon in days.
#' @param overdispersion_threshold Numeric. Dispersion ratio
#'   (deviance / residual df) above which the Poisson fit is treated as
#'   overdispersed and refitted with a negative-binomial GLM instead of the
#'   quasi-Poisson standard-error correction.
#' @return Tibble with one row per HZ. Attributes `weekly_model_data` and
#'   `model_summary` contain diagnostics.
compute_alert_multipliers <- function(
    val_alerts,
    true_cases,
    hz_cfr,
    evd = val_alerts,
    alert_date_col = "date_heure_notification_alerte",
    case_date_col = "alert_date_debut_symptoms",
    fallback_date_col = "s2_date_debut_signes_symptomes",
    min_model_weeks = 2L,
    confidence_level = 0.95,
    use_full_window = FALSE,
    windows = NULL,
    nowcast = NULL,
    ref_date = NULL,
    max_delay = 21L,
    overdispersion_threshold = 1.5,
    lag_to_notification = TRUE) {
  alert_required_columns(
    val_alerts,
    c("zone_sante_notification", "nature_alerte", alert_date_col),
    "compute_alert_multipliers()"
  )
  alert_required_columns(
    true_cases,
    c("zone_sante_notification", "estimated_true_cases_recent"),
    "compute_alert_multipliers()"
  )
  alert_required_columns(
    hz_cfr,
    c("zone_sante_notification", "cfr_used"),
    "compute_alert_multipliers()"
  )
  alert_required_columns(
    evd,
    c(
      "zone_sante_notification",
      "classification_finale",
      "lab_resultat_final"
    ),
    "compute_alert_multipliers()"
  )

  if (!any(c(case_date_col, fallback_date_col) %in% names(evd))) {
    rlang::abort(c(
      "compute_alert_multipliers() requires a case date column.",
      x = paste(c(case_date_col, fallback_date_col), collapse = ", ")
    ))
  }

  hz_parameters <- true_cases |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::select(
      zone_sante_notification,
      estimated_true_cases_recent,
      dplyr::any_of(c(
        "threshold_time_key",
        "recent_case_window_start",
        "recent_case_window_end"
      ))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("recent_case_window_start", "recent_case_window_end")),
        as.Date
      )
    )

  # Resolve the analysis grid used to bin the weekly model data (harmonized
  # time axis). An explicit `windows` grid wins; otherwise derive the grid
  # from the window columns carried by `true_cases`. When neither is
  # available (legacy/static calls) the model falls back to Monday calendar
  # weeks via bin_weekly_events().
  if (is.null(windows)) {
    windows <- derive_window_grid(hz_parameters)
  }

  cfr_join_by <- if (
    all(c("threshold_time_key") %in% names(hz_parameters)) &&
      all(c("threshold_time_key") %in% names(hz_cfr))
  ) {
    dplyr::join_by(zone_sante_notification, threshold_time_key)
  } else {
    dplyr::join_by(zone_sante_notification)
  }

  hz_parameters <- hz_parameters |>
    dplyr::full_join(
      hz_cfr |>
        dplyr::filter(!is.na(zone_sante_notification)) |>
        dplyr::select(
          zone_sante_notification,
          dplyr::any_of(c("threshold_time_key", "cfr_used"))
        ),
      by = cfr_join_by,
      relationship = "one-to-one"
    )

  confirmed_weekly <- evd |>
    dplyr::mutate(
      onset_date = alert_resolve_case_date(
        evd,
        case_date_col = case_date_col,
        fallback_date_col = fallback_date_col
      ),
      notif_date = alert_resolve_notification_date(
        evd,
        notification_col = alert_date_col
      ),
      case_date = if (isTRUE(lag_to_notification)) {
        dplyr::coalesce(notif_date, onset_date)
      } else {
        onset_date
      },
      is_confirmed = alert_is_confirmed_case(evd)
    ) |>
    dplyr::filter(
      is_confirmed,
      !is.na(zone_sante_notification),
      !is.na(case_date)
    )

  if (!use_full_window && is.null(windows)) {
    confirmed_weekly <- confirmed_weekly |>
      filter_dated_to_true_case_window(
        hz_parameters = hz_parameters,
        date_col = "case_date"
      )
  }

  # Grid-path data carries threshold_time_key; include it in the grouping so
  # the downstream join with validated_weekly does not create duplicate
  # .x/.y columns.
  confirmed_weekly <- confirmed_weekly |>
    bin_weekly_events(windows, "case_date")

  confirmed_count_by <- c("zone_sante_notification", "week_bin")
  if ("threshold_time_key" %in% names(confirmed_weekly)) {
    confirmed_count_by <- c(confirmed_count_by, "threshold_time_key")
  }

  confirmed_weekly <- confirmed_weekly |>
    dplyr::summarise(
      confirmed_cases_week = dplyr::n(),
      .by = dplyr::all_of(confirmed_count_by)
    )

  # Nowcast adjustment applies only when the true-case window is current.
  apply_nowcast <- !is.null(nowcast) && nrow(nowcast) > 0L
  if (apply_nowcast) {
    nowcast_ref <- ref_date
    if (is.null(nowcast_ref)) nowcast_ref <- attr(nowcast, "ref_date")
    if (is.null(nowcast_ref)) nowcast_ref <- max(nowcast$date, na.rm = TRUE)
    window_end_max <- if ("recent_case_window_end" %in% names(hz_parameters)) {
      max(as.Date(hz_parameters$recent_case_window_end), na.rm = TRUE)
    } else {
      as.Date(nowcast_ref)
    }
    apply_nowcast <-
      is.finite(as.Date(nowcast_ref)) &&
      is.finite(window_end_max) &&
      window_end_max >= as.Date(nowcast_ref) - max_delay + 1L
  }

  if (apply_nowcast) {
    week_delta <- nowcast |>
      dplyr::mutate(
        delta = pmax(.data$nowcast_median - .data$observed, 0),
        delta_low = pmax(.data$nowcast_lower_90 - .data$observed, 0),
        delta_high = pmax(.data$nowcast_upper_90 - .data$observed, 0)
      ) |>
      bin_weekly_events(windows, "date")

    delta_count_by <- c("zone_sante_notification", "week_bin")
    if ("threshold_time_key" %in% names(week_delta)) {
      delta_count_by <- c(delta_count_by, "threshold_time_key")
    }

    week_delta <- week_delta |>
      dplyr::summarise(
        delta = sum(.data$delta, na.rm = TRUE),
        delta_low = sum(.data$delta_low, na.rm = TRUE),
        delta_high = sum(.data$delta_high, na.rm = TRUE),
        .by = dplyr::all_of(delta_count_by)
      )

    delta_join_by <- if (all(c("threshold_time_key") %in% names(confirmed_weekly)) &&
                         all(c("threshold_time_key") %in% names(week_delta))) {
      dplyr::join_by(zone_sante_notification, week_bin, threshold_time_key)
    } else {
      dplyr::join_by(zone_sante_notification, week_bin)
    }

    confirmed_weekly <- confirmed_weekly |>
      dplyr::full_join(
        week_delta,
        by = delta_join_by
      ) |>
      dplyr::mutate(
        confirmed_cases_week = tidyr::replace_na(.data$confirmed_cases_week, 0L),
        confirmed_cases_week_nowcast = dplyr::if_else(
          !is.na(.data$delta),
          as.double(.data$confirmed_cases_week) + .data$delta,
          NA_real_
        ),
        confirmed_cases_week_nowcast_low = dplyr::if_else(
          !is.na(.data$delta_low),
          as.double(.data$confirmed_cases_week) + .data$delta_low,
          NA_real_
        ),
        confirmed_cases_week_nowcast_high = dplyr::if_else(
          !is.na(.data$delta_high),
          as.double(.data$confirmed_cases_week) + .data$delta_high,
          NA_real_
        ),
        case_count_source = dplyr::if_else(
          !is.na(.data$delta),
          "nowcast",
          "observed"
        )
      ) |>
      dplyr::select(-c("delta", "delta_low", "delta_high"))
  } else {
    confirmed_weekly <- confirmed_weekly |>
      dplyr::mutate(
        confirmed_cases_week_nowcast = NA_real_,
        confirmed_cases_week_nowcast_low = NA_real_,
        confirmed_cases_week_nowcast_high = NA_real_,
        case_count_source = "observed"
      )
  }

  validated_weekly <- val_alerts |>
    dplyr::mutate(
      alert_date = as.Date(.data[[alert_date_col]])
    ) |>
    dplyr::filter(
      !is.na(zone_sante_notification),
      !is.na(alert_date)
    )

  if (!use_full_window && is.null(windows)) {
    validated_weekly <- validated_weekly |>
      filter_dated_to_true_case_window(
        hz_parameters = hz_parameters,
        date_col = "alert_date"
      )
  }

  validated_weekly <- validated_weekly |>
    bin_weekly_events(windows, "alert_date")

  alert_count_by <- c("zone_sante_notification", "week_bin")
  if ("threshold_time_key" %in% names(validated_weekly)) {
    alert_count_by <- c(alert_count_by, "threshold_time_key")
  }

  validated_weekly <- validated_weekly |>
    dplyr::summarise(
      case_alerts_week = sum(nature_alerte == "Vivant", na.rm = TRUE),
      death_alerts_week = sum(nature_alerte == "Décédé", na.rm = TRUE),
      .by = dplyr::all_of(alert_count_by)
    )

  # Join weekly confirmed and alert counts. When the grid path is active both
  # sides carry threshold_time_key; join on it too so no duplicate .x/.y
  # columns are created.
  model_join_keys <- c("zone_sante_notification", "week_bin")
  if (all(c("threshold_time_key") %in% names(confirmed_weekly)) &&
      all(c("threshold_time_key") %in% names(validated_weekly))) {
    model_join_keys <- c(model_join_keys, "threshold_time_key")
  }

  weekly_model_data <- dplyr::full_join(
    confirmed_weekly,
    validated_weekly,
    by = model_join_keys,
    relationship = "one-to-one"
  ) |>
    dplyr::full_join(
      hz_parameters,
      by = dplyr::join_by(zone_sante_notification),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      confirmed_cases_week = tidyr::replace_na(confirmed_cases_week, 0L),
      case_alerts_week = tidyr::replace_na(case_alerts_week, 0L),
      death_alerts_week = tidyr::replace_na(death_alerts_week, 0L),
      case_count_source = tidyr::replace_na(case_count_source, "observed")
    ) |>
    dplyr::mutate(
      case_weight_count = dplyr::coalesce(
        confirmed_cases_week_nowcast,
        as.double(confirmed_cases_week)
      ),
      confirmed_cases_hz = sum(case_weight_count, na.rm = TRUE),
      weekly_case_weight = dplyr::if_else(
        confirmed_cases_hz > 0,
        case_weight_count / confirmed_cases_hz,
        NA_real_
      ),
      estimated_true_cases_week = estimated_true_cases_recent * weekly_case_weight,
      expected_deaths = estimated_true_cases_recent * cfr_used,
      expected_deaths_week = estimated_true_cases_week * cfr_used,
      case_model_included = !is.na(week_bin) &
        is.finite(estimated_true_cases_week) &
        estimated_true_cases_week > 0,
      death_model_included = !is.na(week_bin) &
        is.finite(expected_deaths_week) &
        expected_deaths_week > 0,
      .by = zone_sante_notification
    ) |>
    dplyr::select(-case_weight_count) |>
    dplyr::arrange(zone_sante_notification, week_bin)

  hz_summary <- weekly_model_data |>
    dplyr::summarise(
      estimated_true_cases_recent = alert_first_non_missing(estimated_true_cases_recent),
      cfr_used = alert_first_non_missing(cfr_used),
      expected_deaths = alert_first_non_missing(expected_deaths),
      val_alive = sum(case_alerts_week, na.rm = TRUE),
      val_dead = sum(death_alerts_week, na.rm = TRUE),
      .by = zone_sante_notification
    ) |>
    dplyr::arrange(zone_sante_notification)

  case_model_summary <- purrr::map_dfr(
    hz_summary$zone_sante_notification,
    \(hz) {
      fit_poisson_offset_rate(
        data = weekly_model_data |>
          dplyr::filter(zone_sante_notification == .env$hz),
        count_col = "case_alerts_week",
        exposure_col = "estimated_true_cases_week",
        model = "case",
        min_model_weeks = min_model_weeks,
        confidence_level = confidence_level,
        overdispersion_threshold = overdispersion_threshold
      )
    }
  )

  death_model_summary <- purrr::map_dfr(
    hz_summary$zone_sante_notification,
    \(hz) {
      fit_poisson_offset_rate(
        data = weekly_model_data |>
          dplyr::filter(zone_sante_notification == .env$hz),
        count_col = "death_alerts_week",
        exposure_col = "expected_deaths_week",
        model = "death",
        min_model_weeks = min_model_weeks,
        confidence_level = confidence_level,
        overdispersion_threshold = overdispersion_threshold
      )
    }
  )

  model_summary <- dplyr::bind_rows(case_model_summary, death_model_summary)

  multipliers <- hz_summary |>
    dplyr::left_join(
      case_model_summary |>
        dplyr::transmute(
          zone_sante_notification,
          beta_c = beta,
          beta_c_low = beta_low,
          beta_c_high = beta_high,
          beta_c_model_status = model_status,
          beta_c_n_model_weeks = n_model_weeks
        ),
      by = dplyr::join_by(zone_sante_notification),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      death_model_summary |>
        dplyr::transmute(
          zone_sante_notification,
          beta_d = beta,
          beta_d_low = beta_low,
          beta_d_high = beta_high,
          beta_d_model_status = model_status,
          beta_d_n_model_weeks = n_model_weeks
        ),
      by = dplyr::join_by(zone_sante_notification),
      relationship = "one-to-one"
    )

  attr(multipliers, "weekly_model_data") <- weekly_model_data
  attr(multipliers, "model_summary") <- model_summary
  multipliers
}

#' Compute alert multipliers across rolling windows
#'
#' Fits Poisson offset rate models for each window on trailing grid windows.
#' When `n_copy_last` is positive and nowcast is unavailable, trailing windows
#' copy estimates from the preceding window as a reporting-delay guard.
#' When nowcast is available and used for a window, its nowcast-adjusted
#' multiplier estimates are preserved instead of being overwritten.
#'
#' @inheritParams compute_alert_multipliers
#' @param window_key_col Character. Window identifier column in `true_cases`.
#' @param lookback_weeks Integer. Number of trailing grid windows to fit on.
#' @param n_copy_last Integer. Number of trailing windows whose estimates
#'   should be copied from the preceding window when nowcast is unavailable.
#' @return Tibble with one row per HZ per window. Attributes
#'   `weekly_model_data` and `model_summary` contain window-tagged diagnostics.
compute_alert_multipliers_by_window <- function(
    val_alerts,
    true_cases,
    hz_cfr,
    evd = val_alerts,
    alert_date_col = "date_heure_notification_alerte",
    case_date_col = "alert_date_debut_symptoms",
    fallback_date_col = "s2_date_debut_signes_symptomes",
    min_model_weeks = 2L,
    confidence_level = 0.95,
    window_key_col = "threshold_time_key",
    use_full_window = FALSE,
    windows = NULL,
    lookback_weeks = 3L,
    n_copy_last = 1L,
    nowcast = NULL,
    ref_date = NULL,
    max_delay = 21L,
    overdispersion_threshold = 1.5,
    lag_to_notification = TRUE) {
  if (!window_key_col %in% names(true_cases)) {
    return(
      compute_alert_multipliers(
        val_alerts = val_alerts,
        true_cases = true_cases,
        hz_cfr = hz_cfr,
        evd = evd,
        alert_date_col = alert_date_col,
        case_date_col = case_date_col,
        fallback_date_col = fallback_date_col,
        min_model_weeks = min_model_weeks,
        confidence_level = confidence_level,
        use_full_window = use_full_window,
        windows = windows,
        nowcast = nowcast,
        ref_date = ref_date,
        max_delay = max_delay,
        overdispersion_threshold = overdispersion_threshold,
        lag_to_notification = lag_to_notification
      )
    )
  }

  alert_required_columns(
    true_cases,
    c(window_key_col, "zone_sante_notification", "estimated_true_cases_recent"),
    "compute_alert_multipliers_by_window()"
  )

  # Resolve the analysis grid. When not passed explicitly, derive it from the
  # window columns carried by `true_cases`.
  if (is.null(windows) && window_key_col %in% names(true_cases)) {
    windows <- true_cases |>
      dplyr::select(
        zone_sante_notification,
        dplyr::any_of(c(
          "recent_case_window_start",
          "recent_case_window_end",
          "threshold_window_index",
          window_key_col
        ))
      ) |>
      dplyr::distinct()
  }

  window_keys <- true_cases |>
    dplyr::filter(!is.na(.data[[window_key_col]])) |>
    dplyr::distinct(window_key = .data[[window_key_col]]) |>
    dplyr::arrange(window_key) |>
    dplyr::pull(window_key)

  window_results <- purrr::map(
    window_keys,
    \(window_key) {
      window_true_cases <- true_cases |>
        dplyr::filter(.data[[window_key_col]] == .env$window_key)

      # Trailing grid up to this window (last `lookback_weeks` windows):
      # Each per-window fit is estimated on the trailing grid windows
      # observed up to that window (one observation per grid window),
      # capturing recent multiplier dynamics without cumulative historical lag.
      trailing_windows <- windows
      if (!is.null(windows)) {
        if ("threshold_window_index" %in% names(windows)) {
          curr_idx <- windows |>
            dplyr::filter(.data[[window_key_col]] == .env$window_key) |>
            dplyr::pull(threshold_window_index)
          if (length(curr_idx) > 0 && !is.na(curr_idx[1])) {
            min_idx <- max(1L, curr_idx[1] - as.integer(lookback_weeks) + 1L)
            trailing_windows <- windows |>
              dplyr::filter(
                .data[[window_key_col]] <= .env$window_key,
                threshold_window_index >= min_idx
              )
          } else {
            trailing_windows <- windows |>
              dplyr::filter(.data[[window_key_col]] <= .env$window_key)
          }
        } else {
          trailing_windows <- windows |>
            dplyr::filter(.data[[window_key_col]] <= .env$window_key)
        }
      }

      window_metadata <- window_true_cases |>
        dplyr::select(
          zone_sante_notification,
          dplyr::any_of(alert_window_metadata_columns(names(true_cases)))
        ) |>
        dplyr::distinct()

      window_cfr <- hz_cfr
      if (window_key_col %in% names(hz_cfr)) {
        window_cfr <- hz_cfr |>
          dplyr::filter(.data[[window_key_col]] == .env$window_key)
      }

      multipliers <- compute_alert_multipliers(
        val_alerts = val_alerts,
        true_cases = window_true_cases |>
          dplyr::select(
            zone_sante_notification,
            estimated_true_cases_recent,
            dplyr::any_of(c(
              "threshold_time_key",
              "recent_case_window_start",
              "recent_case_window_end"
            ))
          ),
        hz_cfr = window_cfr,
        evd = evd,
        alert_date_col = alert_date_col,
        case_date_col = case_date_col,
        fallback_date_col = fallback_date_col,
        min_model_weeks = min_model_weeks,
        confidence_level = confidence_level,
        use_full_window = use_full_window,
        windows = trailing_windows,
        nowcast = nowcast,
        ref_date = ref_date,
        max_delay = max_delay,
        overdispersion_threshold = overdispersion_threshold,
        lag_to_notification = lag_to_notification
      )

      weekly_model_data <- attr(multipliers, "weekly_model_data")
      model_summary <- attr(multipliers, "model_summary")

      list(
        multipliers = add_alert_window_metadata(multipliers, window_metadata),
        weekly_model_data = add_alert_window_metadata(
          weekly_model_data,
          window_metadata
        ),
        model_summary = add_alert_window_metadata(model_summary, window_metadata)
      )
    }
  )

  multipliers <- purrr::map_dfr(window_results, "multipliers")
  weekly_model_data <- purrr::map_dfr(window_results, "weekly_model_data")
  model_summary <- purrr::map_dfr(window_results, "model_summary")

  n_windows <- length(window_keys)
  effective_copy <- min(n_copy_last, max(0L, n_windows - 1L))

  if (effective_copy > 0L) {
    nowcast_flags <- weekly_model_data |>
      dplyr::summarise(
        .nowcast_used = any(.data$case_count_source == "nowcast", na.rm = TRUE),
        .by = dplyr::all_of(c("zone_sante_notification", window_key_col))
      )

    join_by_expr <- if (window_key_col %in% names(multipliers)) {
      dplyr::join_by(zone_sante_notification, !!rlang::sym(window_key_col))
    } else {
      dplyr::join_by(zone_sante_notification)
    }

    multipliers <- multipliers |>
      dplyr::left_join(
        nowcast_flags,
        by = join_by_expr,
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        .nowcast_used = tidyr::replace_na(.data$.nowcast_used, FALSE)
      ) |>
      dplyr::group_by(zone_sante_notification) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(window_key_col))) |>
      dplyr::mutate(
        .should_copy = (dplyr::row_number() > n_windows - effective_copy) & !.data$.nowcast_used,
        beta_c = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_c, n_windows - effective_copy),
          beta_c
        ),
        beta_c_low = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_c_low, n_windows - effective_copy),
          beta_c_low
        ),
        beta_c_high = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_c_high, n_windows - effective_copy),
          beta_c_high
        ),
        beta_c_model_status = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_c_model_status, n_windows - effective_copy),
          beta_c_model_status
        ),
        beta_c_n_model_weeks = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_c_n_model_weeks, n_windows - effective_copy),
          beta_c_n_model_weeks
        ),
        beta_d = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_d, n_windows - effective_copy),
          beta_d
        ),
        beta_d_low = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_d_low, n_windows - effective_copy),
          beta_d_low
        ),
        beta_d_high = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_d_high, n_windows - effective_copy),
          beta_d_high
        ),
        beta_d_model_status = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_d_model_status, n_windows - effective_copy),
          beta_d_model_status
        ),
        beta_d_n_model_weeks = dplyr::if_else(
          .data$.should_copy,
          dplyr::nth(beta_d_n_model_weeks, n_windows - effective_copy),
          beta_d_n_model_weeks
        )
      ) |>
      dplyr::select(-c(".nowcast_used", ".should_copy")) |>
      dplyr::ungroup()
  }

  attr(multipliers, "weekly_model_data") <- weekly_model_data
  attr(multipliers, "model_summary") <- model_summary
  multipliers
}

#' Bin events for the weekly model onto the analysis grid
#'
#' When a grid is available, events are assigned to their 7-day grid window
#' via `bin_dates_by_grid()` (harmonized time axis). Without a grid
#' (legacy/static calls), events fall back to Monday calendar weeks.
#'
#' @param data Dataframe with an event date column.
#' @param windows Dataframe or `NULL`. Analysis grid.
#' @param date_col Character. Event date column in `data`.
#' @return `data` with `week_bin` added (and `threshold_time_key` when the
#'   grid path is used), restricted to in-grid events on the grid path.
bin_weekly_events <- function(data, windows, date_col) {
  if (!is.null(windows)) {
    return(bin_dates_by_grid(data, windows, date_col))
  }

  data |>
    dplyr::mutate(
      week_bin = lubridate::floor_date(
        .data[[date_col]],
        "week",
        week_start = 1
      )
    )
}

filter_dated_to_true_case_window <- function(data, hz_parameters, date_col) {
  window_cols <- c("recent_case_window_start", "recent_case_window_end")

  if (!all(window_cols %in% names(hz_parameters))) {
    return(data)
  }

  data |>
    dplyr::left_join(
      hz_parameters |>
        dplyr::select(zone_sante_notification, dplyr::all_of(window_cols)),
      by = dplyr::join_by(zone_sante_notification),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(
      is.na(recent_case_window_start) |
        is.na(recent_case_window_end) |
        (
          .data[[date_col]] >= recent_case_window_start &
            .data[[date_col]] <= recent_case_window_end
        )
    ) |>
    dplyr::select(-dplyr::all_of(window_cols))
}

add_alert_window_metadata <- function(data, window_metadata) {
  if (is.null(data) || nrow(data) == 0L) {
    return(data)
  }

  # Drop any window metadata already carried by the data so the join below
  # re-attaches a single canonical set of columns (no .x/.y suffixes).
  data <- data |>
    dplyr::select(-dplyr::any_of(alert_window_metadata_columns(names(data))))

  joined <- data |>
    dplyr::left_join(
      window_metadata,
      by = dplyr::join_by(zone_sante_notification),
      relationship = "many-to-one"
    )
  metadata_columns <- alert_window_metadata_columns(names(joined))

  joined |>
    dplyr::relocate(
      dplyr::any_of(metadata_columns),
      .before = zone_sante_notification
    )
}

alert_first_non_missing <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  x[[1L]]
}

fit_poisson_offset_rate <- function(data,
                                    count_col,
                                    exposure_col,
                                    model,
                                    min_model_weeks,
                                    confidence_level,
                                    overdispersion_threshold = 1.5) {
  zone <- data$zone_sante_notification[[1L]]
  n_weeks <- sum(!is.na(data$week_bin))

  model_data <- data |>
    dplyr::filter(
      !is.na(week_bin),
      is.finite(.data[[count_col]]),
      .data[[count_col]] >= 0,
      is.finite(.data[[exposure_col]]),
      .data[[exposure_col]] > 0
    )

  n_model_weeks <- nrow(model_data)
  total_count <- sum(model_data[[count_col]], na.rm = TRUE)
  total_exposure <- sum(model_data[[exposure_col]], na.rm = TRUE)

  if (n_model_weeks == 0L || !is.finite(total_exposure) ||
      total_exposure <= 0) {
    return(rate_result(
      zone = zone,
      model = model,
      n_weeks = n_weeks,
      n_model_weeks = n_model_weeks,
      total_count = total_count,
      total_exposure = total_exposure,
      model_status = "no_valid_exposure"
    ))
  }

  rate <- total_count / total_exposure
  exact_ci <- poisson_rate_ci(
    total_count,
    total_exposure,
    confidence_level = confidence_level
  )

  if (n_model_weeks < min_model_weeks) {
    return(rate_result(
      zone = zone,
      model = model,
      beta = rate,
      beta_low = exact_ci[["low"]],
      beta_high = exact_ci[["high"]],
      n_weeks = n_weeks,
      n_model_weeks = n_model_weeks,
      total_count = total_count,
      total_exposure = total_exposure,
      model_status = "insufficient_weeks_ratio_fallback"
    ))
  }

  if (total_count == 0) {
    return(rate_result(
      zone = zone,
      model = model,
      beta = 0,
      beta_low = exact_ci[["low"]],
      beta_high = exact_ci[["high"]],
      n_weeks = n_weeks,
      n_model_weeks = n_model_weeks,
      total_count = total_count,
      total_exposure = total_exposure,
      model_status = "zero_count_rate"
    ))
  }

  glm_data <- model_data |>
    dplyr::transmute(
      count = .data[[count_col]],
      exposure = .data[[exposure_col]]
    )

  fit <- tryCatch(
    stats::glm(
      count ~ 1 + offset(log(exposure)),
      family = stats::poisson(),
      data = glm_data
    ),
    error = \(error) error
  )

  if (inherits(fit, "error")) {
    return(rate_result(
      zone = zone,
      model = model,
      beta = rate,
      beta_low = exact_ci[["low"]],
      beta_high = exact_ci[["high"]],
      n_weeks = n_weeks,
      n_model_weeks = n_model_weeks,
      total_count = total_count,
      total_exposure = total_exposure,
      model_status = "glm_failed_ratio_fallback"
    ))
  }

  coefficient <- unname(stats::coef(fit)[[1L]])
  se <- sqrt(stats::vcov(fit)[1L, 1L])
  dispersion <- poisson_dispersion(fit)
  z <- stats::qnorm(1 - (1 - confidence_level) / 2)

  # Overdispersion: when the dispersion ratio exceeds the threshold, refit
  # with a negative-binomial GLM (theta estimated by MASS::glm.nb) instead of
  # relying on the quasi-Poisson standard-error inflation below.
  use_negbin <- is.finite(dispersion) && dispersion > overdispersion_threshold

  if (use_negbin) {
    negbin_warnings <- character(0)

    negbin_fit <- tryCatch(
      withCallingHandlers(
        MASS::glm.nb(
          count ~ 1 + offset(log(exposure)),
          data = glm_data,
          link = "log"
        ),
        warning = function(w) {
          negbin_warnings <<- c(negbin_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = \(error) error
    )

    # A theta estimate that hits the iteration limit (or produces NaNs) is
    # unstable and drifts toward Poisson; treat it as a failed NB fit and
    # fall back to the quasi-Poisson correction below.
    negbin_ok <- !inherits(negbin_fit, "error") &&
      length(negbin_warnings) == 0L &&
      isTRUE(negbin_fit$converged)

    if (negbin_ok) {
      negbin_coef <- unname(stats::coef(negbin_fit)[[1L]])
      negbin_se <- sqrt(stats::vcov(negbin_fit)[1L, 1L])
      theta <- negbin_fit$theta
      negbin_ok <- is.finite(negbin_coef) &&
        is.finite(negbin_se) &&
        is.finite(theta) &&
        theta > 0
    }

    if (negbin_ok) {
      beta <- exp(negbin_coef)
      beta_low <- exp(negbin_coef - z * negbin_se)
      beta_high <- exp(negbin_coef + z * negbin_se)

      return(rate_result(
        zone = zone,
        model = model,
        beta = beta,
        beta_low = beta_low,
        beta_high = beta_high,
        se = negbin_se,
        se_adjusted = NA_real_,
        dispersion = NA_real_,
        theta = theta,
        n_weeks = n_weeks,
        n_model_weeks = n_model_weeks,
        total_count = total_count,
        total_exposure = total_exposure,
        model_status = "negative_binomial"
      ))
    }
  }

  dispersion_scale <- if (is.finite(dispersion)) {
    sqrt(max(1, dispersion))
  } else {
    1
  }
  se_adjusted <- se * dispersion_scale

  if (is.finite(coefficient) && is.finite(se_adjusted)) {
    beta <- exp(coefficient)
    beta_low <- exp(coefficient - z * se_adjusted)
    beta_high <- exp(coefficient + z * se_adjusted)
  } else {
    beta <- rate
    beta_low <- exact_ci[["low"]]
    beta_high <- exact_ci[["high"]]
  }

  rate_result(
    zone = zone,
    model = model,
    beta = beta,
    beta_low = beta_low,
    beta_high = beta_high,
    se = se,
    se_adjusted = se_adjusted,
    dispersion = dispersion,
    n_weeks = n_weeks,
    n_model_weeks = n_model_weeks,
    total_count = total_count,
    total_exposure = total_exposure,
    model_status = if (use_negbin) "negbin_failed_quasi_poisson" else "poisson_offset"
  )
}

poisson_rate_ci <- function(total_count,
                            total_exposure,
                            confidence_level) {
  if (!is.finite(total_count) || !is.finite(total_exposure) ||
      total_exposure <= 0 || total_count < 0) {
    return(c(low = NA_real_, high = NA_real_))
  }

  ci <- tryCatch(
    stats::poisson.test(
      round(total_count),
      T = total_exposure,
      conf.level = confidence_level
    )$conf.int,
    error = \(error) c(NA_real_, NA_real_)
  )

  c(low = ci[[1L]], high = ci[[2L]])
}

poisson_dispersion <- function(fit) {
  df_residual <- stats::df.residual(fit)

  if (!is.finite(df_residual) || df_residual <= 0) {
    return(NA_real_)
  }

  stats::deviance(fit) / df_residual
}

rate_result <- function(zone,
                        model,
                        beta = NA_real_,
                        beta_low = NA_real_,
                        beta_high = NA_real_,
                        se = NA_real_,
                        se_adjusted = NA_real_,
                        dispersion = NA_real_,
                        theta = NA_real_,
                        n_weeks,
                        n_model_weeks,
                        total_count,
                        total_exposure,
                        model_status) {
  tibble::tibble(
    zone_sante_notification = zone,
    model = model,
    beta = beta,
    beta_low = beta_low,
    beta_high = beta_high,
    se = se,
    se_adjusted = se_adjusted,
    dispersion = dispersion,
    theta = theta,
    n_weeks = n_weeks,
    n_model_weeks = n_model_weeks,
    total_count = total_count,
    total_exposure = total_exposure,
    model_status = model_status
  )
}
