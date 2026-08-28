#' Compute exponential growth rate by health zone using non-overlapping 7-day windows
#'
#' For each health zone, generates non-overlapping 7-day windows anchored
#' backward from the most recent onset date. Each window spans exactly
#' `window_days` days; the first window may extend before the earliest onset
#' date (days with zero cases). For the last `n_copy_last` windows, growth-rate
#' estimates are copied from the preceding window to mitigate bias from
#' reporting delays in the most recent days. Defaults to 0: the nowcast
#' tail (when supplied) already corrects for reporting delays, so the last
#' window reports its exact estimate.
#'
#' A pooled estimate (across all health zones) is computed for each window
#' date and used as fallback when the health-zone-specific estimate is
#' unavailable.
#'
#' @param data Dataframe. EVD line list.
#' @param window_days Integer. Width of each rolling fitting window in days.
#' @param min_cases Integer. Minimum cases required in the fitting window.
#' @param min_days Integer. Minimum daily observations required after filling
#'   the observed date range.
#' @param n_copy_last Integer. Number of trailing windows whose estimates
#'   should be copied from the immediately preceding window (reporting-delay
#'   guard). Defaults to 0; the nowcast tail is preferred when available.
#'   Set to a positive value to re-enable copying.
#' @param threshold_dates Date vector. Optional threshold application dates.
#' @param threshold_step_days Integer. Days each generated threshold applies.
#' @param analysis_start_date Date. Optional lower bound for generated windows.
#' @param analysis_end_date Date. Optional upper bound for generated windows.
#' @param windows Dataframe. Optional shared time-window grid (distinct window
#'   metadata or a full HZ x window table such as `hz_recent_cases`). When
#'   supplied, both per-HZ and pooled fits use exactly these windows so every
#'   health zone shares the same `threshold_time_key` grid.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`; the truncated tail of recent windows is
#'   replaced with nowcast counts before fitting.
#' @param max_delay Integer. Nowcast tail length in days.
#' @return Tibble with one row per health zone per rolling window. Includes
#'   standard threshold columns, growth-rate estimates, pooled fallback columns,
#'   and resolved `r_used` / `doubling_time_used` columns.
compute_growth_rate_by_hz <- function(data,
                                      window_days = 7L,
                                      min_cases = 3L,
                                      min_days = 3L,
                                      n_copy_last = 0L,
                                      threshold_dates = NULL,
                                      threshold_step_days = 7L,
                                      analysis_start_date = NULL,
                                      analysis_end_date = NULL,
                                      windows = NULL,
                                      case_date_col = "alert_date_debut_symptoms",
                                      fallback_date_col = "s2_date_debut_signes_symptomes",
                                      nowcast = NULL,
                                      max_delay = 21L) {
  alert_required_columns(
    data,
    c(
      "zone_sante_notification",
      "classification_finale",
      "lab_resultat_final"
    ),
    "compute_growth_rate_by_hz()"
  )

  windows_meta <- if (is.null(windows)) {
    NULL
  } else {
    alert_window_grid(windows)
  }

  if (!any(c(case_date_col, fallback_date_col) %in% names(data))) {
    rlang::abort(c(
      "compute_growth_rate_by_hz() requires a case or fallback date column.",
      x = paste(c(case_date_col, fallback_date_col), collapse = ", ")
    ))
  }

  if (!requireNamespace("incidence", quietly = TRUE)) {
    rlang::abort(c(
      "The {.pkg incidence} package is required for growth-rate estimation.",
      i = "Install it with {.code pak::pak(\"incidence\")} in this renv."
    ))
  }

  hz_list <- data |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::distinct(zone_sante_notification) |>
    dplyr::arrange(zone_sante_notification) |>
    dplyr::pull(zone_sante_notification)

  # Per-HZ rolling growth rates

  hz_growth <- purrr::map_dfr(
    hz_list,
    \(hz) {
      compute_growth_rate_hz(
        data = data |>
          dplyr::filter(zone_sante_notification == hz),
        zone_sante_notification = hz,
        window_days = window_days,
        min_cases = min_cases,
        min_days = min_days,
        n_copy_last = n_copy_last,
        threshold_dates = threshold_dates,
        threshold_step_days = threshold_step_days,
        analysis_start_date = analysis_start_date,
        analysis_end_date = analysis_end_date,
        windows = windows_meta,
        case_date_col = case_date_col,
        fallback_date_col = fallback_date_col,
        nowcast = nowcast,
        max_delay = max_delay
      )
    }
  )

  # Pooled rolling growth rates (all zones combined, global date range)

  pooled_growth <- compute_growth_rate_hz(
    data = data,
    zone_sante_notification = NA_character_,
    window_days = window_days,
    min_cases = min_cases,
    min_days = min_days,
    n_copy_last = n_copy_last,
    threshold_dates = threshold_dates,
    threshold_step_days = threshold_step_days,
    analysis_start_date = analysis_start_date,
    analysis_end_date = analysis_end_date,
    windows = windows_meta,
    case_date_col = case_date_col,
    fallback_date_col = fallback_date_col,
    nowcast = nowcast,
    max_delay = max_delay
  )

  pooled_cols <- pooled_growth |>
    dplyr::select(
      threshold_time_key,
      pooled_r = r,
      pooled_r_low = r_low,
      pooled_r_high = r_high,
      pooled_doubling_time = doubling_time
    )

  # Join pooled fallback by threshold_time_key (handles HZs with different
  # date ranges where window_index may not align across HZs)

  hz_growth |>
    dplyr::left_join(pooled_cols, by = "threshold_time_key") |>
    dplyr::mutate(
      growth_fallback_reason = dplyr::case_when(
        !is.finite(r) & is.finite(pooled_r) ~ "pooled_growth_rate",
        !is.finite(r) & !is.finite(pooled_r) ~ "missing_growth_rate",
        .default = NA_character_
      ),
      pooled_growth_used = dplyr::coalesce(
        growth_fallback_reason == "pooled_growth_rate",
        FALSE
      ),
      r_used = dplyr::if_else(pooled_growth_used, pooled_r, r),
      r_low_used = dplyr::if_else(pooled_growth_used, pooled_r_low, r_low),
      r_high_used = dplyr::if_else(pooled_growth_used, pooled_r_high, r_high),
      doubling_time_used = dplyr::if_else(
        pooled_growth_used,
        pooled_doubling_time,
        doubling_time
      )
    )
}

#' Compute rolling-window exponential growth rates for one health-zone subset
#' @noRd
compute_growth_rate_hz <- function(data,
                                   zone_sante_notification,
                                   window_days,
                                   min_cases,
                                   min_days,
                                   n_copy_last = 0L,
                                   threshold_dates = NULL,
                                   threshold_step_days = 7L,
                                   analysis_start_date = NULL,
                                   analysis_end_date = NULL,
                                   windows = NULL,
                                   case_date_col = "alert_date_debut_symptoms",
                                   fallback_date_col = "s2_date_debut_signes_symptomes",
                                   nowcast = NULL,
                                   max_delay = 21L) {
  
  date_values <- list()
  if (case_date_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <- as.Date(data[[case_date_col]])
  }
  if (fallback_date_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <- as.Date(data[[fallback_date_col]])
  }
  
  if (length(date_values) > 0) {
    onset_dates <- purrr::reduce(date_values, dplyr::coalesce)
  } else {
    onset_dates <- as.Date(rep(NA, nrow(data)))
  }
  valid_onset_dates <- onset_dates[!is.na(onset_dates)]

  empty_result <- function(reason, window_info = NULL) {
    base <- tibble::tibble(
      zone_sante_notification = zone_sante_notification,
      growth_n_cases = NA_integer_,
      growth_n_cases_nowcast = NA_integer_,
      growth_n_days = NA_integer_,
      r = NA_real_,
      r_low = NA_real_,
      r_high = NA_real_,
      doubling_time = NA_real_,
      doubling_time_low = NA_real_,
      doubling_time_high = NA_real_,
      halving_time = NA_real_,
      halving_time_low = NA_real_,
      halving_time_high = NA_real_,
      predicted_last_count = NA_real_,
      growth_count_source = "observed",
      growth_direction = NA_character_,
      growth_rate_source = "incidence_fit",
      growth_status = reason
    )
    if (!is.null(window_info)) {
      dplyr::bind_cols(window_info, base)
    } else {
      dplyr::bind_cols(
        tibble::tibble(
          threshold_time_key = NA_character_,
          threshold_window_id = NA_character_,
          threshold_window_index = NA_integer_,
          threshold_valid_from = as.Date(NA),
          threshold_valid_to = as.Date(NA),
          recent_case_window_start = as.Date(NA),
          recent_case_window_end = as.Date(NA),
          recent_case_window_days = NA_integer_,
          recent_case_anchor_date = as.Date(NA)
        ),
        base
      )
    }
  }

  if (is.null(windows)) {
    if (length(valid_onset_dates) == 0) {
      return(empty_result("missing_onset_dates"))
    }

    threshold_dates_resolved <- resolve_threshold_dates(
      case_dates = valid_onset_dates,
      threshold_dates = threshold_dates,
      window_days = as.integer(window_days),
      threshold_step_days = as.integer(threshold_step_days),
      analysis_start_date = analysis_start_date,
      analysis_end_date = analysis_end_date
    )

    if (length(threshold_dates_resolved) == 0L) {
      return(empty_result("sparse_days"))
    }

    windows <- build_recent_case_windows(
      threshold_dates = threshold_dates_resolved,
      window_days = as.integer(window_days),
      threshold_step_days = as.integer(threshold_step_days)
    )
  } else if (length(valid_onset_dates) == 0) {
    return(
      purrr::map_dfr(
        seq_len(nrow(windows)),
        \(i) empty_result("missing_onset_dates", windows[i, ])
      )
    )
  }

  n_windows <- nrow(windows)

  # Adjust n_copy_last when there are too few windows

  effective_copy <- min(n_copy_last, max(0L, n_windows - 1L))
  if (effective_copy < n_copy_last && n_windows > 0) {
    rlang::warn(c(
      sprintf(
        "Only %d window(s) for zone %s; reduced n_copy_last from %d to %d.",
        n_windows,
        zone_sante_notification %||% "(pooled)",
        n_copy_last,
        effective_copy
      )
    ))
  }

  # Pre-filter confirmed cases with valid dates (superset for all windows)

  all_cases <- data |>
    dplyr::mutate(
      date = onset_dates,
      is_confirmed = alert_is_confirmed_case(data)
    ) |>
    dplyr::filter(is_confirmed, !is.na(date)) |>
    dplyr::count(date, name = "I")

  # Compute growth rate for every window

  window_results <- purrr::map_dfr(
    seq_len(n_windows),
    function(i) {
      compute_growth_rate_window(
        all_cases = all_cases,
        zone_sante_notification = zone_sante_notification,
        window_info = windows[i, ],
        min_cases = min_cases,
        min_days = min_days,
        empty_result = empty_result,
        nowcast = nowcast,
        max_delay = max_delay
      )
    }
  )

  # Copy last `effective_copy` windows from the preceding window

  if (effective_copy > 0L) {
    copy_start <- n_windows - effective_copy + 1L
    for (i in seq(from = copy_start, to = n_windows)) {
      # Keep a nowcast-corrected estimate instead of copying over it.
      if (identical(window_results$growth_count_source[[i]], "nowcast")) next
      preceding <- window_results[i - 1L, ]
      window_info <- windows[i, ]
      for (col in names(window_info)) {
        preceding[[col]] <- window_info[[col]]
      }
      window_results[i, ] <- preceding |>
        dplyr::mutate(
          growth_rate_source = "copied",
          growth_status = "copied_from_preceding"
        )
    }
  }

  window_results
}

#' Compute growth rate for a single rolling window
#' @noRd
compute_growth_rate_window <- function(all_cases,
                                       zone_sante_notification,
                                       window_info,
                                       min_cases,
                                       min_days,
                                       empty_result,
                                       nowcast = NULL,
                                       max_delay = 21L) {
  window_start <- window_info$recent_case_window_start
  window_end <- window_info$recent_case_window_end

  hz_cases <- all_cases |>
    dplyr::filter(date >= window_start, date <= window_end)

  total_cases <- sum(hz_cases$I, na.rm = TRUE)

  if (total_cases < min_cases) {
    return(empty_result("sparse_cases", window_info) |>
      dplyr::mutate(
        growth_n_cases = total_cases
      ))
  }

  dates_full <- seq.Date(window_start, window_end, by = "day")
  hz_full <- tibble::tibble(date = dates_full) |>
    dplyr::left_join(hz_cases, by = dplyr::join_by(date)) |>
    tidyr::replace_na(list(I = 0L))

  # Replace the reporting-lag tail with nowcast counts when the window ends
  # within the nowcast horizon (pooled rows keep observed counts).
  count_source <- "observed"
  if (!is.null(nowcast) && !is.na(zone_sante_notification)) {
    hz_full_nowcast <- splice_nowcast_tail(
      hz_full,
      nowcast,
      zone = zone_sante_notification,
      ref_date = window_end,
      max_delay = max_delay
    )
    if (!identical(hz_full_nowcast$I, hz_full$I)) {
      hz_full <- hz_full_nowcast
      count_source <- "nowcast"
    }
  }

  if (nrow(hz_full) < min_days) {
    return(empty_result("sparse_days", window_info) |>
      dplyr::mutate(
        growth_n_cases = total_cases,
        growth_n_days = nrow(hz_full)
      ))
  }

  tryCatch(
    {
      incidence_data <- incidence::as.incidence(
        hz_full$I,
        hz_full$date,
        interval = 1
      )
      fit <- incidence::fit(incidence_data, quiet = TRUE)
      r_conf <- as.numeric(fit$info$r.conf[1, ])
      doubling_conf <- if ("doubling.conf" %in% names(fit$info)) {
        as.numeric(fit$info$doubling.conf[1, ])
      } else {
        c(NA_real_, NA_real_)
      }
      halving_conf <- if ("halving.conf" %in% names(fit$info)) {
        as.numeric(fit$info$halving.conf[1, ])
      } else {
        c(NA_real_, NA_real_)
      }

      r <- as.numeric(fit$info$r)
      r_low <- r_conf[[1]]
      r_high <- r_conf[[2]]
      doubling_time <- if ("doubling" %in% names(fit$info)) {
        as.numeric(fit$info$doubling)
      } else {
        NA_real_
      }
      halving_time <- if ("halving" %in% names(fit$info)) {
        as.numeric(fit$info$halving)
      } else {
        NA_real_
      }

      # N0 is the total (nowcast-corrected when available) count in the window
      N0 <- if (count_source == "nowcast") {
        sum(hz_full$I, na.rm = TRUE)
      } else {
        total_cases
      }
      # t is the window duration in days
      t <- as.numeric(window_end - window_start) + 1L

      predicted_last_count <- as.numeric(N0 * exp(r * t))

      if (!is.finite(predicted_last_count)) {
        predicted_last_count <- NA_real_
      }

      base <- tibble::tibble(
        zone_sante_notification = zone_sante_notification,
        growth_n_cases = total_cases,
        growth_n_cases_nowcast = dplyr::if_else(
          count_source == "nowcast",
          as.integer(sum(hz_full$I, na.rm = TRUE)),
          NA_integer_
        ),
        growth_n_days = nrow(hz_full),
        r = dplyr::if_else(is.finite(r), r, NA_real_),
        r_low = dplyr::if_else(is.finite(r_low), r_low, NA_real_),
        r_high = dplyr::if_else(is.finite(r_high), r_high, NA_real_),
        doubling_time = dplyr::if_else(
          is.finite(doubling_time),
          doubling_time,
          NA_real_
        ),
        doubling_time_low = dplyr::if_else(
          is.finite(doubling_conf[[1]]),
          doubling_conf[[1]],
          NA_real_
        ),
        doubling_time_high = dplyr::if_else(
          is.finite(doubling_conf[[2]]),
          doubling_conf[[2]],
          NA_real_
        ),
        halving_time = dplyr::if_else(
          is.finite(halving_time),
          halving_time,
          NA_real_
        ),
        halving_time_low = dplyr::if_else(
          is.finite(halving_conf[[1]]),
          halving_conf[[1]],
          NA_real_
        ),
        halving_time_high = dplyr::if_else(
          is.finite(halving_conf[[2]]),
          halving_conf[[2]],
          NA_real_
        ),
        predicted_last_count = predicted_last_count,
        growth_count_source = count_source,
        growth_direction = dplyr::case_when(
          r > 0 ~ "growing",
          r < 0 ~ "declining",
          r == 0 ~ "stable",
          .default = NA_character_
        ),
        growth_rate_source = "incidence_fit",
        growth_status = "estimated"
      )
      
      dplyr::bind_cols(window_info, base)
    },
    error = function(e) {
      empty_result("fit_error", window_info) |>
        dplyr::mutate(
          growth_n_cases = total_cases,
          growth_n_cases_nowcast = dplyr::if_else(
            count_source == "nowcast",
            as.integer(sum(hz_full$I, na.rm = TRUE)),
            NA_integer_
          ),
          growth_n_days = nrow(hz_full),
          growth_count_source = count_source
        )
    }
  )
}
