#' Compute rolling recent confirmed/positive cases by health zone
#'
#' Counts confirmed/positive cases in a flexible day-level window anchored at
#' each threshold date. By default, thresholds are generated weekly because the
#' trend pipeline aggregates alerts by week.
#'
#' @param data Dataframe. EVD line list.
#' @param case_date_col Character. Preferred case date column.
#' @param fallback_date_col Character. Fallback date column.
#' @param window_days Integer. Number of recent exposure days to count.
#' @param threshold_dates Date vector. Optional threshold application dates.
#' @param threshold_step_days Integer. Days each generated threshold applies.
#' @param analysis_start_date Date. Optional lower bound for generated windows.
#' @param analysis_end_date Date. Optional upper bound for generated windows.
#' @param hz_index Optional vector/dataframe of health zones to retain.
#' @param allow_partial_first_window Logical. If `FALSE`, generated thresholds
#'   start only after a full exposure window is available.
#' @param n_copy_last Integer. Number of trailing windows whose observed
#'   counts should be copied from the immediately preceding window. Defaults
#'   to 0: nowcast-adjusted counts already correct the reporting-lag tail, so
#'   the last window reports its exact observed count.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param max_delay Integer. Nowcast tail length in days.
#' @return Tibble with one row per health-zone threshold window.
compute_recent_confirmed_windows_by_hz <- function(
    data,
    case_date_col = "alert_date_debut_symptoms",
    fallback_date_col = "s2_date_debut_signes_symptomes",
    window_days = 7L,
    threshold_dates = NULL,
    threshold_step_days = 7L,
    analysis_start_date = NULL,
    analysis_end_date = NULL,
    hz_index = NULL,
    n_copy_last = 0L,
    nowcast = NULL,
    max_delay = 21L) {
  alert_required_columns(
    data,
    c(
      "zone_sante_notification",
      "classification_finale",
      "lab_resultat_final"
    ),
    "compute_recent_confirmed_windows_by_hz()"
  )

  if (!any(c(case_date_col, fallback_date_col) %in% names(data))) {
    rlang::abort(c(
      "compute_recent_confirmed_windows_by_hz() requires a case or fallback date column.",
      x = paste(c(case_date_col, fallback_date_col), collapse = ", ")
    ))
  }
  if (!is.numeric(window_days) || length(window_days) != 1L ||
      window_days < 1) {
    rlang::abort("`window_days` must be a positive integer.")
  }
  if (!is.numeric(threshold_step_days) ||
      length(threshold_step_days) != 1L ||
      threshold_step_days < 1) {
    rlang::abort("`threshold_step_days` must be a positive integer.")
  }

  case_dates <- resolve_recent_case_date(
    data,
    case_date_col = case_date_col,
    fallback_date_col = fallback_date_col
  )
  hz_index <- resolve_recent_case_hz_index(data, hz_index)
  threshold_dates <- resolve_threshold_dates(
    case_dates = case_dates,
    threshold_dates = threshold_dates,
    window_days = as.integer(window_days),
    threshold_step_days = as.integer(threshold_step_days),
    analysis_start_date = analysis_start_date,
    analysis_end_date = analysis_end_date
  )
  windows <- build_recent_case_windows(
    threshold_dates = threshold_dates,
    window_days = as.integer(window_days),
    threshold_step_days = as.integer(threshold_step_days)
  )

  grid <- tidyr::crossing(windows, hz_index)

  if (nrow(grid) == 0L) {
    return(grid |> add_empty_recent_case_count())
  }

  recent_counts <- data |>
    dplyr::mutate(
      case_date = case_dates,
      is_confirmed = alert_is_confirmed_case(data)
    ) |>
    dplyr::filter(
      is_confirmed,
      !is.na(zone_sante_notification),
      !is.na(case_date)
    ) |>
    dplyr::inner_join(
      windows,
      by = dplyr::join_by(
        case_date >= recent_case_window_start,
        case_date <= recent_case_window_end
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::count(
      threshold_time_key,
      threshold_window_id,
      threshold_window_index,
      threshold_valid_from,
      threshold_valid_to,
      recent_case_window_start,
      recent_case_window_end,
      recent_case_window_days,
      recent_case_anchor_date,
      zone_sante_notification,
      name = "n_recent_confirmed"
    )

  out <- grid |>
    dplyr::left_join(
      recent_counts,
      by = dplyr::join_by(
        threshold_time_key,
        threshold_window_id,
        threshold_window_index,
        threshold_valid_from,
        threshold_valid_to,
        recent_case_window_start,
        recent_case_window_end,
        recent_case_window_days,
        recent_case_anchor_date,
        zone_sante_notification
      ),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      n_recent_confirmed = tidyr::replace_na(n_recent_confirmed, 0L)
    )

  n_windows <- nrow(windows)
  effective_copy <- min(n_copy_last, max(0L, n_windows - 1L))
  if (effective_copy < n_copy_last && n_windows > 0) {
    rlang::warn(sprintf("Only %d window(s); reduced n_copy_last to %d.", n_windows, effective_copy))
  }

  if (effective_copy > 0L) {
    out <- out |>
      dplyr::group_by(zone_sante_notification) |>
      dplyr::arrange(threshold_valid_from) |>
      dplyr::mutate(
        n_recent_confirmed = dplyr::if_else(
          dplyr::row_number() > n_windows - effective_copy,
          dplyr::nth(n_recent_confirmed, n_windows - effective_copy),
          n_recent_confirmed
        )
      ) |>
      dplyr::ungroup()
  }

  # Nowcast-adjusted count for windows overlapping the reporting-lag tail.
  if (!is.null(nowcast)) {
    ref_date <- attr(nowcast, "ref_date")
    if (is.null(ref_date)) ref_date <- max(nowcast$date, na.rm = TRUE)

    window_keys <- out |>
      dplyr::select(
        "threshold_time_key",
        "recent_case_window_start",
        "recent_case_window_end"
      ) |>
      dplyr::distinct()

    delta_by_window <- nowcast |>
      dplyr::filter(.data$date >= ref_date - max_delay + 1L) |>
      dplyr::mutate(
        delta = pmax(.data$nowcast_median - .data$observed, 0),
        delta_low = pmax(.data$nowcast_lower_90 - .data$observed, 0),
        delta_high = pmax(.data$nowcast_upper_90 - .data$observed, 0)
      ) |>
      dplyr::inner_join(
        window_keys,
        by = dplyr::join_by(
          date >= recent_case_window_start,
          date <= recent_case_window_end
        ),
        relationship = "many-to-many"
      ) |>
      dplyr::summarise(
        dplyr::across(
          c("delta", "delta_low", "delta_high"),
          \(x) sum(x, na.rm = TRUE)
        ),
        .by = c(
          "zone_sante_notification",
          "threshold_time_key",
          "recent_case_window_start",
          "recent_case_window_end"
        )
      )

    out <- out |>
      dplyr::left_join(
        delta_by_window,
        by = dplyr::join_by(
          zone_sante_notification,
          threshold_time_key,
          recent_case_window_start,
          recent_case_window_end
        ),
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        n_recent_confirmed_nowcast = dplyr::if_else(
          !is.na(.data$delta),
          as.integer(round(.data$n_recent_confirmed + .data$delta)),
          NA_integer_
        ),
        n_recent_confirmed_nowcast_low = dplyr::if_else(
          !is.na(.data$delta_low),
          .data$n_recent_confirmed + .data$delta_low,
          NA_real_
        ),
        n_recent_confirmed_nowcast_high = dplyr::if_else(
          !is.na(.data$delta_high),
          .data$n_recent_confirmed + .data$delta_high,
          NA_real_
        ),
        recent_count_source = dplyr::if_else(
          !is.na(.data$delta),
          "nowcast",
          "observed"
        )
      ) |>
      dplyr::select(-c("delta", "delta_low", "delta_high"))
  } else {
    out <- out |>
      dplyr::mutate(
        n_recent_confirmed_nowcast = NA_integer_,
        n_recent_confirmed_nowcast_low = NA_real_,
        n_recent_confirmed_nowcast_high = NA_real_,
        recent_count_source = "observed"
      )
  }

  out |>
    dplyr::arrange(threshold_valid_from, zone_sante_notification)
}

resolve_threshold_dates <- function(case_dates,
                                    threshold_dates,
                                    window_days,
                                    threshold_step_days,
                                    analysis_start_date,
                                    analysis_end_date) {
  if (!is.null(threshold_dates)) {
    return(sort(unique(as.Date(threshold_dates))))
  }

  valid_dates <- case_dates[!is.na(case_dates)]
  if (!is.null(analysis_start_date)) {
    valid_dates <- valid_dates[valid_dates >= as.Date(analysis_start_date)]
  }
  if (!is.null(analysis_end_date)) {
    valid_dates <- valid_dates[valid_dates <= as.Date(analysis_end_date)]
  }

  if (length(valid_dates) == 0L) {
    return(as.Date(character()))
  }

  first_case_date <- min(valid_dates)
  latest_case_date <- min(max(valid_dates), Sys.Date())
  if (!is.null(analysis_end_date)) {
    latest_case_date <- max(latest_case_date, as.Date(analysis_end_date))
  }

  if (as.integer(latest_case_date - first_case_date) + 1L < window_days) {
    return(as.Date(character()))
  }

  window_ends <- rev(seq.Date(from = latest_case_date, to = first_case_date, by = -window_days))
  threshold_dates <- window_ends - window_days + 1L

  threshold_dates
}

build_recent_case_windows <- function(threshold_dates,
                                      window_days,
                                      threshold_step_days) {
  tibble::tibble(
    threshold_window_index = seq_along(threshold_dates),
    threshold_valid_from = as.Date(threshold_dates),
    threshold_valid_to = threshold_valid_from + threshold_step_days - 1L,
    threshold_time_key = format(threshold_valid_from),
    recent_case_window_start = threshold_valid_from,
    recent_case_window_end = threshold_valid_from + window_days - 1L,
    recent_case_window_days = window_days,
    recent_case_anchor_date = threshold_valid_from,
    threshold_window_id = paste(
      format(recent_case_window_start),
      format(recent_case_window_end),
      sep = "__"
    )
  ) |>
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
    )
}

resolve_recent_case_hz_index <- function(data, hz_index) {
  if (is.null(hz_index)) {
    return(
      data |>
        dplyr::filter(!is.na(zone_sante_notification)) |>
        dplyr::distinct(zone_sante_notification)
    )
  }

  if (is.data.frame(hz_index)) {
    alert_required_columns(
      hz_index,
      "zone_sante_notification",
      "compute_recent_confirmed_windows_by_hz()"
    )

    return(
      hz_index |>
        dplyr::filter(!is.na(zone_sante_notification)) |>
        dplyr::distinct(zone_sante_notification)
    )
  }

  tibble::tibble(zone_sante_notification = hz_index) |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::distinct(zone_sante_notification)
}

resolve_recent_case_date <- function(data,
                                     case_date_col,
                                     fallback_date_col) {
  date_values <- list()

  if (case_date_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <- as.Date(data[[case_date_col]])
  }
  if (fallback_date_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <-
      as.Date(data[[fallback_date_col]])
  }

  purrr::reduce(date_values, dplyr::coalesce)
}

add_empty_recent_case_count <- function(data) {
  data |>
    dplyr::mutate(
      n_recent_confirmed = integer(),
      n_recent_confirmed_nowcast = integer(),
      n_recent_confirmed_nowcast_low = double(),
      n_recent_confirmed_nowcast_high = double(),
      recent_count_source = character()
    )
}
