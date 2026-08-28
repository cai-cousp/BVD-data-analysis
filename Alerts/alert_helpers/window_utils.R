# Shared time-window utilities for the alert pipeline
#
# Single source of truth for the "analysis grid": the backward-anchored,
# non-overlapping 7-day windows produced by
# `compute_recent_confirmed_windows_by_hz()` / `build_recent_case_windows()`
# (window metadata documented by `alert_window_metadata_columns()` in
# cfr_backcalc_utils.R).
#
# Convention (harmonized, 2026-08-16):
#   The analysis grid is the ONLY weekly time axis used for threshold
#   estimation and trend analysis. Indicators, alert counts, and nowcast
#   deltas are binned onto the grid via `bin_dates_by_grid()`. Calendar-week
#   (ISO/Monday) binning with `floor_date(..., week_start = 1)` is NOT used
#   for live indicators; it remains only as a fallback for legacy/static
#   calls that carry no window metadata, and for the EVD10 historical
#   benchmark (`compute_epiweek()`).


#' Bin event dates onto the analysis grid
#'
#' Range-joins `data` to the analysis grid so each event is assigned to the
#' single 7-day window containing its date. Events outside the grid are
#' dropped. Adds `week_bin` (= `recent_case_window_start`, the grid window
#' start) and, when present on the grid, `threshold_time_key`.
#'
#' @param data Dataframe with the event date column.
#' @param windows Dataframe of grid windows with at least
#'   `recent_case_window_start` and `recent_case_window_end`; optionally
#'   `zone_sante_notification` (per-HZ windows) and `threshold_time_key`.
#' @param date_col Character. Event date column in `data`.
#' @return `data` with `week_bin` and `threshold_time_key` (when on the grid),
#'   restricted to events that fall inside a grid window.
bin_dates_by_grid <- function(data, windows, date_col) {
  if (!all(
    c("recent_case_window_start", "recent_case_window_end") %in%
      names(windows)
  )) {
    rlang::abort(
      "`windows` must contain recent_case_window_start and recent_case_window_end."
    )
  }
  if (!date_col %in% names(data)) {
    rlang::abort(sprintf("`data` has no column '%s'.", date_col))
  }

  # Fixed internal column name so the range join avoids dynamic join_by()
  # injection.
  data <- data |>
    dplyr::rename(.event_date = dplyr::all_of(date_col))

  by_expr <- if ("zone_sante_notification" %in% names(windows)) {
    dplyr::join_by(
      zone_sante_notification,
      .event_date >= recent_case_window_start,
      .event_date <= recent_case_window_end
    )
  } else {
    dplyr::join_by(
      .event_date >= recent_case_window_start,
      .event_date <= recent_case_window_end
    )
  }

  data |>
    dplyr::inner_join(
      windows |>
        dplyr::select(dplyr::any_of(c(
          "zone_sante_notification",
          "recent_case_window_start",
          "recent_case_window_end",
          "threshold_time_key"
        ))),
      by = by_expr,
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(week_bin = as.Date(recent_case_window_start)) |>
    dplyr::select(-dplyr::all_of(
      c("recent_case_window_start", "recent_case_window_end")
    )) |>
    dplyr::rename(!!rlang::sym(date_col) := .event_date)
}

#' Resolve the analysis grid from window-carrying parameters
#'
#' Extracts the distinct windows from `hz_parameters` (a per-HZ tibble that
#' carries `recent_case_window_start`/`end` and optionally
#' `threshold_time_key`). Used when a caller did not pass an explicit grid.
#'
#' @param hz_parameters Dataframe with `zone_sante_notification` and window
#'   columns.
#' @return A tibble of distinct windows (per HZ), or `NULL` when no window
#'   columns are present.
derive_window_grid <- function(hz_parameters) {
  if (!all(
    c("recent_case_window_start", "recent_case_window_end") %in%
      names(hz_parameters)
  )) {
    return(NULL)
  }

  grid_cols <- c(
    "zone_sante_notification",
    "recent_case_window_start",
    "recent_case_window_end"
  )
  if ("threshold_time_key" %in% names(hz_parameters)) {
    grid_cols <- c(grid_cols, "threshold_time_key")
  }

  hz_parameters |>
    dplyr::select(dplyr::all_of(grid_cols)) |>
    dplyr::distinct()
}
