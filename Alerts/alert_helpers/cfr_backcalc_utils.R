# Shared utilities for CFR back-calculation helpers.

alert_required_columns <- function(data, required, caller) {
  missing <- setdiff(required, names(data))

  if (length(missing) > 0) {
    rlang::abort(c(
      paste0(caller, " requires columns that are missing from the data."),
      x = paste(missing, collapse = ", ")
    ))
  }

  invisible(data)
}

#' Resolve a single case/onset date per row
#'
#' Coalesces the preferred onset date column onto the fallback column.
#' Shared by the windowed indicator helpers so every window subset is
#' filtered on the same resolved onset date.
#'
#' @param data Dataframe. Line list data.
#' @param case_date_col Character. Preferred date column.
#' @param fallback_date_col Character. Fallback date column.
#'
#' @return A Date vector of length `nrow(data)`.
alert_resolve_case_date <- function(data,
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

  if (length(date_values) == 0L) {
    return(as.Date(rep(NA, nrow(data))))
  }

  purrr::reduce(date_values, dplyr::coalesce)
}

#' Resolve a single alert notification date per row
#'
#' Coalesces the preferred notification date column with fallback investigation
#' and laboratory dates.
#'
#' @param data Dataframe. Line list data.
#' @param notification_col Character. Preferred notification date column.
#'   Default: `"date_heure_notification_alerte"`.
#' @param fallback_cols Character vector. Fallback date columns.
#'   Default: `c("date_investigation", "lab_date_reception", "lab_date_prelevement")`.
#'
#' @return A Date vector of length `nrow(data)`.
alert_resolve_notification_date <- function(data,
                                            notification_col = "date_heure_notification_alerte",
                                            fallback_cols = c("date_investigation", "lab_date_reception", "lab_date_prelevement")) {
  cols_to_check <- c(notification_col, fallback_cols)
  date_values <- list()
  for (col in cols_to_check) {
    if (col %in% names(data)) {
      date_values[[length(date_values) + 1L]] <- as.Date(data[[col]])
    }
  }
  if (length(date_values) == 0L) {
    return(as.Date(rep(NA, nrow(data))))
  }
  purrr::reduce(date_values, dplyr::coalesce)
}

#' Names of the shared window-metadata columns
#'
#' These are the nine columns produced by `build_recent_case_windows()` and
#' carried by `hz_recent_cases` (see
#' `compute_recent_confirmed_windows_by_hz()`).
#'
#' @param names Character vector of available column names.
#'
#' @return Character vector of window-metadata columns present in `names`.
alert_window_metadata_columns <- function(names) {
  intersect(
    c(
      "threshold_time_key",
      "threshold_window_id",
      "threshold_window_index",
      "threshold_valid_from",
      "threshold_valid_to",
      "recent_case_window_start",
      "recent_case_window_end",
      "recent_case_window_days",
      "recent_case_anchor_date"
    ),
    names
  )
}

#' Extract the distinct analysis time grid from a window table
#'
#' Accepts either the full HZ x window grid (e.g. `hz_recent_cases`) or a
#' window-only tibble and returns one row per distinct window.
#'
#' @param windows Dataframe with window-metadata columns.
#'
#' @return Tibble with one row per distinct window.
alert_window_grid <- function(windows) {
  alert_required_columns(
    windows,
    c("threshold_time_key", "recent_case_window_end"),
    "alert_window_grid()"
  )

  meta_cols <- alert_window_metadata_columns(names(windows))

  windows |>
    dplyr::select(dplyr::all_of(meta_cols)) |>
    dplyr::distinct() |>
    dplyr::arrange(threshold_valid_from)
}

#' Bind window metadata to a per-window estimate
#'
#' Prepends the window-metadata columns of a one-row window tibble to a
#' per-window result, recycling the metadata across every row.
#'
#' @param data Dataframe. Per-window estimates.
#' @param window_info One-row tibble of window metadata.
#'
#' @return `data` with the window-metadata columns prepended.
alert_bind_window_metadata <- function(data, window_info) {
  meta_cols <- alert_window_metadata_columns(names(window_info))
  missing_meta <- setdiff(meta_cols, names(window_info))

  if (length(missing_meta) > 0L) {
    rlang::abort(c(
      "Window metadata columns missing from `window_info`.",
      x = paste(missing_meta, collapse = ", ")
    ))
  }

  window_meta <- window_info |>
    dplyr::select(dplyr::all_of(meta_cols)) |>
    dplyr::slice(rep(1L, nrow(data)))

  dplyr::bind_cols(window_meta, data)
}

#' Distinct health-zone index from line-list data
#'
#' @param data Dataframe with a `zone_sante_notification` column.
#'
#' @return Tibble with one row per non-missing health zone.
alert_hz_index <- function(data) {
  data |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::distinct(zone_sante_notification) |>
    dplyr::arrange(zone_sante_notification)
}

#' Nowcast tail delta for a data subset, optionally truncated at a window end
#'
#' Applies `nowcast_applies_to_data()` (so historical cumulative subsets are
#' not corrected) and then sums the nowcast delta per zone, optionally only
#' through `through_date`.
#'
#' @param nowcast Dataframe. Nowcast table from `compute_nowcasts_by_zone()`.
#' @param data Dataframe. Line-list subset.
#' @param ref_date Date. Optional explicit nowcast reference date.
#' @param max_delay Integer. Nowcast horizon in days.
#' @param case_date_col,fallback_date_col Character. Onset columns.
#' @param through_date Date. Optional upper bound for the summed tail.
#'
#' @return One-row-per-zone tibble with `delta_median`, `delta_low` and
#'   `delta_high` (empty when the nowcast does not apply).
nowcast_delta_for_data <- function(nowcast,
                                   data,
                                   ref_date,
                                   max_delay,
                                   case_date_col,
                                   fallback_date_col,
                                   through_date = NULL) {
  empty_delta <- tibble::tibble(
    zone_sante_notification = character(),
    delta_median = double(),
    delta_low = double(),
    delta_high = double()
  )

  if (!nowcast_applies_to_data(
    nowcast,
    data,
    ref_date = ref_date,
    max_delay = max_delay,
    case_date_col = case_date_col,
    fallback_date_col = fallback_date_col
  )) {
    return(empty_delta)
  }

  nowcast_total_delta(nowcast, through_date = through_date) |>
    dplyr::transmute(
      .data$zone_sante_notification,
      delta_median = .data$delta_median,
      delta_low = .data$delta_lower,
      delta_high = .data$delta_upper
    )
}

alert_is_confirmed_case <- function(data) {
  alert_required_columns(
    data,
    c("classification_finale", "lab_resultat_final"),
    "alert_is_confirmed_case()"
  )

  classification <- dplyr::coalesce(
    as.character(data$classification_finale),
    ""
  )
  lab_result <- dplyr::coalesce(as.character(data$lab_resultat_final), "")

  classification == "Cas confirmé" | lab_result == "Positif"
}

alert_has_non_negative_lab <- function(data, no_lab_values = c("")) {
  alert_required_columns(
    data,
    "lab_resultat_final",
    "alert_has_non_negative_lab()"
  )

  lab_raw <- as.character(data$lab_resultat_final)
  lab_result <- stringr::str_squish(dplyr::coalesce(lab_raw, ""))
  no_lab_values <- stringr::str_squish(no_lab_values)

  lab_result == "Positif" | lab_result %in% no_lab_values
}

alert_is_observed_death <- function(data) {
  alert_required_columns(
    data,
    c("nature_alerte", "s6_statut_final_patient"),
    "alert_is_observed_death()"
  )

  alert_status <- dplyr::coalesce(as.character(data$nature_alerte), "")
  final_status <- dplyr::coalesce(
    as.character(data$s6_statut_final_patient),
    ""
  )

  alert_status == "Décédé" | final_status == "Décédé"
}

#' Is a confirmed case an observed death?
#'
#' A confirmed case is counted as dead when any status field indicates
#' `"Décédé"` or when a death date is present. `s6_date_deces` is the primary
#' date-based death indicator, with `date_de_deces` as a secondary one.
#'
#' @param data Dataframe. EVD line list.
#'
#' @return Logical vector of length `nrow(data)`.
#' @export
alert_is_dead <- function(data) {
  alert_required_columns(
    data,
    c(
      "classification_finale",
      "lab_resultat_final",
      "s6_statut_final_patient",
      "s5_statut_patient_lors_prelev",
      "nature_alerte",
      "date_de_deces",
      "s6_date_deces"
    ),
    "alert_is_dead()"
  )

  s6_status <- dplyr::coalesce(
    as.character(data$s6_statut_final_patient),
    ""
  )
  s5_status <- dplyr::coalesce(
    as.character(data$s5_statut_patient_lors_prelev),
    ""
  )
  alert_status <- dplyr::coalesce(
    as.character(data$nature_alerte),
    ""
  )

  status_dead <- s6_status == "Décédé" |
    s5_status == "Décédé" |
    alert_status == "Décédé"
  date_dead <- !is.na(as.Date(data$s6_date_deces)) |
    !is.na(as.Date(data$date_de_deces))

  alert_is_confirmed_case(data) & (status_dead | date_dead)
}

alert_resolve_death_date <- function(
    data,
    death_date_cols = c(
      "alert_date_deces",
      "s1_si_decede_date_deces",
      "s6_date_deces"
    )) {
  available_cols <- intersect(death_date_cols, names(data))

  if (length(available_cols) == 0) {
    return(as.Date(rep(NA, nrow(data))))
  }

  date_values <- purrr::map(data[available_cols], as.Date)
  purrr::reduce(date_values, dplyr::coalesce)
}

#' Resolve the death/report date used for the confirmed-death nowcast
#'
#' Death dates are coalesced in the order supplied by `death_date_cols`
#' (`s6_date_deces` then `date_de_deces` by default) and finally fall back to
#' the notification date when no death date is available. The notification
#' fallback is a reporting proxy only; it does not classify a row as dead.
#'
#' @param data Dataframe. EVD line list.
#' @param death_date_cols Character vector of death date columns in priority
#'   order.
#' @param report_fallback_col Character. Notification/report date column used
#'   when every death date is missing.
#'
#' @return A Date vector of length `nrow(data)`.
#' @export
alert_resolve_death_report_date <- function(
    data,
    death_date_cols = c("s6_date_deces", "date_de_deces"),
    report_fallback_col = "date_heure_notification_alerte") {
  date_values <- list()

  for (col in death_date_cols) {
    if (col %in% names(data)) {
      date_values[[length(date_values) + 1L]] <- as.Date(data[[col]])
    }
  }
  if (report_fallback_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <-
      as.Date(data[[report_fallback_col]])
  }

  if (length(date_values) == 0L) {
    return(as.Date(rep(NA, nrow(data))))
  }

  purrr::reduce(date_values, dplyr::coalesce)
}

alert_binom_ci <- function(x, n, level = 0.95) {
  if (!is.finite(x) || !is.finite(n) || n <= 0 || x < 0 || x > n) {
    return(c(low = NA_real_, high = NA_real_))
  }

  ci <- stats::binom.test(round(x), round(n), conf.level = level)$conf.int
  c(low = ci[[1]], high = ci[[2]])
}

alert_valid_ratio <- function(numerator, denominator) {
  ratio <- numerator / denominator
  dplyr::if_else(
    is.finite(ratio) & denominator > 0,
    ratio,
    NA_real_
  )
}
