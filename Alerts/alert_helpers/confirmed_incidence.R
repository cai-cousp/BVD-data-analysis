# Shared construction of confirmed-case incidence
#
# Centralises the line-list -> daily confirmed/positive count transformation
# used by the Rt, growth-rate and nowcasting helpers. This is the strategy
# previously written out in `R/test.R`: confirmed/positive cases, onset dates
# (primary + fallback), zero-filled through the reference date, with future
# onset dates dropped as data-entry errors.

#' Build a zero-filled daily count of confirmed/positive cases by onset date
#'
#' @param data Dataframe. EVD line list (or an HZ subset).
#' @param zone Character. Optional health zone to keep.
#' @param min_date Date. Earliest onset date to keep; `NULL` keeps all.
#' @param ref_date Date. Reference date; the series is zero-filled through
#'   this date and onset dates after it are dropped.
#' @param case_date_col Character. Preferred onset date column.
#' @param fallback_date_col Character. Fallback onset date column.
#' @param min_days Integer. Minimum series length required.
#'
#' @return Tibble with columns `date` and `I` (integer).
#' @export
build_confirmed_daily <- function(data,
                                  zone = NULL,
                                  min_date = NULL,
                                  ref_date = Sys.Date(),
                                  case_date_col = "alert_date_debut_symptoms",
                                  fallback_date_col = "s2_date_debut_signes_symptomes",
                                  min_days = 3L) {
  alert_required_columns(
    data,
    c("classification_finale", "lab_resultat_final"),
    "build_confirmed_daily()"
  )
  if (!any(c(case_date_col, fallback_date_col) %in% names(data))) {
    rlang::abort(
      "build_confirmed_daily() requires a case or fallback date column."
    )
  }

  build_daily_incidence(
    data = data,
    zone = zone,
    min_date = min_date,
    ref_date = ref_date,
    case_date_col = case_date_col,
    fallback_date_col = fallback_date_col,
    min_days = min_days,
    keep = alert_is_confirmed_case(data),
    empty_message = "No confirmed/positive cases with a valid onset date."
  )
}

#' Build a zero-filled daily count of confirmed deaths by onset date
#'
#' Same contract as `build_confirmed_daily()`, but counts confirmed cases that
#' are observed deaths according to `alert_is_dead()`.
#'
#' @inheritParams build_confirmed_daily
#'
#' @return Tibble with columns `date` and `I` (integer).
#' @export
build_confirmed_death_daily <- function(data,
                                        zone = NULL,
                                        min_date = NULL,
                                        ref_date = Sys.Date(),
                                        case_date_col = "alert_date_debut_symptoms",
                                        fallback_date_col = "s2_date_debut_signes_symptomes",
                                        min_days = 3L) {
  if (!any(c(case_date_col, fallback_date_col) %in% names(data))) {
    rlang::abort(
      "build_confirmed_death_daily() requires a case or fallback date column."
    )
  }

  build_daily_incidence(
    data = data,
    zone = zone,
    min_date = min_date,
    ref_date = ref_date,
    case_date_col = case_date_col,
    fallback_date_col = fallback_date_col,
    min_days = min_days,
    keep = alert_is_dead(data),
    empty_message = "No confirmed deaths with a valid onset date."
  )
}

#' Shared zero-filled daily incidence builder
#' @noRd
build_daily_incidence <- function(data,
                                  zone,
                                  min_date,
                                  ref_date,
                                  case_date_col,
                                  fallback_date_col,
                                  min_days,
                                  keep,
                                  empty_message) {
  date_values <- list()
  if (case_date_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <- as.Date(data[[case_date_col]])
  }
  if (fallback_date_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <-
      as.Date(data[[fallback_date_col]])
  }
  onset_dates <- purrr::reduce(date_values, dplyr::coalesce)

  ref_date <- as.Date(ref_date)
  min_date <- if (is.null(min_date)) NULL else as.Date(min_date)

  hz_cases <- data |>
    dplyr::mutate(
      date = onset_dates
    ) |>
    dplyr::filter(
      keep,
      !is.na(.data$date),
      .data$date <= ref_date
    )

  if (!is.null(zone)) {
    alert_required_columns(data, "zone_sante_notification",
                           "build_confirmed_daily()")
    hz_cases <- hz_cases |>
      dplyr::filter(.data$zone_sante_notification == zone)
  }
  if (!is.null(min_date)) {
    hz_cases <- hz_cases |>
      dplyr::filter(.data$date >= min_date)
  }

  if (nrow(hz_cases) == 0L) {
    rlang::abort(empty_message)
  }

  hz_cases <- hz_cases |>
    dplyr::count(.data$date, name = "I")

  dates_full <- seq.Date(min(hz_cases$date), ref_date, by = "day")
  hz_full <- tibble::tibble(date = dates_full) |>
    dplyr::left_join(hz_cases, by = "date") |>
    tidyr::replace_na(list(I = 0L))

  if (nrow(hz_full) < min_days) {
    rlang::abort(
      sprintf(
        "Onset series has %d days; at least %d are required.",
        nrow(hz_full),
        min_days
      )
    )
  }

  hz_full
}

#' Replace the truncated tail of a daily count with nowcast values
#'
#' For each date in the last `max_delay` days before `ref_date`, the observed
#' count is replaced by the nowcast median (rounded, floored at the observed
#' count). Dates without a nowcast value keep their observed count.
#'
#' @param counts Tibble with columns `date` and `I`.
#' @param nowcasts Tibble with columns `date` and `nowcast_median`
#'   (optionally a `zone_sante_notification` column).
#' @param zone Character. Optional health zone to match in `nowcasts`.
#' @param ref_date Date. Reference date defining the tail.
#' @param max_delay Integer. Tail length in days.
#'
#' @return `counts` with `I` replaced in the tail; same columns as input.
#' @export
splice_nowcast_tail <- function(counts,
                                nowcasts,
                                zone = NULL,
                                ref_date = max(counts$date),
                                max_delay = 21L) {
  alert_required_columns(counts, c("date", "I"), "splice_nowcast_tail()")
  if (nrow(nowcasts) == 0L) {
    return(counts)
  }
  alert_required_columns(nowcasts, "nowcast_median", "splice_nowcast_tail()")

  ref_date <- as.Date(ref_date)
  max_delay <- as.integer(max_delay)

  tail_nowcasts <- nowcasts |>
    dplyr::filter(
      .data$date >= ref_date - max_delay + 1L,
      .data$date <= ref_date
    )
  if (!is.null(zone) && "zone_sante_notification" %in% names(tail_nowcasts)) {
    tail_nowcasts <- tail_nowcasts |>
      dplyr::filter(.data$zone_sante_notification == zone)
  }
  if (nrow(tail_nowcasts) == 0L) {
    return(counts)
  }

  counts |>
    dplyr::left_join(
      tail_nowcasts |>
        dplyr::select("date", "nowcast_median"),
      by = "date",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      I = dplyr::coalesce(
        pmax(as.integer(round(.data$nowcast_median)), .data$I),
        .data$I
      ),
      I = as.integer(pmax(.data$I, 0L))
    ) |>
    dplyr::select(-dplyr::any_of("nowcast_median"))
}

#' Should a nowcast be applied to this line-list subset?
#'
#' Used by cumulative-total helpers to avoid applying a current nowcast to
#' historical (cumulative) data subsets: the correction only applies when the
#' subset's latest onset date is inside the nowcast horizon.
#'
#' @param nowcast Dataframe. Nowcast table from `compute_nowcasts_by_zone()`.
#' @param data Dataframe. Line-list subset.
#' @param ref_date Date. Optional explicit reference date.
#' @param max_delay Integer. Nowcast horizon in days.
#' @param case_date_col, fallback_date_col Character. Onset columns.
#'
#' @return Logical scalar.
#' @export
nowcast_applies_to_data <- function(nowcast,
                                    data,
                                    ref_date = NULL,
                                    max_delay = 21L,
                                    case_date_col = "alert_date_debut_symptoms",
                                    fallback_date_col = "s2_date_debut_signes_symptomes") {
  if (is.null(nowcast) || nrow(nowcast) == 0L) return(FALSE)

  ref <- ref_date
  if (is.null(ref)) ref <- attr(nowcast, "ref_date")
  if (is.null(ref)) ref <- max(nowcast$date, na.rm = TRUE)
  if (length(ref) == 0L || is.na(as.Date(ref))) return(FALSE)

  date_values <- list()
  if (case_date_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <- as.Date(data[[case_date_col]])
  }
  if (fallback_date_col %in% names(data)) {
    date_values[[length(date_values) + 1L]] <-
      as.Date(data[[fallback_date_col]])
  }
  if (length(date_values) == 0L) return(FALSE)

  onset <- purrr::reduce(date_values, dplyr::coalesce)
  latest_onset <- max(onset, na.rm = TRUE)
  is.finite(latest_onset) &&
    latest_onset >= as.Date(ref) - as.integer(max_delay) + 1L
}
