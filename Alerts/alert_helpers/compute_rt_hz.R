#' Compute Rt by HZ
#'
#' Estimates the effective reproduction number (Rt) from confirmed/positive
#' cases with symptom onset up to `end_date`. By default the whole dataset
#' period is used; pass `window_days` to restrict to a trailing window of that
#' length in days. By default the anchor is the most recent confirmed/positive
#' onset date in `hz_data`.
#'
#' When `nowcast` is supplied (output of `compute_nowcasts_by_zone()`), the
#' truncated tail of the window is replaced with nowcast counts before Rt is
#' estimated; otherwise raw counts are used unchanged.
#'
#' The value returned is the last `Mean(R)` of the `estimate_R()` sliding
#' windows — the current reproduction number at the end of the series.
#'
#' @param hz_data Dataframe. Data filtered for a specific HZ.
#' @param window_days Numeric. Length in days of the trailing incidence window;
#'   `NULL` (default) uses the whole dataset period up to `end_date`.
#' @param end_date Date. Anchor date for the trailing window; `NULL` uses the
#'   most recent confirmed/positive onset date in `hz_data`.
#' @param case_date_col Character. Preferred case date column.
#' @param fallback_date_col Character. Fallback date column.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param ref_date Date. Reference date used to build the daily series.
#' @param max_delay Integer. Nowcast tail length in days.
#' @param min_date Date. Optional lower bound for the daily series.
compute_rt_hz <- function(hz_data,
                          window_days = NULL,
                          end_date = NULL,
                          case_date_col = "alert_date_debut_symptoms",
                          fallback_date_col = "s2_date_debut_signes_symptomes",
                          nowcast = NULL,
                          ref_date = Sys.Date(),
                          max_delay = 21L,
                          min_date = NULL) {
  if (!is.null(window_days) &&
      (!is.numeric(window_days) || length(window_days) != 1L || window_days < 1)) {
    rlang::abort("`window_days` must be a positive number or NULL (whole dataset period).")
  }

  hz_full <- tryCatch(
    build_confirmed_daily(
      hz_data,
      min_date = min_date,
      ref_date = ref_date,
      case_date_col = case_date_col,
      fallback_date_col = fallback_date_col,
      min_days = 3L
    ),
    error = function(e) NULL
  )
  if (is.null(hz_full)) return(NA_real_)

  # Anchor on confirmed/positive onset dates unless an explicit end date is
  # supplied (e.g. the window end in the cumulative 01b pipeline).
  if (is.null(end_date)) end_date <- max(hz_full$date, na.rm = TRUE)
  end_date <- as.Date(end_date)

  if (is.null(window_days)) {
    hz_cases <- hz_full |>
      dplyr::filter(.data$date <= end_date)
  } else {
    hz_cases <- hz_full |>
      dplyr::filter(
        .data$date >= (end_date - window_days),
        .data$date <= end_date
      )
  }

  if (!is.null(nowcast)) {
    zone <- NULL
    if ("zone_sante_notification" %in% names(hz_data)) {
      zone_values <- unique(
        hz_data$zone_sante_notification[!is.na(hz_data$zone_sante_notification)]
      )
      if (length(zone_values) == 1L) zone <- zone_values[[1L]]
    }
    hz_cases <- splice_nowcast_tail(
      hz_cases,
      nowcast,
      zone = zone,
      ref_date = end_date,
      max_delay = max_delay
    )
  }

  if (sum(hz_cases$I, na.rm = TRUE) < 3) return(NA_real_)
  if (nrow(hz_cases) < 3) return(NA_real_)

  # Standard SI for EVD: mean 12, sd 5 (Uganda 2000-01)
  tryCatch(
    {
      res <- EpiEstim::estimate_R(
        hz_cases$I,
        method = "parametric_si",
        config = EpiEstim::make_config(list(
          mean_si = 12, std_si = 5
        ))
      )
      dplyr::last(res$R$`Mean(R)`)
    },
    error = function(e) NA_real_
  )
}

#' Compute Rt per health zone per time window
#'
#' Iterates the shared analysis grid (`hz_recent_cases` / `alert_window_grid`)
#' and estimates Rt for each health zone on the whole dataset period (or a
#' trailing window of `window_days`) ending at each window end, using
#' cumulative line-list data up to that window end. Rows where the
#' zone-specific estimate is unavailable fall back to the pooled (all-zone)
#' Rt for the same window.
#'
#' @param data Dataframe. EVD line list.
#' @param windows Dataframe. Shared time-window grid (distinct window metadata
#'   or a full HZ x window table such as `hz_recent_cases`).
#' @param window_days Numeric. Length in days of the trailing incidence
#'   window used by `compute_rt_hz()`; `NULL` (default) uses the whole
#'   dataset period up to each window end.
#' @param case_date_col Character. Preferred case date column.
#' @param fallback_date_col Character. Fallback date column.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param max_delay Integer. Nowcast tail length in days.
#' @return Tibble with one row per health zone per time window and columns
#'   `rt`, `pooled_rt` and `rt_used`.
compute_rt_by_hz <- function(data,
                             windows,
                             window_days = NULL,
                             case_date_col = "alert_date_debut_symptoms",
                             fallback_date_col = "s2_date_debut_signes_symptomes",
                             nowcast = NULL,
                             max_delay = 21L) {
  if (is.null(windows)) {
    rlang::abort("`compute_rt_by_hz()` requires a shared `windows` grid.")
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
      subset <- data |>
        dplyr::mutate(.alert_resolved_date = resolved_date) |>
        dplyr::filter(
          !is.na(.alert_resolved_date),
          .alert_resolved_date <= w$recent_case_window_end
        ) |>
        dplyr::select(-.alert_resolved_date)

      rows <- purrr::map_dfr(
        hz_index$zone_sante_notification,
        \(zone) {
          rt <- compute_rt_hz(
            hz_data = subset |>
              dplyr::filter(zone_sante_notification == .env$zone),
            window_days = window_days,
            end_date = w$recent_case_window_end,
            ref_date = w$recent_case_window_end,
            nowcast = nowcast,
            max_delay = max_delay
          )
          tibble::tibble(
            zone_sante_notification = zone,
            rt = rt
          )
        }
      )

      rows |>
        dplyr::mutate(
          pooled_rt = dplyr::if_else(
            sum(is.finite(rt), na.rm = TRUE) > 0,
            mean(rt, na.rm = TRUE),
            NA_real_
          ),
          rt_used = dplyr::if_else(is.finite(rt), rt, pooled_rt)
        ) |>
        alert_bind_window_metadata(w)
    }
  ) |>
    dplyr::arrange(threshold_valid_from, zone_sante_notification)
}
