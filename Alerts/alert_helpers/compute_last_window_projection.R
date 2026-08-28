#' Project confirmed cases for the last rolling window using exponential growth
#'
#' Rather than using raw observed counts (which may under-count due to late
#' reporting), this helper replaces the last window's `n_recent_confirmed` with
#' an exponential projection anchored on the incidence fit from the growth-rate
#' estimation window.
#'
#' The projection formula is:
#'   N(t) = N_0 * exp(r * t)
#' where:
#'   N_0 = predicted count at the last day of the fitting window
#'         (`predicted_last_count` from `compute_growth_rate_by_hz()`)
#'   r   = exponential growth rate from the fit (`r_used`)
#'   t   = projection horizon in days (default 7, the length of one window)
#'
#' When the projection is not possible (N_0 missing or non-positive, r
#' missing), the observed count is retained and `projection_method` is set to
#' `"observed_fallback"`.
#'
#' @param hz_recent_cases Tibble from
#'   `compute_recent_confirmed_windows_by_hz()`.
#' @param hz_growth Tibble from `compute_growth_rate_by_hz()`.
#' @param projection_days Integer. Days to project forward from N_0.
#' @param max_fold_change Numeric. Cap projected value at
#'   `N_0 * max_fold_change` to guard against implausible extrapolation when
#'   `r` is large and positive.
#' @return Tibble with the same shape as `hz_recent_cases`, with
#'   `n_recent_confirmed` replaced for the last window and an extra column
#'   `projection_method` ("observed", "exponential", or "observed_fallback").
compute_last_window_projection <- function(hz_recent_cases,
                                           hz_growth,
                                           projection_days = 1L,
                                           max_fold_change = 10) {
  alert_required_columns(
    hz_recent_cases,
    c("threshold_time_key", "zone_sante_notification", "n_recent_confirmed"),
    "compute_last_window_projection()"
  )
  alert_required_columns(
    hz_growth,
    c("zone_sante_notification", "r_used", "predicted_last_count"),
    "compute_last_window_projection()"
  )
  if (!is.numeric(projection_days) || length(projection_days) != 1L ||
      projection_days < 1L) {
    rlang::abort("`projection_days` must be a positive integer.")
  }
  if (!is.numeric(max_fold_change) || length(max_fold_change) != 1L ||
      max_fold_change <= 1) {
    rlang::abort("`max_fold_change` must be a number greater than 1.")
  }

  if (nrow(hz_recent_cases) == 0L) {
    return(hz_recent_cases |>
      dplyr::mutate(projection_method = character()))
  }

  has_nowcast <- "n_recent_confirmed_nowcast" %in% names(hz_recent_cases)
  last_threshold_time_key <- max(hz_recent_cases$threshold_time_key)

  # Tag every row as observed to start
  out <- hz_recent_cases |>
    dplyr::mutate(projection_method = "observed")

  last_mask <- out$threshold_time_key == last_threshold_time_key
  if (!any(last_mask)) {
    return(out)
  }

  # Pull the projection inputs from the growth-rate table (one row per
  # HZ x window when the growth table is windowed)
  growth_lookup <- hz_growth |>
    dplyr::select(
      zone_sante_notification,
      dplyr::any_of("threshold_time_key"),
      r_for_projection = r_used,
      n0 = predicted_last_count
    )

  # Build projected values for last-window rows
  last_rows <- out[last_mask, , drop = FALSE] |>
    dplyr::left_join(
      growth_lookup,
      by = if (all(c("threshold_time_key") %in% names(out)) &&
        all(c("threshold_time_key") %in% names(growth_lookup))) {
        dplyr::join_by(zone_sante_notification, threshold_time_key)
      } else {
        dplyr::join_by(zone_sante_notification)
      },
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      nowcast_value = if (.env$has_nowcast) {
        .data$n_recent_confirmed_nowcast
      } else {
        NA_integer_
      },
      projected = dplyr::case_when(
        is.finite(n0) & n0 > 0 ~ n0,
        .default = NA_real_
      ),
      projected_cap = dplyr::if_else(
        is.finite(n0), n0 * max_fold_change, NA_real_
      ),
      projected_capped = !is.na(projected) &
        is.finite(projected_cap) &
        projected > projected_cap,
      projected = dplyr::if_else(projected_capped, projected_cap, projected),
      projection_method = dplyr::case_when(
        .env$has_nowcast & !is.na(.data$nowcast_value) ~ "nowcast",
        !is.na(projected) ~ "exponential",
        .default = "observed_fallback"
      ),
      n_recent_confirmed = dplyr::coalesce(
        as.integer(.data$nowcast_value),
        as.integer(round(.data$projected)),
        .data$n_recent_confirmed
      )
    ) |>
    dplyr::select(
      -r_for_projection, -n0, -nowcast_value,
      -projected, -projected_cap, -projected_capped
    )

  # Splice projected last-window rows back in
  out[last_mask, ] <- last_rows
  out
}
