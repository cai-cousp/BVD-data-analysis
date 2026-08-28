# Nowcasting of the daily confirmed-case series by symptom-onset date.
#
# `hz_full` counts new confirmed/positive EVD cases by onset date. The most
# recent entries are right-truncated: cases with a recent onset are still in
# the notification / laboratory pipeline and are not yet visible in the line
# list. These helpers rebuild that series, characterise the onset -> lab
# confirmation delay, fit an EpiNow2 nowcast that corrects the truncated tail,
# and reshape the posterior into tidy tables.
#
# Model orientation: because the observed series is keyed by *onset* date
# rather than report date, no convolution delay is used (`delay_opts()`) and
# the onset -> lab delay enters the model as the *truncation* distribution
# (`trunc_opts()`). With that setup `obs_reports(t) = infections(t) * F(T-t)`
# and the `infections` variable is the truncation-adjusted nowcast of the true
# onset-date counts.

#' Derive the reference ("now") date from the line-list notification dates
#'
#' The nowcast horizon should end at the last date for which the surveillance
#' system has actually reported, not at the system date: when the line list is
#' stale, zero-filling through `Sys.Date()` would fabricate days with no data.
#' This helper returns the most recent notification date in `evd` that is not
#' later than `max_date`; notification dates after the system date are treated
#' as data-entry errors and excluded.
#'
#' @param evd Dataframe. Cleaned EVD line list.
#' @param notification_col Character. Notification date column.
#' @param max_date Date. Upper bound for the notification dates; defaults to
#'   the system date.
#'
#' @return A single Date.
#' @export
evd_notification_ref_date <- function(evd,
                                      notification_col = "date_heure_notification_alerte",
                                      max_date = Sys.Date()) {
  if (!notification_col %in% names(evd)) {
    rlang::abort(
      sprintf("`evd` must contain a '%s' column.", notification_col)
    )
  }
  max_date <- as.Date(max_date)
  dates <- as.Date(evd[[notification_col]])
  dates <- dates[!is.na(dates) & dates <= max_date]
  if (length(dates) == 0L) {
    rlang::abort(
      sprintf(
        "No notification date <= %s found in `evd`.",
        format(max_date, "%Y-%m-%d")
      )
    )
  }
  max(dates)
}

#' Build a zero-filled daily count of confirmed/positive cases by onset date
#'
#' @param evd Dataframe. Cleaned EVD line list.
#' @param min_date Date. Earliest onset date to keep; `NULL` keeps all.
#' @param ref_date Date. Reference ("now") date; `NULL` uses the most recent
#'   notification date in `evd` that is not later than the system date. Onset
#'   dates after this are treated as data-entry errors and dropped.
#' @param onset_col Character. Onset date column.
#' @param min_days Integer. Minimum number of days required in the series.
#'
#' @return Tibble with columns `date` and `I`.
#' @export
build_hz_full <- function(evd,
                          min_date = NULL,
                          ref_date = NULL,
                          onset_col = "alert_date_debut_symptoms",
                          min_days = 3L) {
  required <- c(onset_col, "classification_finale", "lab_resultat_final")
  missing_cols <- setdiff(required, names(evd))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      c(
        "Missing required columns in `evd`.",
        i = paste0("Missing: ", paste(missing_cols, collapse = ", "))
      )
    )
  }

  ref_date <- if (is.null(ref_date)) {
    evd_notification_ref_date(evd)
  } else {
    as.Date(ref_date)
  }

  hz_cases <- evd |>
    dplyr::mutate(date = as.Date(.data[[onset_col]])) |>
    dplyr::filter(
      .data$classification_finale == "Cas confirmé" |
        .data$lab_resultat_final == "Positif",
      !is.na(.data$date),
      .data$date <= ref_date
    )

  if (!is.null(min_date)) {
    min_date <- as.Date(min_date)
    hz_cases <- hz_cases |> dplyr::filter(.data$date >= min_date)
  }

  if (nrow(hz_cases) == 0L) {
    rlang::abort("No confirmed/positive cases with a valid onset date.")
  }

  hz_cases <- hz_cases |>
    dplyr::count(.data$date, name = "I")

  # Zero-fill through the reference date so the nowcast horizon always ends at
  # "now" (the last few onset dates are typically still empty in the line list).
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

#' Build the observed onset -> lab confirmation delay sample
#'
#' Only delays that are fully observable at the reference date are kept: the
#' onset must be at least `max_delay` days before `ref_date`, so that every
#' possible delay up to `max_delay` could have been observed.
#'
#' @param evd Dataframe. Cleaned EVD line list.
#' @param onset_col Character. Onset date column.
#' @param report_col Character. Lab confirmation date column.
#' @param ref_date Date. Reference ("now") date; `NULL` uses the most recent
#'   notification date in `evd` that is not later than the system date.
#' @param max_delay Integer. Maximum delay in days to retain.
#' @param min_onset Date. Earliest onset date to use; `NULL` uses all.
#' @param min_n Integer. Minimum number of usable delays.
#'
#' @return Tibble with columns `onset`, `report` and `delay` (integer days).
#' @export
build_reporting_delays <- function(evd,
                                   onset_col = "alert_date_debut_symptoms",
                                   report_col = "lab_date_analyse",
                                   ref_date = NULL,
                                   max_delay = 21L,
                                   min_onset = NULL,
                                   min_n = 20L) {
  required <- c(
    onset_col,
    report_col,
    "classification_finale",
    "lab_resultat_final"
  )
  missing_cols <- setdiff(required, names(evd))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      c(
        "Missing required columns in `evd`.",
        i = paste0("Missing: ", paste(missing_cols, collapse = ", "))
      )
    )
  }

  ref_date <- if (is.null(ref_date)) {
    evd_notification_ref_date(evd)
  } else {
    as.Date(ref_date)
  }
  max_delay <- as.integer(max_delay)
  min_onset <- if (is.null(min_onset)) NULL else as.Date(min_onset)

  delays <- evd |>
    dplyr::filter(
      .data$classification_finale == "Cas confirmé" |
        .data$lab_resultat_final == "Positif"
    ) |>
    dplyr::transmute(
      onset = as.Date(.data[[onset_col]]),
      report = as.Date(.data[[report_col]]),
      delay = as.integer(as.numeric(.data$report - .data$onset))
    ) |>
    dplyr::filter(
      !is.na(.data$onset),
      !is.na(.data$delay),
      .data$onset <= ref_date - max_delay,
      .data$delay >= 0L,
      .data$delay <= max_delay
    )

  if (!is.null(min_onset)) {
    delays <- delays |> dplyr::filter(.data$onset >= min_onset)
  }

  if (nrow(delays) < min_n) {
    rlang::abort(
      sprintf(
        "Only %d fully observed onset -> lab delays; at least %d are required.",
        nrow(delays),
        min_n
      )
    )
  }

  delays
}

#' Build the observed onset -> death/report delay sample
#'
#' Keeps only confirmed deaths with fully observable delays at the reference
#' date. The report date is the resolved death date (`s6_date_deces` first,
#' then `date_de_deces`) with `date_heure_notification_alerte` as the final
#' fallback.
#'
#' @param evd Dataframe. Cleaned EVD line list.
#' @param onset_col Character. Preferred onset date column.
#' @param fallback_onset_col Character. Fallback onset date column.
#' @param death_date_cols Character vector of death date columns in priority
#'   order.
#' @param report_fallback_col Character. Notification date column used when no
#'   death date is available.
#' @param ref_date Date. Reference ("now") date; `NULL` uses the most recent
#'   notification date in `evd` that is not later than the system date.
#' @param max_delay Integer. Maximum delay in days to retain.
#' @param min_onset Date. Earliest onset date to use; `NULL` uses all.
#' @param min_n Integer. Minimum number of usable delays.
#'
#' @return Tibble with columns `onset`, `report` and `delay` (integer days).
#' @export
build_death_reporting_delays <- function(evd,
                                         onset_col = "alert_date_debut_symptoms",
                                         fallback_onset_col = "s2_date_debut_signes_symptomes",
                                         death_date_cols = c("s6_date_deces", "date_de_deces"),
                                         report_fallback_col = "date_heure_notification_alerte",
                                         ref_date = NULL,
                                         max_delay = 21L,
                                         min_onset = NULL,
                                         min_n = 10L) {
  if (!any(c(onset_col, fallback_onset_col) %in% names(evd))) {
    rlang::abort(
      "build_death_reporting_delays() requires an onset or fallback date column."
    )
  }
  if (!report_fallback_col %in% names(evd)) {
    rlang::abort(
      sprintf(
        "build_death_reporting_delays() requires fallback column '%s'.",
        report_fallback_col
      )
    )
  }

  alert_is_dead(evd)

  onset_values <- list()
  if (onset_col %in% names(evd)) {
    onset_values[[length(onset_values) + 1L]] <- as.Date(evd[[onset_col]])
  }
  if (fallback_onset_col %in% names(evd)) {
    onset_values[[length(onset_values) + 1L]] <-
      as.Date(evd[[fallback_onset_col]])
  }
  onset <- purrr::reduce(onset_values, dplyr::coalesce)
  report <- alert_resolve_death_report_date(
    evd,
    death_date_cols = death_date_cols,
    report_fallback_col = report_fallback_col
  )

  ref_date <- if (is.null(ref_date)) {
    evd_notification_ref_date(evd, notification_col = report_fallback_col)
  } else {
    as.Date(ref_date)
  }
  max_delay <- as.integer(max_delay)
  min_onset <- if (is.null(min_onset)) NULL else as.Date(min_onset)

  delays <- evd |>
    dplyr::mutate(
      onset = onset,
      report = report,
      is_dead = alert_is_dead(evd)
    ) |>
    dplyr::filter(
      .data$is_dead,
      !is.na(.data$onset),
      !is.na(.data$report)
    ) |>
    dplyr::transmute(
      onset = .data$onset,
      report = .data$report,
      delay = as.integer(as.numeric(.data$report - .data$onset))
    ) |>
    dplyr::filter(
      !is.na(.data$delay),
      .data$onset <= ref_date - max_delay,
      .data$delay >= 0L,
      .data$delay <= max_delay
    )

  if (!is.null(min_onset)) {
    delays <- delays |> dplyr::filter(.data$onset >= min_onset)
  }

  if (nrow(delays) < min_n) {
    rlang::abort(
      sprintf(
        "Only %d fully observed onset -> death delays; at least %d are required.",
        nrow(delays),
        min_n
      )
    )
  }

  delays
}

#' Summarise a vector of reporting delays
#'
#' @param delay_values Integer vector of delay days.
#'
#' @return One-row tibble of summary statistics.
#' @export
summarise_reporting_delays <- function(delay_values) {
  delay_values <- as.integer(delay_values)
  delay_values <- delay_values[is.finite(delay_values)]

  if (length(delay_values) == 0L) {
    rlang::abort("`delay_values` is empty.")
  }

  tibble::tibble(
    n = length(delay_values),
    min = min(delay_values),
    p25 = as.numeric(stats::quantile(delay_values, 0.25)),
    median = as.numeric(stats::median(delay_values)),
    mean = mean(delay_values),
    p75 = as.numeric(stats::quantile(delay_values, 0.75)),
    p90 = as.numeric(stats::quantile(delay_values, 0.90)),
    p95 = as.numeric(stats::quantile(delay_values, 0.95)),
    max = max(delay_values)
  )
}

#' Empirical probability and cumulative mass of the reporting delay
#'
#' @param delay_values Integer vector of delay days.
#' @param max_delay Integer. Maximum delay to tabulate.
#'
#' @return Tibble with columns `delay`, `n`, `prob` and `cum_prob`.
#' @export
reporting_delay_cdf <- function(delay_values, max_delay = 21L) {
  delay_values <- as.integer(delay_values)
  max_delay <- as.integer(max_delay)
  total <- length(delay_values)

  tibble::tibble(delay = 0:max_delay) |>
    dplyr::left_join(
      tibble::tibble(delay = delay_values) |>
        dplyr::count(.data$delay, name = "n"),
      by = "delay"
    ) |>
    tidyr::replace_na(list(n = 0L)) |>
    dplyr::mutate(
      prob = .data$n / total,
      cum_prob = cumsum(.data$prob)
    )
}

#' Fit an EpiNow2 nowcast for an onset-keyed daily case series
#'
#' @param reported_cases Tibble with columns `date` and integer `confirm`.
#' @param delay_values Integer vector of onset -> lab delays (days).
#' @param max_delay Integer. Maximum delay used for the truncation
#'   distribution.
#' @param truncation_dist Optional pre-fitted EpiNow2 distribution spec for the
#'   truncation delay; when supplied, `delay_values` is not re-bootstrapped
#'   (useful when many zones share one delay distribution).
#' @param generation_time An EpiNow2 distribution spec for the EVD generation
#'   time. Defaults to the Uganda 2000-01 serial interval (mean 12, sd 5) used
#'   as the generation-time proxy.
#' @param dist_samples Integer. Total posterior draws requested from the
#'   bootstrapped truncation-distribution fit. `bootstrapped_dist_fit()` splits
#'   these across its bootstrap replicates; the default of 20000 avoids the
#'   low effective-sample-size warnings produced by EpiNow2's default of 2000.
#' @param obs, rt, stan Optional EpiNow2 option lists.
#' @param CrIs Numeric vector of credible interval levels.
#' @param seed Integer. Optional RNG seed applied before the delay bootstrap.
#' @param verbose Logical. Passed to `EpiNow2::estimate_infections()`.
#'
#' @return The object returned by `EpiNow2::estimate_infections()`.
#' @export
fit_nowcast_epinow2 <- function(reported_cases,
                                delay_values,
                                max_delay = 21L,
                                truncation_dist = NULL,
                                generation_time = EpiNow2::Gamma(
                                  mean = 12,
                                  sd = 5,
                                  max = 30
                                ),
                                dist_samples = 10000L,
                                obs = NULL,
                                rt = NULL,
                                stan = NULL,
                                CrIs = c(0.2, 0.5, 0.9),
                                seed = NULL,
                                verbose = FALSE) {
  if (!requireNamespace("EpiNow2", quietly = TRUE)) {
    rlang::abort("The {.pkg EpiNow2} package is required for nowcasting.")
  }
  if (!all(c("date", "confirm") %in% names(reported_cases))) {
    rlang::abort("`reported_cases` must have `date` and `confirm` columns.")
  }

  delay_values <- as.integer(delay_values[is.finite(delay_values)])
  if (length(delay_values) < 10L) {
    rlang::abort("At least 10 delay observations are required.")
  }

  if (!is.null(seed)) set.seed(as.integer(seed))

  if (is.null(truncation_dist)) {
    truncation_dist <- EpiNow2::bootstrapped_dist_fit(
      values = delay_values,
      dist = "lognormal",
      samples = dist_samples,
      bootstraps = 10,
      bootstrap_samples = 250,
      max_value = as.integer(max_delay)
    )
  }

  if (is.null(obs)) obs <- EpiNow2::obs_opts(week_effect = FALSE)
  if (is.null(rt)) rt <- EpiNow2::rt_opts()
  if (is.null(stan)) stan <- EpiNow2::stan_opts()

  EpiNow2::estimate_infections(
    data = reported_cases,
    generation_time = EpiNow2::gt_opts(generation_time),
    # No convolution delay: the series is keyed by onset date, so the
    # onset -> lab delay acts purely as right truncation.
    delays = EpiNow2::delay_opts(),
    truncation = EpiNow2::trunc_opts(dist = truncation_dist),
    obs = obs,
    rt = rt,
    forecast = NULL,
    stan = stan,
    CrIs = CrIs,
    verbose = verbose
  )
}

#' Reshape the `infections` posterior into a tidy nowcast table
#'
#' @param fit Output of `fit_nowcast_epinow2()` (or `estimate_infections()`).
#' @param reported_cases Tibble with columns `date` and integer `confirm`.
#' @param tail_days Integer. Number of most recent days flagged as nowcast.
#'
#' @return Tibble sorted by date with `observed` plus nowcast quantiles and a
#'   `tail` flag.
#' @export
tidy_nowcast <- function(fit, reported_cases, tail_days = 5L) {
  summarised <- fit$summarised
  if (!all(c("date", "variable", "type") %in% names(summarised))) {
    rlang::abort(
      "`fit` must contain `summarised` with `date`, `variable` and `type`."
    )
  }

  infections <- summarised |>
    dplyr::filter(.data$variable == "infections")
  if (nrow(infections) == 0L) {
    rlang::abort(
      c(
        "No `infections` variable found in the fit.",
        i = paste0(
          "Available variables: ",
          paste(unique(summarised$variable), collapse = ", ")
        )
      )
    )
  }

  quantile_cols <- names(infections)[
    grepl("^(lower|upper)_[0-9]+$", names(infections))
  ]
  keep <- c("date", "type", "median", "mean", "sd", quantile_cols)

  out <- infections |>
    dplyr::select(dplyr::any_of(keep)) |>
    dplyr::left_join(
      reported_cases |>
        dplyr::transmute(
          date = .data$date,
          observed = as.integer(.data$confirm)
        ),
      by = "date"
    ) |>
    dplyr::arrange(.data$date)

  ref_date <- max(out$date, na.rm = TRUE)
  out <- out |>
    dplyr::mutate(
      tail = .data$date > ref_date - tail_days,
      correction = dplyr::if_else(
        .data$tail & .data$observed > 0,
        .data$median / .data$observed,
        NA_real_
      )
    )

  rename_map <- c(
    median = "nowcast_median",
    mean = "nowcast_mean",
    sd = "nowcast_sd"
  )
  for (col in quantile_cols) {
    rename_map[[col]] <- paste0("nowcast_", col)
  }
  present <- intersect(names(out), names(rename_map))
  names(out)[match(present, names(out))] <- unname(rename_map[present])

  out
}

#' Extract slim posterior samples for the nowcast variables
#'
#' @param fit Output of `fit_nowcast_epinow2()`.
#'
#' @return Tibble with `.draw`, `date`, `variable`, `type` and `value`.
#' @export
extract_nowcast_samples <- function(fit) {
  samples <- fit$samples
  if (!is.data.frame(samples)) {
    rlang::abort("`fit$samples` must be a data frame.")
  }
  if (!all(c("variable", "date") %in% names(samples))) {
    rlang::abort("`fit$samples` must contain `variable` and `date` columns.")
  }

  draw_col <- intersect(c(".draw", "sample"), names(samples))
  if (length(draw_col) == 0L) {
    rlang::abort("No draw identifier column found in `fit$samples`.")
  }
  draw_col <- draw_col[[1L]]
  value_col <- intersect(c("value", "cases"), names(samples))
  if (length(value_col) == 0L) {
    rlang::abort("No value column found in `fit$samples`.")
  }
  value_col <- value_col[[1L]]

  samples |>
    dplyr::filter(.data$variable %in% c("infections", "reported_cases")) |>
    dplyr::select(
      .draw = tidyselect::all_of(draw_col),
      "date",
      "variable",
      dplyr::any_of("type"),
      value = tidyselect::all_of(value_col)
    ) |>
    dplyr::arrange(.data$variable, .data$date, .draw)
}
