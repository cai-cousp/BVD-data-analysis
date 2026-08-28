# Per-health-zone nowcasting of confirmed-case incidence.
#
# Runs a full EpiNow2 nowcast for every eligible health zone (shared onset ->
# lab delay, zone-specific case series). Zones that are too sparse for a
# reliable fit, or whose fit fails, fall back to an empirical truncation
# correction built from the same delay distribution. Results are cached on
# disk keyed by the line-list snapshot so the daily pipeline only refits when
# new data arrives.

#' Locate the most recent cleaned EVD line list file
#'
#' @param base_path Character. Path to the Output folder.
#' @param pattern Character. Regular expression for target files.
#'
#' @return Character path of the latest matching file.
#' @export
latest_evd_file <- function(base_path,
                            pattern = "evd.clean_Int_.*\\.rds") {
  files <- list.files(
    path = base_path,
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(files) == 0L) {
    rlang::abort(
      sprintf(
        "No cleaned data file matching pattern '%s' found in %s.",
        pattern,
        base_path
      )
    )
  }
  files[which.max(as.numeric(gsub("\\D", "", basename(files))))]
}

#' Extract date/timestamp stamp from an EVD file name or snapshot key
#'
#' @param file_name Character scalar (filename, path, or snapshot key).
#' @return Character scalar with the date/timestamp portion (e.g. `"2026_08_23_1515"`),
#'   or `NULL` if no date pattern is found.
#' @export
extract_evd_date_stamp <- function(file_name) {
  if (is.null(file_name) || length(file_name) == 0L || is.na(file_name)) {
    return(NULL)
  }
  fname <- basename(file_name)
  match_int <- stringr::str_match(
    fname,
    "clean_Int_([0-9]{4}_[0-9]{2}_[0-9]{2}(?:_[0-9]+)?)\\.(?:rds|xlsx)$"
  )
  if (!is.na(match_int[1, 2])) {
    return(match_int[1, 2])
  }
  match_analysis <- stringr::str_match(
    fname,
    "(?:01_thresholds_synthesis|01_thresholds_ensemble|01_intermediate_parameters|02_trends_smooth_adeq|02_trends_smooth|02_recent_adequacy|05_nowcast_by_zone|05_nowcast_by_zone_deaths|synthesis_short|EpiSize_BDV)_([0-9]{4}_[0-9]{2}_[0-9]{2}(?:_[0-9]+)?)\\.(?:rds|xlsx|pdf)$"
  )
  if (!is.na(match_analysis[1, 2])) {
    return(match_analysis[1, 2])
  }
  match_nowcast <- stringr::str_match(
    fname,
    "05_nowcast_by_zone(?:_deaths|_both)?_([0-9]{4}_[0-9]{2}_[0-9]{2}(?:_[0-9]+)?)\\.rds"
  )
  if (!is.na(match_nowcast[1, 2])) {
    return(match_nowcast[1, 2])
  }
  gen_match <- stringr::str_extract(
    fname,
    "[0-9]{4}_[0-9]{2}_[0-9]{2}(?:_[0-9]+)?"
  )
  if (!is.na(gen_match)) {
    return(gen_match)
  }
  digits <- gsub("\\D", "", fname)
  if (nchar(digits) >= 8L) {
    if (nchar(digits) >= 12L) {
      return(
        paste0(
          substr(digits, 1, 4), "_",
          substr(digits, 5, 6), "_",
          substr(digits, 7, 8), "_",
          substr(digits, 9, 12)
        )
      )
    }
    return(
      paste0(
        substr(digits, 1, 4), "_",
        substr(digits, 5, 6), "_",
        substr(digits, 7, 8)
      )
    )
  }
  NULL
}

#' Build the standard nowcast cache filename
#' @noRd
build_nowcast_cache_filename <- function(series = c("confirmed_cases", "confirmed_deaths", "both"),
                                        evd_date_stamp = NULL) {
  series <- match.arg(series)
  prefix <- switch(
    series,
    confirmed_cases  = "05_nowcast_by_zone_",
    confirmed_deaths = "05_nowcast_by_zone_deaths_",
    both             = "05_nowcast_by_zone_both_"
  )
  if (is.null(evd_date_stamp) || nchar(evd_date_stamp) == 0L) {
    paste0(sub("_$", "", prefix), ".rds")
  } else {
    paste0(prefix, evd_date_stamp, ".rds")
  }
}

#' Resolve the file path to save the nowcast RDS
#' @noRd
resolve_nowcast_save_path <- function(cache_path, series, evd_date_stamp) {
  if (is.null(cache_path)) return(NULL)
  if (dir.exists(cache_path)) {
    return(file.path(cache_path, build_nowcast_cache_filename(series, evd_date_stamp)))
  }
  fname <- basename(cache_path)
  dir <- dirname(cache_path)
  if (grepl("^05_nowcast_by_zone(?:_deaths|_both)?\\.rds$", fname) &&
      !is.null(evd_date_stamp) && nchar(evd_date_stamp) > 0L) {
    return(file.path(dir, build_nowcast_cache_filename(series, evd_date_stamp)))
  }
  cache_path
}

#' Find and validate an existing nowcast cache file
#' @noRd
find_cached_nowcast <- function(cache_path,
                                snapshot_key,
                                series,
                                max_delay,
                                dist_samples,
                                generation_time,
                                death_date_cols,
                                death_report_fallback_col,
                                evd_date_stamp = NULL) {
  if (is.null(cache_path) && is.null(snapshot_key)) return(NULL)

  candidate_files <- character()

  if (!is.null(cache_path)) {
    if (file.exists(cache_path) && !dir.exists(cache_path)) {
      candidate_files <- c(candidate_files, cache_path)
    }
    dir_to_check <- if (dir.exists(cache_path)) cache_path else dirname(cache_path)
    if (!is.null(evd_date_stamp) && nchar(evd_date_stamp) > 0L) {
      target_in_dir <- file.path(
        dir_to_check,
        build_nowcast_cache_filename(series, evd_date_stamp)
      )
      if (file.exists(target_in_dir)) {
        candidate_files <- c(candidate_files, target_in_dir)
      }
    }
    search_dirs <- unique(c(dir_to_check, dirname(dir_to_check)))
    if (file.exists(here::here("output"))) {
      search_dirs <- unique(c(search_dirs, here::here("output")))
    }
    for (s_dir in search_dirs) {
      if (dir.exists(s_dir)) {
        fs <- list.files(
          s_dir,
          pattern = "^05_nowcast_by_.*\\.rds$",
          full.names = TRUE,
          recursive = TRUE
        )
        if (series == "confirmed_deaths") {
          fs <- fs[grepl("^05_nowcast_by_zone_deaths_", basename(fs))]
        } else if (series == "both") {
          fs <- fs[grepl("^05_nowcast_by_zone_both_", basename(fs))]
        } else {
          fs <- fs[
            grepl("^05_nowcast_by_zone_", basename(fs)) &
            !grepl("^05_nowcast_by_zone_deaths_", basename(fs)) &
            !grepl("^05_nowcast_by_zone_both_", basename(fs))
          ]
        }
        if (!is.null(evd_date_stamp) && nchar(evd_date_stamp) > 0L) {
          fs_stamped <- fs[grepl(evd_date_stamp, basename(fs))]
          if (length(fs_stamped) > 0L) fs <- fs_stamped
        }
        candidate_files <- c(candidate_files, fs)
      }
    }
  }

  candidate_files <- unique(candidate_files)
  if (length(candidate_files) == 0L) return(NULL)

  for (f in candidate_files) {
    if (!file.exists(f)) next
    cached <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(cached) || !is.list(cached)) next

    if (!identical(cached$series, series)) next

    if (!is.null(snapshot_key) && !is.null(cached$snapshot_key)) {
      key_exact <- identical(cached$snapshot_key, snapshot_key)
      stamp_a <- extract_evd_date_stamp(cached$snapshot_key)
      stamp_b <- extract_evd_date_stamp(snapshot_key)
      stamp_match <- !is.null(stamp_a) && !is.null(stamp_b) && identical(stamp_a, stamp_b)
      if (!key_exact && !stamp_match) next
    } else if (!is.null(snapshot_key)) {
      next
    }

    if (!is.null(cached$max_delay) && !identical(cached$max_delay, max_delay)) next
    if (!is.null(cached$dist_samples) && !identical(cached$dist_samples, dist_samples)) next
    if (!is.null(cached$generation_time) && !identical(cached$generation_time, generation_time)) next
    if (series == "confirmed_deaths") {
      if (!is.null(cached$death_date_cols) && !identical(cached$death_date_cols, death_date_cols)) next
      if (!is.null(cached$death_report_fallback_col) &&
          !identical(cached$death_report_fallback_col, death_report_fallback_col)) next
    }

    message("Loaded cached nowcasts from: ", f)
    return(list(path = f, cached = cached))
  }

  NULL
}

#' Bootstrap the empirical cumulative distribution of reporting delays
#'
#' @param delay_values Integer vector of delay days.
#' @param max_delay Integer. Maximum delay to tabulate.
#' @param n_boot Integer. Number of bootstrap resamples.
#' @param seed Integer. Optional RNG seed.
#'
#' @return Tibble with columns `delay`, `f_median`, `f_low`, `f_high`.
#' @export
empirical_delay_cdf <- function(delay_values,
                                max_delay = 21L,
                                n_boot = 500L,
                                seed = NULL) {
  delay_values <- as.integer(delay_values[is.finite(delay_values)])
  if (length(delay_values) == 0L) {
    rlang::abort("`delay_values` is empty.")
  }
  if (!is.null(seed)) set.seed(as.integer(seed))

  max_delay <- as.integer(max_delay)
  k_grid <- 0:max_delay
  bootstrap_matrix <- replicate(
    n_boot,
    {
      sample_delays <- sample(delay_values, replace = TRUE)
      vapply(k_grid, function(k) mean(sample_delays <= k), numeric(1L))
    }
  )

  tibble::tibble(
    delay = k_grid,
    f_median = apply(bootstrap_matrix, 1L, stats::median),
    f_low = apply(bootstrap_matrix, 1L, stats::quantile, probs = 0.025),
    f_high = apply(bootstrap_matrix, 1L, stats::quantile, probs = 0.975)
  )
}

#' Empirical tail correction from a delay CDF
#' @noRd
nowcast_tail_empirical <- function(counts,
                                   cdf,
                                   ref_date,
                                   max_delay,
                                   zone,
                                   status) {
  tail_counts <- counts |>
    dplyr::filter(.data$date >= ref_date - max_delay + 1L) |>
    dplyr::mutate(
      k = as.integer(ref_date - .data$date),
      observed = .data$I
    ) |>
    dplyr::left_join(
      cdf |>
        dplyr::transmute(
          k = .data$delay,
          f_median = .data$f_median,
          f_low = .data$f_low,
          f_high = .data$f_high
        ),
      by = "k",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      nowcast_median = dplyr::if_else(
        .data$f_median > 0,
        .data$observed / .data$f_median,
        .data$observed
      ),
      nowcast_lower_90 = dplyr::if_else(
        .data$f_high > 0,
        .data$observed / .data$f_high,
        .data$observed
      ),
      nowcast_upper_90 = dplyr::if_else(
        .data$f_low > 0,
        .data$observed / .data$f_low,
        .data$observed
      ),
      zone_sante_notification = zone,
      method = "empirical_delay_correction",
      status = status
    ) |>
    dplyr::select(
      "zone_sante_notification",
      "date",
      "observed",
      "nowcast_median",
      "nowcast_lower_90",
      "nowcast_upper_90",
      "method",
      "status"
    )
}

#' Compute EpiNow2 nowcasts for every health zone
#'
#' Nowcasts the confirmed-case series (default), the confirmed-death series,
#' or both. `series = "confirmed_deaths"` counts confirmed cases identified as
#' dead by `alert_is_dead()` and uses the onset -> death/report delay.
#'
#' @param ref_date Date. Reference ("now") date; `NULL` uses the most recent
#'   notification date in `evd` that is not later than the system date.
#' @param series Character. One of `"confirmed_cases"`, `"confirmed_deaths"`,
#'   or `"both"`.
#' @param death_date_cols Character vector of death date columns in priority
#'   order (`s6_date_deces` then `date_de_deces` by default).
#' @param death_report_fallback_col Character. Notification date column used
#'   as the last report-date fallback for the death delay.
#'
#' @return When `series` is a single stratum, a tibble with one row per zone
#'   per tail date plus a `series` attribute. When `series = "both"`, a named
#'   list with `confirmed_cases` and `confirmed_deaths`.
#' @export
compute_nowcasts_by_zone <- function(evd,
                                     ref_date = NULL,
                                     onset_col = "alert_date_debut_symptoms",
                                     report_col = "lab_date_analyse",
                                     series = c("confirmed_cases", "confirmed_deaths", "both"),
                                     death_date_cols = c("s6_date_deces", "date_de_deces"),
                                     death_report_fallback_col = "date_heure_notification_alerte",
                                     max_delay = 21L,
                                     min_date = NULL,
                                     recent_window_days = 21L,
                                     min_recent_cases = 10L,
                                     min_series_days = 14L,
                                     delay_min_onset = NULL,
                                     generation_time = EpiNow2::Gamma(
                                       mean = 12,
                                       sd = 5,
                                       max = 30
                                     ),
                                     dist_samples = 10000L,
                                     stan = NULL,
                                     seed = 42L,
                                     fit_fun = fit_nowcast_epinow2,
                                     cache_path = NULL,
                                     snapshot_key = NULL,
                                     verbose = FALSE) {
  series <- rlang::arg_match(series)

  alert_required_columns(
    evd,
    c(
      "zone_sante_notification",
      "classification_finale",
      "lab_resultat_final"
    ),
    "compute_nowcasts_by_zone()"
  )
  if (!any(c(onset_col, "s2_date_debut_signes_symptomes") %in% names(evd))) {
    rlang::abort("compute_nowcasts_by_zone() requires an onset date column.")
  }
  if (series != "confirmed_cases") {
    alert_required_columns(
      evd,
      c(
        "s6_statut_final_patient",
        "s5_statut_patient_lors_prelev",
        "nature_alerte",
        "date_de_deces",
        "s6_date_deces",
        death_report_fallback_col
      ),
      "compute_nowcasts_by_zone()"
    )
  }

  ref_date <- if (is.null(ref_date)) {
    evd_notification_ref_date(evd)
  } else {
    as.Date(ref_date)
  }
  max_delay <- as.integer(max_delay)

  evd_date_stamp <- extract_evd_date_stamp(
    if (!is.null(snapshot_key)) {
      snapshot_key
    } else if (!is.null(cache_path)) {
      basename(cache_path)
    } else {
      NULL
    }
  )

  if (series == "both") {
    cached_hit <- find_cached_nowcast(
      cache_path = cache_path,
      snapshot_key = snapshot_key,
      series = "both",
      max_delay = max_delay,
      dist_samples = dist_samples,
      generation_time = generation_time,
      death_date_cols = death_date_cols,
      death_report_fallback_col = death_report_fallback_col,
      evd_date_stamp = evd_date_stamp
    )
    if (!is.null(cached_hit)) {
      cached <- cached_hit$cached
      return(structure(
        cached$nowcasts,
        ref_date = cached$ref_date,
        delay_summary = cached$delay_summary,
        delay_cdf = cached$delay_cdf,
        max_delay = cached$max_delay,
        generation_time = cached$generation_time,
        dist_samples = cached$dist_samples,
        snapshot_key = cached$snapshot_key,
        series = "both",
        generated_at = cached$generated_at
      ))
    }

    cases <- compute_nowcast_stratum(
      evd,
      ref_date = ref_date,
      onset_col = onset_col,
      report_col = report_col,
      series = "confirmed_cases",
      death_date_cols = death_date_cols,
      death_report_fallback_col = death_report_fallback_col,
      max_delay = max_delay,
      min_date = min_date,
      recent_window_days = recent_window_days,
      min_recent_cases = min_recent_cases,
      min_series_days = min_series_days,
      delay_min_onset = delay_min_onset,
      generation_time = generation_time,
      dist_samples = dist_samples,
      stan = stan,
      seed = seed,
      fit_fun = fit_fun,
      cache_path = NULL,
      snapshot_key = NULL,
      verbose = verbose
    )
    deaths <- compute_nowcast_stratum(
      evd,
      ref_date = ref_date,
      onset_col = onset_col,
      report_col = report_col,
      series = "confirmed_deaths",
      death_date_cols = death_date_cols,
      death_report_fallback_col = death_report_fallback_col,
      max_delay = max_delay,
      min_date = min_date,
      recent_window_days = recent_window_days,
      min_recent_cases = min_recent_cases,
      min_series_days = min_series_days,
      delay_min_onset = delay_min_onset,
      generation_time = generation_time,
      dist_samples = dist_samples,
      stan = stan,
      seed = seed,
      fit_fun = fit_fun,
      cache_path = NULL,
      snapshot_key = NULL,
      verbose = verbose
    )

    out <- list(confirmed_cases = cases, confirmed_deaths = deaths)

    save_path <- resolve_nowcast_save_path(cache_path, "both", evd_date_stamp)
    if (!is.null(save_path) && !is.null(snapshot_key)) {
      saveRDS(
        list(
          nowcasts = out,
          series = "both",
          ref_date = ref_date,
          delay_summary = attr(cases, "delay_summary"),
          delay_cdf = attr(cases, "delay_cdf"),
          max_delay = max_delay,
          dist_samples = dist_samples,
          generation_time = generation_time,
          death_date_cols = death_date_cols,
          death_report_fallback_col = death_report_fallback_col,
          snapshot_key = snapshot_key,
          generated_at = Sys.time()
        ),
        save_path
      )
    }

    return(out)
  }

  compute_nowcast_stratum(
    evd,
    ref_date = ref_date,
    onset_col = onset_col,
    report_col = report_col,
    series = series,
    death_date_cols = death_date_cols,
    death_report_fallback_col = death_report_fallback_col,
    max_delay = max_delay,
    min_date = min_date,
    recent_window_days = recent_window_days,
    min_recent_cases = min_recent_cases,
    min_series_days = min_series_days,
    delay_min_onset = delay_min_onset,
    generation_time = generation_time,
    dist_samples = dist_samples,
    stan = stan,
    seed = seed,
    fit_fun = fit_fun,
    cache_path = cache_path,
    snapshot_key = snapshot_key,
    verbose = verbose
  )
}

#' Compute EpiNow2 nowcasts for a single stratum
#' @noRd
compute_nowcast_stratum <- function(evd,
                                    ref_date = NULL,
                                    onset_col = "alert_date_debut_symptoms",
                                    report_col = "lab_date_analyse",
                                    series = c("confirmed_cases", "confirmed_deaths", "both"),
                                    death_date_cols = c("s6_date_deces", "date_de_deces"),
                                    death_report_fallback_col = "date_heure_notification_alerte",
                                    max_delay = 21L,
                                    min_date = NULL,
                                    recent_window_days = 21L,
                                    min_recent_cases = 10L,
                                    min_series_days = 14L,
                                    delay_min_onset = NULL,
                                    generation_time = EpiNow2::Gamma(
                                      mean = 12,
                                      sd = 5,
                                      max = 30
                                    ),
                                    dist_samples = 10000L,
                                    stan = NULL,
                                    seed = 42L,
                                    fit_fun = fit_nowcast_epinow2,
                                    cache_path = NULL,
                                    snapshot_key = NULL,
                                    verbose = FALSE) {
  series <- rlang::arg_match(series)
  alert_required_columns(
    evd,
    c(
      "zone_sante_notification",
      "classification_finale",
      "lab_resultat_final"
    ),
    "compute_nowcasts_by_zone()"
  )
  if (!any(c(onset_col, "s2_date_debut_signes_symptomes") %in% names(evd))) {
    rlang::abort("compute_nowcasts_by_zone() requires an onset date column.")
  }

  ref_date <- if (is.null(ref_date)) {
    evd_notification_ref_date(evd)
  } else {
    as.Date(ref_date)
  }
  max_delay <- as.integer(max_delay)

  evd_date_stamp <- extract_evd_date_stamp(
    if (!is.null(snapshot_key)) {
      snapshot_key
    } else if (!is.null(cache_path)) {
      basename(cache_path)
    } else {
      NULL
    }
  )

  cached_hit <- find_cached_nowcast(
    cache_path = cache_path,
    snapshot_key = snapshot_key,
    series = series,
    max_delay = max_delay,
    dist_samples = dist_samples,
    generation_time = generation_time,
    death_date_cols = death_date_cols,
    death_report_fallback_col = death_report_fallback_col,
    evd_date_stamp = evd_date_stamp
  )
  if (!is.null(cached_hit)) {
    cached <- cached_hit$cached
    return(structure(
      cached$nowcasts,
      ref_date = cached$ref_date,
      delay_summary = cached$delay_summary,
      delay_cdf = cached$delay_cdf,
      max_delay = cached$max_delay,
      generation_time = cached$generation_time,
      dist_samples = cached$dist_samples,
      snapshot_key = cached$snapshot_key,
      series = series,
      generated_at = cached$generated_at
    ))
  }

  fallback_date_col <- if ("s2_date_debut_signes_symptomes" %in% names(evd)) {
    "s2_date_debut_signes_symptomes"
  } else {
    onset_col
  }

  pooled_delays <- if (series == "confirmed_deaths") {
    build_death_reporting_delays(
      evd,
      onset_col = onset_col,
      fallback_onset_col = fallback_date_col,
      death_date_cols = death_date_cols,
      report_fallback_col = death_report_fallback_col,
      ref_date = ref_date,
      max_delay = max_delay,
      min_onset = delay_min_onset,
      min_n = 10L
    )
  } else {
    build_reporting_delays(
      evd,
      onset_col = onset_col,
      report_col = report_col,
      ref_date = ref_date,
      max_delay = max_delay,
      min_onset = delay_min_onset,
      min_n = 10L
    )
  }
  delay_summary <- summarise_reporting_delays(pooled_delays$delay)
  delay_cdf <- empirical_delay_cdf(
    pooled_delays$delay,
    max_delay = max_delay,
    seed = seed
  )

  zones <- evd |>
    dplyr::filter(!is.na(.data$zone_sante_notification)) |>
    dplyr::distinct(.data$zone_sante_notification) |>
    dplyr::arrange(.data$zone_sante_notification) |>
    dplyr::pull(.data$zone_sante_notification)

  if (is.null(stan)) {
    stan <- EpiNow2::stan_opts(
      samples = 2000L,
      warmup = 1000L,
      chains = 4L,
      cores = 4L,
      control = list(
        adapt_delta = 0.95,
        max_treedepth = 12
      )
    )
  }

  # Fit the shared truncation distribution lazily, once, and reuse it for
  # every zone (each zone would otherwise re-run the same delay bootstrap).
  trunc_cache <- new.env(parent = emptyenv())

  # EpiNow2 re-emits per-chain Stan warnings through futile.logger. Those
  # warnings contain mc-stan.org URLs, which Positron may try to open as an
  # external application. Capture WARN lines here instead of printing them,
  # while still letting ERROR/INFO lines reach the console.
  stan_warnings <- new.env(parent = emptyenv())
  stan_warnings$lines <- character()
  epinow_logger <- "EpiNow2.epinow.estimate_infections.fit"
  old_appender <- futile.logger::flog.appender(name = epinow_logger)
  capture_stan_warning_appender <- function(line) {
    if (grepl("^WARN", line)) {
      stan_warnings$lines <- c(stan_warnings$lines, line)
    } else {
      cat(line, sep = "")
    }
  }
  futile.logger::flog.appender(
    capture_stan_warning_appender,
    name = epinow_logger
  )
  on.exit(
    futile.logger::flog.appender(old_appender, name = epinow_logger),
    add = TRUE
  )

  nowcasts <- purrr::map_dfr(
    seq_along(zones),
    function(i) {
      zone <- zones[[i]]
      counts <- tryCatch(
        if (series == "confirmed_deaths") {
          build_confirmed_death_daily(
            evd,
            zone = zone,
            min_date = min_date,
            ref_date = ref_date,
            case_date_col = onset_col,
            fallback_date_col = fallback_date_col
          )
        } else {
          build_confirmed_daily(
            evd,
            zone = zone,
            min_date = min_date,
            ref_date = ref_date,
            case_date_col = onset_col,
            fallback_date_col = fallback_date_col
          )
        },
        error = function(e) NULL
      )
      if (is.null(counts)) {
        return(empty_zone_nowcast(zone, ref_date, max_delay, "no_series"))
      }

      recent_n <- sum(
        counts$I[counts$date >= ref_date - recent_window_days + 1L],
        na.rm = TRUE
      )
      eligible <- recent_n >= min_recent_cases &&
        nrow(counts) >= min_series_days

      if (eligible) {
        reported_cases <- counts |>
          dplyr::transmute(
            date = .data$date,
            confirm = as.integer(.data$I)
          )
        truncation_dist <- NULL
        if (identical(fit_fun, fit_nowcast_epinow2)) {
          if (is.null(trunc_cache$dist)) {
            if (!is.null(seed)) set.seed(as.integer(seed))
            trunc_cache$dist <- EpiNow2::bootstrapped_dist_fit(
              values = pooled_delays$delay,
              dist = "lognormal",
              samples = dist_samples,
              bootstraps = 10,
              bootstrap_samples = 250,
              max_value = max_delay
            )
          }
          truncation_dist <- trunc_cache$dist
        }
        fit <- tryCatch(
          fit_fun(
            reported_cases = reported_cases,
            delay_values = pooled_delays$delay,
            max_delay = max_delay,
            truncation_dist = truncation_dist,
            generation_time = generation_time,
            stan = stan,
            seed = seed + i,
            verbose = verbose
          ),
          error = function(e) NULL
        )
        if (!is.null(fit)) {
          tidy <- tryCatch(
            tidy_nowcast(fit, reported_cases),
            error = function(e) NULL
          )
          if (!is.null(tidy)) {
            return(
              tidy |>
                dplyr::filter(.data$date >= ref_date - max_delay + 1L) |>
                dplyr::mutate(
                  zone_sante_notification = zone,
                  method = "epinow2",
                  status = "fit_ok"
                ) |>
                dplyr::select(
                  "zone_sante_notification",
                  "date",
                  "observed",
                  "nowcast_median",
                  "nowcast_lower_90",
                  "nowcast_upper_90",
                  "method",
                  "status"
                )
            )
          }
        }
        return(
          nowcast_tail_empirical(
            counts,
            delay_cdf,
            ref_date,
            max_delay,
            zone,
            "fit_error"
          )
        )
      }

      nowcast_tail_empirical(
        counts,
        delay_cdf,
        ref_date,
        max_delay,
        zone,
        if (recent_n == 0L) "no_recent_cases" else "below_min_cases"
      )
    }
  )

  attr(nowcasts, "stan_warning_count") <- length(stan_warnings$lines)
  attr(nowcasts, "stan_warnings") <- stan_warnings$lines
  attr(nowcasts, "ref_date") <- ref_date
  attr(nowcasts, "delay_summary") <- delay_summary
  attr(nowcasts, "delay_cdf") <- delay_cdf
  attr(nowcasts, "max_delay") <- max_delay
  attr(nowcasts, "generation_time") <- generation_time
  attr(nowcasts, "snapshot_key") <- snapshot_key
  attr(nowcasts, "series") <- series
  attr(nowcasts, "generated_at") <- Sys.time()

  if (length(stan_warnings$lines) > 0L) {
    message(
      sprintf(
        "Suppressed %d EpiNow2 per-chain Stan warning(s); inspect attr(x, \"stan_warnings\").",
        length(stan_warnings$lines)
      )
    )
  }

  save_path <- resolve_nowcast_save_path(cache_path, series, evd_date_stamp)
  if (!is.null(save_path) && !is.null(snapshot_key)) {
    saveRDS(
      list(
        nowcasts = nowcasts,
        series = series,
        ref_date = ref_date,
        delay_summary = delay_summary,
        delay_cdf = delay_cdf,
        max_delay = max_delay,
        dist_samples = dist_samples,
        generation_time = generation_time,
        death_date_cols = death_date_cols,
        death_report_fallback_col = death_report_fallback_col,
        snapshot_key = snapshot_key,
        generated_at = attr(nowcasts, "generated_at")
      ),
      save_path
    )
  }

  nowcasts
}

#' Empty nowcast row set for zones without a usable series
#' @noRd
empty_zone_nowcast <- function(zone, ref_date, max_delay, status) {
  tibble::tibble(
    zone_sante_notification = zone,
    date = seq.Date(ref_date - max_delay + 1L, ref_date, by = "day"),
    observed = NA_integer_,
    nowcast_median = NA_real_,
    nowcast_lower_90 = NA_real_,
    nowcast_upper_90 = NA_real_,
    method = "none",
    status = status
  )
}

#' Aggregate the nowcast tail delta for one or all zones
#'
#' @param nowcasts Tibble from `compute_nowcasts_by_zone()`.
#' @param zone Character. Optional zone to restrict to.
#' @param through_date Date. Optional upper bound for the summed tail; only
#'   nowcast dates `<= through_date` are included.
#'
#' @return One-row-per-zone tibble with tail totals and `delta_*` columns
#'   (nowcast minus observed).
#' @export
nowcast_total_delta <- function(nowcasts, zone = NULL, through_date = NULL) {
  alert_required_columns(
    nowcasts,
    c("zone_sante_notification", "observed", "nowcast_median"),
    "nowcast_total_delta()"
  )
  if (!is.null(zone)) {
    nowcasts <- nowcasts |>
      dplyr::filter(.data$zone_sante_notification == zone)
  }
  if (!is.null(through_date)) {
    through_date <- as.Date(through_date)
    nowcasts <- nowcasts |>
      dplyr::filter(.data$date <= through_date)
  }

  nowcasts |>
    dplyr::summarise(
      tail_observed = sum(.data$observed, na.rm = TRUE),
      tail_nowcast_median = sum(.data$nowcast_median, na.rm = TRUE),
      tail_nowcast_lower = sum(.data$nowcast_lower_90, na.rm = TRUE),
      tail_nowcast_upper = sum(.data$nowcast_upper_90, na.rm = TRUE),
      .by = "zone_sante_notification"
    ) |>
    dplyr::mutate(
      delta_median = pmax(0, .data$tail_nowcast_median - .data$tail_observed),
      delta_lower = pmax(0, .data$tail_nowcast_lower - .data$tail_observed),
      delta_upper = pmax(0, .data$tail_nowcast_upper - .data$tail_observed)
    )
}
