#' Normalize threshold column names for trend analysis
#'
#' @param thresholds Dataframe of static or longitudinal thresholds.
#' @return Dataframe with unprefixed trend threshold columns.
normalize_threshold_columns <- function(thresholds) {
  required_columns <- c(
    "case_lower",
    "case_upper",
    "death_lower",
    "death_upper"
  )

  for (column in required_columns) {
    alert_column <- paste0("Alert_", column)

    if (!column %in% names(thresholds) && alert_column %in% names(thresholds)) {
      names(thresholds)[names(thresholds) == alert_column] <- column
    }
  }

  missing_columns <- setdiff(required_columns, names(thresholds))

  if (length(missing_columns) > 0L) {
    rlang::abort(c(
      "Thresholds are missing required trend columns.",
      x = paste(missing_columns, collapse = ", ")
    ))
  }

  thresholds
}

#' Join alert counts to static or longitudinal thresholds
#'
#' Evaluates alerts over the exact backward-anchored 7-day non-overlapping 
#' windows derived from the thresholds.
#'
#' @param data Raw EVD alert line list.
#' @param thresholds Static HZ thresholds or rolling HZ-week thresholds.
#' @param date_col Character. The column containing alert notification dates.
#' @return Alert counts with threshold columns, mapped to time windows
#'   (`week_start` = window start date).
join_thresholds_to_alert_counts <- function(data, thresholds, date_col = "date_heure_notification_alerte") {
  alert_required_columns(
    data,
    c("zone_sante_notification", "nature_alerte", date_col),
    "join_thresholds_to_alert_counts()"
  )

  thresholds <- normalize_threshold_columns(thresholds)
  alert_required_columns(
    thresholds,
    "zone_sante_notification",
    "join_thresholds_to_alert_counts()"
  )

  if ("threshold_time_key" %in% names(thresholds)) {
    duplicate_thresholds <- thresholds |>
      dplyr::count(zone_sante_notification, threshold_time_key) |>
      dplyr::filter(n > 1L)

    if (nrow(duplicate_thresholds) > 0L) {
      rlang::abort("Thresholds must be unique by HZ and threshold time key.")
    }

    hz_thresholds <- thresholds |>
      dplyr::filter(zone_sante_notification != "Ensemble de la zone affectée")
    has_ensemble <- any(thresholds$zone_sante_notification == "Ensemble de la zone affectée")

    alert_counts <- data |>
      dplyr::mutate(.alert_date = as.Date(.data[[date_col]])) |>
      dplyr::filter(!is.na(.alert_date), !is.na(zone_sante_notification)) |>
      dplyr::inner_join(
        hz_thresholds |> dplyr::select(
          zone_sante_notification, 
          threshold_valid_from = recent_case_window_start, 
          threshold_valid_to = recent_case_window_end, 
          threshold_time_key
        ),
        by = dplyr::join_by(
          zone_sante_notification,
          .alert_date >= threshold_valid_from,
          .alert_date <= threshold_valid_to
        ),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        type = factor(dplyr::if_else(nature_alerte %in% c("Décédé", "Décédée", "décédé", "décédée"), "death_alerts", "case_alerts"), levels = c("case_alerts", "death_alerts"))
      ) |>
      dplyr::count(zone_sante_notification, threshold_time_key, type, .drop = FALSE, name = "n_alerts") |>
      tidyr::pivot_wider(names_from = type, values_from = n_alerts, values_fill = list(case_alerts = 0L, death_alerts = 0L))

    if (has_ensemble) {
      overall_counts <- alert_counts |>
        dplyr::summarise(
          case_alerts = sum(case_alerts, na.rm = TRUE),
          death_alerts = sum(death_alerts, na.rm = TRUE),
          .by = threshold_time_key
        ) |>
        dplyr::mutate(zone_sante_notification = "Ensemble de la zone affectée")

      alert_counts <- dplyr::bind_rows(alert_counts, overall_counts)
    }

    return(
      thresholds |>
        dplyr::left_join(
          alert_counts,
          by = dplyr::join_by(
            zone_sante_notification,
            threshold_time_key
          ),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          week_start = recent_case_window_start,
          case_alerts = tidyr::replace_na(case_alerts, 0L),
          death_alerts = tidyr::replace_na(death_alerts, 0L),
          total_alerts = case_alerts + death_alerts
        )
    )
  }

  duplicate_thresholds <- thresholds |>
    dplyr::count(zone_sante_notification) |>
    dplyr::filter(n > 1L)

  if (nrow(duplicate_thresholds) > 0L) {
    rlang::abort("Thresholds must be unique by HZ for static joins.")
  }

  # Generate backward-anchored 7-day windows dynamically for static thresholds
  alert_dates <- as.Date(data[[date_col]])
  alert_dates <- alert_dates[!is.na(alert_dates)]
  
  if (length(alert_dates) > 0) {
    t_dates <- resolve_threshold_dates(
      case_dates = alert_dates,
      threshold_dates = NULL,
      window_days = 7L,
      threshold_step_days = 7L,
      analysis_start_date = NULL,
      analysis_end_date = NULL
    )
    windows <- build_recent_case_windows(
      threshold_dates = t_dates,
      window_days = 7L,
      threshold_step_days = 7L
    )

    hz_names <- setdiff(unique(thresholds$zone_sante_notification), "Ensemble de la zone affectée")
    has_ensemble <- any(thresholds$zone_sante_notification == "Ensemble de la zone affectée")

    alert_counts <- data |>
      dplyr::mutate(.alert_date = as.Date(.data[[date_col]])) |>
      dplyr::filter(!is.na(.alert_date), !is.na(zone_sante_notification), zone_sante_notification %in% hz_names) |>
      dplyr::inner_join(
        windows,
        by = dplyr::join_by(
          .alert_date >= threshold_valid_from,
          .alert_date <= threshold_valid_to
        ),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        type = factor(dplyr::if_else(nature_alerte %in% c("Décédé", "Décédée", "décédé", "décédée"), "death_alerts", "case_alerts"), levels = c("case_alerts", "death_alerts"))
      ) |>
      dplyr::count(zone_sante_notification, threshold_valid_from, type, .drop = FALSE, name = "n_alerts") |>
      tidyr::pivot_wider(names_from = type, values_from = n_alerts, values_fill = list(case_alerts = 0L, death_alerts = 0L))

    hz_grid <- tidyr::crossing(
      zone_sante_notification = hz_names,
      threshold_valid_from = windows$threshold_valid_from
    )

    alert_counts <- hz_grid |>
      dplyr::left_join(
        alert_counts,
        by = dplyr::join_by(zone_sante_notification, threshold_valid_from),
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        case_alerts = tidyr::replace_na(case_alerts, 0L),
        death_alerts = tidyr::replace_na(death_alerts, 0L),
        total_alerts = case_alerts + death_alerts,
        week_start = threshold_valid_from
      ) |>
      dplyr::select(-threshold_valid_from)

    if (has_ensemble) {
      overall_counts <- alert_counts |>
        dplyr::summarise(
          case_alerts = sum(case_alerts, na.rm = TRUE),
          death_alerts = sum(death_alerts, na.rm = TRUE),
          total_alerts = sum(total_alerts, na.rm = TRUE),
          .by = week_start
        ) |>
        dplyr::mutate(zone_sante_notification = "Ensemble de la zone affectée")

      alert_counts <- dplyr::bind_rows(alert_counts, overall_counts)
    }
  } else {
    alert_counts <- tibble::tibble(
      zone_sante_notification = character(),
      week_start = as.Date(character()),
      case_alerts = integer(),
      death_alerts = integer(),
      total_alerts = integer()
    )
  }

  alert_counts |>
    dplyr::left_join(
      thresholds,
      by = dplyr::join_by(zone_sante_notification),
      relationship = "many-to-one"
    )
}
