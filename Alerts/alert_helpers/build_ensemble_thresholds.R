#' Build whole-area thresholds without the Beni benchmark
#'
#' Aggregates zone-level thresholds by analysis window. The ensemble uses
#' Approach A and Approach C only: Approach B remains zone-specific and is
#' represented as missing for the whole affected area.
#'
#' @param hz_thresholds Dataframe of zone-level threshold synthesis rows.
#' @param confirmed_notification_dates Dataframe with one row per health zone
#'   containing `zone_sante_notification` and
#'   `first_confirmed_notification_date`.
#' @param case_derived_params Optional dataframe with one row per health zone
#'   per threshold window containing beta multipliers and the combined
#'   detection rate. These parameters are averaged across active health zones.
#' @return Tibble with one whole-area row per threshold time key.
build_ensemble_thresholds <- function(
    hz_thresholds,
    confirmed_notification_dates,
    case_derived_params = NULL) {
  required_cols <- c(
    "threshold_time_key",
    "zone_sante_notification",
    "threshold_valid_from",
    "threshold_valid_to",
    "recent_case_window_start",
    "recent_case_window_end",
    "recent_case_window_days",
    "recent_case_anchor_date",
    "week_start",
    "death_threshold_lower_A",
    "death_threshold_upper_A",
    "alert_case_threshold_lower_C",
    "alert_case_threshold_upper_C",
    "alert_death_threshold_lower_C",
    "alert_death_threshold_upper_C"
  )
  alert_required_columns(
    hz_thresholds,
    required_cols,
    "build_ensemble_thresholds()"
  )
  alert_required_columns(
    confirmed_notification_dates,
    c(
      "zone_sante_notification",
      "first_confirmed_notification_date"
    ),
    "build_ensemble_thresholds()"
  )

  parameter_cols <- c(
    "beta_c",
    "beta_c_low",
    "beta_c_high",
    "beta_d",
    "beta_d_low",
    "beta_d_high",
    "detection_rate_adj"
  )
  if (!is.null(case_derived_params)) {
    alert_required_columns(
      case_derived_params,
      c(
        "threshold_time_key",
        "zone_sante_notification",
        parameter_cols
      ),
      "build_ensemble_thresholds()"
    )

    duplicate_params <- case_derived_params |>
      dplyr::filter(
        zone_sante_notification != "Ensemble de la zone affectée",
        !is.na(threshold_time_key),
        !is.na(zone_sante_notification)
      ) |>
      dplyr::count(
        zone_sante_notification,
        threshold_time_key,
        name = "n"
      ) |>
      dplyr::filter(n > 1L)
    if (nrow(duplicate_params) > 0L) {
      duplicate_param_keys <- paste0(
        duplicate_params$zone_sante_notification,
        "@",
        duplicate_params$threshold_time_key
      )
      rlang::abort(c(
        "Case-derived parameters must contain one row per health zone per window.",
        x = paste("Duplicate zone-window rows:", paste(
          head(duplicate_param_keys, 5L),
          collapse = ", "
        ))
      ))
    }

    hz_thresholds <- hz_thresholds |>
      dplyr::left_join(
        case_derived_params |>
          dplyr::select(
            zone_sante_notification,
            threshold_time_key,
            dplyr::all_of(parameter_cols)
          ),
        by = dplyr::join_by(
          zone_sante_notification,
          threshold_time_key
        ),
        relationship = "many-to-one"
      )
  }

  confirmed_notification_dates <- confirmed_notification_dates |>
    dplyr::mutate(
      first_confirmed_notification_date = as.Date(first_confirmed_notification_date)
    )

  if (anyNA(confirmed_notification_dates$first_confirmed_notification_date)) {
    rlang::abort(c(
      "build_ensemble_thresholds() requires valid, non-missing confirmation dates.",
      x = "`first_confirmed_notification_date` cannot be missing or unparseable."
    ))
  }

  duplicate_zones <- confirmed_notification_dates |>
    dplyr::count(zone_sante_notification, name = "n") |>
    dplyr::filter(n > 1L) |>
    dplyr::pull(zone_sante_notification)
  if (length(duplicate_zones) > 0L) {
    rlang::abort(c(
      "Confirmation dates must contain one row per health zone.",
      x = paste("Duplicate zones:", paste(duplicate_zones, collapse = ", "))
    ))
  }

  hz_zones <- unique(
    hz_thresholds$zone_sante_notification[
      !is.na(hz_thresholds$zone_sante_notification) &
        hz_thresholds$zone_sante_notification != "Ensemble de la zone affectée"
    ]
  )
  zones_without_dates <- setdiff(
    hz_zones,
    confirmed_notification_dates$zone_sante_notification
  )
  if (length(zones_without_dates) > 0L) {
    rlang::abort(c(
      "Confirmation dates are missing for the following health zones:",
      x = paste(zones_without_dates, collapse = ", ")
    ))
  }

  summed_cols <- c(
    "Population",
    "expected_weekly_deaths_cmr",
    "death_threshold_lower_A",
    "death_threshold_upper_A",
    "alert_case_threshold_C",
    "alert_case_threshold_lower_C",
    "alert_case_threshold_upper_C",
    "alert_death_threshold_C",
    "alert_death_threshold_lower_C",
    "alert_death_threshold_upper_C"
  )
  first_cols <- c(
    "threshold_valid_from",
    "threshold_valid_to",
    "recent_case_window_start",
    "recent_case_window_end",
    "recent_case_window_days",
    "recent_case_anchor_date",
    "week_start"
  )

  hz_thresholds |>
    dplyr::filter(
      zone_sante_notification != "Ensemble de la zone affectée"
    ) |>
    dplyr::left_join(
      confirmed_notification_dates,
      by = dplyr::join_by(zone_sante_notification),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(
      threshold_valid_from >= first_confirmed_notification_date
    ) |>
    dplyr::summarise(
      Province = "Ensemble",
      zone_sante_notification = "Ensemble de la zone affectée",
      dplyr::across(dplyr::any_of(summed_cols), ~sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::any_of(parameter_cols), ~mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(first_cols), dplyr::first),
      .by = threshold_time_key
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(parameter_cols),
        ~dplyr::if_else(is.finite(.x), .x, NA_real_)
      )
    ) |>
    dplyr::mutate(
      alert_case_threshold_lower_B = NA_real_,
      alert_case_threshold_upper_B = NA_real_,
      alert_death_threshold_lower_B = NA_real_,
      alert_death_threshold_upper_B = NA_real_,
      Alert_case_lower = tidyr::replace_na(alert_case_threshold_lower_C, 0),
      Alert_case_upper = tidyr::replace_na(alert_case_threshold_upper_C, 0),
      Alert_death_lower = (
        death_threshold_lower_A +
          tidyr::replace_na(alert_death_threshold_lower_C, 0)
      ) / 2,
      Alert_death_upper = (
        death_threshold_upper_A +
          tidyr::replace_na(alert_death_threshold_upper_C, 0)
      ) / 2,
      Alert_case_threshold = (Alert_case_lower + Alert_case_upper) / 2,
      Alert_death_threshold = (Alert_death_lower + Alert_death_upper) / 2
    )
}
