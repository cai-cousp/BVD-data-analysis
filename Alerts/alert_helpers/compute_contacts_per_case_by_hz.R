# Contacts per confirmed case per health zone
# Purpose: estimate the average number of followed contacts per confirmed case
# from the cleaned contact follow-up line list and the EVD line list, with a
# pooled (overall) fallback for sparse or zero-contact health zones.

#' Estimate contacts per confirmed case per health zone
#'
#' Uses the cleaned contact follow-up data (wide format, one row per contact)
#' and the cleaned EVD line list to estimate the number of followed contacts
#' per confirmed case within the analysis window. HZ-specific ratios are used
#' when the HZ has at least `min_cases` confirmed cases in the window and a
#' positive ratio; otherwise the pooled (overall) ratio is used.
#'
#' A contact counts as "followed" when `date_debut_suivi` is non-missing and
#' not earlier than 30 days before `enrolement_date` (guards against
#' mis-parsed legacy dates).
#'
#' @param contacts Dataframe. Cleaned contact follow-up data (wide format).
#' @param evd Dataframe. Cleaned EVD line list.
#' @param analysis_start_date,analysis_end_date Date scalars (optional).
#'   When `NULL`, derived from confirmed-case symptom onsets:
#'   start = max(min onset, 2026-04-01), end = min(max onset, Sys.Date()).
#' @param min_cases Integer. Minimum confirmed cases in the window required
#'   for an HZ-specific ratio. Default: 5.
#' @param error_if_none Logical. When `TRUE` (default), abort if no followed
#'   contacts are available in the window; when `FALSE`, return a zero-row
#'   tibble instead.
#' @param windows Dataframe. Optional shared time-window grid. When supplied,
#'   contacts per case are estimated on trailing time-window data (default 21 days
#'   up to each window end), one row per health zone per time window.
#' @param lookback_days Integer. Number of days in the trailing rate window
#'   (default 21L).
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param ref_date Date. Optional explicit nowcast reference date.
#' @param max_delay Integer. Nowcast horizon in days.
#'
#' @return A tibble with one row per HZ having confirmed cases in the window
#'   (and per time window when `windows` is supplied):
#'   `zone_sante_notification`, `n_contacts`, `n_followed_contacts`,
#'   `n_confirmed_cases`, `contacts_per_case_hz`, `contacts_per_case_pooled`,
#'   `contacts_per_case_used`, `source` ("hz" or "pooled").
#' @export
compute_contacts_per_case_by_hz <- function(
    contacts,
    evd,
    analysis_start_date = NULL,
    analysis_end_date = NULL,
    windows = NULL,
    lookback_days = 21L,
    min_cases = 5L,
    error_if_none = TRUE,
    nowcast = NULL,
    ref_date = NULL,
    max_delay = 21L) {

  empty_result <- function() {
    tibble::tibble(
      zone_sante_notification = character(),
      n_contacts = integer(),
      n_followed_contacts = integer(),
      n_confirmed_cases = integer(),
      n_confirmed_cases_nowcast = integer(),
      n_confirmed_cases_nowcast_low = double(),
      n_confirmed_cases_nowcast_high = double(),
      contacts_per_case_hz = double(),
      contacts_per_case_pooled = double(),
      contacts_per_case_nowcast = double(),
      contacts_per_case_used = double(),
      source = character()
    )
  }

  # ---- Input validation ------------------------------------------------------
  if (!is.data.frame(contacts)) {
    rlang::abort("`contacts` must be a data frame.")
  }
  if (!is.data.frame(evd)) {
    rlang::abort("`evd` must be a data frame.")
  }

  contacts_req <- c("zone_sante_notification", "enrolement_date", "date_debut_suivi")
  missing_contacts <- setdiff(contacts_req, names(contacts))
  if (length(missing_contacts) > 0) {
    rlang::abort(c(
      "Contact data is missing required columns.",
      x = paste(missing_contacts, collapse = ", ")
    ))
  }

  cls_col <- intersect(
    c("classification_finale", "classification_finale_cas"),
    names(evd)
  )
  onset_cols <- intersect(
    c("alert_date_debut_symptoms", "s2_date_debut_signes_symptomes"),
    names(evd)
  )

  missing_evd <- setdiff(c("zone_sante_notification", "lab_resultat_final"), names(evd))
  if (length(cls_col) == 0L) {
    missing_evd <- c(missing_evd, "classification_finale|classification_finale_cas")
  }
  if (length(onset_cols) == 0L) {
    missing_evd <- c(missing_evd, "alert_date_debut_symptoms|s2_date_debut_signes_symptomes")
  }
  if (length(missing_evd) > 0) {
    rlang::abort(c(
      "EVD data is missing required columns.",
      x = paste(missing_evd, collapse = ", ")
    ))
  }

  # ---- Windowed execution if windows grid is supplied ------------------------
  if (!is.null(windows)) {
    windows_meta <- alert_window_grid(windows)
    hz_index <- alert_hz_index(evd)

    global_fallback <- tryCatch({
      compute_contacts_per_case_by_hz(
        contacts = contacts,
        evd = evd,
        analysis_start_date = analysis_start_date,
        analysis_end_date = analysis_end_date,
        min_cases = min_cases,
        error_if_none = FALSE,
        nowcast = nowcast,
        ref_date = ref_date,
        max_delay = max_delay
      )
    }, error = function(e) empty_result())

    overall_pooled_val <- if (nrow(global_fallback) > 0L) {
      unique(global_fallback$contacts_per_case_pooled)[1]
    } else {
      NA_real_
    }

    return(
      purrr::map_dfr(
        seq_len(nrow(windows_meta)),
        \(i) {
          w <- windows_meta[i, ]
          window_end <- w$recent_case_window_end
          window_start <- if (is.null(lookback_days) || is.na(lookback_days)) {
            as.Date("1900-01-01")
          } else {
            window_end - as.integer(lookback_days) + 1L
          }

          res <- compute_contacts_per_case_by_hz(
            contacts = contacts,
            evd = evd,
            analysis_start_date = window_start,
            analysis_end_date = window_end,
            min_cases = min_cases,
            error_if_none = FALSE,
            nowcast = nowcast,
            ref_date = ref_date,
            max_delay = max_delay
          )

          if (nrow(res) == 0L) {
            res <- hz_index |>
              dplyr::mutate(
                n_contacts = 0L,
                n_followed_contacts = 0L,
                n_confirmed_cases = 0L,
                n_confirmed_cases_nowcast = NA_integer_,
                n_confirmed_cases_nowcast_low = NA_real_,
                n_confirmed_cases_nowcast_high = NA_real_,
                contacts_per_case_hz = NA_real_,
                contacts_per_case_pooled = overall_pooled_val,
                contacts_per_case_nowcast = NA_real_,
                contacts_per_case_used = overall_pooled_val,
                source = "pooled"
              )
          } else {
            res <- hz_index |>
              dplyr::left_join(res, by = "zone_sante_notification") |>
              dplyr::mutate(
                contacts_per_case_pooled = dplyr::coalesce(contacts_per_case_pooled, overall_pooled_val),
                contacts_per_case_used = dplyr::coalesce(contacts_per_case_used, contacts_per_case_pooled, overall_pooled_val),
                source = dplyr::coalesce(source, "pooled")
              )
          }

          alert_bind_window_metadata(res, w)
        }
      ) |>
        dplyr::arrange(threshold_valid_from, zone_sante_notification)
    )
  }

  # ---- Confirmed cases and analysis window ------------------------------------
  classification <- dplyr::coalesce(as.character(evd[[cls_col[1]]]), "")
  lab_result <- dplyr::coalesce(as.character(evd$lab_resultat_final), "")
  is_confirmed <- classification == "Cas confirmé" | lab_result == "Positif"

  onset <- as.Date(rep(NA_character_, nrow(evd)))
  for (col in onset_cols) {
    onset <- dplyr::coalesce(onset, as.Date(evd[[col]]))
  }

  if (is.null(analysis_start_date)) {
    analysis_start_date <- max(min(onset[is_confirmed], na.rm = TRUE), as.Date("2026-04-01"))
  }
  if (is.null(analysis_end_date)) {
    analysis_end_date <- min(max(onset[is_confirmed], na.rm = TRUE), Sys.Date())
  }

  confirmed_in_window <- evd |>
    dplyr::mutate(.onset = onset) |>
    dplyr::filter(
      is_confirmed,
      !is.na(.onset),
      .onset >= analysis_start_date,
      .onset <= analysis_end_date
    )

  contacts_in_window <- contacts |>
    dplyr::mutate(
      .enrol = as.Date(enrolement_date),
      .db = as.Date(date_debut_suivi)
    ) |>
    dplyr::filter(
      !is.na(.enrol),
      .enrol >= analysis_start_date,
      .enrol <= analysis_end_date
    )

  # Followed = any follow-up visit recorded, not earlier than 30 days before
  # enrollment (drops mis-parsed legacy dates).
  followed_contacts <- contacts_in_window |>
    dplyr::filter(!is.na(.db), .db >= .enrol - 30)

  n_confirmed_total <- nrow(confirmed_in_window)
  n_followed_total <- nrow(followed_contacts)

  if (n_confirmed_total == 0L || n_followed_total == 0L) {
    if (error_if_none) {
      rlang::abort(
        "No followed contacts available to estimate contacts per confirmed case."
      )
    }
    return(empty_result())
  }

  pooled_ratio <- n_followed_total / n_confirmed_total

  hz_confirmed <- confirmed_in_window |>
    dplyr::count(zone_sante_notification, name = "n_confirmed_cases")

  if (nowcast_applies_to_data(
    nowcast,
    evd,
    ref_date = ref_date,
    max_delay = max_delay
  )) {
    hz_confirmed <- hz_confirmed |>
      dplyr::left_join(
        nowcast_total_delta(nowcast) |>
          dplyr::transmute(
            .data$zone_sante_notification,
            delta_median = .data$delta_median,
            delta_low = .data$delta_lower,
            delta_high = .data$delta_upper
          ),
        by = dplyr::join_by(zone_sante_notification),
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        n_confirmed_cases_nowcast = dplyr::if_else(
          is.finite(.data$delta_median),
          as.integer(round(.data$n_confirmed_cases + .data$delta_median)),
          NA_integer_
        ),
        n_confirmed_cases_nowcast_low = dplyr::if_else(
          is.finite(.data$delta_low),
          .data$n_confirmed_cases + .data$delta_low,
          NA_real_
        ),
        n_confirmed_cases_nowcast_high = dplyr::if_else(
          is.finite(.data$delta_high),
          .data$n_confirmed_cases + .data$delta_high,
          NA_real_
        )
      ) |>
      dplyr::select(
        -dplyr::any_of(c("delta_median", "delta_low", "delta_high"))
      )
  } else {
    hz_confirmed <- hz_confirmed |>
      dplyr::mutate(
        n_confirmed_cases_nowcast = NA_integer_,
        n_confirmed_cases_nowcast_low = NA_real_,
        n_confirmed_cases_nowcast_high = NA_real_
      )
  }

  hz_enrolled <- contacts_in_window |>
    dplyr::count(zone_sante_notification, name = "n_contacts")

  hz_followed <- followed_contacts |>
    dplyr::count(zone_sante_notification, name = "n_followed_contacts")

  hz_confirmed |>
    dplyr::left_join(hz_enrolled, by = "zone_sante_notification") |>
    dplyr::left_join(hz_followed, by = "zone_sante_notification") |>
    dplyr::mutate(
      n_contacts = tidyr::replace_na(n_contacts, 0L),
      n_followed_contacts = tidyr::replace_na(n_followed_contacts, 0L),
      contacts_per_case_hz = n_followed_contacts / n_confirmed_cases,
      contacts_per_case_pooled = pooled_ratio,
      contacts_per_case_nowcast = dplyr::if_else(
        is.finite(n_confirmed_cases_nowcast) &
          n_confirmed_cases_nowcast > 0,
        n_followed_contacts / n_confirmed_cases_nowcast,
        NA_real_
      ),
      contacts_per_case_used = dplyr::if_else(
        n_confirmed_cases >= min_cases &
          is.finite(contacts_per_case_hz) &
          contacts_per_case_hz > 0,
        contacts_per_case_hz,
        contacts_per_case_pooled
      ),
      source = dplyr::if_else(
        n_confirmed_cases >= min_cases &
          is.finite(contacts_per_case_hz) &
          contacts_per_case_hz > 0,
        "hz",
        "pooled"
      )
    ) |>
    dplyr::select(
      zone_sante_notification, n_contacts, n_followed_contacts,
      n_confirmed_cases, n_confirmed_cases_nowcast,
      n_confirmed_cases_nowcast_low, n_confirmed_cases_nowcast_high,
      contacts_per_case_hz, contacts_per_case_pooled,
      contacts_per_case_nowcast, contacts_per_case_used, source
    )
}
