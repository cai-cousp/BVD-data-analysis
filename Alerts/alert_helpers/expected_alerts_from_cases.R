#' Derive expected alerts
#' @param true_cases Numeric. Estimated true cases.
#' @param sar Numeric. Secondary attack rate.
#' @param cfr Numeric. Case fatality rate.
expected_alerts_from_cases <- function(true_cases, sar, cfr) {
  list(
    expected_case_alerts = true_cases * (1 + sar),
    expected_death_alerts = true_cases * cfr
  )
}
