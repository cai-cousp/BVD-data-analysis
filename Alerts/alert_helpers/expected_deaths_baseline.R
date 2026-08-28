#' Expected community deaths per HZ based on CMR
#' @param pop Numeric. Population.
#' @param cmr Numeric. Crude mortality rate per 1000 per year.
#' @param period_days Numeric. Number of days in the period (default 7 for week).
expected_deaths_baseline <- function(pop, cmr, period_days = 7) {
  (pop * (cmr / 1000) / 365) * period_days
}
