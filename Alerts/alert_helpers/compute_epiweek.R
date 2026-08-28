#' Wrapper for ISO epiweek with Monday start
#' @param date_col Date vector.
compute_epiweek <- function(date_col) {
  lubridate::floor_date(as.Date(date_col), unit = "week", week_start = 1)
}
