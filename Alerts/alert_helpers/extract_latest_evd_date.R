#' Extract the date from the most recent EVD cleaned line list filename
#'
#' Scans the same files as [load_latest_evd()] but returns the embedded date
#' rather than loading the full RDS.  The filename is expected to follow the
#' convention `evd.clean_Int_YYYY_MM_DD_HHMM.rds`.
#'
#' @param base_path Character. Path to the Output folder containing dated
#'   directories.  Defaults to the standard data-cleaning output location
#'   relative to `evd17_root`.
#' @return A `Date` scalar.
#' @keywords internal
extract_latest_evd_date <- function(
    base_path = file.path(evd17_root, "DataCleaning", "data", "Output")
) {
  files <- list.files(
    path   = base_path,
    pattern = "evd.clean_Int_.*\\.rds",
    full.names = TRUE,
    recursive  = TRUE
  )

  if (length(files) == 0L) {
    rlang::abort("No cleaned EVD line list found.")
  }

  # Same "latest file" heuristic used by load_latest_evd(): strip non-digits,
  # take the maximum numeric value.
  latest_file <- files[which.max(as.numeric(gsub("\\D", "", basename(files))))]

  # The filename embeds the date in YYYY_MM_DD format preceding the _HHMM
  # timestamp.  Collapse all digits and take the first 8 (YYYYMMDD).
  digits <- gsub("\\D", "", basename(latest_file))
  date_str <- substr(digits, 1L, 8L)

  d <- as.Date(date_str, "%Y%m%d")
  if (is.na(d) || !is.finite(d)) {
    rlang::abort(
      sprintf("Could not parse date from EVD filename: %s", basename(latest_file))
    )
  }

  d
}
