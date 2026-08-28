#' Load the most recent cleaned line list (EVD or contact)
#'
#' Scans `base_path` recursively for files matching `pattern` and loads the
#' file whose embedded numeric timestamp is the largest (the same "latest
#' file" heuristic across all dated output folders).
#'
#' @param base_path Character. Path to the Output folder containing dated
#'   directories (and nested cleaning subfolders).
#' @param pattern Character. Regular expression for the target files.
#'   Default: `"evd.clean_Int_.*\\.rds"` (EVD line list). Use
#'   `"contact.clean_Int_.*\\.rds"` for the cleaned contact follow-up data.
#'
#' @return The data frame stored in the most recent matching RDS file.
#' @export
load_latest_evd <- function(base_path, pattern = "evd.clean_Int_.*\\.rds") {
  files <- list.files(
    path   = base_path,
    pattern = pattern,
    full.names = TRUE,
    recursive  = TRUE
  )
  if (length(files) == 0) {
    rlang::abort(
      sprintf(
        "No cleaned data file matching pattern '%s' found in %s.",
        pattern, base_path
      )
    )
  }

  # Find the most recent file based on the numeric timestamp in the filename
  latest_file <- files[which.max(as.numeric(gsub("\\D", "", basename(files))))]

  data <- readRDS(latest_file)

  # Data dictionary migration: the canonical column name for the final case
  # classification is `classification_finale`. Older clean exports used
  # `classification_finale_cas`; rename those here so all downstream helpers
  # can use the canonical name.
  if (!is.null(data) &&
      "classification_finale_cas" %in% names(data) &&
      !"classification_finale" %in% names(data)) {
    names(data)[names(data) == "classification_finale_cas"] <-
      "classification_finale"
  }

  data
}
