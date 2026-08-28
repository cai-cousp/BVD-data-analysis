#' Create dated output dir
#' @param base_dir Character. Base output directory.
create_output_dir <- function(base_dir) {
  today_dir <- file.path(base_dir, format(Sys.Date(), "%Y_%m_%d"))
  if (!dir.exists(today_dir)) {
    dir.create(today_dir, recursive = TRUE)
  }
  today_dir
}

#' Find the most recent dated output directory containing a target file
#'
#' @param output_dir Character. Base output directory (e.g. here::here("output")).
#' @param pattern Character. Filename pattern to locate.
#' @return Character path to the most recent matching file, or NA if none.
latest_output_file <- function(output_dir, pattern) {
  candidates <- list.files(
    path = output_dir,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(candidates) == 0L) {
    return(NA_character_)
  }
  # Prefer the newest dated subdirectory: sort by full path descending
  candidates <- sort(candidates, decreasing = TRUE)
  candidates[1L]
}

