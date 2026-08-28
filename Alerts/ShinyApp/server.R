# =============================================================================
# BVD Alerts App — Server Definition
# =============================================================================
# Initializes filter module and the three tab modules.
# =============================================================================

# --- Source dependencies (guarded against double-loading) --------------------
if (!exists(".bvd_global_loaded") || !.bvd_global_loaded) {
  source("R/global.R")
}
source("R/mod_utils.R")
source("R/mod_trends.R")
source("R/mod_export.R")

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  # --- Initialize filter module ----------------------------------------------
  filters <- filter_server("filters")

  # --- Initialize tab modules ------------------------------------------------
  trends_server("trends", filters)
  export_server("export", filters)

  # --- Footer: data source and author info -----------------------------------
  output$data_source_info <- renderText({
    max_window <- max(as.Date(all_time_windows), na.rm = TRUE)
    dhis2_date <- max_window + 6L
    paste0("DHIS2-Tracker - ", format(dhis2_date, "%d-%m-%Y"))
  })

  output$app_author_info <- renderText({
    max_window <- max(as.Date(all_time_windows), na.rm = TRUE)
    dhis2_date <- max_window + 6L
    year <- format(dhis2_date, "%Y")
    paste0("IOA-CAI © ", year)
  })

  # --- Session info ----------------------------------------------------------
  session$onSessionEnded(function() {
    message("Session ended at ", Sys.time())
  })
}
