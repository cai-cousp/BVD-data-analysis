# =============================================================================
# BVD Alerts Dashboard — Launch Script
# =============================================================================
# Entry point for launching the Shiny app. Sources ui.R and server.R,
# then creates the shinyApp object.
#
# Launch with:
#   shiny::runApp(".")
# or from R console:
#   shiny::runApp("/path/to/ShinyApp")
# =============================================================================

# Source UI and server definitions
source("ui.R")
source("server.R")

# Create and return the Shiny app object
shinyApp(ui = ui, server = server)
