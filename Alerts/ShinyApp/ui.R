# =============================================================================
# BVD Alerts App — UI Definition
# =============================================================================
# Single-page layout with frozen centered header at the top, trends analysis,
# health zone monitoring, and export options at the bottom.
# =============================================================================

# --- Source dependencies (guarded against double-loading) --------------------
if (!exists(".bvd_global_loaded") || !.bvd_global_loaded) {
  source("R/global.R")
}
source("R/mod_utils.R")
source("R/mod_trends.R")
source("R/mod_export.R")

# --- UI ----------------------------------------------------------------------
ui <- tagList(
  # Custom CSS & inline full-width enforcement
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(HTML("
      html, body {
        width: 100% !important;
        max-width: 100% !important;
        margin: 0 !important;
        padding: 0 !important;
      }
      .container-fluid,
      .container-xxl,
      .container-xl,
      .container-lg,
      .container,
      main {
        width: 100% !important;
        max-width: 100% !important;
        margin-left: 0 !important;
        margin-right: 0 !important;
        padding-left: 0 !important;
        padding-right: 0 !important;
      }
      .app-content-body {
        padding: 1.5rem 2rem !important;
      }

      /* Frozen Header: sticky right at top of page, centered */
      .app-frozen-header {
        position: sticky !important;
        top: 0 !important;
        z-index: 1030 !important;
        background-color: #ffffff !important;
        border-bottom: 1px solid #e9ecef !important;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05) !important;
        padding: 1.15rem 2rem !important;
        width: 100% !important;
        text-align: center !important;
      }

      .app-frozen-title {
        font-size: 1.45rem !important;
        font-weight: 700 !important;
        letter-spacing: -0.01em !important;
        color: #212529 !important;
        margin-top: 0 !important;
        margin-bottom: 0.35rem !important;
        text-align: center !important;
      }

      .app-frozen-subtitle {
        font-size: 0.92rem !important;
        color: #6c757d !important;
        line-height: 1.45 !important;
        margin-bottom: 0 !important;
        max-width: 900px !important;
        margin-left: auto !important;
        margin-right: auto !important;
        text-align: center !important;
      }

      @media (max-width: 768px) {
        .app-frozen-header {
          padding: 0.85rem 1rem !important;
        }
        .app-frozen-title {
          font-size: 1.2rem !important;
        }
        .app-frozen-subtitle {
          font-size: 0.82rem !important;
        }
        .app-content-body {
          padding: 1rem !important;
        }
      }
    "))
  ),

  page_fluid(
    theme = create_app_theme(),
    title = "Analyse des tendances et performance des alertes de la MVE/B",

    # Frozen header centered at top of page
    div(
      class = "app-frozen-header",
      h1(class = "app-frozen-title", "Analyse des tendances et performance des alertes de la MVE/B"),
      p(
        class = "app-frozen-subtitle text-muted mb-0",
        "Suivi longitudinal des alertes de cas et de décès par rapport aux seuils attendus, ",
        "avec évaluation de la performance (adéquation) au niveau global et par zone de santé."
      )
    ),

    # Main page content
    div(
      class = "app-content-body",
      trends_ui("trends"),

      tags$hr(class = "my-5"),

      # Export section at bottom of main page
      export_ui("export")
    ),

    # Page footer
    div(
      class = "app-footer text-center mt-5 mb-4",
      tags$p(
        class = "text-muted small mb-1",
        "Source des données : ", textOutput("data_source_info", inline = TRUE)
      ),
      tags$p(
        class = "text-muted small mb-0",
        textOutput("app_author_info", inline = TRUE)
      )
    )
  )
)
