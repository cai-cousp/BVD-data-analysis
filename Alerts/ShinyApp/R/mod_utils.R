# =============================================================================
# BVD Alerts App — Shared utilities: theme, filter module, helpers
# =============================================================================

# --- Theme configuration -----------------------------------------------------
create_app_theme <- function() {
  bs_theme(
    version   = 5,
    bootswatch = "litera",
    primary   = "#0D6EFD",
    secondary = "#6C757D",
    success   = "#198754",
    danger    = "#DC3545",
    warning   = "#FFC107",
    info      = "#0DCAF0",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    "font-size-base"     = "1rem",
    "card-border-radius" = "0.75rem",
    "card-border-width"  = "0",
    "card-cap-bg"        = "transparent",
    "navbar-bg"          = "#ffffff",
    "navbar-light-color" = "#495057",
    "navbar-light-active-color" = "#0D6EFD"
  )
}

# --- Filter module UI (horizontal, compact) ---------------------------------
# Designed to sit at the top of a tab as a card-based filter bar.
filter_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "filter-bar",
    layout_column_wrap(
      width = 1 / 4,
      heights_equal = "all",
      gap = "16px",
      fill = FALSE,

      # Date range
      pickerInput(
        inputId = ns("week_start"),
        label = "Start week",
        choices = setNames(as.character(all_weeks), format(all_weeks, "%d %b %Y")),
        selected = as.character(date_range_full[1]),
        options = list(`live-search` = TRUE, size = 10),
        width = "100%"
      ),

      pickerInput(
        inputId = ns("week_end"),
        label = "End week",
        choices = setNames(as.character(all_weeks), format(all_weeks, "%d %b %Y")),
        selected = as.character(date_range_full[2]),
        options = list(`live-search` = TRUE, size = 10),
        width = "100%"
      ),

      # Health zone multi-select grouped by province
      pickerInput(
        inputId = ns("selected_hzs"),
        label = "Health zones",
        choices = split(all_hz, hz_metadata$Province[match(all_hz, hz_metadata$zone_sante_notification)]),
        selected = all_hz,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        width = "100%"
      ),

      # Alert level + adequacy combined
      div(
        radioButtons(
          inputId = ns("alert_level"),
          label = "Alert type",
          choices = c(
            "All"   = "all",
            "Cases" = "case",
            "Deaths" = "death"
          ),
          selected = "all",
          inline = TRUE
        )
      )
    )
  )
}

# --- Filter module server ----------------------------------------------------
filter_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Validate date range
    observeEvent(input$week_start, {
      req(input$week_start, input$week_end)
      if (as.Date(input$week_start) > as.Date(input$week_end)) {
        updatePickerInput(session, "week_end",
                          selected = input$week_start)
      }
    })

    list(
      date_range = reactive({
        if (is.null(input$week_start) || is.null(input$week_end)) {
          return(date_range_full)
        }
        c(as.Date(input$week_start), as.Date(input$week_end))
      }),
      selected_hzs = reactive({
        if (is.null(input$selected_hzs)) {
          return(all_hz)
        }
        input$selected_hzs
      }),
      alert_level = reactive({
        input$alert_level %||% "all"
      }),
      adequacy_filter = reactive({
        c("Under-alerting", "Adequate", "Over-alerting")
      })
    )
  })
}

# --- Shared reactive: filtered trends ----------------------------------------
filtered_trends <- function(filters) {
  reactive({
    dr <- filters$date_range()
    hzs <- filters$selected_hzs()

    trends_smooth |>
      filter(
        .data$week_start >= dr[1],
        .data$week_start <= dr[2],
        .data$zone_sante_notification %in% hzs
      )
  })
}

# --- Shared reactive: filtered synthesis -------------------------------------
filtered_synthesis <- function(filters) {
  reactive({
    dr <- filters$date_range()
    hzs <- filters$selected_hzs()

    synthesis |>
      filter(
        .data$week_start >= dr[1],
        .data$week_start <= dr[2],
        .data$zone_sante_notification %in% hzs
      )
  })
}

# --- Formatting helpers ------------------------------------------------------
format_pct <- function(x, digits = 1) {
  paste0(round(x * 100, digits), "%")
}

format_number <- function(x, digits = 0) {
  formatC(round(x, digits), format = "f", big.mark = ",", digits = digits)
}

title_case_hz <- function(x) {
  str_to_title(x)
}

# --- Color constants ---------------------------------------------------------
adequacy_colors <- c(
  "Under-alerting" = "#DC3545",
  "Adequate"       = "#198754",
  "Over-alerting"  = "#FFC107"
)

trend_icons <- c(
  "Increasing" = "arrow-up",
  "Decreasing" = "arrow-down",
  "Stable"     = "minus",
  "Unknown"    = "question"
)

approach_colors <- c(
  "Approach A (CMR)"            = "#0D6EFD",
  "Approach B (Beni)"           = "#FD7E14",
  "Approach C (Case-derived)"   = "#198754",
  "Consensus"                   = "#6C757D"
)

# --- DT datatable scrollable helper (continuous scrolling, no row limits) ----
render_scrollable_dt <- function(df, col_names = NULL, title = NULL,
                                 scrollY = "350px",
                                 num_cols_1 = NULL, num_cols_2 = NULL, num_cols_3 = NULL) {
  dt <- DT::datatable(
    df,
    rownames = FALSE,
    caption = title,
    colnames = if (!is.null(col_names)) col_names else names(df),
    options = list(
      paging = FALSE,
      scrollY = scrollY,
      scrollX = TRUE,
      scrollCollapse = TRUE,
      dom = "ti",
      autoWidth = FALSE
    ),
    class = "compact stripe hover row-border",
    style = "bootstrap4"
  )
  if (!is.null(num_cols_1)) dt <- DT::formatRound(dt, num_cols_1, digits = 1)
  if (!is.null(num_cols_2)) dt <- DT::formatRound(dt, num_cols_2, digits = 2)
  if (!is.null(num_cols_3)) dt <- DT::formatRound(dt, num_cols_3, digits = 3)
  dt
}

