# Alert trend plots with threshold boundaries
#
# Produces ggplot2 line charts of `case_alerts` or `death_alerts` over
# `threshold_time_key` with a ribbon band for lower and upper thresholds,
# plus an interactive plotly variant via `ggplotly()` or native `plot_ly()`.
#
# Data source: output from R/02_alert_trends.R (02_trends_smooth_adeq.rds)
#
# Conventions: native pipe `|>`, `.by` grouping, here::here() paths.


suppressPackageStartupMessages({
  library(ggplot2)
})

# ---------------------------------------------------------------- #
# Helpers
# ---------------------------------------------------------------- #

#' Find the most recent dated output directory containing a target file
#'
#' @param output_dir Character. Base output directory (e.g. here::here("output")).
#' @param pattern Character. Filename pattern to locate.
#' @return Character path to the most recent matching file, or NA if none.
#' @keywords internal
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

#' Extract the date from the most recent EVD cleaned line list filename
#'
#' Scans for `evd.clean_Int_YYYY_MM_DD_HHMM.rds` files, finds the most recent,
#' and parses the embedded date — without loading the full RDS.
#' Used by [resolve_source_date()] to determine the "as-of" date for the
#' `"Source: DHIS2 Tracker"` plot caption.
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
    path      = base_path,
    pattern   = "evd.clean_Int_.*\\.rds",
    full.names = TRUE,
    recursive  = TRUE
  )

  if (length(files) == 0L) {
    rlang::abort("No cleaned EVD line list found.")
  }

  # Same "latest file" heuristic as load_latest_evd(): strip non-digits,
  # take the maximum numeric value.
  latest_file <- files[which.max(as.numeric(gsub("\\D", "", basename(files))))]

  # Filename embeds the date in YYYY_MM_DD format preceding the _HHMM
  # timestamp.  Collapse all digits and take the first 8 (YYYYMMDD).
  digits   <- gsub("\\D", "", basename(latest_file))
  date_str <- substr(digits, 1L, 8L)

  d <- as.Date(date_str, "%Y%m%d")
  if (is.na(d) || !is.finite(d)) {
    rlang::abort(
      sprintf("Could not parse date from EVD filename: %s", basename(latest_file))
    )
  }

  d
}

#' Prepare trends_smooth_adeq data for plotting
#'
#' Converts `threshold_time_key` to Date, optionally filters by health zone and
#' a start date, and drops rows with missing dates so ggplot x-axis scaling
#' works.
#'
#' @param data Dataframe. Defaults to reading the most recent RDS.
#' @param data_path Character. Optional explicit path to the RDS file.
#' @param hz Character vector. Optional health zone(s) to keep.
#' @param start_date Date or character. Earliest date (inclusive) to keep on
#'   the x-axis. Defaults to the first day of May 2026 (`"2026-05-01"`), so all
#'   plots start from the first week of May. Pass `NULL` to keep all rows.
#' @param metric Character. `"case"` or `"death"` — which alert metric to
#'   validate columns for.
#' @return A tibble with an added `date` column (Date).
#' @keywords internal
prepare_trend_data <- function(data = NULL, data_path = NULL, hz = NULL,
                               start_date = as.Date("2026-05-01"),
                               metric = c("case", "death"),
                               approach = c("Average", "C", "B")) {
  if (is.null(data)) {
    if (is.null(data_path)) {
      data_path <- latest_output_file(here::here("output"), "02_trends_smooth_adeq\\.rds")
      if (is.na(data_path)) {
        rlang::abort(
          "No '02_trends_smooth_adeq.rds' found under output/. ",
          "Run R/02_alert_trends.R first or pass `data` / `data_path`."
        )
      }
    }
    data <- readRDS(data_path)
  }

  # Capture the DHIS2 "as-of" date attribute before dplyr verbs process the
  # data.  The attribute is re-attached at the end so resolve_source_date()
  # can find it downstream.
  .evd_max_date <- attr(data, "evd_max_date")

  # Resolve metric column names for validation
  cols <- resolve_metric_columns(metric, approach)

  required <- c("threshold_time_key", cols$y,
                cols$lower, cols$upper,
                "zone_sante_notification")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(c(
      "Input data is missing required columns:",
      x = paste(missing_cols, collapse = ", ")
    ))
  }

  out <- data |>
    dplyr::mutate(date = as.Date(threshold_time_key)) |>
    dplyr::filter(!is.na(.data$date))

  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    out <- out |> dplyr::filter(.data$date >= .env$start_date)
    if (nrow(out) == 0L) {
      rlang::abort(c(
        "No rows remain after filtering by `start_date`.",
        x = paste("Earliest date requested:", start_date)
      ))
    }
  }

  if (!is.null(hz)) {
    out <- out |> dplyr::filter(zone_sante_notification %in% .env$hz)
    if (nrow(out) == 0L) {
      rlang::abort(c(
        "No rows remain after filtering by `hz`.",
        x = paste("Requested:", paste(hz, collapse = ", ")),
        i = "Check spelling against `unique(data$zone_sante_notification)`."
      ))
    }
  }

  # Re-attach the DHIS2 "as-of" date so downstream resolve_source_date() can
  # discover it even when dplyr verbs have stripped the attribute.
  if (!is.null(.evd_max_date)) {
    attr(out, "evd_max_date") <- .evd_max_date
  }

  out
}

#' Resolve the DHIS2 tracker "as-of" date for the plot caption
#'
#' Determines the date to show in captions formatted as
#' `"Source: DHIS2 Tracker - DDMMYYYY"`. Resolution order:
#'   1. explicit `source_date` argument (takes precedence);
#'   2. date embedded in the most recent `evd.clean_Int_*.rds` filename
#'      (data-export date, via [extract_latest_evd_date()]);
#'   3. `attr(data, "evd_max_date")` attached by 01_alert_thresholds.R;
#'   4. `max(data$date)` if the prepared data has a `date` column;
#'   5. `Sys.Date()` as a final fallback.
#'
#' @param data Dataframe. Prepared trend data (may carry the attribute).
#' @param source_date Date/character/NULL. Explicit override.
#' @return A `Date` scalar (always finite).
#' @keywords internal
resolve_source_date <- function(data = NULL, source_date = NULL) {
  # 1. Explicit override
  if (!is.null(source_date)) {
    d <- suppressWarnings(as.Date(source_date))
    if (!is.na(d)) return(d)
  }
  # 2. EVD filename date — data-export date from the cleaned line list
  d <- tryCatch(
    extract_latest_evd_date(),
    error = function(e) NULL
  )
  if (!is.null(d) && is.finite(d)) return(d)
  # 3. Attribute carried by the RDS produced in 01_alert_thresholds.R
  if (!is.null(data)) {
    a <- attr(data, "evd_max_date")
    if (!is.null(a) && length(a) > 0L) {
      d <- suppressWarnings(as.Date(a))
      if (!is.na(d) && is.finite(d)) return(d)
    }
    # 4. max(date) in the prepared data
    if ("date" %in% names(data)) {
      d <- suppressWarnings(max(as.Date(data$date), na.rm = TRUE))
      if (is.finite(d)) return(d)
    }
  }
  # 5. Final fallback
  Sys.Date()
}

#' Format a date as DDMMYYYY (zero-padded day and month, full year)
#'
#' @param x Date scalar.
#' @return Character scalar, e.g. `"17072026"` for 2026-07-17.
#' @keywords internal
format_dmy <- function(x) format(as.Date(x), "%d-%m-%Y")

# Shared Plotly legend placement for interactive alert plots
plotly_top_legend <- list(
  orientation = "h",
  x = 0,
  xanchor = "left",
  y = 1.02,
  yanchor = "bottom"
)

# Canonical paper-coordinate bands for interactive alert trends. Faceted plots
# need one extra band for health-zone panel labels below the shared legend.
alert_plotly_layout_bands <- list(
  single = list(
    title = list(x = 0, xanchor = "left", y = 1.19, yanchor = "bottom"),
    subtitle = list(x = 0, xanchor = "left", y = 1.14, yanchor = "bottom"),
    subtitle_secondary = list(x = 0, xanchor = "left", y = 1.07, yanchor = "bottom"),
    legend = list(orientation = "h", x = 0, xanchor = "left", y = 1, yanchor = "bottom"),
    caption = list(x = 0, xanchor = "left", y = -0.085, yanchor = "top")
  ),
  faceted = list(
    title = list(x = 0, xanchor = "left", y = 1.23, yanchor = "bottom"),
    subtitle = list(x = 0, xanchor = "left", y = 1.16, yanchor = "bottom"),
    subtitle_secondary = list(x = 0, xanchor = "left", y = 1.09, yanchor = "bottom"),
    legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.04, yanchor = "bottom"),
    caption = list(x = 0, xanchor = "left", y = -0.085, yanchor = "top")
  )
)

alert_plotly_margins <- list(
  single = list(t = 165, b = 75, l = 65, r = 25),
  faceted = list(t = 190, b = 75, l = 65, r = 25)
)

alert_plotly_annotation <- function(text, position, size = 12) {
  list(
    text = text,
    xref = "paper",
    yref = "paper",
    x = position$x,
    y = position$y,
    xanchor = position$xanchor,
    yanchor = position$yanchor,
    showarrow = FALSE,
    font = list(size = size)
  )
}

# ---------------------------------------------------------------- #
# Metric column resolver
# ---------------------------------------------------------------- #

#' Resolve column names for case or death alert metric
#'
#' Maps a metric string to corresponding column names in the
#' trends_smooth_adeq data so all plot functions share a single
#' source of truth.
#'
#' @param metric Character. Either `"case"` or `"death"`.
#' @return A named list of column names and display labels.
#' @keywords internal
resolve_metric_columns <- function(metric = c("case", "death"), approach = c("Average", "C", "B")) {
  metric <- rlang::arg_match(metric)
  approach <- rlang::arg_match(approach)
  if (approach == "Average") {
    list(
      y            = paste0(metric, "_alerts"),
      lower        = paste0("Alert_", metric, "_threshold_lower"),
      upper        = paste0("Alert_", metric, "_threshold_upper"),
      midpoint     = paste0("Alert_", metric, "_threshold"),
      midpoint_alt = NULL,
      avg_lower    = paste0("Alert_", metric, "_threshold_lower"),
      avg_upper    = paste0("Alert_", metric, "_threshold_upper"),
      rolling      = paste0(metric, "_alerts_3w"),
      label        = if (metric == "case") "Alertes de cas" else "Alertes de décès",
      adequacy_col = "adequacy_category"
    )
  } else {
    list(
      y            = paste0(metric, "_alerts"),
      lower        = paste0("alert_", metric, "_threshold_lower_", approach),
      upper        = paste0("alert_", metric, "_threshold_upper_", approach),
      midpoint     = paste0("alert_", metric, "_threshold_", approach),
      midpoint_alt = paste0(metric, "_threshold_mid_", approach),
      avg_lower    = paste0("Alert_", metric, "_threshold_lower"),
      avg_upper    = paste0("Alert_", metric, "_threshold_upper"),
      rolling      = paste0(metric, "_alerts_3w"),
      label        = if (metric == "case") "Alertes de cas" else "Alertes de décès",
      adequacy_col = paste0("adequacy_category_", approach)
    )
  }
}

# ---------------------------------------------------------------- #
# Static ggplot2
# ---------------------------------------------------------------- #

#' Plot alert trends (cases or deaths) with threshold boundaries
#'
#' Line chart of `case_alerts` or `death_alerts` over time, with a ribbon band
#' bounded by the corresponding threshold columns.
#'
#' @param data Dataframe. Optional trends_smooth_adeq data (avoids re-reading RDS).
#' @param data_path Character. Optional explicit path to the RDS file.
#' @param hz Character vector. Health zone(s) to filter to. When a single HZ
#'   is given, the plot is a single panel; when multiple and `facet = TRUE`,
#'   they are faceted.
#' @param facet Logical. Facet by `zone_sante_notification`? Default `TRUE`.
#'   Automatically suppressed when a single HZ is selected.
#' @param ncol Integer. Number of facet columns when faceting. Default `NULL`
#'   lets ggplot choose.
#' @param scales Character. Axis scaling across facets: `"fixed"` (default, shared
#'   y-axis) or `"free_y"` / `"free"`.
#' @param show_rolling Logical. Overlay the 3-week rolling mean as a dashed
#'   red line? Default `FALSE`.
#' @param colour_by_adequacy Logical. Colour points by `adequacy_category`?
#'   Default `FALSE`.
#' @param one_per_hz Logical. When `TRUE`, return a **named list** of ggplot
#'   objects - one plot per health zone in the (optionally `hz`-filtered) data,
#'   each a single-panel chart. When `FALSE` (default), return a single ggplot
#'   (faceted across HZs, or single-panel if a single HZ is selected). The
#'   `facet` argument is ignored when `one_per_hz = TRUE`.
#' @param metric Character. `"case"` (default) plots case alerts against case
#'   thresholds; `"death"` plots death alerts against death thresholds.
#' @param start_date Date or character. Earliest date (inclusive) to plot.
#'   Defaults to the first day of May 2026 (`"2026-05-01"`) so every plot starts
#'   from the first week of May. Pass `NULL` to keep the full history.
#' @param source_date Date or character. "As-of" date shown in the caption as
#'   `Source: DHIS2 Tracker - DDMMYYYY`. When `NULL` (default), the date is
#'   resolved from the most recent `evd.clean_Int_*.rds` filename date
#'   (via `extract_latest_evd_date()`), then from
#'   `attr(data, "evd_max_date")` (attached by `01_alert_thresholds.R`),
#'   then from `max(data$date)`, then `Sys.Date()`.
#' @param for_plotly Logical. Internal: when `TRUE`, add a `text` aesthetic
#'   carrying hover tooltips for [plotly::ggplotly()]. When `FALSE` (default),
#'   the `text` aesthetic is omitted to avoid ggplot2 "unknown aesthetic"
#'   warnings on the static plot. Set automatically by
#'   [plot_alert_trends_interactive()].
#' @return A ggplot object, or (when `one_per_hz = TRUE`) a named list of
#'   ggplot objects keyed by `zone_sante_notification`.
#' @export
plot_alert_trends <- function(
  data = NULL,
  data_path = NULL,
  hz = NULL,
  facet = TRUE,
  ncol = NULL,
  scales = "fixed",
  show_rolling = FALSE,
  colour_by_adequacy = FALSE,
  one_per_hz = FALSE,
  metric = c("case", "death"),
  approach = c("Average", "C", "B"),
  start_date = as.Date("2026-05-01"),
  source_date = NULL,
  for_plotly = FALSE
) {
  metric <- rlang::arg_match(metric)
  approach <- rlang::arg_match(approach)
  cols <- resolve_metric_columns(metric, approach)

  # --- One-plot-per-HZ mode ---------------------------------------------
  # Return a named list of single-panel plots, one per health zone. Recurses
  # into the single-plot path (one_per_hz reset to FALSE) so the exact same
  # styling/theming is reused. `facet` and `ncol` are ignored here.
  if (one_per_hz) {
    prep <- prepare_trend_data(data = data, data_path = data_path, hz = hz,
                               start_date = start_date, metric = metric, approach = approach)
    hz_list <- unique(prep$zone_sante_notification)
    if (length(hz_list) == 0L) {
      rlang::abort("`one_per_hz = TRUE` but no health zones found after filtering.")
    }
    plots <- lapply(hz_list, function(z) {
      plot_alert_trends(
        data = prep, hz = z, facet = FALSE, ncol = NULL, scales = scales,
        show_rolling = show_rolling,
        colour_by_adequacy = colour_by_adequacy,
        one_per_hz = FALSE, metric = metric, approach = approach,
        start_date = start_date,
        source_date = source_date, for_plotly = for_plotly
      )
    })
    return(stats::setNames(plots, hz_list))
  }

  plot_data <- prepare_trend_data(data = data, data_path = data_path, hz = hz,
                                  start_date = start_date, metric = metric, approach = approach)
  # Resolve the caption "as-of" date once; re-use on the ggplot labs(caption=).
  caption_date <- resolve_source_date(data = plot_data, source_date = source_date)

  has_rolling <- show_rolling && cols$rolling %in% names(plot_data)
  has_adequacy <- colour_by_adequacy && cols$adequacy_col %in% names(plot_data)
  midpoint_col <- if (cols$midpoint %in% names(plot_data)) {
    cols$midpoint
  } else if (!is.null(cols$midpoint_alt) && cols$midpoint_alt %in% names(plot_data)) {
    cols$midpoint_alt
  } else {
    NULL
  }

  # Sort so lines render correctly per HZ
  plot_data <- plot_data |>
    dplyr::arrange(zone_sante_notification, date)

  # --- Base aesthetics -------------------------------------------------
  # A `text` aesthetic carrying hover tooltips is only added when building
  # for plotly (`for_plotly = TRUE`); the static ggplot omits it so ggplot2
  # does not emit "Ignoring unknown aesthetics" warnings.
  # The tooltip column is always built so the interactive wrapper can reuse
  # the same data path.
  plot_data <- plot_data |>
    dplyr::mutate(
      adequacy_val = if (has_adequacy) .data[[cols$adequacy_col]] else NA_character_,
      adequacy_fr = dplyr::case_when(
        adequacy_val == "Under-alerting" ~ "Sous-alerte",
        adequacy_val == "Adequate"       ~ "Adéquat",
        adequacy_val == "Over-alerting"  ~ "Sur-alerte",
        .default = as.character(adequacy_val)
      ),
      tooltip_text = paste0(
        "Zone de santé : ", zone_sante_notification,
        "\nSemaine de notification (début) : ", format(date, "%d/%m/%Y"),
        "\n", cols$label, " : ", .data[[cols$y]],
        "\nSeuil attendu : [", round(.data[[cols$lower]], 1),
        " - ", round(.data[[cols$upper]], 1), "]",
        if (!is.null(midpoint_col)) {
          paste0("\nSeuil médian : ", round(.data[[midpoint_col]], 1))
        } else {
          ""
        },
        if (has_adequacy) paste0("\nPerformance : ", adequacy_fr) else ""
      )
    )

  # Build aes() components dynamically so we can reference metric columns
  y_sym  <- rlang::sym(cols$y)
  lo_sym <- rlang::sym(cols$lower)
  up_sym <- rlang::sym(cols$upper)
  mi_sym <- rlang::sym(cols$midpoint)
  rl_sym <- rlang::sym(cols$rolling)

  avg_lo_sym <- rlang::sym(cols$avg_lower)
  avg_up_sym <- rlang::sym(cols$avg_upper)

  p <- ggplot(plot_data, aes(x = date))
  
  # Optional base Average threshold band behind the main one
  if (approach != "Average") {
    p <- p + geom_ribbon(
      aes(ymin = !!avg_lo_sym, ymax = !!avg_up_sym),
      fill = "grey", alpha = 0.15, colour = NA
    )
  }

  p <- p +
    # Threshold band (drawn first so it sits behind the line)
    geom_ribbon(
      aes(ymin = !!lo_sym, ymax = !!up_sym),
      fill = "steelblue", alpha = 0.18, colour = NA
    ) +
    # Optional mid-threshold reference line
    {
      if (!is.null(midpoint_col)) {
        geom_line(
          aes(y = .data[[midpoint_col]], linetype = "Seuil médian"),
          colour = "steelblue",
          linewidth = 0.6,
          na.rm = TRUE
        )
      }
    }

  # --- Observed alerts line + points -----------------------------------
  # The line traces the overall trend in a single colour (black).
  # Points carry the adequacy-category colour so each week's status
  # stands out individually.  Both layers inherit the optional `text`
  # aesthetic for plotly tooltips when `for_plotly = TRUE`.
  #
  # Line aesthetic: y only, plus text if for_plotly
  line_extra <- list()
  if (for_plotly) {
    line_extra$text <- rlang::quo(.data[["tooltip_text"]])
  }
  line_aes <- aes(y = !!y_sym, !!!line_extra)

  # Point aesthetic: y, plus colour if has_adequacy, plus text if for_plotly
  point_extra <- list()
  if (has_adequacy) {
    point_extra$colour <- rlang::sym(cols$adequacy_col)
  }
  if (for_plotly) {
    point_extra$text <- rlang::quo(.data[["tooltip_text"]])
  }
  point_aes <- aes(y = !!y_sym, !!!point_extra)

  p <- p + geom_line(line_aes, colour = "black", linewidth = 0.7) +
    geom_point(point_aes, size = 1.6)

  if (!is.null(midpoint_col)) {
    p <- p + ggplot2::scale_linetype_manual(
      name = NULL,
      values = c("Seuil médian" = "dotted")
    )
  }

  # --- Optional 3-week rolling mean overlay ----------------------------
  if (has_rolling) {
    p <- p + geom_line(
      aes(y = !!rl_sym),
      colour = "firebrick", linetype = "dashed", linewidth = 0.6, na.rm = TRUE
    )
  }

  # --- Scales ----------------------------------------------------------
  if (has_adequacy) {
    adequacy_levels <- c("Under-alerting", "Adequate", "Over-alerting")
    adequacy_palette <- c(
      "Under-alerting" = "#D55E00",
      "Adequate"        = "#009E73",
      "Over-alerting"   = "#CC79A7"
    )
    adequacy_labels <- c(
      "Under-alerting" = "Sous-alerte",
      "Adequate"        = "Adéquat",
      "Over-alerting"   = "Sur-alerte"
    )
    present_levels <- intersect(adequacy_levels, unique(plot_data[[cols$adequacy_col]]))
    p <- p + scale_colour_manual(
      name = "Performance",
      values = adequacy_palette,
      breaks = present_levels,
      labels = unname(adequacy_labels[present_levels]),
      drop = TRUE
    )
  }

  # --- Faceting --------------------------------------------------------
  # Facet by health zone unless the caller selected a single HZ (a single
  # panel needs no facet strip).
  single_hz <- !is.null(hz) && length(unique(hz)) == 1L
  if (facet && !single_hz) {
    facet_args <- list(~zone_sante_notification)
    if (!is.null(ncol)) facet_args$ncol <- ncol
    p <- p + do.call(facet_wrap, c(facet_args, list(scales = scales)))
  }

  # --- Labels, theme, scales -------------------------------------------
  metric_title_label <- if (metric == "case") "alertes de cas" else "alertes de décès"

  # When a single health zone is shown, name it in the subtitle so each per-HZ
  # chart is self-identifying.
  base_subtitle <- sprintf(
    "%s observées (ligne/points) vs. seuils attendus (ruban)",
    cols$label
  )
  subtitle_text <- if (single_hz) {
    zone_label <- if (hz[[1L]] == "Ensemble de la zone affectée") {
      "Ensemble de la zone affectée"
    } else {
      paste0("Zone de santé : ", hz[[1L]])
    }
    paste0(base_subtitle, "\n", zone_label, "\nApproche : ", approach)
  } else {
    paste0(base_subtitle, "\nApproche : ", approach)
  }

  y_label <- sprintf("%s (nombre)", cols$label)

  unique_dates <- sort(unique(plot_data$date))
  p <- p +
    scale_x_date(breaks = unique_dates, date_labels = "%d-%m\n%Y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    labs(
      title = sprintf("Tendances des %s vs seuils attendus", metric_title_label),
      subtitle = subtitle_text,
      x = "Semaine de notification (date de début)",
      y = y_label,
      caption = sprintf("Source : DHIS2 Tracker - %s", format_dmy(caption_date))
    ) +
    #theme_minimal(base_size = 11) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      #axis.ticks = element_line(colour = "grey50", linewidth = 0.3),
      axis.line = element_line(colour = "grey50", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = rel(0.85)),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = rel(0.75)),
      legend.position = "top",
      legend.direction = "horizontal",
      plot.title = element_text(face = "bold")
    )

  # Large-facet guard: warn but still render
  if (facet && !single_hz) {
    n_hz <- dplyr::n_distinct(plot_data$zone_sante_notification)
    if (n_hz > 24L) {
      rlang::warn(c(
        "!" = paste0(n_hz, " health zones will be faceted; the plot may be hard to read."),
        i = "Pass `hz = c(...)` to restrict, or call `facet = FALSE`."
      ))
    }
  }

  p
}

# ---------------------------------------------------------------- #
# Interactive plotly
# ---------------------------------------------------------------- #

#' Position an interactive alert trend header and legend
#'
#' `ggplotly()` and the native Plotly fallback use different mechanisms for
#' titles and subtitles. This helper gives both a consistent vertical order:
#' title, subtitle, horizontal legend, then plot area.
#'
#' @param widget A plotly htmlwidget built by [plot_alert_trends_interactive()].
#' @return The plotly htmlwidget with repositioned header elements.
#' @keywords internal
position_alert_plotly_header <- function(widget,
                                         show_legend = TRUE,
                                         show_header = TRUE,
                                         show_caption = TRUE,
                                         margin = NULL) {
  widget <- plotly::plotly_build(widget)
  layout <- widget$x$layout
  annotations <- layout$annotations
  xaxis_names <- grep("^xaxis([0-9]+)?$", names(layout), value = TRUE)
  bands_name <- if (length(xaxis_names) > 1L) "faceted" else "single"
  bands <- alert_plotly_layout_bands[[bands_name]]

  annotations <- lapply(annotations, function(annotation) {
    text <- if (is.null(annotation$text)) "" else annotation$text
    if (grepl("Tendances des .*vs seuils attendus", text)) {
      if (!isTRUE(show_header)) return(NULL)
      annotation$x <- bands$title$x
      annotation$xanchor <- bands$title$xanchor
      annotation$y <- bands$title$y
      annotation$yanchor <- bands$title$yanchor
    } else if (grepl("observées \\(ligne/points\\)", text)) {
      if (!isTRUE(show_header)) return(NULL)
      annotation$x <- bands$subtitle$x
      annotation$xanchor <- bands$subtitle$xanchor
      annotation$y <- bands$subtitle$y
      annotation$yanchor <- bands$subtitle$yanchor
    } else if (grepl("^Source\\s*:", text)) {
      if (!isTRUE(show_caption)) return(NULL)
      annotation$x <- bands$caption$x
      annotation$xanchor <- bands$caption$xanchor
      annotation$y <- bands$caption$y
      annotation$yanchor <- bands$caption$yanchor
    }
    annotation
  })
  annotations <- Filter(Negate(is.null), annotations)

  layout_title <- layout$title
  title <- NULL
  if (!is.null(layout_title)) {
    title_text <- if (is.character(layout_title)) {
      layout_title
    } else {
      layout_title$text
    }
    title <- title_text
  }

  if (!is.null(title) && nzchar(title)) {
    if (isTRUE(show_header)) {
      has_title_annotation <- any(vapply(
        annotations,
        function(annotation) {
          text <- if (is.null(annotation$text)) "" else annotation$text
          grepl("Tendances des .*vs seuils attendus", text)
        },
        logical(1)
      ))
      if (!has_title_annotation) {
        annotations <- c(
          list(alert_plotly_annotation(title, bands$title, size = 16)),
          annotations
        )
      }
    }
    widget$x$layout$title <- list(text = "")
  }

  widget$x$layout$annotations <- annotations
  if (isTRUE(show_legend)) {
    widget$x$layout$legend <- bands$legend
    widget$x$layout$showlegend <- TRUE
  } else {
    widget$x$layout$showlegend <- FALSE
    widget$x$layout$legend <- list(visible = FALSE)
  }

  calc_margin <- if (!is.null(margin)) {
    margin
  } else if (!isTRUE(show_header) && !isTRUE(show_legend)) {
    if (isTRUE(show_caption)) {
      list(t = 15, r = 15, b = 50, l = 55)
    } else {
      list(t = 15, r = 15, b = 40, l = 55)
    }
  } else {
    alert_plotly_margins[[bands_name]]
  }

  widget$x$layout$margin <- calc_margin
  widget$x$layoutAttrs <- list()
  widget
}

#' Interactive (plotly) version of the alert trends plot
#'
#' Wraps [plot_alert_trends()] with [plotly::ggplotly()] for hover tooltips and
#' zooming. Reuses the same base plot so styling stays consistent.
#'
#' @param ... Arguments forwarded to [plot_alert_trends()], including
#'   `one_per_hz`.
#' @param tooltip Character. tooltip columns for plotly. A custom `text`
#'   aesthetic is built into the plot; default `c("text")` uses it.
#' @param show_legend Logical. Display legend? Default `TRUE`.
#' @param show_header Logical. Display chart title & subtitle annotations? Default `TRUE`.
#' @param show_caption Logical. Display source caption annotation? Default `TRUE`.
#' @param margin Optional list of plotly margins (e.g. `list(t = 15, r = 15, b = 40, l = 55)`).
#' @param display_mode_bar Logical. Display Plotly control mode bar? Default `NULL` (unmodified).
#' @return A plotly htmlwidget object, or (when `one_per_hz = TRUE` is passed
#'   through `...`) a named list of plotly htmlwidgets keyed by
#'   `zone_sante_notification`.
#' @export
plot_alert_trends_interactive <- function(...,
                                          tooltip = "text",
                                          show_legend = TRUE,
                                          show_header = TRUE,
                                          show_caption = TRUE,
                                          margin = NULL,
                                          display_mode_bar = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    rlang::abort(
      "Package 'plotly' is required for interactive plots. Install with install.packages('plotly')."
    )
  }

  dots <- list(...)
  show_legend <- if (!is.null(dots$show_legend)) isTRUE(dots$show_legend) else isTRUE(show_legend)
  show_header <- if (!is.null(dots$show_header)) isTRUE(dots$show_header) else isTRUE(show_header)
  show_caption <- if (!is.null(dots$show_caption)) isTRUE(dots$show_caption) else isTRUE(show_caption)
  display_mode_bar <- if (!is.null(dots$display_mode_bar)) {
    isTRUE(dots$display_mode_bar)
  } else if (!is.null(dots$displayModeBar)) {
    isTRUE(dots$displayModeBar)
  } else {
    display_mode_bar
  }
  if (!is.null(dots$margin)) margin <- dots$margin

  # Resolve metric early (default "case") so all paths share it.
  metric <- if (is.null(dots$metric)) "case" else match.arg(dots$metric, c("case", "death"))
  approach <- if (is.null(dots$approach)) "Average" else match.arg(dots$approach, c("Average", "C", "B"))
  cols <- resolve_metric_columns(metric, approach)

  # --- One-widget-per-HZ mode ------------------------------------------
  # Recurse over the HZ list, building one standalone interactive widget per
  # zone. Returns a named list parallelling plot_alert_trends(one_per_hz=TRUE).
  if (isTRUE(dots$one_per_hz)) {
    prep_arg_names <- c("data", "data_path", "hz", "start_date", "source_date", "metric", "approach")
    prep_args <- dots[names(dots) %in% prep_arg_names]
    prep <- do.call(prepare_trend_data, prep_args)
    hz_list <- unique(prep$zone_sante_notification)
    if (length(hz_list) == 0L) {
      rlang::abort("`one_per_hz = TRUE` but no health zones found after filtering.")
    }
    widgets <- lapply(hz_list, function(z) {
      sub_args <- dots
      sub_args$hz <- z
      sub_args$one_per_hz <- FALSE
      sub_args$tooltip <- NULL  # set via the explicit argument below
      sub_args$metric <- metric
      sub_args$approach <- approach
      sub_args$show_legend <- show_legend
      sub_args$show_header <- show_header
      sub_args$show_caption <- show_caption
      sub_args$margin <- margin
      sub_args$display_mode_bar <- display_mode_bar
      do.call(plot_alert_trends_interactive, c(sub_args, list(tooltip = tooltip)))
    })
    return(stats::setNames(widgets, hz_list))
  }

  # Resolve data/filter args once so both code paths share them.
  prep_arg_names <- c("data", "data_path", "hz", "start_date", "source_date", "metric", "approach")
  prep_args <- dots[names(dots) %in% prep_arg_names]
  plot_data <- do.call(prepare_trend_data, prep_args)
  has_rolling <- isTRUE(dots$show_rolling) && cols$rolling %in% names(plot_data)
  colour_by <- if (is.null(dots$colour_by_adequacy)) FALSE else isTRUE(dots$colour_by_adequacy)
  has_adequacy <- colour_by && cols$adequacy_col %in% names(plot_data)

  # --- Attempt 1: ggplotly on the static plot ----------------------------
  # Reuses the ggplot styling (theme, scales) for a faithful interactive copy.
  # NOTE: plotly 4.10.x is incompatible with ggplot2 4.0.0 (S7) and ggplotly()
  # fails with "subscript out of bounds" on every plot, including built-in
  # datasets. We fall back to a native plot_ly() build below when that happens.
  # The doomed attempt is wrapped in suppressWarnings() so the known
  # "Ignoring unknown aesthetics: text" note does not clutter the console.
  gp <- tryCatch(
    {
      gg_args <- dots
      gg_args$for_plotly <- TRUE
      gg_args$metric <- metric
      gg_args$approach <- approach
      p <- suppressWarnings(do.call(plot_alert_trends, gg_args))
      suppressWarnings(plotly::ggplotly(p, tooltip = tooltip))
    },
    error = function(e) NULL
  )
  res_widget <- if (!is.null(gp)) {
    position_alert_plotly_header(
      gp,
      show_legend = show_legend,
      show_header = show_header,
      show_caption = show_caption,
      margin = margin
    )
  } else {
    # --- Attempt 2: native plot_ly() build ---------------------------------
    # Renders the same line + ribbon chart directly via plotly's R API so the
    # interactive plot still works even when ggplotly() is broken.
    caption_date <- resolve_source_date(data = plot_data, source_date = dots$source_date)
    build_plotly_alert_trends(
      data = plot_data,
      metric = metric,
      approach = approach,
      has_rolling = has_rolling,
      has_adequacy = has_adequacy,
      source_date = caption_date,
      show_legend = show_legend,
      show_header = show_header,
      show_caption = show_caption,
      margin = margin
    )
  }

  if (!is.null(display_mode_bar)) {
    res_widget <- plotly::config(res_widget, displayModeBar = isTRUE(display_mode_bar))
  }
  res_widget
}

# ---------------------------------------------------------------- #
# Native plotly builder (ggplotly fallback)
# ---------------------------------------------------------------- #

#' Build alert trend chart directly with plotly::plot_ly
#'
#' Renders the same line + threshold ribbon as the ggplot version, but via
#' plotly's native R API. Used as the fallback when
#' [plotly::ggplotly()] fails (e.g. plotly 4.10.4 vs ggplot2 4.0.0).
#'
#' @param data Prepared trend data (output of [prepare_trend_data()]).
#' @param metric Character. `"case"` or `"death"`.
#' @param has_rolling Logical. Overlay 3-week rolling mean?
#' @param has_adequacy Logical. Colour points by adequacy category?
#' @param show_legend Logical. Display legend? Default `TRUE`.
#' @param show_header Logical. Display header annotations? Default `TRUE`.
#' @param show_caption Logical. Display source caption? Default `TRUE`.
#' @param margin Optional list of margins.
#' @return A plotly htmlwidget.
#' @keywords internal
build_plotly_alert_trends <- function(data, metric = c("case", "death"),
                                       approach = c("Average", "C", "B"),
                                       has_rolling = FALSE, has_adequacy = TRUE,
                                       source_date = NULL,
                                       show_legend = TRUE,
                                       show_header = TRUE,
                                       show_caption = TRUE,
                                       margin = NULL) {
  metric <- rlang::arg_match(metric)
  approach <- rlang::arg_match(approach)
  cols <- resolve_metric_columns(metric, approach)
  midpoint_col <- if (cols$midpoint %in% names(data)) {
    cols$midpoint
  } else if (!is.null(cols$midpoint_alt) && cols$midpoint_alt %in% names(data)) {
    cols$midpoint_alt
  } else {
    NULL
  }

  # Resolve the caption date from the data (or explicit override)
  caption_date <- resolve_source_date(data = data, source_date = source_date)
  caption_text <- sprintf("Source : DHIS2 Tracker - %s", format_dmy(caption_date))
  plot_data <- data |>
    dplyr::mutate(
      adequacy_val = if (has_adequacy) .data[[cols$adequacy_col]] else NA_character_,
      adequacy_fr = dplyr::case_when(
        adequacy_val == "Under-alerting" ~ "Sous-alerte",
        adequacy_val == "Adequate"       ~ "Adéquat",
        adequacy_val == "Over-alerting"  ~ "Sur-alerte",
        .default = as.character(adequacy_val)
      ),
      tooltip_text = paste0(
        "Zone de santé : ", zone_sante_notification,
        "\nSemaine de notification (début) : ", format(date, "%d/%m/%Y"),
        "\n", cols$label, " : ", .data[[cols$y]],
        "\nSeuil attendu : [", round(.data[[cols$lower]], 1),
        " - ", round(.data[[cols$upper]], 1), "]",
        if (!is.null(midpoint_col)) {
          paste0("\nSeuil médian : ", round(.data[[midpoint_col]], 1))
        } else {
          ""
        },
        if (has_adequacy) paste0("\nPerformance : ", adequacy_fr) else ""
      )
    ) |>
    dplyr::arrange(zone_sante_notification, date)

  hz_list <- unique(plot_data$zone_sante_notification)
  y_label <- sprintf("%s (nombre)", cols$label)
  metric_title_label <- if (metric == "case") "alertes de cas" else "alertes de décès"
  chart_title <- sprintf("Tendances des %s vs seuils attendus", metric_title_label)
  chart_subtitle <- sprintf(
    "%s observées (ligne/points) vs. seuils attendus (ruban)",
    cols$label
  )

  # Per-HZ sub-plot builder
  one_hz_plot <- function(hz_name, df) {
    # Base trace: black alerts line (the overall trend)
    base <- plotly::plot_ly(
      data = df,
      x = ~date, y = as.formula(paste0("~", cols$y)),
      type = "scatter", mode = "lines",
      line = list(color = "black", width = 1.6),
      name = cols$label,
      showlegend = (isTRUE(show_legend) && hz_name == hz_list[1L])
    )

    # Threshold band as a filled polygon trace (transparent)
    band_df <- df |>
      dplyr::arrange(date) |>
      dplyr::select(date, dplyr::all_of(c(cols$lower, cols$upper, cols$avg_lower, cols$avg_upper))) |>
      tidyr::drop_na()
    p <- base

    if (approach != "Average" && nrow(band_df) > 0L) {
      p <- plotly::add_ribbons(
        p,
        x = band_df$date,
        ymin = band_df[[cols$avg_lower]],
        ymax = band_df[[cols$avg_upper]],
        line = list(width = 0),
        fillcolor = "rgba(128,128,128,0.15)",
        name = "Seuil moyen",
        showlegend = (isTRUE(show_legend) && hz_name == hz_list[1L])
      )
    }

    if (nrow(band_df) > 0L) {
      p <- plotly::add_ribbons(
        p,
        x = band_df$date,
        ymin = band_df[[cols$lower]],
        ymax = band_df[[cols$upper]],
        line = list(width = 0),
        fillcolor = "rgba(70,130,180,0.18)",
        name = "Bande de seuil",
        showlegend = (isTRUE(show_legend) && hz_name == hz_list[1L])
      )
    }

    if (!is.null(midpoint_col) && midpoint_col %in% names(df)) {
      p <- plotly::add_trace(
        p,
        data = df,
        x = ~date,
        y = as.formula(paste0("~", midpoint_col)),
        type = "scatter",
        mode = "lines",
        text = ~tooltip_text,
        hoverinfo = "text",
        line = list(color = "steelblue", dash = "dot", width = 1.4),
        name = "Seuil médian",
        showlegend = (isTRUE(show_legend) && hz_name == hz_list[1L])
      )
    }

    # Observed alerts line + markers
    if (has_adequacy) {
      pal <- c(
        "Under-alerting" = "#D55E00",
        "Adequate"        = "#009E73",
        "Over-alerting"   = "#CC79A7"
      )
      lbl_fr <- c(
        "Under-alerting" = "Sous-alerte",
        "Adequate"        = "Adéquat",
        "Over-alerting"   = "Sur-alerte"
      )
      # Per-category markers only (the line is already rendered as the
      # base trace above; markers sit on top to highlight each week's
      # adequacy status).
      for (lvl in names(pal)) {
        sub <- df |> dplyr::filter(.data[[cols$adequacy_col]] == lvl)
        if (nrow(sub) == 0L) next
        show_leg <- (isTRUE(show_legend) && hz_name == hz_list[1L])
        p <- plotly::add_trace(
          p, data = sub, x = ~date, y = as.formula(paste0("~", cols$y)),
          type = "scatter", mode = "markers",
          text = ~tooltip_text, hoverinfo = "text",
          name = lbl_fr[[lvl]],
          legendgroup = lvl,
          showlegend = show_leg,
          marker = list(color = pal[[lvl]], size = 6),
          inherit = FALSE
        )
      }
    } else {
      p <- plotly::add_trace(
        p, data = df, x = ~date, y = as.formula(paste0("~", cols$y)),
        type = "scatter", mode = "markers",
        text = ~tooltip_text, hoverinfo = "text",
        name = sprintf("%s (points)", cols$label),
        showlegend = isTRUE(show_legend),
        marker = list(color = "black", size = 6),
        inherit = FALSE
      )
    }

    # Optional rolling mean
    if (has_rolling && cols$rolling %in% names(df)) {
      p <- plotly::add_trace(
        p, data = df, x = ~date, y = as.formula(paste0("~", cols$rolling)),
        type = "scatter", mode = "lines",
        line = list(color = "firebrick", dash = "dash", width = 1.4),
        name = "Moyenne mobile (3 sem)",
        showlegend = (isTRUE(show_legend) && hz_name == hz_list[1L])
      )
    }

    unique_dates <- sort(unique(df$date))
    xaxis_cfg <- list(
      title = "Semaine de notification (date de début)",
      tickmode = "array",
      tickvals = unique_dates,
      ticktext = format(unique_dates, "%d-%m\n%Y"),
      tickangle = 0,
      tickfont = list(size = 10)
    )

    # Per-subplot title is the HZ name when faceting; for a single HZ the
    # title is the chart name and the HZ is shown as a subtitle annotation.
    if (length(hz_list) > 1L) {
      p <- plotly::layout(
        p,
        xaxis = xaxis_cfg,
        yaxis = list(title = y_label)
      )
    } else {
      bands <- alert_plotly_layout_bands$single
      zone_subtitle <- if (hz_name == "Ensemble de la zone affectée") {
        "Ensemble de la zone affectée"
      } else {
        paste0("Zone de santé : ", hz_name)
      }

      annot_list <- list()
      if (isTRUE(show_header)) {
        annot_list <- c(
          annot_list,
          list(
            alert_plotly_annotation(chart_title, bands$title, size = 16),
            alert_plotly_annotation(chart_subtitle, bands$subtitle),
            alert_plotly_annotation(
              paste0(zone_subtitle, " | Approche : ", approach),
              bands$subtitle_secondary
            )
          )
        )
      }
      if (isTRUE(show_caption)) {
        annot_list <- c(
          annot_list,
          list(alert_plotly_annotation(caption_text, bands$caption, size = 10))
        )
      }

      calc_margin <- if (!is.null(margin)) {
        margin
      } else if (!isTRUE(show_header) && !isTRUE(show_legend)) {
        if (isTRUE(show_caption)) {
          list(t = 15, r = 15, b = 50, l = 55)
        } else {
          list(t = 15, r = 15, b = 40, l = 55)
        }
      } else {
        alert_plotly_margins$single
      }

      p <- plotly::layout(
        p,
        annotations = annot_list,
        legend = if (isTRUE(show_legend)) bands$legend else list(visible = FALSE),
        showlegend = isTRUE(show_legend),
        xaxis = xaxis_cfg,
        yaxis = list(title = y_label),
        margin = calc_margin
      )
    }
    p
  }

  plots <- lapply(hz_list, function(z) one_hz_plot(z, dplyr::filter(plot_data, .data$zone_sante_notification == z)))

  if (length(plots) == 1L) {
    plots[[1L]]
  } else {
    faceted_widget <- plotly::subplot(
      plots,
      nrows = ceiling(length(plots) / 3L),
      shareX = FALSE, shareY = FALSE,
      titleX = TRUE, titleY = TRUE
    )

    # plotly::subplot() does not retain individual subplot titles, so place
    # each HZ label above its own axis domain.
    panel_annotations <- lapply(seq_along(hz_list), function(i) {
      axis_suffix <- if (i == 1L) "" else as.character(i)
      x_domain <- faceted_widget$x$layout[[paste0("xaxis", axis_suffix)]]$domain
      y_domain <- faceted_widget$x$layout[[paste0("yaxis", axis_suffix)]]$domain
      list(
        text = hz_list[[i]],
        xref = "paper",
        yref = "paper",
        x = mean(x_domain),
        y = max(y_domain) + 0.005,
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 11)
      )
    })

    bands <- alert_plotly_layout_bands$faceted
    annot_list <- panel_annotations
    if (isTRUE(show_header)) {
      annot_list <- c(
        annot_list,
        list(
          alert_plotly_annotation(chart_title, bands$title, size = 16),
          alert_plotly_annotation(chart_subtitle, bands$subtitle),
          alert_plotly_annotation(
            paste0("Approche : ", approach),
            bands$subtitle_secondary
          )
        )
      )
    }
    if (isTRUE(show_caption)) {
      annot_list <- c(
        annot_list,
        list(alert_plotly_annotation(caption_text, bands$caption, size = 10))
      )
    }

    calc_margin <- if (!is.null(margin)) {
      margin
    } else if (!isTRUE(show_header) && !isTRUE(show_legend)) {
      if (isTRUE(show_caption)) {
        list(t = 25, r = 15, b = 50, l = 55)
      } else {
        list(t = 25, r = 15, b = 40, l = 55)
      }
    } else {
      alert_plotly_margins$faceted
    }

    faceted_widget |>
      plotly::layout(
        legend = if (isTRUE(show_legend)) bands$legend else list(visible = FALSE),
        showlegend = isTRUE(show_legend),
        annotations = annot_list,
        margin = calc_margin
      )
  }
}

# ---------------------------------------------------------------- #
# Stacked case / death adequacy plot
# ---------------------------------------------------------------- #

#' Prepare adequacy trend data for the stacked adequacy plot
#'
#' Loads (or reuses) `trends_smooth_adeq` data, converts the time key to Date,
#' and validates that `case_adequacy` / `death_adequacy` columns exist.
#'
#' @param data Dataframe or NULL.
#' @param data_path Character or NULL. Path to the RDS.
#' @param hz Character vector. Optional health zone filter.
#' @param start_date Date/character/NULL. Earliest date to keep.
#' @return A tibble with `date` and the original columns.
#' @keywords internal
prepare_adequacy_data <- function(data = NULL, data_path = NULL, hz = NULL,
                                  start_date = as.Date("2026-05-01")) {
  if (is.null(data)) {
    if (is.null(data_path)) {
      data_path <- latest_output_file(here::here("output"), "02_trends_smooth_adeq\\.rds")
      if (is.na(data_path)) {
        rlang::abort(
          "No '02_trends_smooth_adeq.rds' found under output/. ",
          "Run R/02_alert_trends.R first or pass `data` / `data_path`."
        )
      }
    }
    data <- readRDS(data_path)
  }

  .evd_max_date <- attr(data, "evd_max_date")

  required <- c("threshold_time_key", "zone_sante_notification",
                "case_adequacy", "death_adequacy")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(c(
      "Input data is missing required columns:",
      x = paste(missing_cols, collapse = ", ")
    ))
  }

  out <- data |>
    dplyr::mutate(date = as.Date(threshold_time_key)) |>
    dplyr::filter(!is.na(.data$date))

  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    out <- out |> dplyr::filter(.data$date >= .env$start_date)
    if (nrow(out) == 0L) {
      rlang::abort(c(
        "No rows remain after filtering by `start_date`.",
        x = paste("Earliest date requested:", start_date)
      ))
    }
  }

  if (!is.null(hz)) {
    out <- out |> dplyr::filter(zone_sante_notification %in% .env$hz)
    if (nrow(out) == 0L) {
      rlang::abort(c(
        "No rows remain after filtering by `hz`.",
        x = paste("Requested:", paste(hz, collapse = ", ")),
        i = "Check spelling against `unique(data$zone_sante_notification)`."
      ))
    }
  }

  if (!is.null(.evd_max_date)) {
    attr(out, "evd_max_date") <- .evd_max_date
  }

  out
}

#' Plot case and death adequacy as stacked bars with threshold reference
#'
#' Stacked bar chart of `case_adequacy` and `death_adequacy` over time, with
#' a horizontal reference line at y = 1 (the "adequate" threshold). Each bar
#' stacks case adequacy (bottom) and death adequacy (top, edge position) so
#' the total height reveals combined alerting pressure relative to a 2-unit
#' ceiling (1 per metric).
#'
#' @param data Dataframe. Optional `trends_smooth_adeq` data.
#' @param data_path Character. Optional explicit path to the RDS file.
#' @param hz Character vector. Health zone(s) to filter to. When a single HZ
#'   is given, the plot is a single panel; when multiple and `facet = TRUE`,
#'   they are faceted.
#' @param facet Logical. Facet by `zone_sante_notification`? Default `TRUE`.
#'   Automatically suppressed when a single HZ is selected.
#' @param ncol Integer. Number of facet columns. Default `NULL` (auto).
#' @param scales Character. Axis scaling across facets: `"fixed"` (default, shared
#'   y-axis) or `"free_y"` / `"free"`.
#' @param one_per_hz Logical. Return a **named list** of ggplot objects, one
#'   per health zone. Default `FALSE`.
#' @param start_date Date/character. Earliest date to plot. Default
#'   `"2026-05-01"`. Pass `NULL` for full history.
#' @param source_date Date/character. "As-of" date for the caption.
#' @param for_plotly Logical. Internal: when `TRUE`, build a `text` aesthetic
#'   for plotly hover tooltips. Default `FALSE`.
#' @return A ggplot object, or (when `one_per_hz = TRUE`) a named list of
#'   ggplot objects keyed by `zone_sante_notification`.
#' @export
plot_adequacy_stacked <- function(
    data = NULL,
    data_path = NULL,
    hz = NULL,
    facet = TRUE,
    ncol = NULL,
    scales = "fixed",
    one_per_hz = FALSE,
    start_date = as.Date("2026-05-01"),
    source_date = NULL,
    for_plotly = FALSE
) {

  # --- One-plot-per-HZ mode -----------------------------------------------
  if (one_per_hz) {
    prep <- prepare_adequacy_data(data = data, data_path = data_path,
                                  hz = hz, start_date = start_date)
    hz_list <- unique(prep$zone_sante_notification)
    if (length(hz_list) == 0L) {
      rlang::abort("`one_per_hz = TRUE` but no health zones found after filtering.")
    }
    plots <- lapply(hz_list, function(z) {
      plot_adequacy_stacked(
        data = prep, hz = z, facet = FALSE, ncol = NULL, scales = scales,
        one_per_hz = FALSE, start_date = start_date,
        source_date = source_date, for_plotly = for_plotly
      )
    })
    return(stats::setNames(plots, hz_list))
  }

  plot_data <- prepare_adequacy_data(data = data, data_path = data_path,
                                     hz = hz, start_date = start_date)
  caption_date <- resolve_source_date(data = plot_data, source_date = source_date)

  plot_data <- plot_data |>
    dplyr::arrange(zone_sante_notification, date)

  # Pivot to long format: one row per (date, metric) so a single geom_col()
  # with position = "dodge" produces two properly dodged bars per date.
  plot_data_long <- plot_data |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(c("case_adequacy", "death_adequacy", "aai", "adequacy_cmr")),
      names_to = "metric",
      values_to = "adequacy"
    ) |>
    dplyr::mutate(
      metric_label = dplyr::case_when(
        metric == "case_adequacy"  ~ "Performance des alertes vivants",
        metric == "death_adequacy" ~ "Performance des alertes décès",
        metric == "aai"            ~ "Performance globale",
       # metric == "adequacy_cmr"   ~ "Rapport alertes décès / décès attendus CMR"
      ),
      metric_label = factor(
        metric_label,
        levels = c(
          "Performance des alertes vivants",
          "Performance des alertes décès",
          "Performance globale"#, "Rapport alertes décès / décès attendus CMR"
        )
      )
    )

  # Build tooltip text for interactive plotly mode
  if (for_plotly) {
    plot_data_long <- plot_data_long |>
      dplyr::mutate(
        tooltip_text = paste0(
          "Zone de santé : ", zone_sante_notification,
          "\nSemaine de notification (début) : ", format(date, "%d/%m/%Y"),
          "\n", metric_label, " : ", ifelse(is.na(adequacy), "N/A", paste0(round(adequacy * 100, 0), "%"))
        )
      )
  }

  single_hz <- !is.null(hz) && length(unique(hz)) == 1L

  # --- Build the plot ----------------------------------------------------
  col_aes <- aes(x = date, y = adequacy, fill = metric_label)
  if (for_plotly) {
    col_aes <- aes(x = date, y = adequacy, fill = metric_label,
                   text = tooltip_text)
  }
  p <- ggplot(plot_data_long, col_aes) +
    geom_hline(yintercept = 0.75, linetype = "dashed", color = "green", linewidth = 0.5) +
    geom_col(position = position_dodge2(preserve = "single"), width = 3.5)

  # --- Faceting ----------------------------------------------------------
  if (facet && !single_hz) {
    facet_args <- list(~zone_sante_notification)
    if (!is.null(ncol)) facet_args$ncol <- ncol
    p <- p + do.call(facet_wrap, c(facet_args, list(scales = scales)))
  }

  # --- Labels / theme / scales -------------------------------------------
  subtitle_text <- paste0(
    "Performance of live alerts = case_alerts / case_alert_threshold\n",
    "Performance of death alerts = death_alerts / death_alert_threshold\n",
    "Overall performance = (performance of live alerts + performance of death alerts) / 2\n"
  )
  if (single_hz) {
    zone_label <- if (hz[[1L]] == "Ensemble de la zone affectée") {
      "Affected area overall"
    } else {
      paste0("Health zone: ", hz[[1L]])
    }
    subtitle_text <- paste0(subtitle_text, "\n", zone_label)
  }

  valid_adeq <- plot_data_long$adequacy[!is.na(plot_data_long$adequacy)]
  y_limits <- if (length(valid_adeq) > 0L && any(valid_adeq >= 1)) NULL else c(0, 1)

  unique_dates <- sort(unique(plot_data_long$date))
  p <- p +
    scale_x_date(breaks = unique_dates, date_labels = "%d-%m\n%Y") +
    scale_y_continuous(labels = scales::percent, limits = y_limits) +
    scale_fill_manual(
      name = "",
      values = c(
        "Performance des alertes vivants"         = "steelblue",
        "Performance des alertes décès"           = "tomato",
        "Performance globale"                     = "purple"#,
        #"Rapport alertes décès / décès attendus CMR" = "forestgreen"
      )
    ) +
    labs(
      title = "Performance des alertes : ratios cas & décès",
      subtitle = NULL,
      x = "Semaine de notification (date de début)",
      y = "Performance (% du seuil)",
      caption = paste0(
        sprintf("Source : DHIS2 Tracker - %s", format_dmy(caption_date)),
        "\n",
        subtitle_text
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
      axis.line = element_line(colour = "grey50", linewidth = 0.3),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = rel(0.75)),
      strip.text = element_text(face = "bold", size = rel(0.85)),
      legend.position = "top",
      legend.direction = "horizontal",
      plot.title = element_text(face = "bold")
    )

  # Large-facet guard
  if (facet && !single_hz) {
    n_hz <- dplyr::n_distinct(plot_data$zone_sante_notification)
    if (n_hz > 24L) {
      rlang::warn(c(
        "!" = paste0(n_hz, " health zones will be faceted; the plot may be hard to read."),
        i = "Pass `hz = c(...)` to restrict, or call `facet = FALSE`."
      ))
    }
  }

  p
}

#' Extract the performance footnote from a stacked adequacy plot
#'
#' Removes the source line from [plot_adequacy_stacked()]'s caption and returns
#' the remaining formula text for display outside the plot.
#'
#' @param plot A ggplot object returned by [plot_adequacy_stacked()].
#' @return Character scalar. Empty if the plot has no caption.
#' @export
plot_adequacy_footnote <- function(plot) {
  caption <- plot$labels$caption
  if (length(caption) != 1L || is.na(caption) || !nzchar(caption)) {
    return("")
  }

  caption_lines <- strsplit(caption, "\n", fixed = TRUE)[[1L]]
  is_source_line <- grepl("^\\s*Source\\s*:", caption_lines)
  footnote_lines <- caption_lines[!is_source_line & nzchar(caption_lines)]
  paste(footnote_lines, collapse = "\n")
}

#' Interactive (plotly) version of the stacked adequacy plot
#'
#' Wraps [plot_adequacy_stacked()] with [plotly::ggplotly()] for hover tooltips
#' and zooming. Supports `one_per_hz = TRUE` for per-HZ HTML widgets.
#'
#' @param ... Arguments forwarded to [plot_adequacy_stacked()], including
#'   `one_per_hz`.
#' @param tooltip Character. Tooltip columns for plotly. Default `"text"`.
#' @param show_legend Logical. Display legend? Default `TRUE`.
#' @param show_header Logical. Display header/title annotations? Default `TRUE`.
#' @param show_caption Logical. Display source caption? Default `TRUE`.
#' @param margin Optional list of plotly margins.
#' @param display_mode_bar Logical. Display Plotly control mode bar? Default `NULL` (unmodified).
#' @return A plotly htmlwidget object, or (when `one_per_hz = TRUE`) a named
#'   list of plotly htmlwidgets keyed by `zone_sante_notification`.
#' @export
plot_adequacy_stacked_interactive <- function(...,
                                              tooltip = "text",
                                              show_legend = TRUE,
                                              show_header = TRUE,
                                              show_caption = TRUE,
                                              margin = NULL,
                                              display_mode_bar = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    rlang::abort(
      "Package 'plotly' is required for interactive plots. Install with install.packages('plotly')."
    )
  }

  dots <- list(...)
  show_legend <- if (!is.null(dots$show_legend)) isTRUE(dots$show_legend) else isTRUE(show_legend)
  show_header <- if (!is.null(dots$show_header)) isTRUE(dots$show_header) else isTRUE(show_header)
  show_caption <- if (!is.null(dots$show_caption)) isTRUE(dots$show_caption) else isTRUE(show_caption)
  display_mode_bar <- if (!is.null(dots$display_mode_bar)) {
    isTRUE(dots$display_mode_bar)
  } else if (!is.null(dots$displayModeBar)) {
    isTRUE(dots$displayModeBar)
  } else {
    display_mode_bar
  }
  if (!is.null(dots$margin)) margin <- dots$margin

  # --- One-widget-per-HZ mode --------------------------------------------
  if (isTRUE(dots$one_per_hz)) {
    prep_arg_names <- c("data", "data_path", "hz", "start_date", "source_date")
    prep_args <- dots[names(dots) %in% prep_arg_names]
    prep <- do.call(prepare_adequacy_data, prep_args)
    hz_list <- unique(prep$zone_sante_notification)
    if (length(hz_list) == 0L) {
      rlang::abort("`one_per_hz = TRUE` but no health zones found after filtering.")
    }
    widgets <- lapply(hz_list, function(z) {
      sub_args <- dots
      sub_args$hz <- z
      sub_args$one_per_hz <- FALSE
      sub_args$show_legend <- show_legend
      sub_args$show_header <- show_header
      sub_args$show_caption <- show_caption
      sub_args$margin <- margin
      sub_args$display_mode_bar <- display_mode_bar
      do.call(plot_adequacy_stacked_interactive, c(sub_args, list(tooltip = tooltip)))
    })
    return(stats::setNames(widgets, hz_list))
  }

  # --- ggplotly path ------------------------------------------------------
  gp <- tryCatch(
    {
      gg_args <- dots
      gg_args$for_plotly <- TRUE
      p <- suppressWarnings(do.call(plot_adequacy_stacked, gg_args))
      suppressWarnings(plotly::ggplotly(p, tooltip = tooltip))
    },
    error = function(e) NULL
  )
  if (!is.null(gp)) {
    if (!isTRUE(show_legend)) {
      gp <- plotly::layout(gp, showlegend = FALSE)
    } else {
      gp <- plotly::layout(gp, showlegend = TRUE, legend = plotly_top_legend)
    }
    calc_margin <- if (!is.null(margin)) {
      margin
    } else if (!isTRUE(show_header) && !isTRUE(show_legend)) {
      if (isTRUE(show_caption)) {
        list(t = 15, r = 15, b = 50, l = 55)
      } else {
        list(t = 15, r = 15, b = 40, l = 55)
      }
    } else if (!isTRUE(show_header) && isTRUE(show_legend)) {
      if (isTRUE(show_caption)) {
        list(t = 35, r = 15, b = 50, l = 55)
      } else {
        list(t = 35, r = 15, b = 40, l = 55)
      }
    } else {
      NULL
    }
    if (!is.null(calc_margin)) {
      gp$x$layout$margin <- calc_margin
    }
    if (!is.null(display_mode_bar)) {
      gp <- plotly::config(gp, displayModeBar = isTRUE(display_mode_bar))
    }
    return(gp)
  }

  # --- Native plotly fallback ---------------------------------------------
  prep_arg_names <- c("data", "data_path", "hz", "start_date", "source_date")
  prep_args <- dots[names(dots) %in% prep_arg_names]
  plot_data <- do.call(prepare_adequacy_data, prep_args)
  caption_date <- resolve_source_date(data = plot_data,
                                      source_date = dots$source_date)

  has_aai <- "aai" %in% names(plot_data)

  plot_data <- plot_data |>
    dplyr::mutate(
      tooltip_text = paste0(
        "Zone de santé : ", zone_sante_notification,
        "\nSemaine de notification (début) : ", format(date, "%d/%m/%Y"),
        "\nPerformance alertes vivants : ", ifelse(is.na(case_adequacy), "N/A", paste0(round(case_adequacy * 100, 0), "%")),
        "\nPerformance alertes décès : ", ifelse(is.na(death_adequacy), "N/A", paste0(round(death_adequacy * 100, 0), "%")),
        if (has_aai) paste0("\nPerformance globale : ", ifelse(is.na(aai), "N/A", paste0(round(aai * 100, 0), "%"))) else ""
      )
    ) |>
    dplyr::arrange(zone_sante_notification, date)

  caption_text <- sprintf("Source: DHIS2 Tracker - %s", format_dmy(caption_date))
  hz_list <- unique(plot_data$zone_sante_notification)

  one_hz_plot <- function(hz_name, df) {
    p <- plotly::plot_ly(
      data = df, x = ~date, y = ~case_adequacy,
      type = "bar", name = "Performance des alertes vivants",
      marker = list(color = "steelblue", line = list(color = "steelblue", width = 0)),
      text = ~tooltip_text, hoverinfo = "text",
      textposition = "none",
      showlegend = (isTRUE(show_legend) && hz_name == hz_list[1L])
    ) |>
      plotly::add_bars(
        y = ~death_adequacy, name = "Performance des alertes décès",
        marker = list(color = "tomato",
                      line = list(color = "tomato", width = 0)),
        text = ~tooltip_text, hoverinfo = "text",
        textposition = "none",
        showlegend = (isTRUE(show_legend) && hz_name == hz_list[1L])
      )

    if (has_aai) {
      p <- p |>
        plotly::add_bars(
          y = ~aai, name = "Performance globale",
          marker = list(color = "purple",
                        line = list(color = "purple", width = 0)),
          text = ~tooltip_text, hoverinfo = "text",
          textposition = "none",
          showlegend = (isTRUE(show_legend) && hz_name == hz_list[1L])
        )
    }

    annot_list <- list()
    if (isTRUE(show_caption)) {
      annot_list <- list(
        list(
          text = caption_text,
          xref = "paper", yref = "paper", x = 0, y = -0.1,
          showarrow = FALSE, font = list(size = 10)
        )
      )
    }

    calc_margin <- if (!is.null(margin)) {
      margin
    } else if (!isTRUE(show_header) && !isTRUE(show_legend)) {
      if (isTRUE(show_caption)) {
        list(t = 15, r = 15, b = 50, l = 55)
      } else {
        list(t = 15, r = 15, b = 40, l = 55)
      }
    } else if (!isTRUE(show_header) && isTRUE(show_legend)) {
      if (isTRUE(show_caption)) {
        list(t = 35, r = 15, b = 50, l = 55)
      } else {
        list(t = 35, r = 15, b = 40, l = 55)
      }
    } else {
      NULL
    }

    unique_dates <- sort(unique(df$date))
    xaxis_cfg <- list(
      title = "Semaine de notification (date de début)",
      tickmode = "array",
      tickvals = unique_dates,
      ticktext = format(unique_dates, "%d-%m\n%Y"),
      tickangle = 0,
      tickfont = list(size = 10)
    )

    all_adeq_vals <- c(df$case_adequacy, df$death_adequacy)
    if (has_aai) all_adeq_vals <- c(all_adeq_vals, df$aai)
    valid_adeq_df <- all_adeq_vals[!is.na(all_adeq_vals)]
    has_over_100_df <- length(valid_adeq_df) > 0L && any(valid_adeq_df >= 1)

    yaxis_cfg <- list(
      title = "Adéquation (% du seuil)",
      tickformat = ".0%"
    )
    if (!has_over_100_df) {
      yaxis_cfg$range <- c(0, 1)
    }

    p <- p |>
      plotly::layout(
        barmode = "group",
        xaxis = xaxis_cfg,
        yaxis = yaxis_cfg,
        legend = if (isTRUE(show_legend)) plotly_top_legend else list(visible = FALSE),
        showlegend = isTRUE(show_legend),
        annotations = annot_list,
        margin = calc_margin
      )

    if (length(hz_list) > 1L) {
      if (isTRUE(show_header)) {
        p <- plotly::layout(p, title = hz_name)
      }
    } else {
      if (isTRUE(show_header)) {
        annot_header <- list(
          list(
            text = if (hz_name == "Ensemble de la zone affectée") "Ensemble de la zone affectée" else paste0("Zone de santé : ", hz_name),
            xref = "paper", yref = "paper", x = 0.5, y = 1.0,
            xanchor = "center", yanchor = "bottom",
            showarrow = FALSE, font = list(size = 12)
          )
        )
        p <- plotly::layout(
          p,
          title = "Alert Adequacy: Case & Death Ratios",
          annotations = c(annot_header, annot_list)
        )
      }
    }

    p
  }

  plots <- lapply(hz_list, function(z) {
    one_hz_plot(z, dplyr::filter(plot_data, zone_sante_notification == z))
  })

  res_widget <- if (length(plots) == 1L) {
    plots[[1L]]
  } else {
    calc_margin <- if (!is.null(margin)) {
      margin
    } else if (!isTRUE(show_header) && !isTRUE(show_legend)) {
      if (isTRUE(show_caption)) {
        list(t = 25, r = 15, b = 50, l = 55)
      } else {
        list(t = 25, r = 15, b = 40, l = 55)
      }
    } else {
      NULL
    }

    annot_list <- list()
    if (isTRUE(show_caption)) {
      annot_list <- list(
        list(
          text = caption_text,
          xref = "paper", yref = "paper", x = 0, y = -0.1,
          showarrow = FALSE, font = list(size = 10)
        )
      )
    }

    plotly::subplot(
      plots,
      nrows = ceiling(length(plots) / 3L),
      shareX = FALSE, shareY = FALSE,
      titleX = TRUE, titleY = TRUE
    ) |>
      plotly::layout(
        title = if (isTRUE(show_header)) "Alert Adequacy: Case & Death Ratios" else "",
        legend = if (isTRUE(show_legend)) plotly_top_legend else list(visible = FALSE),
        showlegend = isTRUE(show_legend),
        annotations = annot_list,
        margin = calc_margin
      )
  }

  if (!is.null(display_mode_bar)) {
    res_widget <- plotly::config(res_widget, displayModeBar = isTRUE(display_mode_bar))
  }
  res_widget
}

# ---------------------------------------------------------------- #
# Save helper
# ---------------------------------------------------------------- #

#' Save an alert trend plot (or a list of plots) to disk
#'
#' Saves ggplot objects (via [ggplot2::ggsave()]) or plotly htmlwidgets
#' (via [htmlwidgets::saveWidget()]) into the output directory. Creates the
#' directory if it is missing.
#'
#' When `plot` is a named **list** (as returned by [plot_alert_trends()] or
#' [plot_alert_trends_interactive()] with `one_per_hz = TRUE`), one file is
#' written per element. In that case `filename` must contain the `{hz}`
#' placeholder, which is replaced by each health zone's (sanitized) name -
#' e.g. `"alert_trends_{hz}.pdf"` writes `alert_trends_Bunia.pdf`,
#' `alert_trends_Beni.pdf`, etc.
#'
#' @param plot A ggplot or plotly object, or a named list of such objects.
#' @param filename Character. Output file name (extension picks method). For
#'   list input, must include a `{hz}` placeholder.
#' @param output_dir Character. Destination directory. Default is
#'   `here::here("output")`, created if needed.
#' @param width,height Numeric. Dimensions for static plots (inches). Ignored
#'   for plotly.
#' @return Invisible path(s) to the saved file(s) - a character vector.
#' @export
save_alert_trend_plot <- function(plot, filename,
                                   output_dir = here::here("output"),
                                   width = 14, height = 10) {
  # --- List input: one file per element ---------------------------------
  if (is.list(plot) && !inherits(plot, "ggplot") &&
      !inherits(plot, "htmlwidget") && !inherits(plot, "plotly")) {

    # --- Combined PDF: all ggplots in a single multi-page document ------
    is_pdf <- grepl("\\.pdf$", filename, ignore.case = TRUE)
    all_ggplot <- all(vapply(plot, inherits, logical(1L), "ggplot"))

    if (is_pdf && all_ggplot) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      # If the filename contains {hz}, replace it with "all"; otherwise use as-is
      if (grepl("{hz}", filename, fixed = TRUE)) {
        filename <- sub("{hz}", "all", filename, fixed = TRUE)
      }
      out_path <- file.path(output_dir, filename)
      grDevices::pdf(out_path, width = width, height = height)
      for (p in plot) print(p)
      grDevices::dev.off()
      return(invisible(out_path))
    }

    # --- Individual files (existing behaviour) --------------------------
    if (!grepl("{hz}", filename, fixed = TRUE)) {
      rlang::abort(c(
        "`filename` must contain the '{hz}' placeholder when saving a list of plots.",
        x = "Example: filename = \"alert_trends_{hz}.pdf\""
      ))
    }
    hz_names <- names(plot)
    if (is.null(hz_names)) {
      # Fallback to positional labels if names are missing
      hz_names <- seq_along(plot)
      rlang::warn("Plot list has no names; using numeric indices in filenames.")
    }
    paths <- vapply(seq_along(plot), function(i) {
      safe_hz <- sanitize_filename(hz_names[[i]])
      this_file <- sub("{hz}", safe_hz, filename, fixed = TRUE)
      save_alert_trend_plot(
        plot[[i]], this_file,
        output_dir = output_dir, width = width, height = height
      )
    }, character(1L))
    return(invisible(paths))
  }

  # --- Single plot -----------------------------------------------------
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  out_path <- file.path(output_dir, filename)

  if (inherits(plot, "htmlwidget") || inherits(plot, "plotly")) {
    if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
      rlang::abort("Package 'htmlwidgets' is required to save plotly objects.")
    }
    # plotly htmlwidgets need a self-contained file
    tmp <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(plot, tmp, selfcontained = TRUE)
    file.rename(tmp, out_path)
  } else {
    ggplot2::ggsave(out_path, plot, width = width, height = height)
  }

  invisible(out_path)
}

#' Sanitize a health zone name for use as a filename component
#'
#' Replaces characters that are problematic in filenames (spaces, slashes) with
#' underscores. Keeps alphanumerics, hyphens, dots, and underscores.
#'
#' @param x Character scalar.
#' @return Character scalar, filename-safe.
#' @keywords internal
sanitize_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9._-]", "_", x)
  x <- gsub("_+", "_", x)  # collapse runs of underscores
  x
}
