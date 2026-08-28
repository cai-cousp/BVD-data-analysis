# Script 4: EpiNow2 nowcast of confirmed cases by onset date
# Purpose: correct the right-truncation (reporting lag) in the last few days
#          of the daily confirmed-case series by symptom-onset date.
# Inputs: latest cleaned EVD line list
# Outputs: 04_nowcast_summary.rds, 04_nowcast_samples.rds,
#          04_nowcast_recent.csv, 04_nowcast_delay_diagnostics.csv,
#          04_nowcast_plot.pdf / .png

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  library(EpiNow2)
})

# Load project-local helpers (also sources alert_helpers/nowcast_epinow2.R)
source(here::here("R/alert_helpers.R"))

# 1. Paths and parameters ----------------------------------------------------
data_folder <- normalizePath(
  file.path(here::here(), "..", "..", "DataCleaning", "data", "Output"),
  mustWork = TRUE
)
output_dir <- create_output_dir(here::here("output"))

ref_date <- NULL                        # "now" for the truncation correction;
                                        # NULL derives it from the latest
                                        # notification date in the line list
min_date <- as.Date("2026-05-01")      # same start as the exploratory hz_full
onset_col <- "alert_date_debut_symptoms"
report_col <- "lab_date_analyse"
max_delay <- 21L                       # truncation horizon (p95 onset->lab = 16)
delay_window_days <- 60L               # recent stable window for delay fitting
tail_days <- 5L                        # flag and report the last 5 days
seed <- 42L

# 2. Load data ---------------------------------------------------------------
message("Loading data...")
evd <- load_latest_evd(data_folder)

# Reference ("now") date: the most recent notification date in the line list
# that is not later than the system date.
ref_date <- evd_notification_ref_date(evd)

# 3. Daily confirmed series by onset date ------------------------------------
message("Building the daily onset-date series...")
hz_full <- build_hz_full(
  evd,
  min_date = min_date,
  ref_date = ref_date,
  onset_col = onset_col
)

message(
  sprintf(
    "Series: %s -> %s (%d days, %d confirmed/positive cases).",
    min(hz_full$date),
    max(hz_full$date),
    nrow(hz_full),
    sum(hz_full$I)
  )
)

# 4. Onset -> lab confirmation delay -----------------------------------------
message("Characterising the onset -> lab confirmation delay...")
delays <- build_reporting_delays(
  evd,
  onset_col = onset_col,
  report_col = report_col,
  ref_date = ref_date,
  max_delay = max_delay,
  min_onset = ref_date - delay_window_days
)

delay_summary <- summarise_reporting_delays(delays$delay)
delay_cdf <- reporting_delay_cdf(delays$delay, max_delay = max_delay)

print(delay_summary)
print(delay_cdf |> dplyr::filter(.data$delay <= 10L))

# 5. Fit the nowcast ---------------------------------------------------------
# Stan compilation happens on the first run and is cached for later runs.
message("Fitting EpiNow2 nowcast (Stan compilation + sampling)...")
reported_cases <- hz_full |>
  dplyr::transmute(date = .data$date, confirm = as.integer(.data$I))

fit <- fit_nowcast_epinow2(
  reported_cases = reported_cases,
  delay_values = delays$delay,
  max_delay = max_delay,
  seed = seed,
  stan = EpiNow2::stan_opts(
    samples = 2000L,
    warmup = 500L,
    chains = 4L,
    cores = 4L,
    seed = seed
  ),
  verbose = FALSE
)

# 6. Tidy outputs ------------------------------------------------------------
message("Tidying and saving outputs...")
nowcast_summary <- tidy_nowcast(fit, reported_cases, tail_days = tail_days)
nowcast_samples <- extract_nowcast_samples(fit)

# MCMC diagnostics (rstan backend used by EpiNow2's default stan_opts())
mcmc_diag <- NULL
sampler_diag <- NULL
if (inherits(fit$fit, "stanfit")) {
  sm <- tryCatch(
    rstan::summary(fit$fit, pars = "infections")$summary,
    error = function(e) NULL
  )
  sp <- tryCatch(
    rstan::get_sampler_params(fit$fit, inc_warmup = FALSE),
    error = function(e) NULL
  )
  if (!is.null(sm)) {
    mcmc_diag <- tibble::tibble(
      n_chains = fit$fit@sim$chains,
      n_draws = length(unique(nowcast_samples$.draw)),
      max_rhat_infections = max(sm[, "Rhat"], na.rm = TRUE),
      min_n_eff_infections = min(sm[, "n_eff"], na.rm = TRUE),
      median_n_eff_infections = stats::median(sm[, "n_eff"], na.rm = TRUE)
    )
  }
  if (!is.null(sp)) {
    sampler_diag <- purrr::map_dfr(
      seq_along(sp),
      function(i) {
        tibble::tibble(
          chain = i,
          n_divergent = sum(sp[[i]][, "divergent__"]),
          max_treedepth = max(sp[[i]][, "treedepth__"]),
          min_ebfmi = min(sp[[i]][, "energy__"])
        )
      }
    )
  }
}
if (!is.null(mcmc_diag)) print(mcmc_diag)
if (!is.null(sampler_diag)) print(sampler_diag)
saveRDS(
  list(sampler = sampler_diag, convergence = mcmc_diag),
  file.path(output_dir, "04_nowcast_diagnostics.rds")
)

attr(nowcast_summary, "ref_date") <- ref_date
attr(nowcast_summary, "min_date") <- min_date
attr(nowcast_summary, "tail_days") <- tail_days
attr(nowcast_summary, "max_delay") <- max_delay
attr(nowcast_summary, "onset_col") <- onset_col
attr(nowcast_summary, "report_col") <- report_col
attr(nowcast_summary, "delay_summary") <- delay_summary
attr(nowcast_summary, "mcmc_diagnostics") <- mcmc_diag
attr(nowcast_summary, "generated_at") <- Sys.time()

saveRDS(nowcast_summary, file.path(output_dir, "04_nowcast_summary.rds"))
saveRDS(nowcast_samples, file.path(output_dir, "04_nowcast_samples.rds"))

nowcast_recent <- nowcast_summary |>
  dplyr::filter(.data$date >= max(.data$date) - 13L)
readr::write_csv(
  nowcast_recent,
  file.path(output_dir, "04_nowcast_recent.csv")
)
readr::write_csv(
  delay_cdf,
  file.path(output_dir, "04_nowcast_delay_diagnostics.csv")
)

message("Nowcast tail (observed vs estimated):")
print(nowcast_recent |> dplyr::filter(.data$tail))

tail_summary <- nowcast_summary |>
  dplyr::filter(.data$tail) |>
  dplyr::summarise(
    observed = sum(.data$observed),
    nowcast_median = sum(.data$nowcast_median),
    nowcast_lower_90 = sum(.data$nowcast_lower_90),
    nowcast_upper_90 = sum(.data$nowcast_upper_90)
  )
message("Cumulative tail (last 5 days):")
print(tail_summary)
readr::write_csv(
  tail_summary,
  file.path(output_dir, "04_nowcast_tail_summary.csv")
)

# 7. Plot --------------------------------------------------------------------
message("Plotting observed vs nowcast (last 14 days)...")
plot_dates <- seq.Date(max(nowcast_summary$date) - 13L,
                       max(nowcast_summary$date),
                       by = "day")
plot_data <- nowcast_summary |>
  dplyr::filter(.data$date %in% plot_dates)
tail_start <- max(plot_data$date) - tail_days + 1L

p <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = .data$date)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = .data$nowcast_lower_90,
      ymax = .data$nowcast_upper_90
    ),
    fill = "#4C78A8",
    alpha = 0.15
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = .data$nowcast_lower_50,
      ymax = .data$nowcast_upper_50
    ),
    fill = "#4C78A8",
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = .data$nowcast_median),
    colour = "#4C78A8",
    linewidth = 1
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = .data$observed),
    size = 2.2,
    colour = "#1F1F1F"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = .data$observed),
    colour = "#1F1F1F",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  ggplot2::geom_vline(
    xintercept = tail_start,
    linetype = "dotted",
    colour = "#B2182B"
  ) +
  ggplot2::labs(
    title = "EpiNow2 nowcast of confirmed EVD cases by onset date",
    subtitle = sprintf(
      "Onset -> lab delay: median %.1f days (p90 %.1f); truncation-adjusted from %s",
      delay_summary$median,
      delay_summary$p90,
      format(tail_start, "%d %b")
    ),
    x = "Symptom onset date",
    y = "Confirmed cases",
    caption = sprintf(
      "Blue: posterior median with 50%%/90%% credible intervals. Dashed red line: nowcast tail start. Reference date: %s.",
      format(ref_date, "%d %b %Y")
    )
  ) +
  ggplot2::scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(hjust = 0)
  )

ggplot2::ggsave(
  file.path(output_dir, "04_nowcast_plot.pdf"),
  p,
  width = 10,
  height = 5.5
)
ggplot2::ggsave(
  file.path(output_dir, "04_nowcast_plot.png"),
  p,
  width = 10,
  height = 5.5,
  dpi = 200
)

message("Done. Outputs written to ", output_dir)
