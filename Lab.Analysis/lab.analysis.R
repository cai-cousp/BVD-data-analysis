# ============================================================================
# Lab analysis and performance indicators - EVD17, RDC
# ============================================================================
# Analyses des performances de laboratoire : indicateurs de flux d'échantillons,
# taux de positivité, délais de turnaround, et analyse spatiale des priorités.
# Section 3c (intégrée depuis scripts/sample_positivity.R) : tables de
# positivité par premier prélèvement (national / province / zone de santé) et
# tendances hebdomadaires par semaine épidémiologique de début des signes
# (défaut ; autres bases de date configurables via date_col / week_date_col).
# ============================================================================
# Report mode
# ============================================================================
# The Quarto report (OutPut/Lab/rapport_echantillons_positivite.qmd) sources
# this script at render time with:
#     options(lab.analysis.report_mode = TRUE)
# In report mode, file exports (ggsave / xlsx / rds / write.xlsx) and
# interactive views are skipped, and packages are not installed; every object
# used by the report is still created in the global environment.
# ============================================================================

skip_output <- isTRUE(getOption("lab.analysis.report_mode", FALSE))

## Loading libraries


My.Packages <- installed.packages()[, "Package"]

if (!"pacman" %in% My.Packages && !skip_output) {
  install.packages("pacman")
}

pacman::p_load(
  "openxlsx", "readxl", "incidence",
  "stringdist", "lubridate",
  "RColorBrewer", "wesanderson",
  "dplyr", "tidyverse", "stringi",
  "ggforce", "ggfittext", "ggthemes",
  "ggrepel", "tidytext",
  "zoo", "forecast",
  "DescTools", "scales",
  install = !skip_output
)


## Loading custom functions

source(here::here("helpers", "paths.R"))
source(here::here("helpers", "DebutSem.R"))
source(here::here("helpers", "lab_weekly.R"))
source(here::here("helpers", "LoadLatestData.R"))
source(here::here("helpers", "lab_indicators.R"))
source(here::here("helpers", "lab_long.R"))
source(here::here("helpers", "lab_indicator_prop.R"))      # compute_lab_indicator_prop() - sample-level indicators
source(here::here("helpers", "lab_indicator_prop_lab.R"))  # compute_lab_indicator_prop_lab() - lab-level sample indicators
source(here::here("helpers", "lab_alert_coverage.R"))      # compute_alert_lab_coverage() - individual-level indicators
source(here::here("helpers", "clean_filename.R"))
source(here::here("helpers", "lab_positivity.R"))  # compute_positivity_rate()
source(here::here("helpers", "lab_plots.R"))       # theme_lab(), plot_sample_flow(), plot_result_breakdown(),
                                    # plot_turnaround_boxplot(), smooth_rate_ci(),
                                    # plot_positivity_trend(), plot_turnaround_trend()


## Importing EVD cleaned data

evd.data <- load_latest_data(
  data_cleaning_output,
  folder_name = "evd.cleaning",
  format      = "rds"
)


## Importing EVD lab cleaned data

evd.lab.data <- load_latest_data(
  data_cleaning_output,
  folder_name = "lab.cleaning",
  format      = "rds"
)
""
# Restrict the analysis to health zones with at least one confirmed case
# (classification_finale == "Cas confirmé"). The zone set is derived from the
# case line list and applied to both datasets so downstream province /
# health-zone aggregations, trends and priority rankings are consistent.

zs_confirmed <- evd.data |>
  filter(classification_finale == "Cas confirmé") |>
  pull(zone_sante_notification) |>
  unique()

cat("Restricting to", length(zs_confirmed),
    "health zones with >= 1 confirmed case\n")

evd.data <- evd.data |>
  filter(zone_sante_notification %in% zs_confirmed)

evd.lab.data <- evd.lab.data |>
  filter(zone_sante_notification %in% zs_confirmed)

cat("After restriction:", nrow(evd.data), "cases |",
    nrow(evd.lab.data), "lab samples\n")

# Reference date for output directory naming

(max.evd.date <- max(
  as.Date(evd.lab.data$lab_date_analyse[as.Date(evd.lab.data$lab_date_analyse) <= Sys.Date()]),
  na.rm = TRUE
))

cat("Loaded", nrow(evd.data), "cases | Reference date:", format(max.evd.date), "\n")
cat("Loaded", nrow(evd.lab.data), "lab samples |",
    length(unique(evd.lab.data$num_epid)), "cases with lab data\n")

# Bind case-level variables onto the raw lab table (by num_epid): symptom
# onset date (positivity windows and weekly estimates), alert notification
# date (testing_rate_21), epidemiologic link, profession, all s2_* symptom
# columns and the s4_* transmission / contact fields. Column names are kept
# unchanged.

evd.lab.data <- join_case_variables(
  evd.lab.data, evd.data,
  alert_date_debut_symptoms,
  date_heure_notification_alerte,
  alert_conlusion,
  alert_lien_epidemiologic,
  alert_profession,
  s2_fievre:s2_sang_urines_hematurie,  # 34 columns incl. s2_mesure / s2_si_oui_temp_c_thermoflash
  s4_1_il_y_a_t_il_eu_contacts_avec_malade_ebola_connu_suspect_simplement_avec_personne_malade,
  s4_ma1_types_contact,
  s4_ma1_lien_parente,
  s4_ma1_personne_etait_vivante_decedee,
  s4_2_patient_a_participe_a_funerailles_avant_maladie_actuelle,
  s4_pf1_avez_vous_porte_touche_corps
)


evd.lab.data <- evd.lab.data |>
  mutate(date_heure_notification_alerte = as.Date(date_heure_notification_alerte)) |>
  filter(lab_date_analyse >= as.Date("2026-03-01")| is.na(lab_date_analyse),
         alert_date_debut_symptoms >= as.Date("2026-03-01") | is.na(alert_date_debut_symptoms),
         date_heure_notification_alerte <= today() | is.na(date_heure_notification_alerte))

## Output directories

day.dir   <- paste0(here::here("OutPut/Lab/"), format(max.evd.date, "%d%b"), "/")
day.dir.a <- paste0(day.dir, "all/")
day.dir.p <- paste0(day.dir, "prov/")
day.dir.z <- paste0(day.dir, "zs/")

for (d in c(day.dir, day.dir.a, day.dir.p, day.dir.z)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}


# ============================================================================
# Section 1: Prepare long-format lab data and per-case lab variables
# ============================================================================
#
# Primary source: normalized long-format lab.cleaning table (one row per
# collected sample). Onset dates are joined from the EVD line list by
# num_epid (decision D2). The wide-format path is preserved in the helpers
# (decision D1) but is no longer used by this pipeline.

names(evd.lab.data)

cat("Preparing long-format lab data...\n")

lab.long <- prepare_lab_long(
  lab_data  = evd.lab.data,
  case_data = evd.data,
  onset_col = "alert_date_debut_symptoms"
)

cat("Total samples in long format:", nrow(lab.long), "\n")
cat("Unique cases in long format:", length(unique(lab.long$num_epid)), "\n")
table(lab.long$result_class, useNA = "ifany")

cat("Computing per-case lab variables...\n")

evd.lab <- compute_lab_variables(lab.long)


# Summary of per-case variables
cat("Cases with samples:", sum(evd.lab$n_samples > 0, na.rm = TRUE), "\n")
cat("Cases with positive result:", sum(!is.na(evd.lab$date_first_positive)), "\n")
cat("Cases with negative-after-positive:", sum(!is.na(evd.lab$date_first_negative)), "\n")
cat("Non-NA counts per variable:\n")
cat("  n_samples:",                sum(!is.na(evd.lab$n_samples)), "\n")
cat("  date_first_positive:",      sum(!is.na(evd.lab$date_first_positive)), "\n")
cat("  delay_onset_first_pos:",    sum(!is.na(evd.lab$delay_onset_first_pos)), "\n")
cat("  date_first_negative:",      sum(!is.na(evd.lab$date_first_negative)), "\n")
cat("  delay_onset_first_neg:",    sum(!is.na(evd.lab$delay_onset_first_neg)), "\n")
cat("  delay_pos_to_neg:",         sum(!is.na(evd.lab$delay_pos_to_neg)), "\n")
cat("  type_first_positive:",      sum(!is.na(evd.lab$type_first_positive)), "\n")
cat("  type_first_negative:",      sum(!is.na(evd.lab$type_first_negative)), "\n")
cat("  sample_numb_first_positive:", sum(!is.na(evd.lab$sample_numb_first_positive)), "\n")
cat("  n_neg_before_positive:",    sum(!is.na(evd.lab$n_neg_before_positive)), "\n")
cat("  n_neg_after_positive:",     sum(!is.na(evd.lab$n_neg_after_positive)), "\n")
cat("  date_last_neg_after_pos:",  sum(!is.na(evd.lab$date_last_negative_after_positive)), "\n")
cat("  date_first_result:",        sum(!is.na(evd.lab$date_first_result)), "\n")
cat("  delay_onset_first_result:", sum(!is.na(evd.lab$delay_onset_first_result)), "\n")
cat("  date_result_first_positive:", sum(!is.na(evd.lab$date_result_first_positive)), "\n")
cat("  delay_onset_to_result_first_pos:",
    sum(!is.na(evd.lab$delay_onset_to_result_first_pos)), "\n")
cat("  delay_collection_to_result_first_pos:",
    sum(!is.na(evd.lab$delay_collection_to_result_first_pos)), "\n")

# ============================================================================
# Section 2: Aggregate indicators
# ============================================================================

cat("Aggregating lab indicators...\n")

# SAMPLE-level indicators at all three aggregation levels, computed by
# compute_lab_indicator_prop() (helpers/lab_indicator_prop.R). The counts
# table carries explicit time-window / sample-type suffixes:
#   *_all - all samples: n_collected_all (rows with any lab evidence),
#           n_arrived_all (reception OR analysis OR recorded result,
#           decision V5), n_processed_all (analysed OR recorded result);
#           headline positivity_rate_all = n_positive_all / n_processed_all
#           — no first-sample restriction, no date window, exact 95% CI.
#   *_1st - one SELECTED sample per case (see first_samples() in
#           helpers/lab_positivity.R): earliest positive if any positive
#           exists, else earliest negative, else invalid, else any other
#           recorded result; cases with no recorded result are excluded.
#   *_21  - 21-day symptom-onset cohort (positivity_rate_21).
#   *_1st_21 - 21-day symptom-onset cohort restricted to the selected first
#              sample per case (positivity_rate_1st_21).
# Individual-level indicators (validated alerts, testing rates) are computed
# separately by compute_alert_lab_coverage() below.
ind_all  <- compute_lab_indicator_prop(lab.long, level = "all")
ind_prov <- compute_lab_indicator_prop(lab.long, level = "prov")
ind_zs   <- compute_lab_indicator_prop(lab.long, level = "zs")

# --- 21-day first-sample positivity (national / province) -------------------
# First-sample positivity within the 21-day symptom-onset cohort, computed
# inside sample_counts() (helpers/lab_indicator_prop.R): same window,
# first-sample basis, exact 95% CI. The report uses these objects for the
# "21 derniers jours" rows of its positivity tables.
pos_21_first_all <- ind_all$counts |>
  select(n_processed_1st_21, n_positive_1st_21,
         positivity_rate_1st_21, ci_lower_1st_21, ci_upper_1st_21)

pos_21_first_prov <- ind_prov$counts |>
  select(province_notification, n_processed_1st_21, n_positive_1st_21,
         positivity_rate_1st_21, ci_lower_1st_21, ci_upper_1st_21)

# Lab-level sample indicators (grouped by laboratory name, lab_destination).
# Samples without a recorded destination laboratory are excluded.
ind_lab <- compute_lab_indicator_prop_lab(
  lab.long |> filter(!is.na(lab_destination))
)

if (interactive()) View(lab.long)
names(lab.long)
if (interactive()) View(ind_prov$turnaround)

names(ind_all$turnaround)
# Print summaries
cat("\n--- National indicators ---\n")
if (interactive()) View(ind_all$counts)
if (interactive()) View(ind_prov$counts)
print(ind_all$turnaround)

cat("\n--- Laboratory indicators ---\n")
if (interactive()) View(ind_lab$counts)
if (interactive()) View(ind_lab$turnaround)

# Proportion indicators (within-threshold turnaround delays) are part of the
# sample-level indicator set: percentage of cases whose delay falls within
# threshold (onset->collection <= 3 days; collection->reception <= 1 day;
# reception->analysis <= 1 day; collection->result <= 2 days), each with a
# 95% confidence interval. One observation per case (earliest collected
# sample); NA and negative delays excluded from numerator and denominator.
cat("\nComputing proportion indicators...\n")

names(lab.long)

prop_all  <- ind_all$prop
prop_prov <- ind_prov$prop
prop_zs   <- ind_zs$prop

if (interactive()) View(prop_prov)

# Lab-level proportions (grouped by laboratory name, lab_destination).
prop_lab <- ind_lab$prop

prop_lab |> names()

cat("\n--- Individual-level indicators (alert-to-lab coverage + testing) ---\n")

# Individual-level indicators at national / province / health-zone level,
# computed from the long-format lab table only (helpers/lab_alert_coverage.R):
# validated individuals -> sampled (any lab evidence) -> tested, each with a
# 95% CI. Validated = "Validée" conclusion OR any lab evidence (collection,
# reception, analysis or recorded result). No onset filter is applied (the
# lab table is used as given). "Sampled" = collection_date OR reception_date
# OR analysis_date OR recorded result; "tested" = reception_date OR
# analysis_date OR recorded result (result_class != "Manquant"). Testing
# rates cover all validated individuals (n_tested_all / testing_rate_all)
# and the 21-day notification cohort (n_validated_21 / n_tested_21 /
# testing_rate_21).
cov_all  <- compute_alert_lab_coverage(lab.long, level = "all")
cov_prov <- compute_alert_lab_coverage(lab.long, level = "prov")
cov_zs   <- compute_alert_lab_coverage(lab.long, level = "zs")

if (interactive()) cov_all |> View()


## Indicators summary tables

# ----------------------------------------------------------------------------
# build_cov_prop()
# Combine individual-level indicators (n_validated, pct_sampled + 95% CI)
# with the sample-level lab-flow volume into a single summary table.
#
# NOTE on denominators: n_sampled / pct_sampled in the coverage table (cov_*)
# are individual-level: validated individuals with any lab evidence. The
# sample-level n_collected_all in the sample counts table (ind_*$counts)
# counts all collected samples in the lab long format - a different
# population - and is included for reference (total lab volume) only.
#
# Rows are driven by the coverage table (individual geography); groups
# with no lab counts get NA for the lab-flow columns. `n_validated` is taken
# directly from the per-group values in `cov` (computed by
# compute_alert_lab_coverage()).
# ----------------------------------------------------------------------------
build_cov_prop <- function(counts, cov, grp) {
  cov_sel <- cov |>
    select(dplyr::all_of(grp),
           n_validated, pct_sampled, lower_sampled, upper_sampled)

  counts_sel <- counts |>
    select(dplyr::all_of(grp), n_collected_all)

  if (length(grp) == 0L) {
    dplyr::cross_join(cov_sel, counts_sel)
  } else {
    dplyr::left_join(cov_sel, counts_sel, by = grp)
  }
}

cov_prop_all <- build_cov_prop(
  counts = ind_all$counts,
  cov    = cov_all,
  grp    = character(0)
)

cov_prop_prov <- build_cov_prop(
  counts = ind_prov$counts,
  cov    = cov_prov,
  grp    = "province_notification"
)

cov_prop_zs <- build_cov_prop(
  counts = ind_zs$counts,
  cov    = cov_zs,
  grp    = c("province_notification", "zone_sante_notification")
)

cat("\n--- National combined coverage / lab-flow summary ---\n")
print(cov_prop_all)

cat("\n--- Province combined coverage / lab-flow summary ---\n")
print(cov_prop_prov)

cat("\n--- Health-zone combined coverage / lab-flow summary ---\n")
print(cov_prop_zs)

cat("\n--- National alert-to-lab coverage ---\n")
print(cov_all)

cat("\n--- National proportion indicators ---\n")
print(prop_all)

if (interactive()) View(prop_prov)


# ============================================================================
# Coverage barplot by province (pct_sampled & testing rate)
# ============================================================================
# Dodged bar chart of the two individual-level coverage indicators with their
# 95% CIs as error bars: % of validated alerts with a sample in the lab
# system (pct_sampled) and % tested (testing_rate_all), by province of
# notification. Follows the project colour convention (theme_lab() + navy/
# blue palette used in helpers/lab_plots.R).

cov_prov_long <- cov_prov |>
  mutate(
    testing_pct      = testing_rate_all * 100,
    testing_ci_lower = testing_ci_lower_all * 100,
    testing_ci_upper = testing_ci_upper_all * 100
  ) |>
  tidyr::pivot_longer(
    cols = c(pct_sampled, testing_pct),
    names_to = "indicateur",
    values_to = "pct"
  ) |>
  mutate(
    lower = ifelse(indicateur == "pct_sampled",
                   lower_sampled, testing_ci_lower),
    upper = ifelse(indicateur == "pct_sampled",
                   upper_sampled, testing_ci_upper),
    indicateur = factor(
      indicateur,
      levels = c("pct_sampled", "testing_pct"),
      labels = c("Alertes validees echantillonnees",
                 "Alertes validees testees")
    ),
    province_notification = as.character(province_notification)
  ) |>
  filter(!is.na(pct))

cov_prov_g <- ggplot(
  cov_prov_long,
  aes(x = pct, y = province_notification, fill = indicateur)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2, linewidth = 0.4
  ) +
  scale_fill_manual(values = c(
    "Alertes validees echantillonnees" = "#03084a",
    "Alertes validees testees"          = "#234a7d"
  )) +
  scale_x_continuous(expand = c(0, 0),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Couverture alerte-vers-laboratoire - Par province",
    subtitle = paste0("Donnees a la date du ",
                      format(max.evd.date, "%d-%m-%Y"),
                      " | barres d'erreur : IC 95%"),
    x = "Pourcentage (%)", y = NULL,
    fill = NULL,
    caption = paste0("IOA - CAI ", "\u00A9", isoyear(Sys.Date()))
  ) +
  theme_lab()

cov_prov_g

# Export the plot
if (!skip_output) {
  cov_prov_g |>
    ggsave(filename = paste0(day.dir.p, "coverage_labflow_prov_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 12, width = 20, scale = 0.40)
}


# ============================================================================
# Testing rate by province: overall vs last 21 days
# ============================================================================
# Dodged bar chart comparing the overall testing rate (testing_rate_all) with
# the testing rate over the last 21 days (testing_rate_21), by province of
# notification, with 95% CIs as error bars. Follows the same structure and
# colour convention as cov_prov_g (theme_lab() + navy/blue palette).

cov_prov_names <- cov_prov |>
  select(province_notification, testing_rate_all, testing_rate_21) |>
  arrange(testing_rate_21) |> pull(province_notification)

cov_prov_21d_long <- cov_prov |>
  mutate(
    testing_pct_all = testing_rate_all * 100,
    testing_pct_21  = testing_rate_21 * 100,
    ci_lower_all    = testing_ci_lower_all * 100,
    ci_upper_all    = testing_ci_upper_all * 100,
    ci_lower_21     = testing_ci_lower_21 * 100,
    ci_upper_21     = testing_ci_upper_21 * 100
  ) |>
  tidyr::pivot_longer(
    cols = c(testing_pct_all, testing_pct_21),
    names_to = "periode",
    values_to = "pct"
  ) |>
  mutate(
    lower = ifelse(periode == "testing_pct_all",
                   ci_lower_all, ci_lower_21),
    upper = ifelse(periode == "testing_pct_all",
                   ci_upper_all, ci_upper_21),
    n_tested = ifelse(periode == "testing_pct_all",
                      n_tested_all, n_tested_21),
    periode = factor(
      periode,
      levels = c("testing_pct_all", "testing_pct_21"),
      labels = c("Taux de testing - Toute la periode",
                 "Taux de testing - 21 derniers jours")
    ),
    province_notification = factor(
      province_notification,
      levels = cov_prov_names
    )
  ) |>
  filter(!is.na(pct))


cov_prov_21d_g <- ggplot(
  cov_prov_21d_long,
  aes(x = pct, y = province_notification, fill = periode)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.7),
    width = 0.2, linewidth = 0.4
  ) +
  geom_text(
    aes(x = upper + 0.5,
        label = paste0("(",n_tested, " tests)")),
    position = position_dodge(width = 0.7),
    hjust = 0, size = 2.6, colour = "grey20"
  ) +
  scale_fill_manual(values = c(
    "Taux de testing - Toute la periode"   = "#6480a6" ,
    "Taux de testing - 21 derniers jours"  = "#01042e"
  )) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Taux de testing - par province (toute la periode vs 21 derniers jours)",
    subtitle = paste0("Donnees a la date du ",
                      format(max.evd.date, "%d-%m-%Y"),
                      " | barres d'erreur : IC 95%"),
    x = "Pourcentage (%)", y = NULL,
    fill = NULL,
    caption = paste0("IOA - CAI ", "\u00A9", isoyear(Sys.Date()))
  ) +
  theme_lab()

cov_prov_21d_g

# Export the plot
if (!skip_output) {
  cov_prov_21d_g |>
    ggsave(filename = paste0(day.dir.p, "testing_rate_prov_21d_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 12, width = 20, scale = 0.40)
}


# positivity rates
# Positivity is computed on the FIRST sample per case (first_only = TRUE):
# repeat samples do not contribute to test positivity.

names(evd.lab.data)

pos_first_all <- compute_positivity_rate(evd.lab.data, first_only = TRUE)

pos_first_prov <- compute_positivity_rate(
  evd.lab.data,
  group_cols = "province_notification",
  first_only = TRUE
)

pos_first_zs <- compute_positivity_rate(
  evd.lab.data,
  group_cols = "zone_sante_notification",
  first_only = TRUE
)

if (interactive()) pos_first_all |> View()
if (interactive()) pos_first_prov |> View()
if (interactive()) pos_first_zs |> View()



# ============================================================================
# Section 2.1: Save summary tables
# ============================================================================

if (!skip_output) {
  wb <- createWorkbook()

  addWorksheet(wb, "national_counts");   writeData(wb, 1, ind_all$counts)
  addWorksheet(wb, "national_turnaround"); writeData(wb, 2, ind_all$turnaround)
  addWorksheet(wb, "prov_counts");       writeData(wb, 3, ind_prov$counts)
  addWorksheet(wb, "prov_turnaround");   writeData(wb, 4, ind_prov$turnaround)
  addWorksheet(wb, "zs_counts");         writeData(wb, 5, ind_zs$counts)
  addWorksheet(wb, "zs_turnaround");     writeData(wb, 6, ind_zs$turnaround)
  addWorksheet(wb, "national_prop");     writeData(wb, 7, prop_all)
  addWorksheet(wb, "prov_prop");         writeData(wb, 8, prop_prov)
  addWorksheet(wb, "zs_prop");           writeData(wb, 9, prop_zs)
  addWorksheet(wb, "lab_counts");        writeData(wb, 10, ind_lab$counts)
  addWorksheet(wb, "lab_turnaround");    writeData(wb, 11, ind_lab$turnaround)
  addWorksheet(wb, "lab_prop");          writeData(wb, 12, prop_lab)
  addWorksheet(wb, "alert_lab_coverage_all");  writeData(wb, 13, cov_all)
  addWorksheet(wb, "alert_lab_coverage_prov"); writeData(wb, 14, cov_prov)
  addWorksheet(wb, "alert_lab_coverage_zs");   writeData(wb, 15, cov_zs)

  saveWorkbook(wb,
    file = paste0(day.dir, "lab_indicators_summary_",
                  format(max.evd.date, "%d%b"), ".xlsx"),
    overwrite = TRUE)
}

# Save enriched case data
if (!skip_output) {
  saveRDS(evd.lab,
    file = paste0(day.dir, "evd_lab_variables_",
                  format(max.evd.date, "%d%b"), ".rds"))

  saveRDS(lab.long,
    file = paste0(day.dir, "lab_long_format_",
                  format(max.evd.date, "%d%b"), ".rds"))
}


# ============================================================================
# Section 2.1.1: Visualisations - Stacked bar charts
# ============================================================================

# Plot functions (theme_lab(), plot_sample_flow(), plot_result_breakdown(),
# plot_turnaround_boxplot(), plot_positivity_trend(), plot_turnaround_trend())
# are defined in helpers/lab_plots.R.

# Generate sample flow plots
if (!skip_output) {
  plot_sample_flow(ind_all$counts, "National", max.evd.date) |>
    ggsave(filename = paste0(day.dir.a, "sample_flow_all_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 12, width = 20, scale = 0.40)

  plot_sample_flow(ind_prov$counts, "Par province", max.evd.date) |>
    ggsave(filename = paste0(day.dir.p, "sample_flow_prov_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  plot_sample_flow(ind_zs$counts |> filter(n_processed_all >= 3),
                    "Par zone de sante", max.evd.date) |>
    ggsave(filename = paste0(day.dir.z, "sample_flow_zs_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 30, width = 25, scale = 0.35)

  # Generate result breakdown plots
  plot_result_breakdown(ind_all$counts, "National", max.evd.date) |>
    ggsave(filename = paste0(day.dir.a, "result_breakdown_all_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 12, width = 20, scale = 0.40)

  plot_result_breakdown(ind_prov$counts, "Par province", max.evd.date) |>
    ggsave(filename = paste0(day.dir.p, "result_breakdown_prov_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  plot_result_breakdown(ind_zs$counts |> filter(n_processed_all >= 3),
                         "Par zone de sante", max.evd.date) |>
    ggsave(filename = paste0(day.dir.z, "result_breakdown_zs_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 30, width = 25, scale = 0.35)
}


# ============================================================================
# Section 2.1.1 (3): Boxplots - Turnaround times
# ============================================================================

# --- Turnaround boxplots (plot_turnaround_boxplot() in helpers/lab_plots.R) ---

# Define the four turnaround steps to plot
turnaround_steps <- list(
  list(col = "delay_onset_to_collection",
       label = "Delai debut des signes - collecte"),
  list(col = "delay_collection_to_reception",
       label = "Delai collecte - reception au labo"),
  list(col = "delay_reception_to_analysis",
       label = "Delai reception - analyse"),
  list(col = "delay_collection_to_result",
       label = "Delai collecte - resultat"),
  list(col = "delay_onset_to_result",
       label = "Delai debut des signes - resultat")
)

# Generate boxplots by province 
if (!skip_output) {
  for (step in turnaround_steps) {
    safe_name <- gsub("[^a-zA-Z0-9]", "_", step$label)

    plot_turnaround_boxplot(lab.long, step$col, step$label, "prov", max.evd.date) |>
      ggsave(filename = paste0(day.dir.p, "turnaround_", safe_name, "_prov_",
                                format(max.evd.date, "%d%b"), ".png"),
             dpi = 300, height = 20, width = 25, scale = 0.35)
  }

  # Generate boxplots by health zone (only ZS with >= 5 samples)
  zs_with_min <- lab.long |>
    count(province_notification, zone_sante_notification) |>
    filter(n >= 5) |>
    pull(zone_sante_notification)

  for (step in turnaround_steps) {
    safe_name <- gsub("[^a-zA-Z0-9]", "_", step$label)

    plot_turnaround_boxplot(
      lab.long |> filter(zone_sante_notification %in% zs_with_min),
      step$col, step$label, "zs", max.evd.date
    ) |>
      ggsave(filename = paste0(day.dir.z, "turnaround_", safe_name, "_zs_",
                                format(max.evd.date, "%d%b"), ".png"),
             dpi = 300, height = 30, width = 25, scale = 0.35)
  }
}


# ============================================================================
# Section 3: Trend analysis
# ============================================================================

# --- Helper functions (smooth_rate_ci(), plot_positivity_trend()) are
# defined in helpers/lab_plots.R ---


# --- 3a. Time series of weekly sample counts and positivity rate ---
# Weekly estimates default to the symptom onset week (onset_date); pass
# date_col = "collection_date" / "reception_date" / "analysis_date"/ "onset_date" to
# change the week basis.

# Weekly positivity is always aggregated on one priority-selected sample per
# case (the same first_samples() definition used by compute_lab_indicator_prop()'s
# positivity_rate_1st); repeat samples do not contribute to test positivity.
# Weekly n_collected counts one lab-evidence sample per case in each week.
weekly_lab <- weekly_aggregate(lab.long, date_col = "onset_date")

View(weekly_lab)
# --- 3b. Weekly median turnaround times ---
# Note: weekly_turnaround_aggregate() stays anchored to the collection week
# (its delays are relative to collection / onset dates); the weekly positivity
# estimates above use the onset week.

weekly_turnaround <- weekly_turnaround_aggregate(lab.long)


# --- Trend plots ---

# Loess smoothing of the positivity rate and its 95% CI, plus the dual-axis
# scale factor, are computed inside plot_positivity_trend() via smooth_rate_ci()

# Positivity rate trend (smoothed) with sample volume bars on primary axis
positivity_trend_g <- plot_positivity_trend(
  weekly_lab,
  "National",
  max.evd.date,
  span        = 1,
  trend_color = "#9a0000",
  week_basis  = "onset"
)

if (!skip_output) {
  positivity_trend_g |>
    ggsave(filename = paste0(day.dir.a, "positivity_trend_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 32, scale = 0.30)
}


# ============================================================================
# Section 3 (cont.): Positivity trend by province and health zone
# ============================================================================

cat("Building province-level positivity trend plots...\n")

# Inclusion thresholds (adjust as needed). weekly_aggregate() counts one
# collected sample per case per week for volume, while positivity remains on
# the case-level result-priority sample.
min_weeks_plot   <- 3  # minimum weeks of data required to plot a unit
min_weeks_smooth <- 5  # minimum points required for loess smoothing
min_zs_samples   <- 10 # minimum per-week case collections for a health zone

# --- Province level ---

weekly_lab_prov <- weekly_aggregate(lab.long, "province_notification",
                                    date_col = "onset_date") |>
  mutate(province_notification = as.character(province_notification))

prov_eligible <- weekly_lab_prov |>
  summarise(n_weeks = n(), .by = province_notification) |>
  filter(n_weeks >= min_weeks_plot) |>
  pull(province_notification)

weekly_lab_prov <- weekly_lab_prov |>
  filter(province_notification %in% prov_eligible)

if (interactive()) weekly_lab_prov |> View()

positivity_trend_prov_list <- split(weekly_lab_prov, weekly_lab_prov$province_notification) |>
  map(~ plot_positivity_trend(
    .x,
    group_label      = unique(.x$province_notification)[1],
    date_ref         = max.evd.date,
    span             = 1,
    min_weeks_smooth = min_weeks_smooth,
    week_basis       = "onset"
  ))

cat("Province trend plots:", length(positivity_trend_prov_list), "\n")

# Export province plots
if (!skip_output) {
  for (prov in names(positivity_trend_prov_list)) {
    positivity_trend_prov_list[[prov]] |>
      ggsave(
        filename = paste0(day.dir.p, "positivity_trend_",
                          clean_filename(prov), "_prov_",
                          format(max.evd.date, "%d%b"), ".png"),
        dpi = 300, height = 20, width = 32, scale = 0.30
      )
  }
}

# --- Health zone level ---

cat("Building health-zone positivity trend plots...\n")

weekly_lab_zs <- weekly_aggregate(
  lab.long, c("province_notification", "zone_sante_notification"),
  date_col = "onset_date"
) |>
  mutate(across(c(province_notification, zone_sante_notification), as.character))

zs_eligible <- weekly_lab_zs |>
  summarise(
    n_weeks = n(),
    n_total = sum(n_collected),
    .by = c(province_notification, zone_sante_notification)
  ) |>
  filter(n_weeks >= min_weeks_plot, n_total >= min_zs_samples)

weekly_lab_zs <- weekly_lab_zs |>
  semi_join(zs_eligible, by = c("province_notification", "zone_sante_notification")) |>
  mutate(unit_key = paste0(province_notification, " | ", zone_sante_notification))

positivity_trend_zs_list <- split(weekly_lab_zs, weekly_lab_zs$unit_key) |>
  map(~ plot_positivity_trend(
    .x,
    group_label      = paste0(unique(.x$zone_sante_notification)[1], " (",
                              unique(.x$province_notification)[1], ")"),
    date_ref         = max.evd.date,
    span             = 1,
    min_weeks_smooth = min_weeks_smooth,
    week_basis       = "onset"
  ))

cat("Health-zone trend plots:", length(positivity_trend_zs_list), "\n")

# Export health-zone plots (filename includes province to avoid collisions)
if (!skip_output) {
  for (nm in names(positivity_trend_zs_list)) {
    positivity_trend_zs_list[[nm]] |>
      ggsave(
        filename = paste0(day.dir.z, "positivity_trend_",
                          clean_filename(nm), "_zs_",
                          format(max.evd.date, "%d%b"), ".png"),
        dpi = 300, height = 20, width = 32, scale = 0.30
      )
  }
}

# Save plot lists for reuse (e.g. in the Quarto report)
if (!skip_output) {
  saveRDS(positivity_trend_prov_list,
          file = paste0(day.dir, "positivity_trend_prov_list_",
                        format(max.evd.date, "%d%b"), ".rds"))
  saveRDS(positivity_trend_zs_list,
          file = paste0(day.dir, "positivity_trend_zs_list_",
                        format(max.evd.date, "%d%b"), ".rds"))
}


# ============================================================================
# Section 3c: Weekly sample positivity by onset date (first sample per case)
# ============================================================================
# Integrated from scripts/sample_positivity.R: computes the 6 positivity
# tables (national / province / health zone x overall / weekly) with
# compute_positivity_overview(). All positivity is computed on one
# result-priority sample per case: repeat samples do not contribute to test
# positivity. n_collected counts one evidence sample per case per geography
# and week. The week basis
# defaults to the symptom onset date (week_date_col = "alert_date_debut_symptoms",
# joined from evd.data above); pass week_date_col = "s5_date_prelev" /
# "lab_date_reception" / "lab_date_analyse" for collection / reception /
# analysis weeks. Tables are exported as .rds + one xlsx workbook under
# day.dir/positivity/; the weekly trend plots are saved alongside the
# collection-week trend plots in day.dir/{all,prov,zs}/.

cat("\nComputing sample positivity overview (onset-date weeks, first samples)...\n")

# first_only = TRUE (explicit): positivity uses one result-priority sample per
# case; repeat samples do not contribute to test positivity.
pos_overview_first <- compute_positivity_overview(
  evd.lab.data,
  week_date_col = "alert_date_debut_symptoms",
  first_only    = TRUE
)

cat("Positivity tables (result-priority samples):",
    length(pos_overview_first), "\n")

# --- Export the 6 tables (individual rds + combined list + xlsx) ------------

pos_out_dir <- paste0(day.dir, "positivity/")
dir.create(pos_out_dir, recursive = TRUE, showWarnings = FALSE)

if (!skip_output) {
  for (nm in names(pos_overview_first)) {
    saveRDS(pos_overview_first[[nm]], file.path(pos_out_dir, paste0(nm, ".rds")))
  }

  saveRDS(pos_overview_first,
          file = file.path(pos_out_dir, paste0("positivity_overview_",
                                               format(max.evd.date, "%d%b"), ".rds")))

  wb_pos <- createWorkbook()
  for (nm in names(pos_overview_first)) {
    addWorksheet(wb_pos, nm)
    writeData(wb_pos, nm, pos_overview_first[[nm]])
  }
  saveWorkbook(wb_pos,
               file = file.path(pos_out_dir, paste0("sample_positivity_",
                                                    format(max.evd.date, "%d%b"), ".xlsx")),
               overwrite = TRUE)
}

# --- National trend plot (first samples only) -------------------------------

positivity_trend_onset_all <-
plot_positivity_trend(
  pos_overview_first$weekly_pos_all,
  "National", max.evd.date,
  span = 1,
  week_col = "week_start", week_basis = "onset"
) 

if (!skip_output) {
  positivity_trend_onset_all |>
    ggsave(filename = paste0(day.dir.a, "positivity_trend_onset_all_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 32, scale = 0.30)
}

# --- Province level (first samples only) ------------------------------------

# First-sample tables have smaller counts; use the lower ZS inclusion
# threshold (min_zs_samples, defined above, already first-sample based).

if (interactive()) pos_overview_first$weekly_pos_prov |> View()

prov_tbl <- pos_overview_first$weekly_pos_prov |>
  mutate(province_notification = as.character(province_notification))

prov_eligible <- prov_tbl |>
  summarise(n_weeks = n(), .by = province_notification) |>
  filter(n_weeks >= min_weeks_plot) |>
  pull(province_notification)

prov_tbl <- prov_tbl |>
  filter(province_notification %in% prov_eligible)

pos_trend_first_prov_list <- split(prov_tbl, prov_tbl$province_notification) |>
  map(~ plot_positivity_trend(
    .x,
    group_label      = unique(.x$province_notification)[1],
    date_ref         = max.evd.date,
    span             = 1,
    min_weeks_smooth = min_weeks_smooth,
    week_col         = "week_start",
    week_basis       = "onset"
  ))

if (!skip_output) {
  for (prov in names(pos_trend_first_prov_list)) {
    pos_trend_first_prov_list[[prov]] |>
      ggsave(
        filename = paste0(day.dir.p, "positivity_trend_onset_",
                          clean_filename(prov), "_prov_",
                          format(max.evd.date, "%d%b"), ".png"),
        dpi = 300, height = 20, width = 32, scale = 0.30
      )
  }
}

cat("Province onset-week trend plots:",
    length(pos_trend_first_prov_list), "\n")

# --- Health zone level ------------------------------------------------------

zs_tbl <- pos_overview_first$weekly_pos_zs |>
  mutate(across(c(province_notification, zone_sante_notification), as.character))

zs_eligible <- zs_tbl |>
  summarise(
    n_weeks     = n(),
    n_collected = sum(n_collected),
    .by = c(province_notification, zone_sante_notification)
  ) |>
  filter(n_weeks >= min_weeks_plot, n_collected >= min_zs_samples)

zs_tbl <- zs_tbl |>
  semi_join(zs_eligible, by = c("province_notification", "zone_sante_notification")) |>
  mutate(unit_key = paste0(province_notification, " | ", zone_sante_notification))

pos_trend_first_zs_list <- split(zs_tbl, zs_tbl$unit_key) |>
  map(~ plot_positivity_trend(
    .x,
    group_label      = paste0(unique(.x$zone_sante_notification)[1], " (",
                              unique(.x$province_notification)[1], ")"),
    date_ref         = max.evd.date,
    span             = 1,
    min_weeks_smooth = min_weeks_smooth,
    week_col         = "week_start",
    week_basis       = "onset"
  ))

if (!skip_output) {
  for (nm in names(pos_trend_first_zs_list)) {
    pos_trend_first_zs_list[[nm]] |>
      ggsave(
        filename = paste0(day.dir.z, "positivity_trend_onset_",
                          clean_filename(nm), "_zs_",
                          format(max.evd.date, "%d%b"), ".png"),
        dpi = 300, height = 20, width = 32, scale = 0.30
      )
  }
}

cat("Health-zone onset-week trend plots:",
    length(pos_trend_first_zs_list), "\n")

# --- Quick summary ----------------------------------------------------------

cat("\n--- National positivity (first sample per case) ---\n")
print(pos_overview_first$pos_all)

cat("\n--- Weekly national positivity (first samples) ---\n")
print(pos_overview_first$weekly_pos_all)


## Workload by laboratory
## Destination of samples by health zone of collection

cat("Building lab destination plot by health zone of collection...\n")

# One row per zone x destination combination (decision D4).
lab_dest <- lab.long |>
  filter(!is.na(lab_destination), !is.na(zone_sante_notification)) |>
  mutate(lab_destination = as.character(lab_destination)) |>
  count(province_notification, zone_sante_notification,
        lab_destination, name = "nSample")

# Keep the top zones and group the tail so the parallel-set plot stays readable.
zone_totals <- lab_dest |>
  summarise(n_zone = sum(nSample), .by = zone_sante_notification) |>
  arrange(desc(n_zone))

top_n_zones <- 20L
top_zones <- zone_totals$zone_sante_notification[
  seq_len(min(top_n_zones, nrow(zone_totals)))
]

lab_dest_plot <- lab_dest |>
  mutate(
    zone_plot = ifelse(
      zone_sante_notification %in% top_zones,
      zone_sante_notification,
      "Autres zones"
    )
  ) |>
  summarise(nSample = sum(nSample), .by = c(zone_plot, lab_destination)) |>
  arrange(desc(nSample))

# Top five destination laboratories for the caption.
lab_totals <- lab_dest_plot |>
  summarise(n_lab = sum(nSample), .by = lab_destination) |>
  arrange(desc(n_lab)) |>
  mutate(
    pct       = round(n_lab / sum(n_lab) * 100, 1),
    lab_label = paste0(lab_destination, " (", pct, "%)")
  )
top_labs <- lab_totals |>
  slice_head(n = 5)

lab_dest_sets <- lab_dest_plot |>
  ggforce::gather_set_data(1:2) |>
  mutate(x = dplyr::recode(
    x,
    zone_plot       = "Zone de santé de collecte",
    lab_destination = "Laboratoire de destination"
  ))

lab_dest_g <- lab_dest_sets |>
  ggplot(aes(x, id = id, split = y, value = nSample)) +
  ggforce::geom_parallel_sets(
    aes(fill = lab_destination),
    n = 500, alpha = 0.5,
    axis.width = 0.1, strength = 0.45, sep = 0.05,
    position = "identity", show.legend = FALSE
  ) +
  ggforce::geom_parallel_sets_axes(
    aes(fill = y),
    position = "identity", axis.width = 0.1, show.legend = FALSE
  ) +
  ggforce::geom_parallel_sets_labels(
    colour = "grey10", size = 3, fontface = "bold", angle = 0
  ) +
  scale_x_discrete(
    limits = c("Zone de santé de collecte",
               "Laboratoire de destination"),
    expand = c(0.112, 0.112)
  ) +
  labs(
    title = paste0(
      "Destination des échantillons par zone de santé de collecte, MVE/B, RDC"
    ),
    subtitle = paste0("Données à la date du ",
                      format(max.evd.date, "%d-%m-%Y")),
    caption = paste0(
      paste(top_labs$lab_label, collapse = " , "),
      "\nreprésentent les principales destinations d'échantillons\n",
      "IOA - CAI ©", isoyear(Sys.Date())
    )
  ) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, color = "#0f5c77", face = "bold"),
    plot.caption = element_text(face = "italic")
  )

if (!skip_output) {
  lab_dest_g |>
    ggsave(
      filename = paste0(day.dir, "lab_destination_by_zs_",
                        format(max.evd.date, "%d%b"), ".png"),
      dpi = 300, height = 25, width = 40, scale = 0.35
    )

  saveRDS(lab_dest_plot,
    file = paste0(day.dir, "lab_destination_by_zs_",
                  format(max.evd.date, "%d%b"), ".rds"))
}


## Lab level sample management 

##

# Median turnaround time trends
turnaround_trend_g <- plot_turnaround_trend(weekly_turnaround, max.evd.date)

if (!skip_output) {
  turnaround_trend_g |>
    ggsave(filename = paste0(day.dir.a, "turnaround_trend_",
                              format(max.evd.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 30, scale = 0.30)
}



# ============================================================================
# Section 4: Spatial analysis - priority areas for lab capacity
# ============================================================================

cat("Computing spatial priority analysis...\n")

# Load accessibility data
access_file <- here::here("data/20250701 RDC_ZS_Accessibilite.xlsx")

if (file.exists(access_file)) {
  accessibility <- readxl::read_xlsx(access_file)

  # Join lab indicators by health zone
  priority_data <- ind_zs$counts |>
    left_join(ind_zs$turnaround,
              by = c("province_notification", "zone_sante_notification")) |>
    left_join(accessibility, by = join_by(zone_sante_notification))

  # Compute composite priority score
  # Higher volume + higher positivity + lower accessibility = higher priority
  priority_data <- priority_data |>
    mutate(
      # Normalise indicators to 0-1 scale for scoring
      vol_score     = n_collected_all / max(n_collected_all, na.rm = TRUE),
      pos_score     = ifelse(is.na(positivity_rate_all), 0, positivity_rate_all),
      # Accessibility: if available, inverse (harder to access = higher score)
      # Use a placeholder if column names differ
      access_score  = 0.5  # default; adjust after inspecting accessibility columns
    ) |>
    mutate(
      priority_score = 0.40 * vol_score + 0.35 * pos_score + 0.25 * access_score
    ) |>
    arrange(desc(priority_score))

  if (interactive()) priority_data |> View()
  # Save priority ranking
  if (!skip_output) {
    write.xlsx(priority_data,
      file = paste0(day.dir, "lab_priority_areas_",
                    format(max.evd.date, "%d%b"), ".xlsx"),
      overwrite = TRUE)
  }

  cat("Priority ranking saved.\n")
  cat("Top 10 priority health zones:\n")
  print(head(priority_data |> select(province_notification, zone_sante_notification,
                                      n_collected_all, positivity_rate_all, priority_score), 10))
} else {
  cat("Accessibility file not found. Generating tabular priority ranking only.\n")

  priority_data <- ind_zs$counts |>
    left_join(ind_zs$turnaround,
              by = c("province_notification", "zone_sante_notification")) |>
    mutate(
      vol_score = n_collected_all / max(n_collected_all, na.rm = TRUE),
      pos_score = ifelse(is.na(positivity_rate_all), 0, positivity_rate_all),
      priority_score = 0.55 * vol_score + 0.45 * pos_score
    ) |>
    arrange(desc(priority_score))

  if (!skip_output) {
    write.xlsx(priority_data,
      file = paste0(day.dir, "lab_priority_areas_",
                    format(max.evd.date, "%d%b"), ".xlsx"),
      overwrite = TRUE)
  }
}


cat("\n=== Lab analysis complete ===\n")
cat("Output directory:", day.dir, "\n")
