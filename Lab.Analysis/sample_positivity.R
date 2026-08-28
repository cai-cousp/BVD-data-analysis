# ============================================================================
# Sample positivity overview - EVD17, RDC (thin wrapper)
# ============================================================================
# Computes the 6 sample-positivity tables (national / province / health zone
# x overall / weekly by symptom onset date, the default week basis) via
# compute_positivity_overview() from helpers/lab_positivity.R and writes them
# as individual .rds + one combined list + one Excel workbook under
# OutPut/Lab/<refdate>/positivity/. Positivity uses one result-priority sample
# per case; n_collected counts one evidence sample per case/window.
#
# The same computation (plus weekly trend plots) now lives in
# scripts/lab.analysis.R (Section 3c); this wrapper keeps the standalone CLI
# usage working with zero duplicated logic.
#
# Usage:
#   Rscript Lab.Analysis/sample_positivity.R
# ============================================================================

library(dplyr)
library(openxlsx)

# --- Source project helpers -------------------------------------------------

source(here::here("helpers", "paths.R"))
source(here::here("helpers", "DebutSem.R"))        # get_monday()
source(here::here("helpers", "LoadLatestData.R"))  # load_latest_data()
source(here::here("helpers", "lab_long.R"))        # classify_result(), join_case_variables()
source(here::here("helpers", "lab_weekly.R"))      # clopper_pearson_ci()
source(here::here("helpers", "lab_positivity.R"))  # compute_positivity_rate(), compute_positivity_overview()

# --- Load the latest lab cleaning export ------------------------------------

lab <- load_latest_data(
  data_cleaning_output,
  folder_name = "lab.cleaning",
  format      = "rds"
)

# Bind the symptom onset date from the case line list (default week basis).
evd.data <- load_latest_data(
  data_cleaning_output,
  folder_name = "evd.cleaning",
  format      = "rds"
)

# Restrict the analysis to health zones with at least one confirmed case
# (classification_finale == "Cas confirmé"). The zone set is derived from the
# case line list and applied to both datasets so health-zone tables are
# consistent with scripts/lab.analysis.R.

zs_confirmed <- evd.data |>
  filter(classification_finale == "Cas confirmé") |>
  pull(zone_sante_notification) |>
  unique()

cat("Restricting to", length(zs_confirmed),
    "health zones with >= 1 confirmed case\n")

evd.data <- evd.data |>
  filter(zone_sante_notification %in% zs_confirmed)

lab <- lab |>
  filter(zone_sante_notification %in% zs_confirmed)

cat("After restriction:", nrow(lab), "samples\n")

lab <- join_case_variables(lab, evd.data, alert_date_debut_symptoms)

cat("Loaded", nrow(lab), "samples from",
    length(unique(lab$num_epid)), "cases\n")

# Reference date for the output directory (last analysis date in the data)
ref_date <- max(as.Date(lab$lab_date_analyse), na.rm = TRUE)
if (!is.finite(ref_date)) ref_date <- Sys.Date()

# --- Compute the 12 positivity tables ---------------------------------------

positivity_tables <- compute_positivity_overview(lab)

stopifnot(
  "All objects must be data frames" =
    all(vapply(positivity_tables, is.data.frame, logical(1)))
)

# --- Save outputs -----------------------------------------------------------

out_dir <- file.path(
  project_root, "OutPut", "Lab", format(ref_date, "%d%b"), "positivity"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (nm in names(positivity_tables)) {
  saveRDS(positivity_tables[[nm]], file.path(out_dir, paste0(nm, ".rds")))
}

saveRDS(
  positivity_tables,
  file.path(out_dir, paste0("positivity_overview_", format(ref_date, "%d%b"), ".rds"))
)

wb <- createWorkbook()
for (nm in names(positivity_tables)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, positivity_tables[[nm]])
}
saveWorkbook(
  wb,
  file = file.path(out_dir, paste0("sample_positivity_", format(ref_date, "%d%b"), ".xlsx")),
  overwrite = TRUE
)

cat("\nOutputs written to:", out_dir, "\n")
cat("Individual .rds files:", length(positivity_tables), "\n")
cat("Excel workbook: sample_positivity_", format(ref_date, "%d%b"), ".xlsx\n", sep = "")

# --- Print a quick summary --------------------------------------------------

cat("\n--- National positivity (first sample per case) ---\n")
print(positivity_tables$pos_all)

cat("\n--- Table sizes ---\n")
print(
  data.frame(
    table = names(positivity_tables),
    rows  = vapply(positivity_tables, nrow, integer(1))
  )
)
