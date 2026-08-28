
# ──── contact_ll_connect.R ────
# Purpose: Fuzzy-match contact line-list records to EVD case line-list records,
#          then identify infector-infectee pairs from both contact-based
#          linkages and the EVD line-list's "suspected infector" field.
# Author: [REDACTED]
# ───────────────────────────────

# ── 0. Setup ──────────────────────────────────────────────────────────────────

# Load required packages using pacman (installs if missing)
pacman::p_load(stringdist, stringr, tidyr, readr, lubridate, fs, cli, dplyr)
source(here::here("helpers", "paths.R"))

# Source helper functions from the helpers/ directory
source(here::here("helpers", "AgeStd.R"))          # Age standardisation utilities
source(here::here("helpers", "OrthoFix.R"))        # Orthographic (spelling) correction helpers
source(here::here("helpers", "OrthoCorrect.R"))    # Additional orthographic correction logic
source(here::here("helpers", "LoadLatestData.R"))  # load_latest_data() — picks most recent RDS
source(here::here("helpers", "pct_distance.R"))    # Percentage-distance function for similarity
source(here::here("helpers", "num_distance.R"))    # Numeric-distance function (abs diff)

# ── 1. Data Loading ───────────────────────────────────────────────────────────

# Directory where cleaning-output sub-folders live
output_dir <- data_cleaning_output

cli_alert_info("Loading contact dataset from latest cleaning output")
# Load the most recent contact-tracing RDS snapshot
contact <- load_latest_data(
  output_dir,
  folder_name = "contact.cleaning",
  format = "rds"
)

cli_alert_info("Loading EVD dataset from latest cleaning output")
# Load the most recent EVD case line-list RDS snapshot
evd <- load_latest_data(
  output_dir,
  folder_name = "evd.cleaning",
  format = "rds"
)

# ── 2. Column Selection & Subsetting ──────────────────────────────────────────

cli_progress_step("Preparing both datasets for matching")

# Columns to retain from the EVD line-list for matching
evd.cols <- c(
  "num_epid",                              # Unique epidemic number (case ID)
  "nom_post_nom_prenom_cas",               # Full name of the case
  "age_ans",                               # Age in years
  "sexe",                                  # Sex
  "province_notification",                 # Province of notification
  "zone_sante_notification",               # Health zone of notification
  "s1_lieu_residence_permanente_zone_sante", # Permanent residence health zone
  "aire_sante_notification",               # Health area of notification
  "alert_village_quartier",                # Village / neighbourhood from alert
  "alert_adresse_complete",                # Full address from alert
  "date_heure_notification_alerte",        # Date/time of alert notification
  "alert_date_debut_symptoms",             # Symptom onset date from alert
  "s1_malade_etait_il_contact_connu",      # Was patient a known contact?
  "s1_malade_etait_il_contact_suivi",      # Was patient a followed-up contact?
  "lab_resultat_final",                    # Final lab result
  "classification_finale_cas"              # Final case classification
)

# Quick inspection of contact dataset structure and unique epid numbers
names(contact)
unique(contact$numero_epid_alerte)

# Peek at contact-to-epid linkage
contact |>
  select(numero_epid_alerte, nom_post_nom_prenom_cas)

# Columns to retain from the contact-tracing dataset
contact.cols <- c(
  "id_contact",                            # Contact record ID
  "nom_prenom_contact",                    # Name of the contact
  "age_ans",                               # Age in years
  "sexe",                                  # Sex
  "province_notification",                 # Province
  "zone_sante_notification",               # Health zone
  "aire_sante_notification",               # Health area (notification)
  "village_quartier",                      # Village / neighbourhood
  "aire_sante",                            # Health area (alternative field)
  "statut_vaccinal",                       # Vaccination status
  "date_dernier_contact_cas_source",       # Date of last contact with source case
  "circonstances_cas",                     # Circumstances of exposure
  "type_contact",                          # Contact type (e.g. familial, community)
  "relation_contact",                      # Relationship to source case
  "date_suivi",                            # Follow-up date
  "date_debut_suivi",                      # Follow-up start date
  "symptomes",                             # Symptoms at follow-up
  "symptomes_2",                           # Additional symptoms
  "resultat_suivi",                        # Follow-up outcome
  "date_fin_suivi",                        # Follow-up end date
  "numero_epid_alerte"                     # Linked epidemic number from alert
)

# Subset EVD to matching columns only
evd.short <-
  select(evd, any_of(evd.cols))

# Subset contact data to matching columns, also pulling the case name as a
# reference for matching
contact.short <-
  select(contact, any_of(contact.cols), noms_cas_source = nom_post_nom_prenom_cas)

# ── 3. Fuzzy Matching: Contact → Case ─────────────────────────────────────────

cli_progress_step("Performing fuzzy match efficiently per province")

# Unique provinces in the EVD dataset → used as loops keys
evd.p <- unique(evd.short$province_notification)

# Lists to accumulate results across provinces
matches.ls      <- list()  # Matches within the same health zone
matches.ls.less <- list()  # High-name-similarity matches across health zones

# Loop over each province to keep comparisons manageable
for (p in evd.p) {

  # Subset EVD and contact records for this province
  evd.p.df      <- evd.short[evd.short$province_notification == p, ]
  contact.p.df  <- contact.short[contact.short$province_notification == p, ]

  # Fuzzy-match each contact name against all EVD names in the province
  # auto_correct_typos returns the best-matching case name (or NA)
  contact.case.matches.p0 <-
    contact.p.df |>
    mutate(matched_case =
             auto_correct_typos(nom_prenom_contact, evd.p.df$nom_post_nom_prenom_cas,
                                match_threshold = 77.5))

  # Enrich matched records with EVD metadata
  contact.case.matches.p1 <- contact.case.matches.p0 |>
    left_join(evd.short,
              by = c("matched_case" = "nom_post_nom_prenom_cas")) |>
    # Ensure ages are integer (avoids type mismatches later)
    mutate(age_ans.y = as.integer(age_ans.y)) |>
    # Cap biologically implausible ages (> 100 → NA)
    mutate_at(vars(age_ans.x, age_ans.y), ~ ifelse(. > 100, NA, .)) |>
    # Keep only records with a valid health zone AND matching sex
    filter(!is.na(zone_sante_notification.y), sexe.x == sexe.y) |>
    # Compute similarity scores for name, address, and health area
    mutate(
      name.sim    = match_similarity(nom_prenom_contact, matched_case),
      quartier.sim = match_similarity(village_quartier, alert_village_quartier),
      adress.sim  = match_similarity(village_quartier, alert_adresse_complete),
      as.sim      = match_similarity(aire_sante_notification.x, aire_sante_notification.y),
      as.sim2     = match_similarity(aire_sante, aire_sante_notification.y),
      age.dist    = num_distance(age_ans.x, age_ans.y),
      # Composite scores: take the best available sub-score
      address.similarity   = pmax(quartier.sim, adress.sim, na.rm = TRUE),
      as.similarity        = pmax(as.sim, as.sim2, na.rm = TRUE),
      location.similarity  = pmax(address.similarity, as.similarity, na.rm = TRUE)
    )

  # ── Tier 1: Within-zone matches ─────────────────────────────────────────────
  # Criteria: age diff ≤ 5, same health zone, location similarity ≥ 75
  contact.case.matches.p2 <-
    contact.case.matches.p1 |>
    filter(age.dist <= 5,
           zone_sante_notification.x == zone_sante_notification.y,
           location.similarity >= 75) |>
    arrange(matched_case, desc(name.sim), desc(location.similarity),
            desc(address.similarity), age.dist) |>
    distinct(matched_case, .keep_all = TRUE)

  # ── Tier 2: Cross-zone matches (relaxed location, strict name) ─────────────
  # Criteria: age diff ≤ 5, different health zone, name similarity ≥ 95
  contact.case.matches.p3 <-
    contact.case.matches.p1 |>
    filter(age.dist <= 5,
           zone_sante_notification.x != zone_sante_notification.y,
           name.sim >= 95) |>
    arrange(matched_case, desc(name.sim), desc(location.similarity),
            desc(address.similarity), age.dist) |>
    distinct(matched_case, .keep_all = TRUE)

  # Store province-level results in the accumulation lists
  matches.ls[[p]]      <- contact.case.matches.p2
  matches.ls.less[[p]] <- contact.case.matches.p3
}

# Combine within-zone matches across all provinces
contact.case.matches.inZS  <- bind_rows(matches.ls)
# Combine cross-zone matches across all provinces
contact.case.matches.outZS <- bind_rows(matches.ls.less)

# Final combined, deduplicated, column-tidy contact-case linkage table
contact.case.matches <-
  contact.case.matches.inZS |>
  bind_rows(contact.case.matches.outZS) |>
  arrange(matched_case, desc(name.sim), age.dist) |>
  distinct(matched_case, .keep_all = TRUE) |>
  # Drop redundant suffix-".y" columns that came from the join
  select(-c(province_notification.y,
            zone_sante_notification.y,
            s1_lieu_residence_permanente_zone_sante,
            aire_sante_notification.y,
            alert_village_quartier,
            alert_adresse_complete,
            age_ans.y,
            sexe.y)) |>
  # Re-order columns: ID, name, matched LL name, epid#, then everything else
  select(id_contact, nom_prenom_contact,
         nom_ll = matched_case, num_epid, everything()) |>
  rename(num_epid_cas_source = numero_epid_alerte) |>
  # Strip the ".x" suffix from remaining duplicate column names
  rename_all(~ str_replace_all(., "\\.x", "")) |> 
  mutate( matched.source = auto_correct_typos(noms_cas_source, evd.short$nom_post_nom_prenom_cas),
          name.sim = match_similarity(matched.source, noms_cas_source ))

# View the final contact-case linkage
contact.case.matches |> View()

# ── 4. Infector Identification from Line-list ─────────────────────────────────

# Step 3: Identify infectors from the EVD line-list using the
#         "s4_ma1_nom_malade_potentiel" field (suspected source case name)

# Subset EVD columns relevant for infector-infectee analysis
evd.ife.ifor <- evd |>
  select(province_notification, zone_sante_notification,
         aire_sante_notification, age_ans, sexe,
         nom_post_nom_prenom_cas, num_epid,
         date_heure_notification_alerte,
         alert_date_debut_symptoms,
         s1_malade_etait_il_contact_connu,
         alert_lien_epidemiologic,
         s4_ma1_nom_malade_potentiel,
        classification_finale_cas, lab_resultat_final)

# Keep only records where a potential infector name was recorded
evd.ifor <-
  evd.ife.ifor |>
  filter(!is.na(s4_ma1_nom_malade_potentiel))

# Fuzzy-match the "suspected infector" name against all case names in the
# full line-list to resolve it to a known case record
evd.ifor.matches.0 <-
  evd.ifor |>
  mutate(matched_cases = auto_correct_typos(s4_ma1_nom_malade_potentiel,
                                            evd.ife.ifor$nom_post_nom_prenom_cas,
                                            match_threshold = 80))

# Enrich with infector metadata
evd.ifor.matches <-
  evd.ifor.matches.0 |>
  left_join(
    select(evd.ife.ifor,
           province_notification, zone_sante_notification,
           aire_sante_notification,
           nom_post_nom_prenom_cas, num_epid, age_ans, sexe,
           date_heure_notification_alerte,
           alert_date_debut_symptoms,
           alert_lien_epidemiologic,
           s1_malade_etait_il_contact_connu,
           , lab_resultat_final,
          classification_finale_cas),
    by = c("matched_cases" = "nom_post_nom_prenom_cas")
  ) |>
  filter(!is.na(num_epid.y)) |>           # Only keep matches that resolved
  mutate(name.sim = match_similarity(s4_ma1_nom_malade_potentiel, matched_cases)) |>
  arrange(matched_cases, desc(name.sim)) |>
  distinct(matched_cases, .keep_all = TRUE)

# Rename columns to mark infectee vs. infector sides
evd.ifor.matches <-
  evd.ifor.matches |> 
  # Columns from the infectee side (original record)
  rename_at(vars(province_notification.x:lab_resultat_final.x),
            ~ paste0(., "_infectee")) |>
  # Columns from the matched infector side
  rename_at(vars(matched_cases:classification_finale_cas.y),
            ~ paste0(., "_infector")) |>
  rename(nom_post_nom_prenom_cas_infector = matched_cases_infector) |>
  # Remove ".x" and ".y" join suffixes
  rename_all(~ str_replace_all(., "\\.x", "")) |>
  rename_all(~ str_replace_all(., "\\.y", "")) |>
  select(-c(name.sim, s4_ma1_nom_malade_potentiel_infectee))

# ── 5. Merge Contact-Based Pairs into Infector-Infectee Table ────────────────

# Extract epid numbers that appear as case sources (infectors) in the
# contact-case linkage
epinum_infector <- evd |>
  filter(num_epid %in% contact.case.matches$num_epid_cas_source) |>
  pull(num_epid)

# Extract epid numbers that appear as contacts-turned-cases (infectees)
epinum_infectee <- evd |>
  filter(num_epid %in% contact.case.matches$num_epid) |>
  pull(num_epid)

# Build infectee metadata from the EVD line-list
evd_infectees <- evd |>
  filter(num_epid %in% c(epinum_infectee)) |>
  select(province_notification, zone_sante_notification,
         aire_sante_notification,
         nom_post_nom_prenom_cas, num_epid, age_ans, sexe,
         date_heure_notification_alerte,
         alert_date_debut_symptoms,
         s1_malade_etait_il_contact_connu,
         alert_lien_epidemiologic, lab_resultat_final,
          classification_finale_cas) |>
  rename_all(~ paste0(., "_infectee"))

# Build infector metadata from the EVD line-list
evd_infectors <- evd |>
  filter(num_epid %in% c(epinum_infector)) |>
  select(province_notification, zone_sante_notification,
         aire_sante_notification,
         nom_post_nom_prenom_cas, num_epid, age_ans, sexe,
         date_heure_notification_alerte,
         alert_date_debut_symptoms,
         s1_malade_etait_il_contact_connu,
         alert_lien_epidemiologic, lab_resultat_final,
          classification_finale_cas) |>
  rename_all(~ paste0(., "_infector"))

# Assemble contact-based infector-infectee pairs by joining the linkage table
# to the infectee and infector metadata
contact.case.matches_infectors <- contact.case.matches |>
  filter(num_epid_cas_source %in% epinum_infector) |>
  select(num_epid_infectee = num_epid,
         num_epid_infector = num_epid_cas_source) |>
  left_join(evd_infectees,
            by = c("num_epid_infectee" = "num_epid_infectee")) |>
  left_join(evd_infectors,
            by = c("num_epid_infector" = "num_epid_infector"))

# Combine line-list-based pairs (step 3) with contact-based pairs (step 5),
# deduplicating by infectee epid number
evd.infectors_infectees <- evd.ifor.matches |>
  bind_rows(contact.case.matches_infectors) |>
  distinct(num_epid_infectee, .keep_all = TRUE)

# ── 6. Export ─────────────────────────────────────────────────────────────────

cli_progress_step("Exporting connected datasets")

# Create a date-stamped sub-directory under the output directory
day_dir    <- format(Sys.Date(), "%d_%B")
export_dir <- fs::path(output_dir, day_dir, "contact_ll_connect")

# Ensure the export directory exists (create recursively if needed)
if (!fs::dir_exists(export_dir)) {
  fs::dir_create(export_dir, recurse = TRUE)
}

# Build a timestamped filename
timestamp  <- format(Sys.time(), "%Y_%m_%d_%H%M")
export_file <- fs::path(export_dir,
                        paste0("contact_ll_connected_Int_", timestamp, ".rds"))

export_file_infector <- fs::path(export_dir,
                        paste0("infector_infectees_Int_", timestamp, ".rds"))

# Save the contact-case linkage table
saveRDS(contact.case.matches, export_file)
cli_alert_success("Contact ll connected Dataset successfully saved to {.file {export_file}}")

# Save the combined infector-infectee table (overwrites the same file;
# NOTE: this means only the second table persists on disk)
saveRDS(evd.infectors_infectees, export_file_infector)
cli_alert_success("Infecteed-Infector Dataset successfully saved to {.file {export_file_infector}}")
