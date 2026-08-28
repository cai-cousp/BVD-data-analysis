

My.Packages = 
  installed.packages()[,"Package"]

if(!"pacman" %in% My.Packages) {
  
  install.packages("pacman")
} else{
  
  pacman::p_load("openxlsx","readxl","incidence",
                 "stringdist","lubridate",
                 "RColorBrewer","wesanderson",
                 "dplyr","tidyverse","stringi",
                 "ggforce","ggfittext","ggthemes",
                 "ggrepel","tidytext",
                 "zoo","forecast",
                 "DescTools",
                 install = T)
  
}

export_dir <- here::here("data/Alerts")

source(here::here("helpers", "LoadLatestData.R"))

evd.data <- load_latest_data(
  file.path(here::here(), "..", "DataCleaning", "data", "Output"),
  "evd.cleaning",
  format = "rds"
)

## Beni alerts

beni_alerts <-
  read_excel("data/Alerts/Base_alertes_Beni.xlsx", sheet = "Base_Alerte")

beni_alerts |> 
  mutate(`Zones Sante` = str_to_title(`Zones Sante`),
         Date_alerte = as.Date(Date_alerte),
         lab_result = case_when(Class_Final == "Confirmé" ~ "Positif",
                                Class_Final == "Non cas" ~ "Negatif",
                                .default = NA)) |>
  select(Date_alerte,Lien,`Zones Sante`,Statut_initial,
         Conclusion_finale=Conc_final,lab_result) |>
  saveRDS(file.path(export_dir, "Alert_Beni.rds"))

##  Summary of alerts


if (!dir.exists(export_dir)) {
  dir.create(export_dir, recursive = TRUE)
}

stopifnot(
  "zone_sante_notification" %in% names(evd.data),
  "alert_conlusion" %in% names(evd.data),
  "nature_alerte" %in% names(evd.data),
  "lab_resultat_final" %in% names(evd.data),
  "classification_finale_cas" %in% names(evd.data),
  "s1_malade_etait_il_contact_suivi" %in% names(evd.data),
  "alert_lien_epidemiologic" %in% names(evd.data)
)

### 1. Total records by health zone

summary_total <- evd.data |>
  filter(!is.na(zone_sante_notification)) |>
  count(zone_sante_notification, name = "Total_Records") |>
  arrange(desc(Total_Records))

### 2. Validated alerts by health zone

summary_validated <- evd.data |>
  filter(alert_conlusion == "Validée") |>
  count(zone_sante_notification, name = "Validated_Alerts") |>
  arrange(desc(Validated_Alerts))

### 3. Dead alerts by health zone

summary_dead <- evd.data |>
  filter(nature_alerte == "Décédé"|s6_statut_final_patient == "Décédé") |>
  count(zone_sante_notification, name = "Dead_Alerts") |>
  arrange(desc(Dead_Alerts))

### 4. Confirmed cases by health zone

summary_confirmed <- evd.data |>
  filter(
    lab_resultat_final == "Positif" | classification_finale_cas == "Cas confirmé"
  ) |>
  count(zone_sante_notification, name = "Confirmed_Cases") |>
  arrange(desc(Confirmed_Cases))

### 5. Confirmed cases with epidemiological link by health zone

summary_epilink <- evd.data |>
  filter(
    lab_resultat_final == "Positif" | classification_finale_cas == "Cas confirmé",
    s1_malade_etait_il_contact_suivi == "Oui" | alert_lien_epidemiologic == "Oui"
  ) |>
  count(zone_sante_notification, name = "Confirmed_With_EpiLink") |>
  arrange(desc(Confirmed_With_EpiLink))

### 6. Confirmed cases among dead alerts by health zone

summary_confirmed_dead <- evd.data |>
  filter(
    nature_alerte == "Décédé"|s6_statut_final_patient == "Décédé",
    lab_resultat_final == "Positif" | classification_finale_cas == "Cas confirmé"
  ) |>
  count(zone_sante_notification, name = "Confirmed_Cases_Among_Dead") |>
  arrange(desc(Confirmed_Cases_Among_Dead))

### Merge and export

alert_summary <- summary_total |>
  full_join(summary_validated, by = "zone_sante_notification") |>
  full_join(summary_dead, by = "zone_sante_notification") |>
  full_join(summary_confirmed, by = "zone_sante_notification") |>
  full_join(summary_epilink, by = "zone_sante_notification") |>
  full_join(summary_confirmed_dead, by = "zone_sante_notification") |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |>
  arrange(desc(Total_Records))

saveRDS(
  alert_summary,
  file.path(export_dir, "evd_alert_Summaries_by_HZ.rds"))

message("Exported ", nrow(alert_summary), " health zones to ",
        file.path(export_dir, "Alert_Summaries_by_HZ.xlsx"))


