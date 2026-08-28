# Children analysis - EVD17, RDC
# Analysis of paediatric cases and contacts (children <5 / <18),
# including incidence, contact follow-up, and infector-infectee chains.
# ============================================================================

## Loading libraries ----

# Same package set as the other analysis scripts (Contact_analysis.R, EpiCurves.R,
# lab.analysis.R): union of the packages loaded across them.
My.Packages <- installed.packages()[, "Package"]

if (!"pacman" %in% My.Packages) {
  install.packages("pacman")
} else {
  pacman::p_load(
    "openxlsx", "readxl", "incidence",
    "stringdist", "lubridate",
    "RColorBrewer", "wesanderson",
    "dplyr", "tidyverse", "stringi",
    "ggforce", "ggfittext", "ggthemes",
    "ggrepel", "tidytext",
    "zoo", "forecast","here",
    "DescTools", "scales", "kableExtra",
    install = TRUE
  )
}

source(here::here("helpers", "paths.R"))

# When sourced from a Quarto report, skip file/directory output generation
skip_output <- isTRUE(getOption("children.analysis.report_mode", FALSE))


## Loading custom functions ----

source(here::here("helpers", "DebutSem.R"))
source(here::here("helpers", "AgeCat.R"))
source(here::here("helpers", "AgeStd.R"))
source(here::here("helpers", "OrthoCorrect.R"))
source(here::here("helpers", "OrthoFix.R"))
source(here::here("helpers", "LoadLatestData.R"))
source(here::here("helpers", "clean_filename.R"))
source(here::here("helpers", "contact.cleaning.fx.R"))


## Importing datasets ----

# Directory where the cleaning-output snapshots live
output_dir <- data_cleaning_output

### EVD line-list (latest cleaned snapshot)
evd.data <- load_latest_data(
  output_dir,
  folder_name = NULL,
  format      = "rds",
  file_pattern  = "evd.clean_Int_.+\\.rds"
)

# Recent cleaning exports renamed this field; keep the historical analysis
# interface stable for the sections below.
if (!hasName(evd.data, "classification_finale_cas") &&
    hasName(evd.data, "classification_finale")) {
  evd.data <- evd.data |>
    mutate(classification_finale_cas = classification_finale)
}

### Contact line-list — wide format (latest cleaned snapshot)
contact.data <- load_latest_data(
  output_dir,
  folder_name   = NULL,
  format        = "rds",
  file_pattern  = "contact.clean_Int.+\\.rds"
)

### Contact line-list — long format (latest cleaned snapshot)
contact.data.long <- load_latest_data(
  output_dir,
  folder_name   = NULL,
  format        = "rds",
  file_pattern  = "contact.long.clean_Int_.+\\.rds"
)

### Contact -> EVD case linkage produced by scripts/contact_ll_connect.R
contact.case.matches <- load_latest_data(
  output_dir,
  folder_name   = NULL,
  format        = "rds",
  file_pattern  = "contact_ll_connected_Int_.+\\.rds"
)

### Infector - infectee pairs produced by scripts/contact_ll_connect.R
evd.infectors_infectees <- load_latest_data(
  output_dir,
  folder_name   = NULL,
  format        = "rds",
  file_pattern  = "infector_infectees_Int_.+\\.rds"
)


## Sanity checks ----

(max.notif.date <- max(as.Date(evd.data$date_heure_notification_alerte[
  which(as.Date(evd.data$date_heure_notification_alerte) <= as.Date(now()))]),
  na.rm = TRUE))

(max.fill.date <- max(as.Date(contact.data$date_maj[
  which(as.Date(contact.data$date_maj) <= as.Date(now()))]),
  na.rm = TRUE))

cat("Loaded EVD cases:", nrow(evd.data),
    "| Reference date:", format(max.notif.date), "\n")
cat("Loaded contacts (wide):", nrow(contact.data),
    "| Reference date:", format(max.fill.date), "\n")
cat("Loaded contacts (long):", nrow(contact.data.long), "\n")
cat("Loaded contact-case linkages:", nrow(contact.case.matches), "\n")
cat("Loaded infector-infectee pairs:", nrow(evd.infectors_infectees), "\n")

## Analysis

table(evd.data$age_ans)
table(evd.data$s6_statut_final_patient, useNA = "ifany")
table(evd.data$etat_sante_actuel, useNA = "ifany")

table(!is.na(evd.data$alert_date_deces), useNA = "ifany")

table(evd.data$etat_sante_actuel,
      evd.data$nature_alerte, useNA = "ifany")


table(!is.na(evd.data$pec_status_avant_admission))

names(evd.data)

evd.data <-
  evd.data |>
  mutate(age_ans = as.numeric(age_ans),
         age_ans = case_when( age_ans > 120 ~ NA, .default = age_ans),
         nature_alerte = ifelse(!grepl("Incons", etat_sante_actuel, ignore.case = TRUE), "Vivant",
         nature_alerte),
         nature_alerte = ifelse(!is.na(alert_date_deces), "Décédé",
         nature_alerte))

evd.data.aggreg <- evd.data |> 
  mutate(age = AgeStd(age_ans, age_mois),
         age.groups = AgeCat(age, n_cats = 5)) |>
  summarise(
            ncases = n(),
            .by = c("age.groups", "sexe", "alert_conlusion",
                    "nature_alerte", "alert_lien_epidemiologic",
                    "s5_qu_prelev_a_deja_ete_soumis_malade",
                    "s5_statut_patient_lors_prelev","lab_resultat_final",
                   "classification_finale_cas","etat_nutritionnel_observe"
                   )
  )

evd.data.aggreg |>
  filter(!is.na(age.groups), !is.na(nature_alerte),
         classification_finale_cas == "Cas confirmé") |>
  summarise(ncases = sum(ncases),
            .by = c("age.groups", "nature_alerte")) |>
  mutate(PropCase = round(ncases / sum(ncases) * 100, 1), 
        .by = c("age.groups"))  |>
  pivot_wider(
    names_from = nature_alerte,
    values_from = c(ncases, PropCase),
    values_fill = 0
  )

age.groups.levels <- levels(AgeCat(0, n_cats=5 ))

## Age-Sex pyramids ----
## Plot 1: Validated alerts | Plot 2: Confirmed cases
## Both faceted by nature_alerte (Vivant / Décédé)

# Shared colour palette (matching AgeSex.R)
SexFill <- c("Masculin" = "#bd6009", "Feminin" = "#640b7c")

# ---- Population overlay data (collapsed to n_cats=44 age groups) ----

pop.zs <- read.csv(
  file.path(project_root, "data", "PopulationParAge", "pop_zs_drc.csv")
)

unique(pop.zs$age_group)

pop.pyramid <- pop.zs |>
  filter(adm1_viz_n %in% c("Ituri", "Nord KIvu")) |>
  mutate(
    age.groups = case_when(
      age_group %in% c("0_1", "1_4") ~ "<5",
      age_group %in% c("5_9", "10_14") ~ "5-14",
      age_group %in% c("15_19", "20_24") ~ "15-24",
      age_group %in% c("25_29", "30_34", "35_39", "40_44", "45_49") ~ "25-49",
      .default =                           "50+" ),
    sexe = case_when(
      gender == "Male"   ~ "Masculin",
      gender == "Female" ~ "Feminin"
    ) ) |>
  summarise(pop = sum(pop), .by = c(age.groups, sexe)) |>
  mutate(
    pop.perct = round(pop / sum(pop) * 100, 1),
    pop.perct = if_else(sexe == "Masculin", -pop.perct, pop.perct),
    age.groups = factor(age.groups, levels = age.groups.levels)
  )

# ---- Plot 1: Validated alerts age-sex pyramid ----

table(evd.data.aggreg$alert_conlusion, useNA = "ifany")

pyramid.validated <- evd.data.aggreg |>
  filter(
    !is.na(age.groups),
    !is.na(sexe),
    !is.na(nature_alerte),
    alert_conlusion == "Validée") |>
  summarise(ncases = sum(ncases), 
            .by = c(age.groups, sexe, nature_alerte)) |>
  mutate(
    PropCase = round(ncases / sum(ncases) * 100, 1),
    .by = nature_alerte
  ) |>
  mutate(
    PropCase = if_else(sexe == "Masculin", -PropCase, PropCase),
    age.groups = factor(age.groups, levels = age.groups.levels)
  )

n.validated <- sum(pyramid.validated$ncases)

pyramid.validated.g <- pyramid.validated |>
  ggplot(aes(x = age.groups, y = PropCase, fill = sexe)) +
  geom_bar(stat = "identity", 
           color = "grey50", width = 1) +
  geom_bar(
    data = pop.pyramid,
    aes(x = age.groups, y = pop.perct),
    fill  = "#dde8f300",
    color = "grey10",
    width =1,
    stat  = "identity"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = SexFill) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = function(x) paste0(abs(x), "%")
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  facet_wrap(~nature_alerte, ncol = 2) +
  labs(
    title    = "Distribution Age-Sexe des alertes validées MVB, RDC",
    subtitle = paste0(
      "Alertes validées à la date du ", format(max.notif.date, "%d-%m-%Y"), "\n",
      n.validated, " alertes validées (",
      sum(pyramid.validated$ncases[pyramid.validated$nature_alerte == "Vivant"]),
      " Vivants, ",
      sum(pyramid.validated$ncases[pyramid.validated$nature_alerte == "Décédé"]),
      " Décédés)"
    ),
    caption = paste0("IOA - CAI \u00a9", isoyear(now())),
    y = "Pourcentage", x = "", fill = ""
  ) +
  theme(
    panel.background   = element_blank(),
    panel.grid.major.x = element_line(linetype = 3, color = "grey", linewidth = 0.4),
    panel.grid.major.y = element_line(linetype = 3, color = "grey", linewidth = 0.4),
    axis.line.x        = element_line(colour = "grey70", linewidth = 0.5),
    axis.text          = element_text(size = 11, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    axis.ticks.y       = element_blank(),
    plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7),
    legend.position    = "top",
    legend.title       = element_blank(),
    legend.key.size    = unit(0.5, "cm"),
    strip.text         = element_text(face = "bold", size = 10),
    strip.background   = element_rect(fill = "grey80")
  )

# ---- Plot 2: Confirmed cases age-sex pyramid ----

pyramid.confirmed <- evd.data.aggreg |>
  filter(
    !is.na(age.groups),
    !is.na(sexe),
    !is.na(nature_alerte),
    classification_finale_cas == "Cas confirmé" | lab_resultat_final == "Positif"
  ) |>
  summarise(ncases = sum(ncases), .by = c(age.groups, sexe, nature_alerte)) |>
  mutate(
    PropCase = round(ncases / sum(ncases) * 100, 1),
    .by = nature_alerte
  ) |>
  mutate(
    PropCase = if_else(sexe == "Masculin", -PropCase, PropCase),
    age.groups = factor(age.groups, levels = age.groups.levels)
  )

n.confirmed <- sum(pyramid.confirmed$ncases)

pyramid.confirmed.g <- pyramid.confirmed |>
  ggplot(aes(x = age.groups, y = PropCase, fill = sexe)) +
  geom_bar(stat = "identity", color = "grey90", width = 1) +
  geom_bar(
    data = pop.pyramid,
    aes(x = age.groups, y = pop.perct),
    fill  = "#dde8f300",
    color = "grey10",
    width = 1,
    stat  = "identity"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = SexFill) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = function(x) paste0(abs(x), "%")
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  facet_wrap(~nature_alerte, ncol = 2) +
  labs(
    title    = "Distribution Age-Sexe des cas confirmés MVB, RDC",
    subtitle = paste0(
      "Cas confirmés à la date du ", format(max.notif.date, "%d-%m-%Y"), "\n",
      n.confirmed, " cas confirmés (",
      sum(pyramid.confirmed$ncases[pyramid.confirmed$nature_alerte == "Vivant"]),
      " Vivants, ",
      sum(pyramid.confirmed$ncases[pyramid.confirmed$nature_alerte == "Décédé"]),
      " Décédés)"
    ),
    caption = paste0("IOA - CAI \u00a9", isoyear(now())),
    y = "Pourcentage", x = "", fill = ""
  ) +
  theme(
    panel.background   = element_blank(),
    panel.grid.major.x = element_line(linetype = 3, color = "grey", linewidth = 0.4),
    panel.grid.major.y = element_line(linetype = 3, color = "grey", linewidth = 0.4),
    axis.line.x        = element_line(colour = "grey70", linewidth = 0.5),
    axis.text          = element_text(size = 11, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    axis.ticks.y       = element_blank(),
    plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7),
    legend.position    = "top",
    legend.title       = element_blank(),
    legend.key.size    = unit(0.5, "cm"),
    strip.text         = element_text(face = "bold", size = 10),
    strip.background   = element_rect(fill = "grey80")
  )


# ---- Province & ZS age-sex pyramids (Vivant / Decede) stored in lists ----
# Mirrors pyramid.confirmed.g but produces one faceted plot per
# administrative unit, collected into named lists (pyramids.prov, pyramids.zs).
# Denominator: PropCase computed within (unit x nature_alerte) so each
# Vivant / Decede facet sums to 100%. Population overlay (hollow grey) is
# the general population of that unit, collapsed to the same 5 age groups.

# Affected provinces ordered by descending confirmed-case count.
# NOTE: evd.data.aggreg lacks geography, so derive from evd.data.
prov.agesex <- evd.data |>
  filter(classification_finale_cas == "Cas confirmé" | lab_resultat_final == "Positif") |>
  mutate(age.groups = AgeCat(AgeStd(age_ans, age_mois), n_cats = 5)) |>
  filter(!is.na(age.groups), !is.na(nature_alerte)) |>
  summarise(ncases = n(), .by = province_notification) |>
  arrange(desc(ncases)) |>
  pull(province_notification)

# Per-unit population overlays (5-group collapse, same schema as pop.pyramid)
pop.pyramid.prov <- pop.zs |>
  filter(adm1_viz_n %in% prov.agesex) |>
  mutate(
    age.groups = case_when(
      age_group %in% c("0_1", "1_4") ~ "<5",
      age_group %in% c("5_9", "10_14") ~ "5-14",
      age_group %in% c("15_19", "20_24") ~ "15-24",
      age_group %in% c("25_29", "30_34", "35_39", "40_44", "45_49") ~ "25-49",
      .default = "50+"
    ),
    sexe = case_when(
      gender == "Male"   ~ "Masculin",
      gender == "Female" ~ "Feminin"
    )
  ) |>
  summarise(pop = sum(pop), .by = c(adm1_viz_n, age.groups, sexe)) |>
  mutate(
    pop.perct = round(pop / sum(pop) * 100, 1),
    .by = adm1_viz_n
  ) |>
  mutate(
    pop.perct = if_else(sexe == "Masculin", -pop.perct, pop.perct),
    age.groups = factor(age.groups, levels = age.groups.levels)
  ) |>
  rename(province_notification = adm1_viz_n)

# ZS with enough confirmed cases to render a stable pyramid (min 5)
zs.agesex <- evd.data |>
  filter(classification_finale_cas == "Cas confirmé" | lab_resultat_final == "Positif") |>
  mutate(age.groups = AgeCat(AgeStd(age_ans, age_mois), n_cats = 5)) |>
  filter(!is.na(age.groups), !is.na(nature_alerte)) |>
  summarise(ncases = n(), .by = zone_sante_notification) |>
  filter(ncases >= 5) |>
  arrange(desc(ncases)) |>
  pull(zone_sante_notification)

pop.pyramid.zs <- pop.zs |>
  filter(adm2_viz_n %in% zs.agesex) |>
  mutate(
    age.groups = case_when(
      age_group %in% c("0_1", "1_4") ~ "<5",
      age_group %in% c("5_9", "10_14") ~ "5-14",
      age_group %in% c("15_19", "20_24") ~ "15-24",
      age_group %in% c("25_29", "30_34", "35_39", "40_44", "45_49") ~ "25-49",
      .default = "50+"
    ),
    sexe = case_when(
      gender == "Male"   ~ "Masculin",
      gender == "Female" ~ "Feminin"
    )
  ) |>
  summarise(pop = sum(pop), .by = c(adm2_viz_n, age.groups, sexe)) |>
  mutate(
    pop.perct = round(pop / sum(pop) * 100, 1),
    .by = adm2_viz_n
  ) |>
  mutate(
    pop.perct = if_else(sexe == "Masculin", -pop.perct, pop.perct),
    age.groups = factor(age.groups, levels = age.groups.levels)
  ) |>
  rename(zone_sante_notification = adm2_viz_n)

# ---- Per-unit confirmed-case aggregates, status-split ----
# Built from evd.data (evd.data.aggreg has no geography columns).

pyramid.confirmed.prov <- evd.data |>
  filter(classification_finale_cas == "Cas confirmé" | lab_resultat_final == "Positif") |>
  mutate(age.groups = AgeCat(AgeStd(age_ans, age_mois), n_cats = 5)) |>
  filter(
    !is.na(age.groups),
    !is.na(sexe),
    !is.na(nature_alerte),
    !is.na(province_notification)
  ) |>
  summarise(
    ncases = n(),
    .by = c(age.groups, sexe, nature_alerte, province_notification)
  ) |>
  mutate(
    PropCase = round(ncases / sum(ncases) * 100, 1),
    .by = c(province_notification, nature_alerte)
  ) |>
  mutate(
    PropCase = if_else(sexe == "Masculin", -PropCase, PropCase),
    age.groups = factor(age.groups, levels = age.groups.levels),
    province_notification = factor(province_notification, levels = prov.agesex)
  ) |>
  filter(province_notification %in% prov.agesex)

pyramid.confirmed.zs <- evd.data |>
  filter(classification_finale_cas == "Cas confirmé" | lab_resultat_final == "Positif") |>
  mutate(age.groups = AgeCat(AgeStd(age_ans, age_mois), n_cats = 5)) |>
  filter(
    !is.na(age.groups),
    !is.na(sexe),
    !is.na(nature_alerte),
    !is.na(zone_sante_notification),
    zone_sante_notification %in% zs.agesex
  ) |>
  summarise(
    ncases = n(),
    .by = c(age.groups, sexe, nature_alerte, zone_sante_notification)
  ) |>
  mutate(
    PropCase = round(ncases / sum(ncases) * 100, 1),
    .by = c(zone_sante_notification, nature_alerte)
  ) |>
  mutate(
    PropCase = if_else(sexe == "Masculin", -PropCase, PropCase),
    age.groups = factor(age.groups, levels = age.groups.levels),
    zone_sante_notification = factor(zone_sante_notification, levels = zs.agesex)
  )

pyramid.confirmed.zs.tbl <- evd.data |>
  filter(classification_finale_cas == "Cas confirmé" | lab_resultat_final == "Positif") |>
  mutate(age.groups = AgeCat(AgeStd(age_ans, age_mois), n_cats = 5)) |>
  filter(
    !is.na(age.groups),
    !is.na(sexe),
    !is.na(nature_alerte),
    !is.na(zone_sante_notification),
    zone_sante_notification %in% zs.agesex
  ) |>
  summarise(
    ncases = n(),
    .by = c(age.groups, nature_alerte, zone_sante_notification)
  ) |>
  mutate(
    n_confirmed = sum(ncases),
    PropCase = round(ncases / n_confirmed * 100, 1),
    .by = c(zone_sante_notification, nature_alerte)
  ) |>
  mutate(
    age.groups = factor(age.groups, levels = age.groups.levels),
    zone_sante_notification = factor(zone_sante_notification, levels = zs.agesex)
  )


pyramid.confirmed.zs.tbl.names <-
  as.character(unique(pyramid.confirmed.zs.tbl$zone_sante_notification[pyramid.confirmed.zs.tbl$age.groups == "<5"]))

pyramid.confirmed.zs.tbl.zs <- as.vector(pyramid.confirmed.zs.tbl |>
  filter(age.groups == "<5", nature_alerte == "Décédé") |>
  arrange(desc(ncases)) |> pull(zone_sante_notification)) 

pyramid.confirmed.zs.tbl.zs <- append(pyramid.confirmed.zs.tbl.zs, 
  pyramid.confirmed.zs.tbl.names[!pyramid.confirmed.zs.tbl.names %in% pyramid.confirmed.zs.tbl.zs])

pyramid.confirmed.zs.table <- pyramid.confirmed.zs.tbl |>
  filter(age.groups == "<5") |>
  mutate(cfr_alerts = paste0(PropCase , "% (",ncases, "/", n_confirmed, ")")) |>
  select(zone_sante_notification, nature_alerte, cfr_alerts) |>
  pivot_wider(names_from = nature_alerte, values_from = c(cfr_alerts),
              names_glue = "Alertes sur {nature_alerte}", 
              values_fill = list(cfr_alerts = "0% (0/0)")) |>
  mutate(zone_sante_notification = factor(zone_sante_notification, levels = pyramid.confirmed.zs.tbl.zs)) |>
  arrange(zone_sante_notification)

# ---- kableExtra table: Alertes cas confirmés <5 ans par zone de santé ----

pyramid.confirmed.zs.table.kbl <- pyramid.confirmed.zs.table |>
  rename(`Zone de Santé` = zone_sante_notification) |>
  kbl(escape = FALSE, align = "c",
      #caption = paste0("Alertes de cas confirmés <5 ans par zone de santé — ",
                       #format(max.notif.date, "%d %B %Y"))
) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
  #add_header_above(c(" " = 1,
                     #"Alertes : cas confirmés pour les <5 ans" = ncol(pyramid.confirmed.zs.table) - 1L),
                   #background = "#3e62bd", color = "white", bold = TRUE) |>
  row_spec(0, background = "#3e62bd", color = "white", bold = TRUE, font_size = "x-large") |>
  row_spec(1:nrow(pyramid.confirmed.zs.table),color = "black", bold = FALSE, font_size = "large") |>
  column_spec(1, bold = TRUE, border_right = TRUE)

# ---- Plot factory: one faceted pyramid per unit (mirrors pyramid.confirmed.g) ----

make_agesex_pyramid <- function(case_data, pop_data = NULL,
                                plot_title, plot_subtitle) {
  p <- ggplot(case_data, aes(x = age.groups, y = PropCase, fill = sexe)) +
    geom_bar(stat = "identity", color = "grey90", width = 1)

  if (!is.null(pop_data) && nrow(pop_data) > 0) {
    p <- p + geom_bar(
      data    = pop_data,
      aes(x = age.groups, y = pop.perct),
      fill  = "#dde8f300",
      color = "grey10",
      width = 1,
      stat  = "identity",
      inherit.aes = FALSE
    )
  }

  p <- p +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    coord_flip() +
    scale_fill_manual(values = SexFill) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = function(x) paste0(abs(x), "%")
    ) +
    # Pin age-group order to <5, 5-14, 15-24, 25-49, 50+ on every province
    # and ZS plot, matching pyramid.confirmed.g's factor levels even when a
    # unit subset is missing some levels or the two geom_bar layers combine.
    scale_x_discrete(expand = c(0, 0), limits = age.groups.levels) +
    facet_wrap(~nature_alerte, ncol = 2) +
    labs(
      title    = plot_title,
      subtitle = plot_subtitle,
      caption  = paste0("IOA - CAI \u00a9", isoyear(now())),
      y = "Pourcentage", x = "", fill = ""
    ) +
    theme(
      panel.background   = element_blank(),
      panel.grid.major.x = element_line(linetype = 3, color = "grey", linewidth = 0.4),
      panel.grid.major.y = element_line(linetype = 3, color = "grey", linewidth = 0.4),
      axis.line.x        = element_line(colour = "grey70", linewidth = 0.5),
      axis.text          = element_text(size = 11, face = "bold"),
      axis.title         = element_text(face = "bold", size = 12),
      axis.ticks.y       = element_blank(),
      plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
      plot.subtitle      = element_text(size = 14),
      plot.caption       = element_text(size = 7),
      legend.position    = "top",
      legend.title       = element_blank(),
      legend.key.size    = unit(0.5, "cm"),
      strip.text         = element_text(face = "bold", size = 10),
      strip.background   = element_rect(fill = "grey80")
    )

  p
}

# Null-coalesce helper (defined before use below)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Build subtitle with Vivant / Decede counts for one unit
unit_subtitle <- function(case_data, unit_col, unit) {
  sub <- case_data |>
    filter(.data[[unit_col]] == unit) |>
    summarise(n = sum(ncases), .by = nature_alerte)
  n_v   <- sub$n[sub$nature_alerte == "Vivant"]  %||% 0
  n_d   <- sub$n[sub$nature_alerte == "Décédé"]   %||% 0
  n_all <- sum(sub$n)
  paste0(
    "Données DHIS2 a la date du ", format(max.notif.date, "%d-%m-%Y"), "\n",
    n_all, " cas confirmes (", n_v, " Vivants, ", n_d, " Decedes)"
  )
}

# ---- Build province list ----

pyramids.prov <- setNames(
  map(prov.agesex, \(p) {
    make_agesex_pyramid(
      case_data     = filter(pyramid.confirmed.prov, province_notification == p),
      pop_data      = filter(pop.pyramid.prov, province_notification == p),
      plot_title    = paste0("Pyramide Age-Sexe - ", p),
      plot_subtitle = unit_subtitle(pyramid.confirmed.prov, "province_notification", p)
    )
  }),
  prov.agesex
)


# ---- Build ZS list (overlay skipped for ZS missing from population data) ----

pyramids.zs <- setNames(
  map(zs.agesex, \(z) {
    pop_z <- filter(pop.pyramid.zs, zone_sante_notification == z)
    if (nrow(pop_z) == 0) pop_z <- NULL
    sub <- unit_subtitle(pyramid.confirmed.zs, "zone_sante_notification", z)
    if (is.null(pop_z)) {
      sub <- paste0(sub, "\n(Pas de donnees de population disponibles pour cette ZS)")
    }
    make_agesex_pyramid(
      case_data     = filter(pyramid.confirmed.zs, zone_sante_notification == z),
      pop_data      = pop_z,
      plot_title    = paste0("Pyramide Age-Sexe - ZS ", z),
      plot_subtitle = sub
    )
  }),
  zs.agesex
)

# Report ZS with no population overlay available (informational only)
zs.no.pop <- setdiff(zs.agesex, unique(pop.pyramid.zs$zone_sante_notification))
if (length(zs.no.pop) > 0) {
  message("Plotted without population overlay for ZS: ",
          paste(zs.no.pop, collapse = ", "))
}

# ---- Sanity checks ----

stopifnot(length(pyramids.prov) == length(prov.agesex))
stopifnot(length(pyramids.zs)   == length(zs.agesex))
stopifnot(all(map_lgl(pyramids.prov, ggplot2::is.ggplot)))
stopifnot(all(map_lgl(pyramids.zs,   ggplot2::is.ggplot)))

cat("Province pyramids:", length(pyramids.prov),
    "| ZS pyramids:", length(pyramids.zs), "\n")

# Confirmed cases proportion by age groups 

evd.data.aggreg.conf.prop <- evd.data.aggreg |>
  filter(!is.na(nature_alerte), !is.na(age.groups),
         classification_finale_cas == "Cas confirmé") |>
  summarise(
    ncases = sum(ncases),
    .by = c("age.groups", "nature_alerte")
  ) |>
  pivot_wider(
    names_from = nature_alerte,
    values_from = ncases,
    values_fill = 0
  ) |>
  mutate(
    total_cases_age = Vivant + Décédé ,
    total_cases = sum(Vivant, Décédé),
    total_dead = sum(Décédé),
    case.perc = total_cases_age / total_cases,
    cfr_age = Décédé / total_dead
  ) |>
  mutate(
    case_test = map2(total_cases_age, total_cases, ~ prop.test(.x, .y, conf.level = 0.95)),
    cfr_test = map2(Décédé, total_dead, ~ prop.test(.x, .y, conf.level = 0.95)),
    case_lower = map_dbl(case_test, ~ .x$conf.int[1]),
    case_upper = map_dbl(case_test, ~ .x$conf.int[2]),
    cfr_lower = map_dbl(cfr_test, ~ .x$conf.int[1]),
    cfr_upper = map_dbl(cfr_test, ~ .x$conf.int[2])
  ) |>
  select(-case_test, -cfr_test)
 

# ---- Side-by-side bar plot: cases and deaths by age group ----

evd.data.aggreg.conf.prop.plot <- bind_rows(
  evd.data.aggreg.conf.prop |>
    select(age.groups, perc = case.perc,
           lower = case_lower, upper = case_upper) |>
    mutate(indicator = "Total cas confirmés"),
  evd.data.aggreg.conf.prop |>
    select(age.groups, perc = cfr_age,
           lower = cfr_lower, upper = cfr_upper) |>
    mutate(indicator = "Décès confirmés")
)

conf.prop.cols <- c("Total cas confirmés" = "#203ec2", "Décès confirmés" = "#6e0404")

evd.data.aggreg.conf.prop.g <- evd.data.aggreg.conf.prop.plot |>
  ggplot(aes(x = age.groups, y = perc, fill = indicator)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7,
           color = "grey50") +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.9),
    width = 0.25, linewidth = 1, color = "black"
  ) +
  scale_fill_manual(values = conf.prop.cols) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Proportion de cas et de décès confirmés par groupe d'âge MVB, RDC",
    subtitle = paste0(
      "Données DHIS2 à la date du ", format(max.notif.date, "%d-%m-%Y"), "\n",
      "Barres d'erreur : intervalles de confiance à 95%"
    ),
    caption = paste0("IOA - CAI \u00a9", isoyear(now())),
    y = "Proportion", x = "Groupe d'âge", fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text          = element_text(size = 11, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7)
  )

evd.data.aggreg.conf.prop.g 



# Death proportion by age groups 

evd.data.aggreg.prop <- evd.data.aggreg |>
  filter(!is.na(nature_alerte), !is.na(age.groups),
         classification_finale_cas == "Cas confirmé") |>
  summarise(
    ncases = sum(ncases),
    .by = c("age.groups", "nature_alerte")
  ) |>
  pivot_wider(
    names_from = nature_alerte,
    values_from = ncases,
    values_fill = 0
  ) |>
  mutate(
    total_cases = Vivant + Décédé,
    cfr = Décédé / total_cases
  ) |>
  mutate(
    cfr_test = map2(Décédé, total_cases, ~ prop.test(.x, .y, conf.level = 0.95)),
    cfr_lower = map_dbl(cfr_test, ~ .x$conf.int[1]),
    cfr_upper = map_dbl(cfr_test, ~ .x$conf.int[2])
  ) |>
  select(-cfr_test)
 

evd.data.aggreg.prop.g <- evd.data.aggreg.prop |>
  ggplot(aes(x = age.groups, y = cfr)) +
  geom_bar(stat = "identity", fill = "#6e0404", color = "grey50", width = 0.7) +
  geom_errorbar(aes(ymin = cfr_lower, ymax = cfr_upper), size=1, width = 0.1, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Proportion de décès confirmés par groupe d'âge MVB, RDC",
    subtitle = paste0(
      "Données DHIS2 à la date du ", format(max.notif.date, "%d-%m-%Y"), "\n",
      "Proportion de décès (communautaire) avec intervalles de confiance à 95%"
    ),
    caption = paste0("IOA - CAI \u00a9", isoyear(now())),
    y = "Proportion de décès", x = "Groupe d'âge"
  ) +
  theme_minimal() +
  theme(
    axis.text          = element_text(size = 11, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7)
  )

evd.data.aggreg.prop 

# Death proportions by age groups over time

evd.data.aggreg.ts <- evd.data |> 
  filter(classification_finale_cas == "Cas confirmé",
         alert_date_debut_symptoms >= "2026-05-01") |>
  mutate(age = AgeStd(age_ans, age_mois),
         age.groups = AgeCat(age, n_cats = 5),
         epiw = get_monday(alert_date_debut_symptoms)) |>
  summarise(
            ncases = n(),
            .by = c("age.groups",
                    "nature_alerte", 
                    "epiw"
                   )
  )

evd.data.cfr.com <- evd.data |> 
  filter(classification_finale_cas == "Cas confirmé",
         alert_date_debut_symptoms >= "2026-05-01") |>
  mutate(age = AgeStd(age_ans, age_mois),
         age.groups = AgeCat(age, n_cats = 5),
         epiw = get_monday(alert_date_debut_symptoms)) |>
  summarise(
            ncases = n(),
            .by = c("age.groups",
                    "sexe",
                    "nature_alerte", 
                    "epiw"
                   )
  ) |>
  pivot_wider(
    names_from = nature_alerte,
    values_from = ncases,
    values_fill = 0
  ) |> 
  mutate(
    ncases.total = Vivant + Décédé,
    cfr_com = round(Décédé / ncases.total, 1)
  ) 

if (!skip_output) {
  evd.data.cfr.com |>
    write.xlsx(
      file = file.path(
        project_root, "OutPut", "Agesex",
        paste0("AgeSex_ConfirmedCases_", format(max.notif.date, "%d%b"), ".xlsx")
      ),
      sheetName = "AgeSex_ConfirmedCases",
      rowNames = FALSE
    )
}

   
# ---- 3-week epidemiological period aggregation ----

evd.data.aggreg.ts.3w <- evd.data.aggreg.ts |>
  mutate(
    epiyear  = isoyear(epiw),
    epiweek  = isoweek(epiw),
    period_3w_id = (epiweek - 1) %/% 3,
    period_3w = paste0(epiyear, "-W", sprintf("%02d", period_3w_id * 3 + 1))
  ) |>
  summarise(
    ncases = sum(ncases),
    epiw_start = min(epiw),
    epiw_end   = max(epiw),
    n_weeks    = n_distinct(epiw),
    .by = c(period_3w, age.groups, nature_alerte)
  ) |>
  pivot_wider(
    names_from  = nature_alerte,
    values_from = ncases,
    values_fill = 0
  ) |>
  mutate(
    ncases.total = Vivant + Décédé
  ) |>
  mutate(
    ncases.vivant = sum(Vivant),
    ncases.décédé = sum(Décédé),
    ncases.total  = sum(ncases.total),
    alive.group   = round(Vivant / ncases.vivant, 3),
    cfr           = round(Décédé / ncases.total, 3),
    cfr.group     = round(Décédé / ncases.décédé, 3),
    .by = period_3w
  ) |>
  mutate(
    cfr_test       = map2(Décédé, ncases.total,  ~ prop.test(.x, .y, conf.level = 0.95)),
    cfr_lower      = map_dbl(cfr_test, ~ .x$conf.int[1]),
    cfr_upper      = map_dbl(cfr_test, ~ .x$conf.int[2]),
    cfr_group_test = map2(Décédé, ncases.décédé, ~ prop.test(.x, .y, conf.level = 0.95)),
    cfr_group_lower = map_dbl(cfr_group_test, ~ .x$conf.int[1]),
    cfr_group_upper = map_dbl(cfr_group_test, ~ .x$conf.int[2]),
    alive_group_test = map2(Vivant, ncases.vivant, ~ prop.test(.x, .y, conf.level = 0.95)),
    alive_group_lower = map_dbl(alive_group_test, ~ .x$conf.int[1]),
    alive_group_upper = map_dbl(alive_group_test, ~ .x$conf.int[2])
  ) |>
  select(-c(cfr_test, cfr_group_test, alive_group_test)) |>
  filter(n_weeks == 3) |>
  arrange(period_3w, age.groups)

#evd.data.aggreg.ts.3w |> View()


evd.data.aggreg.ts.3w.stack.g <- evd.data.aggreg.ts.3w |>
  ggplot(aes(x = period_3w, y = Décédé, fill = age.groups)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Proportion de décès par groupe d'âge (agrégation 3 semaines)",
    x     = "Période de 3 semaines épidémiologiques",
    y     = "Proportion de décès",
    fill  = "Groupe d'âge"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bar.cols <- c("Vivant" = "#3e62bd",  "Décédé" = "#8a0d0d")


evd.data.aggreg.ts.3w.g <- evd.data.aggreg.ts.3w |> 
  filter(age.groups == "<5") |>
  ggplot(aes(x=epiw_start))+
  geom_line(aes(y=cfr.group, color = "Décédé"), size=1)+
  geom_line(aes(y=alive.group, color= "Vivant"), size=1)+
  geom_point(aes(y=cfr.group, color = "Décédé"))+
  geom_point(aes(y=alive.group, color= "Vivant"))+
  geom_errorbar(aes(ymin=cfr_group_lower, 
                    ymax=cfr_group_upper, color = "Décédé"), width=0.5)+
  geom_errorbar(aes(ymin=alive_group_lower, 
                    ymax=alive_group_upper, color = "Vivant"), width=0.5)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_date(limits = c(min(evd.data.aggreg.ts.3w$epiw_start),
                          max(evd.data.aggreg.ts.3w$epiw_end)),
               expand = c(0.05,0),
               labels = function(x) paste0(isoweek(x), "-", 
                                           isoweek(x + weeks(2)),"\n", isoyear(x)),
               breaks = seq(min(evd.data.aggreg.ts.3w$epiw_start),
                           max(evd.data.aggreg.ts.3w$epiw_end),
                           by = "3 week"))+
  scale_color_manual(values = bar.cols)+
  labs(
    title    = "Tendances de la proportion d'alerts confirmées de la MVB pour les <5 ans, RDC",
    subtitle = paste0(
      "Donées DHIS2-tracker à la date du ", format(max.notif.date, "%d-%m-%Y"), "\n",
      "Proportion d'alertes confirmées avec intervalles de confiance à 95%"
    ),
    caption = paste0("IOA - CAI \u00a9", isoyear(now())),
    y = "Proportions(%)", x = "semaine épidémiologique de début de symptômes",
    color =""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.text          = element_text(size = 11, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7),
    legend.position = c(0.1,0.9)
  )


# sample collection of confirmed cases by age groups over time
# NOTE: bloc exploratoire désactivé — références de colonnes brutes non valides
# (date_heure_du_prelevement_de_l_echantillon) et pipe "|> names() |> mutate()"
# incorrect. La logique utile est reprise et corrigée ci-dessous (evd.data.samples).

# date_heure_du_prelevement_de_l_echantillon
#
# evd.data |> names()
#   mutate(age = AgeStd(age_ans, age_mois),
#          age.groups = AgeCat(age, n_cats = 5),
#          epiw = get_monday(alert_date_debut_symptoms)) |>
#   summarise(
#     nlartes = n(),
#     .by = c(age.groups, epiw)
#   )

# ── Samples arriving at lab per case ────────────────────────────────────────
# Each non-NA lab_date_reception_n column = 1 sample that reached the lab.
# Columns: lab_date_reception_1, _2, _3, _4, _5, _6, _9, _10
# "lab_resultat_final"
# "lab_date_reception_"

evd.data.samples <- evd.data |>
  mutate(age = AgeStd(age_ans, age_mois),
         age.groups = AgeCat(age, n_cats = 5),
         epiw = get_monday(alert_date_debut_symptoms),
         n_samples_arrived = rowSums(
         !is.na(pick(starts_with("lab_date_reception_")))),
         n_samples_processed = rowSums(
         !is.na(pick(starts_with("lab_resultat_final")))),
         n_samples_positive = rowSums(
         pick(starts_with("lab_resultat_final")) == "Positif", na.rm = TRUE),
         # Current cleaning exports carry the finalized result once per case.
         n_first_processed = !is.na(lab_resultat_final),
         n_first_positive = lab_resultat_final == "Positif",
  )

sum(table(evd.data.samples$n_samples_positive, useNA = "ifany"))

# Distribution of sample counts across cases

evd.data.samples.sum <- evd.data.samples |>
  summarise(
    n_alerts = n(),
    n_sampled = sum(s5_qu_prelev_a_deja_ete_soumis_malade=="Oui", na.rm = TRUE),
    n_processed = sum(n_first_processed, na.rm = TRUE),
    n_positive = sum(n_first_positive, na.rm = TRUE),
    #n_positive = sum(lab_resultat_final == "Positif", na.rm = TRUE),
    n_confirmed = sum(classification_finale_cas == "Cas confirmé", na.rm = TRUE),
    Sampling.rate = round(n_sampled / n_alerts * 100, 1),
    positivity = round(n_positive / n_processed * 100, 1),
    .by = age.groups
  ) |>
  mutate(
    sampling_ci = map2(n_sampled, n_alerts, ~ prop.test(.x, .y, conf.level = 0.95)),
    sampling_lower = map_dbl(sampling_ci, ~ .x$conf.int[1]),
    sampling_upper = map_dbl(sampling_ci, ~ .x$conf.int[2]),
    positivity_ci = map2(n_positive, n_processed, ~ prop.test(.x, .y, conf.level = 0.95)),
    positivity_lower = map_dbl(positivity_ci, ~ .x$conf.int[1]),
    positivity_upper = map_dbl(positivity_ci, ~ .x$conf.int[2]),
    age.groups = factor(age.groups, levels = age.groups.levels)
  ) |>
  select(-c(sampling_ci, positivity_ci))

# ---- Side-by-side bar plot: Sampling rate and positivity by age group ----

evd.data.samples.plot <- bind_rows(
  evd.data.samples.sum |>
    select(age.groups, rate = Sampling.rate,
           lower = sampling_lower, upper = sampling_upper) |>
    mutate(indicator = "Sampling rate",
           lower = lower * 100, upper = upper * 100),
  evd.data.samples.sum |>
    select(age.groups, rate = positivity,
           lower = positivity_lower, upper = positivity_upper) |>
    mutate(indicator = "Positivity",
           lower = lower * 100, upper = upper * 100)
) |> filter(!is.na(age.groups), !is.na(indicator))

indicator.cols <- c("Sampling rate" = "#faefe5", "Positivity" = "#ff5959")

evd.data.samples.g <- evd.data.samples.plot |>
  ggplot(aes(x = age.groups, y = rate, fill = indicator)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7,
           color = "grey30") +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.9),
    width = 0.25
  ) +
  scale_fill_manual(values = indicator.cols) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0), limits = c(0, NA)
  ) +
  labs(
    title    = "Taux d'échantillonnage et positivité par groupe d'âge, MVB RDC",
    subtitle = paste0(
      "Données à la date du ", format(max.notif.date, "%d-%m-%Y"),
      "\nBarres d'erreur : intervalles de confiance à 95%"
    ),
    caption = paste0("IOA - CAI \u00a9", isoyear(now())),
    x = "Groupe d'âge", y = "Pourcentage", fill = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(size = 11, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7),
    legend.position    = "top"
  )

evd.data.samples.g

# symptoms to notification delay by age groups

evd.data.delay.notif.g <- evd.data |>
  filter(!is.na(alert_date_debut_symptoms),
         !is.na(date_heure_notification_alerte),
        alert_date_debut_symptoms >= "2026-05-01") |>
  mutate(
    date_heure_notification_alerte = as.Date(date_heure_notification_alerte),
    age = AgeStd(age_ans, age_mois),
    age.groups = AgeCat(age, n_cats = 5),
    delay_days0 = as.numeric(date_heure_notification_alerte-alert_date_debut_symptoms),
    delay_days = as.numeric(difftime(date_heure_notification_alerte,
                                      alert_date_debut_symptoms,
                                      units = "days"))
  ) |>
  summarise(
    n_alertes = n(),
    mean_delay = mean(delay_days0, na.rm = TRUE),
    median_delay = median(delay_days0, na.rm = TRUE),
    lower_delay = quantile(delay_days0, probs = 0.25, na.rm = TRUE),
    upper_delay = quantile(delay_days0, probs = 0.75, na.rm = TRUE),
    .by = age.groups
  ) |>
  ggplot(aes(x = age.groups, y = median_delay)) +
  geom_col(fill = "#3e62bd", color = "grey30", width = 0.7) +
  geom_errorbar(aes(ymin = lower_delay, ymax = upper_delay), width = 0.25) +
  labs(
    title    = "Délai médian (jours) entre symptômes et notification par groupe d'âge",
    subtitle = paste0(
      "Données à la date du ", format(max.notif.date, "%d-%m-%Y"),
      "\nBarres d'erreur : quartiles (Q1-Q3)"
    ),
    caption = paste0("IOA - CAI \u00a9", isoyear(now())),
    x = "Groupe d'âge", y = "Délai médian (jours)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(size = 11, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7)
  )

# contact matrix by age groups — all records
evd.infectors_infectees.AgeMatrix <-
  evd.infectors_infectees |> 
  mutate(age.groups.infectees = AgeCat(age_ans_infectee, n_cats = 5),
         age.groups.infectors = AgeCat(age_ans_infector, n_cats = 5)) |>
  summarise(
    n_pairs = n(),
    .by = c(age.groups.infectors, age.groups.infectees)
  )

# Ensure both axes share the same factor levels for a square matrix
evd.infectors_infectees.AgeMatrix <- evd.infectors_infectees.AgeMatrix |>
  mutate(
    age.groups.infectors = factor(age.groups.infectors, levels = age.groups.levels),
    age.groups.infectees = factor(age.groups.infectees, levels = age.groups.levels)
  )

# ── Heatmap: All infector-infectee pairs ──────────────────────────────────────

age.matrix.heatmap.all <- evd.infectors_infectees.AgeMatrix |>
  ggplot(aes(x = age.groups.infectees, y = age.groups.infectors, fill = n_pairs)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n_pairs), size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#fff7ec", high = "#7f0000", na.value = "grey90") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title    = "Matrice de transmission par groupe d'âge — Tous les paires infector-infecté",
    subtitle = paste0(
      nrow(evd.infectors_infectees), " paires infector-infecté identifiées",
      " à la date du ", format(max.notif.date, "%d-%m-%Y")
    ),
    caption  = paste0("IOA - CAI \u00a9", isoyear(now())),
    x        = "Groupe d'âge de l'infecté",
    y        = "Groupe d'âge de l'infector",
    fill     = "Nombre\nde paires"
  ) +
  theme_minimal() +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_text(size = 11, face = "bold"),
    axis.title       = element_text(face = "bold", size = 12),
    plot.title       = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle    = element_text(size = 14),
    plot.caption     = element_text(size = 7),
    legend.position  = "right"
  )

# ── Confirmed-only infector-infectee pairs ────────────────────────────────────

evd.infectors_infectees.confirmed <- evd.infectors_infectees |>
  filter(
    (classification_finale_cas_infectee  == "Cas confirmé" |
       lab_resultat_final_infectee       == "Positif"),
    (classification_finale_cas_infector  == "Cas confirmé" |
       lab_resultat_final_infector       == "Positif")
  )

evd.infectors_infectees.AgeMatrix.confirmed <-
  evd.infectors_infectees.confirmed |>
  mutate(age.groups.infectees  = AgeCat(age_ans_infectee, n_cats = 5),
         age.groups.infectors  = AgeCat(age_ans_infector, n_cats = 5)) |>
  summarise(
    n_pairs = n(),
    .by = c(age.groups.infectors, age.groups.infectees)
  ) |>
  mutate(
    age.groups.infectors = factor(age.groups.infectors, levels = age.groups.levels),
    age.groups.infectees = factor(age.groups.infectees, levels = age.groups.levels)
  )

# ── Heatmap: Confirmed cases only ─────────────────────────────────────────────

age.matrix.heatmap.confirmed <- evd.infectors_infectees.AgeMatrix.confirmed |>
  ggplot(aes(x = age.groups.infectees, y = age.groups.infectors, fill = n_pairs)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n_pairs), size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#fff7ec", high = "#7f0000", na.value = "grey90") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title    = "Matrice de transmission par groupe d'âge — Cas confirmés uniquement",
    subtitle = paste0(
      nrow(evd.infectors_infectees.confirmed), " paires confirmées",
      " à la date du ", format(max.notif.date, "%d-%m-%Y")
    ),
    caption  = paste0("IOA - CAI \u00a9", isoyear(now())),
    x        = "Groupe d'âge de l'infecté",
    y        = "Groupe d'âge de l'infector",
    fill     = "Nombre\nde paires"
  ) +
  theme_minimal() +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_text(size = 11, face = "bold"),
    axis.title       = element_text(face = "bold", size = 12),
    plot.title       = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle    = element_text(size = 14),
    plot.caption     = element_text(size = 7),
    legend.position  = "right"
  )
## contact matrix with contact-to-case links from the matching table (contact.case.matches) and age groups for infectors and infectees

contact.case.matches.ud <-
  contact.case.matches |> 
  mutate( matched.source = auto_correct_typos(noms_cas_source, evd.data$nom_post_nom_prenom_cas),
          name.sim = match_similarity(matched.source, noms_cas_source )) |>
  left_join(evd.data |> 
            select(num_epid.source= num_epid,nom_post_nom_prenom_cas, age_ans_source=age_ans, age_mois_source=age_mois, 
           sexe_source = sexe),
          by = c("matched.source" = "nom_post_nom_prenom_cas")) |>
  mutate(age.groups.infectees  = AgeCat(age_ans, n_cats = 5),
         age.groups.infectors  = AgeCat(age_ans_source, n_cats = 5)) |>
  distinct(num_epid, .keep_all = TRUE)

## contact follow-up data by age groups

# NOTE: références de colonnes brutes (symboles) désactivées — non assignées et
# non valides hors d'un data.frame.
# circonstances_cas
# type_contact
# relation_contact


contact.data.circ <- contact.data |>
  filter(!is.na(circonstances_cas),
         !is.na(age_ans)) |>
  mutate(age.groups = AgeCat(age_ans, n_cats = 5)) |>
  summarise(
    n_contacts = n(),
    .by = c(age.groups, circonstances_cas)) |>
  mutate(
    n_contacts_total = sum(n_contacts),
    circonstances_pct = n_contacts / n_contacts_total,
    .by = age.groups)


# ---- Donut chart helper: categorical contact variable by age group ----
# Replicates the circonstances_cas donut for any categorical contact column.
# Slices use cumulative PROPORTIONS so every facet sweeps a full circle,
# rows are pre-sorted so the largest slice comes first within each pie.
make_contact_donut <- function(data, category_col, plot_title, fill_label) {

  plot_data <- data |>
    filter(!is.na({{ category_col }}), !is.na(age_ans)) |>
    mutate(age.groups = AgeCat(age_ans, n_cats = 5)) |>
    summarise(
      n_contacts = n(),
      .by = c(age.groups, {{ category_col }})
    ) |>
    mutate(
      n_contacts_total = sum(n_contacts),
      pct = n_contacts / n_contacts_total,
      .by = age.groups
    ) |>
    arrange(age.groups, desc(n_contacts)) |>
    mutate(
      # Cumulative PROPORTIONS -> every facet sweeps a full circle
      # (raw counts only fill the largest facet's arc under coord_polar).
      maxy = cumsum(pct),
      miny = c(0, head(maxy, n = -1)),
      label.pos = (miny + maxy) / 2,
      .by = age.groups
    )

  center_data <- plot_data |>
    summarise(n_contacts_total = first(n_contacts_total), .by = age.groups)

  plot_data |>
    ggplot(aes(ymax = maxy, ymin = miny, xmax = 4, xmin = 3,
               fill = {{ category_col }})) +
    geom_rect(color = "white", linewidth = 0.5) +
    geom_text(
      data = center_data,
      aes(x = 2, y = 0, label = paste0("n = ", n_contacts_total)),
      inherit.aes = FALSE,
      size = 3.8, fontface = "bold", color = "grey30"
    ) +
    ggrepel::geom_label_repel(
      aes(x = 4, y = label.pos,
          label = scales::percent(pct, accuracy = 0.1)),
      fill = "#ffffff5d", nudge_y = 0, nudge_x = 0,
      color = "#010b17",
      direction = "both", hjust = 0.5, vjust = 0.25,
      max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
      size = 3.5
    ) +
    coord_polar(theta = "y") +
    xlim(c(2, 4.25)) +
    facet_wrap(~age.groups) +
    labs(
      title    = plot_title,
      subtitle = paste0(
        "Données à la date du ", format(max.notif.date, "%d-%m-%Y")
      ),
      caption  = paste0("IOA - CAI \u00a9", isoyear(now())),
      fill     = fill_label
    ) +
    theme_void() +
    theme(
      legend.position    = "bottom",
      plot.title         = element_text(colour = "#3e62bd", face = "bold",
                                        size = 16),
      plot.subtitle      = element_text(size = 14),
      plot.caption       = element_text(size = 7),
      strip.text         = element_text(face = "bold", size = 10),
      legend.title       = element_text(face = "bold")
    )
}


# ---- Pie chart: Circonstances de contact by age group ----
# Uses geom_rect + coord_polar pattern (matching AgeSex.R donut style)

contact.data.circ.plot <- contact.data.circ |>
  # Sort rows by descending n_contacts within each age group so the
  # cumulative sum (maxy/miny) places the largest slice first in every pie.
  arrange(age.groups, desc(n_contacts)) |>
  mutate(
    # Use cumulative PROPORTIONS so every facet sweeps a full circle
    # (raw counts only fill the largest facet's arc under coord_polar).
    maxy = cumsum(circonstances_pct),
    miny = c(0, head(maxy, n = -1)),
    label.pos = (miny + maxy) / 2,
    .by = age.groups
  )

contact.data.circ.center <- contact.data.circ.plot |>
  summarise(
    n_contacts_total = first(n_contacts_total),
    .by = age.groups
  )

contact.data.circ.plot.g <- contact.data.circ.plot |>
  ggplot(aes(ymax = maxy, ymin = miny, xmax = 4, xmin = 3,
             fill = circonstances_cas)) +
  geom_rect(color = "white", linewidth = 0.5) +
  geom_text(
    data = contact.data.circ.center,
    aes(x = 2, y = 0, label = paste0("n = ", n_contacts_total)),
    inherit.aes = FALSE,
    size = 3.8, fontface = "bold", color = "grey30"
  ) +
  ggrepel::geom_label_repel(
    aes(x = 4, y = label.pos,
        label = scales::percent(circonstances_pct, accuracy = 0.1)),
    fill = "#ffffff5d", nudge_y = 0, nudge_x = 0,
    color = "#010b17",
    direction = "both", hjust = 0.5, vjust = 0.25,
    max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
    size = 3.5
  ) +
  coord_polar(theta = "y") +
  xlim(c(2, 4.25)) +
  facet_wrap(~age.groups) +
  labs(
    title    = "Circonstances de contact par groupe d'âge, MVB RDC",
    subtitle = paste0(
      "Données DHIS2 à la date du ", format(max.notif.date, "%d-%m-%Y")
    ),
    caption  = paste0("IOA - CAI \u00a9", isoyear(now())),
    fill     = ""
  ) +
  theme_void() +
  theme(
    legend.position    = "top",
    plot.title         = element_text(colour = "#3e62bd", face = "bold",
                                      size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7),
    strip.text         = element_text(face = "bold", size = 10),
    legend.title       = element_text(face = "bold")
  )

# ---- Pie chart: Type de contact by age group ----

contact.data.contact.type.g <- make_contact_donut(
  data       = contact.data,
  category_col = type_contact,
  plot_title = "Type de contact par groupe d'âge, MVB RDC",
  fill_label = "Type de contact"
)

# ---- Pie chart: Relation de contact by age group ----
contact.data.contact.relation.g <- make_contact_donut(
  data       = contact.data,
  category_col = relation_contact,
  plot_title = "Relation de contact par groupe d'âge, MVB RDC",
  fill_label = "Relation de contact"
)

# secondary attack rates by age groups

contact.case.matches.ud.sar <- contact.case.matches.ud |> 
  summarise(n_exposed = n(),
            n_secondary_cases = sum(classification_finale_cas == "Cas confirmé",
                                    na.rm = TRUE),
            .by = c(age.groups.infectees)) |>
  mutate(
    sar = n_secondary_cases / n_exposed,
    ci  = map2(n_secondary_cases, n_exposed, ~ prop.test(.x, .y, conf.level = 0.95)),
    ci_lower = map_dbl(ci, ~ .x$conf.int[1]),
    ci_upper = map_dbl(ci, ~ .x$conf.int[2])
  ) |>
  select(-ci) |>
  mutate(age.groups.infectees = factor(age.groups.infectees,
                                       levels = age.groups.levels))

# ---- Dot-and-whisker: SAR by contact age group ----

sar.dotwhisker <- contact.case.matches.ud.sar |>
  ggplot(aes(x = age.groups.infectees, y = sar)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.1, size = 0.5, color = "grey40"
  ) +
  geom_point(size = 2, color = "#091f59") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  coord_flip()+
  labs(
    title    = "Taux d'attaques secondaires (SAR) par groupe d'âge l'exposé, MVB RDC",
    subtitle = paste0(
      "Données à la date du ", format(max.notif.date, "%d-%m-%Y"), "\n",
      "Barres d'erreur : intervalle de confiance à 95%"
    ),
    caption  = paste0("IOA - CAI \u00a9", isoyear(now())),
    x        = "Groupe d'âge du contact",
    y        = "Taux d'attaques secondaires (SAR)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(size = 11, face = "bold"),
    axis.title         = element_text(face = "bold", size = 12),
    plot.title         = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle      = element_text(size = 14),
    plot.caption       = element_text(size = 7)
  )

contact.case.matches.ud |> 
  filter(circonstances_cas == "1.Foyer") |>
  summarise(n_exposed =n(),
            n_secondary_cases = sum(classification_finale_cas == "Cas confirmé"),
            sar = round(n_secondary_cases / n_exposed*100, 1),
            .by = c(age.groups.infectees))

contact.case.matches.ud |> 
  filter(sexe_source == "Feminin") |>
  summarise(n_exposed =n(),
            n_secondary_cases = sum(classification_finale_cas == "Cas confirmé"),
            sar = round(n_secondary_cases / n_exposed*100, 1),
            .by = c(age.groups.infectees))

contact.case.matches.ud |> 
  filter(type_contact == "1.Contact Physique Direct") |>
  summarise(n_exposed =n(),
            n_secondary_cases = sum(classification_finale_cas == "Cas confirmé"),
            sar = round(n_secondary_cases / n_exposed*100, 1),
            .by = c(age.groups.infectees))

names(contact.case.matches.ud)

contact.case.matches.ud |> 
  #filter(sexe_source == "Masculin") |>
  summarise(n_exposed =n(),
            n_secondary_cases = sum(classification_finale_cas == "Cas confirmé"),
            sar = round(n_secondary_cases / n_exposed*100, 1),
            .by = c(circonstances_cas))

  evd.infectors_infectees |> 
  mutate(age.groups.infectees = AgeCat(age_ans_infectee, n_cats = 5),
         age.groups.infectors = AgeCat(age_ans_infector, n_cats = 5)) |> 
  summarise(n_exposed =n(),
            n_secondary_cases = sum(classification_finale_cas_infectee == "Cas confirmé" &
                                    classification_finale_cas_infector == "Cas confirmé") ,
            sar = round(n_secondary_cases / n_exposed*100, 1),
            .by = c(age.groups.infectees))


# ── Age matrices from contact.case.matches.ud ─────────────────────────────────
# contact.case.matches.ud uses contact-line-list linkages (contact → case)
# where classification_finale_cas refers to the infectee (contact) side only.

contact.case.matches.ud.AgeMatrix <- contact.case.matches.ud |>
  summarise(
    n_pairs = n(),
    .by = c(age.groups.infectors, age.groups.infectees)
  ) |>
  mutate(
    age.groups.infectors = factor(age.groups.infectors, levels = age.groups.levels),
    age.groups.infectees = factor(age.groups.infectees, levels = age.groups.levels)
  ) |>
  filter(!is.na(age.groups.infectors), !is.na(age.groups.infectees))

# ── Heatmap: All contact-to-case links ────────────────────────────────────────

age.matrix.contact.all <- contact.case.matches.ud.AgeMatrix |>
  ggplot(aes(x = age.groups.infectees, y = age.groups.infectors, fill = n_pairs)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n_pairs), size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#fff7ec", high = "#7f0000", na.value = "grey90") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title    = "Matrice de transmission par groupe d'âge — Contacts devenus cas (tous)",
    subtitle = paste0(
      nrow(contact.case.matches.ud), " paires contact→cas",
      " à la date du ", format(max.notif.date, "%d-%m-%Y")
    ),
    caption  = paste0("IOA - CAI \u00a9", isoyear(now())),
    x        = "Groupe d'âge du contact",
    y        = "Groupe d'âge de la source",
    fill     = "Nombre\nde paires"
  ) +
  theme_minimal() +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_text(size = 11, face = "bold"),
    axis.title       = element_text(face = "bold", size = 12),
    plot.title       = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle    = element_text(size = 14),
    plot.caption     = element_text(size = 7),
    legend.position  = "right"
  )

# ── Confirmed-only contact-to-case links ──────────────────────────────────────

contact.case.matches.ud.confirmed <- contact.case.matches.ud |>
  filter(
    classification_finale_cas == "Cas confirmé" | lab_resultat_final == "Positif"
  )

contact.case.matches.ud.AgeMatrix.confirmed <- contact.case.matches.ud.confirmed |>
  summarise(
    n_pairs = n(),
    .by = c(age.groups.infectors, age.groups.infectees)
  ) |>
  mutate(
    age.groups.infectors = factor(age.groups.infectors, levels = age.groups.levels),
    age.groups.infectees = factor(age.groups.infectees, levels = age.groups.levels)
  )|>
  filter(!is.na(age.groups.infectors), !is.na(age.groups.infectees))

# ── Heatmap: Confirmed contacts only ──────────────────────────────────────────

age.matrix.contact.confirmed <- contact.case.matches.ud.AgeMatrix.confirmed |>
  ggplot(aes(x = age.groups.infectees, y = age.groups.infectors, fill = n_pairs)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n_pairs), size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#fff7ec", high = "#7f0000", na.value = "grey90") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title    = "Matrice de transmission par groupe d'âge — Contacts confirmés uniquement",
    subtitle = paste0(
      nrow(contact.case.matches.ud.confirmed), " paires contact→cas confirmées",
      " (classification infecté uniquement)",
      "\nDonnées à la date du ", format(max.notif.date, "%d-%m-%Y")
    ),
    caption  = paste0("IOA - CAI \u00a9", isoyear(now())),
    x        = "Groupe d'âge du contact",
    y        = "Groupe d'âge de la source",
    fill     = "Nombre\nde paires"
  ) +
  theme_minimal() +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_text(size = 11, face = "bold"),
    axis.title       = element_text(face = "bold", size = 12),
    plot.title       = element_text(colour = "#3e62bd", face = "bold", size = 16),
    plot.subtitle    = element_text(size = 14),
    plot.caption     = element_text(size = 7),
    legend.position  = "right"
  )


# ---- Output ----

# Exports générés uniquement en exécution autonome du script ;
# le rendu Quarto (report_mode = TRUE) ne produit aucun fichier.

if (!skip_output) {
  day.dir.ch  <- paste0(here::here("OutPut/Children/"), format(max.notif.date, "%d%b"), "/")
  day.dir.ch.a <- paste0(day.dir.ch, "all/")
  day.dir.ch.p <- paste0(day.dir.ch, "prov/")
  day.dir.ch.z <- paste0(day.dir.ch, "zs/")

  for (d in c(day.dir.ch, day.dir.ch.a, day.dir.ch.p, day.dir.ch.z)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  # ---- Graphiques globaux (all/) ----

  pyramid.validated.g |>
    ggsave(filename = paste0(day.dir.ch.a, "Pyramide_AlertesValidees_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  pyramid.confirmed.g |>
    ggsave(filename = paste0(day.dir.ch.a, "Pyramide_CasConfirmes_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  evd.data.aggreg.conf.prop.g |>
    ggsave(filename = paste0(day.dir.ch.a, "Proportion_CasDeces_Age_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  evd.data.aggreg.prop.g |>
    ggsave(filename = paste0(day.dir.ch.a, "CFR_Age_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  evd.data.aggreg.ts.3w.g |>
    ggsave(filename = paste0(day.dir.ch.a, "CFR_Tendances_3Semaines_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 31, scale = 0.3)

  evd.data.aggreg.ts.3w.stack.g |>
    ggsave(filename = paste0(day.dir.ch.a, "ProportionDeces_3Semaines_Age_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 18, width = 30, scale = 0.3)

  evd.data.samples.g |>
    ggsave(filename = paste0(day.dir.ch.a, "Echantillonnage_Positivite_Age_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  evd.data.delay.notif.g |>
    ggsave(filename = paste0(day.dir.ch.a, "Delai_Symptomes_Notification_Age_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  age.matrix.heatmap.all |>
    ggsave(filename = paste0(day.dir.ch.a, "Matrice_InfecteurInfecte_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 18, width = 22, scale = 0.35)

  age.matrix.heatmap.confirmed |>
    ggsave(filename = paste0(day.dir.ch.a, "Matrice_InfecteurInfecte_Confirme_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 18, width = 22, scale = 0.35)

  contact.data.circ.plot.g |>
    ggsave(filename = paste0(day.dir.ch.a, "Donut_Circonstances_Contact_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 22, width = 20, scale = 0.3)

  contact.data.contact.type.g |>
    ggsave(filename = paste0(day.dir.ch.a, "Donut_Type_Contact_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 22, width = 20, scale = 0.3)

  contact.data.contact.relation.g |>
    ggsave(filename = paste0(day.dir.ch.a, "Donut_Relation_Contact_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 22, width = 20, scale = 0.3)

  sar.dotwhisker |>
    ggsave(filename = paste0(day.dir.ch.a, "SAR_Age_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 25, scale = 0.35)

  age.matrix.contact.all |>
    ggsave(filename = paste0(day.dir.ch.a, "Matrice_ContactCas_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 18, width = 22, scale = 0.35)

  age.matrix.contact.confirmed |>
    ggsave(filename = paste0(day.dir.ch.a, "Matrice_ContactCas_Confirme_",
                             format(max.notif.date, "%d%b"), ".png"),
           dpi = 300, height = 18, width = 22, scale = 0.35)

  # ---- Pyramides par province et par zone de santé ----

  imap(pyramids.prov, \(p, nm) ggsave(
    filename = paste0(day.dir.ch.p, "Pyramide_Prov_", clean_filename(nm), "_",
                      format(max.notif.date, "%d%b"), ".png"),
    plot = p, dpi = 300, height = 20, width = 25, scale = 0.35))

  imap(pyramids.zs, \(p, nm) ggsave(
    filename = paste0(day.dir.ch.z, "Pyramide_ZS_", clean_filename(nm), "_",
                      format(max.notif.date, "%d%b"), ".png"),
    plot = p, dpi = 300, height = 20, width = 25, scale = 0.35))

  # ---- Tableau kableExtra et données sous-jacentes ----

  pyramid.confirmed.zs.table.kbl |>
    save_kable(file = paste0(day.dir.ch.z, "ZS_ConfirmedUnder5_Alerts_",
                             format(max.notif.date, "%d%b"), ".html"))

  pyramid.confirmed.zs.table.kbl |>
    save_kable(file = paste0(day.dir.ch.z, "ZS_ConfirmedUnder5_Alerts_",
                             format(max.notif.date, "%d%b"), ".png"), zoom = 2)

  write.xlsx(
    pyramid.confirmed.zs.table,
    file = paste0(day.dir.ch.z, "ZS_ConfirmedUnder5_Alerts_",
                  format(max.notif.date, "%d%b"), ".xlsx"),
    sheetName = "ZS_ConfirmedUnder5_Alerts",
    rowNames = FALSE
  )
}
