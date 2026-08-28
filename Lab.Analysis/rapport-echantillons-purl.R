# ============================================================================
# Librairies — chunk JAMAIS mis en cache : garantit l'attachement des
# packages à chaque rendu (les chunks en cache n'exécutent pas leur code
# lors d'un cache hit, ce qui sauterait les library()).
# ============================================================================
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(lubridate)
library(scales)
library(kableExtra)
library(ggforce)
library(here)

# ============================================================================
# Setup : exécute le script d'analyse scripts/lab.analysis.R en mode rapport
# (options(lab.analysis.report_mode = TRUE)). Toutes les données, indicateurs
# et graphiques du rapport sont produits par le script — aucun calcul n'est
# dupliqué ici, le rapport reste donc toujours synchronisé avec lui. En mode
# rapport, le script saute les exports de fichiers (ggsave / xlsx / rds) et
# les vues interactives ; tous les objets restent disponibles en mémoire.
# ============================================================================

# Résoudre les chemins du projet indépendamment du répertoire de rendu.
library(here)
source(here::here("helpers", "paths.R"))

options(lab.analysis.report_mode = TRUE)
source(here::here("Lab.Analysis", "lab.analysis.R"))

## --- Helpers de formatage d'affichage (français), propres au rapport ---
fmt_pct <- function(x) formatC(round(x, 1), digits = 1, format = "f",
                               decimal.mark = ",")
fmt_num <- function(x) format(x, big.mark = " ", decimal.mark = ",",
                              trim = TRUE)
fmt_ci  <- function(lower, upper) {
  paste0(fmt_pct(lower), "–", fmt_pct(upper))
}
fmt_pct_na <- function(x) {
  dplyr::if_else(is.na(x), "—", fmt_pct(x))
}
fmt_rate_ci <- function(rate, lower, upper) {
  dplyr::if_else(
    is.na(rate) | is.na(lower) | is.na(upper),
    "—",
    paste0(fmt_pct(rate), "% (", fmt_ci(lower, upper), ")")
  )
}

src_tbl <- tibble::tibble(
  Source = c("Ligne list EVD nettoyée (`evd.data`)",
             "Données laboratoire nettoyées (`evd.lab.data`)",
             "Table long-format standardisée (`lab.long`)"),
  Contenu = c("Cas et alertes : géographie de notification, date de début des signes, conclusion de l'alerte, variables cliniques",
              "Un enregistrement par échantillon : dates de collecte, réception et analyse, résultat final, laboratoire de destination",
              "Dérivée de `evd.lab.data` : une ligne par échantillon avec dates standardisées et délais (début des signes → collecte → réception → analyse)"),
  Lignes = c(fmt_num(nrow(evd.data)),
             fmt_num(nrow(evd.lab.data)),
             fmt_num(nrow(lab.long)))
)

src_tbl |>
  kableExtra::kbl(col.names = c("Source", "Contenu", "Lignes"),
      align = "llr") |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) |>
  kableExtra::column_spec(1, bold = TRUE)

cov_prov_g

cov_prop_zs_tbl <- cov_prop_zs |>
  left_join(
    ind_zs$counts |>
      select(province_notification, zone_sante_notification,
             n_collected_all, n_arrived_all),
    by = c("province_notification", "zone_sante_notification")
  ) |>
  mutate(
    sampled  = paste0(fmt_pct(pct_sampled), "% (",
                      fmt_ci(lower_sampled, upper_sampled), ")"),
    arriving = ifelse(
      is.na(n_collected_all) | n_collected_all == 0,
      "—",
      paste0(fmt_pct(n_arrived_all / n_collected_all * 100), "%")
    )
  ) |>
  arrange(province_notification, desc(pct_sampled)) |>
  select(province_notification, zone_sante_notification,
         n_validated, sampled, arriving)

zs_cov_groups <- cov_prop_zs_tbl |>
  summarise(n = n(), .by = province_notification)

cov_prop_zs_tbl |>
  kableExtra::kbl(
    col.names = c("Province", "Zone de santé", "Alertes validées",
                  "Alertes échantillonnées (%, IC 95 %)",
                  "Échantillons reçus au labo (%)"),
    align = "llccc"
  ) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) |>
  kableExtra::column_spec(1, bold = TRUE) |>
  kableExtra::pack_rows(index = setNames(zs_cov_groups$n, zs_cov_groups$province_notification)) |>
  kableExtra::scroll_box(width = "100%", height = "550px")

lab_dest_g

prop_lab_tbl <- prop_lab |>
  mutate(
    cr_n = n_collection_to_reception_1d,
    cr_pct = paste0(fmt_pct(pct_collection_to_reception_1d), "% (",
                    fmt_ci(lower_collection_to_reception_1d,
                           upper_collection_to_reception_1d), ")"),
    ra_n = n_reception_to_analysis_1d,
    ra_pct = paste0(fmt_pct(pct_reception_to_analysis_1d), "% (",
                    fmt_ci(lower_reception_to_analysis_1d,
                           upper_reception_to_analysis_1d), ")"),
    cr2_n = n_collection_to_result_2d,
    cr2_pct = paste0(fmt_pct(pct_collection_to_result_2d), "% (",
                     fmt_ci(lower_collection_to_result_2d,
                            upper_collection_to_result_2d), ")")
  ) |>
  select(lab_destination, cr_n, cr_pct, ra_n, ra_pct, cr2_n, cr2_pct)

prop_lab_tbl |>
  kableExtra::kbl(
    col.names = c("Laboratoire",
                  "n", "Collecte → Réception ≤ 1 j",
                  "n", "Réception → Analyse ≤ 1 j",
                  "n", "Collecte → Résultat ≤ 2 j"),
    align = "lrrrrrr"
  ) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) |>
  kableExtra::column_spec(1, bold = TRUE) |>
  kableExtra::add_header_above(c(" " = 1,
                     "Indicateur 1 : réception rapide" = 2,
                     "Indicateur 2 : analyse rapide" = 2,
                     "Indicateur 3 : résultat rapide" = 2))

testing_all_tbl <- tibble::tibble(
  periode = c("Toute la période", "21 derniers jours"),
  validated = fmt_num(c(cov_all$n_validated, cov_all$n_validated_21)),
  tested = fmt_num(c(cov_all$n_tested_all, cov_all$n_tested_21)),
  rate = c(
    fmt_rate_ci(
      cov_all$testing_rate_all * 100,
      cov_all$testing_ci_lower_all * 100,
      cov_all$testing_ci_upper_all * 100
    ),
    fmt_rate_ci(
      cov_all$testing_rate_21 * 100,
      cov_all$testing_ci_lower_21 * 100,
      cov_all$testing_ci_upper_21 * 100
    )
  )
)

testing_all_tbl |>
  kableExtra::kbl(
    col.names = c("Période", "Alertes validées", "Alertes testées",
                  "Taux de test (%, IC 95 %)"),
    align = "lrrc"
  ) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) |>
  kableExtra::column_spec(1, bold = TRUE)

testing_prov_tbl <- cov_prov |>
  mutate(
    testing_all = fmt_rate_ci(
      testing_rate_all * 100,
      testing_ci_lower_all * 100,
      testing_ci_upper_all * 100
    ),
    testing_21 = fmt_rate_ci(
      testing_rate_21 * 100,
      testing_ci_lower_21 * 100,
      testing_ci_upper_21 * 100
    )
  ) |>
  arrange(desc(testing_rate_all)) |>
  select(province_notification, n_validated, n_tested_all, testing_all,
         n_validated_21, n_tested_21, testing_21)

testing_prov_tbl |>
  kableExtra::kbl(
    col.names = c("Province", "Alertes validées", "Alertes testées",
                  "Taux de test (%, IC 95 %)", "Alertes validées",
                  "Alertes testées", "Taux de test (%, IC 95 %)"),
    align = "lrrrcrr"
  ) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) |>
  kableExtra::column_spec(1, bold = TRUE) |>
  kableExtra::add_header_above(c(" " = 1, "Toute la période" = 3,
                                 "21 derniers jours" = 3)) |>
  kableExtra::scroll_box(width = "100%", height = "550px")

positivity_all_tbl <- tibble::tibble(
  periode = c("Toute la période", "21 derniers jours"),
  validated = fmt_num(c(cov_all$n_validated, cov_all$n_validated_21)),
  sampled = c(
    fmt_rate_ci(cov_all$pct_sampled,
                cov_all$lower_sampled, cov_all$upper_sampled),
    fmt_rate_ci(cov_all$pct_sampled_21,
                cov_all$lower_sampled_21, cov_all$upper_sampled_21)
  ),
  collected = fmt_num(c(ind_all$counts$n_collected_all,
                        ind_all$counts$n_collected_21)),
  processed = c(
    fmt_pct_na(ind_all$counts$n_processed_all /
                 ind_all$counts$n_collected_all * 100),
    fmt_pct_na(ind_all$counts$n_processed_21 /
                 ind_all$counts$n_collected_21 * 100)
  ),
  positivity = c(
    fmt_rate_ci(ind_all$counts$positivity_rate_all * 100,
                ind_all$counts$ci_lower_all * 100,
                ind_all$counts$ci_upper_all * 100),
    fmt_rate_ci(ind_all$counts$positivity_rate_21 * 100,
                ind_all$counts$ci_lower_21 * 100,
                ind_all$counts$ci_upper_21 * 100)
  )
)

positivity_all_tbl |>
  kableExtra::kbl(
    col.names = c("Période", "Alertes validées",
                  "Alertes échantillonnées (%, IC 95 %)",
                  "Échantillons enregistrés", "Échantillons traités (%)",
                  "Positivité (%, IC 95 %)"),
    align = "lrcccc"
  ) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) |>
  kableExtra::column_spec(1, bold = TRUE)

positivity_trend_onset_all

positivity_prov_tbl <- bind_rows(
  ind_prov$counts |>
    left_join(
      cov_prov |>
        select(province_notification, n_validated, n_sampled,
               pct_sampled, lower_sampled, upper_sampled),
      by = "province_notification"
    ) |>
    transmute(
      province_notification,
      periode = "Toute la période",
      validated = fmt_num(n_validated),
      sampled = fmt_rate_ci(pct_sampled, lower_sampled, upper_sampled),
      collected = fmt_num(n_collected_all),
      processed = fmt_pct_na(n_processed_all / n_collected_all * 100),
      positivity = fmt_rate_ci(positivity_rate_all * 100,
                               ci_lower_all * 100, ci_upper_all * 100)
    ),
  ind_prov$counts |>
    left_join(
      cov_prov |>
        select(province_notification, n_validated_21, n_sampled_21,
               pct_sampled_21, lower_sampled_21, upper_sampled_21),
      by = "province_notification"
    ) |>
    transmute(
      province_notification,
      periode = "21 derniers jours",
      validated = fmt_num(n_validated_21),
      sampled = fmt_rate_ci(pct_sampled_21,
                            lower_sampled_21, upper_sampled_21),
      collected = fmt_num(n_collected_21),
      processed = fmt_pct_na(n_processed_21 / n_collected_21 * 100),
      positivity = fmt_rate_ci(positivity_rate_21 * 100,
                               ci_lower_21 * 100, ci_upper_21 * 100)
    )
  ) |>
  mutate(periode = factor(periode,
                          levels = c("Toute la période", "21 derniers jours"))) |>
  arrange(province_notification, periode)

positivity_prov_groups <- positivity_prov_tbl |>
  summarise(n = n(), .by = province_notification)

positivity_prov_tbl |>
  kableExtra::kbl(
    col.names = c("Province", "Période", "Alertes validées",
                  "Alertes échantillonnées (%, IC 95 %)",
                  "Échantillons enregistrés", "Échantillons traités (%)",
                  "Positivité (%, IC 95 %)"),
    align = "llrcccc"
  ) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) |>
  kableExtra::column_spec(1, bold = TRUE) |>
  kableExtra::pack_rows(index = setNames(positivity_prov_groups$n,
                                         positivity_prov_groups$province_notification)) |>
  kableExtra::scroll_box(width = "100%", height = "550px")

# Graphiques produits par scripts/lab.analysis.R (section 3c) : mêmes seuils
# d'inclusion (min_weeks_plot / min_weeks_smooth) que les sorties du script.
for (g in names(pos_trend_first_prov_list)) {
  print(pos_trend_first_prov_list[[g]])
}

zs_pos_tbl <- pos_overview_first$pos_zs |>
  left_join(
    cov_zs |>
      select(province_notification, zone_sante_notification,
             testing_rate_all, testing_ci_lower_all, testing_ci_upper_all,
             testing_rate_21, testing_ci_lower_21, testing_ci_upper_21),
    by = c("province_notification", "zone_sante_notification")
  ) |>
  mutate(
    positivity_pct = fmt_pct(positivity_rate * 100),
    ci = fmt_ci(ci_lower * 100, ci_upper * 100),
    testing_all = fmt_rate_ci(testing_rate_all * 100,
                              testing_ci_lower_all * 100,
                              testing_ci_upper_all * 100),
    testing_21 = fmt_rate_ci(testing_rate_21 * 100,
                             testing_ci_lower_21 * 100,
                             testing_ci_upper_21 * 100)
  ) |>
  arrange(province_notification, desc(positivity_rate)) |>
  select(province_notification, zone_sante_notification,
         n_processed, n_positive, positivity_pct, ci, testing_all, testing_21)

zs_groups <- zs_pos_tbl |>
  summarise(n = n(), .by = province_notification)

zs_pos_tbl |>
  kableExtra::kbl(
    col.names = c("Province", "Zone de santé",
                  "1ers prélèv. traités", "Positifs",
                  "Positivité (%, IC 95 %)",
                  "Taux de test — toute la période (%, IC 95 %)",
                  "Taux de test — 21 derniers jours (%, IC 95 %)"),
    align = "llrrccc"
  ) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) |>
  kableExtra::column_spec(1, bold = TRUE) |>
  kableExtra::pack_rows(index = setNames(zs_groups$n, zs_groups$province_notification)) |>
  kableExtra::scroll_box(width = "100%", height = "550px")
