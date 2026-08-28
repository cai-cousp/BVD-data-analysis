# Contacts.Analysis — Contact tracing analysis | Analyse du suivi des contacts

[English](#english) · [Français](#français)

---

## English

**Topic.** The operational health of contact tracing: are contacts found quickly, visited as often
as expected, followed to the end of their 21-day period, or lost — and how does that vary by
province and health zone over the last three weeks? A second strand links contact records to case
records to rebuild infector–infectee pairs.

**Scripts.**

| File | Purpose |
|---|---|
| `Contact_analysis.R` | Main engine (~2,300 lines). Computes every follow-up indicator, trend test and export below. |
| `Contact_3w_pptx.R` | Builds a native, editable **PowerPoint** of the 3-week indicators for reporting. Sources `Contact_analysis.R` in report mode. |
| `contact_ll_connect.R` | Probabilistic (fuzzy) linkage of the contact line list to the EVD case line list; exports the matched pairs and infector–infectee table as RDS for reuse by other topics. |
| `TransmissionChain.R` | **Legacy / exploratory.** `epicontacts` networks and `EpiEstim`/`R0` estimates on an older Kasai line list. Currently references an undefined `svfldrE` for three of its figures and writes networks to `OutPut/Transmission/`, a folder that does not exist. Not part of the EVD17/Ituri pipeline. |

**Indicators.** Contacts per confirmed case; time from last exposure to first follow-up visit; visits
achieved versus expected; proportion of contacts followed at least once; weekly follow-up
percentages; "Suivi complété" (complete follow-up) versus "Perdus de vue" (lost to follow-up) and
their duration; contacts that later became suspect or confirmed cases; contacts never visited.
Mann–Kendall tests give the direction and significance of the 3-week trends.

**Filters.** Provinces and health zones are kept only if they have at least one confirmed case, and
the PPTX highlights the five most-affected health zones over the 3-week window.

**Inputs.** `load_latest_data()` on `data_cleaning_output` for `contact.long.clean_Int_*.rds`,
`contact.clean_Int*.rds` and `evd.cleaning/evd.clean_Int_*.rds`. Helpers: `paths.R`, `DebutSem.R`,
`AgeCat.R`, `AgeStd.R`, `OrthoCorrect.R`, `OrthoFix.R`, `LoadLatestData.R`, `clean_filename.R`,
`contact.cleaning.fx.R`; linkage uses `pct_distance.R` and `num_distance.R`.

**Outputs.**

| Script | Artefacts | Destination |
|---|---|---|
| `Contact_analysis.R` | `ContactFU_3w_indicateurs_<date>.xlsx` (sheets `Ensemble`, `Par_Province`, `Par_ZS`, `MK_Ensemble`, `MK_Par_Province`, `MK_Par_ZS`), `ContactFU_daily_pct_<date>.xlsx` | `OutPut/Contacts/<ddMon>/all/` |
| | `Indicateurs_3w_ensemble_`, `MK_3w_ensemble_` (`.html` via `save_kable`) | `all/` |
| | `Indicateurs_3w_prov_`, `MK_3w_prov_` | `prov/` |
| | `Indicateurs_3w_zs_`, `MK_3w_zs_`, `NContacts_tbl_zs_`, `Contacts_jamais_vus_tbl_zs_`, `ContactFU_tbl_zs_` | `zs/` |
| | `ContactFU_3w_trend_`, `ContactFU_3w_trend_prov_`, `ContactFU_3w_trend_zs_`, `Tendance_delai_Expo_Suivi_`, `ContactByCase_ts_`, `ContactFU_ts_`, `ContactFU_ts_daily_`, `Duree_suivi_perdu_vue_`, `Contacts_devenus_suspect_confirme_`, `Delai_Expo_Suivi_zs_`, `Contacts_symptomes_zs_` (`.png`) | by level |
| `Contact_3w_pptx.R` | `Indicateurs_3w_contacts_<ddMon>.pptx` | `OutPut/Contacts/` |
| `contact_ll_connect.R` | `contact_ll_connected_Int_<timestamp>.rds`, `infector_infectees_Int_<timestamp>.rds` | `../DataCleaning/data/Output/<dd_%B>/contact_ll_connect/` |

**Quarto report.** `rapport_contact_followup.qmd` — "Analyse du suivi des contacts — MVE/B, RDC",
French, `flatly` theme, self-contained HTML with folded code. It sources `Contact_analysis.R` with
`options(contact.analysis.report_mode = TRUE)`, which sets `skip_output` so the script computes
without writing files. Output: `rapport_contact_followup.html` (also copied to `OutPut/Contacts/`).

**Language.** French plots, tables and exports; `contact_ll_connect.R` is commented in English. Set
the locale carefully when sourcing in report mode — `load_latest_data()` parses `%d_%B` folder names
according to the active locale.

---

## Français

**Thématique.** La santé opérationnelle du traçage des contacts : les contacts sont-ils repérés vite,
visités aussi souvent que prévu, suivis jusqu'au bout de leur période de 21 jours, ou perdus de vue —
et comment cela évolue-t-il par province et par zone de santé sur les trois dernières semaines ? Un
second volet apparie les enregistrements de contacts aux enregistrements de cas pour reconstituer les
paires infecteur–infecté.

**Scripts.**

| Fichier | Objet |
|---|---|
| `Contact_analysis.R` | Moteur principal (~2 300 lignes). Calcule tous les indicateurs de suivi, tests de tendance et exports décrits ci-dessous. |
| `Contact_3w_pptx.R` | Produit une présentation **PowerPoint** native et éditable des indicateurs 3 semaines. Source `Contact_analysis.R` en mode rapport. |
| `contact_ll_connect.R` | Appariement probabiliste (flou) de la line list contacts à la line list cas EVD ; exporte les paires appariées et la table infecteur–infecté en RDS pour les autres thématiques. |
| `TransmissionChain.R` | **Hérité / exploratoire.** Réseaux `epicontacts` et estimations `EpiEstim`/`R0` sur une ancienne line list du Kasaï. Référence une variable `svfldrE` jamais définie pour trois figures et écrit les réseaux dans `OutPut/Transmission/`, dossier inexistant. Ne fait pas partie du pipeline EVD17/Ituri. |

**Indicateurs.** Contacts par cas confirmé ; délai entre la dernière exposition et la première visite
de suivi ; visites réalisées versus attendues ; proportion de contacts suivis au moins une fois ;
pourcentages hebdomadaires de suivi ; « Suivi complété » versus « Perdus de vue » et leur durée ;
contacts devenus ensuite cas suspects ou confirmés ; contacts jamais vus. Les tests de Mann–Kendall
donnent la direction et la signification des tendances sur 3 semaines.

**Filtres.** Provinces et zones de santé ne sont conservées que si elles comptent au moins un cas
confirmé, et le PPTX met en évidence les cinq zones les plus touchées sur la fenêtre de 3 semaines.

**Entrées.** `load_latest_data()` sur `data_cleaning_output` pour `contact.long.clean_Int_*.rds`,
`contact.clean_Int*.rds` et `evd.cleaning/evd.clean_Int_*.rds`. Fonctions d'appui : `paths.R`,
`DebutSem.R`, `AgeCat.R`, `AgeStd.R`, `OrthoCorrect.R`, `OrthoFix.R`, `LoadLatestData.R`,
`clean_filename.R`, `contact.cleaning.fx.R` ; l'appariement utilise `pct_distance.R` et
`num_distance.R`.

**Sorties.**

| Script | Artefacts | Destination |
|---|---|---|
| `Contact_analysis.R` | `ContactFU_3w_indicateurs_<date>.xlsx` (feuilles `Ensemble`, `Par_Province`, `Par_ZS`, `MK_Ensemble`, `MK_Par_Province`, `MK_Par_ZS`), `ContactFU_daily_pct_<date>.xlsx` | `OutPut/Contacts/<jjMon>/all/` |
| | `Indicateurs_3w_ensemble_`, `MK_3w_ensemble_` (`.html` via `save_kable`) | `all/` |
| | `Indicateurs_3w_prov_`, `MK_3w_prov_` | `prov/` |
| | `Indicateurs_3w_zs_`, `MK_3w_zs_`, `NContacts_tbl_zs_`, `Contacts_jamais_vus_tbl_zs_`, `ContactFU_tbl_zs_` | `zs/` |
| | `ContactFU_3w_trend_`, `ContactFU_3w_trend_prov_`, `ContactFU_3w_trend_zs_`, `Tendance_delai_Expo_Suivi_`, `ContactByCase_ts_`, `ContactFU_ts_`, `ContactFU_ts_daily_`, `Duree_suivi_perdu_vue_`, `Contacts_devenus_suspect_confirme_`, `Delai_Expo_Suivi_zs_`, `Contacts_symptomes_zs_` (`.png`) | par niveau |
| `Contact_3w_pptx.R` | `Indicateurs_3w_contacts_<jjMon>.pptx` | `OutPut/Contacts/` |
| `contact_ll_connect.R` | `contact_ll_connected_Int_<horodatage>.rds`, `infector_infectees_Int_<horodatage>.rds` | `../DataCleaning/data/Output/<jj_%B>/contact_ll_connect/` |

**Rapport Quarto.** `rapport_contact_followup.qmd` — « Analyse du suivi des contacts — MVE/B, RDC »,
en français, thème `flatly`, HTML autonome avec code replié. Il source `Contact_analysis.R` avec
`options(contact.analysis.report_mode = TRUE)`, ce qui active `skip_output` : le script calcule sans
écrire de fichier. Sortie : `rapport_contact_followup.html` (également copié dans
`OutPut/Contacts/`).

**Langue.** Graphiques, tableaux et exports en français ; `contact_ll_connect.R` est commenté en
anglais. Soignez la locale lors du sourçage en mode rapport : `load_latest_data()` interprète les noms
de dossiers `%d_%B` selon la locale active.
