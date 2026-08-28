# Child.Analysis — Paediatric cases and contacts | Cas et contacts pédiatriques

[English](#english) · [Français](#français)

---

## English

**Topic.** Everything the response needs to know about children in this outbreak: who among
confirmed cases, valid alerts and contacts is under 5 or under 18, how fatal the disease is in those
groups, how well they are sampled and traced, and how infection moves between age groups.

**Script.** `Children_Analysis.R` (~1,700 lines). Run from the project root:
`Rscript Child.Analysis/Children_Analysis.R`.

**Indicators computed.**

- Age–sex pyramids of validated alerts and of confirmed cases (<5 and <18), overall, per province
  and per health zone.
- CFR by age group, and the 3-week trend of both CFR and the proportion of deaths by age.
- Sampling coverage and positivity by age group.
- Symptom-onset to notification delay by age.
- Infector–infectee and contact→case age matrices, restricted to confirmed pairs where applicable.
- Secondary attack rate (SAR) by age group, plus donut charts of contact circumstance, contact type
  and relationship to the case.
- Table of health zones with confirmed cases under 5 alongside their alert counts.

**Inputs.** All snapshots are resolved by `load_latest_data()` from `data_cleaning_output`, using
`file_pattern` to select each dataset: `evd.clean_Int_*.rds`, `contact.clean_Int*.rds`,
`contact.long.clean_Int_*.rds`, `contact_ll_connected_Int_*.rds` (the last two produced by
[`../Contacts.Analysis/contact_ll_connect.R`](../Contacts.Analysis/README.md)) and
`infector_infectees_Int_*.rds`. Population denominator: `data/PopulationParAge/pop_zs_drc.csv`.
Helpers: `paths.R`, `DebutSem.R`, `AgeCat.R`, `AgeStd.R`, `OrthoCorrect.R`, `OrthoFix.R`,
`LoadLatestData.R`, `clean_filename.R`, `contact.cleaning.fx.R`.

**Outputs.** All under `OutPut/Children/<ddMon>/` (`all/`, `prov/`, `zs/`), date-stamped with the
latest notification date:

| Artefact | Location |
|---|---|
| `Pyramide_AlertesValidees_`, `Pyramide_CasConfirmes_`, `Proportion_CasDeces_Age_`, `CFR_Age_`, `CFR_Tendances_3Semaines_`, `ProportionDeces_3Semaines_Age_`, `Echantillonnage_Positivite_Age_`, `Delai_Symptomes_Notification_Age_`, `Matrice_InfecteurInfecte_` (+ `_Confirme_`), `Donut_Circonstances_Contact_`, `Donut_Type_Contact_`, `Donut_Relation_Contact_`, `SAR_Age_`, `Matrice_ContactCas_` (+ `_Confirme_`) — all `.png` | `all/` |
| `Pyramide_Prov_<province>_<ddMon>.png` | `prov/` |
| `Pyramide_ZS_<zone>_<ddMon>.png`, and `ZS_ConfirmedUnder5_Alerts_<ddMon>` as `.html`, `.png`, `.xlsx` | `zs/` |
| `AgeSex_ConfirmedCases_<ddMon>.xlsx` | `OutPut/Agesex/` |

**Quarto report.** `OutPut/Children/rapport_children_analysis.qmd` sources this script with
`options(children.analysis.report_mode = TRUE)`, which sets the internal `skip_output` flag so no
file is written and every object in the script feeds the document instead. Render with
`quarto render OutPut/Children/rapport_children_analysis.qmd`.

**Language.** English comments, French figure titles and table headers.

---

## Français

**Thématique.** Tout ce que la riposte doit savoir sur les enfants dans cette flambée : quelle part
des cas confirmés, des alertes validées et des contacts a moins de 5 ans ou moins de 18 ans, quelle
est la gravité de la maladie dans ces groupes, comment ils sont dépistés et suivis, et comment
l'infection circule entre classes d'âge.

**Script.** `Children_Analysis.R` (~1 700 lignes). Depuis la racine du projet :
`Rscript Child.Analysis/Children_Analysis.R`.

**Indicateurs calculés.**

- Pyramides des âges et des sexes des alertes validées et des cas confirmés (<5 et <18), au niveau
  global, par province et par zone de santé.
- Létalité par classe d'âge, et tendance sur 3 semaines de la létalité et de la proportion de décès
  par âge.
- Couverture de l'échantillonnage et positivité par classe d'âge.
- Délai entre début des symptômes et notification, par âge.
- Matrices infecteur–infecté et contact→cas par âge, restreintes aux paires confirmées lorsque
  pertinent.
- Taux d'attaque secondaire (TAS) par classe d'âge, avec diagrammes circulaires des circonstances de
  contact, du type de contact et de la relation avec le cas.
- Tableau des zones de santé ayant des cas confirmés de moins de 5 ans, avec le nombre d'alertes
  associées.

**Entrées.** Tous les instantanés sont résolus par `load_latest_data()` depuis
`data_cleaning_output`, avec un `file_pattern` par jeu de données : `evd.clean_Int_*.rds`,
`contact.clean_Int*.rds`, `contact.long.clean_Int_*.rds`, `contact_ll_connected_Int_*.rds` (les deux
derniers produits par [`../Contacts.Analysis/contact_ll_connect.R`](../Contacts.Analysis/README.md))
et `infector_infectees_Int_*.rds`. Dénominateur de population :
`data/PopulationParAge/pop_zs_drc.csv`. Fonctions d'appui : `paths.R`, `DebutSem.R`, `AgeCat.R`,
`AgeStd.R`, `OrthoCorrect.R`, `OrthoFix.R`, `LoadLatestData.R`, `clean_filename.R`,
`contact.cleaning.fx.R`.

**Sorties.** Sous `OutPut/Children/<jjMon>/` (`all/`, `prov/`, `zs/`), datées de la dernière
notification des données :

| Artefact | Emplacement |
|---|---|
| `Pyramide_AlertesValidees_`, `Pyramide_CasConfirmes_`, `Proportion_CasDeces_Age_`, `CFR_Age_`, `CFR_Tendances_3Semaines_`, `ProportionDeces_3Semaines_Age_`, `Echantillonnage_Positivite_Age_`, `Delai_Symptomes_Notification_Age_`, `Matrice_InfecteurInfecte_` (+ `_Confirme_`), `Donut_Circonstances_Contact_`, `Donut_Type_Contact_`, `Donut_Relation_Contact_`, `SAR_Age_`, `Matrice_ContactCas_` (+ `_Confirme_`) — en `.png` | `all/` |
| `Pyramide_Prov_<province>_<jjMon>.png` | `prov/` |
| `Pyramide_ZS_<zone>_<jjMon>.png`, ainsi que `ZS_ConfirmedUnder5_Alerts_<jjMon>` en `.html`, `.png` et `.xlsx` | `zs/` |
| `AgeSex_ConfirmedCases_<jjMon>.xlsx` | `OutPut/Agesex/` |

**Rapport Quarto.** `OutPut/Children/rapport_children_analysis.qmd` source ce script avec
`options(children.analysis.report_mode = TRUE)`, ce qui active l'indicateur interne `skip_output` :
aucun fichier n'est écrit et tous les objets alimentent le document. Rendu :
`quarto render OutPut/Children/rapport_children_analysis.qmd`.

**Langue.** Commentaires en anglais, titres de figures et en-têtes de tableaux en français.
