# Risk.Factors — Exposure and risk factors | Exposition et facteurs de risque

> **Status: legacy.** This script is not part of the EVD17 / Ituri pipeline. See
> [Caveats](#caveats--points-dattention) below.
>
> **Statut : hérité.** Ce script ne fait pas partie du pipeline EVD17 / Ituri. Voir
> [Points d'attention](#caveats--points-dattention).

[English](#english) · [Français](#français)

---

## English

**Topic.** Risk factors for death among EVD cases: how outcomes differ by exposure setting
(community transmission versus care setting), with unadjusted tables and a mosaic/alluvial
presentation of the exposure pathways.

**Script.** `RiskFactors.r` (~930 lines). Package set is much wider than the rest of the project —
`gtsummary`, `epitools`, `EpiStats`, `twoxtwo`, `ggalluvial`, `ggmosaic`, `tidygeocoder`,
`googledrive`/`googlesheets4`.

**Inputs and caveats.**

- **Does not source [`../helpers/paths.R`](../helpers/paths.R).** Instead it searches the home folder
  recursively for a `Mpox.Rproj` file and sources `CleaningFunctions.R` from wherever it finds it, so
  behaviour depends on files outside this project.
- The analysis dataset is `EVD_Kasai_CleanedLL_*.xlsx` — a **Kasaï** EVD line list, not the EVD17
  Ituri/Nord-Kivu data used everywhere else.
- Reference files `HealthPyramidDHIS2_Full_2025.xlsx` and `DPS_ZS_for_EpiNum.xlsx` are likewise
  located by glob under that external folder.
- Output folder is set with `svfldrE = here::here("OutPut")`, i.e. directly into `OutPut/` rather than
  a dated topic sub-folder.

**Outputs.** Verified in the code: `nHealthAreasBulape.xlsx` written at the **project root**, and one
figure `OutPut/evdCFRRiskFactorASW_EW<iso-week>.png` (risk factors for CFR by age/sex, by
epidemiological week). No dated sub-folder, no Quarto report.

**Recommendation.** Read it as a worked example of a CFR risk-factor analysis from an earlier outbreak
response. If this line of analysis is needed for EVD17, it should be rebuilt against
`load_latest_data()` — the association analysis for EVD17 deaths already exists in
[`../evd_deaths/`](../evd_deaths/README.md) (`R/03_deaths_associations.R`), which is the better
starting point.

---

## Français

**Thématique.** Facteurs de risque de décès chez les cas de MVE : comment l'issue varie selon le lieu
d'exposition (transmission communautaire versus milieu de soins), avec des tableaux non ajustés et une
présentation en mosaïque/alluviale des voies d'exposition.

**Script.** `RiskFactors.r` (~930 lignes). La liste de paquets est beaucoup plus large que le reste du
projet : `gtsummary`, `epitools`, `EpiStats`, `twoxtwo`, `ggalluvial`, `ggmosaic`, `tidygeocoder`,
`googledrive`/`googlesheets4`.

**Entrées et points d'attention.**

- **Ne source pas [`../helpers/paths.R`](../helpers/paths.R).** Le script cherche récursivement un
  fichier `Mpox.Rproj` dans le dossier personnel et y source `CleaningFunctions.R` : son comportement
  dépend donc de fichiers extérieurs au projet.
- Le jeu de données analysé est `EVD_Kasai_CleanedLL_*.xlsx`, une line list EVD du **Kasaï**, et non
  les données EVD17 d'Ituri/Nord-Kivu utilisées partout ailleurs.
- Les fichiers de référence `HealthPyramidDHIS2_Full_2025.xlsx` et `DPS_ZS_for_EpiNum.xlsx` sont
  également localisés par recherche sous ce dossier externe.
- Le dossier de sortie est défini par `svfldrE = here::here("OutPut")`, c'est-à-dire directement dans
  `OutPut/` et non dans un sous-dossier thématique daté.

**Sorties.** Vérifiées dans le code : `nHealthAreasBulape.xlsx` écrit à la **racine du projet**, et une
figure `OutPut/evdCFRRiskFactorASW_EW<semaine ISO>.png` (facteurs de risque de létalité par âge/sexe,
par semaine épidémiologique). Pas de sous-dossier daté, pas de rapport Quarto.

**Recommandation.** À lire comme un exemple abouti d'analyse des facteurs de risque de létalité, issu
d'une riposte antérieure. Si cette analyse est nécessaire pour EVD17, elle doit être reconstruite sur
`load_latest_data()` — l'analyse des associations pour les décès EVD17 existe déjà dans
[`../evd_deaths/`](../evd_deaths/README.md) (`R/03_deaths_associations.R`), meilleur point de départ.
