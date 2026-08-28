# AgeSex — Age and sex distribution | Distribution par âge et sexe

[English](#english) · [Français](#français)

---

## English

**Topic.** Describes the age and sex structure of confirmed EVD17 cases and of deaths among them,
against population denominators, and quantifies how the risk of death varies by age group.

**Script.** `AgeSex.R` — the only file in this folder; run it from the project root with
`Rscript AgeSex/AgeSex.R`.

What it produces:

- Donut chart of the sex distribution among confirmed cases.
- Age–sex pyramid of confirmed cases, split by outcome (`Vivant` / `Décédé`), with an estimated
  population overlay so the shape can be read against the underlying demographic structure.
- Same pyramid at province level and for the four most-affected health zones.
- Male/female case ratio by health zone.
- Risk ratio of death per age group, using the lowest-CFR group as reference (`chisq.test` plus
  `epiR::epi.2by2`).

**Inputs.** Loads the latest `evd.cleaning` RDS snapshot through `load_latest_data()`
(`data_cleaning_output`), keeping cases where `classification_finale_cas == "Cas confirmé"` or
`lab_resultat_final == "Positif"`. Population denominators come from
`data/PopulationParAge/pop_zs_drc.csv` and `pop_prov_drc.csv`. Helper functions: `helpers/paths.R`,
`DebutSem.R`, `AgeCat.R`, `AgeStd.R`, `LoadLatestData.R`.

**Outputs.** `ggsave()` PNGs into `OutPut/Agesex/<ddMon>/`, split by level (`all/`, `prov/`, `zs/`,
`as/`), where `<ddMon>` is the latest notification date in the data:

| File | Level |
|---|---|
| `Distr.sex.donut_<ddMon>.png` | `all/` |
| `AgeSexBVD_<ddMon>.all_<ddMon>.png` | `all/` |
| `AgeSexBVD.ratio_<ddMon>.png` | `all/` |
| `AgeS.RiskRatio_<ddMon>.png` | `all/` |
| `AgeSexBVD.prov_<ddMon>.png` | `prov/` |
| `AgeSexBVD.zs_<ddMon>.png` | `zs/` |

Note: `OutPut/Agesex/AgeSex_ConfirmedCases_<ddMon>.xlsx` is written by
[`../Child.Analysis/Children_Analysis.R`](../Child.Analysis/Children_Analysis.R), not by this
script.

**Language.** English comments, French plot titles and captions. No Quarto report.

**Related.** The paediatric breakdown of the same distribution lives in
[`../Child.Analysis/`](../Child.Analysis/README.md); deaths among confirmed cases are analysed in
depth in [`../evd_deaths/`](../evd_deaths/README.md).

---

## Français

**Thématique.** Décrit la structure par âge et par sexe des cas confirmés de MVE17 et des décès
qu'ils ont entraînés, rapportée aux effectifs de population, et quantifie la variation du risque de
décès selon l'âge.

**Script.** `AgeSex.R` — seul fichier du dossier ; à lancer depuis la racine du projet avec
`Rscript AgeSex/AgeSex.R`.

Productions :

- Diagramme circulaire de la répartition par sexe des cas confirmés.
- Pyramide des âges et des sexes des cas confirmés, séparée selon l'issue (`Vivant` / `Décédé`),
  avec superposition de la population estimée afin de lire la forme par rapport à la structure
  démographique sous-jacente.
- Même pyramide au niveau provincial et pour les quatre zones de santé les plus touchées.
- Ratio hommes/femmes de cas par zone de santé.
- Risque relatif de décès par classe d'âge, la référence étant la classe à la létalité la plus
  faible (`chisq.test` et `epiR::epi.2by2`).

**Entrées.** Charge l'instantané RDS `evd.cleaning` le plus récent via `load_latest_data()`
(`data_cleaning_output`), en conservant les cas où `classification_finale_cas == "Cas confirmé"` ou
`lab_resultat_final == "Positif"`. Les dénominateurs de population proviennent de
`data/PopulationParAge/pop_zs_drc.csv` et `pop_prov_drc.csv`. Fonctions d'appui :
`helpers/paths.R`, `DebutSem.R`, `AgeCat.R`, `AgeStd.R`, `LoadLatestData.R`.

**Sorties.** Fichiers PNG (`ggsave()`) dans `OutPut/Agesex/<jjMon>/`, classés par niveau (`all/`,
`prov/`, `zs/`, `as/`), `<jjMon>` étant la date de dernière notification des données :

| Fichier | Niveau |
|---|---|
| `Distr.sex.donut_<jjMon>.png` | `all/` |
| `AgeSexBVD_<jjMon>.all_<jjMon>.png` | `all/` |
| `AgeSexBVD.ratio_<jjMon>.png` | `all/` |
| `AgeS.RiskRatio_<jjMon>.png` | `all/` |
| `AgeSexBVD.prov_<jjMon>.png` | `prov/` |
| `AgeSexBVD.zs_<jjMon>.png` | `zs/` |

Remarque : `OutPut/Agesex/AgeSex_ConfirmedCases_<jjMon>.xlsx` est écrit par
[`../Child.Analysis/Children_Analysis.R`](../Child.Analysis/Children_Analysis.R), et non par ce
script.

**Langue.** Commentaires en anglais, titres et légendes des graphiques en français. Pas de rapport
Quarto.

**Voir aussi.** Le découpage pédiatrique de cette même distribution se trouve dans
[`../Child.Analysis/`](../Child.Analysis/README.md) ; les décès parmi les cas confirmés sont analysés
en détail dans [`../evd_deaths/`](../evd_deaths/README.md).
