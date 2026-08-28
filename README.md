# EVD17 / MVE Ituri — Data Analysis

Analyses épidémiologiques et laboratoires pour la riposte à la 17<sup>e</sup> flambée de maladie à
virus Ebola (MVE17, virus Bundibugyo) en République démocratique du Congo — Ituri et Nord-Kivu.

Epidemiological and laboratory analyses supporting the response to the 17th Ebola virus disease
outbreak (EVD17, Bundibugyo virus) in the Democratic Republic of the Congo — Ituri and Nord-Kivu
provinces.

---

## English

### What this project is

`DataAnalysis` is the analytic layer of the EVD17 response. It consumes cleaned line lists produced
by the sibling **`DataCleaning`** project and produces the figures, tables, workbooks, PowerPoint
decks and HTML reports used by the surveillance team. Each analytic topic lives in its own folder
and is driven by one main R script; there is no single orchestrating entry point for the whole
project.

### Prerequisites

- **R ≥ 4.1** (R 4.5 used at the time of writing)
- **`here`** — every script resolves paths from the project root. A zero-byte `.here` file marks
  the root; do not move or delete it.
- **`pacman`** — most scripts self-install their dependency set with
  `pacman::p_load(..., install = TRUE)` on first run.
- **Quarto CLI ≥ 1.3** — required only to render the `.qmd` reports.
- **`cmdstanr` + CmdStan** — required only for the nowcasting scripts
  (`scripts/Nowcasting.R`, `Alerts/R/04_nowcast.R`) and the Bayesian sensitivity model in
  `evd_deaths/`.

### External dependencies

Paths are centralised in [`helpers/paths.R`](helpers/paths.R), which defines four roots and checks
each one with `assert_dir()`, so a missing directory fails loudly at load time:

| Variable | Resolves to | Content |
|---|---|---|
| `data_cleaning_output` | `../DataCleaning/data/Output` | dated `%d_%B` folders of cleaned snapshots (`evd.cleaning/`, `contact.cleaning/`, `lab.cleaning/`) |
| `data_cleaning_processed` | `../DataCleaning/data/processed` | fully processed single-file RDS snapshots |
| `evd17_maps` | `../Maps` | GRID3 health zone / health area layers, admin boundaries |
| `mve_ituri_maps` | `../../Maps` | broader DRC map library (OSM GeoJSON) |

`helpers/LoadLatestData.R` provides `load_latest_data()`, the shared snapshot picker: it descends
into the most recent `%d_%B` folder and returns the highest-numbered file matching a pattern. Almost
every script in this project loads its data this way, which is why outputs are always tied to the
latest cleaning run rather than a hard-coded file.

### Folder map

| Folder | Topic | Main entry point(s) | Writes to |
|---|---|---|---|
| [`AgeSex/`](AgeSex/README.md) | Age–sex pyramid, sex ratio, age-specific risk of death | `AgeSex.R` | `OutPut/Agesex/` |
| [`Child.Analysis/`](Child.Analysis/README.md) | Paediatric (<5 / <18) cases and contacts, CFR and SAR by age | `Children_Analysis.R` | `OutPut/Children/` |
| [`Contacts.Analysis/`](Contacts.Analysis/README.md) | Contact-tracing performance, 3-week indicators, contact↔case linkage | `Contact_analysis.R`, `Contact_3w_pptx.R` | `OutPut/Contacts/` |
| [`Epi.Curves/`](Epi.Curves/README.md) | Epicurves by outcome status with weekly CFR overlay | `EpiCurves.R` | `OutPut/EpiCurves/` |
| [`Epi.Maps/`](Epi.Maps/README.md) | Choropleth maps: HZ/AS progression and newly-affected areas | `EpiMaps.R` | `OutPut/Epimaps/` |
| [`Lab.Analysis/`](Lab.Analysis/README.md) | Lab cascade, positivity, turnaround times, spatial prioritisation | `lab.analysis.R`, `sample_positivity.R` | `OutPut/Lab/` |
| [`Risk.Factors/`](Risk.Factors/README.md) | Exposure and risk-factor tables — **legacy**, not EVD17 data | `RiskFactors.r` | `OutPut/` |
| [`Spread.Risk/`](Spread.Risk/README.md) | Geographic spread risk from population mobility flows | `population.relocation.R` | `OutPut/RiskAssessment/` |
| [`Symptoms.Analysis/`](Symptoms.Analysis/README.md) | Symptom frequency among lab-positive cases | `CaseDefinition.R` | `OutPut/symptoms/` |
| [`Transmission/`](Transmission/README.md) | Onset-date imputation and incidence exploration — **interactive only** | `incidence.R` | (none) |
| [`Alerts/`](Alerts/README.md) | Expected alert thresholds, adequacy trends, mapping capacity, nowcast | `R/01_…`–`R/04_…` | `Alerts/output/` |
| [`evd_deaths/`](evd_deaths/README.md) | CFR descriptives, adjusted associations, death prediction | `R/run_all.R` | `OutPut/Deaths/` |
| [`helpers/`](helpers/) | Shared functions: paths, loaders, age/date, name correction, lab metrics | — | — |
| [`data/`](data/) | Static reference data: population, accessibility, mobility, Beni alerts | — | — |
| [`tests/`](tests/) | Root-level test scripts and `testthat` suite for the lab helpers | — | — |
| [`scripts/`](scripts/) | Nowcasting and DHIS2 QC utilities; older duplicates | `Nowcasting.R` | `OutPut/nowcast/` |
| [`dhis2r/`](dhis2r/README.md) | Local in-development R client for the DHIS2 REST API (own package) | — | — |
| `OutPut/` | All generated artefacts, grouped by topic and snapshot date | — | — |

`Alerts/`, `Lab.Analysis/`, `evd_deaths/` and `dhis2r/` already carried their own README files; they
are left as-is and are more detailed than the rest.

### Output conventions

Figures and tables land in `OutPut/<Topic>/<ddMon>/` — the date is taken from the **latest
notification in the data**, not the system clock — with one sub-folder per administrative level:

```
OutPut/EpiCurves/23Aug/
├── all/     national
├── prov/    province
├── zs/      health zone (zone de santé)
└── as/      health area (aire de santé)
```

File names are date-stamped (`Epicurve.all.status.Daily_23Aug.png`), and human-facing content — plot
titles, table headers, captions, reports — is in **French**, with an `IOA – CAI ©` caption. Code
comments are in English.

### Rendering a report instead of a file dump

Three scripts double as report engines. When sourced by a Quarto document they skip every `ggsave()`
and workbook write and leave the objects in the global environment:

```r
options(contact.analysis.report_mode = TRUE)   # Contacts.Analysis/Contact_analysis.R
options(children.analysis.report_mode = TRUE)  # Child.Analysis/Children_Analysis.R
options(lab.analysis.report_mode = TRUE)       # Lab.Analysis/lab.analysis.R
```

The Quarto sources in this project are `Contacts.Analysis/rapport_contact_followup.qmd`,
`Lab.Analysis/rapport_echantillons_positivite.qmd`, `OutPut/Children/rapport_children_analysis.qmd`
and `evd_deaths/docs/report/rapport_deces.qmd`. Render with
`quarto render <file>.qmd`.

### Testing

`tests/` holds both standalone scripts (`test_lab_analysis_pipeline.R`, `test_sample_positivity.R`,
`test_weekly_aggregate.R`, `test_load_latest_data.R`, `test_contact_indicators.R`,
`test_lab_indicators.R`) and a `testthat` suite for the lab helpers and the data loader. Run a
standalone script with `Rscript tests/test_lab_indicators.R`, or the suite with
`testthat::test_dir("tests/testthat")`. Coverage is concentrated on the lab pipeline; the
epi-curve, map and contact scripts are untested. `Alerts/` and `evd_deaths/` maintain their own test
suites.

### Caveats worth knowing

- **`scripts/Contact_analysis.R` is a near-duplicate** (103 KB) of
  `Contacts.Analysis/Contact_analysis.R` (105 KB). Treat `Contacts.Analysis/` as canonical.
- **`Risk.Factors/RiskFactors.r` is not part of the EVD17 pipeline.** It globs a `Mpox.Rproj` folder
  outside this project for its helpers and reads a **Kasai-2025** EVD line list.
- **`Symptoms.Analysis/CaseDefinition.R` pins one snapshot**
  (`evd.clean_Int_2026_06_05_1003.rds`) instead of calling `load_latest_data()`, so its figures do
  not refresh automatically.
- **`Contacts.Analysis/TransmissionChain.R` and `Epi.Maps/Maps.R` are legacy/exploratory.** The
  former references an undefined `svfldrE` and targets a non-existent `OutPut/Transmission/`; the
  latter still contains Mpox mapping examples and writes nothing.
- **`Lab.Analysis/*.R.new` are staging leftovers** (git-ignored). The live implementations of those
  helpers are in `helpers/`.
- Most data is git-ignored (`data/raw/`, `*.csv`, `*.xlsx`, rendered HTML) because it is sensitive
  outbreak data. Expect to supply it locally.

---

## Français

### Objet du projet

`DataAnalysis` constitue le niveau analytique de la riposte EVD17. Il consomme les line lists
nettoyées produites par le projet jumeau **`DataCleaning`** et génère les graphiques, tableaux,
classeurs Excel, présentations PowerPoint et rapports HTML utilisés par l'équipe de surveillance.
Chaque thématique d'analyse occupe son propre dossier et est pilotée par un script R principal ; il
n'existe pas de point d'entrée unique pour l'ensemble du projet.

### Prérequis

- **R ≥ 4.1** (R 4.5 utilisé à la rédaction)
- **`here`** — tous les scripts résolvent les chemins depuis la racine du projet, marquée par le
  fichier vide `.here` ; ne pas le déplacer ni le supprimer.
- **`pacman`** — la plupart des scripts installent automatiquement leurs dépendances au premier
  lancement via `pacman::p_load(..., install = TRUE)`.
- **Quarto CLI ≥ 1.3** — nécessaire uniquement pour le rendu des rapports `.qmd`.
- **`cmdstanr` + CmdStan** — nécessaire pour les scripts de nowcasting (`scripts/Nowcasting.R`,
  `Alerts/R/04_nowcast.R`) et le modèle bayésien de sensibilité de `evd_deaths/`.

### Dépendances externes

Les chemins sont centralisés dans [`helpers/paths.R`](helpers/paths.R), qui définit quatre racines
et vérifie chacune avec `assert_dir()` : un dossier manquant provoque donc une erreur explicite au
chargement.

| Variable | Résout vers | Contenu |
|---|---|---|
| `data_cleaning_output` | `../DataCleaning/data/Output` | dossiers datés `%d_%B` des instantanés nettoyés (`evd.cleaning/`, `contact.cleaning/`, `lab.cleaning/`) |
| `data_cleaning_processed` | `../DataCleaning/data/processed` | instantanés RDS entièrement traités, en fichier unique |
| `evd17_maps` | `../Maps` | couches GRID3 zones et aires de santé, limites administratives |
| `mve_ituri_maps` | `../../Maps` | bibliothèque cartographique RDC plus large (GeoJSON OSM) |

`helpers/LoadLatestData.R` fournit `load_latest_data()`, le sélecteur d'instantané partagé : il
descend dans le dossier `%d_%B` le plus récent et renvoie le fichier au numéro le plus élevé
correspondant au motif demandé. Presque tous les scripts du projet chargent leurs données ainsi, ce
qui explique que les résultats soient toujours liés à la dernière opération de nettoyage plutôt qu'à
un fichier figé.

### Cartographie des dossiers

| Dossier | Thématique | Point d'entrée principal | Écrit dans |
|---|---|---|---|
| [`AgeSex/`](AgeSex/README.md) | Pyramide âge–sexe, sex-ratio, risque de décès par âge | `AgeSex.R` | `OutPut/Agesex/` |
| [`Child.Analysis/`](Child.Analysis/README.md) | Cas et contacts pédiatriques (<5 / <18), létalité et TAS par âge | `Children_Analysis.R` | `OutPut/Children/` |
| [`Contacts.Analysis/`](Contacts.Analysis/README.md) | Performance du suivi des contacts, indicateurs 3 semaines, appariement contacts↔cas | `Contact_analysis.R`, `Contact_3w_pptx.R` | `OutPut/Contacts/` |
| [`Epi.Curves/`](Epi.Curves/README.md) | Courbes épidémiques par issue avec létalité hebdomadaire | `EpiCurves.R` | `OutPut/EpiCurves/` |
| [`Epi.Maps/`](Epi.Maps/README.md) | Cartes choroplèthes : progression et nouvelles zones touchées | `EpiMaps.R` | `OutPut/Epimaps/` |
| [`Lab.Analysis/`](Lab.Analysis/README.md) | Cascade laboratoire, positivité, délais de rendu, priorisation spatiale | `lab.analysis.R`, `sample_positivity.R` | `OutPut/Lab/` |
| [`Risk.Factors/`](Risk.Factors/README.md) | Tableaux d'exposition et de facteurs de risque — **hérité**, données hors EVD17 | `RiskFactors.r` | `OutPut/` |
| [`Spread.Risk/`](Spread.Risk/README.md) | Risque de diffusion géographique via les flux de population | `population.relocation.R` | `OutPut/RiskAssessment/` |
| [`Symptoms.Analysis/`](Symptoms.Analysis/README.md) | Fréquence des signes cliniques chez les cas positifs | `CaseDefinition.R` | `OutPut/symptoms/` |
| [`Transmission/`](Transmission/README.md) | Imputation des dates de début des symptômes — **interactif uniquement** | `incidence.R` | (aucun) |
| [`Alerts/`](Alerts/README.md) | Seuils d'alertes attendues, tendances d'adéquation, capacités, nowcast | `R/01_…`–`R/04_…` | `Alerts/output/` |
| [`evd_deaths/`](evd_deaths/README.md) | Létalité descriptive, associations ajustées, prédiction des décès | `R/run_all.R` | `OutPut/Deaths/` |
| [`helpers/`](helpers/) | Fonctions partagées : chemins, chargeurs, âge/date, correction de noms, indicateurs labo | — | — |
| [`data/`](data/) | Données de référence statiques : population, accessibilité, mobilité, alertes de Beni | — | — |
| [`tests/`](tests/) | Scripts de test racine et suite `testthat` des fonctions laboratoire | — | — |
| [`scripts/`](scripts/) | Nowcasting et utilitaires de contrôle DHIS2 ; anciennes copies | `Nowcasting.R` | `OutPut/nowcast/` |
| [`dhis2r/`](dhis2r/README.md) | Client R local (en développement) pour l'API REST DHIS2 | — | — |
| `OutPut/` | Tous les artefacts générés, classés par thématique et par date d'instantané | — | — |

Les dossiers `Alerts/`, `Lab.Analysis/`, `evd_deaths/` et `dhis2r/` possédaient déjà un README ; ils
n'ont pas été modifiés et sont plus détaillés que les autres.

### Conventions de sortie

Graphiques et tableaux sont écrits dans `OutPut/<Thématique>/<jjMon>` — la date provient de la
**dernière notification présente dans les données**, et non de l'horloge système — avec un
sous-dossier par niveau administratif :

```
OutPut/EpiCurves/23Aug/
├── all/     niveau national
├── prov/    province
├── zs/      zone de santé
└── as/      aire de santé
```

Les noms de fichiers portent la date (`Epicurve.all.status.Daily_23Aug.png`). Le contenu destiné aux
utilisateurs — titres, en-têtes de tableaux, légendes, rapports — est en **français**, avec la
mention `IOA – CAI ©`. Les commentaires de code sont en anglais.

### Rendre un rapport plutôt qu'une production de fichiers

Trois scripts servent aussi de moteur de rapport. Sourcés par un document Quarto, ils ignorent tous
les `ggsave()` et écritures de classeur, et laissent les objets dans l'environnement global :

```r
options(contact.analysis.report_mode = TRUE)   # Contacts.Analysis/Contact_analysis.R
options(children.analysis.report_mode = TRUE)  # Child.Analysis/Children_Analysis.R
options(lab.analysis.report_mode = TRUE)       # Lab.Analysis/lab.analysis.R
```

Les sources Quarto du projet sont `Contacts.Analysis/rapport_contact_followup.qmd`,
`Lab.Analysis/rapport_echantillons_positivite.qmd`,
`OutPut/Children/rapport_children_analysis.qmd` et `evd_deaths/docs/report/rapport_deces.qmd`.
Rendu : `quarto render <fichier>.qmd`.

### Tests

`tests/` contient à la fois des scripts autonomes (`test_lab_analysis_pipeline.R`,
`test_sample_positivity.R`, `test_weekly_aggregate.R`, `test_load_latest_data.R`,
`test_contact_indicators.R`, `test_lab_indicators.R`) et une suite `testthat` couvrant les fonctions
laboratoire et le chargeur d'instantanés. Un script autonome se lance avec
`Rscript tests/test_lab_indicators.R`, la suite avec `testthat::test_dir("tests/testthat")`.
La couverture se concentre sur le pipeline laboratoire ; les scripts de courbes épidémiques,
cartographiques et de contacts ne sont pas testés. `Alerts/` et `evd_deaths/` disposent de leur
propre suite.

### Points d'attention

- **`scripts/Contact_analysis.R` est un quasi-doublon** (103 Ko) de
  `Contacts.Analysis/Contact_analysis.R` (105 Ko). Le dossier `Contacts.Analysis/` fait foi.
- **`Risk.Factors/RiskFactors.r` ne fait pas partie du pipeline EVD17.** Il cherche un dossier
  `Mpox.Rproj` hors du projet pour ses fonctions, et lit une line list EVD **Kasaï 2025**.
- **`Symptoms.Analysis/CaseDefinition.R` fige un instantané**
  (`evd.clean_Int_2026_06_05_1003.rds`) au lieu d'appeler `load_latest_data()` : ses figures ne se
  rafraîchissent donc pas automatiquement.
- **`Contacts.Analysis/TransmissionChain.R` et `Epi.Maps/Maps.R` sont hérités/expploratoires.** Le
  premier référence une variable `svfldrE` jamais définie et cible un `OutPut/Transmission/`
  inexistant ; le second contient encore des exemples Mpox et n'écrit aucun fichier.
- **Les `Lab.Analysis/*.R.new` sont des résidus de copie** (ignorés par git). Les implémentations
  actives de ces fonctions sont dans `helpers/`.
- La plupart des données sont exclues du dépôt (`data/raw/`, `*.csv`, `*.xlsx`, HTML rendus) car il
  s'agit de données de surveillance sensibles ; elles doivent être fournies localement.
