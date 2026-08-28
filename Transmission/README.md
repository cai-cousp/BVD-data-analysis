# Transmission — Onset dates and incidence | Dates de début des symptômes et incidence

[English](#english) · [Français](#français)

---

## English

**Topic.** An exploratory study of transmission parameters, focused on the biggest obstacle to
estimating them: missing symptom-onset dates. Without an onset date a case cannot be placed on the
epidemic curve by day of infection, so the working question is whether the curve can be reconstructed
dependably.

**Script.** `incidence.R` (~190 lines).

**Method.**

- Loads the most recent processed snapshot by picking the file with the largest digit-stripped name
  (its own inline version of the loader, not `load_latest_data()`).
- Derives the onset-to-notification delay.
- Handles missing onset dates by **multiple imputation** with `mice` (predictive mean matching),
  then compares candidate datasets by signal-to-noise before choosing one for the curve.
- Loads the estimation toolkit for downstream work: `incidence2`, `epitools`, `epiR`, `EpiEstim`,
  `epimdr`.

**Inputs.** RDS files in `data_cleaning_processed` (`file_pattern = ".rds"`, latest by numeric key).
Helpers: `paths.R`, `DebutSem.R`, `AgeCat.R`, `AgeStd.R`.

**Outputs.** **None.** The script produces interactive plots in the session only — there is no
`ggsave()`, `write.xlsx()` or `saveRDS()` call anywhere in it. Nothing appears under `OutPut/`.

**Caveats.**

- This is a workbench, not a pipeline: results exist only as long as the session does. Save the
  objects explicitly if you need them.
- Imputation of onset dates rests on the missing-at-random assumption given the observed notification
  date and covariates. That assumption is not testable here, and systematic under-reporting of onset
  dates would bias any R0 or generation-interval estimate derived from the imputed curve.
- For current EVD17 transmission estimates, prefer the maintained code:
  [`../Alerts/R/04_nowcast.R`](../Alerts/README.md) and
  [`../scripts/Nowcasting.R`](../scripts/) (reporting-delay nowcasting with `epinowcast`/`EpiNow2`),
  and the association analysis in [`../evd_deaths/`](../evd_deaths/README.md). The interactive
  transmission-chain work in `Contacts.Analysis/TransmissionChain.R` is legacy.

**Language.** English comments. No Quarto report.

---

## Français

**Thématique.** Étude exploratoire des paramètres de transmission, centrée sur le principal obstacle à
leur estimation : les dates de début des symptômes manquantes. Sans date de début, un cas ne peut être
placé sur la courbe épidémique par date d'infection ; la question de travail est donc de savoir si cette
courbe peut être reconstruite de façon fiable.

**Script.** `incidence.R` (~190 lignes).

**Méthode.**

- Charge l'instantané traité le plus récent en sélectionnant le fichier au nom numérique le plus élevé
  (une version locale du chargeur, distincte de `load_latest_data()`).
- Dérive le délai entre début des symptômes et notification.
- Traite les dates de début manquantes par **imputation multiple** (`mice`, appariement prédictif des
  moyennes), puis compare les jeux de données candidats selon le rapport signal/bruit avant d'en retenir
  un pour la courbe.
- Charge la boîte à outils d'estimation pour les travaux ultérieurs : `incidence2`, `epitools`, `epiR`,
  `EpiEstim`, `epimdr`.

**Entrées.** Fichiers RDS de `data_cleaning_processed` (`file_pattern = ".rds"`, plus récent selon la clé
numérique). Fonctions d'appui : `paths.R`, `DebutSem.R`, `AgeCat.R`, `AgeStd.R`.

**Sorties.** **Aucune.** Le script ne produit que des graphiques interactifs en session — il n'y a ni
`ggsave()`, ni `write.xlsx()`, ni `saveRDS()` dans le fichier. Rien n'apparaît dans `OutPut/`.

**Points d'attention.**

- C'est un atelier, pas un pipeline : les résultats n'existent que le temps de la session. Sauvegarder
  explicitement les objets si nécessaire.
- L'imputation des dates de début suppose un mécanisme de données manquantes aléatoire conditionnellement
  à la date de notification observée et aux covariables. Cette hypothèse n'est pas vérifiable ici, et une
  sous-déclaration systématique des dates de début biaiserait toute estimation du R0 ou de l'intervalle de
  génération dérivée de la courbe imputée.
- Pour les estimations actuelles de la transmission EVD17, préférer le code maintenu :
  [`../Alerts/R/04_nowcast.R`](../Alerts/README.md) et
  [`../scripts/Nowcasting.R`](../scripts/) (nowcasting des délais de notification avec
  `epinowcast`/`EpiNow2`), ainsi que l'analyse des associations dans
  [`../evd_deaths/`](../evd_deaths/README.md). Les travaux interactifs sur les chaînes de transmission
  dans `Contacts.Analysis/TransmissionChain.R` sont hérités.

**Langue.** Commentaires en anglais. Pas de rapport Quarto.
