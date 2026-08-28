# Symptoms.Analysis — Clinical presentation | Présentation clinique

[English](#english) · [Français](#français)

---

## English

**Topic.** How frequently each clinical sign is reported among laboratory-confirmed EVD17 cases, and
how that case mix varies by age group.

**Script.** `CaseDefinition.R` (~350 lines).

**Method.**

- Keeps only `lab_resultat_final == "Positif"`.
- Standardises age with `AgeStd(age_ans, age_mois)` and cuts it into 12 groups with
  `AgeCat(AgeStandard, 12)`.
- Works over the clinical-sign block `s2_fievre` … `s2_autres_signes_hemorragiques`,
  `s2_autres_signes_cliniques_non_hemorragiques`. `"Ne sait pas"` is recoded to `NA` so that
  unknown-status responses are excluded from the denominator rather than counted as absent signs —
  otherwise every frequency would be biased downward.
- Each sign is compared against `"Oui"`, then averaged per case to give a prevalence; a per-case count
  of signs reported (`n.sympts`) is also derived.

**Inputs.** Reads one **hard-coded snapshot** rather than calling `load_latest_data()`:

```r
readRDS(file.path(data_cleaning_processed, "evd.clean_Int_2026_06_05_1003.rds"))
```

Helpers: `paths.R`, `AgeCat.R`, `AgeStd.R`.

**Outputs.** Fixed names, no dated sub-folder:

| File | Content |
|---|---|
| `OutPut/sympt.csv` | Sign-by-sign counts and percentages (`nombre`, `Percent`) |
| `OutPut/symptoms/symptom_bar.png` | Bar chart of sign prevalence, zero-prevalence signs dropped |
| `OutPut/symptoms/symptom_donut.png` | Donut of the same distribution |

**Caveats.**

- Because the snapshot is pinned to 2026-06-05, these figures **do not refresh** when new data arrive.
  Update the path, or switch the script to `load_latest_data()`, before quoting the numbers.
- Sign frequencies are computed on **confirmed cases only** and are constrained by what was recorded on
  the MVE form: a sign absent from the output means it was not documented, not that it did not occur.
- Signs with zero prevalence are filtered out of the bar chart, so the visual is not a complete list of
  possible symptoms.
- The variable `sympts` is assigned twice — first to the clinical-sign columns, then overwritten with
  `bvd.names` (all columns of the data frame). The downstream `across()` ranges, not `all_of(sympts)`,
  are what actually restrict the analysis.

**Language.** French figure titles ("Symptômes des cas confirmés de la MVE, Ituri RDC"). No Quarto
report.

---

## Français

**Thématique.** Fréquence de déclaration de chaque signe clinique chez les cas de MVE17 confirmés par
laboratoire, et variation de cette répartition selon l'âge.

**Script.** `CaseDefinition.R` (~350 lignes).

**Méthode.**

- Ne conserve que `lab_resultat_final == "Positif"`.
- Standardise l'âge avec `AgeStd(age_ans, age_mois)` puis le découpe en 12 classes avec
  `AgeCat(AgeStandard, 12)`.
- Travaille sur le bloc de signes cliniques `s2_fievre` … `s2_autres_signes_hemorragiques`,
  `s2_autres_signes_cliniques_non_hemorragiques`. La valeur `"Ne sait pas"` est recodée en `NA` afin que
  les réponses « statut inconnu » soient exclues du dénominateur plutôt que comptées comme absence de
  signe — sans quoi toutes les fréquences seraient biaisées vers le bas.
- Chaque signe est comparé à `"Oui"`, puis moyenné par cas pour donner une prévalence ; un décompte
  individuel de signes rapportés (`n.sympts`) est également calculé.

**Entrées.** Le script lit un **instantané figé** au lieu d'appeler `load_latest_data()` :

```r
readRDS(file.path(data_cleaning_processed, "evd.clean_Int_2026_06_05_1003.rds"))
```

Fonctions d'appui : `paths.R`, `AgeCat.R`, `AgeStd.R`.

**Sorties.** Noms fixes, sans sous-dossier daté :

| Fichier | Contenu |
|---|---|
| `OutPut/sympt.csv` | Décomptes et pourcentages par signe (`nombre`, `Percent`) |
| `OutPut/symptoms/symptom_bar.png` | Histogramme des prévalences, signes à prévalence nulle retirés |
| `OutPut/symptoms/symptom_donut.png` | Diagramme circulaire de la même répartition |

**Points d'attention.**

- L'instantané étant fixé au 5 juin 2026, ces chiffres **ne se rafraîchissent pas** à l'arrivée de
  nouvelles données. Mettre à jour le chemin, ou basculer le script sur `load_latest_data()`, avant de
  citer ces valeurs.
- Les fréquences sont calculées sur les **cas confirmés uniquement** et dépendent de ce qui a été saisi
  sur la fiche MVE : un signe absent du résultat signifie qu'il n'a pas été documenté, pas qu'il n'a pas
  été observé.
- Les signes de prévalence nulle sont filtrés sur l'histogramme : la figure ne constitue donc pas une
  liste exhaustive des symptômes possibles.
- La variable `sympts` est affectée deux fois — d'abord aux colonnes de signes cliniques, puis écrasée
  par `bvd.names` (toutes les colonnes du data frame). Ce sont les plages `across()` en aval, et non
  `all_of(sympts)`, qui restreignent réellement l'analyse.

**Langue.** Titres des figures en français (« Symptômes des cas confirmés de la MVE, Ituri RDC »). Pas de
rapport Quarto.
