# Epi.Curves — Epidemic curves | Courbes épidémiques

[English](#english) · [Français](#français)

---

## English

**Topic.** The core situational-trend picture: confirmed and probable cases over time, split by
outcome status (alive / dead), with weekly case-fatality overlaid, at national, provincial, health
zone and health area level.

**Script.** `EpiCurves.R` (~1,570 lines). Run from the project root:
`Rscript Epi.Curves/EpiCurves.R`.

**What it does.**

- Rebuilds a single case-level laboratory result (`lab_resultat_final2`) from the twelve per-sample
  result columns (`lab_resultat_final_1` … `_12`), with precedence Positif > Négatif > Invalide. This
  matters because a case can be re-sampled for test-of-cure, and naive counting would double-count.
- Daily epicurves of confirmed cases, and of cases by status, nationally, per province and per health
  zone.
- Weekly epicurves with a CFR ("Létalité") overlay, to separate changes in surveillance intensity from
  changes in severity.
- Health-area-level curves for the selected health zones.
- Aggregation tables of confirmed cases by epidemiological week.

**Inputs.** `load_latest_data(data_cleaning_output, "evd.cleaning", format = "rds")`. Helpers:
`paths.R`, `DebutSem.R` (epidemiological week start), `AgeCat.R`, `AgeStd.R`, `LoadLatestData.R`.

**Outputs.** Under `OutPut/EpiCurves/<ddMon>/`:

| Type | Files | Level |
|---|---|---|
| PNG | `Epicurve.all.confirmed.Daily_`, `Epicurve.all.status.Daily_`, `Epicurve.all.status.Weekly_` | `all/` |
| PNG | `Epicurve.prov.status.Daily_` (plus one per province), `Epicurve.prov.status.Weekly_`, `Epicurve.status.Daily_prov.zs_<province>_` | `prov/` |
| PNG | `Epicurve.zs.status.Daily_` (plus one per ZS), `Epicurve.zs.status.Weekly_<zs>` | `zs/` |
| PNG | `Epicurve.status.Daily_zs.as_<zs>` | `as/` |
| XLSX | `Confirmed.Cases.By.Prov.HZ.AS.EpiWk.xlsx`, `Confirmed.Cases.By.HA.EpiWk.xlsx`, `Confirmed.BVD.AS_<ddMon>.xlsx` | `zs/`, `all/`, `as/` |

**Language.** English comments, French titles and captions ("Tendances des cas confirmés et décès de
la MVB…", SGI MVB). No Quarto report — the figures are the deliverable.

**Related.** Age structure of the same cases in [`../AgeSex/`](../AgeSex/README.md); severity and
deaths in [`../evd_deaths/`](../evd_deaths/README.md); reporting-delay adjustment of the same curve in
[`../scripts/Nowcasting.R`](../scripts/).

---

## Français

**Thématique.** Le tableau de bord des tendances : cas confirmés et probables dans le temps,
ventilés par issue (vivants / décédés), avec la létalité hebdomadaire en superposition, aux niveaux
national, provincial, zone de santé et aire de santé.

**Script.** `EpiCurves.R` (~1 570 lignes). Depuis la racine du projet :
`Rscript Epi.Curves/EpiCurves.R`.

**Traitements.**

- Reconstitue un résultat laboratorial unique par cas (`lab_resultat_final2`) à partir des douze
  colonnes de résultat par échantillon (`lab_resultat_final_1` … `_12`), avec précédence
  Positif > Négatif > Invalide. Cette étape est nécessaire car un cas peut être re-prélevé pour le
  test de guérison, et un décompte naïf compterait deux fois le même cas.
- Courbes épidémiques quotidiennes des cas confirmés, et des cas par issue, au niveau national, par
  province et par zone de santé.
- Courbes hebdomadaires avec superposition de la létalité, pour distinguer l'effort de surveillance de
  la sévérité réelle.
- Courbes au niveau aire de santé pour les zones sélectionnées.
- Tableaux de cas confirmés agrégés par semaine épidémiologique.

**Entrées.** `load_latest_data(data_cleaning_output, "evd.cleaning", format = "rds")`. Fonctions
d'appui : `paths.R`, `DebutSem.R` (début de semaine épidémiologique), `AgeCat.R`, `AgeStd.R`,
`LoadLatestData.R`.

**Sorties.** Sous `OutPut/EpiCurves/<jjMon>/` :

| Type | Fichiers | Niveau |
|---|---|---|
| PNG | `Epicurve.all.confirmed.Daily_`, `Epicurve.all.status.Daily_`, `Epicurve.all.status.Weekly_` | `all/` |
| PNG | `Epicurve.prov.status.Daily_` (plus un par province), `Epicurve.prov.status.Weekly_`, `Epicurve.status.Daily_prov.zs_<province>_` | `prov/` |
| PNG | `Epicurve.zs.status.Daily_` (plus un par ZS), `Epicurve.zs.status.Weekly_<zs>` | `zs/` |
| PNG | `Epicurve.status.Daily_zs.as_<zs>` | `as/` |
| XLSX | `Confirmed.Cases.By.Prov.HZ.AS.EpiWk.xlsx`, `Confirmed.Cases.By.HA.EpiWk.xlsx`, `Confirmed.BVD.AS_<jjMon>.xlsx` | `zs/`, `all/`, `as/` |

**Langue.** Commentaires en anglais, titres et légendes en français (SGI MVB). Pas de rapport Quarto :
les graphiques constituent le livrable.

**Voir aussi.** Structure par âge des mêmes cas dans [`../AgeSex/`](../AgeSex/README.md) ; sévérité et
décès dans [`../evd_deaths/`](../evd_deaths/README.md) ; correction des délais de notification de la
même courbe dans [`../scripts/Nowcasting.R`](../scripts/).
