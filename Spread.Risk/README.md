# Spread.Risk — Geographic spread risk | Risque de diffusion géographique

[English](#english) · [Français](#français)

---

## English

**Topic.** Which health zones are most likely to be reached next. Estimated population movements out
of affected zones are combined with confirmed-case counts and road accessibility to rank not-yet-affected
neighbouring zones.

**Script.** `population.relocation.R` (~730 lines).

**Method.**

1. Take confirmed cases and alert counts per health zone from the processed line list.
2. Classify each zone as *Already affected* or *Not affected*.
3. Load GRID3 estimated **relocation** flows (`v2.0` products) and keep the movements leaving affected
   zones in Ituri, North Kivu and South Kivu towards zones that have not yet reported a case.
4. Weight and rank destination zones, joining the accessibility file so that hard-to-reach zones are
   not penalised as if they were merely unobserved.
5. Produce per-province bar charts and a flow map.

**Inputs.**

| Source | Path |
|---|---|
| Mobility estimates | `data/drc-estimated-relocations-2020_03-2026_03-v2.0-external.csv` and `...-2026_04-v2.0-external.csv` (both versions are read and compared) |
| Accessibility | `data/20250701 RDC_ZS_Accessibilité.xlsx` |
| Cases | `load_latest_data(data_cleaning_processed, file_pattern = ".*\\.rds$")` — note this is the **processed** folder, unlike most other topics which read `data_cleaning_output` |
| Boundaries | `evd17_maps/GRID3_COD_health_zones_v8_0.gdb`, `evd17_maps/cod_admin_boundaries.shp/cod_admin1.shp` |
| Helpers | `paths.R`, `DivideGroups.R`, `LoadLatestData.R` |

**Outputs.** Written with fixed names (not date-stamped) into `OutPut/RiskAssessment/`:

- Tables: `bvd.confirmedByHZ.xlsx`, `evd.alerts.xlsx`, `drc.reloc.itr.xlsx`, `drc.reloc.nkv.xlsx`,
  `risk.area_<date>.xlsx`, `access.by.HZ.xlsx`, `Acces.secur.xlsx`, `Acces.secur.Full.xlsx`,
  `suspect.l7d.xlsx`, `suspect.alltime.xlsx`, `suspect.dead.NoLab.xlsx`,
  `Ituri.RiskAssessment.Updated.AllSurroundings.xlsx`.
- Figures: `drc.reloc.itr.png`, `drc.reloc.nk.png`, `drc.reloc.sk.png`.

The folder also holds `relocation.map.png`, `relocation.est_flowsJanApril2026.xlsx`,
`Ituri.RiskAssessment.Updated.xlsx`, `Bvd.SuspectsByHZ.xlsx`, `suspect.l48h.xlsx` and
`Community deaths.xlsx`, which no longer have a matching write call in the current script — they are
leftovers from earlier revisions.

**Known issue.** The last block saves a relocation flow map with
`ggsave(paste0(day.dir.mp.a, "relocation.map_", ...))`, but `day.dir.mp.a` is **not defined in this
file** — it comes from `EpiMaps.R`. Running the script standalone fails on that final statement; either
define the directory first or drop the block.

**Language.** Mixed French/English. No Quarto report.

---

## Français

**Thématique.** Quelles zones de santé ont le plus de chances d'être atteintes ensuite. Les déplacements
de population estimés au départ des zones touchées sont combinés aux décomptes de cas confirmés et à
l'accessibilité routière pour classer les zones voisines encore indemnes.

**Script.** `population.relocation.R` (~730 lignes).

**Méthode.**

1. Extraire les cas confirmés et les alertes par zone de santé depuis la line list traitée.
2. Classer chaque zone en *Already affected* ou *Not affected*.
3. Charger les flux de **relocalisation** estimés par GRID3 (produits `v2.0`) et conserver les
   mouvements quittant les zones touchées de l'Ituri, du Nord-Kivu et du Sud-Kivu vers des zones
   n'ayant pas encore déclaré de cas.
4. Pondérer et classer les zones de destination, en joignant le fichier d'accessibilité afin que les
   zones difficiles d'accès ne soient pas pénalisées comme si elles étaient simplement non observées.
5. Produire des graphiques par province et une carte des flux.

**Entrées.**

| Source | Chemin |
|---|---|
| Estimations de mobilité | `data/drc-estimated-relocations-2020_03-2026_03-v2.0-external.csv` et `...-2026_04-v2.0-external.csv` (les deux versions sont lues et comparées) |
| Accessibilité | `data/20250701 RDC_ZS_Accessibilité.xlsx` |
| Cas | `load_latest_data(data_cleaning_processed, file_pattern = ".*\\.rds$")` — dossier **traité**, contrairement à la plupart des thématiques qui lisent `data_cleaning_output` |
| Limites | `evd17_maps/GRID3_COD_health_zones_v8_0.gdb`, `evd17_maps/cod_admin_boundaries.shp/cod_admin1.shp` |
| Fonctions d'appui | `paths.R`, `DivideGroups.R`, `LoadLatestData.R` |

**Sorties.** Noms fixes (sans datation), dans `OutPut/RiskAssessment/` :

- Tableaux : `bvd.confirmedByHZ.xlsx`, `evd.alerts.xlsx`, `drc.reloc.itr.xlsx`, `drc.reloc.nkv.xlsx`,
  `risk.area_<date>.xlsx`, `access.by.HZ.xlsx`, `Acces.secur.xlsx`, `Acces.secur.Full.xlsx`,
  `suspect.l7d.xlsx`, `suspect.alltime.xlsx`, `suspect.dead.NoLab.xlsx`,
  `Ituri.RiskAssessment.Updated.AllSurroundings.xlsx`.
- Figures : `drc.reloc.itr.png`, `drc.reloc.nk.png`, `drc.reloc.sk.png`.

Le dossier contient aussi `relocation.map.png`, `relocation.est_flowsJanApril2026.xlsx`,
`Ituri.RiskAssessment.Updated.xlsx`, `Bvd.SuspectsByHZ.xlsx`, `suspect.l48h.xlsx` et
`Community deaths.xlsx`, pour lesquels le script actuel n'a plus d'appel d'écriture correspondant : ce
sont des reliquats de versions antérieures.

**Problème connu.** Le dernier bloc enregistre une carte des flux avec
`ggsave(paste0(day.dir.mp.a, "relocation.map_", ...))`, or `day.dir.mp.a` **n'est pas défini dans ce
fichier** — il provient de `EpiMaps.R`. En exécution autonome, le script échoue sur cette dernière
instruction ; il faut soit définir le dossier au préalable, soit supprimer le bloc.

**Langue.** Français/anglais mélangés. Pas de rapport Quarto.
