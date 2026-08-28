# Epi.Maps — Situation maps | Cartes de situation

[English](#english) · [Français](#français)

---

## English

**Topic.** Where the outbreak is, and where it is heading. Choropleth maps of health zones and health
areas showing both the current epidemiological trend and which units have just started transmitting.

**Scripts.**

| File | Purpose |
|---|---|
| `EpiMaps.R` | Main pipeline (~1,070 lines): all maps and the confirmed-case workbook. |
| `Maps.R` | **Prototype / legacy.** Loads the OSM DRC health-area GeoJSON and `View()`s it; the mapping code still contains Mpox examples and it writes no files. Kept for reference. |

**Method.** For each health zone and health area, a Poisson model is fitted to the daily case counts
with `incidence::fit()` over an 8-day observation window ending 6 days before the reference date. The
6-day lag prevents the artefact of reading a decline in reported cases as a decline in transmission
when it is only incomplete reporting. Fitted slopes classify units as *progression*, *stabilisation*
or *régression*. A separate layer flags units newly affected in the current epidemiological week.

**Inputs.** GRID3 vector layers from `evd17_maps`: `GRID3_COD_health_areas_v8_0.gdb`,
`GRID3_COD_health_zones_v8_0.gdb`, `cod_admin_boundaries`. Case data via
`load_latest_data(data_cleaning_output, "evd.cleaning", format = "rds")`. Health-zone names are
corrected with `helpers/OrthoFix.R` (`fix_typos_by_parts`) before joining to the boundaries — the
join is the fragile step here, so unmatched names silently disappear from the map.

**Outputs.** Under `OutPut/Epimaps/<ddMon>/`:

| File | Level | Content |
|---|---|---|
| `HZbyProv_count_<ddMon>.png` | `prov/` | Health zones affected per province |
| `Distr.prov.donut_<ddMon>.png` | `prov/` | Provincial distribution donut |
| `PregressionByHZ_<ddMon>.png` | `all/` | Trend classification (progression / stabilisation / regression) |
| `NewAffectedByHZ_<ddMon>.png` | `all/` | Newly affected units by epidemiological week |
| `OutPut/evd.pos.AS_ZS.xlsx` | project root of `OutPut/` | Confirmed-positive counts, sheets `evd.pos.zs` and `evd.pos.as` |

**Language.** French map captions ("Fenêtre d'observations…", "Semaine épidémiologique"). No Quarto
report.

**Related.** Mobility-driven spread risk in
[`../Spread.Risk/`](../Spread.Risk/README.md); alert-volume adequacy per health zone in
[`../Alerts/`](../Alerts/README.md).

---

## Français

**Thématique.** Où se situe la flambée, et vers où elle progresse. Cartes choroplèthes des zones et
aires de santé montrant à la fois la tendance épidémiologique courante et les unités qui viennent de
démarrer une transmission.

**Scripts.**

| Fichier | Objet |
|---|---|
| `EpiMaps.R` | Pipeline principal (~1 070 lignes) : toutes les cartes et le classeur de cas confirmés. |
| `Maps.R` | **Prototype / hérité.** Charge le GeoJSON OSM des aires de santé RDC et l'affiche avec `View()` ; le code de cartographie contient encore des exemples Mpox et aucun fichier n'est écrit. Conservé pour référence. |

**Méthode.** Pour chaque zone et aire de santé, un modèle de Poisson est ajusté aux décomptes quotidiens
de cas avec `incidence::fit()`, sur une fenêtre d'observation de 8 jours s'arrêtant 6 jours avant la
date de référence. Ce décalage de 6 jours évite de confondre une baisse des cas rapportés — due à un
simple retard de notification — avec une baisse réelle de la transmission. Les pentes ajustées classent
les unités en *progression*, *stabilisation* ou *régression*. Une couche distincte signale les unités
nouvellement touchées dans la semaine épidémiologique courante.

**Entrées.** Couches vectorielles GRID3 depuis `evd17_maps` : `GRID3_COD_health_areas_v8_0.gdb`,
`GRID3_COD_health_zones_v8_0.gdb`, `cod_admin_boundaries`. Données de cas via
`load_latest_data(data_cleaning_output, "evd.cleaning", format = "rds")`. Les noms de zones de santé
sont corrigés avec `helpers/OrthoFix.R` (`fix_typos_by_parts`) avant jointure aux limites
administratives — cette jointure est le maillon fragile : un nom non apparié disparaît silencieusement
de la carte.

**Sorties.** Sous `OutPut/Epimaps/<jjMon>/` :

| Fichier | Niveau | Contenu |
|---|---|---|
| `HZbyProv_count_<jjMon>.png` | `prov/` | Zones de santé touchées par province |
| `Distr.prov.donut_<jjMon>.png` | `prov/` | Répartition provinciale (diagramme circulaire) |
| `PregressionByHZ_<jjMon>.png` | `all/` | Classification de tendance (progression / stabilisation / régression) |
| `NewAffectedByHZ_<jjMon>.png` | `all/` | Unités nouvellement touchées par semaine épidémiologique |
| `OutPut/evd.pos.AS_ZS.xlsx` | racine de `OutPut/` | Décomptes de cas positifs, feuilles `evd.pos.zs` et `evd.pos.as` |

**Langue.** Légendes des cartes en français (« Fenêtre d'observations… », « Semaine épidémiologique »).
Pas de rapport Quarto.

**Voir aussi.** Risque de diffusion lié à la mobilité dans
[`../Spread.Risk/`](../Spread.Risk/README.md) ; adéquation du volume d'alertes par zone de santé dans
[`../Alerts/`](../Alerts/README.md).
