## ============================================================================
##  Contact_3w_pptx.R
##  Export des indicateurs de suivi des contacts (3 dernières semaines) vers
##  une présentation PowerPoint (fichier .pptx natif et éditable).
##
##  Filtres appliqués :
##    - Provinces et zones de santé : ≥ 1 cas confirmé uniquement
##    - Zones de santé : 5 les plus touchées (nombre de cas confirmés issu de
##      la dernière line-list EVD nettoyée, même fenêtre de 3 semaines)
##
##  Contenu :
##    1. Diapositive titre
##    2. Synthèse — Ensemble (3 dernières semaines) : 11 indicateurs clés
##    3. Tendances quotidiennes des taux de suivi (graphique)
##       -> contact.fu.ts.all.ts.d.g
##    4. Tableau par province (+ identification meilleur Suivi complété %
##                              et plus fort Perdus de vue %)
##    5. Points saillants — Province
##    6. Tableau des 5 ZS les plus touchées (+ mêmes identifications)
##    7. Points saillants — Zone de santé
##
##  Sortie : OutPut/Contacts/Indicateurs_3w_contacts_<ddMon>.pptx
## ============================================================================

suppressPackageStartupMessages({
  library(here)
  library(officer)
  library(flextable)
  library(dplyr)
})

## --- Rechargement des objets calculés (mode rapport = sans sorties fichier) -
##      Reproduit exactement le bloc d'amorce du .qmd rapport_contact_followup.
##      NB : on NE modifie PAS la locale avant le source() — load_latest_data()
##      parse les répertoires de données ("%d_%B", ex. "09_August") selon la
##      locale courante ; forcer fr_FR avant empêcherait la détection.
source(here::here("helpers", "paths.R"))
options(contact.analysis.report_mode = TRUE)
source(here::here("Contacts.Analysis", "Contact_analysis.R"))

## Stamp du nom de fichier (locale C = anglais), cohérent avec Contact_analysis.R
ddmon <- format(ref.date, "%d%b")

## --- Locale française pour les étiquettes de date (après chargement) ------
##      Influence format(ref.date, "%d %B %Y") et les axes "%b" du graphique
##      (évalués au moment du rendu par ggsave). Tolérant si non disponible.
tryCatch(suppressWarnings(Sys.setlocale("LC_TIME", "fr_FR.UTF-8")),
         error = function(e) invisible(NULL))

## --- Palette / constantes --------------------------------------------------
COL_GREEN_BG  <- "#d4edda"; COL_GREEN_TXT <- "#155724"
COL_RED_BG    <- "#f8d7da"; COL_RED_TXT   <- "#721c24"
COL_HEADER_BG <- "#295aaa"; COL_HEADER_TXT <- "#FFFFFF"
COL_TITLE     <- "#234a7d"

date_label <- format(ref.date, "%d %B %Y")
out_file   <- file.path(
  project_root, "OutPut", "Contacts",
  paste0("Indicateurs_3w_contacts_", ddmon, ".pptx")
)
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)

## --- Helpers de formatage -------------------------------------------------
##   Les ZS sélectionnées peuvent être sans données de contacts (indicateurs
##   NA) : afficher "non disponible" plutôt que "NA".
fmt_int   <- function(x) ifelse(is.na(x), "non disponible",
                                format(as.integer(x), big.mark = " ", scientific = FALSE))
fmt_pct   <- function(x) ifelse(is.na(x), "non disponible", paste0(x, " %"))
fmt_ratio <- function(x) ifelse(is.na(x), "non disponible",
                                format(x, nsmall = 1, scientific = FALSE))
fmt_val   <- function(v, suffix = " %") {
  if (is.na(v)) "non disponible" else paste0(v, suffix)
}

## Helper : noms(s) associé(s) au max d'un indicateur (gestion ex aequo + NA)
top_names <- function(df, value_col, name_col) {
  vals <- df[[value_col]]
  ok <- !is.na(vals)
  if (!any(ok)) return(list(names = "—", value = NA_real_))
  m <- max(vals[ok])
  list(names = paste(df[[name_col]][ok & vals == m], collapse = ", "),
       value = m)
}

## --- Data frames d'affichage (sous-ensemble de colonnes) ------------------
## Filtre : ne conserver que les provinces ayant au moins 1 cas confirmé.
## Pour les ZS, les 5 plus touchées sont déterminées par le nombre de cas
## confirmés calculé directement sur la dernière line-list EVD nettoyée
## (evd.conf.3w.zs, même fenêtre de 3 semaines), puis jointure avec les
## indicateurs de suivi des contacts. NB : une ZS ainsi sélectionnée peut
## être sans données de contacts (indicateurs NA affichés "non disponible").
raw.prov <- contact.3w.prov |>
  filter(n.confirmed.cases >= 1) |>
  arrange(desc(n.contacts))

zs.top <- evd.conf.3w.zs |>
  slice_max(order_by = n.confirmed.cases, n = 5, with_ties = FALSE)

raw.zs <- zs.top |>
  left_join(
    contact.3w.zs |>
      select(-n.confirmed.cases, -ratio.case.confirmed),
    by = c("province_notification", "zone_sante_notification")
  ) |>
  mutate(ratio.case.confirmed = ifelse(n.confirmed.cases > 0,
                                       round(n.source.cases / n.confirmed.cases, 1),
                                       NA_real_)) |>
  arrange(province_notification, desc(n.contacts))

df.prov <- raw.prov |>
  transmute(
    Province = province_notification,
    `Cas confirmés`               = fmt_int(n.confirmed.cases),
    `Contacts`                    = fmt_int(n.contacts),
    `Cas sources`                 = fmt_int(n.source.cases),
    `Ratio contacts/cas`          = fmt_ratio(ratio.contact.case),
    `Ratio cas sources/confirmés` = fmt_ratio(ratio.case.confirmed),
    `Contact suivis >= 1 fois (%)`        = fmt_pct(pct.followed.once),
    `Contacts suivis >= 1 fois (%) du total` = fmt_pct(pct.followed.once.total),
    `Visites attendues`          = fmt_int(n.expected.visits),
    `Visites réalisées (n)`      = fmt_int(n.achieved.visits),
    `Visites réalisées (%)`      = fmt_pct(pct.visits),
    `Suivi complété (%)`         = fmt_pct(pct.completed.21),
    `Perdus de vue (%)`          = fmt_pct(pct.lofu)
  )

df.zs <- raw.zs |>
  transmute(
    Province = province_notification,
    `Zone de Santé`              = zone_sante_notification,
    `Cas confirmés`               = fmt_int(n.confirmed.cases),
    `Contacts`                    = fmt_int(n.contacts),
    `Cas sources`                 = fmt_int(n.source.cases),
    `Ratio contacts/cas`          = fmt_ratio(ratio.contact.case),
    `Ratio cas sources/confirmés` = fmt_ratio(ratio.case.confirmed),
    `Contact suivis >= 1 fois (%)`        = fmt_pct(pct.followed.once),
    `Contacts suivis >= 1 fois (%) du total` = fmt_pct(pct.followed.once.total),
    `Visites attendues`           = fmt_int(n.expected.visits),
    `Visites réalisées (n)`       = fmt_int(n.achieved.visits),
    `Visites réalisées (%)`       = fmt_pct(pct.visits),
    `Suivi complété (%)`          = fmt_pct(pct.completed.21),
    `Perdus de vue (%)`           = fmt_pct(pct.lofu)
  )

## Identifications best/worst (calculées sur les data frames filtrés)
prov.best  <- top_names(raw.prov, "pct.completed.21", "province_notification")
prov.worst <- top_names(raw.prov, "pct.lofu",          "province_notification")
zs.best    <- top_names(raw.zs,   "pct.completed.21", "zone_sante_notification")
zs.worst   <- top_names(raw.zs,   "pct.lofu",          "zone_sante_notification")

## --- KPI Ensemble (synthèse) ----------------------------------------------
o <- contact.3w.overall
kpi <- data.frame(
  Indicateur = c("Cas confirmés", "Contacts", "Cas sources",
                 "Ratio contacts/cas", "Ratio cas sources/confirmés",
                 "Contact suivis >= 1 fois (%)",
                 "Contacts suivis >= 1 fois (%) du total", "Visites attendues",
                 "Visites réalisées (n)", "Visites réalisées (%)",
                 "Suivi complété (%)", "Perdus de vue (%)"),
  Valeur = c(fmt_int(o$n.confirmed.cases), fmt_int(o$n.contacts),
             fmt_int(o$n.source.cases), fmt_ratio(o$ratio.contact.case),
             fmt_ratio(o$ratio.case.confirmed), fmt_pct(o$pct.followed.once),
             fmt_pct(o$pct.followed.once.total),
             fmt_int(o$n.expected.visits), fmt_int(o$n.achieved.visits),
             fmt_pct(o$pct.visits), fmt_pct(o$pct.completed.21),
             fmt_pct(o$pct.lofu)),
  stringsAsFactors = FALSE
)

ft_kpi <- function() {
  ft <- flextable(kpi)
  ft <- bg(ft, bg = COL_HEADER_BG, part = "header")
  ft <- color(ft, color = COL_HEADER_TXT, part = "header")
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- align(ft, align = "center", part = "all")
  ft <- align(ft, align = "left", j = "Indicateur", part = "body")
  ft <- bold(ft, bold = TRUE, j = "Indicateur", part = "body")
  ft <- bg(ft, bg = COL_GREEN_BG, j = "Valeur", part = "body")
  ft <- color(ft, color = COL_GREEN_TXT, j = "Valeur", part = "body")
  ft <- bold(ft, bold = TRUE, j = "Valeur", part = "body")
  ft <- font(ft, fontname = "Arial", part = "all")
  ft <- fontsize(ft, size = 13, part = "all")
  ft <- fontsize(ft, size = 12, part = "header")
  ft <- padding(ft, padding = 6, part = "all")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- width(ft, j = ~ Indicateur, width = 5.8)
  ft <- width(ft, j = ~ Valeur, width = 3.2)
  ft <- set_table_properties(ft, layout = "fixed")
  ft
}

## --- Tableau d'indicateurs (province / ZS) avec surlignage best/worst ------
##   disp_df : data frame déjà mis en forme (chaînes)
##   raw_df  : data frame numérique ARRANGÉ DANS LE MÊME ORDRE que disp_df
##             (servant au calcul des indices de lignes à surligner)
ft_indicators <- function(disp_df, raw_df, n_label, font_size = 10,
                          total_width = 12.4) {
  ft <- flextable(disp_df)
  ## En-tête
  ft <- bg(ft, bg = COL_HEADER_BG, part = "header")
  ft <- color(ft, color = COL_HEADER_TXT, part = "header")
  ft <- bold(ft, bold = TRUE, part = "header")
  ft <- align(ft, align = "center", part = "header")
  ft <- valign(ft, valign = "center", part = "header")
  ## Corps
  ft <- align(ft, align = "center", part = "body")
  ft <- align(ft, align = "left", j = seq_len(n_label), part = "body")
  ft <- bold(ft, bold = TRUE, j = seq_len(n_label), part = "body")
  ft <- font(ft, fontname = "Arial", part = "all")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- padding(ft, padding = 3, part = "all")
  ft <- valign(ft, valign = "center", part = "body")
  ## Largeurs : colonnes libellés plus larges, indicateurs équirépartis
  n_ind <- ncol(disp_df) - n_label
  label_w <- if (n_label == 1) 2.2 else 3.4   # Province / (Province+ZS)
  ind_w   <- (total_width - label_w) / n_ind
  ft <- width(ft, j = seq_len(n_label), width = if (n_label == 1) label_w else c(label_w - 1.6, 1.6))
  ft <- width(ft, j = (n_label + 1):ncol(disp_df), width = ind_w)
  ft <- set_table_properties(ft, layout = "fixed")
  ## Surlignage : meilleur Suivi complété (%) en vert, plus fort Perdus de vue (%) en rouge
  if (!all(is.na(raw_df$pct.completed.21))) {
    m <- max(raw_df$pct.completed.21, na.rm = TRUE)
    idx <- which(!is.na(raw_df$pct.completed.21) & raw_df$pct.completed.21 == m)
    if (length(idx)) {
      ft <- bg(ft,    i = idx, j = "Suivi complété (%)", bg = COL_GREEN_BG)
      ft <- color(ft, i = idx, j = "Suivi complété (%)", color = COL_GREEN_TXT)
      ft <- bold(ft,  i = idx, j = "Suivi complété (%)", bold = TRUE)
    }
  }
  if (!all(is.na(raw_df$pct.lofu))) {
    m <- max(raw_df$pct.lofu, na.rm = TRUE)
    idx <- which(!is.na(raw_df$pct.lofu) & raw_df$pct.lofu == m)
    if (length(idx)) {
      ft <- bg(ft,    i = idx, j = "Perdus de vue (%)", bg = COL_RED_BG)
      ft <- color(ft, i = idx, j = "Perdus de vue (%)", color = COL_RED_TXT)
      ft <- bold(ft,  i = idx, j = "Perdus de vue (%)", bold = TRUE)
    }
  }
  ft
}

## --- Pagination de la table ZS --------------------------------------------
paginate_rows <- function(raw_df, disp_df, per_page = 15) {
  n <- nrow(disp_df)
  if (n == 0) return(list(list(disp = disp_df, raw = raw_df)))
  chunks <- split(seq_len(n), ceiling(seq_len(n) / per_page))
  lapply(chunks, function(i) list(disp = disp_df[i, , drop = FALSE],
                                  raw  = raw_df[i, , drop = FALSE]))
}

## --- Assemblage de la présentation ----------------------------------------
doc <- read_pptx()   # modèle par défaut (4:3, 10 x 7.5 po)

## 1) Diapositive titre -----------------------------------------------------
doc <- add_slide(doc, layout = "Title Slide")
doc <- ph_with(doc,
  "Indicateurs de suivi des contacts — 3 dernières semaines",
  location = ph_location_type(type = "ctrTitle"))
doc <- ph_with(doc,
  paste0("Maladie à virus Bundibugyo (MVE/B), RDC\n",
         "Données au ", date_label, "\nIOA-CAI"),
  location = ph_location_type(type = "subTitle"))

## 2) Synthèse — Ensemble ---------------------------------------------------
doc <- add_slide(doc, layout = "Title Only")
doc <- ph_with(doc, "Synthèse — Ensemble (3 dernières semaines)",
              location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = ft_kpi(),
              location = ph_location(left = 0.5, top = 1.3, width = 9.0, height = 4.6,
                                     newlabel = "kpi_tbl"))
doc <- ph_with(doc,
  paste0("Fenêtre d'analyse : 3 dernières semaines (21 jours) — ",
         "Kasai exclu pour les cas confirmés. Source : Contact_analysis.R"),
  location = ph_location(left = 0.5, top = 6.3, width = 9.0, height = 0.6,
                         newlabel = "kpi_foot"))

## 3) Tendances quotidiennes (graphique) ------------------------------------
##    Rendu en PNG à l'aspect 2:1 (14x7 po), identique au rendu du rapport,
##    puis intégré dans un emplacement 2:1 pour éviter toute distorsion.
plot_png <- tempfile(fileext = ".png")
on.exit(unlink(plot_png), add = TRUE)
ggplot2::ggsave(plot_png,
                plot = contact.fu.ts.all.ts.d.g,
                width = 14, height = 7, dpi = 200, units = "in")
doc <- add_slide(doc, layout = "Title Only")
doc <- ph_with(doc, "Tendances quotidiennes des taux de suivi des contacts",
              location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = external_img(src = plot_png),
              location = ph_location(left = 0.4, top = 1.4,
                                     width = 9.2, height = 4.6,
                                     newlabel = "trend_plot"))

## 4) Tableau par province --------------------------------------------------
ft.prov <- ft_indicators(df.prov, raw.prov, n_label = 1, font_size = 10,
                         total_width = 9.5)
doc <- add_slide(doc, layout = "Title Only")
doc <- ph_with(doc, "Suivi des contacts par province — 3 dernières semaines",
              location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = ft.prov,
              location = ph_location(left = 0.25, top = 1.35,
                                     width = 9.5, height = 4.4,
                                     newlabel = "prov_tbl"))
doc <- ph_with(doc,
  paste0("Meilleur Suivi complété (21 j) : ", prov.best$names,
         " (", fmt_val(prov.best$value), ")  |  ",
         "Plus fort Perdus de vue : ", prov.worst$names,
         " (", fmt_val(prov.worst$value), ")"),
  location = ph_location(left = 0.25, top = 5.9, width = 9.5, height = 1.0,
                         newlabel = "prov_foot"))

## 5) Points saillants — Province ------------------------------------------
prov_bullets <- block_list(
  fpar(ftext("Points saillants — Province",
             prop = fp_text(color = COL_TITLE, bold = TRUE, font.size = 26))),
  fpar(ftext("", prop = fp_text(font.size = 12))),
  fpar(
    ftext("Meilleur Suivi complété (21 j) : ", prop = fp_text(bold = TRUE, font.size = 18)),
    ftext(paste0(prov.best$names, " (", fmt_val(prov.best$value), ")"),
          prop = fp_text(color = COL_GREEN_TXT, bold = TRUE, font.size = 18))
  ),
  fpar(
    ftext("Plus fort taux de Perdus de vue : ", prop = fp_text(bold = TRUE, font.size = 18)),
    ftext(paste0(prov.worst$names, " (", fmt_val(prov.worst$value), ")"),
          prop = fp_text(color = COL_RED_TXT, bold = TRUE, font.size = 18))
  ),
  fpar(ftext("", prop = fp_text(font.size = 12))),
  fpar(ftext("• Les disparités inter-provinciales guident le redéploiement des ressources de suivi.",
             prop = fp_text(font.size = 14))),
  fpar(ftext("• Un fort taux de perdus de vue signale une rupture de la chaîne de surveillance et un risque de transmission silencieuse.",
             prop = fp_text(font.size = 14)))
)
doc <- add_slide(doc, layout = "Title Only")
doc <- ph_with(doc, "Points saillants — Province",
              location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = prov_bullets,
              location = ph_location(left = 0.5, top = 1.5, width = 9.0, height = 5.4,
                                     newlabel = "prov_bullets"))

## 6) Tableau par zone de santé (paginé) ------------------------------------
##    Les 5 ZS les plus touchées sont déjà sélectionnées à partir des cas
##    confirmés de la line-list EVD (evd.conf.3w.zs) : une seule page suffit
##    dans la plupart des cas.
zs_pages <- paginate_rows(raw.zs, df.zs, per_page = 15)
for (k in seq_along(zs_pages)) {
  ft.zs.k <- ft_indicators(zs_pages[[k]]$disp, zs_pages[[k]]$raw,
                           n_label = 2, font_size = 8, total_width = 9.6)
  doc <- add_slide(doc, layout = "Title Only")
  doc <- ph_with(doc,
    paste0("Suivi des contacts par zone de santé — 3 dernières semaines",
           " (5 ZS les plus touchées)",
           if (length(zs_pages) > 1) paste0(" — page ", k, "/", length(zs_pages)) else ""),
    location = ph_location_type(type = "title"))
  doc <- ph_with(doc, value = ft.zs.k,
                location = ph_location(left = 0.2, top = 1.35,
                                       width = 9.6, height = 5.6,
                                       newlabel = paste0("zs_tbl_", k)))
}

## 7) Points saillants — Zone de santé -------------------------------------
zs_bullets <- block_list(
  fpar(ftext("Points saillants — Zone de santé",
             prop = fp_text(color = COL_TITLE, bold = TRUE, font.size = 26))),
  fpar(ftext("", prop = fp_text(font.size = 12))),
  fpar(
    ftext("Meilleur Suivi complété (21 j) : ", prop = fp_text(bold = TRUE, font.size = 18)),
    ftext(paste0(zs.best$names, " (", fmt_val(zs.best$value), ")"),
          prop = fp_text(color = COL_GREEN_TXT, bold = TRUE, font.size = 18))
  ),
  fpar(
    ftext("Plus fort taux de Perdus de vue : ", prop = fp_text(bold = TRUE, font.size = 18)),
    ftext(paste0(zs.worst$names, " (", fmt_val(zs.worst$value), ")"),
          prop = fp_text(color = COL_RED_TXT, bold = TRUE, font.size = 18))
  ),
  fpar(ftext("", prop = fp_text(font.size = 12))),
  fpar(ftext("• Les ZS à forte proportion de perdus de vue sont des points chauds de transmission potentielle.",
             prop = fp_text(font.size = 14))),
  fpar(ftext("• Un suivi ciblé (rattrapage, renforcement des équipes de proximité) y est indispensable.",
             prop = fp_text(font.size = 14)))
)
doc <- add_slide(doc, layout = "Title Only")
doc <- ph_with(doc, "Points saillants — Zone de santé",
              location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = zs_bullets,
              location = ph_location(left = 0.5, top = 1.5, width = 9.0, height = 5.4,
                                     newlabel = "zs_bullets"))

## --- Sauvegarde + vérification --------------------------------------------
print(doc, target = out_file)

stopifnot(file.exists(out_file), file.size(out_file) > 50000)
cat(sprintf("OK : %s (%.0f KB)\n", out_file, file.size(out_file) / 1024))
