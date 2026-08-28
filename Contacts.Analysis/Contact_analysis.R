# Contact analysis 

## loading libraries

My.Packages =
  installed.packages()[,"Package"]

if(!"pacman" %in% My.Packages) {

  install.packages("pacman")
} else{

  pacman::p_load("openxlsx","readxl","incidence",
                 "stringdist","lubridate",
                 "RColorBrewer","wesanderson",
                 "dplyr","tidyverse","stringi",
                 "ggforce","ggfittext","ggthemes",
                 "ggrepel","tidytext",
                 "zoo","forecast","Kendall",
                 "DescTools","kableExtra",
                 install = T)

}

source(here::here("helpers", "paths.R"))

# When sourced from a Quarto report, skip file/directory output generation
skip_output <- isTRUE(getOption("contact.analysis.report_mode", FALSE))

## Loading custom functions

source(here::here("helpers", "DebutSem.R"))
source(here::here("helpers", "AgeCat.R"))
source(here::here("helpers", "AgeStd.R"))
source(here::here("helpers", "OrthoCorrect.R"))
source(here::here("helpers", "OrthoFix.R"))
source(here::here("helpers", "LoadLatestData.R"))
source(here::here("helpers", "clean_filename.R"))
source(here::here("helpers", "contact.cleaning.fx.R"))


## Importing contact data

### Long format 
contact.data.long <- load_latest_data( 
  data_cleaning_output,
  folder_name = NULL,
  format = "rds",
  file_pattern = "contact.long.clean_Int_.+\\.rds"
)

# NOTE (Aug 2026): the long format is now exported one row per follow-up visit
# with follow_up_date (Date) and follow_up_day (integer). The former collapsed
# comma-separated date_suivi column no longer exists; no expansion is needed.

contact.data.long <- contact.data.long |> 
  filter(follow_up_date <= now())

### wide format
contact.data <- load_latest_data(
  data_cleaning_output,
  folder_name = NULL,
  format = "rds",
  file_pattern = "contact.clean_Int.+\\.rds"
)

if (interactive()) contact.data |> View()
names(contact.data)

### EVD line-list (latest cleaned snapshot) — pour les cas confirmés
evd.data <- load_latest_data(
  data_cleaning_output,
  folder_name = "evd.cleaning",
  format = "rds",
  file_pattern = "evd.clean_Int_.+\\.rds"
)

evd.data <-
  evd.data |> 
  mutate(date_heure_notification_alerte = as.Date(date_heure_notification_alerte)) |>
  filter(date_heure_notification_alerte <= now())

# Restrict all contact summaries to provinces and health zones with at least
# one confirmed case in 2026 in the EVD line list. This prevents reporting
# areas with contact records but no confirmed transmission in the current
# outbreak from appearing in the dashboard.
confirmed_case_geo <- evd.data |>
  filter(
    lab_resultat_final == "Positif" | classification_finale == "Cas confirmé",
    !is.na(date_heure_notification_alerte),
    year(date_heure_notification_alerte) == 2026,
    !is.na(province_notification), !is.na(zone_sante_notification)
  ) |>
  distinct(province_notification, zone_sante_notification)

contact.data.long <- contact.data.long |>
  semi_join(confirmed_case_geo,
            by = c("province_notification", "zone_sante_notification"))

contact.data <- contact.data |>
  semi_join(confirmed_case_geo,
            by = c("province_notification", "zone_sante_notification"))

# Nom de la colonne de date d'enrôlement des cas (vérifié, avec repli)
evd.enrol.col <- intersect(c("enrolement_date", "enrollment_date",
                             "date_enrollement"), names(evd.data))[1]
if (is.na(evd.enrol.col)) {
  stop("Colonne de date d'enrôlement introuvable dans evd.data. Colonnes disponibles : ",
       paste(grep("date|enrol|Date", names(evd.data), value = TRUE, ignore.case = TRUE),
             collapse = ", "))
}

(max.fill.date <- max(as.Date(contact.data$date_maj[which(as.Date(contact.data$date_maj) <= as.Date(now()))]), na.rm = TRUE))

# NOTE (Aug 2026): the wide format no longer carries follow-up dates
# (date_suivi was removed; visit-level data lives in the long format).
# max.fill.date (from date_maj above) is the reference date used downstream.

### Defining output directories

if (!skip_output) {
  day.dir <- paste0(here::here("OutPut/Contacts/"),format(max.fill.date, "%d%b"),"/")
  day.dir.a <- paste0(here::here("OutPut/Contacts/"),format(max.fill.date, "%d%b"),"/all/")
  day.dir.p <- paste0(here::here("OutPut/Contacts/"),format(max.fill.date, "%d%b"),"/prov/")
  day.dir.z <- paste0(here::here("OutPut/Contacts/"),format(max.fill.date, "%d%b"),"/zs/")
  day.dir.s <- paste0(here::here("OutPut/Contacts/"),format(max.fill.date, "%d%b"),"/as/")

  if(!dir.exists(day.dir)){
      dir.create(day.dir)
    }

  if (!dir.exists(day.dir.a)){
      dir.create(day.dir.a)
    }

  if (!dir.exists(day.dir.p)){
      dir.create(day.dir.p)
    }

  if (!dir.exists(day.dir.z)){
      dir.create(day.dir.z)
    }

  if (!dir.exists(day.dir.s)){
      dir.create(day.dir.s)
    }
} 

### Excluding Kasai province from analysis

contact.data |>
  filter(is.na(date_debut_suivi),!is.na(date_dernier_contact_cas_source))


contact.data_fu <-
  contact.data |>
  filter(province_notification != "Kasai")|>
  mutate(fu_delay = as.numeric(date_debut_suivi - date_dernier_contact_cas_source),
         fu_date = case_when(is.na(date_debut_suivi)~"Not Available",
                            fu_delay < 0 ~ "Abnormal",
                            .default = "Normal"),
         age_grp = AgeCat(age_ans, n_cats = 4),
         contact.recycle = grepl("r[eéè]c",resultat_suivi, ignore.case = T),
         devenu.suspect =  grepl("susp",resultat_suivi, ignore.case = T)|!is.na(date_symptomes)|symptomes ==  "Oui",
         devenu.confirme = grepl("conf",resultat_suivi, ignore.case = T))

### FU delay in follow-up

contact.data_fu.delays <-
  contact.data_fu |> 
  filter(fu_date == "Normal")  |> 
  select(province_notification,zone_sante_notification,aire_sante_notification,
         nom_prenom_contact,nom_post_nom_prenom_cas,age_grp,sexe,type_contact, 
         symptomes, resultat_suivi, date_dernier_contact_cas_source, fu_delay) 

contact.data_fu.n.contacts <-
  contact.data_fu.delays |>
  summarise(contacts.n = median(n(), na.rm = T),
            .by=c(nom_post_nom_prenom_cas, 
                  zone_sante_notification, 
                  province_notification))

contact.data_fu.delays.zs <-
  contact.data_fu.delays |>
  summarise(fu_delay_med = round(median(fu_delay)),
            fu_delay_mean = round(mean(fu_delay)),
            n.contacts = n(),
            .by = c(province_notification,zone_sante_notification))

contact.data_fu.delay.as <-
  contact.data_fu |>
  filter(fu_date == "Normal") |>
  summarise(fu_delay_med = round(median(fu_delay)),
            fu_delay_mean = round(mean(fu_delay)),
            n.contacts = n(),
            .by = c(zone_sante_notification, aire_sante_notification))


##### FU Number of contacts by cases

contact.data_fu.n.zs <-
  contact.data_fu |>
  summarise(contacts.n = median(n(), na.rm = T),
            .by=c(nom_post_nom_prenom_cas, 
                  zone_sante_notification, 
                  province_notification))

contact.data_fu.n.as <-
  contact.data_fu |>
  summarise(contacts.n = median(n(), na.rm = T),
            .by=c(nom_post_nom_prenom_cas, 
                  aire_sante_notification,
                  zone_sante_notification
                  ))


contact.data_fu.n.zs |>
  summarise(contacts.med = round(median(contacts.n)),
            contacts.mean = round(mean(contacts.n)),
            n.cases = n(),
            .by = c(province_notification,zone_sante_notification))

contact.data_fu.n.zs |>
  summarise(contacts.med = round(median(contacts.n)),
            contacts.mean = round(mean(contacts.n)),
            n.cases = n(),
            .by = province_notification)

contact.data_fu.n.as |>
  summarise(contacts.med = round(median(contacts.n)),
            contacts.mean = round(mean(contacts.n)),
            n.cases = n(),
            .by = c(aire_sante_notification,
                  zone_sante_notification))

### figures 
#### Delays par health zones
##### boxplots

zs.delays <-
  contact.data_fu.delays.zs |> 
  filter(n.contacts >= 5 ) |>   
  mutate(zs_prov= paste0(zone_sante_notification, " (",province_notification,")"))|>
  arrange(fu_delay_med) #|> pull(zs_prov)

contact.data_fu.delays.a <-
  contact.data_fu.delays |>
  mutate(delay8d = ifelse(fu_delay < 8, "0-7", "8+"),
         type_contact2 = case_when(grepl("1|2",type_contact)~"Direct-Touché(1-2)",
                                   .default = "Autres(3-4)")) |>
  summarise(n.delay = n(),
            .by = c(delay8d,province_notification,zone_sante_notification,
            aire_sante_notification,type_contact2)) |>
  pivot_wider(names_from = delay8d, 
              values_from = n.delay,
              values_fill = list(n.delay = 0)) |>
  mutate(Ttl.contacts = rowSums(across(c(`0-7`,`8+`))))


(contact.data_fu.delays.z <-
  contact.data_fu.delays.a |>
  arrange(zone_sante_notification) |>
  summarise(n.contacts = sum(Ttl.contacts),
            n8plus = sum(`8+`),
            perct8plus = round(sum(`8+`)/sum(Ttl.contacts)*100,1),
          .by = c(province_notification,zone_sante_notification,type_contact2)))


(contact.data_fu.delays.all <-
  contact.data_fu.delays.a |>
  summarise(n.contacts = sum(Ttl.contacts),
            n8plus = sum(`8+`),
            perct8plus = round(sum(`8+`)/sum(Ttl.contacts)*100,1),
          .by = c(type_contact2)) |>
  group_by(type_contact2)|>
  mutate(lci = round(prop.test(n8plus,n.contacts)$conf.int[1]*100,1),
          uci = round(prop.test(n8plus,n.contacts)$conf.int[2]*100,1)))


contact.data_fu.delays.4f <-
contact.data_fu.delays |>
  filter(zone_sante_notification %in% zs.delays$zone_sante_notification,
        fu_delay <= 21)|>
  mutate(zs_prov= paste0(zone_sante_notification, " (",province_notification,")"),
         zs_prov = factor(zs_prov,levels = zs.delays$zs_prov)) 

contact.data_fu.n.zs

max(contact.data_fu.delays.4f$fu_delay, na.rm = T)
sort(contact.data_fu.delays.4f$fu_delay, decreasing = T)[1:100]

# delay of contact follow-up foer health zones with at least 5 contacts followed-up

(contact.data_fu.delays.4f.g <-
 contact.data_fu.delays.4f |>
  ggplot(aes(x = zs_prov, y = fu_delay))+
  geom_jitter(width = 0.1, size=1.2, color = "#295aaa38")+
  geom_boxplot(width = 0.4, fill= "#99b5e164", 
               median.color = "#010409", outlier.color ="#18479abe",
               median.linewidth = 1.5, box.color ="#010409", 
               whisker.color= "#010409",staple.color ="#010409")+
  coord_flip()+
  scale_y_continuous(breaks = seq(0, max(contact.data_fu.delays.4f$fu_delay)*1.05, 7),
                     expand = c(0.02,0),
                     labels = function(x) paste0(x, " jours"))+
  labs(
    title = "Délai suivi des contacts MVE/B par zone de santé, RDC",
    subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"),"\n",
                      "Analyse basée sur ",sum(contact.data_fu.n.contacts$contacts.n), " contacts de ", 
                      nrow(contact.data_fu.n.contacts), " Cas confirmés de MVE/B avec date de suivi disponible","\n",
                      "Délai median de ",
                      round(median(contact.data_fu.delays.4f$fu_delay, na.rm = T)), "  jours, moyen de ",
                      round(MeanCI(contact.data_fu.delays.4f$fu_delay, na.rm = T)[[1]])," jours [",
                    floor(MeanCI(contact.data_fu.delays.4f$fu_delay, na.rm = T)[[2]]),"-",
                  round(MeanCI(contact.data_fu.delays.4f$fu_delay, na.rm = T)[[3]]),"]","\n",
                      "Seules les ZS avec au moins 5 Contacts suivis sont présentes sur ce graphique."),
    y = "Délai", x = NULL,
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
    fill = "") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
    panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, hjust = 1),
    axis.title = element_text(size = 13),
    axis.line.x = element_line(color = "grey30"),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    plot.title = element_text(size = 16, face = "bold", color = "#03084a"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 7, hjust = 1),
    legend.position = "top"
  )
)

if (!skip_output) {
contact.data_fu.delays.4f.g |>
  ggsave(filename = paste0(day.dir.z,"Delai_Expo_Suivi_zs_",
                          format(max.fill.date, "%d%b"),
                          ".png"),
         dpi = 300, height = 20, width = 25, scale = 0.35)
}



contact.data_fu.delays.linear <-
  contact.data_fu.delays |>
  filter(date_dernier_contact_cas_source >= as.Date("2026-05-01"),
         fu_delay <= 21)|>
  mutate(epiw = get_monday(date_dernier_contact_cas_source))

smoothed <- "#03054b"



(contact.data_fu.delays.linear.g <-
 contact.data_fu.delays.linear |>
  ggplot(aes(x = epiw, y = fu_delay))+
  geom_jitter(width = 0.5, size=1.2, color = "#295aaa38")+
  geom_boxplot(aes(group= epiw),
               width = 1, fill= "#99b5e164", 
               median.color = "#010409", outlier.color ="#18479abe",
               median.linewidth = 1.5, box.color ="#010409", 
               whisker.color= "#010409",staple.color ="#010409")+
  scale_y_continuous(breaks = seq(0, max(contact.data_fu.delays.linear$fu_delay)*1.05, 7),
                     expand = c(0.02,0),
                     labels = function(x) paste0(x, " jours"))+
  scale_x_date(date_breaks = "1 week",
               date_labels = "%V\n%Y",
               expand = c(0.005,0))+
  labs(
    title = "Délai suivi des contacts MVE/B par semaine épidémiologique, RDC",
    subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"),"\n",
                      "Analyse basée sur ",sum(contact.data_fu.n.contacts$contacts.n), " contacts de ", 
                      nrow(contact.data_fu.n.contacts), " Cas confirmés de MVE/B avec date de suivi disponible","\n",
                      "Délai median de ",
                      round(median(contact.data_fu.delays.4f$fu_delay, na.rm = T)), "  jours, moyen de ",
                      round(MeanCI(contact.data_fu.delays.4f$fu_delay, na.rm = T)[[1]])," jours [",
                    floor(MeanCI(contact.data_fu.delays.4f$fu_delay, na.rm = T)[[2]]),"-",
                  round(MeanCI(contact.data_fu.delays.4f$fu_delay, na.rm = T)[[3]]),"]","\n",
                      "Seules les ZS avec au moins 5 Contacts suivis sont présentes sur ce graphique."),
    y = "Délai",  x="Semaine épidémiologique de dernier contact avec le cas confirmé",
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now()))) +
    theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 12),
        legend.position = c(0.125, 0.90),
        legend.key.size = unit(0.5, "cm"))
)


if (!skip_output) {
contact.data_fu.delays.linear.g |>
  ggsave(file=paste0(day.dir.a,"Tendance_delai_Expo_Suivi_",
                          format(max.fill.date, "%d%b"),
                          ".png"),
         dpi = 300, height = 20, width = 30, scale = 0.3)
}


### Number of contacts per confirmed case

zs.prov.ncontacts <-
  contact.data_fu.n.zs |>
  summarise( n.contact = sum(contacts.n, na.rm = T),
             n.source = n(),
             n.contacts.med =round( median(contacts.n, na.rm = T)),
            #n.contacts.mean = round(mean(contacts.n, na.rm = T)),
             n.contact.ratio = round(sum(contacts.n)/n()),
            .by = c(zone_sante_notification,province_notification)) |>
  filter(n.source >= 5) |> 
  arrange(desc(n.contacts.med))

### ---- kableExtra table: Contacts par cas confirmé par ZS ----

# Prepare table data with formatted columns
zs.prov.ncontacts.tbl <- zs.prov.ncontacts |>
  select(zone_sante_notification, province_notification, 
         n.contact, n.source, n.contacts.med, n.contact.ratio) |>
  rename(`Zone de Santé` = zone_sante_notification,
         Province = province_notification,
         `Total contacts` = n.contact,
         `Cas sources` = n.source,
         `Contacts médian/cas` = n.contacts.med,
         `Ratio contacts/cas` = n.contact.ratio)

# Build the kable table
tbl.ncontacts <- zs.prov.ncontacts.tbl |>
  kbl(escape = FALSE, align = "c",
      caption = paste0("Nombre de contacts par cas confirmé MVE/B par zone de santé — ",
                       format(max.fill.date, "%d %B %Y"))) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
  add_header_above(c(" " = 1, " " = 1, "Nombre de contacts" = 2, "Médiane" = 1, "Ratio" = 1)) |>
  column_spec(1, bold = TRUE, border_right = TRUE) |>
  column_spec(2, border_right = TRUE)

if (!skip_output) {
tbl.ncontacts |>
  save_kable(file = paste0(day.dir.z,
                           "NContacts_tbl_zs_",
                           format(max.fill.date, "%d%b"),
                           ".html"))
}

zs.ncontacts <-  zs.prov.ncontacts |>
  arrange(n.contacts.med) |>
  pull(zone_sante_notification)



contact.data_fu.n.zs.4f <-
contact.data_fu.n.zs |>
  mutate(zone_sante_notification = 
          factor(zone_sante_notification, 
                 levels = zs.ncontacts))|>
  filter(zone_sante_notification %in% zs.prov.ncontacts$zone_sante_notification) 

contact.data_fu.n.zs.4f.g <-
  contact.data_fu.n.zs.4f |>
  ggplot(aes(x = zone_sante_notification, y = contacts.n))+
  #geom_violin(fill="#2b6ed90f", color= "#0327624c")+
  #geom_jitter(width = 0.1, size=1.2, color = "#0b26506b")+
  geom_boxplot(width = 0.07, fill= "#99b5e164", 
               median.color = "#010409", outlier.color ="#4e0404d0",
               median.linewidth = 1.5, box.color ="#010409", 
               whisker.color= "#010409",staple.color ="#010409")+
  scale_y_continuous(transform = "log",
                     breaks= seq(0, max(contact.data_fu.n.zs.4f$contacts.n, na.rm = T)*1.1, 10),
                     labels = function(x) round(x),
                     expand = c(0.02,0))+
  scale_x_discrete(labels = function(x) paste0(x, "(", 
                                    zs.prov.ncontacts$province_notification[zs.prov.ncontacts$zone_sante_notification == x],")"))+
  coord_flip()+
  labs(
    title = "Nombre de contacts par cas confirmé MVE/B par zone de santé, RDC",
    subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"),"\n",
                      "Analyse basée sur ",sum(contact.data_fu.n.zs$contacts.n), " contacts de ", 
                      nrow(contact.data_fu.n.zs), " Cas confirmés de MVE/B","\n",
                      "Ratio : ",
                      round(sum(contact.data_fu.n.zs$contacts.n)/nrow(contact.data_fu.n.zs),0), 
                      " Contacts listés pour 1 Cas confirmé.","\n",
                      "Seules les ZS avec au moins 5 Cas sources sont présentées sur ce graphique."),
    y = "n contacts (médian)", x = NULL,
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now()))) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
    panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 12, hjust = 1),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_blank(),
    axis.line.y = element_line(color = "grey30"),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    plot.title = element_text(size = 16, face = "bold", color = "#03084a"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 7, hjust = 1),
    legend.position = "top"
  )

#contact.data_fu.n.zs.4f.g |>
 # ggsave(filename = paste0(day.dir.z,"Ncontacts_zs_",
  #                        format(max.fill.date, "%d%b"),
   #                       ".png"),
    #     dpi = 300, height = 20, width = 25, scale = 0.36)



#### lienar


contact.data_fu.n.zs


contact.data_fu.n.zs.d <-
  contact.data_fu |>
  filter(!is.na(date_dernier_contact_cas_source),
         date_dernier_contact_cas_source >= as.Date("2026-05-01")) |>
  summarise(contacts.n = median(n(), na.rm = T),
            .by=c(nom_post_nom_prenom_cas, 
                  date_dernier_contact_cas_source,
                  zone_sante_notification, 
                  province_notification)) |>
  mutate(epiw = get_monday(date_dernier_contact_cas_source)) 

median.contact_case <- round(nrow(contact.data_fu)/
                        length(unique(contact.data_fu$nom_post_nom_prenom_cas)))

ratio.contact_case <- contact.data_fu |> filter(!is.na(date_dernier_contact_cas_source)) |>
  summarise(n.contacts = n(),
            n.cases = length(unique(nom_post_nom_prenom_cas))) |>
  mutate(ratio.contact_case = round(n.contacts/n.cases,0))

(contact.data_fu.n.zs.d.g <-
 contact.data_fu.n.zs.d |>
  ggplot(aes(x = epiw, y = contacts.n))+
  geom_jitter(width = 0.5, size=1.2, color = "#295aaa38")+
  geom_boxplot(aes(group= epiw),
               width = 1, fill= "#99b5e164", 
               median.color = "#010409", outlier.color ="#18479abe",
               median.linewidth = 1.5, box.color ="#010409", 
               whisker.color= "#010409",staple.color ="#010409")+
  scale_y_continuous(transform = "log",
                     breaks = seq(0, max(contact.data_fu.n.zs.d$contacts.n)*1.05, 10),
                     expand = c(0.02,0))+
  scale_x_date(date_breaks = "1 week",
               date_labels = "%V\n%Y",
               expand = c(0.005,0))+
  labs(
    title = "Contacts par cas confirmé de MVE/B par semaine épidémiologique, RDC",
    subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"),"\n",
                      "Analyse basée sur ",ratio.contact_case$n.contacts, " contacts de ", 
                      ratio.contact_case$n.cases, " Cas confirmés de MVE/B avec date de dernier contact disponible","\n",
                    "Ratio : ",
                      ratio.contact_case$ratio.contact_case, 
                      " Contacts listés pour 1 Cas confirmé."),
    y = "Délai en jours (log)",  x="Semaine épidémiologique d'exposition au cas confirmé",
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now()))) +
    theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 12),
        legend.position = c(0.125, 0.90),
        legend.key.size = unit(0.5, "cm"))
)


if (!skip_output) {
contact.data_fu.n.zs.d.g |>
  ggsave(file=paste0(day.dir.a,"ContactByCase_ts_",
                          format(max.fill.date, "%d%b"),
                          ".png"),
         dpi = 300, height = 20, width = 30, scale = 0.3)
}


### Follow-up 

names(contact.data.long)

contact.long.e.fu <- contact.data.long |>
  mutate(name_id = paste0(nom_prenom_contact,"_",id_contact),
         date_suivi = as.Date(follow_up_date)) |>  # follow_up_date replaces date_suivi (refactored Aug 2026)
  summarise(date_expo= min(date_dernier_contact_cas_source, na.rm = T),
            f.fu = min(date_suivi, na.rm = T),
            n.fu = sum(!is.na(date_suivi)),
            .by = name_id) |>
  filter(!is.infinite(f.fu))


contacts.jamais.vus <- 
  data.frame(
    ncontacts = nrow(contact.data_fu),
    ncontacts.jamais.vus = sum(is.na(contact.data_fu$date_debut_suivi)),
    perct.contacts.jamais.vus = round(sum(is.na(contact.data_fu$date_debut_suivi))/nrow(contact.data_fu)*100, 1)
  )

if (!skip_output) {
if (interactive()) contact.data_fu |>
  filter(zone_sante_notification == "Kilo") |>
  select(nom_prenom_contact,all_of(contains("date"))) |> View()
}

contact.data_fu.JV <- contact.data_fu |>
  summarise(`N contacts` = n(),
            `N contacts jamais vus` = sum(is.na(date_debut_suivi)),
            .by = c(province_notification, zone_sante_notification)) |>
  mutate(`Zone de santé` = paste0(zone_sante_notification, " (",province_notification,")"),
         `Contacts jamais vus (%)` = round(`N contacts jamais vus`/`N contacts`*100, 1)) |>
  select(`Zone de santé`, `N contacts`, `N contacts jamais vus`, `Contacts jamais vus (%)`) |>
  arrange(desc(`Contacts jamais vus (%)`)) 

if (!skip_output) {
contact.data_fu.JV |>
  kbl(escape = FALSE, align = "c",
      caption = paste0("Contacts jamais vus par zone de santé — ",
                       format(max.fill.date, "%d %B %Y"))) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
  add_header_above(c(" " = 1, "Contacts" = 3)) |>
  column_spec(1, bold = TRUE, border_right = TRUE) |>
  column_spec(2:4, border_right = TRUE) |>
  save_kable(file = paste0(day.dir.z,
                           "Contacts_jamais_vus_tbl_zs_",
                           format(max.fill.date, "%d%b"),
                           ".html"))
}

# time series follow-up of contacts

#contact.data.long |> View()

table(contact.data.long$contact_vu, useNA = "ifany")

contact.long.prep <- contact.data.long |>
  mutate(name_id = paste0(nom_prenom_contact, "_", id_contact),
         date_suivi = as.Date(follow_up_date),  # follow_up_date replaces date_suivi (refactored Aug 2026)
         contact.recycle = grepl("r[eéè]c",resultat_suivi, ignore.case = T),
         devenu.suspect =  grepl("susp",resultat_suivi, ignore.case = T)|!is.na(symptomes_2)|symptomes ==  "Oui",
         devenu.confirme = grepl("conf",resultat_suivi, ignore.case = T))

contact.long.prep |> names()

contact.fu.meta <- contact.long.prep |>
  summarise(
    zone_sante_notification =
      (zone_sante_notification[!is.na(zone_sante_notification)])[1],
    aire_sante_notification =
      (aire_sante_notification[!is.na(aire_sante_notification)])[1],
    expo_date = as.Date(min(date_dernier_contact_cas_source, na.rm = TRUE)),
    ef.fu = expo_date + 1,
    f.fu = min(date_suivi, na.rm = TRUE),
    .by = name_id
  ) |>
  filter(!is.infinite(ef.fu)) |>
  #filter(!is.infinite(f.fu), f.fu <= expo_date + 21) |>
  mutate(e.l.fu = expo_date + 21)


## Build the per-contact date grid (one row per follow-up day). reframe()
## handles variable-length output per group and un-nests atomically.
contact.fu.grid <- contact.fu.meta |>
  reframe(date_suivi = seq(ef.fu, e.l.fu, by = "1 day"), .by = name_id)

## Left-join observed follow-up records onto the grid (full_join keeps any
## observed follow-up records outside [f.fu, l.fu], matching the original loop)

contact.fu.ts <- contact.fu.grid |>
  full_join(
    contact.long.prep |> select(name_id, date_suivi, contact_vu, contact.recycle, 
                                devenu.suspect, devenu.confirme),
    by = c("name_id", "date_suivi")
  ) |>
  left_join(
    contact.fu.meta |> select(name_id, zone_sante_notification,
                              aire_sante_notification,e.l.fu),
    by = "name_id"
  ) |>
  mutate(contact_vu = ifelse(is.na(contact_vu), "Non", contact_vu)) |>
  select(zone_sante_notification, aire_sante_notification,
         name_id, date_suivi, contact_vu, contact.recycle, 
         devenu.suspect, devenu.confirme,e.l.fu) |>
  arrange(name_id, date_suivi) |>
  group_split(name_id)

rm(contact.fu.meta, contact.fu.grid, contact.long.prep)

contact.fu.ts.all <- contact.fu.ts |>
  bind_rows() |>
  filter(date_suivi >= "2026-05-01" &
  date_suivi <= as.Date(max(contact.data.long$follow_up_date, na.rm = T))) |> 
  mutate(new.fu = date_suivi == min(date_suivi),
         end.fu = date_suivi == e.l.fu,
         LoFU = flag_lofu(contact_vu),
         .by = name_id) |>
  arrange(date_suivi) |>
  select(-e.l.fu)

#contact.fu.ts.all |> arrange(name_id, date_suivi) |> View()

### ---- Follow-up % relative to expected visits of ALL contacts (incl. jamais-vus) ----
## The visit grid above (contact.fu.ts.all) is built from the long format and
## therefore only covers contacts with at least one follow-up record, and its
## full_join keeps visits recorded OUTSIDE the [expo+1, expo+21] window. To
## compute a follow-up percentage relative to the TRUE expected workload, we
## rebuild a dedicated grid from the FULL contact roster — including contacts
## never seen at least once (jamais-vus). The roster is the union of:
##   - contacts present in the long format (expo taken from the long roster, so
##     it matches the grid above), and
##   - contacts present only in the wide roster (never seen) with a usable expo.
## Each contact contributes one expected visit-day per day in [expo+1, expo+21],
## capped at max.fill.date. Observed follow-up records (contact.data.long) are
## LEFT-joined onto this grid: a grid day with a "Oui" record counts as a seen
## expected visit; a grid day with no record counts as not seen. Visits recorded
## outside [expo+1, expo+21] (off-grid) are dropped, so the numerator is
## on-grid seen and perct.vu cannot exceed 100%. These perct.vu values are
## joined downstream into the daily/weekly summaries; they do NOT alter
## n.contacts, n.contacts.vu, LoFU, end.fu, etc.

## Guard: required columns
.roster.cols <- c("nom_prenom_contact", "id_contact",
                  "date_dernier_contact_cas_source",
                  "zone_sante_notification",
                  "aire_sante_notification")
stopifnot(all(.roster.cols %in% names(contact.data)))
stopifnot(all(.roster.cols %in% names(contact.data.long)))
stopifnot("follow_up_date" %in% names(contact.data.long))

n.no.expo <- sum(is.na(contact.data$date_dernier_contact_cas_source))
if (n.no.expo > 0) {
  warning(n.no.expo,
          " contact(s) sans date de dernier contact avec le cas source ",
          "exclus du denominateur des visites attendues.",
          call. = FALSE)
}

## Roster: contacts in the long format (expo from long, reliable)
.all.meta.long <- contact.data.long |>
  mutate(name_id = paste0(nom_prenom_contact, "_", id_contact)) |>
  filter(!is.na(date_dernier_contact_cas_source)) |>
  summarise(
    zone_sante_notification =
      (zone_sante_notification[!is.na(zone_sante_notification)])[1],
    aire_sante_notification =
      (aire_sante_notification[!is.na(aire_sante_notification)])[1],
    expo_date = as.Date(min(date_dernier_contact_cas_source, na.rm = TRUE)),
    .by = name_id
  ) |>
  filter(!is.na(expo_date))

## Roster: contacts only in the wide format (never seen), expo from wide
.all.meta.wide <- contact.data |>
  mutate(name_id = paste0(nom_prenom_contact, "_", id_contact)) |>
  filter(!is.na(date_dernier_contact_cas_source)) |>
  anti_join(.all.meta.long, by = "name_id") |>
  summarise(
    zone_sante_notification =
      (zone_sante_notification[!is.na(zone_sante_notification)])[1],
    aire_sante_notification =
      (aire_sante_notification[!is.na(aire_sante_notification)])[1],
    expo_date = as.Date(date_dernier_contact_cas_source[1]),
    .by = name_id
  ) |>
  filter(!is.na(expo_date))

contact.all.meta <- bind_rows(.all.meta.long, .all.meta.wide) |>
  mutate(ef.fu = expo_date + 1,
         e.l.fu = expo_date + 21)

## Observed follow-up records: one row per (contact, day), "Oui" if seen that day
.all.observed <- contact.data.long |>
  mutate(name_id = paste0(nom_prenom_contact, "_", id_contact),
         date_suivi = as.Date(follow_up_date)) |>
  filter(!is.na(date_suivi)) |>
  summarise(contact_vu = ifelse(any(contact_vu == "Oui", na.rm = TRUE),
                                "Oui", "Non"),
            .by = c(name_id, date_suivi))

## All-contacts grid with observed joined (on-grid only; off-grid dropped)
contact.all.fu <- contact.all.meta |>
  reframe(date_suivi = seq(ef.fu, e.l.fu, by = "1 day"), .by = name_id) |>
  left_join(contact.all.meta |>
              select(name_id, zone_sante_notification, aire_sante_notification),
            by = "name_id") |>
  left_join(.all.observed, by = c("name_id", "date_suivi")) |>
  mutate(contact_vu = ifelse(is.na(contact_vu), "Non", contact_vu)) |>
  filter(date_suivi >= as.Date("2026-05-01"),
         date_suivi <= max.fill.date)

## Helper: percentage with zero-denominator guard
.pct <- function(num, den) ifelse(den > 0, round(num / den * 100, 1), NA_real_)

## Follow-up % summaries at each aggregation level (on-grid seen / expected)
perct.all.d <- contact.all.fu |>
  summarise(n.expected.all = n(),
            n.vu.on.grid = sum(contact_vu == "Oui"),
            .by = date_suivi) |>
  mutate(perct.vu = .pct(n.vu.on.grid, n.expected.all),
         perct.non.vu = ifelse(n.expected.all > 0,
                               round(100 - perct.vu, 1), NA_real_)) |>
  arrange(date_suivi)

perct.all.d.zs <- contact.all.fu |>
  filter(!is.na(zone_sante_notification)) |>
  summarise(n.expected.all = n(),
            n.vu.on.grid = sum(contact_vu == "Oui"),
            .by = c(zone_sante_notification, date_suivi)) |>
  mutate(perct.vu = .pct(n.vu.on.grid, n.expected.all),
         perct.non.vu = ifelse(n.expected.all > 0,
                               round(100 - perct.vu, 1), NA_real_))

perct.all.d.zs.as <- contact.all.fu |>
  filter(!is.na(zone_sante_notification), !is.na(aire_sante_notification)) |>
  summarise(n.expected.all = n(),
            n.vu.on.grid = sum(contact_vu == "Oui"),
            .by = c(zone_sante_notification, aire_sante_notification, date_suivi)) |>
  mutate(perct.vu = .pct(n.vu.on.grid, n.expected.all),
         perct.non.vu = ifelse(n.expected.all > 0,
                               round(100 - perct.vu, 1), NA_real_))

perct.all.wk <- contact.all.fu |>
  mutate(epiw = get_monday(date_suivi)) |>
  summarise(n.expected.all = n(),
            n.vu.on.grid = sum(contact_vu == "Oui"),
            .by = epiw) |>
  mutate(perct.vu = .pct(n.vu.on.grid, n.expected.all),
         perct.non.vu = ifelse(n.expected.all > 0,
                               round(100 - perct.vu, 1), NA_real_)) |>
  arrange(epiw)

rm(.roster.cols, n.no.expo, .all.meta.long, .all.meta.wide,
   contact.all.meta, .all.observed, contact.all.fu, .pct)

### contact follow-up by day of follow-up

if (interactive()) View(contact.fu.ts.all)

if (interactive()) contact.fu.ts.all |>
  arrange(name_id, date_suivi) |> View()


epiw.dates <-  unique(get_monday(sort(contact.fu.ts.all$date_suivi)))

contact.fu.ts.all.ts.d <- contact.fu.ts.all |>
  filter(!is.na(LoFU)) |>
  summarise(n.contacts = n(),
            n.contacts.nouveau = sum(new.fu, na.rm = TRUE),
            n.contacts.vu = sum(contact_vu == "Oui"),
            n.contacts.non.vu = sum(contact_vu == "Non"),
            perdu.de.vu = sum(LoFU, na.rm = TRUE),
            recylce = sum(contact.recycle, na.rm = TRUE),
            suspect = sum(devenu.suspect, na.rm = TRUE),
            confirme = sum(devenu.confirme, na.rm = TRUE),
            end.fu = sum(end.fu, na.rm = TRUE),
             .by = c(date_suivi)) |>
  left_join(perct.all.d, by = "date_suivi") |>
  mutate(n.expected.all = tidyr::replace_na(n.expected.all, 0),
         percent.nouveau = round(n.contacts.nouveau / n.contacts * 100, 1),
         perct.perdu.de.vu = round(perdu.de.vu / n.contacts * 100, 1),
         perct.recylce = round(recylce / n.contacts * 100, 1),
         perct.suspect = round(suspect / n.contacts * 100, 1),
         perct.confirme = round(confirme / n.contacts * 100, 1),
         perct.end.fu = round(end.fu / n.contacts * 100, 1)) |>
  arrange(desc(date_suivi))

contact.fu.ts.all.ts.d.zs <- contact.fu.ts.all |>
  filter(!is.na(LoFU)) |>
  summarise(n.contacts = n(),
            n.contacts.nouveau = sum(new.fu, na.rm = TRUE),
            n.contacts.vu = sum(contact_vu == "Oui"),
            n.contacts.non.vu = sum(contact_vu == "Non"),
            perdu.de.vu = sum(LoFU, na.rm = TRUE),
            recylce = sum(contact.recycle, na.rm = TRUE),
            suspect = sum(devenu.suspect, na.rm = TRUE),
            confirme = sum(devenu.confirme, na.rm = TRUE),
            end.fu = sum(end.fu, na.rm = TRUE),
            .by = c(zone_sante_notification,date_suivi)) |>
  left_join(perct.all.d.zs,
            by = c("zone_sante_notification", "date_suivi")) |>
  mutate(n.expected.all = tidyr::replace_na(n.expected.all, 0),
         percent.nouveau = round(n.contacts.nouveau / n.contacts * 100, 1),
         perct.perdu.de.vu = round(perdu.de.vu / n.contacts * 100, 1),
         perct.recylce = round(recylce / n.contacts * 100, 1),
         perct.suspect = round(suspect / n.contacts * 100, 1),
         perct.confirme = round(confirme / n.contacts * 100, 1),
         perct.end.fu = round(end.fu / n.contacts * 100, 1)) |>
  arrange(desc(date_suivi))


contact.fu.ts.all.ts.d.zs.as <- contact.fu.ts.all |>
  filter(!is.na(LoFU)) |>
  summarise(n.contacts = n(),
            n.contacts.nouveau = sum(new.fu, na.rm = TRUE),
            n.contacts.vu = sum(contact_vu == "Oui"),
            n.contacts.non.vu = sum(contact_vu == "Non"),
            perdu.de.vu = sum(LoFU, na.rm = TRUE),
            recylce = sum(contact.recycle, na.rm = TRUE),
            suspect = sum(devenu.suspect, na.rm = TRUE),
            confirme = sum(devenu.confirme, na.rm = TRUE),
            end.fu = sum(end.fu, na.rm = TRUE),
            .by = c(zone_sante_notification, aire_sante_notification 
                    ,date_suivi)) |>
  left_join(perct.all.d.zs.as,
            by = c("zone_sante_notification", "aire_sante_notification",
                   "date_suivi")) |>
  mutate(n.expected.all = tidyr::replace_na(n.expected.all, 0),
         percent.nouveau = round(n.contacts.nouveau / n.contacts * 100, 1),
         perct.perdu.de.vu = round(perdu.de.vu / n.contacts * 100, 1),
         perct.recylce = round(recylce / n.contacts * 100, 1),
         perct.suspect = round(suspect / n.contacts * 100, 1),
         perct.confirme = round(confirme / n.contacts * 100, 1),
         perct.end.fu = round(end.fu / n.contacts * 100, 1)) |>
  arrange(desc(date_suivi))


### ---- XLSX export: daily follow-up with percentage columns ----

if (!skip_output) {

wb <- createWorkbook()

# Blue header style
header_style <- createStyle(
  fgFill       = "#2b6ed9",
  fontColour   = "#FFFFFF",
  textDecoration = "bold",
  border       = "TopBottomLeftRight",
  borderColour = "#1a4fa0",
  halign       = "center"
)

# Sheet 1: All (daily summary)
addWorksheet(wb, "All_daily")
writeData(wb, "All_daily", contact.fu.ts.all.ts.d)
addFilter(wb, "All_daily", rows = 1, cols = seq_len(ncol(contact.fu.ts.all.ts.d)))
addStyle(wb, "All_daily", style = header_style,
         rows = 1, cols = seq_len(ncol(contact.fu.ts.all.ts.d)), gridExpand = TRUE)
setColWidths(wb, "All_daily", cols = seq_len(ncol(contact.fu.ts.all.ts.d)), widths = "auto")
freezePane(wb, "All_daily", firstRow = TRUE)

# Sheet 2: By zone de santé
addWorksheet(wb, "By_ZS")
writeData(wb, "By_ZS", contact.fu.ts.all.ts.d.zs)
addFilter(wb, "By_ZS", rows = 1, cols = seq_len(ncol(contact.fu.ts.all.ts.d.zs)))
addStyle(wb, "By_ZS", style = header_style,
         rows = 1, cols = seq_len(ncol(contact.fu.ts.all.ts.d.zs)), gridExpand = TRUE)
setColWidths(wb, "By_ZS", cols = seq_len(ncol(contact.fu.ts.all.ts.d.zs)), widths = "auto")
freezePane(wb, "By_ZS", firstRow = TRUE)

# Sheet 3: By zone de santé × aire de santé
addWorksheet(wb, "By_ZS_AS")
writeData(wb, "By_ZS_AS", contact.fu.ts.all.ts.d.zs.as)
addFilter(wb, "By_ZS_AS", rows = 1, cols = seq_len(ncol(contact.fu.ts.all.ts.d.zs.as)))
addStyle(wb, "By_ZS_AS", style = header_style,
         rows = 1, cols = seq_len(ncol(contact.fu.ts.all.ts.d.zs.as)), gridExpand = TRUE)
setColWidths(wb, "By_ZS_AS", cols = seq_len(ncol(contact.fu.ts.all.ts.d.zs.as)), widths = "auto")
freezePane(wb, "By_ZS_AS", firstRow = TRUE)

# Save workbook
saveWorkbook(
  wb,
  file = paste0(day.dir.a, "ContactFU_daily_pct_", format(max.fill.date, "%d%b"), ".xlsx"),
  overwrite = TRUE
)

}

#contact.fu.ts.all |> View()

contact.fu.ts.all.ts.wk <- contact.fu.ts.all |>
  filter(!is.na(LoFU)) |>
  mutate(epiw = get_monday(date_suivi)) |>
  summarise(n.contacts = n(),
            n.contacts.vu = sum(contact_vu == "Oui"),
            n.contacts.non.vu = sum(contact_vu == "Non"),
            perdu.de.vu = sum(LoFU, na.rm = TRUE),
            recylce = sum(contact.recycle, na.rm = TRUE),
            suspect = sum(devenu.suspect, na.rm = TRUE),
            confirme = sum(devenu.confirme, na.rm = TRUE),
            end.fu = sum(end.fu, na.rm = TRUE),
            .by = c(epiw)) 

contact.fu.ts.all.ts.wk1  <- contact.fu.ts.all.ts.wk |>
  left_join(perct.all.wk, by = "epiw") |>
  mutate(n.expected.all = tidyr::replace_na(n.expected.all, 0),
         perct.perdu.de.vu = round(perdu.de.vu/n.contacts*100,1),
         perct.recylce = round(recylce/n.contacts*100,1),
         perct.suspect = round(suspect/n.contacts*100,1),
         perct.confirme = round(confirme/n.contacts*100,1),
         perct.end.fu = round(end.fu/n.contacts*100,1))

if (interactive()) contact.fu.ts.all.ts.wk1 |> View()

length(unique(contact.fu.ts.all$name_id))

unique(contact.fu.ts.all$name_id)


contact.fu.ts.all.ts <- 
  contact.fu.ts.all.ts.wk1 |>
  select(epiw, perct.vu, perct.non.vu) |>
  pivot_longer(cols = c(perct.vu, perct.non.vu),
               names_to = "type", values_to = "perct") 

contact.fu.ts.all.zs.ts <- contact.fu.ts.all |>
  mutate(epiw = get_monday(date_suivi)) |>
  summarise(n.contacts = n(),
            n.contacts.vu = sum(contact_vu == "Oui"),
            .by = c(epiw,zone_sante_notification)) |>
  mutate(perct.vu = round(n.contacts.vu/n.contacts*100,1))

contact.fu.ts.all.zs.lew <- contact.fu.ts.all |>
  mutate(epiw = get_monday(date_suivi)) |>
  filter(epiw >= nth(epiw.dates, -3), !is.na(zone_sante_notification)) |>
  summarise(n.contacts = n(),
            n.contacts.vu = sum(contact_vu == "Oui"),
            .by = c(zone_sante_notification)) |>
  mutate(perct.vu = round(n.contacts.vu/n.contacts*100,1)) |>
  arrange(desc(perct.vu))

contact.fu.ts.all.zs.ts |>
  filter(zone_sante_notification == "Bunia")

ncontact.f.fu <- unique(contact.fu.ts.all$name_id) |> length()

if (interactive()) View(contact.fu.ts.all.ts)

symptoms.cols <- c("Devenu suspect" = "#f08649", "Devenu confirmé" = "#ea4e42")
visit.cols <- c("perct.vu" = "#65bfed", "perct.non.vu" = "#fb838d",
                "Perdu de vue" = "#2f1c1c")
outcome.cols <- c("% suspects" = "#f08649", "% confirmés" = "#ea4e42")

(contact.fu.ts.all.ts.g <-
 contact.fu.ts.all.ts |>
  ggplot(aes(x = epiw))+
  geom_area(aes(y = perct, fill = type))+
  geom_area(data = contact.fu.ts.all.ts.wk1,
            aes(y = perct.perdu.de.vu, fill = "Perdu de vue"), alpha=0.5)+
  #geom_line(data = contact.fu.ts.all.ts.wk1,
            #aes(y = perct.suspect, color = "% suspects"),
            #linetype=2, linewidth = 1)+
  #geom_line(data = contact.fu.ts.all.ts.wk1,
            #aes(y = perct.confirme, color = "% confirmés"),
            #linetype=6, linewidth = 1)+
  scale_fill_manual(values = visit.cols,
                    labels = c("perct.vu" = "Visites vues (% des attendues)",
                               "perct.non.vu" = "Visites attendues non vues",
                               "Perdu de vue" = "Perdu de vue"))+
  scale_color_manual(values = outcome.cols)+
  scale_y_continuous(expand = c(0.02,0),
                     labels = scales::percent_format(scale=1))+
  scale_x_date(date_breaks = "1 week",
               date_labels = "%V\n%Y",
               expand = c(0.005,0))+
  labs(
    title = "Evolution des suivis (%) des contacts MVE/B par semaine épidémiologique, RDC",
    subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"),"\n",
                      "Analyse basée sur ",ncontact.f.fu, " contacts"),
    fill=NULL, color=NULL,
    y = "Pourcentage de suivis",  x="Semaine épidémiologique de suivi",
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now()))) +
    theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"))
)


if (!skip_output) {
contact.fu.ts.all.ts.g |>
  ggsave(file=paste0(day.dir.a,"ContactFU_ts_",
                          format(max.fill.date, "%d%b"),
                          ".png"),
         dpi = 300, height = 20, width = 33, scale = 0.3)
}

### Daily contact follow-up time series plot

contact.fu.ts.all.ts.d1 <- contact.fu.ts.all.ts.d |>
  filter(date_suivi <= max.fill.date) |>
  mutate(perct.perdu.de.vu = round(perdu.de.vu / n.contacts * 100, 1),
         perct.recylce = round(recylce / n.contacts * 100, 1),
         perct.suspect = round(suspect / n.contacts * 100, 1),
         perct.confirme = round(confirme / n.contacts * 100, 1))

### Daily contact follow-up by zone de santé

contact.fu.ts.all.ts.d.zs <- contact.fu.ts.all |>
  filter(!is.na(LoFU) & date_suivi <= max.fill.date) |>
  summarise(n.contacts = n(),
            n.contacts.vu = sum(contact_vu == "Oui"),
            n.contacts.non.vu = sum(contact_vu == "Non"),
            perdu.de.vu = sum(LoFU, na.rm = TRUE),
            recylce = sum(contact.recycle, na.rm = TRUE),
            suspect = sum(devenu.suspect, na.rm = TRUE),
            confirme = sum(devenu.confirme, na.rm = TRUE),
            end.fu = sum(end.fu, na.rm = TRUE),
            .by = c(date_suivi, zone_sante_notification)) |>
  left_join(perct.all.d.zs,
            by = c("zone_sante_notification", "date_suivi")) |>
  mutate(n.expected.all = tidyr::replace_na(n.expected.all, 0))

contact.fu.ts.all.ts.d1.zs <- contact.fu.ts.all.ts.d.zs |>
  mutate(perct.perdu.de.vu = round(perdu.de.vu / n.contacts * 100, 1),
         perct.recylce = round(recylce / n.contacts * 100, 1),
         perct.suspect = round(suspect / n.contacts * 100, 1),
         perct.confirme = round(confirme / n.contacts * 100, 1))

### ---- kableExtra table: Contacts vus par ZS (last 21 days) ----

# Reshape to wide format: rows = ZS, columns = dates, values = perct.vu
contact.fu.ts.all.ts.d1.zs.tbl <- contact.fu.ts.all.ts.d1.zs |>
  filter(date_suivi >= max.fill.date - 20) |>
  select(zone_sante_notification, date_suivi, perct.vu) |>
  pivot_wider(
    names_from = date_suivi,
    values_from = perct.vu,
    values_fill = NA
  ) |>
  arrange(zone_sante_notification)

# Create parallel table of n.vu.on.grid (seen within expected window) for hover tooltips
contact.fu.ts.all.ts.d1.zs.tbl.nvu <- contact.fu.ts.all.ts.d1.zs |>
  filter(date_suivi >= max.fill.date - 20) |>
  select(zone_sante_notification, date_suivi, n.vu.on.grid) |>
  pivot_wider(
    names_from = date_suivi,
    values_from = n.vu.on.grid,
    values_fill = NA
  ) |>
  arrange(zone_sante_notification)

# Create parallel table of n.expected.all (denominator: all contacts incl. jamais-vus)
contact.fu.ts.all.ts.d1.zs.tbl.nexp <- contact.fu.ts.all.ts.d1.zs |>
  filter(date_suivi >= max.fill.date - 20) |>
  select(zone_sante_notification, date_suivi, n.expected.all) |>
  pivot_wider(
    names_from = date_suivi,
    values_from = n.expected.all,
    values_fill = NA
  ) |>
  arrange(zone_sante_notification)

# Format date column names to French-readable style
date_cols <- names(contact.fu.ts.all.ts.d1.zs.tbl)[-1]
formatted_dates <- format(as.Date(date_cols), "%d %b")
names(contact.fu.ts.all.ts.d1.zs.tbl)[-1] <- formatted_dates
names(contact.fu.ts.all.ts.d1.zs.tbl.nvu)[-1] <- formatted_dates
names(contact.fu.ts.all.ts.d1.zs.tbl.nexp)[-1] <- formatted_dates

# Build and save the HTML table
n_date_cols <- ncol(contact.fu.ts.all.ts.d1.zs.tbl) - 1

tbl <- contact.fu.ts.all.ts.d1.zs.tbl |>
  rename(`Zone de Santé` = zone_sante_notification) |>
  kbl(escape = FALSE, align = "c",
      caption = paste0("Suivi des contacts par zone de santé (visites vues / visites attendues, tous contacts inclus ceux jamais vus) — 21 derniers jours, ",
                       format(max.fill.date, "%d %B %Y"))) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
  add_header_above(c(" " = 1,
    "Contacts vus par zone de santé au cours des 21 derniers jours (% des visites attendues)" = n_date_cols)) |>
  column_spec(1, bold = TRUE, border_right = TRUE)

# Apply conditional formatting to each date column using column_spec
for (i in seq_len(n_date_cols)) {
  col_idx <- i + 1  # +1 because first column is Zone de Santé
  nvu_vals <- contact.fu.ts.all.ts.d1.zs.tbl.nvu[[col_idx]]
  nexp_vals <- contact.fu.ts.all.ts.d1.zs.tbl.nexp[[col_idx]]
  pct_vals <- contact.fu.ts.all.ts.d1.zs.tbl[[col_idx]]
  
  tbl <- tbl |>
    column_spec(col_idx,
                background = ifelse(is.na(pct_vals), "white",
                             ifelse(pct_vals >= 95, "#4CAF50",
                             ifelse(pct_vals >= 50, "#FF9800", "#F44336"))),
                color = ifelse(is.na(pct_vals), "grey", "white"),
                bold = TRUE,
                popover = ifelse(is.na(nexp_vals),
                          "Pas de données",
                          paste0("Contacts vus: ", nvu_vals,
                                 " / attendues: ", nexp_vals)))
}

if (!skip_output) {
tbl |>
  save_kable(file = paste0(day.dir.z,
                           "ContactFU_tbl_zs_",
                           format(max.fill.date, "%d%b"),
                           ".html"))
}

### Daily contact follow-up by aire de santé

contact.fu.ts.all.ts.d.as <- contact.fu.ts.all |>
  filter(!is.na(LoFU) & date_suivi <= max.fill.date) |>
  summarise(n.contacts = n(),
            n.contacts.vu = sum(contact_vu == "Oui"),
            n.contacts.non.vu = sum(contact_vu == "Non"),
            perdu.de.vu = sum(LoFU, na.rm = TRUE),
            recylce = sum(contact.recycle, na.rm = TRUE),
            suspect = sum(devenu.suspect, na.rm = TRUE),
            confirme = sum(devenu.confirme, na.rm = TRUE),
            end.fu = sum(end.fu, na.rm = TRUE),
            .by = c(date_suivi, aire_sante_notification))

contact.fu.ts.all.ts.d1.as <- contact.fu.ts.all.ts.d.as |>
  filter(date_suivi <= max.fill.date) |>
  mutate(perct.vu = round(n.contacts.vu / n.contacts * 100, 1),
         perct.non.vu = round(n.contacts.non.vu / n.contacts * 100, 1),
         perct.perdu.de.vu = round(perdu.de.vu / n.contacts * 100, 1),
         perct.recylce = round(recylce / n.contacts * 100, 1),
         perct.suspect = round(suspect / n.contacts * 100, 1),
         perct.confirme = round(confirme / n.contacts * 100, 1))

### The plot below shows the daily follow-up of contacts, with the percentage of contacts lost to follow-up, suspected, and confirmed cases.

contact.fu.ts.all.ts.d.long <-
  contact.fu.ts.all.ts.d1 |>
  filter(date_suivi <= max.fill.date) |>
  select(date_suivi, perct.vu, perct.non.vu) |>
  pivot_longer(cols = c(perct.vu, perct.non.vu),
               names_to = "type", values_to = "perct")

(contact.fu.ts.all.ts.d.g <-
  contact.fu.ts.all.ts.d.long |>
  ggplot(aes(x = date_suivi)) +
  geom_area(aes(y = perct, fill = type)) +
  geom_area(data = contact.fu.ts.all.ts.d1,
            aes(y = perct.perdu.de.vu, fill = "Perdu de vue"),
            alpha = 0.5) +
  #geom_line(data = contact.fu.ts.all.ts.d1,
   #         aes(y = perct.suspect, color = "% suspects"),
    #        linetype = 2, linewidth = 1) +
  #geom_line(data = contact.fu.ts.all.ts.d1,
   #         aes(y = perct.confirme, color = "% confirmés"),
    #        linetype = 6, linewidth = 1) +
  scale_fill_manual(values = visit.cols,
                    labels = c("perct.vu" = "Visites vues (% des attendues)",
                               "perct.non.vu" = "Visites attendues non vues",
                               "Perdu de vue" = "Perdu de vue")) +
  scale_color_manual(values = outcome.cols) +
  scale_y_continuous(expand = c(0.02, 0),
                     labels = scales::percent_format(scale = 1)) +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%d\n%b",
               expand = c(0.005, 0)) +
  labs(
    title = "Evolution des suivis (%) des contacts MVE/B par jour, RDC",
    subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"), "\n",
                      "Analyse basée sur ", ncontact.f.fu, " contacts"),
    fill = NULL, color = NULL,
    y = "Pourcentage de suivis",
    x = "Date de suivi",
    caption = paste0("IOA - CAI ", "\u00a9", isoyear(now()))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey90", linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90", linetype = 2),
        plot.title = element_text(colour = "#234a7d", face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.line = element_line(color = "grey25", size = 0.25),
        axis.ticks = element_line(size = 0.75, colour = "grey75"),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"))
)

if (!skip_output) {
contact.fu.ts.all.ts.d.g |>
  ggsave(file = paste0(day.dir.a, "ContactFU_ts_daily_",
                       format(max.fill.date, "%d%b"),
                       ".png"),
         dpi = 300, height = 20, width = 33, scale = 0.3)
}

### Contact follow-up terminated

contact.data_fu

names(contact.data_fu)

contact.data.fu <- contact.data_fu |>
  mutate(epiw = get_monday(date_debut_suivi),
         efu = date_dernier_contact_cas_source+21,
         n.suivi = as.numeric(date_fin_suivi-date_debut_suivi)+1,
         n.suivi.e = as.numeric(efu-date_debut_suivi)+1,
         LoFU = (efu - date_fin_suivi )>= 3 | nb_jr_consecutive_absence >= 3) |>
  filter(!is.na(date_debut_suivi),
          date_debut_suivi >= as.Date("2026-05-01"),
         date_dernier_contact_cas_source >= as.Date("2026-05-01"),
         efu <= max.fill.date) 

contact.data.fu.dfu <- contact.data.fu |>
  summarise(n.contacts.suivi = sum(!is.na(nb_jr_suivi), na.rm = TRUE),
            n.j.suivi = sum(n.suivi, na.rm = TRUE),
            n.j.suivi.e = sum(n.suivi.e, na.rm = TRUE),
            n.absence = sum(nb_jr_absence, na.rm = TRUE),
            perdu.de.vue= sum(nb_jr_consecutive_absence >= 3, na.rm = TRUE),
            contact.recycle = sum(contact.recycle, na.rm = TRUE),
            n.contacts.suivi.7 = sum(!is.na(nb_jr_suivi) & nb_jr_suivi <= 7, na.rm = TRUE),
            n.contacts.suivi.820 = sum(!is.na(nb_jr_suivi) & nb_jr_suivi > 7 & nb_jr_suivi <= 20, na.rm = TRUE),
            `n.contacts.suivi.21p` = sum(!is.na(nb_jr_suivi) & nb_jr_suivi > 20 , na.rm = TRUE),
            perdu.de.vue.efu = sum(LoFU, na.rm = TRUE),
            perdu.de.vue.p = round(sum(perdu.de.vue, na.rm = TRUE) / sum(!is.na(nb_jr_suivi), na.rm = TRUE) * 100, 1),
            nb_jr_suivi =round( median(nb_jr_suivi, na.rm = TRUE)),
             .by = c(epiw)) |>
  arrange(epiw)

contact.data.fu.dfu.long <-
  contact.data.fu.dfu |>
  select(epiw, n.contacts.suivi.7, 
         n.contacts.suivi.820, `n.contacts.suivi.21p`) |>
  pivot_longer(cols = -epiw,
               names_to = "suivi.cat", values_to = "n.contacts.suivi.cat") |>
  mutate(suivi.cat = factor(suivi.cat, levels = c( "n.contacts.suivi.21p", "n.contacts.suivi.820","n.contacts.suivi.7"),
                            labels = c( "21+ jours", "8-20 jours","1-7 jours")))

suivi.cols <- c("1-7 jours" = "#ea4e42", "8-20 jours" = "#f08649", "21+ jours" = "#3db96f")


max_n_contacts <- max(contact.data.fu.dfu$n.contacts.suivi, na.rm = TRUE)
pct_breaks <- pretty(c(0, 100), n = 6)
y_breaks_national <- pct_breaks / 100 * max_n_contacts

(contact.data.fu.dfu.g <-
  contact.data.fu.dfu |>
  ggplot()+
  geom_bar(data = contact.data.fu.dfu.long,
           aes(x = epiw, y = n.contacts.suivi.cat, fill = suivi.cat),
           stat = "identity", position = "stack", width=7)+
  geom_col(aes(x = epiw, y = perdu.de.vue.efu),
           fill = "#92929b1a", color="black",width=7)+
  geom_line(aes(x = epiw, y = perdu.de.vue.p * max_n_contacts / 100),
            color = "#2b090f", linewidth = 1)+
  geom_point(aes(x = epiw, y = perdu.de.vue.p * max_n_contacts / 100),
             color = "#2b090f", size = 2)+
  scale_fill_manual(values = suivi.cols)+
   scale_y_continuous(breaks = y_breaks_national,
                      labels = function(x) round(x),
                      expand = c(0.02,0),
                      sec.axis = sec_axis(~ . * 100 / max_n_contacts,
                                          name = "Perdu de vue",
                                          breaks = pct_breaks,
                                          labels = function(x) paste0(round(x), "%")))+
  scale_x_date(date_breaks = "1 week",
               date_labels = "%V\n%Y",
               expand = c(0.005,0))+
  labs(
    title = "Evolution de la durée de suivis des contacts MVE/B par semaine épidémiologique, RDC",
    subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"),"\n",
                      "Analyse basée sur ",sum(contact.data.fu.dfu$n.contacts.suivi),
                      " contacts ayant terminé le suivi","\n",
                      "Les colonnes transparentes aux contours noirs representes les contacts n'ayant pas été suivi","\n",
                      "jusqu'au jour 21 post exposition"),
    y = "Nombre de contacts",  x="Semaine épidémiologique de suivi",
    fill="Durée de suivi",
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now()))) +
    theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.title.y.right = element_text(color = "#2b090f", size = 13),
        axis.text.y.right = element_text(color = "#2b090f", size = 11),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 12),
        legend.position = c(0.125, 0.90),
        legend.key.size = unit(0.5, "cm"))
)

if (!skip_output) {
contact.data.fu.dfu.g |>
  ggsave(file = paste0(day.dir.a, "Duree_suivi_perdu_vue_",
                       format(max.fill.date, "%d%b"),
                       ".png"),
         dpi = 300, height = 20, width = 35, scale = 0.3)
}


### Contact follow-up terminated by zone de sante

contact.data.fu.dfu.zs <- contact.data.fu |>
  summarise(n.contacts.suivi = sum(!is.na(nb_jr_suivi), na.rm = TRUE),
            n.j.suivi = sum(n.suivi, na.rm = TRUE),
            n.j.suivi.e = sum(n.suivi.e, na.rm = TRUE),
            n.absence = sum(nb_jr_absence, na.rm = TRUE),
            perdu.de.vue = sum(nb_jr_consecutive_absence >= 3, na.rm = TRUE),
            contact.recycle = sum(contact.recycle, na.rm = TRUE),
            n.contacts.suivi.7 = sum(!is.na(nb_jr_suivi) & nb_jr_suivi <= 7, na.rm = TRUE),
            n.contacts.suivi.820 = sum(!is.na(nb_jr_suivi) & nb_jr_suivi > 7 & nb_jr_suivi <= 20, na.rm = TRUE),
            `n.contacts.suivi.21p` = sum(!is.na(nb_jr_suivi) & nb_jr_suivi > 20, na.rm = TRUE),
            perdu.de.vue.efu = sum(LoFU, na.rm = TRUE),
            perdu.de.vue.p = round(sum(perdu.de.vue, na.rm = TRUE) / sum(!is.na(nb_jr_suivi), na.rm = TRUE) * 100, 1),
            nb_jr_suivi = round(median(nb_jr_suivi, na.rm = TRUE)),
            .by = c(epiw, zone_sante_notification)) |>
  arrange(zone_sante_notification, epiw)

zs.fu.keep <- contact.data.fu.dfu.zs |>
  summarise(total.suivi = sum(n.contacts.suivi, na.rm = TRUE),
            .by = zone_sante_notification) |>
  filter(total.suivi >= 5) |>
  pull(zone_sante_notification)

contact.data.fu.dfu.zs.long <- contact.data.fu.dfu.zs |>
  filter(zone_sante_notification %in% zs.fu.keep) |>
  select(epiw, zone_sante_notification, n.contacts.suivi.7,
         n.contacts.suivi.820, `n.contacts.suivi.21p`) |>
  pivot_longer(cols = -c(epiw, zone_sante_notification),
               names_to = "suivi.cat", values_to = "n.contacts.suivi.cat") |>
  mutate(suivi.cat = factor(suivi.cat,
                            levels = c("n.contacts.suivi.21p", "n.contacts.suivi.820", "n.contacts.suivi.7"),
                            labels = c("21+ jours", "8-20 jours", "1-7 jours")))

if (!skip_output) {
for (zs in zs.fu.keep) {
  df_zs <- contact.data.fu.dfu.zs |>
    filter(zone_sante_notification == zs)

  df_zs_long <- contact.data.fu.dfu.zs.long |>
    filter(zone_sante_notification == zs)

  max_n_contacts_zs <- max(df_zs$n.contacts.suivi, na.rm = TRUE)
  pct_breaks_zs <- pretty(c(0, 100), n = 6)
  y_breaks_zs <- pct_breaks_zs / 100 * max_n_contacts_zs

  contact.data.fu.dfu.zs.g <- df_zs |>
    ggplot()+
    geom_bar(data = df_zs_long,
             aes(x = epiw, y = n.contacts.suivi.cat, fill = suivi.cat),
             stat = "identity", position = "stack", width = 7)+
    geom_col(aes(x = epiw, y = perdu.de.vue.efu),
             fill = "#92929b1a", color = "black", width = 7)+
    geom_line(aes(x = epiw, y = perdu.de.vue.p * max_n_contacts_zs / 100),
              color = "#2b090f", linewidth = 1)+
    geom_point(aes(x = epiw, y = perdu.de.vue.p * max_n_contacts_zs / 100),
               color = "#2b090f", size = 2)+
    scale_fill_manual(values = suivi.cols)+
    scale_y_continuous(breaks = y_breaks_zs,
                       labels = function(x) round(x),
                       expand = c(0.02, 0),
                       sec.axis = sec_axis(~ . * 100 / max_n_contacts_zs,
                                           name = "Perdu de vue",
                                           breaks = pct_breaks_zs,
                                           labels = function(x) paste0(round(x), "%")))+
    scale_x_date(date_breaks = "1 week",
                 date_labels = "%V\n%Y",
                 expand = c(0.005, 0))+
    labs(
      title = paste0("Evolution de la durée de suivis des contacts MVE/B — ", zs),
      subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"), "\n",
                        "Analyse basée sur ", sum(df_zs$n.contacts.suivi),
                        " contacts ayant terminé le suivi", "\n",
                        "Les colonnes transparentes aux contours noirs representes les contacts n'ayant pas été suivi","\n",
                        "jusqu'au jour 21 post exposition"),
      y = "Nombre de contacts", x = "Semaine épidémiologique de suivi",
      fill="Durée de suivi",
      caption = paste0("IOA - CAI ", "\u00a9", isoyear(now()))) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey90",
                                            linetype = 2),
          panel.grid.major.x = element_line(colour = "grey90",
                                            linetype = 2),
          plot.title = element_text(colour = "#234a7d",
                                    face = "bold",
                                    size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 7),
          axis.title = element_text(size = 13),
          axis.title.y.right = element_text(color = "#2b090f", size = 13),
          axis.text.y.right = element_text(color = "#2b090f", size = 11),
          axis.line = element_line(color = "grey25",
                                   size = 0.25),
          axis.ticks = element_line(size = 0.75,
                                    colour = "grey75"),
          axis.text = element_text(size = 12),
          legend.position = c(0.125, 0.90),
          legend.key.size = unit(0.5, "cm"))

  contact.data.fu.dfu.zs.g |>
    ggsave(file = paste0(day.dir.z, "Duree_suivi_perdu_vue_",
                         clean_filename(zs), "_",
                         format(max.fill.date, "%d%b"),
                         ".png"),
           dpi = 300, height = 20, width = 30, scale = 0.3)
}
}


### Contact with sumptoms

contact.data_fu.symptoms <- contact.data_fu |>
  filter(!is.na(date_debut_suivi),
         date_debut_suivi >= as.Date("2026-05-01")) |>
  mutate(epiw = get_monday(date_debut_suivi)) |>
  summarise(n.contacts = n(),        
            devenu.suspect = sum(devenu.suspect, na.rm = TRUE),
            devenu.confirme = sum(devenu.confirme, na.rm = TRUE),
          .by = c(epiw)) |>
  mutate(perct.suspect = round(devenu.suspect/n.contacts*100,1),
         perct.confirme = round(devenu.confirme/n.contacts*100,1)) |>
  arrange(epiw)


contact.data_fu.symptoms.g <-
  contact.data_fu.symptoms |>
  ggplot(aes(x= epiw))+
  geom_area(aes(y=perct.suspect, fill="Devenu suspect"), alpha=0.7)+
  geom_area(aes(y=perct.confirme, fill="Devenu confirmé"), alpha=0.7)+
  scale_fill_manual(values = c("Devenu suspect" = "#f08649", "Devenu confirmé" = "#ea4e42"))+
  scale_y_continuous(breaks = seq(0, 100, 10),
                     limits = c(0, 100),
                     expand = c(0.02,0),
                     labels = scales::percent_format(scale = 1, accuracy = 1))+
  scale_x_date(date_breaks = "1 week",
               date_labels = "%V\n%Y",
               expand = c(0.005,0))+
  labs(
    title = "Contacts MVE/B devenus suspects et confirmés par semaine épidémiologique, RDC",
    subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"),"\n",
                      "Analyse basée sur ",sum(contact.data_fu.symptoms$n.contacts),
                      " contacts avec au moins un jour de suivi"),
    y = "Pourcentage",  x="Semaine épidémiologique de suivi",
    fill="Durée de suivi",
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now()))) +
    theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.title.y.right = element_text(color = "#2b090f", size = 13),
        axis.text.y.right = element_text(color = "#2b090f", size = 11),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 12),
        legend.position = c(0.125, 0.90),
        legend.key.size = unit(0.5, "cm"))

if (!skip_output) {
contact.data_fu.symptoms.g |>
  ggsave(file = paste0(day.dir.a, "Contacts_devenus_suspect_confirme_",
                       format(max.fill.date, "%d%b"),
                       ".png"),
         dpi = 300, height = 20, width = 35, scale = 0.3)
}


### Contact with symptoms by zone de sante

contact.data_fu.symptoms.zs <- contact.data_fu |>
  filter(!is.na(date_debut_suivi),
         !is.na(zone_sante_notification),
         date_debut_suivi >= as.Date("2026-05-01")) |>
  mutate(epiw = get_monday(date_debut_suivi)) |>
  summarise(n.contacts = n(),
            devenu.suspect = sum(devenu.suspect, na.rm = TRUE),
            devenu.confirme = sum(devenu.confirme, na.rm = TRUE),
            .by = c(epiw, zone_sante_notification)) |>
  mutate(perct.suspect = round(devenu.suspect / n.contacts * 100, 1),
         perct.confirme = round(devenu.confirme / n.contacts * 100, 1)) |>
  arrange(zone_sante_notification, epiw)

zs.symptoms.keep <- contact.data_fu.symptoms.zs |>
  summarise(total.contacts = sum(n.contacts, na.rm = TRUE),
            .by = zone_sante_notification) |>
  filter(total.contacts >= 5) |>
  pull(zone_sante_notification)

symptoms.cols <- c("Devenu suspect" = "#f08649", "Devenu confirmé" = "#ea4e42")
zs="Bunia"

if (!skip_output) {
for (zs in zs.symptoms.keep) {
  df_zs_sym <- contact.data_fu.symptoms.zs |>
    filter(zone_sante_notification == zs)

  contact.data_fu.symptoms.zs.g <- df_zs_sym |>
    ggplot(aes(x = epiw)) +
    geom_area(aes(y = perct.suspect, fill = "Devenu suspect"), alpha = 0.7) +
    geom_area(aes(y = perct.confirme, fill = "Devenu confirmé"), alpha = 0.7) +
    scale_fill_manual(values = symptoms.cols) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       limits = c(0, 100),
                       expand = c(0.02, 0),
                       labels = scales::percent_format(scale = 1, accuracy = 1)) +
    scale_x_date(date_breaks = "1 week",
                date_labels = "%V\n%Y",
                expand = c(0.005, 0)) +
    labs(
      title = paste0("Contacts MVE/B devenus suspects et confirmés — ", zs),
      subtitle = paste0("Données à la date du ", format(max.fill.date, "%d-%m-%Y"), "\n",
                        "Analyse basée sur ", sum(df_zs_sym$n.contacts),
                        " contacts avec au moins un jour de suivi"),
      y = "Pourcentage", x = "Semaine épidémiologique de suivi",
      fill = "Durée de suivi",
      caption = paste0("IOA - CAI ", "\u00a9", isoyear(now()))) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey90", linetype = 2),
          panel.grid.major.x = element_line(colour = "grey90", linetype = 2),
          plot.title = element_text(colour = "#234a7d", face = "bold", size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 7),
          axis.title = element_text(size = 13),
          axis.title.y.right = element_text(color = "#2b090f", size = 13),
          axis.text.y.right = element_text(color = "#2b090f", size = 11),
          axis.line = element_line(color = "grey25", size = 0.25),
          axis.ticks = element_line(size = 0.75, colour = "grey75"),
          axis.text = element_text(size = 12),
          legend.position = c(0.125, 0.90),
          legend.key.size = unit(0.5, "cm"))

  contact.data_fu.symptoms.zs.g |>
    ggsave(file = paste0(day.dir.z, "Contacts_symptomes_zs_",
                         clean_filename(zs), "_",
                         format(max.fill.date, "%d%b"),
                         ".png"),
           dpi = 300, height = 20, width = 30, scale = 0.3)
}
}


### Last 21 days follow-up

contact.data.fu.21 <- contact.data_fu |>
  mutate(epiw = get_monday(date_dernier_contact_cas_source),
         efu = date_dernier_contact_cas_source+21,
         n.suivi = as.numeric(date_fin_suivi-date_debut_suivi)+1,
         n.suivi.e = as.numeric(efu-date_debut_suivi)+1,
         LoFU = (efu - date_fin_suivi )>= 3 | nb_jr_consecutive_absence >= 3) |>
  filter(!is.na(date_debut_suivi),
          date_debut_suivi >= max.fill.date-21) 


contact.data.fu.21.dfu <- contact.data.fu.21 |>
  summarise(n.contacts.suivi = sum(!is.na(nb_jr_suivi), na.rm = TRUE),
            n.j.suivi = sum(n.suivi, na.rm = TRUE),
            n.j.suivi.e = sum(n.suivi.e, na.rm = TRUE),
            n.absence = sum(nb_jr_absence, na.rm = TRUE),
            perdu.de.vue= sum(nb_jr_consecutive_absence >= 3, na.rm = TRUE),
            contact.recycle = sum(contact.recycle, na.rm = TRUE),
            n.contacts.suivi.7 = sum(!is.na(nb_jr_suivi) & nb_jr_suivi <= 7, na.rm = TRUE),
            n.contacts.suivi.820 = sum(!is.na(nb_jr_suivi) & nb_jr_suivi > 7 & nb_jr_suivi <= 20, na.rm = TRUE),
            `n.contacts.suivi.21p` = sum(!is.na(nb_jr_suivi) & nb_jr_suivi > 20 , na.rm = TRUE),
            perdu.de.vue.efu = sum(LoFU, na.rm = TRUE),
            perdu.de.vue.p = round(sum(perdu.de.vue, na.rm = TRUE) / sum(!is.na(nb_jr_suivi), na.rm = TRUE) * 100, 1),
            nb_jr_suivi =round( median(nb_jr_suivi, na.rm = TRUE)),
             .by = c(epiw)) |>
  arrange(epiw)


### ============================================================================
### Analyse des contacts sur les 3 dernières semaines (21 jours)
### ============================================================================
### Fenêtre d'analyse : enrolement_date dans [max.fill.date - 20, max.fill.date]
###   -> enrolement_date sert UNIQUEMENT à filtrer la cohorte.
### Toutes les métriques (fu_start, efu, fu_day, semaines, attendus,
### complétude, perdus de vue) sont dérivées du format LONG (contact.data.long),
### ancrées sur date_dernier_contact_cas_source (exposition).
### Niveaux : Ensemble, Province, Zone de santé.

## ---- Vérification de la colonne de filtrage ----

stopifnot("enrolement_date" %in% names(contact.data.long))

## ---- Fenêtre d'analyse (3 semaines = 21 jours) ----

ref.date  <- max.fill.date
win.start <- ref.date - 20          # 21 jours = 3 semaines

## ---- Préparation des données longues (1 ligne / visite) ----
## Filtrage par enrolement_date uniquement ; toutes les métriques sont
## dérivées des colonnes du format long.


table(contact.data.long$contact_vu, useNA = "ifany")

contact.3w.long <- contact.data.long |>
  mutate(enrolement_date = as.Date(enrolement_date)) |>
  filter(enrolement_date >= win.start & enrolement_date <= ref.date) |>
  mutate(name_id = paste0(nom_prenom_contact, "_", id_contact),
         date_suivi = as.Date(follow_up_date),  # follow_up_date replaces date_suivi (refactored Aug 2026)
         expo_date = as.Date(date_dernier_contact_cas_source),
         fu_day = as.numeric(date_suivi - expo_date),
         fu_week = case_when(fu_day <= 7 ~ "W1",
                             fu_day > 7 & fu_day <= 14 ~ "W2",
                             fu_day > 14  & fu_day <= 21 ~ "W3",
                             .default = NA),
         contact_vu = ifelse(is.na(contact_vu), "Non", contact_vu),
         vu = contact_vu == "Oui",
         vu.expected = !is.na(expo_date) & !is.na(date_suivi) &
                       date_suivi >= expo_date + 1)

## ---- Fonction : indicateurs des 3 dernières semaines ----
## Tous les indicateurs sont dérivés du format long (1 ligne / visite) :
## résumé par contact, puis agrégation par niveau.

contact_indicators_3w <- function(long, ref.date, group_vars = character(),
                                 wide_total = NULL) {

  pct <- function(num, den) ifelse(is.na(den) | den == 0, NA_real_,
                                   round(num / den * 100, 1))

  safe_min_date <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) as.Date(NA) else as.Date(min(x))
  }
  safe_max_date <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) as.Date(NA) else as.Date(max(x))
  }
  first_non_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else x[1]
  }

  use_grp <- length(group_vars) > 0
  if (!use_grp) {
    long <- long |> mutate(.grp = "Ensemble")
    grp <- ".grp"
  } else {
    grp <- group_vars
  }

  # Dénominateur total issu du format large (1 ligne / contact) :
  #   - data.frame : total global (nrow) ou totaux par groupe (count + join)
  #   - scalaire   : total global fixe, uniquement sans regroupement
  if (is.data.frame(wide_total)) {
    if (use_grp) {
      wide_total <- wide_total |>
        summarise(n.total.contacts = n(), .by = all_of(grp))
    } else {
      wide_total <- tibble(.grp = "Ensemble",
                           n.total.contacts = as.numeric(nrow(wide_total)))
    }
  } else if (!is.null(wide_total)) {
    if (use_grp) {
      stop("`wide_total` scalaire non supporté avec `group_vars` : ",
           "passez le tableau large (ex. contact.data_fu) pour des totaux par groupe.")
    }
    wide_total <- as.numeric(wide_total)
  }

  # Résumé par contact (1 ligne / contact) dérivé des visites (format long)
  contact.summary <- long |>
    filter(!is.na(name_id)) |>
    summarise(
      n.achieved   = sum(vu, na.rm = TRUE),
      vu.once      = n.achieved > 0,
      vu.wk1       = any(vu & fu_week == "W1"),
      vu.wk2       = any(vu & fu_week == "W2"),
      vu.wk3       = any(vu & fu_week == "W3"),
      province_notification   = first_non_na(province_notification),
      zone_sante_notification = first_non_na(zone_sante_notification),
      nom_post_nom_prenom_cas = first_non_na(nom_post_nom_prenom_cas),
      expo_date      = safe_min_date(date_dernier_contact_cas_source),
      date_fin_suivi = safe_max_date(date_suivi),
      .by = name_id
    ) |>
    mutate(fu_start = expo_date + 1,                       # jour 1 de suivi
           efu = expo_date + 21,                           # jour 21 de suivi
           n.expected = pmax(0, as.numeric(efu - fu_start) + 1),
           LoFU = !is.na(date_fin_suivi) & !is.na(efu) &
                  efu < ref.date & ((efu - date_fin_suivi) >= 3),
           fu_week1.expected = !is.na(fu_start) & as.numeric(ref.date - fu_start) <= 7,
           fu_week2.expected = !is.na(fu_start) & as.numeric(ref.date - fu_start) > 7 &
                                 as.numeric(ref.date - fu_start) <= 14,
           fu_week3.expected = !is.na(fu_start) & as.numeric(ref.date - fu_start) > 14 &
                                 as.numeric(ref.date - fu_start) <= 21,
           completed.den = !is.na(efu) & n.achieved > 0 & efu <= ref.date)

  # Le niveau "Ensemble" (groupe constant) est ajouté au résumé par contact
  if (!use_grp) contact.summary <- contact.summary |> mutate(.grp = "Ensemble")

  out <- contact.summary |>
    mutate(n.achieved = ifelse(is.na(n.achieved), 0L, n.achieved),
           vu.once    = ifelse(is.na(vu.once), FALSE, vu.once),
           vu.wk1     = ifelse(is.na(vu.wk1), FALSE, vu.wk1),
           vu.wk2     = ifelse(is.na(vu.wk2), FALSE, vu.wk2),
           vu.wk3     = ifelse(is.na(vu.wk3), FALSE, vu.wk3)) |>
    summarise(
      n.contacts = n(),
      n.source.cases = length(unique(nom_post_nom_prenom_cas[!is.na(nom_post_nom_prenom_cas)])),
      ratio.contact.case = round(n.contacts / n.source.cases, 1),
      n.expected.visits  = sum(n.expected, na.rm = TRUE),
      n.achieved.visits  = sum(n.achieved, na.rm = TRUE),
      n.unachieved.visits = sum(n.expected, na.rm = TRUE) - sum(n.achieved, na.rm = TRUE),
      contacts.saw.once = sum(vu.once),
      pct.followed.once = pct(contacts.saw.once, n.contacts),
      pct.visits = pct(n.achieved.visits, n.expected.visits),
      pct.wk1 = pct(sum(vu.wk1), sum(fu_week1.expected)),
      pct.wk2 = pct(sum(vu.wk2), sum(fu_week2.expected)),
      pct.wk3 = pct(sum(vu.wk3), sum(fu_week3.expected)),
      completed.expected = sum(completed.den, na.rm = TRUE),
      completed.actual = sum(!is.na(date_fin_suivi) & date_fin_suivi >= efu, na.rm = TRUE),
      n.lofu = sum(LoFU, na.rm = TRUE),
      pct.completed = pct(completed.actual, completed.expected),
      pct.completed.21 = pct(completed.actual, contacts.saw.once),
      pct.lofu = pct(sum(LoFU, na.rm = TRUE), contacts.saw.once),
      .by = all_of(grp)
    ) |>
    mutate(ratio.contact.case = ifelse(is.na(ratio.contact.case) |
                                        !is.finite(ratio.contact.case), NA, ratio.contact.case))

  # Ajout du dénominateur total (global ou par groupe) puis % "du total"
  if (is.data.frame(wide_total)) {
    out <- out |> left_join(wide_total, by = grp)
  } else if (!is.null(wide_total)) {
    out <- out |> mutate(n.total.contacts = wide_total)
  }

  if (!is.null(wide_total)) {
    out <- out |>
      mutate(pct.followed.once.total = pct(contacts.saw.once, n.total.contacts))
  } else {
    out <- out |> mutate(pct.followed.once.total = NA_real_)
  }

  if (!use_grp) out <- out |> select(-.grp)
  out
}

## ---- Indicateurs : Ensemble / Province / Zone de santé ----

(contact.3w.overall <- contact_indicators_3w(contact.3w.long, ref.date,
                                             wide_total = contact.data_fu))

(contact.3w.prov <- contact_indicators_3w(contact.3w.long, ref.date,
                                         c("province_notification"),
                                         wide_total = contact.data_fu))

(contact.3w.zs <- contact_indicators_3w(contact.3w.long, ref.date,
                                       c("province_notification", "zone_sante_notification"),
                                       wide_total = contact.data_fu))

## ---- Cas confirmés (EVD) : Ensemble / Province / Zone de santé ----
## Même fenêtre de 3 semaines (date d'enrôlement du cas), Kasai exclu.

evd.conf.3w <- evd.data |>
  mutate(enrol_date = as.Date(.data[[evd.enrol.col]])) |>
  filter(lab_resultat_final == "Positif" | classification_finale == "Cas confirmé",
         !is.na(enrol_date),
         enrol_date >= win.start & enrol_date <= ref.date,
         province_notification != "Kasai")

evd.conf.3w.all  <- evd.conf.3w |> summarise(n.confirmed.cases = n())
evd.conf.3w.prov <- evd.conf.3w |> summarise(n.confirmed.cases = n(),
                                             .by = province_notification)
evd.conf.3w.zs   <- evd.conf.3w |> summarise(n.confirmed.cases = n(),
                                             .by = c(province_notification,
                                                     zone_sante_notification))

contact.3w.overall <- contact.3w.overall |>
  mutate(n.confirmed.cases = evd.conf.3w.all$n.confirmed.cases,
         ratio.case.confirmed = ifelse(n.confirmed.cases > 0,
                              round(n.source.cases / n.confirmed.cases, 1),
                               NA_real_))

contact.3w.prov <- contact.3w.prov |>
  left_join(evd.conf.3w.prov, by = "province_notification") |>
  mutate(n.confirmed.cases = tidyr::replace_na(n.confirmed.cases, 0L),
         ratio.case.confirmed = ifelse(n.confirmed.cases > 0,
                              round(n.source.cases / n.confirmed.cases, 1),
                               NA_real_))

contact.3w.zs <- contact.3w.zs |>
  left_join(evd.conf.3w.zs,
            by = c("province_notification", "zone_sante_notification")) |>
  mutate(n.confirmed.cases = tidyr::replace_na(n.confirmed.cases, 0L),
         ratio.case.confirmed = ifelse(n.confirmed.cases > 0,
                              round(n.source.cases / n.confirmed.cases, 1),
                               NA_real_))

## Contrôles de cohérence des décomptes de cas confirmés
if (contact.3w.overall$n.confirmed.cases != sum(contact.3w.prov$n.confirmed.cases)) {
  warning("Total cas confirmés (", contact.3w.overall$n.confirmed.cases,
          ") != somme par province (", sum(contact.3w.prov$n.confirmed.cases),
          ") : cas confirmés dans des provinces sans contacts dans la fenêtre.")
}
if (sum(contact.3w.prov$n.confirmed.cases) != sum(contact.3w.zs$n.confirmed.cases)) {
  warning("Somme des cas confirmés par province (", sum(contact.3w.prov$n.confirmed.cases),
          ") != somme par zone de santé (", sum(contact.3w.zs$n.confirmed.cases),
          ") : cas avec province/ZS non renseignée.")
}

## ---- Séries quotidiennes (visites réalisées / non réalisées / perdus de vue) ----

contact.3w.long.ts <- contact.3w.long |>
  mutate(LoFU.day = flag_lofu(contact_vu, group = name_id, sort_by = date_suivi))

contact.3w.ts <- contact.3w.long.ts |>
  filter(date_suivi >= win.start & date_suivi <= ref.date, !is.na(date_suivi)) |>
  summarise(achieved   = sum(vu, na.rm = TRUE),
            unachieved = sum(!vu, na.rm = TRUE),
            lofu       = sum(LoFU.day, na.rm = TRUE),
            .by = date_suivi) |>
  tidyr::complete(date_suivi = seq(win.start, ref.date, by = "1 day"),
                  fill = list(achieved = 0, unachieved = 0, lofu = 0))

contact.3w.ts.prov <- contact.3w.long.ts |>
  filter(date_suivi >= win.start & date_suivi <= ref.date,
         !is.na(date_suivi), !is.na(province_notification)) |>
  summarise(achieved   = sum(vu, na.rm = TRUE),
            unachieved = sum(!vu, na.rm = TRUE),
            lofu       = sum(LoFU.day, na.rm = TRUE),
            .by = c(province_notification, date_suivi)) |>
  tidyr::complete(province_notification,
                  date_suivi = seq(win.start, ref.date, by = "1 day"),
                  fill = list(achieved = 0, unachieved = 0, lofu = 0))

contact.3w.ts.zs <- contact.3w.long.ts |>
  filter(date_suivi >= win.start & date_suivi <= ref.date,
         !is.na(date_suivi), !is.na(zone_sante_notification)) |>
  summarise(achieved   = sum(vu, na.rm = TRUE),
            unachieved = sum(!vu, na.rm = TRUE),
            lofu       = sum(LoFU.day, na.rm = TRUE),
            .by = c(zone_sante_notification, date_suivi)) |>
  tidyr::complete(zone_sante_notification,
                  date_suivi = seq(win.start, ref.date, by = "1 day"),
                  fill = list(achieved = 0, unachieved = 0, lofu = 0))

## ---- Test de tendance de Mann-Kendall sur les séries quotidiennes ----

contact_mk_test <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 4) return(tibble(tau = NA_real_, p_value = NA_real_, n = length(x)))
  mk <- tryCatch(Kendall::MannKendall(x), error = function(e) NULL)
  if (is.null(mk)) {
    tibble(tau = NA_real_, p_value = NA_real_, n = length(x))
  } else {
    tibble(tau = round(mk$tau[1], 3), p_value = round(mk$sl[1], 4), n = length(x))
  }
}

contact.3w.mk <- contact.3w.ts |>
  tidyr::pivot_longer(cols = c(achieved, unachieved, lofu),
                      names_to = "metric", values_to = "value") |>
  reframe(contact_mk_test(value), .by = metric) |>
  mutate(trend_direction = case_when(
    is.na(p_value) ~ "Inconnue",
    p_value < 0.05 & tau > 0 ~ "À la hausse",
    p_value < 0.05 & tau < 0 ~ "À la baisse",
    .default = "Stable"))

contact.3w.mk.prov <- contact.3w.ts.prov |>
  tidyr::pivot_longer(cols = c(achieved, unachieved, lofu),
                      names_to = "metric", values_to = "value") |>
  reframe(contact_mk_test(value), .by = c(province_notification, metric)) |>
  mutate(trend_direction = case_when(
    is.na(p_value) ~ "Inconnue",
    p_value < 0.05 & tau > 0 ~ "À la hausse",
    p_value < 0.05 & tau < 0 ~ "À la baisse",
    .default = "Stable")) |>
  arrange(province_notification, metric)

contact.3w.mk.zs <- contact.3w.ts.zs |>
  tidyr::pivot_longer(cols = c(achieved, unachieved, lofu),
                      names_to = "metric", values_to = "value") |>
  reframe(contact_mk_test(value), .by = c(zone_sante_notification, metric)) |>
  mutate(trend_direction = case_when(
    is.na(p_value) ~ "Inconnue",
    p_value < 0.05 & tau > 0 ~ "À la hausse",
    p_value < 0.05 & tau < 0 ~ "À la baisse",
    .default = "Stable")) |>
  arrange(zone_sante_notification, metric)

## ---- Graphiques : tendance quotidienne des visites ----

contact.3w.cols <- c("achieved" = "#65bfed", "unachieved" = "#fb838d", "lofu" = "#2f1c1c")
contact.3w.labs <- c(achieved = "Visites réalisées",
                     unachieved = "Visites non réalisées",
                     lofu = "Perdus de vue")

contact.3w.ts.long <- contact.3w.ts |>
  tidyr::pivot_longer(cols = c(achieved, unachieved, lofu),
                      names_to = "type", values_to = "n")

(contact.3w.ts.g <- contact.3w.ts.long |>
  ggplot(aes(x = date_suivi, y = n, color = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = contact.3w.cols, labels = contact.3w.labs) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d\n%b",
               expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.02, 0)) +
  labs(
    title = "Suivi quotidien des contacts MVE/B — 3 dernières semaines, RDC",
    subtitle = paste0("Données à la date du ", format(ref.date, "%d-%m-%Y"), "\n",
                      "Cohorte : contacts enrôlés entre le ",
                      format(win.start, "%d-%m-%Y"), " et le ", format(ref.date, "%d-%m-%Y"),
                      " (n = ", contact.3w.overall$n.contacts, " contacts)"),
    color = NULL, x = "Date de suivi", y = "Nombre de visites",
    caption = paste0("IOA - CAI ", "\u00a9", isoyear(now()))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey90", linetype = 2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "#234a7d", face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.line = element_line(color = "grey25", size = 0.25),
        axis.ticks = element_line(size = 0.75, colour = "grey75"),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"))
)

contact.3w.ts.prov.long <- contact.3w.ts.prov |>
  tidyr::pivot_longer(cols = c(achieved, unachieved, lofu),
                      names_to = "type", values_to = "n")

(contact.3w.ts.prov.g <- contact.3w.ts.prov.long |>
  ggplot(aes(x = date_suivi, y = n, color = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  facet_wrap(~ province_notification, scales = "free_y") +
  scale_color_manual(values = contact.3w.cols, labels = contact.3w.labs) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d\n%b",
               expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.02, 0)) +
  labs(
    title = "Suivi quotidien des contacts MVE/B par province — 3 dernières semaines, RDC",
    subtitle = paste0("Données à la date du ", format(ref.date, "%d-%m-%Y")),
    color = NULL, x = "Date de suivi", y = "Nombre de visites",
    caption = paste0("IOA - CAI ", "\u00a9", isoyear(now()))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey90", linetype = 2),
        plot.title = element_text(colour = "#234a7d", face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.line = element_line(color = "grey25", size = 0.25),
        axis.text = element_text(size = 10),
        strip.background = element_rect(fill = "#eaf1fb", color = NA),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"))
)

contact.3w.ts.zs.long <- contact.3w.ts.zs |>
  tidyr::pivot_longer(cols = c(achieved, unachieved, lofu),
                      names_to = "type", values_to = "n")

(contact.3w.ts.zs.g <- contact.3w.ts.zs.long |>
  ggplot(aes(x = date_suivi, y = n, color = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  facet_wrap(~ zone_sante_notification, scales = "free_y") +
  scale_color_manual(values = contact.3w.cols, labels = contact.3w.labs) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d\n%b",
               expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.02, 0)) +
  labs(
    title = "Suivi quotidien des contacts MVE/B par zone de santé — 3 dernières semaines, RDC",
    subtitle = paste0("Données à la date du ", format(ref.date, "%d-%m-%Y")),
    color = NULL, x = "Date de suivi", y = "Nombre de visites",
    caption = paste0("IOA - CAI ", "\u00a9", isoyear(now()))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey90", linetype = 2),
        plot.title = element_text(colour = "#234a7d", face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 7),
        axis.title = element_text(size = 13),
        axis.line = element_line(color = "grey25", size = 0.25),
        axis.text = element_text(size = 10),
        strip.background = element_rect(fill = "#eaf1fb", color = NA),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"))
)

## ---- Export des sorties (désactivé en mode rapport) ----

if (!skip_output) {

  ## Tables HTML (kableExtra) — Ensemble
  contact.3w.overall.tbl <- contact.3w.overall |>
    rename(`Cas confirmés` = n.confirmed.cases,
           `Contacts` = n.contacts,
           `Cas sources` = n.source.cases,
           `Ratio contacts/cas` = ratio.contact.case,
           `Ratio cas sources/confirmés` = ratio.case.confirmed,
           `Visites attendues` = n.expected.visits,
           `Visites non réalisées` = n.unachieved.visits,
           `Visites réalisées (n)` = n.achieved.visits,
           `Visites réalisées (%)` = pct.visits,
           `Contacts suivis >= 1 fois (%)` = pct.followed.once,
           `Contacts suivis >= 1 fois (%) du total` = pct.followed.once.total,
           `Semaine 1 (%)` = pct.wk1,
           `Semaine 2 (%)` = pct.wk2,
           `Semaine 3 (%)` = pct.wk3,
           `Suivi complété (%)` = pct.completed.21,
           `Perdus de vue (%)` = pct.lofu) |>
    select(`Cas confirmés`, `Contacts`, `Cas sources`,
           `Ratio contacts/cas`, `Ratio cas sources/confirmés`,
           `Visites attendues`, `Visites non réalisées`, `Visites réalisées (n)`, `Visites réalisées (%)`,
           `Contacts suivis >= 1 fois (%)`, `Contacts suivis >= 1 fois (%) du total`,
           `Semaine 1 (%)`, `Semaine 2 (%)`, `Semaine 3 (%)`,
           `Suivi complété (%)`, `Perdus de vue (%)`) |>
    kbl(escape = FALSE, align = "c",
        caption = paste0("Indicateurs de suivi des contacts — 3 dernières semaines (Ensemble), ",
                         format(ref.date, "%d %B %Y"))) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
    add_header_above(c("Indicateurs de suivi des contacts — 3 dernières semaines" = 16))

  ## Tables HTML (kableExtra) — Province
  contact.3w.prov.tbl <- contact.3w.prov |>
    rename(Province = province_notification,
           `Cas confirmés` = n.confirmed.cases,
           `Contacts` = n.contacts,
           `Cas sources` = n.source.cases,
           `Ratio contacts/cas` = ratio.contact.case,
           `Ratio cas sources/confirmés` = ratio.case.confirmed,
           `Visites attendues` = n.expected.visits,
           `Visites non réalisées` = n.unachieved.visits,
           `Visites réalisées (n)` = n.achieved.visits,
           `Visites réalisées (%)` = pct.visits,
           `Contacts suivis >= 1 fois (%)` = pct.followed.once,
           `Contacts suivis >= 1 fois (%) du total` = pct.followed.once.total,
           `Semaine 1 (%)` = pct.wk1,
           `Semaine 2 (%)` = pct.wk2,
           `Semaine 3 (%)` = pct.wk3,
           `Suivi complété (%)` = pct.completed.21,
           `Perdus de vue (%)` = pct.lofu) |>
    select(Province, `Cas confirmés`, `Contacts`, `Cas sources`,
           `Ratio contacts/cas`, `Ratio cas sources/confirmés`,
           `Visites attendues`, `Visites non réalisées`, `Visites réalisées (n)`, `Visites réalisées (%)`,
           `Contacts suivis >= 1 fois (%)`, `Contacts suivis >= 1 fois (%) du total`,
           `Semaine 1 (%)`, `Semaine 2 (%)`, `Semaine 3 (%)`,
           `Suivi complété (%)`, `Perdus de vue (%)`) |>
    kbl(escape = FALSE, align = "c",
        caption = paste0("Indicateurs de suivi des contacts par province — 3 dernières semaines, ",
                         format(ref.date, "%d %B %Y"))) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
    add_header_above(c(" " = 1, "Indicateurs de suivi des contacts" = 16))

  ## Tables HTML (kableExtra) — Zone de santé
  contact.3w.zs.tbl <- contact.3w.zs |>
    rename(Province = province_notification,
           `Zone de Santé` = zone_sante_notification,
           `Cas confirmés` = n.confirmed.cases,
           `Contacts` = n.contacts,
           `Cas sources` = n.source.cases,
           `Ratio contacts/cas` = ratio.contact.case,
           `Ratio cas sources/confirmés` = ratio.case.confirmed,
           `Visites attendues` = n.expected.visits,
           `Visites non réalisées` = n.unachieved.visits,
           `Visites réalisées (n)` = n.achieved.visits,
           `Visites réalisées (%)` = pct.visits,
           `Contacts suivis >= 1 fois (%)` = pct.followed.once,
           `Contacts suivis >= 1 fois (%) du total` = pct.followed.once.total,
           `Semaine 1 (%)` = pct.wk1,
           `Semaine 2 (%)` = pct.wk2,
           `Semaine 3 (%)` = pct.wk3,
           `Suivi complété (%)` = pct.completed.21,
           `Perdus de vue (%)` = pct.lofu) |>
    select(Province, `Zone de Santé`,
           `Cas confirmés`, `Contacts`, `Cas sources`,
           `Ratio contacts/cas`, `Ratio cas sources/confirmés`,
           `Visites attendues`, `Visites non réalisées`, `Visites réalisées (n)`, `Visites réalisées (%)`,
           `Contacts suivis >= 1 fois (%)`, `Contacts suivis >= 1 fois (%) du total`,
           `Semaine 1 (%)`, `Semaine 2 (%)`, `Semaine 3 (%)`,
           `Suivi complété (%)`, `Perdus de vue (%)`) |>
    kbl(escape = FALSE, align = "c",
        caption = paste0("Indicateurs de suivi des contacts par zone de santé — 3 dernières semaines, ",
                         format(ref.date, "%d %B %Y"))) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
    add_header_above(c(" " = 2, "Indicateurs de suivi des contacts" = 16))

  contact.3w.overall.tbl |> save_kable(file = paste0(day.dir.a, "Indicateurs_3w_ensemble_",
                                                     format(ref.date, "%d%b"), ".html"))
  contact.3w.prov.tbl    |> save_kable(file = paste0(day.dir.p, "Indicateurs_3w_prov_",
                                                     format(ref.date, "%d%b"), ".html"))
  contact.3w.zs.tbl      |> save_kable(file = paste0(day.dir.z, "Indicateurs_3w_zs_",
                                                     format(ref.date, "%d%b"), ".html"))

  ## Tables Mann-Kendall (HTML)
  contact.3w.mk.tbl <- contact.3w.mk |>
    mutate(metric = recode(metric, achieved = "Visites réalisées",
                           unachieved = "Visites non réalisées",
                           lofu = "Perdus de vue")) |>
    rename(`Métrique` = metric, `Tau de Kendall` = tau, `p-value` = p_value,
           `n (jours)` = n, `Tendance` = trend_direction) |>
    kbl(escape = FALSE, align = "c",
        caption = paste0("Test de tendance de Mann-Kendall — 3 dernières semaines (Ensemble), ",
                         format(ref.date, "%d %B %Y"))) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

  contact.3w.mk.prov.tbl <- contact.3w.mk.prov |>
    mutate(metric = recode(metric, achieved = "Visites réalisées",
                           unachieved = "Visites non réalisées",
                           lofu = "Perdus de vue")) |>
    rename(Province = province_notification, `Métrique` = metric,
           `Tau de Kendall` = tau, `p-value` = p_value, `n (jours)` = n,
           `Tendance` = trend_direction) |>
    kbl(escape = FALSE, align = "c",
        caption = paste0("Test de tendance de Mann-Kendall par province — 3 dernières semaines, ",
                         format(ref.date, "%d %B %Y"))) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

  contact.3w.mk.zs.tbl <- contact.3w.mk.zs |>
    mutate(metric = recode(metric, achieved = "Visites réalisées",
                           unachieved = "Visites non réalisées",
                           lofu = "Perdus de vue")) |>
    rename(`Zone de Santé` = zone_sante_notification, `Métrique` = metric,
           `Tau de Kendall` = tau, `p-value` = p_value, `n (jours)` = n,
           `Tendance` = trend_direction) |>
    kbl(escape = FALSE, align = "c",
        caption = paste0("Test de tendance de Mann-Kendall par zone de santé — 3 dernières semaines, ",
                         format(ref.date, "%d %B %Y"))) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

  contact.3w.mk.tbl     |> save_kable(file = paste0(day.dir.a, "MK_3w_ensemble_",
                                                    format(ref.date, "%d%b"), ".html"))
  contact.3w.mk.prov.tbl |> save_kable(file = paste0(day.dir.p, "MK_3w_prov_",
                                                     format(ref.date, "%d%b"), ".html"))
  contact.3w.mk.zs.tbl   |> save_kable(file = paste0(day.dir.z, "MK_3w_zs_",
                                                     format(ref.date, "%d%b"), ".html"))

  ## Graphiques (PNG)
  contact.3w.ts.g |>
    ggsave(file = paste0(day.dir.a, "ContactFU_3w_trend_",
                         format(ref.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 30, scale = 0.35)
  contact.3w.ts.prov.g |>
    ggsave(file = paste0(day.dir.p, "ContactFU_3w_trend_prov_",
                         format(ref.date, "%d%b"), ".png"),
           dpi = 300, height = 20, width = 30, scale = 0.35)
  contact.3w.ts.zs.g |>
    ggsave(file = paste0(day.dir.z, "ContactFU_3w_trend_zs_",
                         format(ref.date, "%d%b"), ".png"),
           dpi = 300, height = 25, width = 35, scale = 0.35)

  ## Export XLSX (Ensemble / Province / ZS + MK)
  wb.3w <- createWorkbook()

  header_style.3w <- createStyle(
    fgFill = "#2b6ed9", fontColour = "#FFFFFF", textDecoration = "bold",
    border = "TopBottomLeftRight", borderColour = "#1a4fa0", halign = "center"
  )

  addWorksheet(wb.3w, "Ensemble")
  writeData(wb.3w, "Ensemble", contact.3w.overall)
  addStyle(wb.3w, "Ensemble", style = header_style.3w,
           rows = 1, cols = seq_len(ncol(contact.3w.overall)), gridExpand = TRUE)
  setColWidths(wb.3w, "Ensemble", cols = seq_len(ncol(contact.3w.overall)), widths = "auto")
  freezePane(wb.3w, "Ensemble", firstRow = TRUE)

  addWorksheet(wb.3w, "Par_Province")
  writeData(wb.3w, "Par_Province", contact.3w.prov)
  addStyle(wb.3w, "Par_Province", style = header_style.3w,
           rows = 1, cols = seq_len(ncol(contact.3w.prov)), gridExpand = TRUE)
  setColWidths(wb.3w, "Par_Province", cols = seq_len(ncol(contact.3w.prov)), widths = "auto")
  freezePane(wb.3w, "Par_Province", firstRow = TRUE)

  addWorksheet(wb.3w, "Par_ZS")
  writeData(wb.3w, "Par_ZS", contact.3w.zs)
  addStyle(wb.3w, "Par_ZS", style = header_style.3w,
           rows = 1, cols = seq_len(ncol(contact.3w.zs)), gridExpand = TRUE)
  setColWidths(wb.3w, "Par_ZS", cols = seq_len(ncol(contact.3w.zs)), widths = "auto")
  freezePane(wb.3w, "Par_ZS", firstRow = TRUE)

  addWorksheet(wb.3w, "MK_Ensemble")
  writeData(wb.3w, "MK_Ensemble", contact.3w.mk)
  addWorksheet(wb.3w, "MK_Par_Province")
  writeData(wb.3w, "MK_Par_Province", contact.3w.mk.prov)
  addWorksheet(wb.3w, "MK_Par_ZS")
  writeData(wb.3w, "MK_Par_ZS", contact.3w.mk.zs)

  saveWorkbook(wb.3w,
               file = paste0(day.dir.a, "ContactFU_3w_indicateurs_",
                             format(ref.date, "%d%b"), ".xlsx"),
               overwrite = TRUE)
}
