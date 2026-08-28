# EpiCurves 

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
                 "zoo","forecast",
                 "DescTools",
                 install = T)
  
}


## Loading custom functions

source(here::here("helpers", "paths.R"))
source(here::here("helpers", "DebutSem.R"))
source(here::here("helpers", "AgeCat.R"))
source(here::here("helpers", "AgeStd.R"))
source(here::here("helpers", "LoadLatestData.R"))

## Importing evd data

evd.data <- load_latest_data(
  data_cleaning_output,
  "evd.cleaning",
  format = "rds"
)

nrow(evd.data)

names(evd.data)

evd.data |>
  filter(grepl("\\w+-\\w+-\\w+-\\d+-[a-z]+", num_epid, ignore.case = TRUE),
         date_heure_notification_alerte >= "2026-05-01",
         lab_resultat_final=="Positif"|
         classification_finale=="Cas confirmé") |> nrow()


evd.data |>
  filter(grepl("RDC-\\w+-\\w+-\\d+-[A-Z]+", 
         num_epid, ignore.case = TRUE)) |> 
  count(classification_finale, lab_resultat_final, name = "NombreCas")

evd.data |>
  filter(grepl("RDC-\\w+-\\w+-\\d+-[A-Z]+", 
         num_epid, ignore.case = TRUE)) |>
  count(classification_finale, lab_resultat_final, name = "NombreCas")


evd.data |>
  filter(num_epid == "RDC-ITU-ARI-26-MBF30-MVE")  |> select(num_epid) 

evd.data |>
  mutate(date_heure_notification_alerte= as.Date(date_heure_notification_alerte)) |>
  filter(#date_heure_notification_alerte >= "2026-04-01",
         lab_resultat_final=="Positif"|
         classification_finale=="Cas confirmé") |> nrow()

(max.notif.date <- max(as.Date(evd.data$date_heure_notification_alerte[which(as.Date(evd.data$date_heure_notification_alerte) <= as.Date(now()))]), na.rm = TRUE))


# ---- Output directories for the current reporting date ----
day.dir.base <- file.path(here::here("OutPut", "EpiCurves"),
                          format(max.notif.date, "%d%b"))

day.dir   <- paste0(day.dir.base, "/")
day.dir.a <- paste0(day.dir.base, "/all/")
day.dir.p <- paste0(day.dir.base, "/prov/")
day.dir.z <- paste0(day.dir.base, "/zs/")
day.dir.s <- paste0(day.dir.base, "/as/")

# Create all dirs; recursive = TRUE builds missing parents,
# showWarnings = FALSE avoids warnings when a dir already exists
purrr::walk(c(day.dir, day.dir.a, day.dir.p, day.dir.z, day.dir.s),
            dir.create, recursive = TRUE, showWarnings = FALSE)

#####

table(evd.data$classification_finale,
      evd.data$lab_resultat_final, useNA = "ifany")

table(evd.data$classification_finale, useNA = "ifany")

table(evd.data$lab_resultat_final, useNA = "ifany")


evd.data |>
  mutate(alert_date_debut_symptoms = as.Date(alert_date_debut_symptoms)) |>
  filter(lab_resultat_final=="Positif"|
         classification_finale=="Cas confirmé",
         isoyear(alert_date_debut_symptoms) >= 2026 ) |>
  count(alert_date_debut_symptoms, name = "NombreCas") |>
  ggplot(aes(x=alert_date_debut_symptoms,
             y=NombreCas))+
  geom_bar(stat = "identity")

evd.data |>
  mutate(s2_date_debut_signes_symptomes = as.Date(s2_date_debut_signes_symptomes))|>
  filter(isoyear(s2_date_debut_signes_symptomes)>= 2026,
          lab_resultat_final=="Positif"|
         classification_finale=="Cas confirmé") |>
  count(s2_date_debut_signes_symptomes, name = "NombreCas") |>
  ggplot(aes(x=s2_date_debut_signes_symptomes,
             y=NombreCas))+
  geom_bar(stat = "identity")

"Mongbwalu"

evd.data |> 
  filter(province_notification == "Nord Kivu",
          #zone_sante_notification == "Butembo",
         classification_finale == "Cas confirmé") |>
  count(lab_resultat_final, name = "NombreCas")

evd.data |>
  filter(classification_finale_cas == "Cas confirmé",
         alert_date_debut_symptoms <= "2026-05-15",
         alert_date_debut_symptoms >= "2026-01-01") |>
  count(alert_date_debut_symptoms, name = "NombreCas")



evd.data |>
  filter(province_notification == "Nord Kivu",
         classification_finale_cas == "Cas confirmé") |>
  count(nature_alerte, name = "NombreCas")

evd.data |> names()
  filter(num_epid == "RDC-ITU-NIN-26-0088596-MVE") |>
  select(all_of(contains("lab"))) |>
  View()

evd.data |>
  filter(is.na(lab_resultat_final),
         classification_finale_cas == "Cas confirmé") 

names(evd.data)[grepl("lab",names(evd.data), ignore.case = T)]




## ---- Recompute lab_resultat_final2 from lab_resultat_final_1.._12 ----
# Priority across the 12 sample columns: Positif > Négatif > Invalide
evd.data <- evd.data |>
  rowwise() |>
  mutate(
    lab_resultat_final2 = {
      v <- c_across(starts_with("lab_resultat_final_"))
      v <- v[!is.na(v)]
      if ("Positif" %in% v) "Positif"
      else if ("Négatif" %in% v) "Négatif"
      else if ("Invalide" %in% v) "Invalide"
      else NA_character_
    }
  ) |>
  ungroup()

# Number of positive cases according to the recomputed result
n_positive_cases <- sum(evd.data$lab_resultat_final2 == "Positif", na.rm = TRUE)
n_positive_cases

names(evd.data)[grepl("r[eéè]sul",names(evd.data), ignore.case = T)]
names(evd.data)[grepl("class",names(evd.data), ignore.case = T)]


table(evd.data$alert_conlusion, useNA = "ifany")
table(evd.data$alert_cas_suspect, useNA = "ifany")
table(evd.data$s5_qu_prelev_a_deja_ete_soumis_malade, useNA = "ifany")
table(evd.data$lab_resultat_final, useNA = "ifany")
table(evd.data$classification_finale_cas, useNA = "ifany")
table(evd.data$classification_finale, useNA = "ifany")

table(evd.data$classification_finale_cas, 
      evd.data$lab_resultat_final, useNA = "ifany")


evd.data |>
  filter(classification_finale_cas == "Cas confirmé",
          is.na(lab_resultat_final) ) |>
  select(num_epid)

evd.data |>
  filter(classification_finale == "Cas confirmé",
          is.na(lab_resultat_final) ) |>
  select(num_epid)

table(evd.data$classification_finale, 
      evd.data$lab_resultat_final, useNA = "ifany")






table(evd.data$s1_malade_etait_il_contact_suivi, useNA = "ifany")

table(evd.data$s1_malade_etait_il_contact_suivi,
      evd.data$classification_finale_cas, useNA = "ifany")


bvd.notif.dates <-  sort(unique(as.Date(evd.data$date_heure_notification_alerte)))
bvd.notif.dates <- bvd.notif.dates[which(bvd.notif.dates <= now())]

evd.data |>
  mutate(date_heure_notification_alerte= as.Date(date_heure_notification_alerte))|>
  filter(date_heure_notification_alerte >= nth(bvd.notif.dates, -7)#,alert_conlusion == "Validée"
) |> count( name = "NombreAlertes")

evd.data |>
  mutate(date_heure_notification_alerte= as.Date(date_heure_notification_alerte))|>
  filter(date_heure_notification_alerte >= nth(bvd.notif.dates, -7),
         alert_conlusion == "Validée"
) |> count( name = "NombreValidée")

evd.data |>
  mutate(date_heure_notification_alerte= as.Date(date_heure_notification_alerte))|>
  filter(date_heure_notification_alerte >= nth(bvd.notif.dates, -7),
         alert_conlusion == "Validée"
) |> count(nature_alerte, name = "NombreValidée")


evd.data |>
  mutate(date_heure_notification_alerte= as.Date(date_heure_notification_alerte))|>
  filter(date_heure_notification_alerte >= nth(bvd.notif.dates, -7),
         alert_conlusion == "Validée", 
         alert_cas_suspect== "Prélevé en communauté"|
         s5_qu_prelev_a_deja_ete_soumis_malade =="Oui",
) |> count( name = "PrelevementAlertesValides")

evd.data |>
  mutate(date_heure_notification_alerte= as.Date(date_heure_notification_alerte))|>
  filter(date_heure_notification_alerte >= nth(bvd.notif.dates, -7),
         lab_resultat_final == "Positif"|
         classification_finale_cas == "Cas confirmé") |>
         count( name = "Cas confirmés")


evd.data |>
  mutate(date_heure_notification_alerte= as.Date(date_heure_notification_alerte))|>
  filter(date_heure_notification_alerte >= nth(bvd.notif.dates, -7),
         lab_resultat_final == "Positif"|classification_finale_cas == "Cas confirmé",
        s1_malade_etait_il_contact_suivi == "Oui") |>
         count( name = "Cas confirmés")


#########
names(evd.data)[grep("lab", names(evd.data), ignore.case = TRUE)]

evd.data |>
  mutate(date_heure_notification_alerte= as.Date(date_heure_notification_alerte))|>
  filter(date_heure_notification_alerte >= nth(bvd.notif.dates, -7),
          !is.na(s5_identifiant_labo)) |> 
    count( name = "NombreAlertes")

################

evd.data.epi <-
  evd.data |> 
  mutate(classification_finale_cas = 
          case_when(lab_resultat_final == "Positif"~"Cas confirmé",
                    lab_resultat_final == "Négatif"~"Non cas",
                    .default = classification_finale_cas),
        age = AgeStd(age_ans, age_mois),
        age.groups = AgeCat(age, n_cats = 5)) |>
  select(province_notification,zone_sante_notification,aire_sante_notification,alert_date_debut_symptoms,
         date_heure_notification_alerte,nature_alerte,age.groups,sexe,s2_date_debut_signes_symptomes,
        lab_resultat_final,classification_finale_cas,s6_statut_final_patient)

table(evd.data$classification_finale_cas)
table(evd.data$lab_resultat_final,evd.data$classification_finale_cas, useNA = "always")
table(evd.data.epi$lab_resultat_final)
table(evd.data.epi$classification_finale_cas)

##### Confirmed and probable cases #####

evd.data.conf <-
  evd.data.epi |> 
  select(-s2_date_debut_signes_symptomes)|>
  mutate(s2_date_debut_signes_symptomes = alert_date_debut_symptoms,
         date_heure_notification_alerte=
         as.Date(date_heure_notification_alerte),
         s6_statut_final_patient = case_when(is.na(s6_statut_final_patient)&
                                             nature_alerte=="Décédé"~"Décédé",
                                             !is.na(s6_statut_final_patient)~s6_statut_final_patient,
                                             TRUE~"Vivant"),
         epi.y = lubridate::isoyear(s2_date_debut_signes_symptomes),
         epi.ew = lubridate::isoweek(s2_date_debut_signes_symptomes),
         epi.ewb =get_monday(s2_date_debut_signes_symptomes)) |>
  filter(lab_resultat_final=="Positif"|
         classification_finale_cas=="Cas confirmé"|
         classification_finale_cas=="Cas probable")

evd.data.pos <-
  evd.data.epi |>
  select(-s2_date_debut_signes_symptomes)|>
  mutate(s2_date_debut_signes_symptomes = alert_date_debut_symptoms,
         date_heure_notification_alerte=
        as.Date(date_heure_notification_alerte),
         s6_statut_final_patient = case_when(is.na(s6_statut_final_patient)&
                                             nature_alerte=="Décédé"~"Décédé",
                                            !is.na(s6_statut_final_patient)~s6_statut_final_patient,
                                             TRUE~"Vivant"),
         epi.y = lubridate::isoyear(s2_date_debut_signes_symptomes),
         epi.ew = lubridate::isoweek(s2_date_debut_signes_symptomes),
         epi.ewb =get_monday(s2_date_debut_signes_symptomes)) |>
  filter(lab_resultat_final=="Positif"|
         classification_finale_cas=="Cas confirmé")



evd.data.pos.imp <-
  evd.data.epi |>
  mutate(date_heure_notification_alerte=
          as.Date(date_heure_notification_alerte),
         s6_statut_final_patient = case_when(is.na(s6_statut_final_patient)&
                                             nature_alerte=="Décédé"~"Décédé",
                                            !is.na(s6_statut_final_patient)~s6_statut_final_patient,
                                             TRUE~"Vivant"),
         epi.y = lubridate::isoyear(s2_date_debut_signes_symptomes),
         epi.ew = lubridate::isoweek(s2_date_debut_signes_symptomes),
         epi.ewb =get_monday(s2_date_debut_signes_symptomes)) |>
  filter(lab_resultat_final=="Positif"|
         classification_finale_cas=="Cas confirmé")

table(evd.data.conf$classification_finale_cas)
table(evd.data.conf$lab_resultat_final,evd.data.conf$classification_finale_cas)
table(evd.data.conf$lab_resultat_final)
table(evd.data.conf$classification_finale_cas)


table(evd.data.pos$classification_finale_cas)


evd.data.pos |>
   mutate(date_debut_sem = get_monday(date_heure_notification_alerte)) |>
   count(date_debut_sem, province_notification,zone_sante_notification,
          aire_sante_notification,   name = "NombreCas") |>
   mutate(Epi_week = isoweek(date_debut_sem)) |>
   write.xlsx(paste0(day.dir.z,"Confirmed.Cases.By.Prov.HZ.AS.EpiWk.xlsx"))



evd.data.pos |>
   mutate(date_debut_sem = get_monday(date_heure_notification_alerte)) |>
   count(date_debut_sem, aire_sante_notification, name = "NombreCas") |>
   mutate(Epi_week = isoweek(date_debut_sem)) |>
   write.xlsx(paste0(day.dir.a,"Confirmed.Cases.By.HA.EpiWk.xlsx"))
 


#####

ClassColorEpi=c("Cas confirmé"="#7b0d0d",
                #"Indeterminé"="#948c8a",
                "Non cas"="#60c59f",
                "Cas probable"="#df9501",#"#2c0404",
                "Cas suspect"="#eedf2a")


VDcolors= c("Vivant"="#022c70",
               "Décédé"="#701f02")

VDcolors2= c("Vivant [V]"="#022c70",
               "Décédé [D]"="#701f02")

VDcolors3= c("Vivant"="#001e4e",
               "Décédé"="#5b0b02")

bvd.class.dly <-
  evd.data.conf |>
  count(classification_finale_cas,
        alert_date_debut_symptoms, 
        name = "NombreCas") |>
  arrange(desc(alert_date_debut_symptoms))


bvd.class.dly.imp <-
  evd.data.conf |>
  count(classification_finale_cas,
        s2_date_debut_signes_symptomes, 
        name = "NombreCas") |>
  arrange(desc(s2_date_debut_signes_symptomes))


bvd.class.status.dly <-
  evd.data.pos |>
  count(s6_statut_final_patient,
        s2_date_debut_signes_symptomes, 
        name = "NombreCas") |>
  mutate(s6_statut_final_patient = factor(s6_statut_final_patient,
                                          levels = c("Vivant", "Décédé"))) |>
  arrange(desc(s2_date_debut_signes_symptomes))

bvd.stat.dly.prov.stat <-
  evd.data.pos |>
  group_by(province_notification) |>
  count(s6_statut_final_patient,
        name = "NombreCas") |>
  mutate(status = str_extract(s6_statut_final_patient, "^\\w{1}"),
         stat.cas= paste(status,NombreCas, sep=": ")) |>
  ungroup()|>
  summarise(NombreCas=sum(NombreCas),
            stat.cas = paste(stat.cas, collapse = " ,"), 
            .by = c(province_notification))|>
  mutate(dps = paste0(province_notification," [", stat.cas,"]"))

bvd.pro <-
  bvd.stat.dly.prov.stat |>
  arrange(desc(NombreCas)) |>
  pull(province_notification)

bvd.pro.f <-
  bvd.stat.dly.prov.stat |>
  arrange(desc(NombreCas)) |>
  pull(dps)

bvd.class.status.prov.dly <-
  evd.data.pos |>
  count(s6_statut_final_patient,
        s2_date_debut_signes_symptomes, 
        province_notification,
        name = "NombreCas") |>
  mutate(s6_statut_final_patient = factor(s6_statut_final_patient,
                                          levels = c("Vivant", "Décédé"))) |>
  left_join(bvd.stat.dly.prov.stat[,c("province_notification","dps")], by = "province_notification")|>
  select(-province_notification)|>
  mutate(province_notification = dps,
         province_notification = factor(province_notification,
                                          levels = bvd.pro.f),
         s6_statut_final_patient = case_when(s6_statut_final_patient == "Décédé" ~ "Décédé [D]",
                                             s6_statut_final_patient == "Vivant" ~ "Vivant [V]",
                                             TRUE ~ s6_statut_final_patient),
         s6_statut_final_patient = factor(s6_statut_final_patient,
                                          levels = c("Vivant [V]", "Décédé [D]"))) |>
  arrange(desc(s2_date_debut_signes_symptomes)) 

bvd.class.status.prov.dly1 <-
  bvd.class.status.prov.dly |>
  filter(!is.na(s2_date_debut_signes_symptomes)) |>
  arrange(desc(s2_date_debut_signes_symptomes)) |>
  summarise(NombreCas = sum(NombreCas), 
           .by=c(province_notification,s2_date_debut_signes_symptomes))

bvd.class.status.dly1 <-
  bvd.class.status.dly |>
  filter(!is.na(s2_date_debut_signes_symptomes)) |>
  summarise(NombreCas = sum(NombreCas), 
           .by=s2_date_debut_signes_symptomes) 


bvd.class.dlw <-
  evd.data.conf |>
  count(classification_finale_cas,
        epi.ewb, 
        name = "NombreCas") |>
  arrange(desc(epi.ewb))


bvd.stat.dly.zs.stat <-
  evd.data.pos |>
  group_by(province_notification,
           zone_sante_notification) |>
  count(s6_statut_final_patient,
        name = "NombreCas") |>
  mutate(status = str_extract(s6_statut_final_patient, "^\\w{1}"),
         stat.cas= paste(status,NombreCas, sep=": ")) |>
  ungroup()|>
  summarise(NombreCas=sum(NombreCas),
            stat.cas = paste(stat.cas, collapse = " ,"), 
            .by = c(province_notification,
                    zone_sante_notification))|>
  mutate(ZS = paste0(zone_sante_notification," [", stat.cas,"]"))

bvd.zs <-
  bvd.stat.dly.zs.stat |>
  arrange(desc(NombreCas)) |>
  pull(zone_sante_notification)

bvd.zs.data <-
  bvd.stat.dly.zs.stat |>
  arrange(desc(NombreCas)) |>
  pull(ZS)


bvd.stat.dly.zs <-
  evd.data.pos |>
  group_by(province_notification,
           zone_sante_notification) |>
  count(s6_statut_final_patient,
        s2_date_debut_signes_symptomes, 
        name = "NombreCas") |>
  ungroup()|>
  left_join(bvd.stat.dly.zs.stat[,c("zone_sante_notification","ZS")], by = "zone_sante_notification")|>
  select(-zone_sante_notification)|>
  mutate(zone_sante_notification = ZS,
         zone_sante_notification = factor(zone_sante_notification,
                                          levels = bvd.zs.data),
         s6_statut_final_patient = case_when(s6_statut_final_patient == "Décédé" ~ "Décédé [D]",
                                             s6_statut_final_patient == "Vivant" ~ "Vivant [V]",
                                             TRUE ~ s6_statut_final_patient),
         s6_statut_final_patient = factor(s6_statut_final_patient,
                                          levels = c("Vivant [V]", "Décédé [D]"))) |>
  arrange(desc(s2_date_debut_signes_symptomes)) |>
  left_join(bvd.stat.dly.prov.stat[,c("province_notification","dps")], by = "province_notification")|>
  select(-province_notification)|>
  mutate(province_notification = dps)


bvd.stat.dlw.zs <-
  evd.data.pos |>
  group_by(zone_sante_notification) |>
  count(s6_statut_final_patient,
        epi.ewb, 
        name = "NombreCas")|>
  mutate(zone_sante_notification = factor(zone_sante_notification,
                                          levels = bvd.zs))  |>
  arrange(desc(epi.ewb))

bvd.class.status.zs.dly1 <-
  bvd.stat.dly.zs |>
  filter(!is.na(s2_date_debut_signes_symptomes)) |>
  arrange(desc(s2_date_debut_signes_symptomes)) |>
  summarise(NombreCas = sum(NombreCas), 
           .by=c(zone_sante_notification,
                s2_date_debut_signes_symptomes))


decompte.all.conf <-
  bvd.class.dly |>
  summarise(NombreCas = sum(NombreCas), 
           .by=classification_finale_cas) |>
  mutate(cas.class= paste0(NombreCas, " ", tolower(classification_finale_cas),"s")) |>
  pull(cas.class) |> paste(collapse = " et ")

decompte.all.alive.d <-
  bvd.class.status.dly |>
  summarise(NombreCas = sum(NombreCas), 
           .by=s6_statut_final_patient) |>
  mutate(cas.class= paste0(NombreCas, " ", tolower(s6_statut_final_patient),"s")) |>
  pull(cas.class) |> paste(collapse = " et ")

nCas <- sum(bvd.class.status.dly$NombreCas)
nCas.prov <- summarise(bvd.class.status.prov.dly1,NombreCas=sum(NombreCas), .by=province_notification)
nCas.zs <- summarise(bvd.stat.dly.zs,NombreCas=sum(NombreCas), .by=zone_sante_notification)

nCas <- 782

decompte.prov <- 
  evd.data.pos |>
  count(s6_statut_final_patient,
        province_notification,
        name = "NombreCas") |>
  mutate(cas.class= paste0(NombreCas, " ", tolower(s6_statut_final_patient),"s")) |>
  summarise(cas.class = paste(cas.class, collapse = " et "), .by = province_notification)


decompte.zs <- 
  evd.data.pos |>
  count(s6_statut_final_patient,
        zone_sante_notification,
        name = "NombreCas") |>
  mutate(cas.class= paste0(NombreCas, " ", 
         tolower(s6_statut_final_patient),"s")) |>
  summarise(cas.class = paste(cas.class, collapse = " et "), 
            .by = zone_sante_notification)


##

bvd.stat.dly.as0 <- 
  evd.data.pos |>
  group_by(zone_sante_notification, aire_sante_notification) |>
  count(s6_statut_final_patient, name = "NombreCas") |>
  ungroup() |>
  summarise(
    NombreCas = sum(NombreCas, na.rm = TRUE),
    .by = c(zone_sante_notification, aire_sante_notification, s6_statut_final_patient)
  ) |>
  mutate(
    status = str_extract(s6_statut_final_patient, "^\\w{1}"),
    stat.cas = paste0(s6_statut_final_patient, " [", status, ": ", NombreCas, "]")
  ) |>
  summarise(
    NombreCas = sum(NombreCas, na.rm = TRUE),
    stat.cas = paste(stat.cas, collapse = ", "),
    .by = c(zone_sante_notification, aire_sante_notification)
  )



bvd.stat.dly.as0 <-
  evd.data.pos |>
  group_by(zone_sante_notification,
           aire_sante_notification) |>
  count(s6_statut_final_patient,
        s2_date_debut_signes_symptomes, 
        name = "NombreCas") |>
  ungroup()


#bvd.stat.dly.as0 |> View()


bvd.stat.dly.as.stat <-
  bvd.stat.dly.as0|>
  summarise(NombreCas=sum(NombreCas),
            .by = c(zone_sante_notification,
                    aire_sante_notification,
                    s6_statut_final_patient))|>
  ungroup()|>
  mutate(status = str_extract(s6_statut_final_patient, "^\\w{1}"),
          stat.cas= paste0( status,":",NombreCas)) |>
  summarise(NombreCas=sum(NombreCas, na.rm = TRUE),
            stat.cas = paste(stat.cas, collapse = ", "),
            .by = c(zone_sante_notification,aire_sante_notification))|>
  mutate(AS = paste0(aire_sante_notification," [", stat.cas,"]"))


bvd.as <-
  bvd.stat.dly.as.stat |>
  arrange(desc(NombreCas)) |>
  pull(aire_sante_notification)


bvd.as.data <-
  bvd.stat.dly.as.stat |>
  arrange(desc(NombreCas)) |>
  pull(AS) |> unique()

table(bvd.stat.dly.as0$s6_statut_final_patient)

bvd.stat.dly.as <-
bvd.stat.dly.as0 |>
  left_join(bvd.stat.dly.as.stat[,c("aire_sante_notification","AS")], 
            by = "aire_sante_notification")|> 
  distinct()|>
  select(-aire_sante_notification)|> 
  mutate(aire_sante_notification = AS,
         aire_sante_notification = factor(aire_sante_notification,
                                          levels = bvd.as.data),
         s6_statut_final_patient = case_when(s6_statut_final_patient == "Décédé" ~ "Décédé [D]",
                                             s6_statut_final_patient == "Vivant" ~ "Vivant [V]",
                                             TRUE ~ s6_statut_final_patient),
         s6_statut_final_patient = factor(s6_statut_final_patient,
                                          levels = c("Vivant [V]", "Décédé [D]"))) |>
  arrange(desc(s2_date_debut_signes_symptomes)) |>
  left_join(bvd.stat.dly.zs.stat[,c("zone_sante_notification","ZS")],
            by = "zone_sante_notification")|>
  select(-zone_sante_notification)|>
  mutate(zone_sante_notification = ZS)

  
bvd.stat.dly.as.stat |>
  select(zone_sante_notification, 
         aire_sante_notification, NombreCas) |>
  write.xlsx(paste0(day.dir.s,"Confirmed.BVD.AS_",
                    format(max.notif.date, "%d%b"),".xlsx"),
                    sheetName = "Confirmed.BVD.AS", 
                  overwrite = TRUE)

####### Daily plots 
### Confirmed and probables 

(bvd.conf.dly.g <-
  bvd.class.dly |>
  ggplot(aes(x=alert_date_debut_symptoms,
             y=NombreCas,
             fill = classification_finale_cas))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 1:max(bvd.class.dly$NombreCas),
                 color="white")+
  scale_x_date(date_breaks = "2 days",
               date_labels = "%d\n%b",
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max(bvd.class.dly$NombreCas)*1.1),
                    breaks = seq(0,max(bvd.class.dly$NombreCas),by = 4))+
  scale_fill_manual(values = ClassColorEpi)+
  labs(title = paste0("Tendances des cas confirmés et probables de la MVB, RDC"),
       subtitle = paste0(paste(bvd.pro, collapse = ", "),", RDC","\n",
                          nCas, " cas confirmés dont ",decompte.all.alive.d,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
       y="N confirmés et probables",
       x="Date de début des symptômes",
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey10",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 8),
        axis.title = element_text(face = "bold"),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 9,
                                   face = "bold"),
        legend.position = c(0.1, 0.95),
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey95",
                                        colour = "grey90"),
        strip.text = element_text(face = "bold",
                                  size = 10.5))
)
  

bvd.conf.dly.g |>
  ggsave(file=paste0(day.dir.a,"Epicurve.all.confirmed.Daily_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 300, height = 20, width = 30, scale = 0.3)




### Alive and dead

#### All affected Health zones

(bvd.class.st.dly.g <-
  bvd.class.status.dly |>
  ggplot(aes(x=s2_date_debut_signes_symptomes,
             y=NombreCas,
             fill =   s6_statut_final_patient))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 1:max(bvd.class.status.dly1$NombreCas*1.05),
                 color="white")+
  scale_x_date(date_breaks = "3 days",
               date_labels = "%d\n%b",
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max(bvd.class.status.dly1$NombreCas)*1.05),
                    breaks = seq(0,max(bvd.class.status.dly1$NombreCas)*1.05,by = 4))+
  scale_fill_manual(values = VDcolors)+
  labs(title = paste0("Tendances des cas confirmés et décès de la MVB, RDC"),
       subtitle = paste0(paste(bvd.pro, collapse = ", "),", RDC","\n",
                          nCas, " cas confirmés dont ",decompte.all.alive.d,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
       y="Nombre des confirmés et décès",
       x="Date de début des symptômes",
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey50",
                                        linetype = 2,
                                        linewidth = 0.5),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2,
                                        linewidth = 0.5),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 17),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 8),
        axis.title = element_text(face = "bold"),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 9,
                                   face = "bold"),
        legend.position = c(0.1, 0.95),
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey95",
                                        colour = "grey90"),
        strip.text = element_text(face = "bold",
                                  size = 10.5))
)
  
export.width = floor(nrow(bvd.class.status.dly)/4.1)

bvd.class.st.dly.g |>
  ggsave(file=paste0(day.dir.a,"Epicurve.all.status.Daily_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 300, height = 20, width = export.width, scale = 0.3)


#### By Province

(bvd.sat.dly.prov.g <-
  ggplot()+
  geom_bar(data = bvd.class.status.dly1,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas),width=0.8,
               fill = "grey80", stat = "identity")+
  geom_bar(data=bvd.class.status.prov.dly,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas,
               fill =  s6_statut_final_patient),
               width=0.8,stat = "identity")+
  geom_hline(yintercept = 1:max(bvd.class.status.dly1$NombreCas*1.05),
                 color="white", linewidth=0.5)+
  scale_x_date(date_breaks = "3 days",
               date_labels = "%d\n%b",
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max(bvd.class.status.dly1$NombreCas)*1.05),
                    breaks = seq(0,max(bvd.class.status.dly1$NombreCas)*1.05,by = 4))+
  scale_fill_manual(values = VDcolors2)+
  facet_wrap(~province_notification, ncol=3)+
  labs(title = paste0("Tendances des cas confirmés et décès de la MVB par DPS, RDC"),
       subtitle = paste0(paste(bvd.pro, collapse = ", "),"\n",
                          nCas, " cas confirmés dont ",decompte.all.alive.d,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB","\n",
                         "Les girs representents les cas confirmés de l'ensemble des DPS affectées en dehors de la DPS actuelle"),
       y="N confirmés et décès",
       x="Date de début des symptômes",
       caption =paste0("\nIOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey10",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(family = "sans",size = 9),
        plot.caption = element_text(family = "sans",size = 7),
        axis.title = element_text(family = "sans",face = "bold"),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(family = "sans",size = 8,
                                   face = "bold"),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey80",
                                        colour = "grey90"),
        strip.text = element_text(family = "sans",face = "bold",
                                  size = 8))
                                  
)
  
n.width = length(unique(bvd.class.status.prov.dly$dps))

bvd.sat.dly.prov.g |>
  ggsave(file=paste0(day.dir.p,"Epicurve.prov.status.Daily_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 300, height = 20, width = export.width*1.8, scale = 0.35)

### Province alone plot

#p= "Nord Kivu [D: 8 ,V: 13]"

for(p in bvd.pro.f){

  provv.data.p <-
    bvd.class.status.prov.dly |>
    filter(province_notification == p)

  p1 = which(bvd.stat.dly.prov.stat$dps==p)
  p1= bvd.stat.dly.prov.stat[p1,1][[1]]

  bvd.sat.dly.prov.p.g <-
  ggplot()+
  geom_bar(data = bvd.class.status.dly1,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas),width=0.8,
               fill = "grey80", stat = "identity")+
  geom_bar(data=provv.data.p,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas,
               fill =  s6_statut_final_patient),
               width=0.8,stat = "identity")+
  geom_hline(yintercept = 1:max(bvd.class.status.dly1$NombreCas*1.05),
                 color="white", linewidth=0.5)+
  scale_x_date(date_breaks = "3 days",
               date_labels = "%d\n%b",
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max(bvd.class.status.dly1$NombreCas)*1.05),
                    breaks = seq(0,max(bvd.class.status.dly1$NombreCas)*1.05,by = 4))+
  scale_fill_manual(values = VDcolors2)+
  #facet_wrap(~province_notification, ncol=3)+
  labs(title = paste0("Tendances des cas confirmés et décès de la MVB, DPS ",p1," RDC"),
       subtitle = paste0(paste(p1, collapse = ", "),", ",
                          nCas.prov$NombreCas[nCas.prov$province_notification==p], " cas confirmés dont ",
                          decompte.prov$cas.class[nCas.prov$province_notification==p],"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB","\n",
                         "Les girs representents les cas confirmés de l'ensemble des DPS affectées en dehors de la DPS ",
                         p1),
       y="N confirmés et décès",
       x="Date de début des symptômes",
       caption =paste0("\nIOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  #coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey50",
                                        linetype = 2,
                                        linewidth = 0.5),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2,
                                        linewidth = 0.5),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(family = "sans",size = 9),
        plot.caption = element_text(family = "sans",size = 7),
        axis.title = element_text(family = "sans",face = "bold"),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(family = "sans",size = 8,
                                   face = "bold"),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey80",
                                        colour = "grey90"),
        strip.text = element_text(family = "sans",face = "bold",
                                  size = 8))
p.width = nrow(provv.data.p)/2.4  

bvd.sat.dly.prov.p.g |>
  ggsave(file=paste0(day.dir.p,"Epicurve.prov.status.Daily_",p1,"_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 300, height = 20, width = export.width, scale = 0.34)

}



#### By Health Znoes

(bvd.sat.dly.zs.g <-
  ggplot()+
  geom_bar(data = bvd.class.status.dly1,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas),width=0.8,
               fill = "grey80", stat = "identity")+
  geom_bar(data=bvd.stat.dly.zs,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas,
               fill =  s6_statut_final_patient),
               width=0.8,stat = "identity")+
  geom_hline(yintercept = 1:max(bvd.class.status.dly1$NombreCas),
                 color="white", linewidth=0.5)+
  scale_x_date(date_breaks = "4 days",
               date_labels = "%d\n%b",
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max(bvd.class.status.dly1$NombreCas)*1.1),
                    breaks = seq(0,max(bvd.class.status.dly1$NombreCas),by = 4))+
  scale_fill_manual(values = VDcolors2)+
  facet_wrap(~zone_sante_notification, ncol=3)+
  labs(title = paste0("Tendances des cas confirmés et décès de la MVB par ZS, RDC"),
       subtitle = paste0(paste(bvd.pro, collapse = ", "),"\n",
                          nCas, " cas confirmés dont ",decompte.all.alive.d,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB","\n",
                         "Les girs representents les cas confirmés de l'ensemble des ZS affectées en dehors de la ZS actuelle"),
       y="N confirmés et décès",
       x="Date de début des symptômes",
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey10",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(family = "sans",size = 12),
        plot.caption = element_text(family = "sans",size = 7),
        axis.title = element_text(family = "sans",face = "bold"),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(family = "sans",size = 10,
                                   face = "bold"),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey80",
                                        colour = "grey90"),
        strip.text = element_text(family = "sans",face = "bold",
                                  size = 11))
                                  
)
  

bvd.sat.dly.zs.g |>
  ggsave(file=paste0(day.dir.z,"Epicurve.zs.status.Daily_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 300, height = 45, width = 37, scale = 0.3)

#### Health zone alone 

for (z in bvd.zs.data){

  z0 = which(bvd.stat.dly.zs.stat$ZS==z)
  z1= bvd.stat.dly.zs.stat[z0,2][[1]]
  zp = bvd.stat.dly.zs.stat[z0,1][[1]]

  prov.data.z <- bvd.class.status.prov.dly1[grep(zp,bvd.class.status.prov.dly1$province_notification),]
  zs.data.z <- bvd.stat.dly.zs[bvd.stat.dly.zs$ZS == z,]
  
  nCas.zs.z <-
     nCas.zs$NombreCas[nCas.zs$zone_sante_notification==z]
  
  decompte.zs.as <-
    decompte.zs$cas.class[nCas.zs$zone_sante_notification==z]

bvd.sat.dly.zs1.g <-
  ggplot()+
  geom_bar(data = prov.data.z,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas),width=0.8,
               fill = "grey80", stat = "identity")+
  geom_bar(data=zs.data.z,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas,
               fill =  s6_statut_final_patient),
               width=0.8,stat = "identity")+
  geom_hline(yintercept = 1:max(prov.data.z$NombreCas*1.05),
                 color="white", linewidth=0.5)+
  scale_x_date(date_breaks = "4 days",
               date_labels = "%d\n%b",
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max(prov.data.z$NombreCas)*1.05),
                    breaks = seq(0,max(prov.data.z$NombreCas)*1.05,by = 4))+
  scale_fill_manual(values = VDcolors2)+
  facet_wrap(~zone_sante_notification, ncol=3)+
  labs(title = paste0("Tendances des cas confirmés et décès de la MVB ZS ",z1,", RDC"),
       subtitle = paste0(paste(z1, collapse = ", "),", ",nCas.zs.z, " cas confirmés dont ",
                          decompte.zs.as,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB","\n",
                         "Les girs representents les cas confirmés de l'ensemble des ZS affectées en dehors de la ZS ",
                         z1),
       y="N confirmés et décès",
       x="Date de début des symptômes",
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  #coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey50",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 15),
        plot.subtitle = element_text(family = "sans",size = 10),
        plot.caption = element_text(family = "sans",size = 7),
        axis.title = element_text(family = "sans",face = "bold"),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(family = "sans",size = 10,
                                   face = "bold"),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey80",
                                        colour = "grey90"),
        strip.text = element_text(family = "sans",face = "bold",
                                  size = 11))
                                  

bvd.sat.dly.zs1.g |>
  ggsave(file=paste0(day.dir.z,"Epicurve.zs.status.Daily_",z1,"_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 300, height = 20, width = export.width*1, scale = 0.35)


}

#### BY Province and health zones
#p= "Ituri [D: 97 ,V: 434]"

for (p in bvd.pro.f){

  p1 = which(bvd.stat.dly.prov.stat$dps==p)
  p1= bvd.stat.dly.prov.stat[p1,1][[1]]


(bvd.sat.dly.prov.zs.g <-
  ggplot()+
  geom_bar(data = bvd.class.status.prov.dly1[bvd.class.status.prov.dly1$province_notification==p,],
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas),width=0.8,
               fill = "grey80", stat = "identity")+
  geom_bar(data=bvd.stat.dly.zs[bvd.stat.dly.zs$province_notification==p,],
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas,
               fill =  s6_statut_final_patient),
               width=0.8,stat = "identity")+
  geom_hline(yintercept = 1:max(bvd.class.status.prov.dly1$NombreCas[bvd.class.status.prov.dly1$province_notification==p]*1.05),
                 color="white", linewidth=0.5)+
  scale_x_date(date_breaks = "7 days",
               date_labels = "%d\n%b",
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max(bvd.class.status.prov.dly1$NombreCas[bvd.class.status.prov.dly1$province_notification==p])*1.05),
                    breaks = seq(0,max(bvd.class.status.prov.dly1$NombreCas[bvd.class.status.prov.dly1$province_notification==p]*1.05),by = 4))+
  scale_fill_manual(values = VDcolors2)+
  facet_wrap(~zone_sante_notification, ncol=4)+
  labs(title = paste0("Tendances des cas confirmés et décès de la MVB par ZS, ",p1," RDC"),
       subtitle = paste0(paste(p1, collapse = ", "),", ",
                          nCas.prov$NombreCas[nCas.prov$province_notification==p], " cas confirmés dont ",
                          decompte.prov$cas.class[nCas.prov$province_notification==p],"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB","\n",
                         "Les girs representents les cas confirmés de l'ensemble des ZS affectées de ",p1,
                         " en dehors de la ZS actuelle"),
       y="N confirmés et décès",
       x="Date de début des symptômes",
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey50",
                                        linetype = 2,
                                        linewidth = 0.5),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2,
                                        linewidth = 0.5),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 16),
        plot.subtitle = element_text(family = "sans",size = 12),
        plot.caption = element_text(family = "sans",size = 7),
        axis.title = element_text(family = "sans",face = "bold", size = 12),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(family = "sans",size = 9,
                                   face = "bold"),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey80",
                                        colour = "grey90"),
        strip.text = element_text(face = "bold",
                                  size = 12,family = "sans"))
                                  
)
    
  bvd.sat.dly.prov.zs.g |>
   ggsave(file=paste0(day.dir.p,"/Epicurve.status.Daily_prov.zs_",
                     p1,"_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 400, height = 45, width = 50, scale = 0.30)

}

### By health zone and health area


for (z in bvd.zs.data){

  z1 = which(bvd.stat.dly.zs.stat$ZS==z)
  z1= bvd.stat.dly.zs.stat[z1,2][[1]]
  max.ncas.zs.all <- max(bvd.class.status.zs.dly1$NombreCas, na.rm = TRUE)
  max.ncas.zs <- max(bvd.class.status.zs.dly1$NombreCas[which(bvd.class.status.zs.dly1$zone_sante_notification==z)], na.rm = TRUE)

  zs.dataz <-
    bvd.class.status.zs.dly1[bvd.class.status.zs.dly1$zone_sante_notification==z,]
  n.as <- length(unique(zs.dataz$zone_sante_notification))

  as.datas <-
    bvd.stat.dly.as |>
    filter(zone_sante_notification==z)


  nCas.zs.z <-
     nCas.zs$NombreCas[nCas.zs$zone_sante_notification==z]
  
  decompte.zs.as <-
    decompte.zs$cas.class[nCas.zs$zone_sante_notification==z]



(bvd.sat.dly.as.g <-
  ggplot()+
  geom_bar(data = zs.dataz,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas),width=0.8,
               fill = "grey80", stat = "identity")+
  geom_bar(data=as.datas,
           aes(x=s2_date_debut_signes_symptomes,
               y=NombreCas,
               fill =  s6_statut_final_patient),
               width=0.8,stat = "identity")+
  geom_hline(yintercept = 1:max(zs.dataz$NombreCas*1.05),
                 color="white", linewidth=0.5)+
  scale_x_date(date_breaks = "7 days",
               date_labels = "%d\n%b",
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max(zs.dataz$NombreCas)*1.05),
                    breaks = seq(0,max(zs.dataz$NombreCas)*1.05,by = 4))+
  scale_fill_manual(values = VDcolors2)+
  facet_wrap(~aire_sante_notification, ncol=3)+
  labs(title = paste0("Tendances des cas confirmés et décès de la MVB par AS, ",z1," RDC"),
       subtitle = paste0(paste(z1, collapse = ", "),", ",nCas.zs.z, " cas confirmés dont ",
                          decompte.zs.as,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB","\n",
                         "Les girs representents les cas confirmés de l'ensemble des AS affectées de ",z1,
                         " en dehors de l'AS actuelle"),
       y="N confirmés et décès",
       x="Date de début des symptômes",
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  #coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey50",
                                        linetype = 2,
                                        linewidth = 0.5),
        panel.grid.major.x = element_line(colour = "grey90",
                                        linetype = 2,
                                        linewidth = 0.5),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(family = "sans",size = 12),
        plot.caption = element_text(family = "sans",size = 7),
        axis.title = element_text(family = "sans",face = "bold", size = 12),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(family = "sans",size = 9,
                                   face = "bold"),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey80",
                                        colour = "grey90"),
        strip.text = element_text(face = "bold",
                                  size = 12,family = "sans"))
                                  
)
  
  flex.h <- case_when(max.ncas.zs < 3 & n.as < 4 ~ 15,
                      max.ncas.zs < 3 ~ 10 ,
                      max.ncas.zs >=3 & max.ncas.zs < 4 ~ 15,
                      .default = max.ncas.zs/max.ncas.zs.all*45)
  
  bvd.sat.dly.as.g |>
   ggsave(file=paste0(day.dir.s,"Epicurve.status.Daily_zs.as_",
                     z1,"_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 400, height = flex.h, width = 50, scale = 0.3)

}

### weekly plots
#### Alive and dead for the whole outbreak area

bvd.class.status.wly <-
bvd.class.status.dly |>
  mutate(date_debut_sem =get_monday(s2_date_debut_signes_symptomes)) |>
  summarise(NombreCas = sum(NombreCas),
            .by = c(s6_statut_final_patient,date_debut_sem)) |>
  ungroup()

max.cas.wk <-
  bvd.class.status.wly|>
  summarise(NombreCas = sum(NombreCas),
            .by = c(date_debut_sem)) |> pull(NombreCas)|> max()

cfr.wly <- bvd.class.status.wly |>
  group_by(date_debut_sem) |>
  summarise(
    Total = sum(NombreCas, na.rm = TRUE),
    Décédé = sum(NombreCas[s6_statut_final_patient == "Décédé"], na.rm = TRUE),
    CFR = ifelse(Total > 0, Décédé / Total, 0)
  )

coeff <- max.cas.wk * 1.05

(bvd.class.st.wly.g <-
  bvd.class.status.wly |>
  ggplot(aes(x=date_debut_sem,
             y=NombreCas,
             fill =   s6_statut_final_patient,
             color =s6_statut_final_patient))+
  geom_bar(stat = "identity", width=7)+
  #geom_hline(yintercept = 1:max.cas.wk,color="white")+
  geom_line(data = cfr.wly, 
            aes(x = date_debut_sem, 
                y = CFR * coeff), 
                inherit.aes = FALSE, 
                color = "#370101", 
                linetype = 6,
                linewidth = 1) +
  scale_x_date(date_breaks = "1 week",
               date_labels = paste0("se","%V\n%Y"),
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max.cas.wk*1.05),
                     breaks = seq(0,max.cas.wk*1.05,by = 20),
                     sec.axis = sec_axis(~ . / coeff, name = "Létalité", labels = scales::percent_format(accuracy = 1)))+
  scale_fill_manual(values = VDcolors)+
  scale_color_manual(values = VDcolors3)+
  labs(title = paste0("Tendances cas confirmés et décès",
                      "\n"," MVB, RDC"),
       subtitle = paste0(paste(bvd.pro, collapse = ", "),", RDC","\n",
                          nCas, " cas confirmés dont ",decompte.all.alive.d,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
       y="Nombre des confirmés et décès",
       x="Semaine début des symptômes",
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="", color="")+
  #coord_equal()+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey95",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8),
        axis.title = element_text(face = "bold", size = 12),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 11,
                                   face = "bold"),
        axis.line.y.right = element_line(color = "#370101", size = 0.25),
        axis.text.y.right = element_text(size = 11, face = "bold", color = "#370101"),
        axis.title.y.right = element_text(face = "bold", size = 12, color = "#370101"),
        legend.position = c(0.1, 0.95),
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey95",
                                        colour = "grey90"),
        strip.text = element_text(face = "bold",
                                  size = 10.5))
)
  

bvd.class.st.wly.g |>
  ggsave(file=paste0(day.dir.a,"Epicurve.all.status.Weekly_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 300, height = 25, width = 18, scale = 0.3)

#### Alive and dead for the whole outbreak area by province

bvd.class.status.prov.wly <-
  bvd.class.status.prov.dly |>
  mutate(date_debut_sem = get_monday(s2_date_debut_signes_symptomes)) |>
  summarise(NombreCas = sum(NombreCas),
            .by = c(s6_statut_final_patient, province_notification, date_debut_sem)) |>
  ungroup()

max.cas.wk.prov <-
  bvd.class.status.prov.wly |>
  summarise(NombreCas = sum(NombreCas),
            .by = c(province_notification, date_debut_sem)) |>
  pull(NombreCas) |> max()

cfr.prov.wly <- bvd.class.status.prov.wly |>
  group_by(province_notification, date_debut_sem) |>
  summarise(
    Total = sum(NombreCas, na.rm = TRUE),
    Décédé = sum(NombreCas[s6_statut_final_patient == "Décédé [D]"], na.rm = TRUE),
    CFR = ifelse(Total > 0, Décédé / Total, 0),
    .groups = "drop"
  )

coeff.prov <- max.cas.wk.prov * 1.05

(bvd.class.st.prov.wly.g <-
  bvd.class.status.prov.wly |>
  ggplot(aes(x=date_debut_sem,
             y=NombreCas,
             fill = s6_statut_final_patient,
             color = s6_statut_final_patient))+
  geom_bar(stat = "identity", width=7)+
  geom_line(data = cfr.prov.wly, 
            aes(x = date_debut_sem, 
                y = CFR * coeff.prov), 
                inherit.aes = FALSE, 
                color = "#370101", 
                linetype = 6,
                linewidth = 1) +
  facet_wrap(~province_notification, ncol=3) +
  scale_x_date(date_breaks = "1 week",
               date_labels = paste0("se","%V\n%Y"),
               expand = c(0.005,0))+
  scale_y_continuous(expand = c(0.0015,0),
                     limits = c(0,max.cas.wk.prov*1.05),
                     breaks = seq(0,max.cas.wk.prov*1.05,by = max(1, round((max.cas.wk.prov*1.05)/5))),
                     sec.axis = sec_axis(~ . / coeff.prov, name = "Létalité", labels = scales::percent_format(accuracy = 1)))+
  scale_fill_manual(values = VDcolors2)+
  scale_color_manual(values = VDcolors2)+
  labs(title = paste0("Tendances cas confirmés et décès par DPS",
                      "\n"," MVB, RDC"),
       subtitle = paste0(paste(bvd.pro, collapse = ", "),", RDC","\n",
                          nCas, " cas confirmés dont ",decompte.all.alive.d,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
       y="Nombre des confirmés et décès",
       x="Semaine début des symptômes",
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="", color="")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey90",
                                        linetype = 2),
        panel.grid.major.x = element_line(colour = "grey95",
                                        linetype = 2),
        plot.title = element_text(colour = "#234a7d",
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8),
        axis.title = element_text(face = "bold", size = 12),
        axis.line = element_line(color = "grey25",
                                   size = 0.25),
        axis.ticks = element_line(size = 0.75,
                                  colour = "grey75"),
        axis.text = element_text(size = 9,
                                   face = "bold"),
        axis.line.y.right = element_line(color = "#370101", size = 0.25),
        axis.text.y.right = element_text(size = 9, face = "bold", color = "#370101"),
        axis.title.y.right = element_text(face = "bold", size = 12, color = "#370101"),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        strip.background = element_rect(fill = "grey95",
                                        colour = "grey90"),
        strip.text = element_text(face = "bold",
                                  size = 10.5))
)
  
bvd.class.st.prov.wly.g |>
  ggsave(file=paste0(day.dir.p,"Epicurve.prov.status.Weekly_",
                     format(max.notif.date, "%d%b"),
                     ".png"),
         dpi = 300, height = 20, width = export.width*2, scale = 0.35)


#### Alive and dead for the whole outbreak area by HZ

bvd.stat.wly.zs <-
  bvd.stat.dly.zs |>
  mutate(date_debut_sem = get_monday(s2_date_debut_signes_symptomes)) |>
  summarise(NombreCas = sum(NombreCas),
            .by = c(s6_statut_final_patient, zone_sante_notification, date_debut_sem)) |>
  ungroup()

for (z in bvd.zs.data) {
  
  z0 = which(bvd.stat.dly.zs.stat$ZS==z)
  z1 = bvd.stat.dly.zs.stat[z0,2][[1]]
  
  zs.data.wly <- bvd.stat.wly.zs |> filter(zone_sante_notification == z)
  
  nCas.zs.z <- nCas.zs$NombreCas[nCas.zs$zone_sante_notification==z]
  decompte.zs.as <- decompte.zs$cas.class[nCas.zs$zone_sante_notification==z]
  
  max.cas.wk.zs <- zs.data.wly |> 
    summarise(NombreCas = sum(NombreCas), .by = date_debut_sem) |> 
    pull(NombreCas) |> 
    max()
    
  cfr.zs.wly <- zs.data.wly |>
    group_by(date_debut_sem) |>
    summarise(
      Total = sum(NombreCas, na.rm = TRUE),
      Décédé = sum(NombreCas[s6_statut_final_patient == "Décédé [D]"], na.rm = TRUE),
      CFR = ifelse(Total > 0, Décédé / Total, 0),
      .groups = "drop"
    )
    
  coeff.zs <- max.cas.wk.zs * 1.05
  if(is.na(coeff.zs) | coeff.zs == 0) coeff.zs <- 1
  
  bvd.class.st.zs.wly.g <-
    zs.data.wly |>
    ggplot(aes(x=date_debut_sem,
               y=NombreCas,
               fill = s6_statut_final_patient,
               color = s6_statut_final_patient))+
    geom_bar(stat = "identity", width=7)+
    geom_line(data = cfr.zs.wly, 
              aes(x = date_debut_sem, 
                  y = CFR * coeff.zs), 
                  inherit.aes = FALSE, 
                  color = "#370101", 
                  linetype = 6,
                  linewidth = 1) +
    scale_x_date(date_breaks = "1 week",
                 date_labels = paste0("se","%V\n%Y"),
                 expand = c(0.005,0))+
    scale_y_continuous(expand = c(0.0015,0),
                       limits = c(0, max.cas.wk.zs * 1.05),
                       breaks = seq(0, max.cas.wk.zs * 1.05, by = max(1, round((max.cas.wk.zs * 1.05)/5))),
                       sec.axis = sec_axis(~ . / coeff.zs, name = "Létalité", labels = scales::percent_format(accuracy = 1)))+
    scale_fill_manual(values = VDcolors2)+
    scale_color_manual(values = VDcolors2)+
    labs(title = paste0("Tendances cas confirmés et décès ZS ", z1, "\n MVB, RDC"),
         subtitle = paste0(z1, ", ", nCas.zs.z, " cas confirmés dont ", decompte.zs.as, "\n", 
                           "Liste lineaire des cas disponible au ",
                           format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
         y="Nombre des confirmés et décès",
         x="Semaine début des symptômes",
         caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
         fill="", color="")+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey90", linetype = 2),
          panel.grid.major.x = element_line(colour = "grey95", linetype = 2),
          plot.title = element_text(colour = "#234a7d", face = "bold", size = 18),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 8),
          axis.title = element_text(face = "bold", size = 12),
          axis.line = element_line(color = "grey25", linewidth = 0.25),
          axis.ticks = element_line(linewidth = 0.75, colour = "grey75"),
          axis.text = element_text(size = 9, face = "bold"),
          axis.line.y.right = element_line(color = "#370101", linewidth = 0.25),
          axis.text.y.right = element_text(size = 9, face = "bold", color = "#370101"),
          axis.title.y.right = element_text(face = "bold", size = 12, color = "#370101"),
          legend.position = "top",
          legend.key.size = unit(0.5, "cm"))
          
  bvd.class.st.zs.wly.g |>
    ggsave(file=paste0(day.dir.z,"Epicurve.zs.status.Weekly_", z1, "_",
                       format(max.notif.date, "%d%b"),
                       ".png"),
           dpi = 300, height = 20, width = export.width, scale = 0.35)
}
