#### Age-Sexe ####

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

pop.zs <- 
  read.csv(file.path(project_root, "data", "PopulationParAge", "pop_zs_drc.csv"))

pop.prov <-
  read.csv(file.path(project_root, "data", "PopulationParAge", "pop_prov_drc.csv"))

unique(pop.zs$age_group)

###

(max.notif.date <- max(as.Date(evd.data$date_heure_notification_alerte[which(as.Date(evd.data$date_heure_notification_alerte) <= as.Date(now()))]), na.rm = TRUE))

max.onset.date <- as.Date(max(evd.data$s2_date_debut_signes_symptomes))


day.dir.ag <- paste0(here::here("OutPut/Agesex/"),format(max.notif.date, "%d%b"),"/")
day.dir.ag.a <- paste0(here::here("OutPut/Agesex/"),format(max.notif.date, "%d%b"),"/all/")
day.dir.ag.p <- paste0(here::here("OutPut/Agesex/"),format(max.notif.date, "%d%b"),"/prov/")
day.dir.ag.z <- paste0(here::here("OutPut/Agesex/"),format(max.notif.date, "%d%b"),"/zs/")
day.dir.ag.s <- paste0(here::here("OutPut/Agesex/"),format(max.notif.date, "%d%b"),"/as/")
  
if(!dir.exists(day.dir.ag)){
    dir.create(day.dir.ag)
  } 

if (!dir.exists(day.dir.ag.a)){
    dir.create(day.dir.ag.a)
  } 

if (!dir.exists(day.dir.ag.p)){
    dir.create(day.dir.ag.p)
  } 

if (!dir.exists(day.dir.ag.z)){
    dir.create(day.dir.ag.z)
  } 

if (!dir.exists(day.dir.ag.s)){
    dir.create(day.dir.ag.s)
  } 



## Preparing BVD dataset

evd.data.agesex <-
evd.data |>
  filter(classification_finale_cas == "Cas confirmé"|
         lab_resultat_final  == "Positif") |>
  mutate(s6_statut_final_patient = case_when(is.na(s6_statut_final_patient)&
                                             nature_alerte=="Décédé"~"Décédé",
                                            !is.na(s6_statut_final_patient)~s6_statut_final_patient,
                                             TRUE~"Vivant"),
         age = AgeStd(age_ans, age_mois),
         age = case_when(age > 100~ NA, 
                         .default = age),
         age.groups = AgeCat(age, n_cats = 18),
         age.group5 =  AgeCat(age, n_cats = 5)) |>
  select(province_notification,zone_sante_notification,
         aire_sante_notification, s6_statut_final_patient,
         date_heure_notification_alerte,nature_alerte,
        classification_finale_cas,
        sexe,age,age.groups, age.group5) 

filter(evd.data.agesex, age.groups =="80+")
## Over the epidemic, all over the entire affected area


evd.data.agesex.all <-
  evd.data.agesex |>
  filter(!is.na(age.groups)) |>
  count(age.groups, sexe, s6_statut_final_patient, name = "NombreCas") |>
  mutate(
    TtlCas = sum(NombreCas),
    PropCase = round(NombreCas / TtlCas * 100, 1),
    .by = s6_statut_final_patient
  ) |>
  mutate(
    PropCase = if_else(sexe == "Masculin", -PropCase, PropCase)
  )

decompte.all.agesex <-
evd.data.agesex |>
  count(s6_statut_final_patient, name = "NombreCas") |>
  mutate(s6_statut_final_patient = paste0(s6_statut_final_patient,"s"),
         statu.cas = paste(NombreCas, s6_statut_final_patient, sep=" ")) |>
  summarise(statu.cas = paste(statu.cas, collapse = " et "))

prov.agesex <- 
  evd.data.agesex |>
  count(province_notification, name = "NombreCas") |>
  arrange(desc(NombreCas)) |> pull(province_notification)


nprov = unique(evd.data.agesex$province_notification)

evd.data.agesex.prov <- 
  evd.data.agesex |>
  filter(!is.na(age.groups)) |>
  filter(province_notification %in% prov.agesex) |>
  count(age.groups, sexe, 
        province_notification, name = "NombreCas") |>
  mutate(
    TtlCas = sum(NombreCas),
    PropCase = round(NombreCas / TtlCas * 100, 1),
    .by = c(province_notification)
  ) |>
  mutate(
    PropCase = if_else(sexe == "Masculin", -PropCase, PropCase),
    province_notification = factor(province_notification,
                                     levels = prov.agesex)
   )


evd.data.agesex.zs.l <-
evd.data.agesex |>
  count(zone_sante_notification, name = "NombreCas") |>
  arrange(desc(NombreCas)) |>
  top_n(4) |>
  pull(zone_sante_notification)


evd.data.agesex.zs <- 
  evd.data.agesex |>
  filter(!is.na(age.groups)) |>
  filter(zone_sante_notification %in% evd.data.agesex.zs.l) |>
  count(age.groups, sexe, 
        zone_sante_notification, name = "NombreCas") |>
  mutate(
    TtlCas = sum(NombreCas),
    PropCase = round(NombreCas / TtlCas * 100, 1),
    .by = c(zone_sante_notification)
  ) |>
  mutate(
    PropCase = if_else(sexe == "Masculin", -PropCase, PropCase),
    zone_sante_notification = factor(zone_sante_notification,
                                     levels = evd.data.agesex.zs.l)
   )


evd.data.agesex.ratio <-
evd.data.agesex |>
  #filter(zone_sante_notification %in% evd.data.agesex.zs.l) |>
  count(sexe,province_notification,
       zone_sante_notification, name = "NombreCas")|>
  pivot_wider(names_from = sexe,
              values_from = NombreCas,
              values_fill = 0) |>
  mutate(ratioMF = round(Masculin/Feminin, 1)) |>
  bind_rows(evd.data.agesex |>
  count(sexe, name = "NombreCas")|>
  pivot_wider(names_from = sexe,
              values_from = NombreCas,
              values_fill = 0) |>
  mutate(ratioMF = round(Masculin/Feminin, 1),
         zone_sante_notification = "Toutess",
         province_notification = "Toutess")) |>
  mutate(ratioMF = ratioMF-1,
         ent.cat = ifelse(zone_sante_notification == "Toutess",
                          "Toutess","zone de santé"),
        zone_sante_notification=factor(zone_sante_notification,
                                      levels = zone_sante_notification[order(ratioMF)]))

decompte.all.gender <- 
  evd.data.agesex.ratio |>
  filter( province_notification == "Toutess") |>
  pivot_longer(cols = c(Feminin,Masculin),
               names_to = "sexe",
               values_to = "NombreCas") |>
  mutate(sex.cas = paste(NombreCas," de sexe ",sexe,sep=" ")) |>
  summarise(ratioMF=mean(ratioMF),sex.cas = paste(sex.cas, collapse = " et "))|>
  mutate(sex.cas = paste(sex.cas, ", ratio Homme/Femme de ",ratioMF,sep=" "))|>
  pull(sex.cas)



#



######

fillPyrCol= 
  adjustcolor("white", alpha.f = 0.05)

###


evd.postifs.sex.dnt <-
  evd.data |>
  filter(classification_finale_cas == "Cas confirmé"|
         lab_resultat_final  == "Positif") |>
  count(sexe, name = "Nombre_de_Cas_Positifs") |>
  mutate(ratio = round(Nombre_de_Cas_Positifs/sum(Nombre_de_Cas_Positifs)*100,1),
         sexe = factor(sexe,
                       levels = sexe[order(Nombre_de_Cas_Positifs, decreasing = TRUE)]),
         maxy = cumsum(Nombre_de_Cas_Positifs),
         miny = c(0, head(maxy, n=-1)),
         label.pos = (miny + maxy) / 2) |>
  filter(!is.na(sexe))

sexratio <- 
  paste0("Sex ratio ",levels(evd.postifs.sex.dnt$sexe)[2], " : ",levels(evd.postifs.sex.dnt$sexe)[1], " est de ",
     round(evd.postifs.sex.dnt$ratio[2]/evd.postifs.sex.dnt$ratio[1],1),":1")

cat("Sex ratio ",
     levels(evd.postifs.sex.dnt$sexe)[2], "to ",levels(evd.postifs.sex.dnt$sexe)[1], "is :","\n",
     round(evd.postifs.sex.dnt$ratio[2]/evd.postifs.sex.dnt$ratio[1],1),":1")

Ttl.cases <- sum(evd.postifs.sex.dnt$Nombre_de_Cas_Positifs)
#Ttl.cases = 956

##### Age-Sex Pyramid #####


agefill=
  c("<1Ans"="#4b644b",
    "1-4Ans"="#748b5e",
    "5-14Ans"="#a1c1b1",
    "15-19Ans"="#e6eff6",
    "20-40Ans"="#b5bfc8",
    ">40Ans"="#59626a")


Agefill2 =
  c("0-4"="#69b9d5",
    "5-9"="#1e97c0",
    "10-14"="#046f94",
    "15-19"="#03536f",
    "20-24"="#02384a",
    "25-29"="#011c25",
    "30-34"="#435a65",
    "35-39"="#72838c",
    "40-44"="#8b9092" ,
    "45-40"="#b9c1c5",
    "50+"="#eceef0")


SexFill =
  c("Masculin"="#bd6009" ,"Feminin"="#640b7c")


##

evd.postifs.sex.dnt.g <-
evd.postifs.sex.dnt |>
  ggplot(aes(ymax=maxy, ymin=miny, xmax=4, xmin=3, fill=factor(sexe))) +
  geom_rect() +
  annotate("text", x=2, y=0, 
           label=paste0(Ttl.cases,"\n", "Cas","\n","confirmés"), size=4, fontface="bold")+
  ggrepel::geom_label_repel(aes(x=4, y=label.pos,
                           label= paste0(ratio,"%")),
                           fill="#ffffff5d", nudge_y = 0, 
                           nudge_x = 0, 
                           color="#010b17",
                           direction = "both", hjust = 0.5, vjust  = 0.25,
                           max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
                           size=6)+
  labs(title = "Cas confirmés MVB par sexe, RDC",
       subtitle = paste0(sexratio,"\n","Distribution à la date du ", 
                         format(max.notif.date, "%d-%m-%Y")),
       caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
       fill="")+
  scale_fill_manual(values = SexFill)+
  coord_polar(theta="y") +
  xlim(c(2, 4.25)) +
  theme_void() +
  theme(plot.title = element_text(size = 18, face = "bold", color = "#3e62bd"),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10, hjust = 1),
        legend.position = "bottom")


evd.postifs.sex.dnt.g |>
  ggsave(file=paste0(day.dir.ag.a,"Distr.sex.donut_",
                     format(max.notif.date, "%d%b"),".png"), 
         dpi = 300, height = 22, width = 20, scale = 0.3)





###

age.lvl <- sort(gsub("_","-",unique(pop.zs$age_group)))[c(1,2,11,3:10,12:18)]
age.lvl = gsub("p","+", age.lvl)
age.lvl = ifelse(age.lvl=="0-1","<1", age.lvl)

pop.zs.all <-
  pop.zs |>
  filter(adm1_viz_n %in% c("Ituri", "Nord KIvu")) |>
  summarise(pop = sum(pop),
            .by = c(age_group,gender)) |>
  ungroup() |>
  mutate(sexe= case_when(gender=="Male" ~ "Masculin",
                         gender == "Female" ~ "Feminin",
                         .default = NA),
         pop.perct = round(pop/sum(pop)*100,1),
         pop.perct = case_when(gender == "Male" ~ -pop.perct,
                               .default = pop.perct),
         age_group = gsub("_","-",age_group),
         age_group = gsub("p","+",age_group),
         age_group = ifelse(age_group=="0-1","<1", age_group),
         age_group = factor(age_group, levels = age.lvl))
  

nCas = Ttl.cases


(evd.data.agesex.all.g <-
    evd.data.agesex.all |>
    ggplot(aes(x=age.groups,
               y=PropCase,
               fill = sexe))+
    geom_bar(stat = "identity",
             color = "grey90",
             width = 1)+
    geom_bar(data = pop.zs.all,
             aes(x=age_group, 
                 y=pop.perct),
                 fill="#dde8f300",
                 color = "grey10",
                 stat = "identity")+
    geom_hline(yintercept = 0, linetype = 1, size = 0.5)+
    coord_flip()+
      scale_fill_manual(values = SexFill)+
    scale_y_continuous(expand = c(0,0),
                       labels = function(x) paste0(abs(x),"%"))+
    scale_x_discrete(expand = c(0,0))+
    facet_wrap(~s6_statut_final_patient, ncol=2, scales = "free_y")+
    labs(title = paste0("Distribution Age-Sexe des cas confirmés et décès de la MVB, RDC"),
         subtitle = paste0(paste(prov.agesex, collapse = ", "),", RDC","\n",
                          nCas, " cas confirmés dont ",decompte.all.agesex,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
         caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
         y="Pourcentage de cas",x="",
         fill="") +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_line(linetype = 3,
                                            color = "grey",size = 0.4),
          panel.grid.major.y = element_line(linetype = 3,
                                            color = "grey",size = 0.4),
          axis.line.x = element_line(colour = "grey70", 
                                     linewidth = 0.5),
          axis.text = element_text(size = 9,
                                   face = "bold"),
          axis.title = element_text(face = "bold",size = 10),
          axis.ticks.y = element_blank(),
          plot.title = element_text(colour = "#3e62bd",
                                    face = "bold",
                                    size = 16),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 7),
          legend.position = "top",
          legend.title = element_blank(),
          legend.key.size = unit(0.5,"cm"),
          strip.text = element_text(face = "bold",size = 10),
          strip.background = element_rect(fill = "grey80"))
        )



evd.data.agesex.all.g |>
  ggsave(filename=paste0(day.dir.ag.a,"AgeSexBVD_",format(max.notif.date, "%d%b"),".all_",
                         format(max.notif.date, "%d%b"),".png"),
         dpi = 300, height = 20, width = 25, scale = 0.35)


## Provinces

evd.data.agesex.prov

unique(pop.prov$adm1_viz_n)

pop.prov.f <-
  pop.prov |>
  mutate(adm1_viz_n = str_replace_all(adm1_viz_n, "-", " "),
        adm1_viz_n = str_squish(adm1_viz_n)) |>
  filter(adm1_viz_n %in% prov.agesex) |>
  group_by(adm1_viz_n) |>
  mutate(sexe= case_when(gender=="Male" ~ "Masculin",
                         gender == "Female" ~ "Feminin",
                         .default = NA),
         PropCase = round(pop/sum(pop)*100,1),
         PropCase = case_when(gender == "Male" ~ -PropCase,
                               .default = PropCase),
         age_group = gsub("_","-",age_group),
         age_group = gsub("p","+",age_group),
         age_group = ifelse(age_group=="0-1","<1", age_group),
         age_group = factor(age_group, levels = age.lvl)) |>
  rename(province_notification = adm1_viz_n)

(evd.data.agesex.prov.g <-
    evd.data.agesex.prov |>
    ggplot(aes(x=age.groups,
               y=PropCase,
               fill = sexe))+
    geom_bar(stat = "identity",
             color = "grey90",
             width = 1)+
    geom_bar(data = pop.prov.f,
             aes(x=age_group, 
                 y=PropCase),
                 fill="#dde8f300",
                 color = "grey10",
                 stat = "identity")+
    geom_hline(yintercept = 0, linetype = 1, size = 0.5)+
      scale_fill_manual(values = SexFill)+
    scale_y_continuous(expand = c(0,0),
                       labels = function(x) paste0(abs(x),"%"))+
    scale_x_discrete(expand = c(0,0))+
    coord_flip()+
    facet_wrap(~province_notification, ncol=3, scale= "free_x")+
    labs(title = paste0("Distribution Age-Sexe des cas confirmés de la MVB, RDC"),
         subtitle = paste0(paste(prov.agesex, collapse = ", "),", RDC","\n",
                          nCas, " cas confirmés","\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
         caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
         y="Pourcentage de cas",x="",
         fill="") +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_line(linetype = 3,
                                            color = "grey",size = 0.4),
          panel.grid.major.y = element_line(linetype = 3,
                                            color = "grey",size = 0.4),
          axis.line.x = element_line(colour = "grey70", 
                                     linewidth = 0.5),
          axis.text = element_text(size = 9,
                                   face = "bold"),
          axis.title = element_text(face = "bold",size = 10),
          axis.ticks.y = element_blank(),
          plot.title = element_text(colour = "#3e62bd",
                                    face = "bold",
                                    size = 16),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 7),
          legend.position = "top",
          legend.title = element_blank(),
          legend.key.size = unit(0.5,"cm"),
          strip.text = element_text(face = "bold",size = 10),
          strip.background = element_rect(fill = "grey80"))
        )



evd.data.agesex.prov.g |>
  ggsave(filename=paste0(day.dir.ag.p,"AgeSexBVD.prov_",
                         format(max.notif.date, "%d%b"),".png"),
         dpi = 300, height = 22, width = 30, scale = 0.35)

####



##

hspot.zs <- unique(evd.data.agesex.zs$zone_sante_notification)

pop.zs.zs <-
  pop.zs |>
  filter(adm2_viz_n %in% hspot.zs) |>
  group_by(adm2_viz_n) |>
  mutate(sexe= case_when(gender=="Male" ~ "Masculin",
                         gender == "Female" ~ "Feminin",
                         .default = NA),
         PropCase = round(pop/sum(pop)*100,1),
         PropCase = case_when(gender == "Male" ~ -PropCase,
                               .default = PropCase),
         age_group = gsub("_","-",age_group),
         age_group = gsub("p","+",age_group),
         age_group = ifelse(age_group=="0-1","<1", age_group),
         age_group = factor(age_group, levels = age.lvl)) |>
  rename(zone_sante_notification=adm2_viz_n)

(evd.data.agesex.zs.g <-
    evd.data.agesex.zs |>
    ggplot(aes(x=age.groups,
               y=PropCase,
               fill = sexe))+
    geom_bar(stat = "identity",
             color = "grey90",
             width = 1)+
    geom_bar(data = pop.zs.zs,
             aes(x=age_group, 
                 y=PropCase),
                 fill="#dde8f300",
                 color = "grey10",
                 stat = "identity")+
    geom_hline(yintercept = 0, linetype = 1, size = 0.5)+
      scale_fill_manual(values = SexFill)+
    scale_y_continuous(expand = c(0,0),
                       labels = function(x) paste0(abs(x),"%"))+
    scale_x_discrete(expand = c(0,0))+
    coord_flip()+
    facet_wrap(~zone_sante_notification, ncol=2, scale= "free_x")+
    labs(title = paste0("Distribution Age-Sexe des cas confirmés et décès de la MVB, RDC"),
         subtitle = paste0(paste(evd.data.agesex.zs.l, collapse = ", "),", RDC","\n",
                          nCas, " cas confirmés","\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
         caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
         y="Pourcentage de cas",x="",
         fill="") +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_line(linetype = 3,
                                            color = "grey",size = 0.4),
          panel.grid.major.y = element_line(linetype = 3,
                                            color = "grey",size = 0.4),
          axis.line.x = element_line(colour = "grey70", 
                                     linewidth = 0.5),
          axis.text = element_text(size = 9,
                                   face = "bold"),
          axis.title = element_text(face = "bold",size = 10),
          axis.ticks.y = element_blank(),
          plot.title = element_text(colour = "#3e62bd",
                                    face = "bold",
                                    size = 16),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 7),
          legend.position = "top",
          legend.title = element_blank(),
          legend.key.size = unit(0.5,"cm"),
          strip.text = element_text(face = "bold",size = 10),
          strip.background = element_rect(fill = "grey80"))
        )



evd.data.agesex.zs.g |>
  ggsave(filename=paste0(day.dir.ag.z,"AgeSexBVD.zs_",
                         format(max.notif.date, "%d%b"),".png"),
         dpi = 300, height = 22, width = 22, scale = 0.35)

####

prov.clrs <- c("#212c73","#2a4d69", "#4b86b4", "#63ace5","#9fb6cd")

fill.prov <- colorRampPalette(prov.clrs)

(evd.data.agesex.ratio.g <-
  evd.data.agesex.ratio |>
  ggplot(aes(x=zone_sante_notification,
             y=ratioMF,
            fill=province_notification))+
  geom_bar(stat = "identity")+
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.0125)),
                     labels = function(x) x+1)+
  geom_hline(yintercept = 0, linetype = 1, size = 0.5)+
  coord_flip()+
  scale_fill_manual(values = fill.prov(length(nprov)+1))+
    labs(
    title = "Distribution du ratio Masculin/Féminin par ZS, RDC",
    subtitle = paste0(paste(prov.agesex, collapse = ", "),", RDC","\n",
                          nCas, " cas confirmés dont ",decompte.all.gender,"\n", 
                         "Liste lineaire des cas disponible au ",
                         format(max.notif.date,"%d-%m-%y"), ", SGI MVB"),
    y = "Ratio Homme/Femme", x = NULL,
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now())),
    fill = "") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_line(linewidth = 0.25, colour = "grey80"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, hjust = 1),
    axis.title = element_text(size = 12),
    axis.line.x = element_line(color = "black", linewidth = 0.2),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    plot.title = element_text(size = 16, face = "bold", color = "#03084a"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 7, hjust = 1),
    legend.position = "top",
    legend.key.size = unit(0.5,"cm")
  ))

evd.data.agesex.ratio.g |>
  ggsave(filename=paste0(day.dir.ag.a,"AgeSexBVD.ratio_",
                         format(max.notif.date, "%d%b"),".png"),
         dpi = 300, height = 20, width = 25, scale = 0.35)

## Age risk factor

evd.data.agesex.rr <-
  evd.data.agesex |>
    filter(!is.na(age)) |>
    summarise(Freq = n(),
              .by = c("s6_statut_final_patient",
                      "age.group5")) |>
    pivot_wider(names_from = s6_statut_final_patient,
                values_from = Freq) |>
    mutate(age.group5 = factor(age.group5, 
                               levels = levels(evd.data.agesex$age.group5)),
          crf = Décédé/(Décédé+Vivant))

chi.t.evd.data.agesex.rr <-
  chisq.test(matrix(c(evd.data.agesex.rr$Décédé, evd.data.agesex.rr$Vivant), 
        ncol = 2))


ref.age <- 
  as.character(evd.data.agesex.rr$age.group5[which.min(evd.data.agesex.rr$crf)])

other.age <-
  sort(as.character(evd.data.agesex.rr$age.group5[which(evd.data.agesex.rr$age.group5 != ref.age)]))


RR.sample <- sum(evd.data.agesex.rr$Décédé,evd.data.agesex.rr$Vivant)

# Risk ratios for death by age group (ref = lowest CFR age group)
age.rr.results <- data.frame(
  age.group    = character(),
  ref.group    = character(),
  risk.ratio   = numeric(),
  lower.ci     = numeric(),
  upper.ci     = numeric(),
  p.value      = numeric()
)

for (age in other.age) {
  # Subset to reference vs. current age group; drop unused factor levels
  sub <- evd.data.agesex.rr |>
    filter(age.group5 %in% c(age, ref.age)) |>
    mutate(age.group5 = factor(age.group5, levels = c(age, ref.age)))

  # 2x2 table: rows = age group (ref first), cols = outcome (Décédé, Vivant)
  tab <- as.table(matrix(
    c(sub$Décédé[sub$age.group5 == age],
      sub$Vivant[sub$age.group5 == age],
      sub$Décédé[sub$age.group5 == ref.age],
      sub$Vivant[sub$age.group5 == ref.age]),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      Exposure = c(age, ref.age),
      Outcome  = c("Décédé","Vivant")
    )
  ))

  res <- epiR::epi.2by2(tab, method = "cohort.count", conf.level = 0.95)

  rr_row <-   res$massoc.summary[res$massoc.summary$var=="Inc risk ratio",]

  age.rr.results <- 
    rbind(age.rr.results, 
      data.frame(
    age.group  = age,
    ref.group  = ref.age,
    risk.ratio = rr_row["est"],
    lower.ci   = rr_row["lower"],
    upper.ci   = rr_row["upper"],
    p.value    = res$massoc.detail$chi2.strata.fisher[[3]]
 
       ))
}

age.rr.results.df <-
  age.rr.results |>
  bind_rows(
    data.frame(
      age.group = "25-49",
      ref.group = "25-49",
      est = 1,
      lower = 1,
      upper = 1,
      p.value = 1
    )
  ) |>
  mutate(age.group = factor(age.group,levels = levels(evd.data.agesex$age.group5))) 

age.rr.results.df.g <-
  age.rr.results.df |>
  ggplot(aes(x= age.group, y= est))+
  geom_hline(yintercept = 1, linetype="dashed", color="grey50")+
  geom_point(shape=18, size=3)+
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.1, color="grey50")+
  coord_flip() +
   scale_y_continuous(expand = c(0.02,0))+
  labs(
    title = "Risque de décès par age des cas confirmés de MVE/B, RDC",
    subtitle = paste0("Données à la date du ", format(max.notif.date, "%d-%m-%Y"),"\n",
                      "Analyse basée sur ",RR.sample, " Cas confirmés de MVE/B avec age disponible","\n",
                      names(chi.t.evd.data.agesex.rr$statistic)," ",
                      round(chi.t.evd.data.agesex.rr$statistic,2),
                      "(df ",chi.t.evd.data.agesex.rr$parameter[[1]],"), p-value ",
                      ifelse(chi.t.evd.data.agesex.rr$p.value < 0.001, " < 0.001",
                             chi.t.evd.data.agesex.rr$p.value)),
    y = "Rapport de risque", x = "Groupes age",
    caption = paste0("IOA - CAI ","\u00a9",isoyear(now()))) +
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


age.rr.results.df.g |>
  ggsave(filename = paste0(day.dir.ag.a,"AgeS.RiskRatio_",
                         format(max.notif.date, "%d%b"),".png"),
         dpi = 300, height = 20, width = 22, scale = 0.35)
