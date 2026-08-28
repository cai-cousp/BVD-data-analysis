#### Transmission ####

#### loading libraries ####


My.Packages = 
  installed.packages()[,"Package"]

if(!"pacman" %in% My.Packages) {
  
  install.packages("pacman")
} else{
  
  pacman::p_load("openxlsx","incidence",
                 "lubridate","tidyverse",
                 "tidytext", "forecast",
                 "DescTools","epicontacts","R0","EpiEstim",
                 install = T)
  
}

#### Loading custom functions #####

source(here::here("helpers", "paths.R"))
source(here::here("helpers", "AgeCat.R"))
source(here::here("helpers", "AgeStd.R"))
source(here::here("helpers", "DebutSem.R"))
source(here::here("helpers", "LoadLatestData.R"))

## Importing evd data

evd.data <- load_latest_data(
  data_cleaning_processed,
  folder_name = NULL,
  format = "rds",
  file_pattern = ".*\\.rds$"
)

infector.infectees <- 
  read.xlsx(here::here("data/InfectorInfectees.xlsx"))

names(evd.data)

## Subsetting the dataset to transmission-related variables

which(grepl("result",names(evd.data), ignore.case = T))
names(evd.data)[grep("result",names(evd.data), ignore.case = T)]

table(evd.data$lab_resultat_final, useNA = "ifany")

table(evd.data$s1_malade_etait_il_contact_connu[which(evd.data$lab_resultat_final=="Positif")], useNA = "ifany")
table(evd.data$s1_malade_etait_il_contact_suivi[which(evd.data$lab_resultat_final=="Positif")], useNA = "ifany")

table(evd.data$s4_ma1_types_contact[which(evd.data$lab_resultat_final=="Positif")], useNA = "ifany")




evd.data.t <-
  evd.data |>
  mutate(s4_ma1_types_contact = trimws(gsub("-.*","",s4_ma1_types_contact))) |>
  select(c(7:14,128,131:134,137:143,146:162,165:175,178:184,
           187:191,194:196,199:205,207:209,212:213,215:230,240,260,276,277)
        )

evd.data.t  |> names()

names(evd.data)

unique(evd.data.t$s4_ma1_cas_ebola)

evd.data.transm <-
  evd.data.t |> 
  filter(lab_resultat_final=="Positif"|
         s4_1_il_y_a_t_il_eu_contacts_avec_malade_ebola_connu_suspect_simplement_avec_personne_malade=="Oui"|
         !is.na(s4_ma1_nom_malade_potentiel)|
         !is.na(s4_ma2_nom_malade_potentiel)|
         !is.na(s4_ma3_nom_malade_potentiel)|
         s4_ma3_cas_ebola %in% c("Probable","Confirmé")|
         s4_ma2_cas_ebola %in% c("Probable","Confirmé")|
         s4_ma3_cas_ebola %in% c("Probable","Confirmé")|
         s4_pf2_cas_ebola %in% c("Probable","Confirmé")|
         s4_pf1_cas_ebola %in% c("Probable","Confirmé"))



evd.data.transm |>
  filter(grepl("katusabe",om_post_nom_prenom_cas, ignore.case = TRUE)) |>
  select(om_post_nom_prenom_cas,sexe,s4_ma1_nom_malade_potentiel)

evd.data |>
  filter(grepl("katusabe",om_post_nom_prenom_cas, ignore.case = TRUE)) |>
  select(om_post_nom_prenom_cas,age_ans,sexe,date_heure_notification_alerte,
       s4_ma1_date_s_contact_debut,
    s1_occupation,s4_ma1_nom_malade_potentiel,
        s5_qu_prelev_a_deja_ete_soumis_malade,nature_alerte,lab_resultat_final)

names(evd.data)

evd.data.transm |> 
  filter(classification_finale_cas=="Cas confirmé") |>
  count(s4_ma1_lien_parente)

names(evd.data.transm)

contact.persons <-
  sort(unique(trimws(c(infector.infectees$Infector,infector.infectees$Infectees))))


evd.data.transm |>
  mutate(om_post_nom_prenom_cas = trimws(om_post_nom_prenom_cas)) |>
  filter(!om_post_nom_prenom_cas %in% contact.persons) |>
  select(om_post_nom_prenom_cas,age_ans,sexe,
        s4_1_il_y_a_t_il_eu_contacts_avec_malade_ebola_connu_suspect_simplement_avec_personne_malade,
        s4_ma1_nom_malade_potentiel,s4_ma2_nom_malade_potentiel,s4_ma3_nom_malade_potentiel,
        s4_ma1_personne_etait_vivante_decedee,s4_ma1_date_s_contact_debut,
        s4_5_patient_a_t_il_consulte_tradipraticien_maison_priere_avant_maladie_actuelle,
        s4_tr_si_oui_nom_tradipraticien,
        lab_resultat_final,classification_finale_cas) |> View()


##### contacts chain analysis #####

evd.data |>
  select()




evdcontacts <-
  evdkas01 |>
  filter(`Classification finale` %in% c("Confirme","Probable")|
           `Resultats Labo`=="Positif") |>
  transmute(source.id=`Cas Source ID`,
            case.id=`N° EPIDEMIO`,
            class=`Classification finale`,
            as=AS,
            InfOport=CirconstancesLiens,
            TransOpport=case_when(CirconstancesLiens %in% c("Famille Routine",
                                                            "Funerailles")~"Transmission communautaire",
                                  CirconstancesLiens %in% c("Nosocomiale","Garde malade","PPL Routine")~
                                    "Transmission en milieu de soins",
                                  .default = NA)
            ) |>
  drop_na(source.id)


evdtranspar0<-
  evdcontacts |>
  left_join(evdkas01 |>
              select(`N° EPIDEMIO`,
                     Date_debut_symptomes,
                     Sexe, Age_groups,
                     Age_IDSR),
            by=c("source.id"="N° EPIDEMIO")) |>
  rename(OnsetSource=Date_debut_symptomes,
         SexeSource=Sexe,
         AgeSource=Age_groups,
         AgeSourceR=Age_IDSR) |>
  left_join(evdkas01 |>
              select(`N° EPIDEMIO`,
                     Date_debut_symptomes,
                     Sexe, Age_groups,
                     Age_IDSR),
            by=c("case.id"="N° EPIDEMIO")) |>
  rename(OnsetCase=Date_debut_symptomes,
         SexeCase=Sexe,
         AgeCase=Age_groups,
         AgeCaseR=Age_IDSR) |>
  mutate_at(vars(AgeSource,AgeCase),
            ~factor(.,levels = 
                      c("0","1-4","10-14","15-19",
                        "20-24","25-29","30-34","35-39",
                        "40-44", "45-40", "50+")))


evdll <-
  evdkas01 |>
  select(Noms,
         `N° EPIDEMIO`,
         Date_debut_symptomes,
         Sexe,Age_Std,
         Profession,
         CirconstancesLiens,
         #labid,
         Province,ZS,AS,
         `Cas Source ID`,`Resultats Labo`,
         `Classification finale`)|>
  drop_na(`N° EPIDEMIO`) |>
  group_by(`N° EPIDEMIO`)|>
  mutate(nt=n(),
         `N° EPIDEMIO`=ifelse(nt>1,
                              paste0(`N° EPIDEMIO`,
                                     "_",
                                     row_number()),
                              `N° EPIDEMIO`)) |> 
  select(-nt)

evdll |>
  filter(grepl("_\\d$",`N° EPIDEMIO`)) |>
  select(`N° EPIDEMIO`,Noms, 
         Sexe,Age_Std,
         `Resultats Labo`,`Classification finale`)

table(evdkas01$CirconstancesLiens, useNA = "ifany")

evdll.ConfProb <-
  evdkas01 |> 
  filter(`Classification finale` %in% 
           c("Confirme","Probable")) |>
  select(Noms,
         `N° EPIDEMIO`,
         Date_debut_symptomes,
         Sexe,Age_Std,
         #labid,
         Province,ZS,AS,
         `Cas Source ID`,`Resultats Labo`,
         CirconstancesLiens,
         `Classification finale`,
         Profession,
         Issue)|>
  drop_na(`N° EPIDEMIO`)|>
  group_by(`N° EPIDEMIO`)|>
  mutate(TransOpport=case_when(CirconstancesLiens %in% c("Famille Routine",
                                                          "Funerailles")~"Transmission communautaire",
                                CirconstancesLiens %in% c("Nosocomiale","Garde malade","PPL Routine")~
                                  "Transmission en milieu de soins",
                                .default = NA),
         nt=n(),
         `N° EPIDEMIO`=ifelse(nt>1,
                              paste0(`N° EPIDEMIO`,
                                     "_",
                                     row_number()),
                              `N° EPIDEMIO`)) |> 
  select(-nt)

table(evdll.ConfProb$TransOpport)

table(evdkas01$`Classification finale`)

evdll |>
  View()



evd.trans <-
  make_epicontacts(
    linelist = evdll,
    contacts = evdcontacts,
    id = "N° EPIDEMIO",
    from = "source.id",
    to = "case.id",
    directed = TRUE
  )

evd.trans1 <-
  make_epicontacts(
    linelist = evdll[evdll$`Classification finale` %in%
                       c("Confirme","Probable"),],
    contacts = evdcontacts,
    id = "N° EPIDEMIO",
    from = "source.id",
    to = "case.id",
    directed = TRUE
  )

evd.conf <-
  make_epicontacts(
    linelist = evdll.ConfProb,
    contacts = evdcontacts,
    id = "N° EPIDEMIO",
    from = "source.id",
    to = "case.id",
    directed = TRUE
  )


evdcontacts |>
  group_by(TransOpport) |>
  count(source.id) |>
  summarise(nSource=sum(n))


table(evdcontacts$TransOpport)
table(evdcontacts$source.id)


###

names(evd.trans)

table(evdll$CirconstancesLiens)





classColor=
  colorRampPalette(c("Confirme"="#bf2447",
                     "Indeterminé"="#948c8a",
                     "Non Cas"="#60c59f",
                     "Probable"="#f07a44",
                     "Suspect"="#eedf2a"))

classColorConf= 
  colorRampPalette(c("Confirme"="#bf2447",
                     "Probable"="#f07a44"))

classColorD= 
  colorRampPalette(c("Décédé"="#bf2447",
                     "Vivant"="#60c59f"))

transmOpport =
  colorRampPalette(c("Transmission communautaire"="#f07a44",
                     "Transmission en milieu de soins"="#bf2447"))

###

plot(evd.trans,
     #title="MVE Kasai: Chaine de transmission global",
     node_color = "Classification finale",
     node_size = "Age_Std",
     col_pal = classColor,
     node_shape = "Sexe",
     shape = c(Féminin="female", Masculin="male"),
     edge_color = "as",
     edge_width=1.5,
     arrow_size=0.75,
     selector=F,
     legend_max=12,
     legend_width=0.075,
     #edge_linetype = "as",
     width = "100%",
     height="100%",
     size_range = c(20,50),#heigth = 1800,width = 1800
) |> visNetwork::visSave(paste0(here::here("OutPut/Transmission/TransmissionChainAll_"),
                                maxOnset,".html"))


table(evdll.ConfProb$Profession)

plot(evd.trans1,
     #title="MVE Kasai: Chaine de transmission global",
     node_color = "CirconstancesLiens",
     node_size = "Age_Std",
     #col_pal = classColor,
     node_shape = "Sexe",
     shape = c(Féminin="female", Masculin="male"),
     thin=FALSE,
     edge_color = "as",
     edge_width=1.5,
     arrow_size=0.75,
     selector=F,
     legend_max=12,
     legend_width=0.075,
     #edge_linetype = "as",
     width = "100%",
     height="100%",
     size_range = c(20,50),#heigth = 1800,width = 1800
     ) |> visNetwork::visSave(paste0(here::here("OutPut/Transmission/TransmissionChain_TransOport_"),
                                     maxOnset,".html"))

plot(evd.conf,
     #title="MVE Kasai: Chaine de transmission global",
     node_color = "TransOpport",
     node_size = "Age_Std",
     col_pal = transmOpport,
     node_shape = "Sexe",
     shape = c(Féminin="female", Masculin="male"),
     thin=FALSE,
     edge_color = "as",
     edge_width=1.5,
     arrow_size=0.75,
     selector=F,
     legend_max=12,
     legend_width=0.075,
     #edge_linetype = "as",
     width = "100%",
     height="100%",
     size_range = c(20,50),#heigth = 1800,width = 1800
) |> visNetwork::visSave(paste0(here::here("OutPut/Transmission/TransmissionChain_TransOport2_"),
                                maxOnset,".html"))


plot(evd.trans1,
     #title="MVE Kasai: Chaine de transmission global",
     node_color = "Classification finale",
     node_size = "Age_Std",
     col_pal = classColor,
     node_shape = "Sexe",
     shape = c(Féminin="female", Masculin="male"),
     thin=FALSE,
     edge_color = "as",
     edge_width=1.5,
     arrow_size=0.75,
     selector=F,
     legend_max=12,
     legend_width=0.075,
     #edge_linetype = "as",
     width = "100%",
     height="100%",
     size_range = c(20,50),#heigth = 1800,width = 1800
) |> visNetwork::visSave(paste0(here::here("OutPut/Transmission/TransmissionChain_"),
                                maxOnset,".html"))


plot(evd.conf,
     #title="MVE Kasai: Chaine de transmission temporelle (confirmés & Probables)",
     x_axis="Date_debut_symptomes",
     node_color = "Classification finale",
     node_size = "Age_Std",
     col_pal = classColorConf,
     thin=F,
     #node_shape = "Sexe",
     #shape = c(Féminin="female", Masculin="male"),
     edge_color = "as",
     edge_width=1.5,
     arrow_size=0.75,
     label=F,
     position_dodge=T,
     #edge_linetype = "as",
     #size_range = c(25,50),
     parent_pos = "middle",
     selector=F,
     legend_max=12,
     legend_width=0.075,
     date_labels="%d-%m-%y",
     heigth = 1500,
     width = 1500)|> visNetwork::visSave(paste0(here::here("OutPut/Transmission/TransmissionChainTemp_"),
                                                maxOnset,".html"))


plot(evd.conf,
     #title="MVE Kasai: Chaine de transmission temporelle (confirmés & Probables)",
     x_axis="Date_debut_symptomes",
     node_color = "Classification finale",
     node_size = "Age_Std",
     col_pal = classColorConf,
     #node_shape = "Sexe",
     #shape = c(Féminin="female", Masculin="male"),
     edge_color = "as",
     edge_width=1.5,
     arrow_size=0.75,
     label=F,
     position_dodge=T,
     #edge_linetype = "as",
     #size_range = c(25,50),
     parent_pos = "middle",
     selector=F,
     legend_max=12,
     legend_width=0.075,
     date_labels="%d-%m-%y",
     heigth = "100%",
     width = "100%")|> visNetwork::visSave(paste0(here::here("OutPut/Transmission/TransmissionChainTemp_"),
                                               maxOnset,".html"))

table(evdll.ConfProb$`Classification finale`)

plot(evd.conf,
     #title="MVE Kasai: Chaine de transmission temporelle basée sur l'issue",
     x_axis="Date_debut_symptomes",
     node_color = "Issue",
     node_size = "Age_Std",
     col_pal = classColorD,
     #node_shape = "Sexe",
     #shape = c(Féminin="female", Masculin="male"),
     edge_color = "as",
     edge_width=1.5,
     arrow_size=0.75,
     label=F,
     position_dodge=T,
     #edge_linetype = "as",
     #size_range = c(25,50),
     parent_pos = "middle",
     selector=F,
     legend_max=12,
     legend_width=0.075,
     date_labels="%d-%m-%y",
     heigth = 1500,
     width = 1500)|> visNetwork::visSave(paste0(here::here("OutPut/Transmission/TransmissionChainTempVD_"),
                                                maxOnset,".html"))


summary(evd.trans)

##### Estimating si, R0 and Re #####

sgiDate=as.Date("2025-09-15")

evdgt0 <-
  evdtranspar0 |>
  transmute(si = as.numeric(OnsetCase-OnsetSource)) |>
  filter(si >0) 

evdgt0 |>
  count(si)|>
  ggplot(aes(x=si, y=n))+
  geom_bar(stat = "identity")

shapiro.test(evdgt0$si)

(MedianSI=MedianCI(evdgt0$si))
(MeanSI = MeanCI(evdgt0$si))
(SDSI= sd(evdgt0$si))


evdgt <-
  est.GT(serial.interval = evdgt0$si)

evdgt <-
  generation.time("gamma",
                c(MeanSI[[1]],SDSI))



unique(sort(evdll.ConfProb$Date_debut_symptomes))

min(evdll.ConfProb$Date_debut_symptomes)+55

evdincid0 =
  incidence(sort(evdll.ConfProb$Date_debut_symptomes))

evdincid0$counts


evdincidG =
  incidence(sort(evdll.ConfProb$Date_debut_symptomes), groups = evdll.ConfProb$TransOpport)


Rt=
  estimate_R(incid = evdincid0[23:72,1],
           method = "non_parametric_si",
           config = make_config(list(si_distr=evdgt$GT)))

Rtp=
  estimate_R(incid = evdincid0$counts[23:72,1],
             method = "parametric_si",
             config = make_config(list(mean_si=MeanSI[[1]],
                                       std_si=SDSI)))
RtpG=
  estimate_R(incid = evdincid0,
           method = "parametric_si",
           config = make_config(list(mean_si=MeanSI[[1]],
                                     std_si=SDSI)))


RtHH=
  estimate_R(incid = evdincidG$counts[46:72,1],
           method = "parametric_si",
           config = make_config(list(mean_si=MeanSI[[1]],
                                     std_si=SDSI)))

RtHF=
  estimate_R(incid = evdincidG$counts[46:72,2],
           method = "parametric_si",
           config = make_config(list(mean_si=MeanSI[[1]],
                                     std_si=SDSI)))


Rt$R

RtHH$R

RtHF$R

#R0Rt=
  estimate.R(epid = evdincid0$counts[,1],
           GT=evdgt,
           methods = c("EG", "ML", "TD", "AR", "SB"),
           pop.size=100000,
           begin = 1,
           end = 21)
  
  
tdR=
  est.R0.TD(epid = evdincid0$counts[34:72,1],
          GT=evdgt)

tdRs = smooth.Rt(tdR, 6)

tdR$R
tdRs$R
tdRs$conf.int



tdRHH=
  est.R0.TD(epid = evdincid0$counts[46:72,1],
            GT=evdgt,
            time.step = 1)

tdRHF=
  est.R0.TD(epid = evdincidG$counts[46:72,2],
            GT=evdgt,
            time.step = 1)

smooth.Rt(tdR, 7)
tdR$R

tdRHH$R
tdRHF$R

R0EstEG=
  est.R0.EG(epid = evdincid0$counts[,1],
            GT=evdgt,
            begin=23,end = 60,
            date.first.obs = as.Date(min(evdll.ConfProb$Date_debut_symptomes)))

est.R0.EG(epid = evdincid0$counts[,1],
          GT=evdgt,
          begin=1,end = 55,
          date.first.obs = as.Date(min(evdll.ConfProb$Date_debut_symptomes)))

est.R0.EG(epid = evdincidG$counts[,2],
          GT=evdgt,
          begin=23,end = 60,
          date.first.obs = as.Date(min(evdll.ConfProb$Date_debut_symptomes)))



R0EstML =
  est.R0.ML(epid = evdincid0$counts[23:60,1],
            GT=evdgt)

R0EstML0 =
  est.R0.ML(epid = evdincid0$counts,#[23:60,1],
          begin=1,end = 55,
          GT=evdgt)

R0EstML0

evdincid0$dates[55]


R0EstSB = 
  est.R0.SB(epid = evdincid0$counts[23:60,1],
          GT=evdgt)

#### Estimating the peack and other incidence related parameters #####

evdincid01 =
  incidence(sort(evdll.ConfProb$Date_debut_symptomes), interval = "1 week")

evdincidG1 =
  incidence(sort(evdll.ConfProb$Date_debut_symptomes), 
            groups = evdll.ConfProb$TransOpport, interval = "1 week")


evdfit0 <-
  fit(evdincid0, split = as.Date("2025-09-11"))


evdrb= 
  paste("r = ",round(evdfit0$before$info$r,3))

evdrbci =
  paste("[",
      round(evdfit0$before$info$r.conf[[1]],3)," - ",
      round(evdfit0$before$info$r.conf[[2]],3),"]")

evddoubl=paste("Doubling = ",
                 round(evdfit0$before$info$doubling,1),
                 " Jours")


evdfit0$before$info$doubling.conf[[1]]
evdfit0$before$info$doubling.conf[[2]]



evdra= 
  paste("r = ",round(evdfit0$after$info$r,3))


evdraci= 
  paste("[",
        round(evdfit0$after$info$r.conf[[1]],3)," - ",
        round(evdfit0$after$info$r.conf[[2]],3),"]")


evdhlv= paste("Halving = ",
                 round(evdfit0$after$info$halving,1),
                 " Jours")

evdfit0$after$info$halving.conf

evdbefore=
  paste(evdrb," ",evdrbci,"\n",evddoubl)

evdafter= 
  paste(evdra," ",evdraci,"\n",evdhlv)

###

evdcumulCases <-
  evdll.ConfProb |> ungroup() |> 
  count(Date_debut_symptomes, name = "NouveauxCas") |>
  full_join(data.frame(Date_debut_symptomes=seq(min(evdll.ConfProb$Date_debut_symptomes),
                                                max(evdll.ConfProb$Date_debut_symptomes)+21, by="1 day"),
                       CasCumul=0),
            by="Date_debut_symptomes") |>
  arrange(Date_debut_symptomes) |>
  mutate(NouveauxCas=ifelse(is.na(NouveauxCas),0,NouveauxCas),
         CasCumul=cumsum(NouveauxCas),
         CasCumul=ifelse(CasCumul==1,CasCumul+0.01,CasCumul))

log(evdcumulCases$CasCumul)


RtDf= 
  RtpG$R |> 
  as.data.frame()|> 
  mutate(Dates=RtpG$dates[8:72]) |> 
  select(Dates,Rt=`Mean(R)`,Rtlci=`Quantile.0.05(R)`,Rtuci=`Quantile.0.95(R)`) |>
  mutate(RtCat=case_when(Rtlci<1&Rtuci>1~"Evolution Incertain",
                         Rtuci<1~"Contrôle",
                         Rtlci>1~"Pas de contrôle",
                         .default = NA))


cumRange=range(0.95, max(evdcumulCases$CasCumul))
RtRange=range(0,max(RtDf$Rtuci, na.rm = T))

RangeCoef=diff(cumRange)/diff(RtRange)



peack= evdcumulCases[which.max(evdcumulCases$NouveauxCas),]

evdfill=c("Cas Cumulatif"=adjustcolor("#1e3e95", alpha.f = 0.5))
evdcol= c("Rt"=adjustcolor("#7c0808",alpha.f = 0.8))

RiDf= data.frame(Ri=R0EstML0$R,
                 RiLci=R0EstML0$conf.int[1],
                 RiUci=R0EstML0$conf.int[2],
                 D1=evdincid0$dates[1],
                 D2=evdincid0$dates[55])


evdEpiDynP <-
  evdcumulCases |>
  ggplot()+
  geom_bar(aes(x=Date_debut_symptomes,
               y=CasCumul, fill = "Cas Cumulatif"),
           stat = "identity")+
  geom_segment(data=peack,
               aes(x=Date_debut_symptomes,
                   y= 1 ,yend = CasCumul-1),
               colour="#060846",
               lineend = "butt",
               linewidth=1,
               arrow = arrow(length = unit(0.1,"cm")))+
  geom_text(data = peack,
            aes(x=Date_debut_symptomes,
                y= CasCumul,
                label = paste0("Pic observé : ",
                               format(Date_debut_symptomes,"%d-%m-%Y"),
                               "\n",
                               NouveauxCas,
                               " nouveaux cas")),
            vjust = -0.25,
            hjust = 1,
            fontface = "bold",
            size = 3)+
  geom_hline(yintercept = ((1-RtRange[1])*RangeCoef)+cumRange[1],
             linetype=5,
             color="#6c1414")+
  geom_ribbon(data = RtDf,
              aes(x=Dates,
                  ymin = ((Rtlci-RtRange[1])*RangeCoef)+cumRange[1],
                  ymax = ((Rtuci-RtRange[1])*RangeCoef)+cumRange[1]),
              fill = adjustcolor("#d78ba4", alpha.f = 0.5))+
  geom_line(data=RtDf,
            aes(x=Dates,
                y=((Rt-RtRange[1])*RangeCoef)+cumRange[1],
                color="Rt"),
            size=1)+
  geom_segment(data = RiDf,
               aes(x=D1,xend = D2,
                   y= ((Ri-RtRange[1])*RangeCoef)+cumRange[1]),
               colour= adjustcolor("#9e2d2d", alpha.f = 0.15),
               lineend = "round", size=1)+
  geom_text(data = RiDf,
            aes(x=D1,y= ((Ri-RtRange[1])*RangeCoef)+cumRange[1],
                label = paste0("Ri = ",
                               round(Ri,3),
                               " [",
                               round(RiLci,3)," - ",
                               round(RiUci,3),"]")),
            colour="#2a0a06",
            vjust=-0.5, hjust=-0.025,fontface = "bold",
            size = 3.5)+
  annotate("text",label = evdbefore,
                x = peack$Date_debut_symptomes-3,
                y = peack$CasCumul-10,
                size = 3,hjust=0.91,
                fontface = "bold",
                color = "#030e2a")+
  annotate("text",label = evdafter,
           x = peack$Date_debut_symptomes,
           y = peack$CasCumul-10,
           size = 3,hjust=-0.075,
           fontface = "bold",
           color = "#030e2a")+
  scale_y_continuous(transform = "log",
                     breaks = c(0,1,2,4,8,16,32,64),
                     expand = c(0.01,0),
                     sec.axis = sec_axis(~((.-cumRange[1])/RangeCoef)+RtRange[1],
                                         name = "Nombre de reproduction effectif (Rt)",
                                         breaks = c(0,0.2,0.5,1,2,4,8,16),
                                         labels = scales::label_number(scale = 1)))+
  scale_x_date(expand = c(0.01,0),
               date_breaks = "7 days",
               date_labels = paste0("%d-%m","\n","%G"))+
  scale_fill_manual(values = evdfill)+
  scale_color_manual(values = evdcol)+
  labs(title = "MVE Kasai: Evolution du nombre cumulatif de cas et de reproduction effectif (Rt)",
       subtitle = paste0("RDC - Kasai ",
                         format(maxNotif, "%d-%m-%Y")),
       caption = 
         paste0("Ri: Nombre de reproduction initial estimé par la méthode de maximum de vraisemblance\n",
                "Rt: Nombre de reproduction effectif estimé par la méthode paramétrique de Cori et al. (2013)\n",
                "r: Taux de croissance avant et après le pic\n",
                "Doubling: Temps de doublement avant le pic\n",
                "Halving: Temps de réduction de moitié après le peack\n",
                "Graphique basée sur les données disponible au ",
                format(maxNotif, "%d-%m-%Y"),"\nCAI COUSP"),
       x="Dates de début des symptômes/signes",
       y="Nombre cumulatif de cas (échelle logarithmique)",
       fill="",
       color="")+
  theme(panel.background = element_rect(fill = "white",
                                         colour = "black",
                                         size = 0.5),
         panel.grid.major = element_line(colour = "grey90",
                                         linetype = 2),
         axis.text = element_text(face = "bold"),
         axis.line = element_line(size = 0.5),
         axis.title = element_text(face = "bold"),
        axis.line.y.right = element_line(color = "#7c0808", size = 0.5),
        axis.text.y.right = element_text(color = "#7c0808", face = "bold"),
        axis.title.y.right = element_text(color = "#7c0808", face = "bold"),
         legend.position = "bottom",
         legend.key.size = unit(0.45, "cm"),
         legend.text = element_text(size = 8),
         legend.title = element_text(size = 9),
         strip.background = element_rect(fill = "white",
                                         colour = "grey95"),
         strip.text = element_text(face = "bold",
                                   size = 12.5),
         plot.title = element_text(face = "bold",
                                   size = 14,
                                   color = "#234a7d")) 




evdEpiDynP |>
  ggsave(filename=paste0(svfldrE,
                         "EpiDynamicsEVD_",
                         maxNotif,".png"),
         dpi = 400, height = 20, width = 30, scale = 0.4)



####

evdEpiParam =
  data.frame(Paramètres=c("Intervalle de serie","Nombre de reproduction initial"),
             Estimations=c(round(MeanSI[[1]]),
                        round(R0Rt$estimates$ML$R,1)),
             `IC95%`=c(paste0("[",round(MeanSI[[2]]),",",
                              round(MeanSI[[3]]),"]"),
                       `CI95%`=paste0("[",round(R0Rt$estimates$ML$conf.int[[1]],1),",",
                                      round(R0Rt$estimates$ML$conf.int[[2]],1),"]")))


evdEpiParam |> flextable()|>
  theme_booktabs(bold_header = T) |>
  theme_vanilla()|>
  set_table_properties(layout = "autofit", width = 1) |>
  autofit()

##### Infector-infected #####

evdtransparII <-
  evdtranspar0 |>
  count(AgeSource,AgeCase) |>
  group_by(AgeCase)|>
  summarise(AgeSource=AgeSource,
            n=n,
            nProp=round(n/sum(n, na.rm = T),1))

(evdtransparIIG <-
   evdtransparII |>
   ggplot(aes(x=AgeCase,
              y=AgeSource,
              fill = nProp))+
   geom_tile(color="#ea8f98")+
    geom_text(aes(label = n, fontface = "bold"), size = 3)+
   scale_fill_gradient(low = "#f0b6bc",
                       high = "#990614")+
   labs(title = "Intéractions cas sources-cas secondaire de la MVE par Age",
        subtitle = paste0("RDC ",
                          format(maxNotif, "%d-%m-%Y")),
        caption = 
          paste0("Graphique basée sur les données disponible au ",
                 format(maxNotif, "%d-%m-%Y"),"\nCAI COUSP"),
        x="Age Cas sécondaires",
        y="Age cas sources",
        fill="Proportion interaction")+
   theme(panel.background = element_rect(fill = "white",
                                         colour = "black",
                                         size = 0.5),
         panel.grid.major = element_line(colour = "white",
                                         linetype = 2),
         axis.text = element_text(face = "bold"),
         axis.line = element_line(size = 0.5),
         axis.title = element_text(face = "bold"),
         legend.position = "bottom",
         legend.key.size = unit(0.45, "cm"),
         legend.text = element_text(size = 8),
         legend.title = element_text(size = 9),
         strip.background = element_rect(fill = "white",
                                         colour = "grey95"),
         strip.text = element_text(face = "bold",
                                   size = 12.5),
         plot.title = element_text(face = "bold",
                                   size = 14,
                                   color = "#234a7d"),
         plot.caption = element_text(face = "italic")))


evdtransparIIG |>
  ggsave(filename=paste0(svfldrE,
                         "SourceCaseAgeEVD_",
                         maxNotif,".png"),
         dpi = 400, height = 20, width = 25, scale = 0.4)


table(evdtranspar0$AgeSource,
      evdtranspar0$AgeCase)

table(evdtranspar0$SexeSource,
      evdtranspar0$SexeCase) |>
  fisher.test()

BinomCI(x=c(2,8),
        n=c(15,13))

#

evdtransparIIs <-
  evdtranspar0 |>
  mutate(SexeSource=case_when(SexeSource=="Féminin"~"Cas source féminin",
                              SexeSource=="Masculin"~"Cas source masculin",
                              .default = SexeSource))|>
  count(SexeSource,AgeSource,AgeCase) |>
  group_by(AgeCase)|>
  summarise(SexeSource=SexeSource,
            AgeSource=AgeSource,
            n=n,
            nProp=round(n/sum(n, na.rm = T),1)) |>
  filter(!is.na(SexeSource)) 

(evdtransparIIsG <-
    evdtransparIIs |>
    ggplot(aes(x=AgeCase,
               y=AgeSource,
               fill = nProp))+
    geom_tile(color="#ea8f98")+
    geom_text(aes(label = n, fontface = "bold"), size = 3)+
    scale_fill_gradient(low = "#f0b6bc",
                        high = "#990614")+
    labs(title = "Intéractions cas sources-cas secondaire de la MVE (Sexe-Age)",
         subtitle = paste0("RDC ",
                           format(maxNotif, "%d-%m-%Y")),
         caption = 
           paste0("Graphique basée sur les données disponible au ",
                  format(maxNotif, "%d-%m-%Y"),"\nCAI COUSP"),
         x="Age Cas sécondaires",
         y="Age cas sources",
         fill="Proportion interaction")+
    facet_wrap(~SexeSource)+
    theme(panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          size = 0.5),
          panel.grid.major = element_line(colour = "white",
                                          linetype = 2),
          axis.text = element_text(face = "bold"),
          axis.line = element_line(size = 0.5),
          axis.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.key.size = unit(0.45, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          strip.background = element_rect(fill = "grey95",
                                          colour = "grey90"),
          strip.text = element_text(face = "bold",
                                    size = 10.5),
          plot.title = element_text(face = "bold",
                                    size = 14,
                                    color = "#234a7d"),
          plot.caption = element_text(face = "italic")))


evdtransparIIsG |>
  ggsave(filename=paste0(svfldrE,
                         "SourceCaseAgeSexEVD_",
                         maxNotif,".png"),
         dpi = 400, height = 16, width = 35, scale = 0.4)




#####
