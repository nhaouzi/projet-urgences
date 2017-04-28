#----------------------------------------------------------------------------
# Ajout nombre personne file attente

library(data.table)
library(lubridate)

source("Codes R/Fonctions/compte_file_attente.R")

DT = fread("Bases/base_tot.csv")

DT[,TS.adm:=gsub(pattern = "-", replacement = "/", TS.adm)]
DT[,TS.adm:=strptime(DT$TS.adm, format = "%Y/%m/%d %H:%M:%S", tz = "Europe/Paris")]
class(DT$TS.adm)
head(DT)

DT[,jour_semaine := weekdays(TS.adm)]
DT[,mois:=month(TS.adm)]
DT[,annee := year(TS.adm)]
DT[,jour_mois := day(TS.adm)]
DT[,heure_adm := hour(TS.adm)]
DT[,min_adm := minute(TS.adm)]

DT[,TS.med := TS.adm + minutes(D.total)]


# nb_pers_attente = sapply(DT$TS.adm, function(x) compte_file_attente(TS_adm = DT$TS.adm, 
#                                                                     TS_med = DT$TS.med,
#                                                                     evt = x))
# DT[,nb_pers_attente := nb_pers_attente]
# DT[,c("TS.adm", "TS.med")] = DT[,lapply(.SD, as.character), .SDcols= c("TS.adm", "TS.med")]

DT = DT[D.total >=0 & D.total <=600,]
DT = DT[tri.iao != 0]

attente = sapply(DT$TS.adm, function(x) 
  compte_file_attente_par_iao(TS_adm = DT$TS.adm, 
                              TS_med = DT$TS.med,
                              evt = x, 
                              tri_iao = DT$tri.iao))

attente_iao1 = apply(attente, 2, function(x) x$attente_iao1)
attente_iao2 = apply(attente, 2, function(x) x$attente_iao2)
attente_iao3 = apply(attente, 2, function(x) x$attente_iao3)
attente_tot = apply(attente, 2, function(x) x$nb_attente)

DT[,nb_pers_attente := attente_tot]
DT[,attente_iao1 := attente_iao1]
DT[,attente_iao2 := attente_iao2]
DT[,attente_iao3 := attente_iao3]

moyenne = sapply(DT$TS.adm, function(x) moyenne_file_moyenne_par_iao(DT$TS.adm, 
                                       DT$TS.med,  
                                       x, 
                                       DT$tri.iao, 4))

moyenne_iao1 = apply(moyenne, 2, function(x) x$attente_iao1)
moyenne_iao2 = apply(moyenne, 2, function(x) x$attente_iao2)
moyenne_iao3 = apply(moyenne, 2, function(x) x$attente_iao3)
moyenne_tot = apply(moyenne, 2, function(x) x$nb_attente)

DT[,attente_moyenne := moyenne_tot]
DT[,moyenne_iao1 := moyenne_iao1]
DT[,moyenne_iao2 := moyenne_iao2]
DT[,moyenne_iao3 := moyenne_iao3]

for (col in c("moyenne_iao1", "moyenne_iao2", "moyenne_iao3", "attente_moyenne")) {
  DT[is.na(get(col)), (col) := -999]
}

jour_fete = c(24, 25, 14, 31, 1)
mois_fete = c(12, 12, 7, 12, 1)

DT[,jour_fetes := trouve_jour_fete(jour_fete, mois_fete, 
                                         mois = DT$mois, 
                                         jour = DT$jour_mois)]



fwrite(DT, "Bases/base_tot_finale.csv",
   quote = F, row.names = F, append = F, sep=";", 
   dateTimeAs = "write.csv")



#--------------------------------------------------------
# construction base avec simulations

library(data.table)
library(lubridate)

setwd("Desktop/ENSAE_3A/S2/Projet info/Git/projet-urgences/")

DT = fread("Bases/base_tot_finale.csv")

prob_joursemaine = as.numeric(round(table(DT$jour_semaine)/nrow(DT),3))
prob_iao = as.numeric(round(table(DT$tri.iao)/nrow(DT),3))
prob_mois = as.numeric(round(table(DT$mois)/nrow(DT),3))
prob_annee = as.numeric(round(table(DT$annee)/nrow(DT),3))
prob_jourmois = as.numeric(round(table(DT$jour_mois)/nrow(DT),3))
prob_heure = as.numeric(round(table(DT$heure_adm)/nrow(DT),3))
prob_minute = as.numeric(round(table(DT$min_adm)/nrow(DT), 3))



fwrite(hopital5, file = "Bases/hopital5.csv",
       append = F, quote = F, row.names = F,
       sep=";", dateTimeAs = "write.csv")




# ---------------------------------

hopital2 = fread("Bases/hopital2.csv")
jour_fete = c(24, 25, 14, 31, 1)
mois_fete = c(12, 12, 7, 12, 1)

hopital5[,jour_fetes := trouve_jour_fete(jour_fete = jour_fete, 
                                         mois_fete = mois_fete, 
                                         mois = hopital5$mois, 
                                         jour = hopital5$jour_mois)]

hopital2[,TS.adm := as.POSIXct(TS.adm, tz="Europe/Paris", format="%Y-%m-%d %H:%M:%S")]
hopital2[,TS.med := as.POSIXct(TS.med, tz="Europe/Paris", format="%Y-%m-%d %H:%M:%S")]

moyenne = sapply(hopital2$TS.adm, function(x) 
  moyenne_file_attente_par_iao(hopital2$TS.adm, hopital2$TS.med,  
                               x, 
                               hopital2$tri.iao, 4))

moyenne_iao1 = apply(moyenne, 2, function(x) x$attente_iao1)
moyenne_iao2 = apply(moyenne, 2, function(x) x$attente_iao2)
moyenne_iao3 = apply(moyenne, 2, function(x) x$attente_iao3)
moyenne_tot = apply(moyenne, 2, function(x) x$nb_attente)

hopital2[,attente_moyenne := moyenne_tot]
hopital2[,moyenne_iao1 := moyenne_iao1]
hopital2[,moyenne_iao2 := moyenne_iao2]
hopital2[,moyenne_iao3 := moyenne_iao3]

for (col in c("moyenne_iao1", "moyenne_iao2", "moyenne_iao3", "attente_moyenne")) {
  hopital2[is.na(get(col)), (col) := -999]
}


fwrite(hopital1, file = "Bases/hopital1.csv",
       append = F, quote = F, row.names = F,
       sep=";", dateTimeAs = "write.csv")



# ---------------------------------
# concatenation des 5 bases

hopital1 = fread("Bases/base_tot_finale.csv")
hopital2 = fread("Bases/hopital2.csv")
hopital3 = fread("Bases/hopital3.csv")
hopital4 = fread("Bases/hopital4.csv")
hopital5 = fread("Bases/hopital5.csv")

hopital1 = hopital1[,names(hopital2), with=F]

sum(names(hopital1)==names(hopital2))

hopital_tot = rbind(hopital1, hopital2, hopital3, hopital4, hopital5)


# ----------------------------------
# ajout observations 2017

jour_fete = c(24, 25, 14, 31, 1)
mois_fete = c(12, 12, 7, 12, 1)

DT = fread("Bases/base_tot_finale.csv")

prob_joursemaine = as.numeric(round(table(DT$jour_semaine)/nrow(DT),3))
prob_iao = as.numeric(round(table(DT$tri.iao)/nrow(DT),3))
prob_mois = as.numeric(round(table(DT$mois)/nrow(DT),3))
prob_annee = as.numeric(round(table(DT$annee)/nrow(DT),3))
prob_jourmois = as.numeric(round(table(DT$jour_mois)/nrow(DT),3))
prob_heure = as.numeric(round(table(DT$heure_adm)/nrow(DT),3))
prob_minute = as.numeric(round(table(DT$min_adm)/nrow(DT), 3))


moyenne_hopital2_2017 = mean(hopital2$D.total)+6
hop2_2017 = simule_observations_2017(prob_joursemaine, 
                                     prob_iao,
                                     prob_mois, 
                                     prob_annee,
                                     prob_jourmois, 
                                     prob_heure,
                                     prob_minute, 
                                     moyenne_hopital = moyenne_hopital2_2017,
                                     6000,
                                     jour_fete, 
                                     mois_fete,
                                     "hopital2")

hop2_2017[,jour_fetes := trouve_jour_fete(jour_fete = jour_fete, 
                                          mois_fete = mois_fete,
                                          mois = hop2_2017$mois,
                                          jour = hop2_2017$jour_mois)]

table(hop2_2017$jour_fetes)
fwrite(hop2_2017, file = "Bases/hopital2_2017.csv",
       append = F, quote = F, row.names = F,
       sep=";", dateTimeAs = "write.csv")


# -------------------------------------------------
# Concaténation des bases 2017 et 2013 2014

hop1_2017 = fread("Bases/hopital1_2017.csv")
hop2_2017 = fread("Bases/hopital2_2017.csv")
hop3_2017 = fread("Bases/hopital3_2017.csv")
hop4_2017 = fread("Bases/hopital4_2017.csv")
hop5_2017 = fread("Bases/hopital5_2017.csv")

hopital1 = fread("Bases/hopital1.csv")
hopital2 = fread("Bases/hopital2.csv")
hopital3 = fread("Bases/hopital3.csv")
hopital4 = fread("Bases/hopital4.csv")
hopital5 = fread("Bases/hopital5.csv")

hopital_tot = rbind(hopital1, hopital2, hopital3, hopital4, hopital5,
                    hop1_2017, hop2_2017, hop3_2017, hop4_2017, hop5_2017)

library(rjson)
library(dplyr)
library(RJSONIO)
library(jsonlite)

json1 = "/Users/noemiehaouzi/Desktop/ENSAE_3A/S2/Projet info/kelhosto-master/app/src/main/assets/json/idf_emergency_hours.json"
json2 = "/Users/noemiehaouzi/Desktop/ENSAE_3A/S2/Projet info/kelhosto-master/app/src/main/assets/json/idf_emergency.json"

json_data = jsonlite::fromJSON(json1, flatten = T)
json_data2 = jsonlite::fromJSON(json2, flatten=T)
json_data = as.data.table(json_data)
json_data2 = as.data.table(json_data2)

names(json_data)
names(json_data2)
json_data2$etablissement

noms_hopitaux_choisis = c("Hôpital Lariboisière",
                     "Hôpital de la Pitié Salpêtrière",
                     "Hôpital Necker",
                     "Hôpital Bichat - Claude Bernard",
                     "Hôpital Ambroise Paré",
                     "Hôpital Hôtel-Dieu",
                     "Hôpital Cochin",
                     "Hôpital Saint-Joseph",
                     "Hôpital Saint-Antoine",
                     "Hôpital Européen - Georges Pompidou")
hopitaux_choisis = json_data2[etablissement %in% noms_hopitaux_choisis, 
                              c("id", "etablissement"), with=F]
setnames(hopitaux_choisis, "id", "id_hopital")
hopitaux_choisis[,id_hopital := as.character(id_hopital)]

hopital_tot[,hopital:=gsub(pattern = "hopital1", 
                           replacement = hopitaux_choisis$id[1], x = hopital)]
hopital_tot[,hopital:=gsub(pattern = "hopital2", 
                           replacement = hopitaux_choisis$id[2], x = hopital)]
hopital_tot[,hopital:=gsub(pattern = "hopital3", 
                           replacement = hopitaux_choisis$id[3], x = hopital)]
hopital_tot[,hopital:=gsub(pattern = "hopital4", 
                           replacement = hopitaux_choisis$id[4], x = hopital)]
hopital_tot[,hopital:=gsub(pattern = "hopital5", 
                           replacement = hopitaux_choisis$id[5], x = hopital)]
setnames(hopital_tot, "hopital", "id_hopital")

hopital_tot = merge(hopital_tot, hopitaux_choisis, 
                    by = "id_hopital")

names(hopital_tot) <- gsub(pattern = "[.]", "_", names(hopital_tot))
setnames(hopital_tot, c("etablissement","D_total", "jour_semaine", "mois", "annee",
                        "attente_iao1", "attente_iao2", "attente_iao3",
                        "attente_moyenne", "moyenne_iao1", "moyenne_iao2",
                        "moyenne_iao3"),
         c("nom_hopital", "Attente_totale", "jour_semaine_adm", "mois_adm", "annee_adm",
           "nb_pers_attente_iao1", "nb_pers_attente_iao2", "nb_pers_attente_iao3",
           "attente_moyenne_4h", "moyenne_iao1_4h", "moyenne_iao2_4h",
           "moyenne_iao3_4h"))

fwrite(hopital_tot, file = "Bases/attente_hopitaux_2013_2014_2017.csv",
       append = F, quote = F, row.names = F,
       sep=";", dateTimeAs = "write.csv")


# -----------------------------------------------
# ajout moyenne attente pour 2h et 1h precedente

library(data.table)
library(lubridate)

hopital_tot = fread("Bases/attente_hopitaux_2013_2014_2017.csv")
table(hopital_tot$id_hopital)
hopital1 = hopital_tot[id_hopital == "750100042"]
hopital2 = hopital_tot[id_hopital == "750100125"]
hopital3 = hopital_tot[id_hopital == "750100208"]
hopital4 = hopital_tot[id_hopital == "750100232"]
hopital5 = hopital_tot[id_hopital == "920100013"]

hopital1 = hopital1[order(hopital1$TS_adm, decreasing = F)]
hopital2 = hopital2[order(hopital2$TS_adm, decreasing = F)]
hopital3 = hopital3[order(hopital3$TS_adm, decreasing = F)]
hopital4 = hopital4[order(hopital4$TS_adm, decreasing = F)]
hopital5 = hopital5[order(hopital5$TS_adm, decreasing = F)]


# test = hopital1[1:2000]
# moy = moyenne_file_attente_par_iao(TS_adm = test$TS_adm, TS_med = test$TS_med,
#                              tri_iao = test$tri_iao,
#                              nb_heure = 4)
# 
# tab = tab = data.table(adm = test$TS_adm[1:2000],
#                        med = test$TS_med[1:2000], 
#                        iao = test$tri_iao[1:2000], 
#                        tps_attente = difftime(test$TS_med[1:2000], 
#                                               test$TS_adm[1:2000], 
#                                               units = "mins"))
# 
# calcule_moyenne_evt(evt = test$TS_adm[1500], tab)

sum(is.na(hopital5$TS_adm))
sum(is.na(hopital5$TS_med))


nb_heure = 2
moy = moyenne_file_attente_par_iao(TS_adm = hopital5$TS_adm, 
                                   TS_med = hopital5$TS_med,
                                   tri_iao = hopital5$tri_iao,
                                   nb_heure = nb_heure)


hopital5[,attente_moyenne_2h := moy$attente_tot]
hopital5[,moyenne_iao1_2h := moy$attente_iao1]
hopital5[,moyenne_iao2_2h := moy$attente_iao2]
hopital5[,moyenne_iao3_2h := moy$attente_iao3]

cols = paste0(c("moyenne_iao1_", "moyenne_iao2_", "moyenne_iao3_", 
                "attente_moyenne_"),
              nb_heure, "h")

for (col in cols) {
  hopital5[is.na(get(col)), (col) := -999]
}

hopital1[,TS_adm:=gsub("/","-",TS_adm)]
hopital1[,TS_med:=gsub("/","-",TS_med)]
hopital1[,TS_adm:=paste0(TS_adm,":00")]
hopital1[,TS_med:=paste0(TS_med,":00")]

ordre = c(names(hopital1)[1:17], "moyenne_iao1_4h",
          "moyenne_iao2_4h", "moyenne_iao3_4h", "attente_moyenne_2h",  
          "moyenne_iao1_2h", "moyenne_iao2_2h", "moyenne_iao3_2h",
          "nom_hopital")
hopital1 = setcolorder(hopital1, ordre)
hopital2 = setcolorder(hopital2, ordre)
hopital3 = setcolorder(hopital3, ordre)
hopital4 = setcolorder(hopital4, ordre)
hopital5 = setcolorder(hopital5, ordre)
hopital_tot = rbind(hopital1, hopital2, hopital3, hopital4, hopital5)

cols = c(paste0(c("moyenne_iao1_", "moyenne_iao2_", "moyenne_iao3_", 
                "attente_moyenne_"), 2, "h"), 
         paste0(c("moyenne_iao1_", "moyenne_iao2_", "moyenne_iao3_", 
                                "attente_moyenne_"), 4, "h"))

for (col in cols) {
  hopital_tot[is.na(get(col)), (col) := -999]
}



fwrite(hopital_tot, file = "Bases/base_5hopitaux.csv",
       append = F, quote = F, row.names = F,
       sep=";", dateTimeAs = "write.csv")


############# Stats Desc 
library(data.table)
tab = fread("Bases/base_5hopitaux.csv")

tab2017 = tab[annee_adm == 2017]
tab2013_2014 = tab[annee_adm == 2013 | annee_adm == 2014]

fwrite(tab2013_2014, "Bases/tab2013_2014.csv")
fwrite(tab2017, "Bases/tab2017.csv")

tab2017[nchar(TS_adm)==22, TS_adm := substr(TS_adm, 1, 19)]
tab2017[nchar(TS_med)==22, TS_med := substr(TS_med, 1, 19)]


###########
tab2013_2014 = fread("Bases/tab2013_2014.csv")
tab2017 = fread("Bases/tab2017.csv")
tmp = tab2013_2014[id_hopital!=750100042 & tri_iao==3]
#tab2013_2014[,Attente_totale := as.numeric(Attente_totale)]
#tab2013_2014[id_hopital!=750100042 & tri_iao == 3, 
Attente_totale := Attente_totale + rexp(20,n = nrow(tmp))]

#fwrite(tab2013_2014, "Bases/tab2013_2014_25041350.csv")
#head(tab2013_2014$Attente_totale + rexp(20, n=nrow(tab2013_2014)))




