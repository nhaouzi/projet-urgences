setwd("Desktop/ENSAE_3A/S2/Projet info/Git/projet-urgences/")
source("Codes R/Fonctions/compte_file_attente.R")

library(data.table)
library(lubridate)

tab2013_2014 = fread("Bases/tab2013_2014.csv")

tab2013_2014 = tab2013_2014[Attente_totale<=600]

tab2013_2014[,c("nb_pers_attente", "nb_pers_attente_iao1", 
                "nb_pers_attente_iao2",
                "nb_pers_attente_iao3", "TS_med",       
                "attente_moyenne_4h", "moyenne_iao1_4h", "moyenne_iao2_4h", 
                "moyenne_iao3_4h",     
                "attente_moyenne_2h", "moyenne_iao1_2h",  "moyenne_iao2_2h", 
                "moyenne_iao3_2h") :=NULL]

tab2013_2014[,Attente_totale := as.numeric(Attente_totale)]
tmp = tab2013_2014[id_hopital!=750100042 & tri_iao==3]
tab2013_2014[id_hopital!=750100042 & tri_iao == 3, Attente_totale := Attente_totale + rexp(20,n = nrow(tmp))]

to_char = c("jour_mois", "mois_adm", "heure_adm", "min_adm")
tab2013_2014[,jour_mois := as.character(jour_mois)]
tab2013_2014[,mois_adm := as.character(mois_adm)]
tab2013_2014[,heure_adm := as.character(mois_adm)]
tab2013_2014[,min_adm := as.character(min_adm)]

tab2013_2014 = as.data.frame(tab2013_2014)
for(col in to_char) {
  tab2013_2014[,col] = lapply(tab2013_2014[,col], function(x) {
    if(nchar(x) < 2) x = paste0("0",x)
  })
}
tab2013_2014 = as.data.table(tab2013_2014)

tab2013_2014[,TS_adm := paste0(annee_adm, "-", mois_adm, "-", jour_mois, " ", heure_adm, ":",
                               min_adm, ":00")]

tab2013_2014[,TS_adm := strptime(TS_adm, format = "%Y-%m-%d %H:%M:%S", 
                                 tz = "Europe/Paris")]

tab2013_2014[,TS_med := TS_adm + minutes(as.integer(Attente_totale))]


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

hopitaux_choisis[etablissement=="Hôpital Hôtel-Dieu"]

#------------------------------------------
jour_fete = c(24, 25, 14, 31, 1)
mois_fete = c(12, 12, 7, 12, 1)

DT = fread("Git/projet-urgences/Bases/base_tot_finale.csv")

prob_joursemaine = as.numeric(round(table(DT$jour_semaine)/nrow(DT),3))
prob_iao = as.numeric(round(table(DT$tri.iao)/nrow(DT),3))
prob_mois = as.numeric(round(table(DT$mois)/nrow(DT),3))
prob_annee = as.numeric(round(table(DT$annee)/nrow(DT),3))
prob_jourmois = as.numeric(round(table(DT$jour_mois)/nrow(DT),3))
prob_heure = as.numeric(round(table(DT$heure_adm)/nrow(DT),3))
prob_minute = as.numeric(round(table(DT$min_adm)/nrow(DT), 3))
prob_annee = c(0.4,0.4,0.2)

moyenne_iao1 = round(mean(DT[tri.iao==1, D.total]))
moyenne_iao2 = round(mean(DT[tri.iao==2, D.total]))
moyenne_iao3 = round(mean(DT[tri.iao==3, D.total]))

moyenne_iao1 = moyenne_iao1 -1
moyenne_iao2 = moyenne_iao2 +4
moyenne_iao3 = moyenne_iao3 +15


hopital6 = simule_observations(prob_joursemaine = prob_joursemaine, 
                               prob_iao = prob_iao, 
                               prob_mois = prob_mois,
                               prob_annee = prob_annee, 
                               prob_heure = prob_heure, 
                               prob_minute = prob_minute, 
                               prob_jourmois = prob_jourmois,
                               moyenne_hopital_iao1 = moyenne_iao1,
                               moyenne_hopital_iao2 = moyenne_iao2, 
                               moyenne_hopital_iao3 = moyenne_iao3, 
                               nb_hopital = 15000, 
                               jour_fete = jour_fete, 
                               mois_fete = mois_fete, 
                               nom_hopital = "Hôpital Hôtel-Dieu",
                               id_hopital = "750100018"
                                )
