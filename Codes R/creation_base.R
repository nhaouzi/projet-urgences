setwd("/Users/noemiehaouzi/Desktop/ENSAE_3A/S2/Projet info")

library(data.table)
library(questionr)
library(lubridate)


#------------------------------------------------------------------
# Gestion base 2013


apr2013 = fread("APR_2013n.csv")
apr2013[,c("Site", "TimeStamp Fin de prise en charge SAU",
           "TimeStamp DATE DE SORTIE (SAU +/-UHCD)"):=NULL]
names(apr2013) = gsub("\xe9", "e", names(apr2013))
names(apr2013) = gsub("\x8e", "e", names(apr2013))



#irec(apr2013$`Tri IAO`)

## Recodage de apr2013[,"Tri IAO"] en apr2013[,"Tri IAO_rec"]
apr2013[,"Tri IAO_rec"] <- apr2013[,"Tri IAO"]
apr2013[,"Tri IAO_rec"][apr2013[,"Tri IAO"] == "(003) 3 - Consultation Urgence  <60mn"] <- "3"
apr2013[,"Tri IAO_rec"][apr2013[,"Tri IAO"] == "(002) 2 - Urgence relative"] <- "2"
apr2013[,"Tri IAO_rec"][apr2013[,"Tri IAO"] == "(001) 1 - Urgence absolue"] <- "1"
apr2013[,"Tri IAO_rec"][apr2013[,"Tri IAO"] == "0"] <- NA
apr2013[,"Tri IAO_rec"][apr2013[,"Tri IAO"] == "2"] <- NA
table(apr2013$`Tri IAO_rec`, exclude = F)

apr2013[,tri_IAO := `Tri IAO_rec`]
apr2013[, c("Tri IAO_rec", "Tri IAO") := NULL]

nb_na = sapply(apr2013, function(x) sum(is.na(x)))

setnames(apr2013, 1:3, c("TS.adm", "TS.med", "TS.iao"))

apr2013[,TS.adm := strptime(TS.adm, format = "%d/%m/%Y %H:%M", tz = "Europe/Paris")]
apr2013[,TS.med := strptime(TS.med, format = "%d/%m/%Y %H:%M", tz = "Europe/Paris")]
apr2013[,TS.iao := strptime(TS.iao, format = "%d/%m/%Y %H:%M", tz = "Europe/Paris")]
head(apr2013)

apr2013[,duree.pre.iao := difftime(TS.iao, TS.adm, units = "mins")]
apr2013[,duree.post.iao := difftime(TS.med, TS.iao, units = "mins")]
apr2013[,duree.total := difftime(TS.med, TS.adm, units = "mins")]

apr2013[,jour_semaine := weekdays(TS.adm)]
apr2013[,mois:=month(TS.adm)]
apr2013[,annee := year(TS.adm)]
apr2013[,jour_mois := day(TS.adm)]
apr2013[,heure_adm := hour(TS.adm)]

nb_na = sapply(apr2013, function(x) sum(is.na(x)))

apr2013[,nb_personnes_attentes_iao := NA]

date = apr2013$TS.adm[10]
tmp = apr2013[as.numeric(TS.adm) < as.numeric(date)]
tmp_iao = tmp[as.numeric(TS.iao) > as.numeric(date)]
nrow(tmp_iao)
tmp_med = tmp[as.numeric(TS.med) > as.numeric(date)]
nrow(tmp_med)

compte_pers_attente_iao_med = function(tab, date) {
  tmp = tab[as.numeric(TS.adm) < as.numeric(date)]
  tmp_iao = tmp[as.numeric(TS.iao) > as.numeric(date)]
  tmp_med = tmp[as.numeric(TS.med) > as.numeric(date)]
  return(list(tot = nrow(tmp_iao)+nrow(tmp_med), iao = nrow(tmp_iao),
              med = nrow(tmp_med)))
}
compte_pers_attente_iao_med(apr2013, date)

nb_personnes_attentes_iao = sapply(apr2013$TS.adm, 
                                   function(x) compte_pers_attente_iao_med(apr2013, x)$iao)
nb_personnes_attentes_med = sapply(apr2013$TS.adm, 
                                   function(x) compte_pers_attente_iao_med(apr2013, x)$med)

apr2013[,nb_personnes_attentes_iao := nb_personnes_attentes_iao]
apr2013[,nb_personnes_attentes_med := nb_personnes_attentes_med]

#-----------------------------------------------------------------------------
# Gestion base 2014 --- manque heure iao

apr2014 = fread("APR_2014_RT.csv")
apr2014[,c("Site", "2h avant premier m\xe9decin",
           "Format heure"):=NULL]
names(apr2014) = gsub("\xe9", "e", names(apr2014))
names(apr2014) = gsub("\x8e", "e", names(apr2014))

setnames(apr2014, c(1,2,4), c("TS.adm", "TS.med", "TS.iao"))

apr2014 = apr2014[]
apr2014[,duree.pre.iao := difftime(TS.iao, TS.adm, units = "mins")]
apr2014[,duree.post.iao := difftime(TS.med, TS.iao, units = "mins")]
apr2014[,duree.total := difftime(TS.med, TS.adm, units = "mins")]

apr2014[,jour_semaine := weekdays(TS.adm)]
apr2014[,mois:=month(TS.adm)]
apr2014[,annee := year(TS.adm)]
apr2014[,jour_mois := day(TS.adm)]
apr2014[,heure_adm := hour(TS.adm)]
