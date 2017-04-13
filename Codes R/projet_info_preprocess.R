APR2013 <- read.csv2('C:/Users/Augustin/Desktop/R/APR_2013n.csv', fileEncoding="CP1252")
APR2013 <- read.csv2('APR_2013n.csv', fileEncoding="CP1252")

head(APR2013)
apr <- APR2013[,-1]
apr <- apr[,-6]
apr <- apr[,-5]
levels(apr$Tri.IAO)[5] <- "0"
levels(apr$Tri.IAO)[3] <- "3"
levels(apr$Tri.IAO)[2] <- "2"
levels(apr$Tri.IAO)[1] <- "1"
summary(apr$Tri.IAO)
names(apr)[1] <- "TS.adm"
names(apr)[2] <- "TS.med"
names(apr)[4] <- "TS.iao"
names(apr)[3] <- "tri.iao"
apr<-apr[,c(1,4,2,3)]


apr$TS.adm <- strptime(apr$TS.adm, format = "%d/%m/%Y %H:%M", tz = "Europe/Paris")
apr$TS.med <- strptime(apr$TS.med, format = "%d/%m/%Y %H:%M", tz = "Europe/Paris")
apr$TS.iao <- strptime(apr$TS.iao, format = "%d/%m/%Y %H:%M", tz = "Europe/Paris")


apr$D.pre.iao <- difftime(apr$TS.iao, apr$TS.adm, units = "mins")
apr$D.post.iao <- difftime(apr$TS.med, apr$TS.iao, units = "mins")
apr$D.total <- difftime(apr$TS.med, apr$TS.adm, units = "mins")

# APR2014 <- read.csv2('C:/Users/Augustin/Desktop/R/APR_2014_RT.csv', 
#                      fileEncoding="CP1252",  sep = ',' )
APR2014 <- read.csv2('APR_2014_RT.csv', 
                     fileEncoding="CP1252",  sep = ',' )


levels(APR2014$Tri.IAO)[1] <- 1
levels(APR2014$Tri.IAO)[2] <- 2
levels(APR2014$Tri.IAO)[3] <- 3
levels(APR2014$Tri.IAO)[4] <- 3
levels(APR2014$Tri.IAO)[4] <- 3




A = matrix(c(mean(APR2014$différence..minutes.[APR2014$Tri.IAO == 1], na.rm=T), 
             mean(APR2014$différence..minutes.[APR2014$Tri.IAO == 2], na.rm=T), 
             mean(APR2014$différence..minutes.[APR2014$Tri.IAO == 3], na.rm=T),
             mean(APR2014$différence..minutes.[APR2014$Tri.IAO == 4], na.rm=T), 
             mean(APR2014$différence..minutes.[APR2014$Tri.IAO == 5], 
            na.rm=T)), nrow=1, ncol=5,byrow = TRUE)
colnames(A) = c('Absolue', 'Relative', 'Consultation', 'Consul 2', 'Consul 3')
barplot(A, main="Temps d'attente total 2014",
        xlab="Urgence")



A = matrix(c(mean(apr$D.total[apr$tri.iao == 1], na.rm=T), 
             mean(apr$D.total[apr$tri.iao == 2], na.rm=T), 
             mean(apr$D.total[apr$tri.iao == 3], na.rm=T)), 
           nrow=1, ncol=3,byrow = TRUE)
colnames(A) = c('Absolue', 'Relative', 'Consultation')
barplot(A, main="Temps d'attente total",
        xlab="Urgence")

A = matrix(c(mean(apr$D.pre.iao[apr$tri.iao == 1], na.rm=T), 
             mean(apr$D.pre.iao[apr$tri.iao == 2], na.rm=T), 
             mean(apr$D.pre.iao[apr$tri.iao == 3], na.rm=T)), 
           nrow=1, ncol=3,byrow = TRUE)
colnames(A) = c('Absolue', 'Relative', 'Consultation')
barplot(A, main="Temps d'attente Pre IAO",
        xlab="Urgence")

A = matrix(c(mean(apr$D.post.iao[apr$tri.iao == 1], na.rm=T), 
             mean(apr$D.post.iao[apr$tri.iao == 2], na.rm=T), 
             mean(apr$D.post.iao[apr$tri.iao == 3], na.rm=T)), 
           nrow=1, ncol=3,byrow = TRUE)
colnames(A) = c('Absolue', 'Relative', 'Consultation')
barplot(A, main="Temps d'attente Post IAO",
        xlab="Urgence")

A = table(apr$D.total)
barplot(A, main="Temps d'attente",
        xlab="Minutes")



A = table(apr$D.total)
barplot(A, main="Temps d'attente",
        xlab="Minutes")


apr$D.total <- gsub(",", "", apr$D.total)   # remove comma
apr$D.total <- as.numeric(apr$D.total)  

hist(rpois(100000,mean(apr$D.total, na.rm=T)), breaks = 200, freq=F)
hist(apr$D.total[apr$D.total<100], breaks= 200, freq=F, add=T)
box()

hist(h1, col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,200), 
     main="Overlapping Histogram", xlab="Variable")
hist(h2, col=rgb(0,0,1,0.5), add=T)
box()


head(APR2014)
colnames(APR2014)[2] <- 'TS.adm'
colnames(APR2014)[7] <- 'tri.iao'
colnames(APR2014)[6] <- 'D.total'
APR2014$TS.adm <- strptime(APR2014$TS.adm, format = "%m/%d/%Y %H:%M", 
                           tz = "Europe/Paris")
base_2014 <- APR2014[, c('TS.adm', 'tri.iao', 'D.total', '')]
base_2013 <- apr[, c('TS.adm', 'tri.iao', 'D.total')]
base_tot <- rbind(base_2013, base_2014)

write.csv(base_tot, 'C:/Users/Augustin/Desktop/R/base_tot.csv')


head(apr)
apr[, c('TS.adm', 'D.total')]


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

fwrite(DT, "Bases/base_tot_finale.csv",
   quote = F, row.names = F, append = F, sep=";", 
   dateTimeAs = "write.csv")


#--------------------------------------------------------
# construction base avec simulations

library(data.table)
library(lubridate)

setwd("Desktop/ENSAE_3A/S2/Projet info/Git/projet-urgences/")

DT = fread("Bases/base_tot_finale.csv")

DT[,hopital := rep("hopital1", nrow(DT))]

prob_joursemaine = as.numeric(round(table(DT$jour_semaine)/nrow(DT),3))
prob_iao = as.numeric(round(table(DT$tri.iao)/nrow(DT),3))
prob_mois = as.numeric(round(table(DT$mois)/nrow(DT),3))
prob_annee = as.numeric(round(table(DT$annee)/nrow(DT),3))
prob_jourmois = as.numeric(round(table(DT$jour_mois)/nrow(DT),3))
prob_heure = as.numeric(round(table(DT$heure_adm)/nrow(DT),3))
prob_minute = as.numeric(round(table(DT$min_adm)/nrow(DT), 3))

nb_hopital2 = 60000
jours = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
hopital2 = data.table(tri.iao = sample(1:3, size=nb_hopital2, replace = T, 
                                       prob = prob_iao),
                      D.total = round(rexp(nb_hopital2, 1/mean(DT$D.total)),0),
                      jour_semaine = sample(jours, size = nb_hopital2, 
                                            replace = T, prob = prob_joursemaine),
                      mois = sample(1:12, size=nb_hopital2, replace = T,
                                    prob = prob_mois),
                      annee = sample(2013:2014, size=nb_hopital2, replace = T,
                                     prob = prob_annee),
                      jour_mois = sample(1:31, size=nb_hopital2, replace = T,
                                         prob=prob_jourmois),
                      heure_adm = sample(0:23, size=nb_hopital2, replace = T,
                                         prob_heure),
                      min_adm = sample(0:59, size=nb_hopital2, replace = T,
                                       prob = prob_minute))

hopital2 = as.data.frame(hopital2)
for(j in c(4,6,7,8)) {
  hopital2[,j] = as.character(hopital2[,j])
  for(i in 1:nrow(hopital2)) {
    if(nchar(hopital2[i,j])<2) hopital2[i,j] = paste0("0", hopital2[i,j]) 
  }
  print(j)
}
hopital2 = as.data.table(hopital2)

hopital2[,adm := paste0(hopital2$annee, "-", hopital2$mois, "-", hopital2$jour_mois, " ",
           hopital2$heure_adm, ":", hopital2$min_adm, ":00")]

hopital2[,TS.adm := strptime(adm, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris")]
hopital2[,TS.med := TS.adm + minutes(D.total)]
hopital2 = na.omit(hopital2, cols = c("TS.adm", "TS.med"))
hopital2 = hopital2[order(hopital2$TS.adm, decreasing = F)]
hopital2[,adm:=NULL]

# hist(DT$D.total, freq = F, col="lightblue", breaks = 100, main="Temps attente total", 
#      xlab = "" )
# lines(density(rexp(n = nrow(DT), rate = 1/mean(DT$D.total))), col="red")

attente = sapply(hopital2$TS.adm, function(x) 
  compte_file_attente_par_iao(TS_adm = hopital2$TS.adm, 
                                                 TS_med = hopital2$TS.med,
                                                 evt = x, tri_iao = hopital2$tri.iao))

attente_iao1 = apply(attente, 2, function(x) x$attente_iao1)
attente_iao2 = apply(attente, 2, function(x) x$attente_iao2)
attente_iao3 = apply(attente, 2, function(x) x$attente_iao3)
attente_tot = apply(attente, 2, function(x) x$nb_attente)

hopital2[,nb_pers_attente := attente_tot]
hopital2[,attente_iao1 := attente_iao1]
hopital2[,attente_iao2 := attente_iao2]
hopital2[,attente_iao3 := attente_iao3]

hopital2[,hopital := "hopital2"]

fwrite(hopital2, file = "Bases/hopital2.csv",
       append = F, quote = F, row.names = F,
       sep=";", dateTimeAs = "write.csv")


