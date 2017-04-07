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

# probleme de NA à corriger

# table(DT[is.na(D.total), annee])
# sum(is.na(APR2013$Date.et.heure.entrŽe))
# sum(is.na(APR2013$Timestamp.1er.mŽdecin))
# sum(is.na(APR2013$Tri.IAO))
# sum(is.na(APR2013$TimeStamp.IAO))
# sum(is.na(APR2013$TimeStamp.Fin.de.prise.en.charge.SAU))
# sum(is.na(APR2013$TimeStamp.DATE.DE.SORTIE..SAU....UHCD.))

DT[,TS.med := TS.adm + minutes(D.total)]

DT[,nb_pers_attente := sapply(DT$TS.adm, function(x) compte_file_attente(TS_adm = DT$TS.adm, 
                                                                         TS_med = DT$TS.med,
                                                                         evt = x))]
fwrite(DT, "Bases/base_tot_finale.csv",
       quote = F, row.names = F, append = F, sep=";")
