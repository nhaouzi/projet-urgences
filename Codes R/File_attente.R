#APR2013 <- read.csv2('C:/Users/Augustin/Desktop/R/APR_2013n.csv', fileEncoding="CP1252")
APR2013 = read.csv2("APR_2013n.csv")
head(APR2013)
apr <- APR2013[,-1]
apr <- apr[,-6]
apr <- apr[,-5]
head(apr)
summary(apr$Tri.IAO)
levels(apr$Tri.IAO)[5] <- "0"
summary(apr$Tri.IAO)
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
head(apr)

#apr$T.adm <- format(apr$TS.adm, format = "%d/%m/%Y %H:%M", usetz = FALSE)
#apr$T.iao <- format(apr$TS.iao, format = "%d/%m/%Y %H:%M", usetz = FALSE)
#apr$T.med <- format(apr$TS.med, format = "%d/%m/%Y %H:%M", usetz = FALSE)
head(apr)
#apr <- apr[,c(1,2,3,5,6,4,7)]
head(apr)

apr$D.pre.iao <- difftime(apr$TS.iao, apr$TS.adm, units = "mins")
apr$D.post.iao <- difftime(apr$TS.med, apr$TS.iao, units = "mins")
apr$D.total <- difftime(apr$TS.med, apr$TS.adm, units = "mins")
head(apr)

A = matrix(c(mean(apr$D.total[apr$tri.iao == 1], na.rm=T), mean(apr$D.total[apr$tri.iao == 2], na.rm=T), mean(apr$D.total[apr$tri.iao == 3], na.rm=T)), nrow=1, ncol=3,byrow = TRUE)
colnames(A) = c('Absolue', 'Relative', 'Consultation')
barplot(A, main="Temps d'attente total",
        xlab="Urgence")

A = matrix(c(mean(apr$D.pre.iao[apr$tri.iao == 1], na.rm=T), mean(apr$D.pre.iao[apr$tri.iao == 2], na.rm=T), mean(apr$D.pre.iao[apr$tri.iao == 3], na.rm=T)), nrow=1, ncol=3,byrow = TRUE)
colnames(A) = c('Absolue', 'Relative', 'Consultation')
barplot(A, main="Temps d'attente Pre IAO",
        xlab="Urgence")

A = matrix(c(mean(apr$D.post.iao[apr$tri.iao == 1], na.rm=T), mean(apr$D.post.iao[apr$tri.iao == 2], na.rm=T), mean(apr$D.post.iao[apr$tri.iao == 3], na.rm=T)), nrow=1, ncol=3,byrow = TRUE)
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

hist(rexp(1000,1/mean(apr$D.total[apr$D.total<500], na.rm=T)), col=rgb(1,0,0,0.5), breaks = 60, freq=F)
hist(apr$D.total[apr$D.total<500], breaks= 60,  col=rgb(0,0,1,0.5), freq=F, add=T)
box()

hist(apr$D.total[apr$D.total<100], breaks= 50,  col=rgb(0,0,1,0.5), freq=T)

savehistory("~/Desktop/Projet/R/apr.Rhistory")
load("~/Desktop/Projet/R/apr.RData")
