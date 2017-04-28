### Statistiques descriptives

setwd("Desktop/ENSAE_3A/S2/Projet info/Git/projet-urgences/")
library(data.table)
library(ggplot2)
library(questionr)


DT = fread("Bases/tab2013_2014.csv")
DT = DT[nom_hopital == "Hôpital Lariboisière"]
DT2013 = DT[annee_adm == 2013]
DT2014 = DT[annee_adm == 2014]

counts2013 = DT2013[,.(mean = mean(Attente_totale)), by=tri_iao]
counts2013[,tri_iao:=as.character(tri_iao)]
counts2013[tri_iao==3, tri_iao:="3-Consultation"]
counts2013[tri_iao==2, tri_iao:="2-Urgence relative"]
counts2013[tri_iao==1, tri_iao:="1-Urgence absolue"]

counts2014 = DT2014[,.(mean = mean(Attente_totale)), by=tri_iao]
counts2014[,tri_iao:=as.character(tri_iao)]
counts2014[tri_iao==3, tri_iao:="3-Consultation"]
counts2014[tri_iao==2, tri_iao:="2-Urgence relative"]
counts2014[tri_iao==1, tri_iao:="1-Urgence absolue"]

counts = rbind(counts2013, counts2014)
counts[,Année:=c(rep("2013", 3), rep("2014",3))]


# theme(plot.title = element_text(size=14, face="bold.italic"),
#   axis.title.x = element_text(color="blue", size=14, face="bold"),
#   axis.title.y = element_text(color="#993333", size=14, face="bold")


p<-ggplot(data=counts, aes(x=tri_iao, y=mean, fill=Année)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Reds") +
  #ggtitle("Attente moyenne par classe IAO") +
  xlab("Classe de tri IAO") +
  ylab("Attente moyenne (mins)")
p


hist(DT2013$Attente_totale, freq = F, col="lightblue", 
     breaks = 60, main="",
     xlab = "Temps d'attente" )
lines(density(rexp(n = nrow(DT2013), rate = 1/mean(DT2013$Attente_totale))), 
      col="red", lty=1)

hist(DT2014$Attente_totale, freq = F, col="blue1", 
     breaks = 60, main="",
     xlab = "Temps d'attente" )
lines(density(rexp(n = nrow(DT2014), rate = 1/mean(DT2014$Attente_totale))), 
      col="red", lty=1)

# ------------------------------
# hist par classe iao

hist(DT[tri_iao==1,Attente_totale], freq = F, col="lightblue", 
     breaks = 60, main="", ylab = "",
     xlab = "Temps d'attente des urgences vitales" )
lines(density(rexp(n = nrow(DT), rate = 1/mean(DT[tri_iao==1,Attente_totale]))), 
      col="red", lty=1)

hist(DT[tri_iao==2,Attente_totale], freq = F, col="purple", 
     breaks = 60, main="", ylab="",
     xlab = "Temps d'attente des urgences relatives" )
lines(density(rexp(n = nrow(DT), rate = 1/mean(DT[tri_iao==2,Attente_totale]))), 
      col="red", lty=1)

hist(DT[tri_iao==3,Attente_totale], freq = F, col="pink", 
     breaks = 60, main="", ylab="",
     xlab = "Temps d'attente des consultations" )
lines(density(rexp(n = nrow(DT), rate = 1/mean(DT[tri_iao==3,Attente_totale]))), 
      col="red", lty=1)


mean(DT[nom_hopital=="Hôpital Necker" & annee_adm == 2013|2014 & tri_iao==3, Attente_totale])

DT = DT[annee_adm == 2013|2014]
DT[,.(mean = mean(Attente_totale)), by=nom_hopital]

#----------
counts = DT[,.(mean = mean(Attente_totale)), by=jour_semaine_adm]

to_ord = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
to_ord=c(5,1,2,6,3,4,7)
setroworder <- function(x, neworder) {
  .Call(data.table:::Creorder, x, as.integer(neworder), PACKAGE = "data.table")
  invisible(x)
}
setroworder(x=counts, neworder=to_ord)
p<-ggplot(data=counts, aes(x=jour_semaine_adm, y=mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Reds") +
  #ggtitle("Attente moyenne par classe IAO") +
  xlab("Jours de la semaine") +
  ylab("Attente moyenne (mins)")
p


#-----
counts = DT[,.(mean = mean(Attente_totale)), by=heure_adm]
plot(x = counts$heure_adm, y=counts$mean, type="l", col="blue",
     lty=1, xlab= "Heures", ylab="Moyenne de temps d'attente")
abline(h=mean(DT$Attente_totale), col="red")
legend("topleft", legend = c("Attente moyenne par heure", "Moyenne des temps d'attente"),
        col = c("blue", "red"), fill = c("blue", 'red'))

counts = DT[,.(mean = mean(Attente_totale)), by=nb_pers_attente]
counts1 = DT[,.(mean = mean(Attente_totale)), by=nb_pers_attente_iao1]
counts2 = DT[,.(mean = mean(Attente_totale)), by=nb_pers_attente_iao2]
counts3 = DT[,.(mean = mean(Attente_totale)), by=nb_pers_attente_iao3]

p<-ggplot(data=counts1, aes(x=nb_pers_attente_iao1, y=mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Reds") +
  #ggtitle("Attente moyenne par classe IAO") +
  xlab("Nombre de personnes en attente - Urgence absolue") +
  ylab("Attente moyenne (mins)")
p

p<-ggplot(data=counts2, aes(x=nb_pers_attente_iao2, y=mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Reds") +
  #ggtitle("Attente moyenne par classe IAO") +
  xlab("Nombre de personnes en attente - Urgence absolue") +
  ylab("Attente moyenne (mins)")
p

p<-ggplot(data=counts3, aes(x=nb_pers_attente_iao3, y=mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Reds") +
  #ggtitle("Attente moyenne par classe IAO") +
  xlab("Nombre de personnes en attente - Urgence absolue") +
  ylab("Attente moyenne (mins)")
p

source('~/Desktop/rapport temp No/Fonctions/lien_xy.R', echo=TRUE)

lien_xy(x=as.factor(DT$nb_pers_attente), y=DT$Attente_totale, 
        nomx = "Nombre total de personnes en attente",
        nomy = "Attente moyenne (mins)")
lien_xy(x=as.factor(DT$nb_pers_attente_iao1), y=DT$Attente_totale,
        nomx = "Nombre de personnes en attente en urgence absolue",
        nomy = "Attente moyenne (mins)")

#irec(DT, nb_pers_attente_iao2)
## Recodage de DT$nb_pers_attente_iao2 en DT$nb_pers_attente_iao2_rec
DT$nb_pers_attente_iao2_rec <- as.character(DT$nb_pers_attente_iao2)
DT$nb_pers_attente_iao2_rec[DT$nb_pers_attente_iao2 == "7"] <- "7 et +"
DT$nb_pers_attente_iao2_rec[DT$nb_pers_attente_iao2 == "8"] <- "7 et +"
DT$nb_pers_attente_iao2_rec[DT$nb_pers_attente_iao2 == "9"] <- "7 et +"
DT$nb_pers_attente_iao2_rec[DT$nb_pers_attente_iao2 == "10"] <- "7 et +"
DT$nb_pers_attente_iao2_rec[DT$nb_pers_attente_iao2 == "11"] <- "7 et +"
DT$nb_pers_attente_iao2_rec <- factor(DT$nb_pers_attente_iao2_rec)

lien_xy(x=as.factor(DT$nb_pers_attente_iao2_rec), y=DT$Attente_totale, 
        nomx = "Nombre de personnes en attente pour une urgence relative",
        nomy = "Attente moyenne (mins)", 
        size = 24)

#irec(DT, nb_pers_attente_iao3)
## Recodage de DT$nb_pers_attente_iao3 en DT$nb_pers_attente_iao3_rec
DT$nb_pers_attente_iao3_rec <- as.character(DT$nb_pers_attente_iao3)
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "10"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "11"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "12"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "13"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "14"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "15"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "16"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "17"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "18"] <- "10 et +"
DT$nb_pers_attente_iao3_rec[DT$nb_pers_attente_iao3 == "19"] <- "10 et +"
DT$nb_pers_attente_iao3_rec <- factor(DT$nb_pers_attente_iao3_rec)

DT$nb_pers_attente_iao3_rec <- factor(DT$nb_pers_attente_iao3_rec, 
                                      levels = DT$nb_pers_attente_iao3_rec[order(DT$Attente_totale)])

lien_xy(x=as.factor(DT$nb_pers_attente_iao3_rec), y=DT$Attente_totale, 
        nomx = "Nombre de personnes en attente de consultation",
        nomy = "Attente moyenne (mins)", limits=c(as.character(0:9),"10 et +"))




lien_xy(x=as.factor(DT$heure_adm), y=DT$Attente_totale, 
        nomx = "Heure d'admission aux urgences",
        nomy = "Attente moyenne (mins)")
