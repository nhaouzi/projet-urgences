### Statistiques descriptives

setwd("Desktop/ENSAE_3A/S2/Projet info/Git/projet-urgences/")
library(data.table)
library(ggplot2)


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

mean(DT[nom_hopital=="Hôpital Necker" & annee_adm == 2013|2014 & tri_iao==3, Attente_totale])

DT = DT[annee_adm == 2013|2014]
DT[,.(mean = mean(Attente_totale)), by=nom_hopital]

