simule_observations = function(prob_joursemaine, 
                               prob_iao,
                               prob_mois, 
                               prob_annee,
                               prob_jourmois, 
                               prob_heure,
                               prob_minute, 
                               moyenne_hopital_iao1,
                               moyenne_hopital_iao2,
                               moyenne_hopital_iao3,
                               nb_hopital,
                               jour_fete, mois_fete,
                               nom_hopital,
                               id_hopital) {
  
  jours = c("Dimanche", "Jeudi", "Lundi", "Mardi", "Mercredi", "Samedi", "Vendredi")
  hopital = data.table(tri_iao = sample(1:3, size=nb_hopital, replace = T, 
                                        prob = prob_iao),
                       jour_semaine = sample(jours, size = nb_hopital, 
                                             replace = T, prob = prob_joursemaine),
                       mois = sample(1:12, size=nb_hopital, replace = T,
                                     prob = prob_mois),
                       annee = sample(c(2013,2014,2017), size=nb_hopital,
                                      prob = prob_annee, replace = T),
                       jour_mois = sample(1:31, size=nb_hopital, replace = T,
                                          prob=prob_jourmois),
                       heure_adm = sample(0:23, size=nb_hopital, replace = T,
                                          prob_heure),
                       min_adm = sample(0:59, size=nb_hopital, replace = T,
                                        prob = prob_minute))
  
  hopital[,mois:=as.character(mois)]
  hopital[nchar(mois)==1, mois:= paste0("0", mois)]
  
  hopital[,jour_mois:=as.character(jour_mois)]
  hopital[nchar(jour_mois)==1, jour_mois:= paste0("0", jour_mois)]
  
  hopital[,heure_adm:=as.character(mois)]
  hopital[nchar(heure_adm)==1, heure_adm:= paste0("0", mois)]
  
  hopital[,min_adm:=as.character(mois)]
  hopital[nchar(min_adm)==1, min_adm:= paste0("0", mois)]
  
  adm = paste0(hopital$annee, "-", hopital$mois, "-", hopital$jour_mois, " ",
                         hopital$heure_adm, ":", hopital$min_adm, ":00")
  hopital[,TS_adm := strptime(adm, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris")]
  
  n1 = nrow(hopital[tri_iao==1])
  hopital[tri_iao == 1, Attente_totale := rexp(n = n1, rate = 1/moyenne_hopital_iao1)]
  
  n2 = nrow(hopital[tri_iao==2])
  hopital[tri_iao == 2, Attente_totale := rexp(n = n2, rate = 1/moyenne_hopital_iao2)]
  
  
  n3 = nrow(hopital[tri_iao==3])
  hopital[tri_iao == 3, Attente_totale := rexp(n = n3, rate = 1/moyenne_hopital_iao3)]
  
  hopital[,TS_med := TS_adm + minutes(round(Attente_totale))]
  hopital = na.omit(hopital, cols = c("TS_adm", "TS_med"))
  
  hopital = hopital[order(hopital$TS_adm, decreasing = F)]
  
  print("compte file attente")
  attente = sapply(hopital$TS_adm, function(x) 
    compte_file_attente_par_iao(TS_adm = hopital$TS_adm, 
                                TS_med = hopital$TS_med,
                                evt = x, 
                                tri_iao = hopital$tri_iao))
  
  attente_iao1 = apply(attente, 2, function(x) x$attente_iao1)
  attente_iao2 = apply(attente, 2, function(x) x$attente_iao2)
  attente_iao3 = apply(attente, 2, function(x) x$attente_iao3)
  attente_tot = apply(attente, 2, function(x) x$nb_attente)
  
  hopital[,nb_pers_attente := attente_tot]
  hopital[,nb_pers_attente_iao1 := attente_iao1]
  hopital[,nb_pers_attente_iao2 := attente_iao2]
  hopital[,nb_pers_attente_iao3 := attente_iao3]
  
  print("moyenne file attente")
  moyenne_4 = moyenne_file_attente_par_iao(TS_adm = hopital$TS_adm, 
                                  TS_med = hopital$TS_med,  
                                  tri_iao = hopital$tri_iao, 
                                  nb_heure = 4)
  
  moyenne_iao1 = apply(moyenne_4, 2, function(x) x$attente_iao1)
  moyenne_iao2 = apply(moyenne_4, 2, function(x) x$attente_iao2)
  moyenne_iao3 = apply(moyenne_4, 2, function(x) x$attente_iao3)
  moyenne_tot = apply(moyenne_4, 2, function(x) x$nb_attente)
  
  hopital[,attente_moyenne_4h := moyenne_tot]
  hopital[,moyenne_iao1_4h := moyenne_iao1]
  hopital[,moyenne_iao2_4h := moyenne_iao2]
  hopital[,moyenne_iao3_4h := moyenne_iao3]
  
  moyenne_2 = moyenne_file_attente_par_iao(TS_adm = hopital$TS_adm, 
                                 TS_med = hopital$TS_med,  
                                 tri_iao = hopital$tri_iao, 
                                 nb_heure = 2)
  
  moyenne_iao1 = apply(moyenne_2, 2, function(x) x$attente_iao1)
  moyenne_iao2 = apply(moyenne_2, 2, function(x) x$attente_iao2)
  moyenne_iao3 = apply(moyenne_2, 2, function(x) x$attente_iao3)
  moyenne_tot = apply(moyenne_2, 2, function(x) x$nb_attente)
  
  hopital[,attente_moyenne_2h := moyenne_tot]
  hopital[,moyenne_iao1_2h := moyenne_iao1]
  hopital[,moyenne_iao2_2h := moyenne_iao2]
  hopital[,moyenne_iao3_2h := moyenne_iao3]
  
  for (col in c("moyenne_iao1", "moyenne_iao2", "moyenne_iao3", "attente_moyenne")) {
    hopital[is.na(get(col)), (col) := -999]
  }
  
  hopital[,jour_fetes := trouve_jour_fete(jour_fete, mois_fete,
                                          mois = hopital$mois,
                                          jour = hopital$jour_mois)]
  hopital[,nom_hopital := nom_hopital]
  hopital[,id_hopital := id_hopital]
  
  return(hopital)
  
}

