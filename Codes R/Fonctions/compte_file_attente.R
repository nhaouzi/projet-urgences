compte_file_attente = function(TS_adm, TS_med,  evt) {
  tab = data.table(adm = TS_adm,
                   med = TS_med)
  tmps_date_adm = difftime(evt, tab$adm, units = "secs") 
  tmps_med_date = difftime(tab$med, evt, units = "secs")
  en_attente = tab[tmps_date_adm > 0 & tmps_med_date > 0]
  return(nb_attente = nrow(en_attente))
}

compte_file_attente_par_iao = function(TS_adm, TS_med,  evt, tri_iao) {
  # trier la base par date de ts.adm peut etre plus rapide
  tab = data.table(adm = TS_adm,
                   med = TS_med, 
                   iao = tri_iao)
  tmps_date_adm = difftime(evt, tab$adm, units = "secs") 
  tmps_med_date = difftime(tab$med, evt, units = "secs")
  en_attente = tab[tmps_date_adm > 0 & tmps_med_date > 0]
  print(dim(en_attente))
  if(nrow(en_attente)!=0) {
    attente_iao1 = nrow(en_attente[iao ==1])
    attente_iao2 = nrow(en_attente[iao ==2])
    attente_iao3 = nrow(en_attente[iao ==3])
    
  }
  else {
    attente_iao1 <- 0
    attente_iao2 <- 0
    attente_iao3 <- 0
  }
  return(list(nb_attente = nrow(en_attente),
              attente_iao1 = attente_iao1,
              attente_iao2 = attente_iao2,
              attente_iao3 = attente_iao3))
}


# moyenne_file_attente_par_iao = function(TS_adm, TS_med,  evt, 
#                                         tri_iao, fenetre_nb_heure) {
#   # trier la base par date de ts.adm peut etre plus rapide
#   TS_adm = as.POSIXct(TS_adm, tz="Europe/Paris", format="%Y/%m/%d %H:%M")
#   TS_med = as.POSIXct(TS_med, tz="Europe/Paris", format="%Y/%m/%d %H:%M")
#   evt = as.POSIXct(evt, tz="Europe/Paris", format="%Y/%m/%d %H:%M")
#   tab = data.table(adm = TS_adm,
#                    med = TS_med, 
#                    iao = tri_iao, 
#                    tps_attente = difftime(TS_med, TS_adm, units = "mins"))
#   #tmps_date_adm = difftime(evt, tab$adm, units = "min") 
#   tmps_med_date = difftime(tab$med, evt, units = "hours")
#   fenetre = tab[tmps_med_date > -fenetre_nb_heure & tmps_med_date < 0]
#   print(dim(fenetre))
#   if(nrow(fenetre)!=0) {
#     attente_iao1 = mean(fenetre[iao ==1]$tps_attente)
#     attente_iao2 = mean(fenetre[iao ==2]$tps_attente)
#     attente_iao3 = mean(fenetre[iao ==3]$tps_attente)
#     
#   }
#   else {
#     attente_iao1 <- NA
#     attente_iao2 <- NA
#     attente_iao3 <- NA
#   }
#   return(list(nb_attente = mean(fenetre$tps_attente),
#               attente_iao1 = attente_iao1,
#               attente_iao2 = attente_iao2,
#               attente_iao3 = attente_iao3))
# }


moyenne_file_attente_par_iao = function(TS_adm, TS_med,   
                                        tri_iao, nb_heure) {
  # trier la base par date de ts.adm peut etre plus rapide
  if(class(TS_adm)[1] != "POSIXct") {
    TS_adm = as.POSIXct(TS_adm, tz="Europe/Paris", format="%Y-%m-%d %H:%M:%S")
  }
  if(class(TS_med)[1] != "POSIXct") {
    TS_med = as.POSIXct(TS_med, tz="Europe/Paris", format="%Y-%m-%d %H:%M:%S")
  }
  # if(class(evt)[1] != "POSIXct") {
  #   evt = as.POSIXct(evt, tz="Europe/Paris", format="%Y/%m/%d %H:%M")
  # }
  tab = data.table(adm = TS_adm,
                   med = TS_med, 
                   iao = tri_iao, 
                   tps_attente = difftime(TS_med, TS_adm, units = "mins"))
  
  tab = tab[order(tab$adm, decreasing = F)]
  
  
  
  moyenne = sapply(tab$adm, function(x) 
    calcule_moyenne_evt(evt=x, tab))
  
  attente_tot = apply(moyenne, 2, function(x) x$attente_tot)
  attente_iao1 = apply(moyenne, 2, function(x) x$attente_iao1)
  attente_iao2 = apply(moyenne, 2, function(x) x$attente_iao2)
  attente_iao3 = apply(moyenne, 2, function(x) x$attente_iao3)
  
  return(list(attente_tot = attente_tot,
              attente_iao1 = attente_iao1,
              attente_iao2 = attente_iao2,
              attente_iao3 = attente_iao3))
}

calcule_moyenne_evt = function(evt, tab) {
  
  # admis avant evt => tmps_evt_adm >0
  tab$tmps_evt_adm = difftime(evt, tab$adm, units = "secs")
  
  # admis apres evt - nb_heure => tmps_adm_evt_fenetre >0
  tab$tmps_adm_evt_fenetre = difftime(tab$adm, 
                                      evt - hours(nb_heure), 
                                      units = "secs")
  
  # vu le medecin avant evt (sinon on a pas le tmps d'attente) => tmps_evt_med > 0
  tab$tmps_evt_med = difftime(evt, tab$med, units = "secs")
  
  n = which(tab$adm == evt)
  if(length(n)!=0) {
    sous_tab = tab[1:n]
    if(nrow(sous_tab) > 1000) {
      sous_tab = sous_tab[(n-400):n,]
    }
  }
  else sous_tab=tab
  
  fenetre = sous_tab[tmps_evt_adm >0 & tmps_adm_evt_fenetre > 0 & tmps_evt_med > 0]
  print(dim(fenetre))
  if(nrow(fenetre) ==0) print(evt)
  
  if(nrow(fenetre)!=0) {
    attente_iao1 = as.numeric(mean(fenetre[iao ==1,tps_attente]))
    attente_iao2 = as.numeric(mean(fenetre[iao ==2,tps_attente]))
    attente_iao3 = as.numeric(mean(fenetre[iao ==3,tps_attente]))
    attente_tot = as.numeric(mean(fenetre$tps_attente))
  }
  else {
    attente_iao1 <- NA
    attente_iao2 <- NA
    attente_iao3 <- NA
    attente_tot <- NA
  }
  return(list(attente_iao1 = attente_iao1,
              attente_iao2 = attente_iao2,
              attente_iao3 = attente_iao3,
              attente_tot = attente_tot))
}


retourne_moyenne_attente = function(hopital, nb_heure) {
  moyenne = sapply(hopital$TS_adm, function(x)
    moyenne_file_attente_par_iao(TS_adm = hopital$TS_adm,
                                 TS_med = hopital$TS_med,  
                                 evt = x,
                                 tri_iao = hopital$tri_iao, 
                                 nb_heure = nb_heure))
  
  moyenne_iao1 = apply(moyenne, 2, function(x) x$attente_iao1)
  moyenne_iao2 = apply(moyenne, 2, function(x) x$attente_iao2)
  moyenne_iao3 = apply(moyenne, 2, function(x) x$attente_iao3)
  moyenne_tot = apply(moyenne, 2, function(x) x$nb_attente)
  
  return(list(moyenne_iao1 = moyenne_iao1,
              moyenne_iao2 = moyenne_iao2,
              moyenne_iao3 = moyenne_iao3,
              moyenne_tot = moyenne_tot))
}










## exemple
# moyenne_file_attente_par_iao(hopital1$TS_adm, hopital1$TS_med,  hopital1$TS_adm[80], 
#                              hopital1$tri_iao, 4)


trouve_jour_fete = function(mois_fete, jour_fete, mois, jour) {
  tab = data.table(mois = as.integer(mois),
                   jour = as.integer(jour),
                   date_char = paste0(jour,mois))
  date_a_comparer = paste0(jour_fete,mois_fete)
  tab[,jour_fete := ifelse(test = date_char %in% date_a_comparer, yes = 1, no = 0)]
  return(tab$jour_fete)
}

## exemple


simule_observations_2017 = function(prob_joursemaine, prob_iao,
                               prob_mois, prob_annee,
                               prob_jourmois, prob_heure,
                               prob_minute, 
                               moyenne_hopital,
                               nb_hopital,
                               jour_fete, mois_fete,
                               nom_hopital) {
 
  jours = c("Dimanche", "Jeudi", "Lundi", "Mardi", "Mercredi", "Samedi", "Vendredi")
  hopital = data.table(tri.iao = sample(1:3, size=nb_hopital, replace = T, 
                                         prob = prob_iao),
                        D.total = round(rexp(nb_hopital, 1/moyenne_hopital),0),
                        jour_semaine = sample(jours, size = nb_hopital, 
                                              replace = T, prob = prob_joursemaine),
                        mois = sample(1:12, size=nb_hopital, replace = T,
                                      prob = prob_mois),
                        annee = rep(2017, nb_hopital),
                        jour_mois = sample(1:31, size=nb_hopital, replace = T,
                                           prob=prob_jourmois),
                        heure_adm = sample(0:23, size=nb_hopital, replace = T,
                                           prob_heure),
                        min_adm = sample(0:59, size=nb_hopital, replace = T,
                                         prob = prob_minute))
  
  hopital = as.data.frame(hopital)
  for(j in c(4,6,7,8)) {
    hopital[,j] = as.character(hopital[,j])
    for(i in 1:nrow(hopital)) {
      if(nchar(hopital[i,j])<2) hopital[i,j] = paste0("0", hopital[i,j]) 
    }
    print(j)
  }
  hopital = as.data.table(hopital)
  
  hopital[,adm := paste0(hopital$annee, "-", hopital$mois, "-", hopital$jour_mois, " ",
                          hopital$heure_adm, ":", hopital$min_adm, ":00")]
  
  hopital[,TS.adm := strptime(adm, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris")]
  hopital[,TS.med := TS.adm + minutes(D.total)]
  hopital = na.omit(hopital, cols = c("TS.adm", "TS.med"))
  hopital = hopital[order(hopital$TS.adm, decreasing = F)]
  hopital[,adm:=NULL]
  
  
  attente = sapply(hopital$TS.adm, function(x) 
    compte_file_attente_par_iao(TS_adm = hopital$TS.adm, 
                                TS_med = hopital$TS.med,
                                evt = x, tri_iao = hopital$tri.iao))
  
  attente_iao1 = apply(attente, 2, function(x) x$attente_iao1)
  attente_iao2 = apply(attente, 2, function(x) x$attente_iao2)
  attente_iao3 = apply(attente, 2, function(x) x$attente_iao3)
  attente_tot = apply(attente, 2, function(x) x$nb_attente)
  
  hopital[,nb_pers_attente := attente_tot]
  hopital[,attente_iao1 := attente_iao1]
  hopital[,attente_iao2 := attente_iao2]
  hopital[,attente_iao3 := attente_iao3]
  
  hopital[,hopital := nom_hopital]
  
  moyenne_4 = sapply(hopital$TS.adm, function(x) 
    moyenne_file_attente_par_iao(hopital$TS.adm, hopital$TS.med,  
                                 x, 
                                 hopital$tri.iao, 4))
  
  moyenne_2 = sapply(hopital$TS.adm, function(x) 
    moyenne_file_attente_par_iao(hopital$TS.adm, hopital$TS.med,  
                                 x, 
                                 hopital$tri.iao, 2))
  
  moyenne_1 = sapply(hopital$TS.adm, function(x) 
    moyenne_file_attente_par_iao(hopital$TS.adm, hopital$TS.med,  
                                 x, 
                                 hopital$tri.iao, 1))
  
  
  moyenne_iao1 = apply(moyenne, 2, function(x) x$attente_iao1)
  moyenne_iao2 = apply(moyenne, 2, function(x) x$attente_iao2)
  moyenne_iao3 = apply(moyenne, 2, function(x) x$attente_iao3)
  moyenne_tot = apply(moyenne, 2, function(x) x$nb_attente)
  
  hopital[,attente_moyenne := moyenne_tot]
  hopital[,moyenne_iao1 := moyenne_iao1]
  hopital[,moyenne_iao2 := moyenne_iao2]
  hopital[,moyenne_iao3 := moyenne_iao3]
  
  for (col in c("moyenne_iao1", "moyenne_iao2", "moyenne_iao3", "attente_moyenne")) {
    hopital[is.na(get(col)), (col) := -999]
  }
  
  # hopital[,jour_fetes := trouve_jour_fete(jour_fete, mois_fete,
  #                                          mois = hopital$mois,
  #                                          jour = hopital$jour_mois)]

  return(hopital)
  
}

