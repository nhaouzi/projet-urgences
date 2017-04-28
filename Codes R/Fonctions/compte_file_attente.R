# compte_file_attente = function(TS_adm, TS_med,  evt) {
#   tab = data.table(adm = TS_adm,
#                    med = TS_med)
#   tmps_date_adm = difftime(evt, tab$adm, units = "secs") 
#   tmps_med_date = difftime(tab$med, evt, units = "secs")
#   en_attente = tab[tmps_date_adm > 0 & tmps_med_date > 0]
#   return(nb_attente = nrow(en_attente))
# }

compte_file_attente_par_iao = function(TS_adm, TS_med,  evt, tri_iao) {
  
  if(class(TS_adm)[1] != "POSIXct") {
    TS_adm = as.POSIXct(TS_adm, tz="Europe/Paris", format="%Y-%m-%d %H:%M:%S")
  }
  if(class(TS_med)[1] != "POSIXct") {
    TS_med = as.POSIXct(TS_med, tz="Europe/Paris", format="%Y-%m-%d %H:%M:%S")
  }
  
  tab = data.table(adm = TS_adm,
                   med = TS_med, 
                   iao = tri_iao,
                   diff_evt_adm = difftime(evt, TS_adm, units = "secs"),
                   diff_med_evt =difftime(TS_med, evt, units = "secs"))
  
  #tmps_date_adm = difftime(evt, tab$adm, units = "secs") 
  #tmps_med_date = difftime(tab$med, evt, units = "secs")
  arrive_avant = tab[diff_evt_adm > 0]
  en_attente = arrive_avant[diff_med_evt > 0]
  
  #print(dim(en_attente))
  
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
  #print(dim(fenetre))
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


