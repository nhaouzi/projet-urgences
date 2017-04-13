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


moyenne_file_attente_par_iao = function(TS_adm, TS_med,  evt, tri_iao, fenetre_nb_heure) {
  # trier la base par date de ts.adm peut etre plus rapide
  tab = data.table(adm = TS_adm,
                   med = TS_med, 
                   iao = tri_iao, tps_attente = difftime(TS_med, TS_adm, units = "mins"))
  #tmps_date_adm = difftime(evt, tab$adm, units = "min") 
  tmps_med_date = difftime(tab$med, evt, units = "hours")
  fenetre = tab[tmps_med_date > -fenetre_nb_heure & tmps_med_date < 0]
  #print(fenetre)
  if(nrow(fenetre)!=0) {
    attente_iao1 = mean(fenetre[iao ==1]$tps_attente)
    attente_iao2 = mean(fenetre[iao ==2]$tps_attente)
    attente_iao3 = mean(fenetre[iao ==3]$tps_attente)
    
  }
  else {
    attente_iao1 <- NA
    attente_iao2 <- NA
    attente_iao3 <- NA
  }
  return(list(nb_attente = mean(fenetre$tps_attente),
              attente_iao1 = attente_iao1,
              attente_iao2 = attente_iao2,
              attente_iao3 = attente_iao3))
}

## exemple
# moyenne_file_attente_par_iao(DT$TS.adm, DT$TS.med,  DT$TS.adm[80], DT$tri.iao, 4)


trouve_jour_fete = function(mois_fete, jour_fete, mois, jour) {
  tab = data.table(mois = as.integer(mois),
                   jour = as.integer(jour),
                   date_char = paste0(jour,mois))
  date_a_comparer = paste0(jour_fete,mois_fete)
  tab[,jour_fete := ifelse(test = date_char %in% date_a_comparer, yes = 1, no = 0)]
  return(tab$jour_fete)
}

