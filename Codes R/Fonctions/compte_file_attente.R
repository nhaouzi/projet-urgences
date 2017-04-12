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


# tmp_iao = tmp[as.numeric(TS.iao) > as.numeric(evt)]
# tmp_med = tmp[as.numeric(TS.med) > as.numeric(evt)]
