compte_file_attente = function(TS_adm, TS_med,  evt) {
  tab = data.table(adm = TS_adm,
                   med = TS_med)
  tmps_date_adm = difftime(evt, tab$adm, units = "secs") 
  tmps_med_date = difftime(tab$med, evt, units = "secs")
  en_attente = tab[tmps_date_adm > 0 & tmps_med_date > 0]
  return(nb_attente = nrow(en_attente))
}

# tmp_iao = tmp[as.numeric(TS.iao) > as.numeric(evt)]
# tmp_med = tmp[as.numeric(TS.med) > as.numeric(evt)]
