from sklearn import linear_model
from datetime import datetime
import pandas as pd
import numpy as np

# Permet de calculer la metrique MAPE
def mape(a, b):
    mask = a != 0
    return ((abs(a-b)/a)[mask].mean())

# Permet d'enlever les types IAO 1, et de faire un fitre sur l'hopital 750100042 qui possède les vraies données
# Ou bien d'utiliser les données simuler (où l'on rajoutera du bruit pour reproduire les différences entre IAO comme constaté 
# sur le vrai modèle 
def filtre(base, filtre_bool, moyenne = 20):
    base = base[base['tri_iao']!=1]
    if filtre_bool:     
        base = base[base['id_hopital'] == 750100042]
    else: 
        base['Attente_totale'][base['tri_iao'] == 3] = base['Attente_totale'][base['tri_iao'] == 3] +    np.random.exponential(moyenne,len(base[base['tri_iao'] == 3]))                                                                                                                   
    return (base)
    
    
# Compte le nombre de personne qui sont en train d'attendre un medecin
def compte_file_attente(dates_list_adm, dates_list_med, list_nom_hopital, evt, vect_iao, nom_hopital, FMT = '%Y-%m-%d %H:%M:%S'):
    vect_a_comparer = np.repeat(datetime.strptime(evt, FMT), len(dates_list_adm), axis=0)
    tmps_date_adm = pd.DataFrame(vect_a_comparer - dates_list_adm, columns=['diff'])
    tmps_date_med = pd.DataFrame(vect_a_comparer - dates_list_med, columns=['diff'])
    
    memehopital = list_nom_hopital == nom_hopital
    tmp1 = tmps_date_adm>0
    tmp1 = tmp1['diff']
    tmp2 = tmps_date_med<0
    tmp2 = tmp2['diff']
    tmp_iao_1 = vect_iao == 1
    tmp_iao_2 = vect_iao == 2
    tmp_iao_3 = vect_iao == 3
    
    indice_all = tmp1 & tmp2 & memehopital
    indice_iao_1 = indice_all & tmp_iao_1 & memehopital
    indice_iao_2 = indice_all & tmp_iao_2 & memehopital
    indice_iao_3 = indice_all & tmp_iao_3 & memehopital

    return([sum(indice_all.values),
    sum(indice_iao_1),
    sum(indice_iao_2),
    sum(indice_iao_3)])

# Calcule les moyennes d'attente des personnes dans le même hopital ces "fenetre_min" dernières minutes
def moyenne_file_attente_par_iao(dates_list_adm, dates_list_med, list_nom_hopital, evt, vect_iao, nom_hopital, fenetre_min = 120, FMT = '%Y-%m-%d %H:%M:%S'):
# trier la base par date de ts.adm peut etre plus rapide
    vect_a_comparer = np.repeat(datetime.strptime(evt, FMT), len(dates_list_adm), axis=0)
    tmps_date_med = vect_a_comparer - dates_list_med
    tmp_base = [date.days*24 + date.seconds/3600  for date in tmps_date_med]
    base_tps_attente = [a - b for a, b in zip(dates_list_med, dates_list_adm)]
    attente_min = [date.days*24*60 + date.seconds/60  for date in base_tps_attente]

    memehopital = list_nom_hopital == nom_hopital

    tmp1 = pd.DataFrame(tmp_base, columns= ['diff']) < fenetre_min 
    tmp2 = pd.DataFrame(tmp_base, columns= ['diff'])> 0
    tmp_iao_1 = vect_iao == 1
    tmp_iao_2 = vect_iao == 2
    tmp_iao_3 = vect_iao == 3
    
    indice_all = tmp1 & tmp2 
    indice_all = indice_all['diff'] & memehopital
    indice_iao_1 = indice_all & tmp_iao_1 & memehopital
    indice_iao_2 = indice_all & tmp_iao_2 & memehopital
    indice_iao_3 = indice_all & tmp_iao_3 & memehopital

    
    df_attente = pd.DataFrame(attente_min, columns=['temps_attente'])
    return(np.mean(df_attente[indice_all]), np.mean(df_attente[indice_iao_1])
          , np.mean(df_attente[indice_iao_2]), np.mean(df_attente[indice_iao_3])) 

# Permet de mettre les jours de la semaine en int
def Jour_semaine(jour_semaine):   
    
    if jour_semaine == 'Dimanche':
        result = 6
    elif jour_semaine == 'Lundi':
        result = 0
    elif jour_semaine == 'Mardi':
        result = 1
    elif jour_semaine == 'Mercredi':
        result = 2
    elif jour_semaine == 'Jeudi':
        result = 3
    elif jour_semaine == 'Vendredi':
        result = 4
    elif jour_semaine == 'Samedi':
        result = 5
    return result


# Permet de savoir si le jour consideré est un jour de fête ou non
def trouve_jour_fete(jour, mois):
    fete = 0
    if (mois == 1 and jour == 1):
        fete = 1
    if (mois == 7 and jour == 14):
        fete = 1
    if (mois == 12):
        if (jour == 24 or jour == 25 or jour == 31):
            fete = 1
    return (fete)


# Permet à partir d'un timestamp et d'une base de donnée temps reel, d'avoir notre df de prédictions
def Timestamp_to_df(timestamp, base, id_hopital, FMT = '%Y-%m-%d %H:%M:%S'):
    dates_list_adm = [datetime.strptime(date, FMT) for date in base['TS_adm']]
    dates_list_med = [datetime.strptime(date, FMT) for date in base['TS_med']]
    list_nom_hopital = base['id_hopital']
   
    mois = int(float(datetime.strptime(timestamp, FMT).strftime('%m')))
    jour_mois = int(float(datetime.strptime(timestamp, FMT).strftime('%d')))
    heure = int(float(datetime.strptime(timestamp, FMT).strftime('%H')))
    jour_semaine = int(float(datetime.strptime(timestamp, FMT).strftime('%w')))
    
    jour_fetes = trouve_jour_fete(jour_mois, mois)

    variables = [jour_semaine, mois, jour_mois, heure, jour_fetes]
    
    nom_colonnes = ['tri_iao', 'jour_semaine_adm', 'mois_adm', 'jour_mois', 'heure_adm', 'jour_fetes', 'nb_pers_attente', 'nb_pers_attente_iao1', 
                    'nb_pers_attente_iao2', 'nb_pers_attente_iao3', 'attente_moyenne_4h', 'moyenne_iao1_4h',
                    'moyenne_iao2_4h', 'moyenne_iao3_4h', 'attente_moyenne_2h',
                    'moyenne_iao1_2h', 'moyenne_iao2_2h', 'moyenne_iao3_2h', 'id_hopital']
 
    
    input0 = pd.DataFrame({'A' : []})
    
    for i in id_hopital:  
        
        comptage = compte_file_attente(dates_list_adm, dates_list_med, list_nom_hopital, timestamp, base['tri_iao'], i )
        moyenne_2h = moyenne_file_attente_par_iao(dates_list_adm, dates_list_med, list_nom_hopital, timestamp, base['tri_iao'], i, fenetre_min=120)
        moyenne_4h = moyenne_file_attente_par_iao(dates_list_adm, dates_list_med, list_nom_hopital, timestamp, base['tri_iao'], i, fenetre_min=240)


        attente_moyenne_2h = moyenne_2h[0][0]
        moyenne_iao1_2h = moyenne_2h[1][0]
        moyenne_iao2_2h = moyenne_2h[2][0]
        moyenne_iao3_2h = moyenne_2h[3][0]

        attente_moyenne_4h = moyenne_4h[0][0]
        moyenne_iao1_4h = moyenne_4h[1][0]
        moyenne_iao2_4h = moyenne_4h[2][0]
        moyenne_iao3_4h = moyenne_4h[3][0]

        nb_attente_all = comptage[0]
        nb_attente_iao1 = comptage[1]
        nb_attente_iao2 = comptage[2]
        nb_attente_iao3 = comptage[3]

        var_dynamique = [nb_attente_all, nb_attente_iao1, nb_attente_iao2,
        nb_attente_iao3, attente_moyenne_4h, moyenne_iao1_4h,
        moyenne_iao2_4h, moyenne_iao3_4h, attente_moyenne_2h,
        moyenne_iao1_2h, moyenne_iao2_2h, moyenne_iao3_2h]
    
        input0 = input0.append(pd.DataFrame([[2] + variables + var_dynamique + [i]], 
                 columns=nom_colonnes))

        input_tmp = pd.DataFrame([[3] + variables + var_dynamique + [i]], 
                 columns=nom_colonnes)

        input0 = input0.append(input_tmp)
    input0 = input0.drop(['A'], axis = 1)
   
    return(input0)

