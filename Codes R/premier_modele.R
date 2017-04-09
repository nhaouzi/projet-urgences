setwd("Desktop/ENSAE_3A/S2/Projet info/Git/projet-urgences/")

library(data.table)
library(Matrix)
library(xgboost)
library(MLmetrics)

DT = fread("Bases/base_tot_finale.csv")
nb_na = sapply(DT, function(x) sum(is.na(x)))

"nb_pers_attente" %in% colnames(stest)

DT = na.omit(DT, cols= "D.total")

DT_modele = DT[,c("TS.adm", "TS.med") :=NULL]
to_factor = c("mois", "annee", "jour_mois")
DT_modele[,(to_factor) := lapply(.SD, function(x) as.factor(x)), .SDcols = to_factor]

sDT_modele = sparse.model.matrix(D.total ~ . -1 , data=DT_modele)
sDT_modele = model.matrix(D.total ~ . -1 , data=DT_modele)

set.seed(123)
ind_train = sample(1:3, 
                   prob = c(0.7,0.2,0.1), 
                   replace = T, 
                   size = nrow(sDT_modele))

strain = sDT_modele[ind_train==1,]
sval = sDT_modele[ind_train==2,]
stest = sDT_modele[ind_train==3,]

y_train = DT_modele[ind_train==1, D.total]
y_val = DT_modele[ind_train==2, D.total]
y_test = DT_modele[ind_train==3, D.total]

dtrain = xgb.DMatrix(strain, label=y_train)
dval = xgb.DMatrix(sval, label=y_val)
dtest = xgb.DMatrix(stest)

params = list(eta=0.01, 
              max_depth = 10, 
              colsample_bytree = 0.85,
              subsample = 0.85,
              eval_metric = "mae")
xgb = xgb.train(dtrain, params = params,
                nrounds = 200,
                early_stopping_rounds = 25,
                verbose = T,
                watchlist = list(train = dtrain, val = dval))
pred = predict(xgb, dtest)
tab = data.table(pred = pred,
                 y_test = y_test)
MLmetrics::MAE(pred, y_test)

imp = xgb.importance(feature_names = colnames(strain),
                     model = xgb)
"nb_pers_attente" %in% imp$Feature
xgb.plot.importance(imp[1:20,])

