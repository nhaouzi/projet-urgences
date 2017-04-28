from flask import Flask, jsonify
from datetime import datetime, timedelta
import pandas as pd
from fonctions_projet.operations import *
import xgboost as xgb

app = Flask(__name__)

@app.route('/')
def hello_world():

    ts = datetime.now() + timedelta(hours=2)
    TS = ts.strftime('%Y-%m-%d %H:%M:%S')

    base_2017 = pd.read_csv('/home/noemiehaouzi/mysite/tab2017.csv', sep = ',')
    base_2017['id_hopital'] = base_2017['id_hopital'].apply(str)


    nom_hopitaux = ['Hopital Ambroise Pare', 'Hopital Bichat - Claude Bernard', 'Hopital Lariboisiere', 'Hopital Necker',
        'Hopital de la Pitie Salpetriere']
    nom_ids = ['920100013', '750100232', '750100042', '750100208', '750100125']

    input0 = Timestamp_to_df(TS, base_2017, nom_ids)
    prediction = pd.get_dummies(input0, dummy_na=True)

    model_path = '/home/noemiehaouzi/mysite/models/xgboost_hopital_simul28041344.model'
    model = xgb.Booster()
    model.load_model(model_path)
    prediction = xgb.DMatrix(prediction)

    prediction = model.predict(prediction)

    json_file = []

    index = range(0,len(nom_ids))

    for ind in index:
        print(int(round(prediction[ind],0)))
        json_file.append({'id': str(nom_ids[ind]),
        'nom_hopital' : str(nom_hopitaux[ind]),
        'min_attente_urgence_relative' : int(round(prediction[2*ind],0)) ,
        'min_attente_consultation': int(round(prediction[2*ind+1],0)) })

    return jsonify(json_file)
