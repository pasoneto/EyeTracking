import pandas as pd
import numpy as np
import time
import os
import random

from pandas.api.types import CategoricalDtype
from plydata import define, group_by, summarize
from plydata.tidy import spread

#os.chdir('../python/')
from keras.models import Sequential
from keras.layers import Dense, LSTM, Dropout
from sklearn import preprocessing
import keras
import keras.backend as kb

from sklearn import preprocessing
from sklearn.metrics import mean_squared_error, accuracy_score, roc_auc_score, roc_curve, confusion_matrix, balanced_accuracy_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.pipeline import make_pipeline
from keras.callbacks import EarlyStopping, ReduceLROnPlateau, ModelCheckpoint

#colsInterest = "parcel|target|_v|nd"
#colsInput = "_v|nd"
#y = input - integer
def lstmFormat(df, colsInput, y, group):
    #df = df[df.columns[df.columns.str.contains(colsInput)]]
    inputList = df.groupby(group).apply(lambda x : x[x.columns[x.columns.str.contains(colsInput)]].to_numpy())
    inputList = np.array(inputList.to_list())
    outputList = df.groupby(group).apply(lambda x : x[y].unique().astype('int'))
    outputList = np.array(outputList.to_list())
    return [inputList, outputList]


def defineLSTMopt(timePoints, nFeatures, nClasses, unitsOPT, dropout1, dropout2, dropout3, dropout4):

    model = Sequential()

    model.add(LSTM(units = nClasses, return_sequences = True, activation = 'relu', input_shape = (timePoints, nFeatures)))
    model.add(Dropout(dropout1))

    model.add(LSTM(units = unitsOPT, activation = 'relu'))
    model.add(Dropout(dropout2))

    model.add(Dense(units = unitsOPT*2, activation = 'sigmoid'))
    model.add(Dropout(dropout3))

    model.add(Dense(units = unitsOPT, activation = 'sigmoid'))
    model.add(Dropout(dropout4))

    model.add(Dense(units = nClasses, activation = 'softmax'))

    model.compile(optimizer = 'adam', loss = "sparse_categorical_crossentropy", metrics = ['accuracy'])
    
    return model

def trainTestSplit(df):
    # Getting names of participants for stratified sampling
    dfNonTEA = df.loc[df['tea'] == 0] 
    dfTEA = df.loc[df['tea'] == 1]

    nonTEANames = dfNonTEA['Recording.name'].unique()
    TEANames = dfTEA['Recording.name'].unique()

    N_nonTEA_train = round(len(nonTEANames) * 0.7)
    N_TEA_train = round(len(TEANames) * 0.7)

    N_nonTEA_test = round(len(nonTEANames) * 0.3)
    N_TEA_test = round(len(TEANames) * 0.3)

    trainTEANames = np.random.choice(TEANames, N_TEA_train, replace=False)
    testTEANames = np.setdiff1d(TEANames, trainTEANames)

    trainNonTEANames = np.random.choice(nonTEANames, N_nonTEA_train, replace=False)
    testNonTEANames = np.setdiff1d(nonTEANames, trainNonTEANames)

    # Filtering original dataset
    train = df[df['Recording.name'].isin(np.concatenate([trainTEANames, trainNonTEANames]))]
    test = df[df['Recording.name'].isin(np.concatenate([testTEANames, testNonTEANames]))]

    return [train, test]


timePoints = 2
nFeatures = 37
nClasses = 2
unitsOPT = nFeatures*2
dropout1 = 0.1
dropout2 = 0.1
dropout3 = 0.1
dropout4 = 0.1
model = defineLSTMopt(timePoints, nFeatures, nClasses, unitsOPT, dropout1, dropout2, dropout3, dropout4)

path = "/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv"
df = pd.read_csv(f"{path}")

# Define the filters
filter1 = lambda x: ~x.str.contains('BL_')
filter2 = lambda x: x == False

# Apply the filters and perform the aggregation
dfF = (df
    .pipe(define, filterDurations=filter2(df['filterDurations']))
    .pipe(define, filterCutoffs=filter2(df['filterCutoffs']))
    .pipe(define, filterConditions=filter2(df['filterConditions']))
    .pipe(define, Presented_Stimulus_name_filter=filter1(df['Presented.Stimulus.name']))
    .query('Presented_Stimulus_name_filter & filterDurations & filterCutoffs & filterConditions')
    .groupby(['Recording.name', 'tea', 'condition', "sexo"])
    .agg(Gaze_event_durationSD=('Gaze.event.duration', 'mean'),
         totalFixationSD=('totalFixation', 'mean'),
         proportionFixationSD=('proportionFixation', 'mean'),
         targetProportionSD=('targetProportion', 'mean'),
         distractorProportionSD=('distractorProportion', 'mean'),
         fundoProportionSD=('fundoProportion', 'mean'),
         rostoProportionSD=('rostoProportion', 'mean'),
         RTSD=('RT', 'mean'),
         TRSD=('TR', 'mean'),
         Gaze_event_durationMIN=('Gaze.event.duration', 'min'),
         totalFixationMIN=('totalFixation', 'min'),
         proportionFixationMIN=('proportionFixation', 'min'),
         targetProportionMIN=('targetProportion', 'min'),
         distractorProportionMIN=('distractorProportion', 'min'),
         fundoProportionMIN=('fundoProportion', 'min'),
         rostoProportionMIN=('rostoProportion', 'min'),
         RTMIN=('RT', 'min'),
         TRMIN=('TR', 'min'),
         Gaze_event_durationMAX=('Gaze.event.duration', 'max'),
         totalFixationMAX=('totalFixation', 'max'),
         proportionFixationMAX=('proportionFixation', 'max'),
         targetProportionMAX=('targetProportion', 'max'),
         distractorProportionMAX=('distractorProportion', 'max'),
         fundoProportionMAX=('fundoProportion', 'max'),
         rostoProportionMAX=('rostoProportion', 'max'),
         RTMAX=('RT', 'max'),
         TRMAX=('TR', 'max'),
         Gaze_event_duration=('Gaze.event.duration', 'mean'),
         totalFixation=('totalFixation', 'mean'),
         proportionFixation=('proportionFixation', 'mean'),
         targetProportion=('targetProportion', 'mean'),
         distractorProportion=('distractorProportion', 'mean'),
         fundoProportion=('fundoProportion', 'mean'),
         rostoProportion=('rostoProportion', 'mean'),
         pupilLeft=('pupil.left', 'mean'),
         pupilRight=('pupil.right', 'mean'),
         RT=('RT', 'mean'),
         TR=('TR', 'mean'))
    .reset_index()
    .melt(id_vars=['Recording.name', 'tea', 'condition', "sexo"])
    .pipe(spread, 'variable', 'value')
)

#dfF.groupby("tea").describe()
x = 'condition|variable|Gaze_event_duration|Gaze_event_durationMAX|Gaze_event_durationMIN|Gaze_event_durationSD|RT|RTMAX|RTMIN|RTSD|TR|TRMAX|TRMIN|TRSD|distractorProportion|distractorProportionMAX|distractorProportionMIN|distractorProportionSD|fundoProportion|fundoProportionMAX|fundoProportionMIN|fundoProportionSD|proportionFixation|proportionFixationMAX|proportionFixationMIN|proportionFixationSD|rostoProportion|rostoProportionMAX|rostoProportionMIN|rostoProportionSD|targetProportion|targetProportionMAX|targetProportionMIN|targetProportionSD|totalFixation|totalFixationMAX|totalFixationMIN|totalFixationSD'
y = 'tea'
categoricals = ["condition", "sexo", "tea"]
condition = "Recording.name"

le = preprocessing.LabelEncoder()
#Convert categorical; #0: TD; 1: TEA; 2: nonTD
for k in categoricals:
    le.fit(dfF[k])
    dfF[k] = le.transform(dfF[k])

train, test = trainTestSplit(dfF)

xTrain, yTrain = lstmFormat(train, x, y, condition)
xTest, yTest = lstmFormat(test, x, y, condition)

model.fit(xTrain, yTrain, epochs = 200, batch_size = 10)

lossValue, _ = model.evaluate(xTest, yTest, verbose=1)
predictions = np.argmax(model.predict(xTest),axis=1)
accuracyValue = balanced_accuracy_score(predictions, yTest)

import matplotlib.pyplot as plt
from sklearn import metrics

predictions = le.inverse_transform(predictions)
yTest = le.inverse_transform(yTest)

confusion_matrix = metrics.confusion_matrix(yTest, predictions)
cm_display = metrics.ConfusionMatrixDisplay(confusion_matrix = confusion_matrix)
cm_display.plot()
plt.show()
