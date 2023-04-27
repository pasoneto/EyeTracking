import pandas as pd
import numpy as np
import time
import os
import random

#os.chdir('../python/')
from keras.models import Sequential
from keras.layers import Dense, LSTM, Dropout
from sklearn import preprocessing
import keras
import keras.backend as kb

from sklearn.metrics import mean_squared_error, accuracy_score, roc_auc_score, roc_curve, confusion_matrix, balanced_accuracy_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.pipeline import make_pipeline
from keras.callbacks import EarlyStopping, ReduceLROnPlateau, ModelCheckpoint

print(os.listdir())

#colsInterest = "parcel|target|_v|nd"
#colsInput = "_v|nd"
#y = input - integer
def lstmFormat(df, colsInterest, colsInput, y, group):
    df = df[df.columns[df.columns.str.contains(colsInput)]]
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
    dfNonTEA = df[df['tea'] == 'TD']
    dfTEA = df[df['tea'] == 'TEA']

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


timePooints = 2
nFeatures =
nClasses = 2
unitsOPT = 50
dropout1 = 0
dropout2 = 0
dropout3 = 0
dropout4 = 0
model = defineLSTMopt(timePoints, nFeatures, nClasses, unitsOPT, dropout1, dropout2, dropout3, dropout4):

path = "/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv"
df = pd.read_csv(f"{path}")

filters = ['filterDurations', 'filterCutoffs', 'filterConditions']

train, test = trainTestSplit(df)

colsInterest = 'Recording.name|condition|tea|target|variable|focus|Gaze.event.duration|pupil.right|pupil.left|totalFixation|proportionFixation|targetProportion|distractorProportion|fundoProportion|rostoProportion|RD|RE|DR|ER|RT|TR|sexo'
categoricals = ["target", "variable", "focus", "condition", "sexo", "tea"]
x = 'condition|target|variable|focus|Gaze.event.duration|pupil.right|pupil.left|totalFixation|proportionFixation|targetProportion|distractorProportion|fundoProportion|rostoProportion|RD|RE|DR|ER|RT|TR|sexo'
y = 'tea'
condition = "Recording.name"

#Convert categorical
for k in categoricals:
    le.fit(df[k])
    df[k] = le.transform(df[k])

xTrain, yTrain = lstmFormat(train, colsInterest, x, y, condition)
xTest, yTest = lstmFormat(test, colsInterest, x, y, condition)

modelML.fit(xTrain, yTrain, epochs = 100, batch_size = 10000)
lossValue, _ = modelML.evaluate(xTest, yTest, verbose=1)
predictions = np.argmax(modelML.predict(xTest),axis=1)
accuracyValue = balanced_accuracy_score(predictions, yTest)

df = df[df.columns[df.columns.str.contains(colsInterest)]]
inputList = df.groupby(condition).apply(lambda i : i[i.columns[i.columns.str.contains(x)]].to_numpy())
inputList = np.array(inputList.to_list())
#outputList = df.groupby(condition).apply(lambda i : i[y].unique().astype('int'))
outputList = df.groupby(condition).apply(lambda i : i[y].unique())
outputList = outputList.to_list()

inputList.shape
