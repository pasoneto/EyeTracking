source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("./utilsML.R")
library("stringr")
library(data.table)
library(dplyr)
library(pROC)
library(randomForest)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Primeiro são filtrados os participantes conforme o critério de inclusão
dfF = df %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(condition == "RJA") %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  group_by(Recording.name, tea) %>%
  summarise(Gaze.event.duration = mean(Gaze.event.duration),
            totalFixation = mean(totalFixation),
            proportionFixation = mean(proportionFixation),
            targetProportion = mean(targetProportion),
            distractorProportion = mean(distractorProportion),
            fundoProportion = mean(fundoProportion),
            rostoProportion = mean(rostoProportion),
            RT = mean(RT),
            TR = mean(TR)) %>%
  melt(id.vars = c("Recording.name", "tea")) %>%
  dcast(Recording.name+tea ~ variable)

runForest = function(df, nRuns){
  testResults = c()
  for(k in 1:nRuns){
    trainTest = trainTestSplit(df)
    train = trainTest$train
    test = trainTest$test

    train$tea = as.factor(train$tea)
    test$tea = as.factor(test$tea)

    forestfit <- randomForest(tea ~ Gaze.event.duration+totalFixation+proportionFixation+targetProportion+distractorProportion+fundoProportion+rostoProportion+RT+TR+Gaze.event.duration+totalFixation+proportionFixation+targetProportion+distractorProportion+fundoProportion+rostoProportion+RT+TR, data=train, ntree = 10000)
    pred = predict(forestfit, test, type="prob")
    predname = predict(forestfit, test)
  
    test$predicted = predname
    test$teaProb = pred[, 2] #Probability of being tea

    testResults[[k]] = test
  }
  return(testResults)
}

testResults1 = runForest(dfF, 10)
rocs1 = extractROCs(testResults1)

bind_rows(testResults1) %>%
  filter(predicted == "TEA") %>%
  select(predicted, teaProb, tea)

rocs1 %>% 
  ggplot(aes(x = TPR, y = FPR, color = as.factor(run)))+
    geom_path(size = 0.2)

rocs1 %>% 
  group_by(run) %>%
  summarise(auc = unique(auc)) %>%
  summarise(meanAuc = mean(auc),
            sd = sd(auc)
  )
