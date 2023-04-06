source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("../utilsML.R")
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
  summarise(Gaze.event.durationSD = mean(Gaze.event.duration),
            totalFixationSD = mean(totalFixation), #tirar
            proportionFixationSD = mean(proportionFixation),
            targetProportionSD = mean(targetProportion),
            distractorProportionSD = mean(distractorProportion),
            fundoProportionSD = mean(fundoProportion),
            rostoProportionSD = mean(rostoProportion),
            RTSD = mean(RT), #Change to weighted alternancias
            TRSD = mean(TR),

            Gaze.event.durationMIN = min(Gaze.event.duration),
            totalFixationMIN = min(totalFixation), #tirar
            proportionFixationMIN = min(proportionFixation),
            targetProportionMIN = min(targetProportion),
            distractorProportionMIN = min(distractorProportion),
            fundoProportionMIN = min(fundoProportion),
            rostoProportionMIN = min(rostoProportion),
            RTMIN = min(RT), #weighted alternancias
            TRMIN = min(TR), 

            Gaze.event.durationMAX = max(Gaze.event.duration),
            totalFixationMAX = max(totalFixation), #tirar
            proportionFixationMAX = max(proportionFixation),
            targetProportionMAX = max(targetProportion),
            distractorProportionMAX = max(distractorProportion),
            fundoProportionMAX = max(fundoProportion),
            rostoProportionMAX = max(rostoProportion),
            RTMAX = max(RT), #weighted alternancias
            TRMAX = max(TR), 

            Gaze.event.duration = mean(Gaze.event.duration),
            totalFixation = mean(totalFixation), #tirar
            proportionFixation = mean(proportionFixation),
            targetProportion = mean(targetProportion),
            distractorProportion = mean(distractorProportion),
            fundoProportion = mean(fundoProportion),
            rostoProportion = mean(rostoProportion),
            RT = mean(RT), #weighted alternancias
            TR = mean(TR)) %>%
  melt(id.vars = c("Recording.name", "tea")) %>%
  dcast(Recording.name+tea ~ variable)

runForest = function(df, nRuns){
  testResults = c()
  for(k in 1:nRuns){
    trainTest = trainTestSplit2(df)
    train = trainTest$train
    test = trainTest$test

    train$tea = as.factor(train$tea)
    test$tea = as.factor(test$tea)

    forestfit <- randomForest(tea ~ Gaze.event.durationSD+totalFixationSD+proportionFixationSD+targetProportionSD+distractorProportionSD+fundoProportionSD+rostoProportionSD+RTSD+TRSD+Gaze.event.durationMIN+totalFixationMIN+proportionFixationMIN+targetProportionMIN+distractorProportionMIN+fundoProportionMIN+rostoProportionMIN+RTMIN+TRMIN+Gaze.event.durationMAX+totalFixationMAX+proportionFixationMAX+targetProportionMAX+distractorProportionMAX+fundoProportionMAX+rostoProportionMAX+RTMAX+TRMAX+Gaze.event.duration+totalFixation+proportionFixation+targetProportion+distractorProportion+fundoProportion+rostoProportion+RT+TR, data=train, ntree = 1000)
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
  filter(tea == "TEA") %>%
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
