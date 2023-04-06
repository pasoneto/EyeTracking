source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("./utilsML.R")
library("stringr")
library(data.table)
library(dplyr)
library(pROC)
library(e1071)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
#df = df %>% drop_na()

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

dfNF = df %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(condition == "RJA") %>%
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

#dfNF = dfNF %>% drop_na()
runSVM = function(df, nRuns){
  testResults = c()
  for(k in 1:nRuns){
    trainTest = trainTestSplit(df)
    train = trainTest$train
    test = trainTest$test

    train$tea = as.factor(train$tea)
    test$tea = as.factor(test$tea)

    svmfit = svm(tea ~ Gaze.event.duration+totalFixation+proportionFixation+targetProportion+distractorProportion+fundoProportion+rostoProportion+RT+TR+Gaze.event.duration+totalFixation+proportionFixation+targetProportion+distractorProportion+fundoProportion+rostoProportion+RT+TR, data = train, kernel = "radial", cost = 10, probability = TRUE)
    pred = predict(svmfit, test, probability = TRUE)

    test$teaProb = unlist(attr(pred, "probabilities")[, 2])

    testResults[[k]] = test
  }
  return(testResults)
}

testResults1 = runSVM(dfF, 10)
rocs1 = extractROCs(testResults1)

testResults2 = runSVM(dfNF, 10)
rocs2 = extractROCs(testResults2)

rocs1$filter = "TRUE"
rocs2$filter = "FALSE"

rocs = bind_rows(rocs1, rocs2)

rocs %>% 
  ggplot(aes(x = TPR, y = FPR, color = as.factor(run)))+
    facet_wrap(~filter)+
    geom_path(size = 0.2)

rocs %>% 
  group_by(run, filter) %>%
  summarise(auc = unique(auc)) %>%
  group_by(filter) %>%
  summarise(meanAuc = mean(auc),
            sd = sd(auc)
  )

testResults = bind_rows(testResults1)
testResults %>% colnames()

testResults %>% 
  ggplot(aes(x = teaProb, y = targetProportion))+
    geom_point(size = 0.2)

testResults %>% 
  ggplot(aes(x = teaProb, y = distractorProportion))+
    geom_point(size = 0.2)


