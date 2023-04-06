source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("../utilsML.R")
library("stringr")
library(data.table)
library(dplyr)
library(pROC)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

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


#Trains logistic regression
runLogistic = function(df, nRUNS){
  testResults = c()
  for(k in 1:nRUNS){
    trainTest = trainTestSplit(dfF)
    train = trainTest$train
    test = trainTest$test
    train$tea = as.factor(train$tea)
    test$tea = as.factor(test$tea)
    logisticR = glm(tea ~ Gaze.event.durationSD+totalFixationSD+proportionFixationSD+targetProportionSD+distractorProportionSD+fundoProportionSD+rostoProportionSD+RTSD+TRSD+Gaze.event.durationMIN+totalFixationMIN+proportionFixationMIN+targetProportionMIN+distractorProportionMIN+fundoProportionMIN+rostoProportionMIN+RTMIN+TRMIN+Gaze.event.durationMAX+totalFixationMAX+proportionFixationMAX+targetProportionMAX+distractorProportionMAX+fundoProportionMAX+rostoProportionMAX+RTMAX+TRMAX+Gaze.event.duration+totalFixation+proportionFixation+targetProportion+distractorProportion+fundoProportion+rostoProportion+RT+TR, data = train, family = "binomial")
    test$teaProb = predict(logisticR, test, type="response")
    test$preds = ifelse(test$teaProb > 0.5, TRUE, FALSE)

    testResults[[k]] = test
  }
  return(testResults)
}



#Primeiro são filtrados os participantes conforme o critério de inclusão
testResults1 = runLogistic(dfF, 10)
rocs1 = extractROCs(testResults1)

rocs1$filter = "TRUE"
rocs2$filter = "FALSE"

rocs = bind_rows(rocs1, rocs2)

rocs %>%
  ggplot(aes(x = TPR, y = FPR, color = as.factor(run)))+
    facet_wrap(~filter)+
    geom_path()

rocs %>% 
  group_by(run, filter) %>%
  summarise(auc = unique(auc)) %>%
  group_by(filter) %>%
  summarise(meanAuc = mean(auc),
            sd = sd(auc)
)
