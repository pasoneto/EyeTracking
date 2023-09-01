source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
source("../utilsML.R")
library(pROC)
library(caret)
library(pROC)
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)

#Alternancia Matched
source("../../anovas/matchedSample.R")
matchedParticipants = subSample

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>% filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD") %>%
    filter(Recording.name %in% matchedParticipants)

df = computeProportions(df)

proportions = df %>% 
  filter(variable %in% c("fundoProportion", "targetProportion")) %>%
  group_by(variable, Recording.name, tea) %>%
  summarise(value = mean(value)) %>%
  arrange(Recording.name)%>%
  dcast(Recording.name+tea ~ variable)

#Alternancia Matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD") %>%
    filter(Recording.name %in% matchedParticipants) %>%
    arrange(Recording.name)

alternancias = computeAlternancias(df)

alternancias = alternancias %>%
  filter(variable %in% c("targetRosto", "rostoTarget")) %>%
  group_by(variable, Recording.name, tea) %>%
  summarise(value = mean(value)) %>%
  arrange(Recording.name)%>%
  dcast(Recording.name+tea ~ variable) %>%
  select(!tea)

df = merge(alternancias, proportions, by.x = "Recording.name", by.y = "Recording.name")


testResults1 = runForest(df, 10)
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

library(caret)

accs = c()
for(i in 1:1000){
  trainTest = trainTestSplit2(df)
  train = trainTest$train
  test = trainTest$test

  forestfit <- randomForest(as.factor(tea) ~ targetProportion, data=train, ntree = 1000)
  pred = as.factor(predict(forestfit, test))
  true = as.factor(test$tea)

  ac = confusionMatrix(pred, true)$overall["Accuracy"]
  accs = c(accs, ac)
}
mean(accs)
