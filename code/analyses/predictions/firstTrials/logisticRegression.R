source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("./utilsML.R")
library("stringr")
library(data.table)
library(dplyr)
library(pROC)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

dfF = df %>%
  filter(condition == "RJA") %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  group_by(Recording.name, tea) %>%
  summarise(Gaze.event.duration = mean(Gaze.event.duration),
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

#Primeiro são filtrados os participantes conforme o critério de inclusão
testResults1 = runLogistic(dfF, 10)
rocs1 = extractROCs(testResults1)

testResults2 = runLogistic(dfNF, 10)
rocs2 = extractROCs(testResults2)

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
