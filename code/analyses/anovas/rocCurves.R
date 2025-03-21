source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(pROC)
ibrary(caret)
library(pROC)
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)

otherTEA = "TD"

#Alternancia Non matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TEA", otherTEA))

df = computeProportions(df)

df = df %>% 
  group_by(variable, Recording.name, tea) %>%
  summarise(value = mean(value)) %>%
  arrange(Recording.name)

conditionSet = FALSE
plotSet = FALSE
nonMatchedA = rocPlotter(df, "fundoProportion", conditionSet, TRUE)
nonMatchedB = rocPlotter(df, "targetProportion", conditionSet, plotSet)
nonMatchedC = rocPlotter(df, "distractorProportion", conditionSet, plotSet)
nonMatchedD = rocPlotter(df, "rostoProportion", conditionSet, plotSet)

aois = c("fundoProportion", "targetProportion", "distractorProportion", "rostoProportion")
k = "rostoProportion"
for(k in aois){
  dfFinal = decisionVisualizer(df, k)
  graphF = dfFinal %>%
    melt(id.vars = c("threshs", "direction")) %>%
    ggplot(aes(x = threshs, y = value, color = variable)) +
      geom_line() +
      labs(title=paste(k, unique(dfFinal$direction)))
  ggsave(paste("/Users/pdealcan/Documents/github/sabara/reports/2023/report12/", k, ".png", sep = ""))
}


## tirar fundo roc, rodar só para não matched

aucA = round(nonMatchedA$auc, 2)
aucB = round(nonMatchedB$auc, 2)
aucC = round(nonMatchedC$auc, 2)
aucD = round(nonMatchedD$auc, 2)

titleAUC = paste('fundo = ', aucA, 
                  'target = ,', aucB,
                  'distractor =', aucC,
                  'rosto =', aucD)

#Minimizar false positive

#Both proportion
ggroc(list(fundo = nonMatchedA, target = nonMatchedB, distractor = nonMatchedC, rosto = nonMatchedD))+
  ggtitle(titleAUC)

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report11/proportionNonMatched.jpg")

#Proportion matched
source("./matchedSample.R")
matchedParticipants = subSample
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TEA", otherTEA)) %>%
    filter(Recording.name %in% matchedParticipants)

df = computeProportions(df)

conditionSet = FALSE
plotSet = FALSE
matchedA = rocPlotter(df, "fundoProportion", conditionSet, plotSet)
matchedB = rocPlotter(df, "targetProportion", conditionSet, plotSet)
matchedC = rocPlotter(df, "distractorProportion", conditionSet, plotSet)
matchedD = rocPlotter(df, "rostoProportion", conditionSet, plotSet)

aucA = round(matchedA$auc, 2)
aucB = round(matchedB$auc, 2)
aucC = round(matchedC$auc, 2)
aucD = round(matchedD$auc, 2)

titleAUC = paste('fundo = ', aucA, 
                  'target = ,', aucB,
                  'distractor =', aucC,
                  'rosto =', aucD)

ggroc(list(fundo = matchedA, target = matchedB, distractor = matchedC, rosto = matchedD))+
  ggtitle(titleAUC)

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report11/proportionMatched.jpg")

###############################
### Alternancias não matched ##
###############################
otherTEA = "nonTD"
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TEA", otherTEA))

df = computeAlternancias(df)

df$value = rescale(df$value)
conditionSet = FALSE 
nonMatchedA = rocPlotter(df, "rostoTarget", conditionSet)
nonMatchedB = rocPlotter(df, "targetRosto", conditionSet)
nonMatchedC = rocPlotter(df, "distractorRosto", conditionSet)
nonMatchedD = rocPlotter(df, "rostoDistractor", conditionSet)

aucA = round(nonMatchedA$auc, 2)
aucB = round(nonMatchedB$auc, 2)
aucC = round(nonMatchedC$auc, 2)
aucD = round(nonMatchedD$auc, 2)

titleAUC = paste('rostoTarget = ', aucA, 
                  'targetRosto = ,', aucB,
                  'distractorRosto =', aucC,
                  'rostoDistractor =', aucD)

#Both proportion
ggroc(list(rostoTarget = nonMatchedA, targetRosto = nonMatchedB, distractorRosto = nonMatchedC, rostoDistractor = nonMatchedD))+
  ggtitle(titleAUC)

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report11/alternanciaNonMatched.jpg")

#Alternancia Matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TEA", otherTEA)) #%>%
    #filter(Recording.name %in% matchedParticipants)

df = computeAlternancias(df)
df$value = rescale(df$value)

conditionSet = FALSE
matchedA = rocPlotter(df, "rostoTarget", conditionSet)
matchedB = rocPlotter(df, "targetRosto", conditionSet)
matchedC = rocPlotter(df, "distractorRosto", conditionSet)
matchedD = rocPlotter(df, "rostoDistractor", conditionSet)

aucA = round(matchedA$auc, 2)
aucB = round(matchedB$auc, 2)
aucC = round(matchedC$auc, 2)
aucD = round(matchedD$auc, 2)

titleAUC = paste('fundo = ', aucA, 
                  'target = ,', aucB,
                  'distractor =', aucC,
                  'rosto =', aucD)

#Both proportion
ggroc(list(fundo = matchedA, target = matchedB, distractor = matchedC, rosto = matchedD))+
  ggtitle(titleAUC)

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report11/alternanciaMatched.jpg")

aois = c("rostoTarget", "targetRosto", "distractorRosto", "rostoDistractor")
k = "targetRosto"
for(k in aois){
  dfFinal = decisionVisualizer(df, k)
  graphF = dfFinal %>%
    ggplot(aes(x = threshs, y = value, color = variable)) +
      geom_line() +
      labs(title=paste(k, unique(dfFinal$direction)))
  ggsave(paste("/Users/pdealcan/Documents/github/sabara/reports/2023/report12/", k, ".png", sep = ""))
}

