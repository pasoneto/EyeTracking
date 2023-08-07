source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(pROC)
library(caret)
library(pROC)
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)

#Mantem AOI, colapsa por condição, soma por variable
rocPlotter = function(df, AOI, condition, plot = FALSE){
  runCondition = is.character(condition)
  if(runCondition){
    df = df %>%
      group_by(variable, Recording.name, tea, condition) %>%
      summarise(value = mean(value)) %>%
      filter(variable == AOI) %>%
      filter(condition == condition)

  } else{
    df = df %>%
      group_by(variable, Recording.name, tea) %>%
      summarise(value = mean(value)) %>%
      filter(variable == AOI)
  }
  return(roc(df$tea, df$value, plot = plot))
}

#Alternancia Matched
source("./matchedSample.R")
matchedParticipants = subSample
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD") %>%
    filter(Recording.name %in% matchedParticipants)

df = computeProportions(df)

df = df %>% 
  filter(variable %in% c("fundoProportion", "targetProportion")) %>%
  group_by(variable, Recording.name, tea) %>%
  summarise(value = mean(value)) %>%
  arrange(Recording.name)

matchedA = rocPlotter(df, "fundoProportion", FALSE, TRUE) #0.73
matchedB = rocPlotter(df, "targetProportion", FALSE, TRUE) #79

#Proportion non matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD")

df = computeProportions(df)

nonMatchedA = rocPlotter(df, "fundoProportion", FALSE) #0.632
nonMatchedB = rocPlotter(df, "targetProportion", FALSE) #0.64

#Both proportion
ggroc(list(matched = matchedA, nonMatched = nonMatchedA))+
  ggtitle(paste0('fundoProportion. AUC matched = ', 0.63, '; ', 'AUC non-matched: ', '0.64'))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report7/rocFundoProportion.jpg")

ggroc(list(matched = matchedB, nonMatched = nonMatchedB))+
  ggtitle(paste0('targetProportion. AUC matched = ', 0.73, '; ', 'AUC non-matched: ', 0.79))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report7/rocTargetProportion.jpg")


#Alternancias não matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD")

df = computeAlternancias(df)

df$value = rescale(df$value)

nonMatchedA = rocPlotter(df, "rostoTarget", FALSE) #0.66
nonMatchedB = rocPlotter(df, "targetRosto", FALSE) #0.66

#Alternancia Matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD") %>%
    filter(Recording.name %in% matchedParticipants)

df = computeAlternancias(df)
df$value = rescale(df$value)

matchedA = rocPlotter(df, "rostoTarget", FALSE) #0.66
matchedB = rocPlotter(df, "targetRosto", FALSE) #0.66

ggroc(list(matched = matchedA, nonMatched = nonMatchedA))+
  ggtitle(paste0('rostoTarget AUC matched = ', 0.66, '; ', 'AUC non-matched: ', '0.66'))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report7/rocRostoTarget.jpg")

ggroc(list(matched = matchedB, nonMatched = nonMatchedB))+
  ggtitle(paste0('targetRosto AUC matched = ', 0.66, '; ', 'AUC non-matched: ', '0.66'))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report7/rocTargetRosto.jpg")


