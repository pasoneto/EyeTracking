source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(pROC)
library(caret)
library(pROC)
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)

#Compute proportions
computeProportions = function(df){
  df = df %>%
    group_by(Recording.name, Presented.Stimulus.name, condition, tea) %>%
    summarise(distractorProportion = unique(distractorProportion),
              fundoProportion = unique(fundoProportion), 
              targetProportion = unique(targetProportion), 
              rostoProportion = unique(rostoProportion)) %>%
    group_by(Recording.name, condition, tea) %>%
    summarise(distractorProportion = mean(distractorProportion),
              fundoProportion = mean(fundoProportion), 
              targetProportion = mean(targetProportion), 
              rostoProportion = mean(rostoProportion)) %>%
    melt(id.vars = c("condition", "tea", "Recording.name"))
  return(df)
}

#Non matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD")

df = computeProportions(df)
plot(roc(df$tea, df$value)) #0.5

#Matched
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
plot(roc(df$tea, df$value)) #0.52


#Alternancias
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD")

computeAlternancias = function(df){
  df = df %>%
    filter(tea != "nonTD") %>%
    group_by(Recording.name, condition, tea) %>%
    #Needs to be mean because if I sum up the number of alternancias, results will be biased towards TD, which is more numerous than TEA.
    #All participants same number of conditions (2)
    summarise(targetRosto = mean(TR),
              rostoTarget = mean(RT), 
              distractorRosto = mean(DR), 
              rostoDistractor = mean(RD)) %>%
    melt(id.vars = c("condition", "Recording.name", "tea"))
  return(df)
}
df = computeAlternancias(df)

rescale <- function(x){(x-min(x))/(max(x)-min(x))}
df$value = rescale(df$value)

plot(roc(df$tea, df$value)) #0.58

#Matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD") %>%
    filter(Recording.name %in% matchedParticipants)

df = computeAlternancias(df)
rescale <- function(x){(x-min(x))/(max(x)-min(x))}
df$value = rescale(df$value)

plot(roc(df$tea, df$value)) #0.58
