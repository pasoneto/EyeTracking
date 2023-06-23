source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)
library(randomForest)
library(caret)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Primeiro são filtrados os participantes conforme o critério de inclusão
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD")

#totalAlternancia: tea < td
rule1 = function(df){
  mediaGeral = df %>%
    filter(tea == "TD") %>%
    group_by(Recording.name) %>%
    summarise(mediaAlternancia = mean(RD+RE+DR+ER+RT+TR))
  mediaGeral = mean(mediaGeral$mediaAlternancia)
  individual = df %>%
    group_by(Recording.name) %>%
    summarise(meanAlternancia = mean(RD+RE+DR+ER+RT+TR)) %>%
    mutate(rule1 = meanAlternancia < mediaGeral)
  individual = individual %>%
    select(Recording.name, rule1)
  return(individual)
}

#proportion distractor: tea > TD
rule2 = function(df){
  mediaGeral = df %>%
    filter(tea == "TD") %>%
    summarise(mediaGeral = mean(distractorProportion))
  mediaGeral = mediaGeral$mediaGeral
  individual = df %>%
    group_by(Recording.name) %>%
    summarise(meanDistractorProportion = mean(distractorProportion)) %>%
    mutate(rule2 = meanDistractorProportion < mediaGeral)
  individual = individual %>%
    select(Recording.name, rule2)
  return(individual)
}

#proportion fundo: tea > TD
rule3 = function(df){
  mediaGeral = df %>%
    filter(tea == "TD") %>%
    summarise(mediaGeral = mean(fundoProportion))
  mediaGeral = mediaGeral$mediaGeral
  individual = df %>%
    group_by(Recording.name) %>%
    summarise(meanFundoProportion = mean(fundoProportion)) %>%
    mutate(rule3 = meanFundoProportion < mediaGeral)
  individual = individual %>%
    select(Recording.name, rule3)
  return(individual)
}

#proportion target: tea < TD
rule4 = function(df){
  mediaGeral = df %>%
    filter(tea == "TD") %>%
    summarise(mediaGeral = mean(targetProportion))
  mediaGeral = mediaGeral$mediaGeral
  individual = df %>%
    group_by(Recording.name) %>%
    summarise(meanTargetProportion = mean(targetProportion)) %>%
    mutate(rule4 = meanTargetProportion < mediaGeral)
  individual = individual %>%
    select(Recording.name, rule4)
  return(individual)
}

a = merge(rule1(df), rule2(df))
b = merge(rule3(df), rule4(df))

a = merge(a, b)
df = merge(a, df %>% filter(tea != "nonTD") %>% select(Recording.name, tea))

df = df %>% distinct() %>% select(!Recording.name)

rf = randomForest(as.factor(tea)~., ntree = 5000, data=train)

p1 <- predict(rf, test)
confusionMatrix(p1, as.factor(test$tea))

df$rule1[df$rule1 == TRUE] <- 'TEA'
df$rule1[df$rule1 == FALSE] <- 'TD'

