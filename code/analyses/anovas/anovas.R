source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Primeiro são filtrados os participantes conforme o critério de inclusão
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE)

df %>%
  select(Recording.name, sexo, tea) %>%
  distinct() %>%
  group_by(sexo) %>%
  filter(tea == "TD") %>%
  summarise(length(Recording.name))


#Fetching matched samples
source("./matchedSample.R")
matchedParticipants = subSample
df = df %>%
  filter(Recording.name %in% matchedParticipants)

#Compute alternancias
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

aovResult <- aov(value ~ condition*tea*variable, data = df)
summary(aovResult)

print(xtable(summary(aovResult), type = "latex"))

#Visualizing TEA effect on alternancias. 
#Mean number of alternancias per diagnostic and trial
df %>%
  group_by(tea) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report6/teaMainAlternancia.png")

#Visualizing effect of variable
unique(df$variable) %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = variable, y = mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report6/variableAlternancia.png")

#Visualizing effect of condition
df %>%
  group_by(condition) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = condition, y = mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report6/conditionAlternancia.png")

#Visualizing interaction of condition and variable
df %>%
  group_by(condition, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = condition, y = mean))+
    facet_wrap(~variable)+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report6/conditionVariableAlternancia.png")
#Visualizing interaction between tea and variable
df %>%
  group_by(tea, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean))+
    facet_wrap(~variable)+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report6/teaVariableAlternancia.png")

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

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea != "nonTD")

df = df %>%
  filter(Recording.name %in% matchedParticipants)

df = computeProportions(df)
aovResult <- aov(value ~ condition*tea*variable, data = df)
summary(aovResult)
print(xtable(summary(aovResult), type = "latex"))

#Visualizing variable effect 
df %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = variable, y = mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report6/variableProportion.png")

#Visualizing interaction between condition and variable 
df %>%
  group_by(condition, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = condition, y = mean))+
    facet_wrap(~variable)+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report6/conditionVariableProportion.png")
#Visualizing interaction between tea and variable 
df %>%
  group_by(tea, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean))+
    facet_wrap(~variable)+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report6/teaVariableProportion.png")
