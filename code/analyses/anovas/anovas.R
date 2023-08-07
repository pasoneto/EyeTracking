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

#Fetching matched samples
source("./matchedSample.R")
matchedParticipants = subSample
df = df %>%
  filter(Recording.name %in% matchedParticipants)

df = computeAlternancias(df)

aovResult <- aov(value ~ condition*tea*variable, data = df)
summary(aovResult)

print(xtable(summary(aovResult), type = "latex"))

#df$condition = as.factor(df$condition)
#df$tea = as.factor(df$tea)

#mixed-designs ANOVA results
#res.aov <- anova_test(
  #data = df, dv = value, wid = Recording.name,
  #between = tea, within = c(condition, variable))

#aovResult = get_anova_table(res.aov)

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
  group_by(condition, variable, tea) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean))+
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

#New graph
#rostoTarget; #targetRosto
df %>%
  filter(variable %in% c("rostoTarget", "targetRosto")) %>%
  #filter(variable %in% c("fundoProportion", "targetProportion")) %>%
  group_by(condition, variable, tea) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean, color = condition))+
    facet_wrap(~variable)+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report7/conditionVariableTEAProportion.png")


#Propotion
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


#New graph
pd = position_dodge(width = 0.8, preserve = "total")

df %>%
  #filter(variable %in% c("rostoTarget", "targetRosto")) %>%
  filter(variable %in% c("fundoProportion", "targetProportion")) %>%
  group_by(condition, variable, tea) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean, group = condition, color = condition))+
    facet_wrap(~variable)+
    geom_point(position = pd)+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = pd)

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report7/alternancias.jpg")

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
