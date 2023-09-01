source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)
library(MatchIt)

set.seed(1000)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Primeiro são filtrados os participantes conforme o critério de inclusão
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TD", "TEA"))

subSample = df %>%
  select(Recording.name, ageJA, sexo, tea) %>%
  distinct(Recording.name, .keep_all = TRUE) %>%
  na.omit()

nonMatchedStatistic = subSample %>%
  group_by(tea) %>%
  summarise(meanAge = mean(ageJA)/365,
            sdAge = sd(ageJA)/365,
            N = length(ageJA),
            minAge = min(ageJA),
            maxAge = max(ageJA))

subSample = matchit(as.factor(tea)~ageJA+as.factor(sexo), 
                    data=subSample, 
                    method="nearest", 
                    ratio=1)

subSample = match.data(subSample)

dStatistic = subSample %>%
  group_by(tea, sexo) %>%
  summarise(meanAge = mean(ageJA)/365,
            sdAge = sd(ageJA)/365,
            minAge = min(ageJA)/365,
            maxAge = max(ageJA)/365)

print(xtable(nonMatchedStatistic, type = "latex"))
print(xtable(dStatistic, type = "latex"))

subSample = subSample$Recording.name
