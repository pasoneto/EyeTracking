source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Primeiro são filtrados os participantes conforme o critério de inclusão
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE)

df = df %>%
  group_by(Recording.name, Presented.Stimulus.name, condition, tea) %>%
  summarise(distractorProportion = unique(distractorProportion),
            fundoProportion = unique(fundoProportion), 
            targetProportion = unique(targetProportion), 
            rostoProportion = unique(rostoProportion)) %>%
  group_by(condition, tea) %>%
  melt(id.vars = c("condition", "tea", "Recording.name", "Presented.Stimulus.name"))

df %>%
  filter(tea != "nonTD") %>%
  ggplot(aes(x = value, y = tea, fill = tea)) +
    geom_density_ridges(scale = 5, alpha = 0.5)+
    facet_wrap(~variable+condition, scale = "free")

