source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(ggridges)
library(dplyr)

a = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Example of how to show number of alternances 
a %>%
  #Primeiro são filtrados os participantes conforme o critério de inclusão
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(filterDurations == FALSE) %>% #Problem here. Not the same as other report. Verify
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  filter(condition == "RJA") %>%
  group_by(Recording.name, condition, target, tea) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>%
  melt(id.vars = c("condition", "target", "Recording.name", "tea")) %>%
  ggplot(aes(y = target, x = value, fill = target)) +
    facet_wrap(~variable+tea, scale="free")+
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

a %>%
  #Primeiro são filtrados os participantes conforme o critério de inclusão
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(filterDurations == FALSE) %>% #Problem here. Not the same as other report. Verify
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  filter(condition == "IJA") %>%
  #Agora plotados
  group_by(Recording.name, condition, target, tea) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>%
  melt(id.vars = c("condition", "target", "Recording.name", "tea")) %>%
  ggplot(aes(y = target, x = value, fill = target)) +
    facet_wrap(~variable+tea, scale="free")+
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

#Example of how to show total fixation duration
a %>%
  #Primeiro são filtrados os participantes conforme o critério de inclusão
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(filterDurations == FALSE) %>% #Problem here. Not the same as other report. Verify
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  #Depois computadas estatísitcas de medida central para cada trial/participante
  group_by(Recording.name, Presented.Stimulus.name, focus, condition, tea) %>%
  summarise(proportionFixation = sum(proportionFixation)) %>%
  #Plotagem dos resultados
  ggplot(aes(x = focus, y = proportionFixation, fill = tea)) +
    facet_wrap(~condition)+
    geom_boxplot()+
    ylab("Proporção de fixação")+
    xlab("")

a %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  group_by(condition, tea) %>%
  summarise(pupil = mean(pupil.right + pupil.left, na.rm = TRUE))


