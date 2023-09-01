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

#Checking videos trimmed from first set of experimental sessions
participantsToTrim = c("FS1IP", "FS2IP", "FS3IP", "FS4IP", "FS5IP", "FS6IP", "FS7IP", "FS8IP", "FS9IP", "FS10IP", "FS11IP", "FS13IP", "FS14IP", "FS15IP", "FS16IP", "FS17IP", "FS18IP", "FS19IP", "FS20IP", "FS21IP", "FS22IP", "FS23IP", "FS25IP", "FS26IP", "FS27IP", "MP28IP", "MP30IP", "MP31IP", "MP32IP", "MP33IP", "MP34IP", "MP35IP", "MP36IP", "MP37IP", "MP38IP", "MP40IP", "MP41IP", "MP42IP", "MP43IP", "MP44IP", "MP45IP", "MP46IP", "MP47IP", "MP48IP", "MP49IP", "MP51IP", "MP52IP", "MP53IP", "MP55IP", "MP56IP", "FS58IP", "FS59IP", "FS61IP", "FS62IP", "FS63IP", "FS64IP", "FS65IP", "FS66IP", "FS67IP", "FS68IP", "FS69IP", "MP70IP", "FS72IP", "FS73IP", "FS75IP", "FS77IP", "MP78IP", "FS81IP", "FS82IP", "FS83IP", "FS84IP", "FS85IP", "FS86IP", "FS87IP", "MP88IP", "FS89IP", "FS90IP", "FS91IP", "FS92IP", "FS94IP", "SM112IP", "SM113IP", "SM114IP", "SM117IP", "SM119IP", "SM120IP", "SM121IP", "SM122IP", "CR131IP", "MP164IP")
videosToTrim = c("RJA_A2_B1_E", "RJA_A1_B2_D", "RJA_A2_B2_E", "RJA_A2_B1_D")
timesToTrim = c(7, 6.18, 7, 7)

a %>%
  filter(Recording.name %in% participantsToTrim) %>%
  filter(grepl("RJA_A2_B1_E", Presented.Stimulus.name)) %>%
  filter(Recording.time.end == 7) %>%
  select(Recording.name, Presented.Stimulus.name, Recording.time.begin, Recording.time.end, Gaze.event.duration)


