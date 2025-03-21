source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)

#Cutoffs
source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsVideoDuration.R")
setwd("/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINALPost")

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   print(all(colnames(a) == c("Presented.Stimulus.name", "Eye.movement.type.index", "Computer.timestamp.begin", "Computer.timestamp.end", "Recording.time.begin", "Recording.time.end", "Gaze.event.duration", "Eye.movement.type", "variable", "value", "pupil.right", "pupil.left", "target", "Recording.name", "numberDuplicated")))
                   a$Computer.timestamp.begin = as.numeric(a$Computer.timestamp.begin)
                   a$Computer.timestamp.end = as.numeric(a$Computer.timestamp.end)
                   return(a)
            }
)
a = bind_rows(file_list)

#N trials / participants after filter
a = a %>% mutate(trialsToFilter = paste(Presented.Stimulus.name, Recording.name, sep = ""))

#Cutoff proporção de 50%
source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsProportion.R")
a = a %>%
  mutate(trialsToFilter = paste(Presented.Stimulus.name, Recording.name, sep = "")) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(!trialsToFilter %in% filterOutDurations$filterOutDurations) %>% #anomalous videos duration
  filter(!trialsToFilter %in% filterOutNames05$filterOutTrials) #0.5 cutoff

#Cutoff um trial por condição
a = a %>% 
  mutate(condition = substr(Presented.Stimulus.name, 1, 3)) %>%
  filter(!str_detect(condition, 'BL_')) %>%
  group_by(Recording.name) %>%
  mutate(nConditions = length(unique(condition))) %>%
  filter(nConditions != 2) %>%
  ungroup() %>%
  select(Recording.name) %>% 
  unique()

filterOutConditions = data.frame(filterOutConditions = unique(a$Recording.name))
