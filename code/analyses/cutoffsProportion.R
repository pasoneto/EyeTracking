#Calculates the total time spent looking at the screen by different conditions
source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("readxl")
library("ggridges")
library(xtable)
directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL/"

setwd(directory)

filesA = list.files()
file_list = lapply(filesA, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)

a = bind_rows(file_list)

allParticipants1 = lapply(file_list, processOne)
allParticipants1 = dplyr::bind_rows(allParticipants1)
allParticipants1 = allParticipants1 %>% filter(condition != "BL")

allParticipants1 = allParticipants1 %>% 
  group_by(Recording.name, Presented.Stimulus.name) %>%
  summarise(totalFixation = unique(totalFixation),
            tea = unique(tea)
  )
allParticipants1
allParticipants1$Original.names = stri_sub(allParticipants1$Presented.Stimulus.name,1, 11)

#Read duration of each trial
timeTrials = fread("../../dataSabara/durationTrials.csv")
timeTrials = timeTrials %>% select(Trial, Tempo) 
colnames(timeTrials) = c("Original.names", "totalDuration")
timeTrials %<>% filter(!Original.names %in% c("BASELINE_A1_B2", "BASELINE_A2_B2"))
timeTrials %<>% filter(!duplicated(Original.names))

#Merge with database
allParticipants1 = merge(allParticipants1, timeTrials, by = "Original.names")

filterOutNames075 = allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  filter(proportion <= 0.75) %>%
  ungroup() %>%
  select(Presented.Stimulus.name, Recording.name) %>%
  mutate(filterOutTrials = paste(Presented.Stimulus.name, Recording.name, sep = "")) %>%
  select(filterOutTrials)

filterOutNames05 = allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  filter(proportion <= 0.5) %>%
  ungroup() %>%
  select(Presented.Stimulus.name, Recording.name) %>%
  mutate(filterOutTrials = paste(Presented.Stimulus.name, Recording.name, sep = "")) %>%
  select(filterOutTrials)

filterOutNames025 = allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  filter(proportion <= 0.25) %>%
  ungroup() %>%
  select(Presented.Stimulus.name, Recording.name) %>%
  mutate(filterOutTrials = paste(Presented.Stimulus.name, Recording.name, sep = "")) %>%
  select(filterOutTrials)

