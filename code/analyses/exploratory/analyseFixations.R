#Calculates the total time spent looking at the screen by different conditions
source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("readxl")
library("ggridges")
library(xtable)
directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipant/"

setwd(directory)

files = list.files()
file_list = lapply(files, function(i){
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

allParticipants1$Original.names = stri_sub(allParticipants1$Presented.Stimulus.name,1, 11)

#Read duration of each trial
timeTrials = fread("../../dataSabara/durationTrials.csv")
timeTrials = timeTrials %>% select(Trial, Tempo) 
colnames(timeTrials) = c("Original.names", "totalDuration")
timeTrials %<>% filter(!Original.names %in% c("BASELINE_A1_B2", "BASELINE_A2_B2"))
timeTrials %<>% filter(!duplicated(Original.names))

#Merge with database
allParticipants1 = merge(allParticipants1, timeTrials, by = "Original.names")

#Merge CARS
cars = fread("../../dataSabara/CARS.csv")
colnames(cars) = c("Recording.name", "cars")
allParticipants1 = merge(allParticipants1, cars, by = "Recording.name")

#Still need to check the cases with "TRANSFERIDO", or missing numbers
#Visualize distribution
allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  summarise(total = mean(proportion),
            cars = as.numeric(unique(cars))) %>% #
  ggplot(aes(x=Original.names, y=total, color=cars)) +
    geom_jitter()
