#Calculates the total time spent looking at the screen by different conditions
source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("readxl")
library(xtable)
#directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipant/"
directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipantPost/"
durations = fread("/Users/pdealcan/Documents/github/sabara/code/verificacoes/accuracy/timeAll.csv") 


#Filtering out outliers by video duration. Non filtered is 431, filtered is 426.
filterOutDurations = durations %>%
  group_by(Event.value) %>%
  mutate(IQR = IQR(time)*1.5,
         Q1 = quantile(time)[2],
         Q3 = quantile(time)[4],
         lower = Q1-IQR,
         upper = Q3+IQR) %>%
  filter(time >= upper |  time <= lower) %>%
  mutate(filterOutDurations = paste(Event.value, Recording.name, sep = "")) %>%
  ungroup() %>%
  select(filterOutDurations)
