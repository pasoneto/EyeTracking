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

#Visualize distribution
histogramaProporcoes = allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  ggplot(aes(x=proportion)) +
    facet_wrap(~Original.names, scale="free")+
    geom_histogram()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/report16/generalDistribution.png")

zero15 = allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  filter(proportion >= 0.15) %>%
  ungroup() %>%
  summarise(N_left = length(unique(Recording.name))/487,
            n_trials = length(Presented.Stimulus.name)/5165,
            threshold = 0.15
  )

zero25 = allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  filter(proportion >= 0.25) %>%
  ungroup() %>%
  summarise(N_left = length(unique(Recording.name))/487,
            n_trials = length(Presented.Stimulus.name)/5165,
            threshold = 0.25
  )

zero50 = allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  filter(proportion >= 0.50) %>%
  ungroup() %>%
  summarise(N_left = length(unique(Recording.name))/487,
            n_trials = length(Presented.Stimulus.name)/5165,
            threshold = 0.50
  )

zero75 = allParticipants1 %>%
  group_by(Recording.name, Original.names) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  filter(proportion >= 0.75) %>%
  ungroup() %>%
  summarise(N_left = length(unique(Recording.name))/487,
            n_trials = length(Presented.Stimulus.name)/5165,
            threshold = 0.75
)

allThresholds = bind_rows(zero75, zero50, zero25, zero15)
xtable(allThresholds)
