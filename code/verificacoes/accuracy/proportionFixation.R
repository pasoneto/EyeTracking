#Calculates the total time spent looking at the screen by different conditions
source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("readxl")
library("ggridges")
directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipant/"

setwd(directory)

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)
file_list[[1]]
allParticipants1 = lapply(file_list, processOne)
allParticipants1 = dplyr::bind_rows(allParticipants1)

allParticipants1 = allParticipants1 %>% filter(condition != "BL")
allParticipants1
allParticipants1 %<>% 
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
allParticipants1 %>%
  group_by(Recording.name, Presented.Stimulus.name) %>%
  mutate(proportion = totalFixation/totalDuration) %>%
  arrange(Recording.name, Presented.Stimulus.name) %>%
  ggplot(aes(x=proportion)) +
    facet_wrap(~Presented.Stimulus.name, scale="free")+
    geom_histogram()

#SI692IP
#IJA_A3_B1_D_33
a = readAndRename("/Users/pdealcan/Documents/github/dataSabara/AllData/3/Joint Attention 2022 SI692IP.xlsx")
a %>% 
    select("Computer.timestamp", "Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", "Eye.movement.type.index", colunas) %>%
    filter(Eye.movement.type == "fixation") %>% divisu())
    dvisu()
  dvisu()
unique(a$Event.value)

#RJA_A1_B2_D
#FS749IP
#Manual calculation = 908+1200+425+1933+825 == 5291*2
#Algorithm calculation = 10.582
#10.582/2 == manual calculation / 10957 with saccades and eyes not found

#Checar video_end-video_start para todos os participantes e trials

#IJA_A1_B2_E (duration reported 7.175039)
#MR275IP (Manual = 7931)
#Manual caltulation = 7315 / 7947 including saccades, eyes not found and non-classified

