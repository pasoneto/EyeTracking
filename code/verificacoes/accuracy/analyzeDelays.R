source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("readxl")
directory = "/Users/pdealcan/Documents/github/dataSabara/AllData"

setwd("/Users/pdealcan/Documents/github/sabara/code/verificacoes/accuracy/")

srs = fread("./samplingRates.csv")
durations = fread("./timeAll.csv") 
colnames(durations) = c("V1", "Presented.Stimulus.name", "time", "N", "Recording.name")

a = merge(durations, srs, by.x = c("Presented.Stimulus.name", "Recording.name"), by.y = c("Presented.Stimulus.name", "Recording.name"))
a$Presented.Stimulus.name = substr(a$Presented.Stimulus.name, 1, 11)

#MR261IP
#RJA_A2_B1_D 

#RJA_A1_B2_D
#FS749IP

#Boxplot of trial durations
boxplotDurations = a %>%
  ggplot(aes(y = time/1000000))+
    facet_wrap(~Presented.Stimulus.name)+
    geom_boxplot()

ggsave("../../../reports/report15/durations.png")

#Getting single outlier case
a %>%
  select(time, Presented.Stimulus.name, Recording.name) %>%
  filter(str_detect(Presented.Stimulus.name, 'RJA_A2_B2_E')) %>%
  filter(Recording.name == "CR641IP") %>%
  unique()
#First presentation of the video RJA_A2_B2_E tem 4.2s, second presentation has 8.6

##Investigating if larger duration time is due to abnormal sampling rates
outlierCase = a %>%
  select(time, Presented.Stimulus.name, Recording.name, d) %>%
  filter(str_detect(Presented.Stimulus.name, 'RJA_A2_B2_E')) %>%
  filter(Recording.name == "CR641IP")
 
baseline = a %>%
  select(time, Presented.Stimulus.name, Recording.name, d) %>%
  filter(str_detect(Presented.Stimulus.name, 'RJA_A2_B2_E'))

outlierCase = data.frame(duration = c(outlierCase$d, baseline$d),
                         case = c(rep("outlierCase", length(outlierCase$d)), rep("baseline", length(baseline$d))))

outlierComparison = outlierCase %>%
  filter(duration > 8300) %>%
  ggplot(aes(x = duration, color = case))+
    geom_density()

ggsave("../../../reports/report15/srs.png")

#Double checking duration for outlier participant #CR641IP, trial #RJA_A2_B2_E
a = readAndRename("/Users/pdealcan/Documents/github/dataSabara/AllData/0/Joint Attention 2022 CR641IP.xlsx")
a %<>%
  filter(!is.na(Presented.Stimulus.name)) %>%
  filter(!str_detect(Presented.Stimulus.name, 'Eyetracker Calibration|BL_|bichinhocolorido|bola|fadinha|MASK'))

indexing = fixationIndexer(a$Presented.Stimulus.name)
a$Presented.Stimulus.name = paste(a$Presented.Stimulus.name, indexing, sep = "_")

a %>%
  filter(str_detect(Presented.Stimulus.name, 'RJA_A2_B2_E')) %>%
  group_by(Presented.Stimulus.name) %>%
  summarise(dur = max(Computer.timestamp) - min(Computer.timestamp))
