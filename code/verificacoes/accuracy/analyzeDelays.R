source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("readxl")
directory = "/Users/pdealcan/Documents/github/dataSabara/AllData"

srs = fread("./samplingRates.csv")
durations = fread("./timeAll.csv") 
colnames(durations) = c("V1", "Presented.Stimulus.name", "time", "N", "Recording.name")

a = merge(durations, srs, by.x = c("Presented.Stimulus.name", "Recording.name"), by.y = c("Presented.Stimulus.name", "Recording.name"))
durations %>% colnames()

a %>%
  filter(Recording.name == "SI692IP") %>%
  group_by(Presented.Stimulus.name, Recording.name) %>%
  summarise(srsSD = sd(d),
            maxSR = max(d),
            minSR = min(d)) %>%
  filter(Recording.name == "SI692IP") %>%
  filter(str_detect(Presented.Stimulus.name, "IJA_A3_B1_D_33")) %>%
  dvisu()

a$Presented.Stimulus.name = substr(a$Presented.Stimulus.name, 1, 11)
a %>%
  group_by(Presented.Stimulus.name, Recording.name) %>%
  summarise(durationsSD = sd(time),
            maxDuration = max(time)/1000000,
            minDuration = min(time)/1000000) %>%
  arrange(minDuration) %>% dvisu()

a %>%
  group_by(Presented.Stimulus.name, Recording.name) %>%
  summarise(durationsSD = sd(d),
            maxDuration = max(d),
            minDuration = min(d)) %>%
  arrange(-maxDuration) %>%
  dvisu()

#Check sampling rate OK
#Duracao total dos vídeos (Boxplot) (duracao dos ms)
#Cortar videos que estao abaixo e acima da media
#Pegar tempo de fixação usando o método do Computer.timestamp, porém só para fixações (excluir sacadas, EyesNotFound, etc...)

a %>%
  group_by(Presented.Stimulus.name) %>%
  filter(str_detect(Presented.Stimulus.name, 'RJA_A1_B2_D')) %>%
  dvisu()

RJA_A1_B2_D
