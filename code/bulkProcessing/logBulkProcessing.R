source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(xtable)

#General info about all participants
infoParticipant = fread("/Users/pdealcan/Documents/github/sabara/details_experiment/infoParticipantes.csv")
colnames(infoParticipant) = c("Recording.name", "dataNascimento", "s", "dataJA", "dataCARS", "pontuacaoCARS", "sexo", "a")
infoParticipant$tea = unlist(lapply(infoParticipant$Recording.name, tagDiagnostico))

infoParticipant$dataNascimento <- as.Date(infoParticipant$dataNascimento, "%m/%d/%Y")
infoParticipant$dataCARS <- as.Date(infoParticipant$dataCARS, "%d/%m/%y")
infoParticipant$dataJA <- as.Date(infoParticipant$dataJA, "%d/%m/%y")

infoParticipant$ageJA <- infoParticipant$dataJA - infoParticipant$dataNascimento
infoParticipant$ageCARS <- infoParticipant$dataCARS - infoParticipant$dataNascimento
######################

## All participants ##
######################
logFile0 = infoParticipant %>% 
  group_by(sexo, tea) %>%
  summarise(nParticipants = NROW(Recording.name),
            meanAgeJA = mean(ageJA, na.rm = TRUE)/365,
            meanAgeCARS = mean(ageCARS, na.rm = TRUE)/365)
observacoes0 = c("Todos os participantes registrados, independente de conclusão do experimento ou não.")

##################################################
## All participants with Eye tracker (raw data) ##
##################################################
setwd("/Users/pdealcan/Documents/github/dataSabara/finalRawData/")
file_list = list.files()
allRaw = bind_rows(lapply(file_list, getName))
allRaw = merge(allRaw, infoParticipant, by.x = "Recording.name", by.y = "Recording.name", all.x=FALSE)

logFile1 = allRaw %>%
  group_by(sexo, tea, Recording.name) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name)),
            ageJA = unique(ageJA),
            ageCARS = unique(ageCARS)) %>%
  group_by(sexo, tea) %>%
  summarise(nTrials = sum(nTrials),
            nParticipants = length(unique(Recording.name)),
            meanAgeJA = mean(ageJA, na.rm = TRUE)/365,
            meanAgeCARS = mean(ageCARS, na.rm = TRUE)/365)

#################################################
## All participants with Eye tracker processed ##
#################################################
setwd("/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL")
processedP = list.files()
processedP = bind_rows(lapply(processedP, getName2))
processedP = merge(processedP, infoParticipant, by.x = "Recording.name", by.y = "Recording.name", all.y=FALSE)
logFile2 = processedP %>%
  group_by(sexo, tea, Recording.name) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name)),
            ageJA = unique(ageJA),
            ageCARS = unique(ageCARS)) %>%
  group_by(sexo, tea) %>%
  summarise(nTrials = sum(nTrials),
            nParticipants = length(unique(Recording.name)),
            meanAgeJA = mean(ageJA, na.rm = TRUE)/365,
            meanAgeCARS = mean(ageCARS, na.rm = TRUE)/365)

################################
## Anomalous videos durations ##
################################
masterF = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
masterF = masterF %>% filter(filterDurations == FALSE)
masterF$Recording.name = str_remove_all(masterF$Recording.name, "-2")
masterF$Recording.name = str_remove_all(masterF$Recording.name, "-3")
masterF$Presented.Stimulus.name = substr(masterF$Presented.Stimulus.name, 1, 11)
#masterF = masterF %>% select(!tea)
#masterF = merge(masterF, infoParticipant, by.x = "Recording.name", by.y = "Recording.name", all.y=FALSE)

logFile3 = masterF %>%
  group_by(sexo, tea, Recording.name, condition) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name)),
            ageJA = unique(ageJA),
            ageCARS = unique(ageCARS)) %>%
  group_by(sexo, tea, condition) %>%
  summarise(nTrials = sum(nTrials),
            nParticipants = length(unique(Recording.name)),
            meanAgeJA = mean(ageJA, na.rm = TRUE)/365,
            meanAgeCARS = mean(ageCARS, na.rm = TRUE)/365)

#################
## Cutoffs 50% ##
#################
logFile4 = masterF %>%
  filter(filterCutoffs == FALSE) %>%
  group_by(sexo, tea, Recording.name, condition) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name)),
            ageJA = unique(ageJA),
            ageCARS = unique(ageCARS)) %>%
  group_by(sexo, tea, condition) %>%
  summarise(nTrials = sum(nTrials),
            nParticipants = length(unique(Recording.name)),
            meanAgeJA = mean(ageJA, na.rm = TRUE)/365,
            meanAgeCARS = mean(ageCARS, na.rm = TRUE)/365)

######################
## Cutoffs condição ##
######################
logFile5 = masterF %>%
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  group_by(sexo, tea, Recording.name, condition) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name)),
            ageJA = unique(ageJA),
            ageCARS = unique(ageCARS)) %>%
  group_by(sexo, tea, condition) %>%
  summarise(nTrials = sum(nTrials),
            nParticipants = length(unique(Recording.name)),
            meanAgeJA = mean(ageJA, na.rm = TRUE)/365,
            meanAgeCARS = mean(ageCARS, na.rm = TRUE)/365)

logFile1$stage = "raw"
logFile2$stage = "processed"
logFile3$stage = "anomalousVideoDuration"
logFile4$stage = "cutoff50pct"
logFile5$stage = "cutoffCondition"

logs = bind_rows(logFile1, logFile2, logFile3, logFile4, logFile5)
write.csv(logs, "/Users/pdealcan/Documents/github/dataSabara/logProcessing.csv")

print(xtable(logs, type = "latex"))
