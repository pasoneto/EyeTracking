source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(xtable)

#Notes: Sexo e sexo2? Varios NaNs

#General info about all participants
infoParticipant = fread("/Users/pdealcan/Documents/github/sabara/details_experiment/infoParticipants.csv")
colnames(infoParticipant) = c("codigo", "Recording.name", "Codinome2", "dataNascimento", "sexo", "CARS", "dataCARS", "ageCARS",     "pontuacaoCARS", "diagnostico", "Consulta", "grupo",    "JA", "dataJA",  "ageJA",     "Obs.JA",   "GeoPref",  "dataGeo",      "Obs.GeoPref",       "pontABEP", "clasABEP", "idadeMAE", "escolPAI", "escolMAE", "idadeCRIANCA",      "regiaoCRIANCA",     "regiaoResponsavel", "RENDA", "numPESSOAS", "qtdePESSOAStrabalham", "tipoMORADIA", "saudeCRIANCA", "moradia", "GEOPREF2", "IntervalDurationAverage", "IntervalDurationMedian", "IntervalDurationCount", "IntervalDurationTotal Time of Interest Duration", "IntervalDurationTotal Recording Duration", "TotalFixDurationEllipse", "TotalFixDurationNon-social", "TotalFixDurationSocial", "TotalFixDurationAverage", "TotalFixDurationMedian", "TotalFixDurationSum", "TotalFixDurationTotal Time of Interest Duration", "TotalFixDurationTotal Recording Duration", "AvgFixationDurEllipse", "AvgFixationDurNon-social", "AvgFixationDurSocial", "AvgFixationDurAverage", "AvgFixationDurMedian", "AvgFixationDurTotal Time of Interest Duration", "AvgFixationDurTotal Recording Duration")

infoParticipant$timeBetweenJAandCARS <- abs(infoParticipant$ageCARS - infoParticipant$ageJA)

infoParticipant$tea = unlist(lapply(infoParticipant$grupo, tagDiagnostico))

#Processed
#	FS76IP	2022-09-26	2021-11-03
#Original
#	FS76IP	CARS - 03/11/21 (dmy)	JA - 9/26/22 (mdy)
#MR280IP
######################

## All participants ##
######################
logFile0 = infoParticipant %>% 
  group_by(sexo, tea) %>%
  filter(!is.na(tea)) %>%
  summarise(nParticipants = NROW(Recording.name))

#Participantes que fizeram JA
infoParticipant = infoParticipant %>%
  filter(!dataJA %in% c("Transferido", "-", "Faltou", "nova", "NAN", "atestado", "remoto", "nan", "N\xe3o fez", "0"))

##################################################
## All participants with Eye tracker (raw data) ##
##################################################
setwd("/Users/pdealcan/Documents/github/dataSabara/finalRawData/")
file_list = list.files()
allRaw = bind_rows(lapply(file_list, getName))
allRaw = merge(allRaw, infoParticipant, by.x = "Recording.name", by.y = "Recording.name", all.x=FALSE)

#Verifying missing participants
allRaw %>%
  filter(!Recording.name %in% infoParticipant$Recording.name) %>%
  distinct(Recording.name, .keep_all = TRUE)

infoParticipant %>%
  filter(JA == 1) %>%
  filter(!Recording.name %in% allRaw$Recording.name) %>% distinct(Recording.name, .keep_all = TRUE) %>%
  select(Recording.name)

logFile1 = allRaw %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(!is.na(tea)) %>%
  group_by(sexo, tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

################################
## Anomalous videos durations ##
################################
masterF = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

masterF$Presented.Stimulus.name = substr(masterF$Presented.Stimulus.name, 1, 11)

logFile2 = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(!is.na(tea)) %>%
  filter(filterDurations == FALSE) %>%
  group_by(sexo, tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

#################
## Cutoffs 50% ##
#################
logFile3 = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(!is.na(tea)) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  group_by(sexo, tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

######################
## Cutoffs condição ##
######################
logFile4 = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  distinct(Recording.name, .keep_all = TRUE) %>%
  filter(!is.na(tea)) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  group_by(sexo, tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

logFile0$stage = "1. raw"
logFile1$stage = "2. valid JA data"
logFile2$stage = "3. anomalousVideoDuration"
logFile3$stage = "4. cutoff50pct"
logFile4$stage = "5. cutoffCondition"

logs = bind_rows(logFile0, logFile1, logFile2, logFile3, logFile4)
write.csv(logs, "/Users/pdealcan/Documents/github/dataSabara/logProcessing.csv")

print("raw")
print(xtable(logFile0, type = "latex"))
print("valid JA data")
print(xtable(logFile1, type = "latex"))
print("anomalous video duration")
print(xtable(logFile2, type = "latex"))
print("cut off 50%")
print(xtable(logFile3, type = "latex"))
print("cut off condition")
print(xtable(logFile4, type = "latex"))

#Age analysis
masterF = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
masterF$Presented.Stimulus.name = substr(masterF$Presented.Stimulus.name, 1, 11)

ageSummary = masterF %>% 
  summarise(sdAgeJA = sd(ageJA, na.rm = TRUE),
            sdAgeCARS = sd(ageCARS, na.rm = TRUE),
            meanAgeJA = mean(ageJA, na.rm = TRUE),
            maxAge = max(ageJA, na.rm = TRUE),
            meanAgeCARS = mean(ageCARS, na.rm = TRUE),
            meanTimeJAandCARS = mean(timeBetweenJAandCARS, na.rm = TRUE),
            sdTimeJAandCARS = sd(timeBetweenJAandCARS, na.rm = TRUE),
            maxTimeJAandCARS = max(timeBetweenJAandCARS, na.rm = TRUE),
            minTimeJAandCARS = min(timeBetweenJAandCARS, na.rm = TRUE)) %>%
t()
