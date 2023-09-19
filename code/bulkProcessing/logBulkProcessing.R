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

#First
first = infoParticipant %>%
  group_by(tea) %>%
  summarise(length(unique(Recording.name)))
first$stage = "planilha raw"

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
  filter(!Recording.name %in% allRaw$Recording.name) %>% 
  distinct(Recording.name, .keep_all = TRUE) %>%
  select(Recording.name)

infoParticipant %>%
  filter(!Recording.name %in% c("FS12IP", "MR313IP", "MR324IP", "CR397IP", "SI421IP", "CR701IP")) %>%
  group_by(tea) %>%
  summarise(length(unique(Recording.name)))

logFile1 = allRaw %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(!is.na(tea)) %>%
  group_by(sexo, tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

#Second
second = allRaw %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(JA == 1) %>%
  group_by(tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

second$stage = "existing JA data"
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

#Third
third = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  group_by(tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

third$stage = "at least one valid trial (No BaseLine)"

#################
## Cutoffs 50% ##
#################
logFile3 = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  group_by(tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

#Fourth
fourth = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(filterDurations == FALSE) %>%
  group_by(tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

fourth$stage = "filter: anomalous duration"
  
#Fifth
fifth = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  group_by(tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))
  
fifth$stage = "filter: anomalus dur + cutoff50%"

#Sixth
sixth = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  group_by(tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

sixth$stage = "filter: anomalous dur + cutoffs + both conditions"

######################
## Cutoffs condição ##
######################
logFile4 = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  select(Recording.name, Gaze.event.duration) %>%
  filter(!is.na(tea)) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  group_by(tea) %>%
  summarise(nParticipants = length(unique(Recording.name)))

filtered = masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(!is.na(tea)) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE)

namesIn = unique(filtered$Recording.name)
infoParticipant$Recording.name

namesIn2 = c()
for(k in infoParticipant$Codinome){
  if(k %in% namesIn){
    namesIn2 = c(namesIn2, TRUE)
  } else {
    namesIn2 = c(namesIn2, FALSE)
  }
}

infoParticipant$finalSample = namesIn2
write.csv(infoParticipant, "/Users/pdealcan/Downloads/infoParticipants.csv")

masterF %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  distinct(Recording.name, .keep_all = TRUE) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  #filter(filterConditions == FALSE) %>%
  group_by(tea) %>%
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

ageSummaryPreFilters = masterF %>% 
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  distinct(Recording.name, .keep_all = TRUE) %>%
  summarise(sdAgeJA = sd(ageJA, na.rm = TRUE)/12,
            sdAgeCARS = sd(ageCARS, na.rm = TRUE)/12,
            meanAgeJA = mean(ageJA, na.rm = TRUE)/12,
            maxAgeJA = max(ageJA, na.rm = TRUE)/12,
            meanAgeCARS = mean(ageCARS, na.rm = TRUE)/12,
            maxAgeCARS = max(ageJA, na.rm = TRUE)/12,
            meanTimeJAandCARS = mean(timeBetweenJAandCARS, na.rm = TRUE)/12,
            sdTimeJAandCARS = sd(timeBetweenJAandCARS, na.rm = TRUE)/12,
            maxTimeJAandCARS = max(timeBetweenJAandCARS, na.rm = TRUE)/12,
            minTimeJAandCARS = min(timeBetweenJAandCARS, na.rm = TRUE)/12)

ageSummaryPostFilters = masterF %>% 
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  distinct(Recording.name, .keep_all = TRUE) %>%
  filter(!is.na(tea)) %>%
  filter(filterDurations == FALSE) %>%
  filter(filterCutoffs == FALSE) %>%
  filter(filterConditions == FALSE) %>%
  summarise(sdAgeJA = sd(ageJA, na.rm = TRUE)/12,
            sdAgeCARS = sd(ageCARS, na.rm = TRUE)/12,
            meanAgeJA = mean(ageJA, na.rm = TRUE)/12,
            maxAgeJA = max(ageJA, na.rm = TRUE)/12,
            meanAgeCARS = mean(ageCARS, na.rm = TRUE)/12,
            maxAgeCARS = max(ageJA, na.rm = TRUE)/12,
            meanTimeJAandCARS = mean(timeBetweenJAandCARS, na.rm = TRUE)/12,
            sdTimeJAandCARS = sd(timeBetweenJAandCARS, na.rm = TRUE)/12,
            maxTimeJAandCARS = max(timeBetweenJAandCARS, na.rm = TRUE)/12,
            minTimeJAandCARS = min(timeBetweenJAandCARS, na.rm = TRUE)/12)


ageSummary = bind_rows(ageSummaryPreFilters, ageSummaryPostFilters) %>% t()

print(xtable(ageSummary, type = "latex"))

print(xtable(first, type = "latex"))
print(xtable(second, type = "latex"))
print(xtable(third, type = "latex"))
print(xtable(fourth, type = "latex"))
print(xtable(fifth, type = "latex"))
print(xtable(sixth, type = "latex"))

