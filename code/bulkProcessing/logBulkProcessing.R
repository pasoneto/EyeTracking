source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)

#Número inicial de participantes
setwd("/Users/pdealcan/Documents/github/dataSabara/finalRawData/")
file_list = list.files()
getName = function(dataFrame){
  a = readAndRename(dataFrame)
  return(unique(a$Recording.name))
}
files = unlist(lapply(file_list, getName))

#N total de participantes. Sem repetições
namesPresent = unique(files)
length(namesPresent)

nDiagnosticos = unlist(lapply(namesPresent, tagDiagnostico))
nDiagnosticos = table(nDiagnosticos)

logFiles = data.frame(etapa = c("Inicio"),
           total = c(length(namesPresent)),
           td = c(nDiagnosticos[[1]]),
           tea = c(nDiagnosticos[[2]])
)

observacoes1 = c(paste("Recebi ", length(files), " files, com nomes repetidos, e participantes sem dados. Mantive apenas um caso para cada participante", sep=""))

#Pré processamento
setwd("/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL")
files = list.files()
files = unlist(lapply(files, getName))

nDiagnosticos = unlist(lapply(files, tagDiagnostico))
nDiagnosticos = table(nDiagnosticos)

logFiles2 = data.frame(etapa = c("Pré processamento - Participantes sem dados"),
            total = c(length(files)),
            td = c(nDiagnosticos[[1]]),
            tea = c(nDiagnosticos[[2]])
)

observacoes2 = c("Excluídos participantes sem dados. Sem fixações, sacadas, etc...")

#Cutoffs
source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsVideoDuration.R")
setwd("/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL")

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)
a = bind_rows(file_list)

#N trials without filter
a %>%
  group_by(Recording.name) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name))) %>%
  select(nTrials) %>%
  ungroup() %>%
  summarise(nTotal = sum(nTrials))

#N trials / participants after filter
a = a %>% mutate(trialsToFilter = paste(Presented.Stimulus.name, Recording.name, sep = ""))

b = a %>%
  filter(!trialsToFilter %in% filterOutDurations$filterOutDurations) %>%
  group_by(Recording.name) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name))) %>%
  select(Recording.name, nTrials) %>%
  ungroup()
b$tea = unlist(lapply(b$Recording.name, tagDiagnostico))
b %>%
  group_by(tea) %>%
  summarise(nTotal = sum(nTrials))

files = unique(b$Recording.name)
nDiagnosticos = unlist(lapply(files, tagDiagnostico))
nDiagnosticos = table(nDiagnosticos)

logFiles3 = data.frame(etapa = c("Pré processamento - Duração anômala dos vídeos"),
            total = c(length(files)),
            td = c(nDiagnosticos[[1]]),
            tea = c(nDiagnosticos[[2]])
)

observacoes3 = c("Excluídas trials com duração outlier. Sem exclusão, 6904 trials. Com exclusão, 6660 trials de participantes TD, e 217 TEA")

#Cutoff proporção de 50%
source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsProportion.R")
b = a %>%
  mutate(trialsToFilter = paste(Presented.Stimulus.name, Recording.name, sep = "")) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  filter(!trialsToFilter %in% filterOutDurations$filterOutDurations) %>% #anomalous videos duration
  filter(!trialsToFilter %in% filterOutNames05$filterOutTrials) #0.5 cutoff

files = unique(b$Recording.name)
nDiagnosticos = unlist(lapply(files, tagDiagnostico))
nDiagnosticos = table(nDiagnosticos)

c = b %>%
  group_by(Recording.name) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name))) %>%
  select(Recording.name, nTrials) %>%
  ungroup()
c$tea = unlist(lapply(c$Recording.name, tagDiagnostico))
c %>%
  group_by(tea) %>%
  summarise(nTotal = sum(nTrials))

logFiles4 = data.frame(etapa = c("Cutoffs - 50% do tempo de fixação"),
            total = c(length(files)),
            td = c(nDiagnosticos[[1]]),
            tea = c(nDiagnosticos[[2]])
)

observacoes4 = c("Excluídos participantes que não olharam pelo menos 50% do tempo do vídeo para a tela. Restam 73 trials de TEA, e 2978 de TD")

#Cutoff um trial por condição
b = b %>% 
  mutate(condition = substr(Presented.Stimulus.name, 1, 3)) %>%
  filter(!str_detect(condition, 'BL_')) %>%
  group_by(Recording.name) %>%
  mutate(nConditions = length(unique(condition))) %>%
  filter(nConditions == 2)

files = unique(b$Recording.name)
nDiagnosticos = unlist(lapply(files, tagDiagnostico))
nDiagnosticos = table(nDiagnosticos)

c = b %>%
  group_by(Recording.name) %>%
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  summarise(nTrials = length(unique(Presented.Stimulus.name))) %>%
  select(Recording.name, nTrials) %>%
  ungroup()
c$tea = unlist(lapply(c$Recording.name, tagDiagnostico))
c %>%
  group_by(tea) %>%
  summarise(nTotal = sum(nTrials))

logFiles5 = data.frame(etapa = c("Cutoffs - 1 trial por condição"),
            total = c(length(files)),
            td = c(nDiagnosticos[[1]]),
            tea = c(nDiagnosticos[[2]])
)

observacoes5 = c("Excluídos participantes que não têm ao menos 1 trial em cada condição. 2790 trials para TD, e 66 para TEA")

#Merging everything
finalLog = bind_rows(logFiles, logFiles2, logFiles3, logFiles4, logFiles5)
print(xtable(finalLog))

print(observacoes1)
print(observacoes2)
print(observacoes3)
print(observacoes4)
print(observacoes5)
