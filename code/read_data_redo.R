source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
setwd("/Users/pdealcan/Documents/github/sabara/data/allData")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})

#Selecting columns of interest by trial type
trials = c("IJA_A3_B1_D","IJA_A1_B2_E","RJA_A2_B1_E","RJA_A1_B2_D","RJA_A2_B2_E","IJA_A1_B1_D","IJA_A3_B1_E","RJA_A2_B1_D","IJA_A1_B1_E")
colunas = c()
foco = c("Rosto.", "Brinquedo.Direita.", "Brinquedo.Esquerda.")
for(i in trials){
 for(k in foco){
  colunas = c(colunas, paste("AOI.hit..", i, "...", k, sep = ""))
 }
}
addedNames = c("AOI.hit..RJA_A2_B1_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B1_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B1_E...Rosto..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B2_E...Rosto..1")
colunas = c(colunas, addedNames)

data = file_list[[1]] %>% 
  filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
  filter(Presented.Stimulus.name %in% trials) %>% #Selecionando video 
  select("Computer.timestamp", "Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", "Eye.movement.type.index", colunas) %>%
  melt(id.vars = c("Computer.timestamp", "Recording.timestamp", "Gaze.event.duration", "Presented.Stimulus.name", "Eye.movement.type", "Eye.movement.type.index"))

data %<>%
  filter(!is.na(value)) %>%
  arrange(Computer.timestamp) %>%
  ungroup() %>%
  mutate(trialIndex = fixationIndexer(Presented.Stimulus.name)) %>%
  group_by(Presented.Stimulus.name, trialIndex)


data %<>%
  #Converting time to seconds. First from microseconds, then from miliseconds
  mutate(Recording.timestamp = (Recording.timestamp - min(Recording.timestamp))/1000000,
         Gaze.event.duration = Gaze.event.duration/1000) %>%
  arrange(trialIndex, Recording.timestamp) %>%
  group_by(Presented.Stimulus.name, variable) %>%
  filter(value == 1) %>%
  arrange(trialIndex, Presented.Stimulus.name, Recording.timestamp) %>%
  ungroup() %>%
  group_by(trialIndex, Eye.movement.type.index) %>%
  group_by(Presented.Stimulus.name, trialIndex, Eye.movement.type.index) %>%
  summarise(Computer.timestamp.begin = min(Computer.timestamp),
            Computer.timestamp.end = max(Computer.timestamp),
            Recording.time.begin = min(Recording.timestamp),
            Recording.time.end = max(Recording.timestamp),
            Gaze.event.duration = unique(Gaze.event.duration),
            Presented.Stimulus.name = unique(Presented.Stimulus.name),
            Eye.movement.type = unique(Eye.movement.type),
            variable = unique(variable),
            value = unique(value))

data %<>%
  ungroup() %>%
  group_by(trialIndex, Eye.movement.type.index, Presented.Stimulus.name) %>%
  arrange(trialIndex, Recording.time.begin)


names = data$variable
subs = c("AOI.hit", trials, ".")
for(i in subs){
  names = str_replace(names, i, "")
}
names = str_replace(names, "Rosto", "R")
names = str_replace(names, "Brinquedo", "B")
names = str_replace(names, "Esquerda", "E")
names = str_replace(names, "Direita", "D")

data$variable = names
data %>%
  ggplot(aes(y = variable, x = 0, color = variable))+
    facet_wrap(~Presented.Stimulus.name+trialIndex, scale = "free")+
    geom_errorbar(aes(xmin = Recording.time.begin, xmax = Recording.time.end), width = 0, size = 3)+
    theme(legend.position = "None",
          strip.text.y = element_blank()) +
    ylab("Stimulus focused")+
    xlab("Elapsed time (ms)")
