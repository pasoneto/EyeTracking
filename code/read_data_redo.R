source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
setwd("/Users/pdealcan/Documents/github/sabara/data/final/")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
path_out = "/Users/pdealcan/Documents/github/sabara/reports/report5/"

file_list[[1]] %>% colnames()

#Selecting columns of interest by trial type
trials = c("IJA_A3_B1_D","IJA_A1_B2_E","RJA_A2_B1_E","RJA_A1_B2_D","RJA_A2_B2_E","IJA_A1_B1_D","IJA_A3_B1_E","RJA_A2_B1_D","IJA_A1_B1_E")
colunas = c()
foco = c("Rosto.", "Brinquedo.Direita.", "Brinquedo.Esquerda.")
for(i in trials){
 for(k in foco){
  colunas = c(colunas, paste("AOI.hit..", i, "...", k, sep = ""))
 }
}

#Filtering, computing trial index
data = file_list[[1]] %>%
  filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
  filter(Presented.Stimulus.name %in% trials) %>% #Selecionando video 
  select("Computer.timestamp", "Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", colunas) %>%
  melt(id.vars = c("Computer.timestamp", "Recording.timestamp", "Gaze.event.duration", "Presented.Stimulus.name", "Eye.movement.type")) %>%
  filter(!is.na(value)) %>%
  arrange(Computer.timestamp) %>%
  ungroup() %>%
  mutate(trialIndex = fixationIndexer(Presented.Stimulus.name)) %>%
  group_by(Presented.Stimulus.name, trialIndex) %>%

  #Converting time to seconds. First from microseconds, then from miliseconds
  mutate(Recording.timestamp = (Recording.timestamp - min(Recording.timestamp))/1000000,
         Gaze.event.duration = Gaze.event.duration/1000) %>%
  arrange(trialIndex, Recording.timestamp) %>%
  group_by(Presented.Stimulus.name, variable) %>%
  filter(value == 1) %>%
  arrange(trialIndex, Presented.Stimulus.name, Recording.timestamp) %>%
  group_by(Presented.Stimulus.name, trialIndex) %>%
  mutate(fixationIndex = fixationIndexer(Gaze.event.duration)) %>% #Gets fixation index by Gaze event because they are always the same for a single fixatio
  ungroup() %>%
  group_by(trialIndex, fixationIndex, Presented.Stimulus.name) %>%
  summarise(Presented.Stimulus.name = unique(Presented.Stimulus.name),
            variable = unique(variable),
            eventStart = min(Recording.timestamp),
            eventEnd = eventStart+unique(Gaze.event.duration)) %>%
  ungroup() %>%
  arrange(trialIndex, eventStart)

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
      geom_errorbar(aes(xmin = eventStart, xmax = eventEnd), width = 0, size = 3)+
      theme(legend.position = "None",
            strip.text.y = element_blank()) +
      ylab("Stimulus focused")+
      xlab("Elapsed time (ms)")
