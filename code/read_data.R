source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
setwd("/Users/pdealcan/Documents/github/sabara/data/samplesTest")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, 
                   function(i){
                     read.table(file = i, sep = '\t', header = TRUE)
                    }
                  )

um     = file_list[[1]]
dois   = file_list[[2]]
tres   = file_list[[3]]
quatro = file_list[[4]]
cinco  = file_list[[5]]

trials = c("IJA_A3_B1_D","IJA_A1_B2_E","RJA_A2_B1_E","RJA_A1_B2_D","RJA_A2_B2_E","IJA_A1_B1_D","IJA_A3_B1_E","RJA_A2_B1_D","IJA_A1_B1_E")

colunas = c()
foco = c("Rosto.", "Brinquedo.Direita.", "Brinquedo.Esquerda.")
for(i in trials){
 for(k in foco){
  colunas = c(colunas, paste("AOI.hit..", i, "...", k, sep = ""))
 }
}

#Baseline
um %<>%
  filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
  filter(Presented.Stimulus.name %in% trials) %>% #Selecionando video 
  select("Computer.timestamp", "Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", colunas) %>%
  melt(id.vars = c("Computer.timestamp", "Recording.timestamp", "Gaze.event.duration", "Presented.Stimulus.name", "Eye.movement.type")) %>%
  filter(!is.na(value)) %>%
  arrange(Computer.timestamp) %>%
  ungroup() %>%
  mutate(trialIndex = fixationIndexer(Presented.Stimulus.name)) %>%
  group_by(Presented.Stimulus.name, trialIndex) %>%
  mutate(Recording.timestamp = Recording.timestamp - min(Recording.timestamp)) %>%
  arrange(variable, Recording.timestamp, Computer.timestamp)

#Calculate trial index!
um %<>%
  group_by(Presented.Stimulus.name, variable) %>%
  mutate(fixationIndex = fixationIndexer(value)) %>%
  filter(value == 1) %>%
  arrange(Presented.Stimulus.name, Recording.timestamp) %>%
  ungroup() %>%
  group_by(fixationIndex, trialIndex, variable) %>%
  summarise(Presented.Stimulus.name = unique(Presented.Stimulus.name),
            variable = unique(variable),
            eventStart = min(Recording.timestamp)/1000,
            eventEnd = (max(Recording.timestamp)+8)/1000,
            fixationDuration = sum(Gaze.event.duration)) %>%
  ungroup() %>%
  arrange(Presented.Stimulus.name, eventStart, trialIndex)

  #group_by(Presented.Stimulus.name) %>%
  #mutate(TimeTonextFixation = nextFixationCalc(eventEnd, eventStart))
names = um$variable
subs = c("AOI.hit", trials, ".")
for(i in subs){
  names = str_replace(names, i, "")
}
names = str_replace(names, "Rosto", "R")
names = str_replace(names, "Brinquedo", "B")
names = str_replace(names, "Esquerda", "E")
names = str_replace(names, "Direita", "D")
um$variable = names

um %>%
  ggplot(aes(y = variable, x = 0, color = variable))+
    facet_wrap(~Presented.Stimulus.name+trialIndex, scale = "free")+
    geom_errorbar(aes(xmin = eventStart, xmax = eventEnd), width = 0, size = 3)+
    theme(legend.position = "None",
          strip.text.y = element_blank()) +
    ylab("Stimulus focused")+
    xlab("Elapsed time (ms)")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/report2/graph_visu5.png", width = 30, height = 20, units = "cm")


