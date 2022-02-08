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
cinco %<>%
  filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
  filter(Presented.Stimulus.name %in% trials) %>% #Selecionando video 
  select("Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", colunas) %>%
  melt(id.vars = c("Recording.timestamp", "Gaze.event.duration", "Presented.Stimulus.name", "Eye.movement.type")) %>%
  group_by(Presented.Stimulus.name) %>%
  mutate(Recording.timestamp = Recording.timestamp - min(Recording.timestamp))

cinco %<>%
  arrange(Presented.Stimulus.name, Recording.timestamp) %>%
  group_by(Presented.Stimulus.name, Recording.timestamp) %>%
  filter(value == 1) %>%
  ungroup() %>% 
  group_by(Presented.Stimulus.name) %>%
  mutate(fixationIndex = fixationIndexer(variable)) %>%
  group_by(Presented.Stimulus.name, fixationIndex) %>%
  summarise(Presented.Stimulus.name = unique(Presented.Stimulus.name),
            variable = unique(variable),
            eventStart = min(Recording.timestamp)/1000,
            eventEnd = (max(Recording.timestamp)+8)/1000,
            fixationDuration = sum(Gaze.event.duration))
  #group_by(Presented.Stimulus.name) %>%
  #mutate(TimeTonextFixation = nextFixationCalc(eventEnd, eventStart))

names = cinco$variable
subs = c("AOI.hit", trials, ".")
for(i in subs){
  names = str_replace(names, i, "")
}
cinco$variable = names

cinco %>%
  ggplot(aes(y = variable, x = 0, color = variable))+
    facet_wrap(~Presented.Stimulus.name, scale = "free")+
    geom_errorbar(aes(xmin =eventStart, xmax = eventEnd), width = 0, size = 3)+
    theme(legend.position = "None",
          strip.text.y = element_blank()) +
    ylab("Stimulus focused")+
    xlab("Elapsed time (ms)")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/report2/graph_visu5.png", width = 30, height = 20, units = "cm")


