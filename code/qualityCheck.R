source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
setwd("/Users/pdealcan/Documents/github/sabara/data/allData")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
unique(file_list[[1]]$Presented.Stimulus.name)
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

eyesFound = function(x){
  type = c()
  for(i in 1:length(x)){
    if(x[i] == 'EyesNotFound'){
     type = c(type, "EyesNotFound")
    } else {
     type = c(type, "EyesFound")
    }
  }
  return(type)
}

ratioValid = function(df){
  data = df %>% 
    filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
    filter(Presented.Stimulus.name != "") %>% ##Removendo partes de calibração
    filter(Presented.Stimulus.name %in% trials) %>% #Selecionando video 
    select("Computer.timestamp", "Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", "Eye.movement.type.index", colunas) %>%
    arrange(Computer.timestamp) %>%
    ungroup() %>%
    mutate(trialIndex = fixationIndexer(Presented.Stimulus.name))

  data %<>%
    group_by(Presented.Stimulus.name, trialIndex, Eye.movement.type.index) %>%
    summarise(Computer.timestamp.begin = min(Computer.timestamp),
              Computer.timestamp.end = max(Computer.timestamp),
              Recording.time.begin = min(Recording.timestamp),
              Recording.time.end = max(Recording.timestamp),
              Gaze.event.duration = unique(Gaze.event.duration),
              Presented.Stimulus.name = unique(Presented.Stimulus.name),
              Eye.movement.type = unique(Eye.movement.type)) %>%
    mutate(Eye.movement.type2 = eyesFound(Eye.movement.type)) %>%
    group_by(Eye.movement.type2) %>%
    select(Eye.movement.type, Eye.movement.type2, Gaze.event.duration) %>%
    summarise(duration = sum(Gaze.event.duration))

    data = data.frame(t(data))
    colnames(data) = data[1, ]
    data = data[2, ]
    data %<>% mutate_if(is.character, as.numeric)
    return(data)
}

ratioValid(file_list[[1]])

final = c()
for(i in i+1:length(file_list)){
  final = bind_rows(final, ratioValid(file_list[[i]]))
}

final %>%
  ggplot(aes(x = 0, y = (EyesFound/(EyesNotFound+EyesFound))*100))+
    geom_violin()+
    geom_boxplot(width= 0.5)+
    theme(legend.position = "None",
          strip.text.y = element_blank())+
    ylab("Porcentagem de dados válidos")+
    xlab("Density")

ggsave("./qualityCheckFigure2.png")

min(final$ratio)

final %<>%
  mutate(ratio = (EyesFound/(EyesNotFound+EyesFound))*100)

final %>%
  summarise(media = mean(ratio),
            min = min(ratio),
            max = max(ratio),
            sd = sd(ratio)) %>%
  write.csv("./targetAndBaseline.csv")

