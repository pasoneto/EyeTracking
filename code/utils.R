library("dplyr")
library("data.table")
library("ggplot2")
source("/Users/pdealcan/Documents/github/doc_suomi/code/utils.R")
library("stringi")

#Computes fixation index. Input must be ordered by time
fixationIndexer = function(x){
  count = 0
  indexes = c()
  for(i in 1:(length(x))){
    if(i == length(x)){
      if(x[i] == x[i-1]){ indexes = c(indexes, count)
      } else{
        count = count+1
        indexes = c(indexes, count)
      }
      return(indexes)
    } else{
      a = x[i] == x[i+1]
      if(a == TRUE){
        indexes = c(indexes, count) 
      } else{
        indexes = c(indexes, count) 
        count = count+1
      }
    }
  }
  return(indexes)
}

#Process participant completely
processParticipant = function(dataFrame, trials, colunas){

  participantID = unique(dataFrame$Recording.name)
  data = dataFrame %>%
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
  names = str_replace(names, "1", "")
  names = gsub("\\.","",names)

  data$variable = names

  foco = stri_sub(data$Presented.Stimulus.name,-1)
  variable = stri_sub(data$variable,-1)
  data$target = foco
  data$variable = variable
  data$Recording.name = participantID

  return(data)
}


#Computa numero de alternancias ocorridas entre dois objetos
computeAlternancia = function(x, alternancia){
  count = 0
  for(i in 1:length(x)-1){
    if(all(c(x[1], x[i+1]) == alternancia) == TRUE){
      count = count + 1
    }
  }
  return(count)
}

#Computes alternancia entre obj1 e obj2
alternanciaCount = function(listFixation, obj1, obj2){
  count = 0
  for(i in 1:(length(listFixation)-1)){
    if( (listFixation[i] == obj1) && (listFixation[i+1] == obj2) ){
      count = count+1
    }
  }
  return(count)
}
