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

#Identifies which case is only focusing on Fundo
fundoUniquer = function(fundo, esquerda, direita, rosto){
  if(is.na(esquerda)){esquerdaNA = 0}else{esquerdaNA = esquerda}
  if(is.na(rosto)){rostoNA = 0}else{rostoNA = rosto}
  if(is.na(direita)){direitaNA = 0}else{direitaNA = direita}
  if(is.na(fundo)){fundoNA = 0}else{fundoNA = fundo}
  soma = esquerdaNA + direitaNA + rostoNA
  if(soma != 0){
    return(0)
  } else {
    return(fundo)
  }
}

#Applies 0 to fundo when it is also focused on rosto, esquerda ou direita
detatchFundo = function(file, directoryOut){
  print(paste("Started file ", sep=""))
  trials = c("IJA_A3_B1_D","IJA_A1_B2_E","RJA_A2_B1_E","RJA_A1_B2_D","RJA_A2_B2_E","IJA_A1_B1_D","IJA_A3_B1_E","RJA_A2_B1_D","IJA_A1_B1_E")
  addedBaseline = c("BL_LC_A1_B2", "BL_S_A1_B2", "BL_LC_A3_B1", "BL_S_A3_B1", "BL_LC_A2_B1", "BL_S_A2_B1", "BL_LC_A2_B2", "BL_S_A2_B2", "BL_LC_A1_B1", "BL_S_A1_B1")
  trialsFundo = c(trials, addedBaseline)

  for(i in 1:length(trialsFundo)){
    fundo = file %>% select(contains(trialsFundo[[i]])) %>% select(contains("Fundo"))
    esquerda = file %>% select(contains(trialsFundo[[i]])) %>% select(contains("Esquerda"))
    direita = file %>% select(contains(trialsFundo[[i]])) %>% select(contains("Direita"))
    rosto = file %>% select(contains(trialsFundo[[i]])) %>% select(contains("rosto"))
    newFundo = c()
    for(k in 1:length(fundo[[1]])){
      newFundo = c(newFundo, fundoUniquer(fundo[[1]][k], esquerda[[1]][k], direita[[1]][k], rosto[[1]][k]))
    }
    colnameToChange = file %>% select(contains(trialsFundo[[i]])) %>% select(contains("Fundo")) %>% colnames()
    file[colnameToChange] = newFundo
  }
  nameFile = unique(file$Recording.name)
  write.csv(file, paste(directoryOut, nameFile, ".csv", sep = ""))
  print(paste("Done with", nameFile, sep=" "))
  return(file)
}

#Process participant completely
processParticipant = function(dataFrame, trials, colunas){
  
  participantID = unique(dataFrame$Recording.name)
  nomeColunas = colnames(dataFrame)
  nomeColunas = str_replace_all(nomeColunas, "..mm.", "")  
  nomeColunas = str_replace_all(nomeColunas, "..μs.", "")  
  nomeColunas = str_replace_all(nomeColunas, "..ms.", "")
  nomeColunas = str_replace_all(nomeColunas, ".novo", "")

  colnames(dataFrame) = nomeColunas

  data = dataFrame %>%
    filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
    select(any_of(c("Pupil.diameter.left", "Pupil.diameter.right", "Computer.timestamp", "Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", "Eye.movement.type.index", colunas)))

  data %<>%
    melt(id.vars = c("Pupil.diameter.left", "Pupil.diameter.right", "Computer.timestamp", "Recording.timestamp", "Gaze.event.duration", "Presented.Stimulus.name", "Eye.movement.type", "Eye.movement.type.index"))

  data %<>%
    filter(!is.na(value)) %>%
    arrange(Computer.timestamp) %>%
    ungroup() %>%
    group_by(Presented.Stimulus.name)

  data %<>%
    #Converting time to seconds. First from microseconds, then from miliseconds
    mutate(Recording.timestamp = (Recording.timestamp - min(Recording.timestamp))/1000000,
           Gaze.event.duration = Gaze.event.duration/1000) %>%
    arrange(Recording.timestamp) %>%
    group_by(Presented.Stimulus.name, variable) %>%
    filter(value == 1)
  
  data$Pupil.diameter.right = as.numeric(gsub(",", ".", data$Pupil.diameter.right)) 
  data$Pupil.diameter.left = as.numeric(gsub(",", ".", data$Pupil.diameter.left)) 
  
  print(data$Pupil.diameter.right)
  print(data$Pupil.diameter.left)

  if(nrow(data) > 1){
    data %<>%
      arrange(Presented.Stimulus.name, Recording.timestamp) %>%
      ungroup() %>%
      group_by(Presented.Stimulus.name, Eye.movement.type.index) %>%
      summarise(Computer.timestamp.begin = min(Computer.timestamp),
                Computer.timestamp.end = max(Computer.timestamp),
                Recording.time.begin = min(Recording.timestamp),
                Recording.time.end = max(Recording.timestamp),
                Gaze.event.duration = unique(Gaze.event.duration),
                Presented.Stimulus.name = unique(Presented.Stimulus.name),
                Eye.movement.type = unique(Eye.movement.type),
                variable = unique(variable),
                value = unique(value),
                pupil.right = mean(Pupil.diameter.right, na.rm = TRUE),
                pupil.left = mean(Pupil.diameter.left, na.rm = TRUE))

    data %<>%
      ungroup() %>%
      group_by(Eye.movement.type.index, Presented.Stimulus.name) %>%
      arrange(Recording.time.begin)

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

    foco = sapply(str_split(data$Presented.Stimulus.name, "_"), `[`, 4)
    variable = stri_sub(data$variable,-1)
    data$target = foco
    data$variable = variable
    data$Recording.name = participantID
    data$variable = str_replace(data$variable, "o", "F")

    data = data %>% arrange(Presented.Stimulus.name, Computer.timestamp.begin)

    data = data %>%
      filter(!is.na(target))

    data$target = str_replace_all(data$target, " \\(1\\)", "")
    data$target = str_replace_all(data$target, " \\(novo\\)", "")
    data$Presented.Stimulus.name = str_replace_all(data$Presented.Stimulus.name, " \\(1\\)", "")
    data$Presented.Stimulus.name = str_replace_all(data$Presented.Stimulus.name, " \\(novo\\)", "")

    data$Gaze.event.duration = data$Recording.time.end - data$Recording.time.begin

    return(data)
  } else {
    return(NULL)
  }
}


#Process participant completely
processPupil = function(dataFrame, trials, colunas){

  participantID = unique(dataFrame$Recording.name)
  data = dataFrame %>%
    filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
    filter(Presented.Stimulus.name %in% trials) %>% #Selecionando video 
    select("Pupil.diameter.left", "Pupil.diameter.right", "Computer.timestamp", "Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", "Eye.movement.type.index", colunas) %>%
    melt(id.vars = c("Pupil.diameter.left", "Pupil.diameter.right", "Computer.timestamp", "Recording.timestamp", "Gaze.event.duration", "Presented.Stimulus.name", "Eye.movement.type", "Eye.movement.type.index"))

  data %<>%
    filter(!is.na(value)) %>%
    arrange(Computer.timestamp) %>%
    ungroup() %>%
    mutate(trialIndex = fixationIndexer(Presented.Stimulus.name)) %>%
    group_by(Presented.Stimulus.name)

  data %<>%
    #Converting time to seconds. First from microseconds, then from miliseconds
    mutate(Recording.timestamp = (Recording.timestamp - min(Recording.timestamp))/1000000,
           Gaze.event.duration = Gaze.event.duration/1000) %>%
    arrange(trialIndex, Recording.timestamp) %>%
    group_by(Presented.Stimulus.name, variable) %>%
    filter(value == 1) %>%
    arrange(trialIndex, Presented.Stimulus.name, Recording.timestamp) %>%
    ungroup() %>%
    group_by(Presented.Stimulus.name, trialIndex, Eye.movement.type.index)

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

  data = data %>%
    select(Pupil.diameter.left, Pupil.diameter.right, Recording.timestamp, Presented.Stimulus.name, trialIndex, variable, target, Recording.name)

  #Mean and normalization of pupil diameter
  data$pupil = rowMeans(data[, c("Pupil.diameter.left", "Pupil.diameter.right")], na.rm = TRUE)

  data = data %>%
    ungroup() %>%
    group_by(Recording.name, Presented.Stimulus.name) %>%
    mutate(pupil = (pupil - mean(pupil, na.rm = TRUE))/sd(pupil, na.rm = TRUE))

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

#Selecting columns of interest by trial type
trials = c("IJA_A3_B1_D","IJA_A1_B2_E","RJA_A2_B1_E","RJA_A1_B2_D","RJA_A2_B2_E","IJA_A1_B1_D","IJA_A3_B1_E","RJA_A2_B1_D","IJA_A1_B1_E")
addedBaseline = c("BL_LC_A1_B2", "BL_S_A1_B2", "BL_LC_A3_B1", "BL_S_A3_B1", "BL_LC_A2_B1", "BL_S_A2_B1", "BL_LC_A2_B2", "BL_S_A2_B2", "BL_LC_A1_B1", "BL_S_A1_B1")
trials = c(trials, addedBaseline)
colunas = c()
foco = c("Rosto.", "Brinquedo.Direita.", "Brinquedo.Esquerda.", "Fundo.")
for(i in trials){
 for(k in foco){
  colunas = c(colunas, paste("AOI.hit..", i, "...", k, sep = ""))
 }
}

fundos = c("AOI.hit..IJA_A3_B1_D...Fundo.", "AOI.hit..IJA_A1_B2_E...Fundo.", "AOI.hit..RJA_A2_B1_E...Fundo.", "AOI.hit..RJA_A1_B2_D...Fundo.", "AOI.hit..IJA_A1_B1_D...Fundo.", "AOI.hit..IJA_A3_B1_E...Fundo.", "AOI.hit..RJA_A2_B1_D...Fundo.", "AOI.hit..IJA_A1_B1_E...Fundo.")
addedNames = c("Fixation.point.X", "AOI.hit..RJA_A2_B1_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B1_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B1_E...Rosto..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B2_E...Rosto..1", "AOI.hit..RJA_A2_B1_E...Fundo..1")
colunasWithFix = c(colunas, addedNames, fundos)
colunasWithoutFix = c(colunas, fundos)

#Participantes com diagnostico tea
tagDiagnostico = function(x){
  diagnostico = c("FS9IP", "FS24IP", "FS65IP", "FS76IP", "FS93IP", "RP100IP", "SM114IP", "SM118IP", "MR135IP", "MR136IP", "MR140IP", "SF142IP", "SF234IP", "SF246IP", "MR281IP", "MR285IP", "MR299IP", "CR348IP", "CR356IP", "RP373IP", "SI429IP", "SI430IP", "SM462IP", "MP466IP", "CR475IP", "MR534IP", "CR559IP", "SM581IP", "MR589IP", "SI619IP", "RP657IP", "CR683IP", "MR688IP", "MR691IP", "SF728IP", "SI776IP", "SM787IP")
  if(x %in% diagnostico){
    return("true")
  } else {
    return("false")
  }
}

#Computes total time of each video per participant
totalTime = function(x){
  if(NROW(x)>2){
    df = x %>%
      group_by(Presented.Stimulus.name, trialIndex) %>%
      summarise(Recording.name = unique(Recording.name),
                totalTime = (max(Recording.timestamp) - min(Recording.timestamp))/1000000)
    return(df)
  }
}

#read excel files and rename problematic columns
readAndRename = function(x){
  #a = read_excel(x)
  a = fread(x)
  colNames = a %>% colnames()
  colNames = lapply(colNames, function(i){str_replace_all(i, "[ -]", ".")} )
  colNames = lapply(colNames, function(i){chartr("[]", "..", i)})
  colnames(a) = unlist(colNames)
  return(a)
}


#Process participant completely
processParticipantTime = function(dataFrame, trials, colunas){
  
  participantID = unique(dataFrame$Recording.name)
  data = dataFrame %>%
    filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
    filter(Presented.Stimulus.name %in% trials) %>% #Selecionando video 
    select("Computer.timestamp", "Presented.Stimulus.name", "Eye.movement.type", "Gaze.event.duration", "Recording.timestamp", "Eye.movement.type.index", colunas)

  data %<>%
    melt(id.vars = c("Computer.timestamp", "Recording.timestamp", "Gaze.event.duration", "Presented.Stimulus.name", "Eye.movement.type", "Eye.movement.type.index"))

  data %<>%
    filter(!is.na(value)) %>%
    arrange(Computer.timestamp) %>%
    ungroup() %>%
    mutate(trialIndex = fixationIndexer(Presented.Stimulus.name)) %>%
    group_by(Presented.Stimulus.name, trialIndex)
    
  data %<>%
    arrange(trialIndex, Recording.timestamp) %>%
    group_by(Presented.Stimulus.name, variable) %>%
    arrange(trialIndex, Presented.Stimulus.name, Recording.timestamp) %>%
    ungroup() %>%
    group_by(Presented.Stimulus.name, trialIndex) %>%
    summarise(Recording.time.begin = min(Recording.timestamp),
              Recording.time.end = max(Recording.timestamp),
              trialDuration = Recording.time.end - Recording.time.begin,
              Presented.Stimulus.name = unique(Presented.Stimulus.name),
              Eye.movement.type = unique(Eye.movement.type))
  data %<>%
    group_by(Presented.Stimulus.name, trialIndex) %>%
    summarise(trialDuration = unique(trialDuration))

  return(data)
}

#Remove fundo. Disconsider cases where participant is looking both to fundo AND somewhere else
duplicatedFundo = function(nrow, variable){
 isDouble = nrow == 2
 isFundo = variable == "F" 
 if(all(isDouble, isFundo)){
   return(TRUE)
 } else{
   return(FALSE)
 }
}

duplicatedFundoWrap = function(a){
  a %<>%
    group_by(Presented.Stimulus.name, Eye.movement.type.index, Computer.timestamp.begin) %>%
    mutate(numberDuplicated = NROW(Presented.Stimulus.name))

  removeFundoDuplicado = c()
  for(k in 1:length(a$variable)){
    fundoDuplo = duplicatedFundo(a$numberDuplicated[k], a$variable[k])
    removeFundoDuplicado = c(removeFundoDuplicado, fundoDuplo)
  }
  a$removeFundoDuplicado = removeFundoDuplicado
  a %<>% filter(removeFundoDuplicado == FALSE)
  a %<>% select(!removeFundoDuplicado, numberDuplicated)
  return(a)
}

processOne = function(data){
  initialColnames = data %>% colnames()
  filterColumns = c("Recording.name", "Presented.Stimulus.name", "Eye.movement.type.index", "Computer.timestamp.begin", "Computer.timestamp.end", "Recording.time.begin", "Recording.time.end", "Gaze.event.duration", "Eye.movement.type", "variable", "value", "pupil.right", "pupil.left", "target", "Recording.name")
  ##Adicionando diagnostico
  diags = c("FS9IP", "FS24IP", "FS65IP", "FS76IP", "FS93IP", "RP100IP", "SM114IP", "SM118IP", "MR135IP", "MR136IP", "MR140IP", "SF142IP", "SF234IP", "SF246IP", "MR281IP", "MR285IP", "MR299IP", "CR348IP", "CR356IP", "RP373IP", "SI429IP", "SI430IP", "SM462IP", "MP466IP", "CR475IP", "MR534IP", "CR559IP", "SM581IP", "MR589IP", "SI619IP", "RP657IP", "CR683IP", "MR688IP", "MR691IP", "SF728IP", "SI776IP", "SM787IP")
  nomeColunas = data %>% colnames()
  if("Presented.Stimulus.name" %in% nomeColunas){
    data %<>%
      select(filterColumns)
    data$condition = sapply(str_split(data$Presented.Stimulus.name, "_"), `[`, 1)
    
    data %<>%
      ungroup() %>%
      mutate(Gaze.event.duration = ((Computer.timestamp.end - Computer.timestamp.begin)/1000000)) %>%
      group_by(Presented.Stimulus.name) %>%
      summarise(totalFixation = sum(Gaze.event.duration),
                condition = unique(condition),
                Recording.name = unique(Recording.name),
                Computer.timestamp.begin = unique(Computer.timestamp.begin)) %>%
      arrange(Recording.name, Presented.Stimulus.name)

      data$tea = FALSE
      data$tea[data$Recording.name %in% diags] <- TRUE
      return(data)
  } else {
    print("No data")
    return(NULL)
  }
}

#Checks if participant is looking to given target
checkCorrectTarget = function(looking, target){
  if(looking == 'E' | looking == "D"){
    if(looking == target){
      return("target")
    } else {
      return("distractor")
    }
  } else {
    return(looking)
  }
}

checkFocus = function(a){
  focus = c()
  for(k in 1:length(a$variable)){
    current = checkCorrectTarget(a$variable[k], a$target[k])
    focus = c(focus, current)
  }
  a$focus = focus
  return(a)
}

#Log of bulk processing
getName = function(dataFrame){
  a = readAndRename(dataFrame)
  a$Recording.name = str_remove_all(a$Recording.name, "-2")
  a$Recording.name = str_remove_all(a$Recording.name, "-3")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, "\\(")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, "\\)")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, " 1")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, " 2")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, " novo")
  a = a %>% select(Recording.name, Presented.Stimulus.name) %>% filter(!str_detect(Presented.Stimulus.name, 'BL_'))
  a = a %>% filter(Presented.Stimulus.name %in% trials)
  a = a %>% group_by(Recording.name) %>% distinct(Presented.Stimulus.name)
  return(a)
}

getName2 = function(dataFrame){
  a = readAndRename(dataFrame)
  a$Recording.name = str_remove_all(a$Recording.name, "-2")
  a$Recording.name = str_remove_all(a$Recording.name, "-3")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, "\\(")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, "\\)")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, " 1")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, " 2")
  a$Presented.Stimulus.name = str_remove_all(a$Presented.Stimulus.name, " novo")
  a = a %>% select(Recording.name, Presented.Stimulus.name) %>% filter(!str_detect(Presented.Stimulus.name, 'BL_'))
  a$Presented.Stimulus.name = substr(a$Presented.Stimulus.name, 1, 11)     
  a = a %>% filter(Presented.Stimulus.name %in% trials)
  a = a %>% group_by(Recording.name) %>% distinct(Presented.Stimulus.name)
  return(a)
}
