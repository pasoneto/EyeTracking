source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
file_list = file_list[1:(length(file_list)-1)] #remove last problematic file

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

checkMissingtrial = function(participant, plot = FALSE){
  a = processParticipant(participant, trials, colunas)
  presentStimulusProcessed = unique(a$Presented.Stimulus.name)
  presentStimulusNonProcessed = unique(participant$Presented.Stimulus.name)
  presentStimulusNonProcessed = presentStimulusNonProcessed[presentStimulusNonProcessed %in% trials]

  print("No missing trials:")
  print(length(presentStimulusProcessed) == length(presentStimulusNonProcessed))

  missingNonProcessed = presentStimulusProcessed[!(presentStimulusProcessed %in% presentStimulusNonProcessed)]
  missingProcessed = presentStimulusNonProcessed[!(presentStimulusNonProcessed %in% presentStimulusProcessed)]
  
  print("Number of missing trials:")
  print(length(missingProcessed))
  if((length(missingProcessed) > 0) && (plot == TRUE)){
    print(missingProcessed)
    participant %>%
      filter(Presented.Stimulus.name %in% missingProcessed) %>%
      select(Presented.Stimulus.name, matches(missingProcessed)) %>%
      dvisu()
  }   
  if(length(missingProcessed) > 0){
    return(i)
  }
}

problematic = c()
for(i in 1:length(file_list)){
  problematic = c(problematic, checkMissingtrial(file_list[[i]]))
}

problematic

file_list[[1]] %>% colnames()
unique(file_list[[1]]$Recording.date)
unique(file_list[[1]]$Recording.Name)
checkMissingtrial(file_list[[49]], plot = TRUE)
