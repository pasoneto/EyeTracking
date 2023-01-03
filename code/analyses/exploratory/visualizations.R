local_utils = "C:\\Users\\Pronas Eye Tracking\\Desktop\\analiseja\\utils.R"
local_data_input = "C:\\Users\\Pronas Eye Tracking\\Desktop\\analiseja\\Data Export - Joint Attention"
local_data_output = "C:\\Users\\Pronas Eye Tracking\\Desktop\\analiseja\\dadosJA\\"

source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
setwd("/Users/pdealcan/Documents/github/dataSabara/allIndexed/")

########## Lendo e limpando o banco de dados
files = list.files()[1:10]
#file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
file_list = lapply(files, fread)
#file_list = file_list[1:(length(file_list)-1)] #remove last problematic file 

#Selecting columns of interest by trial type
trials = c("IJA_A3_B1_D","IJA_A1_B2_E","RJA_A2_B1_E","RJA_A1_B2_D","RJA_A2_B2_E","IJA_A1_B1_D","IJA_A3_B1_E","RJA_A2_B1_D","IJA_A1_B1_E")
colunas = c()
foco = c("Rosto.", "Brinquedo.Direita.", "Brinquedo.Esquerda.")
for(i in trials){
  for(k in foco){
    colunas = c(colunas, paste("AOI.hit..", i, "...", k, sep = ""))
  }
}
#addedNames = c("AOI.hit..RJA_A2_B1_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B1_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B1_E...Rosto..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B2_E...Rosto..1")
#colunas = c(colunas, addedNames)
i = 1 
#processParticipant(setDT(file_list[[1]]), trials, colunas) %>%
for(i in 1:length(file_list)){
  tryCatch(
      expr = {
          participant_id = unique(file_list[[i]]$Recording.name)
          processParticipant(setDT(file_list[[i]]), trials, colunas) %>%
            ggplot(aes(y = variable, x = 0, color = variable))+
              facet_wrap(~Presented.Stimulus.name, scale = "free")+
              geom_errorbar(aes(xmin = Recording.time.begin, xmax = Recording.time.end), width = 0, size = 3)+
              theme(legend.position = "None", strip.text.y = element_blank()) +
              ylab("Stimulus focused")+
              xlab("Elapsed time (ms)")
          ggsave(paste('/Users/pdealcan/Documents/github/sabara/contract/', participant_id, ".jpg", sep = ""))
          print(participant_id)
      },
      error = function(e){
          print("Error:")
          print(participant_id)
      },
      warning = function(w){
      },
      finally = {
      }
  )    
}

