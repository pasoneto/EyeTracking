source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
directory = "/Users/pdealcan/Documents/github/dataSabara/processedFundoAll/"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/processedPupil/"
library("readxl")

setwd(directory)

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)
setwd("../")

###############################################
########### Process pupil dilation ############
###############################################
functionMerge = function(i){
  nameParticipant = unique(i$Recording.name)
  a = processPupil(i, trials, colunas)
  write.csv(a, paste(directoryOut, nameParticipant, '.csv', sep=""))
}

for(i in file_list){
    skip_to_next <- FALSE   
    tryCatch(
      functionMerge(i),
      error = function(e){
        skip_to_next <<- TRUE
        print(e)
      },
      finally = {
        print(paste("Finished", unique(i$Recoring.name), sep = " "))
      }
    )
}

