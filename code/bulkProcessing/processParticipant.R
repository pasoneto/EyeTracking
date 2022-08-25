source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")
directory = "/Users/pdealcan/Documents/github/dataSabara/processedFundoAll/"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/processedParticipantAll/"

setwd(directory)

files = list.files()

#Reading all data
file_list = lapply(files, function(i){fread(i)})

functionMerge = function(i){
  nameParticipant = unique(i$Recording.name)
  a = processParticipant(i, trials, colunas)
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
