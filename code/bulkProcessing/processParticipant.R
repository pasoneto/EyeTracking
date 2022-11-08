source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
library("readxl")
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")
directory = "/Users/pdealcan/Documents/github/dataSabara/allIndexed/"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/processedParticipant/"

setwd(directory)

functionMerge = function(i){
  nameParticipant = unique(i$Recording.name)
  a = processParticipant(i, trials, colunas) #Count duration of each fixation
  if(!is.null(a)){
    a = duplicatedFundoWrap(a) #Remove duplicated fundo
    write.csv(a, paste(directoryOut, nameParticipant, '.csv', sep=""))
  } else {
    print("No data")
  }
}

folders = list.files()
file_list = lapply(folders, fread)

for(l in file_list){
  functionMerge(l)
}

