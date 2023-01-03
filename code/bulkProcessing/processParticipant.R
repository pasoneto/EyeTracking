source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
library("readxl")
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")
directory = "/Users/pdealcan/Documents/github/dataSabara/allIndexedFINAL/"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL/"
library("tidyselect")

setwd(directory)

functionMerge = function(i){
  nameParticipant = unique(i$Recording.name)
  a = processParticipant(i, trials, colunas) #Count duration of each fixation
  if(!is.null(a)){
    a = duplicatedFundoWrap(a) #Remove duplicated fundo
    print(unique(a$Recording.name))
    write.csv(a, paste(directoryOut, nameParticipant, '.csv', sep=""))
    return("fine")
  } else {
    print("No data")
    return(nameParticipant)
  }
}

file_list = list.files()
file_list = lapply(file_list, fread)

logErrorsList = c()
for(l in file_list){
  logErrors = functionMerge(l)
  if(logErrors != "fine"){
    logErrorsList = c(logErrors, logErrorsList)
  }
}

print("Problematic participants below. They have no data at all. Only unclassified/eyes not found. Some fixations, but only during calibration")
print(logErrorsList)

#pFiles = c("SM462IP",   "SI421IP",   "SF226IP",   "MT706IP-2", "MR324IP",  "MR313IP", "FS12IP", "CR632IP", "CR397IP")
#fread(paste(pFiles[1], ".csv", sep=""))

