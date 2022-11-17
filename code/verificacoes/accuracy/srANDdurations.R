source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("/Users/pdealcan/Documents/github/sabara/code/verificacoes/verificationFunctions.R")
library("stringr")
library("ggridges")
library("readxl")
directory = "/Users/pdealcan/Documents/github/dataSabara/AllData"
setwd(directory)

samplingRates = c()
durations = c()
for(l in c("0", "1", "2", "3", "4")){
  directoryCurrent = paste(directory, "/", l, sep = "")
  files = list.files(directoryCurrent)
  file_list = lapply(files, function(i){readAndRename(paste(directoryCurrent, i, sep = "/"))})
  samplingRates = c(samplingRates, lapply(file_list, samplingRate))
  durations = c(durations, lapply(file_list, timeConsistency))
  print(l)
}

samplingRates = bind_rows(samplingRates)
samplingRates = samplingRates %>% select(d, Recording.name)
samplingRates %>% write.csv("/Users/pdealcan/Documents/github/sabara/code/verificacoes/accuracy/samplingRates.csv")

durations = bind_rows(durations)
durations %>% write.csv("/Users/pdealcan/Documents/github/sabara/code/verificacoes/accuracy/timeAll.csv")
