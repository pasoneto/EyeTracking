source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
directory = "/Users/pdealcan/Documents/github/dataSabara/finalRawData"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/allIndexedFINAL/"
setwd(directory)
library("readxl")

setwd(directory)

#options(scipen = 100, digits = 4)
#df = fread("Joint Attention 2022 CR736IP.tsv")
#var1 = df$`Computer timestamp`
#var1 = as.numeric(sub(",", ".", sub(".", "", var1, fixed=TRUE), fixed=TRUE))
#df$`Computer timestamp` = var1
#write.csv(df, "Joint Attention 2022 CR736IP.tsv")

functionMerge = function(fileNames){
    logFailed = c()
    file_list = lapply(fileNames, function(i){
      a = readAndRename(i)
      namesOfColumns = a %>% colnames()
      duplicatedCols = !all(namesOfColumns %>% duplicated())
      if(duplicatedCols){
        colnames(a) = make.unique(namesOfColumns) 
      }
      currentName = unique(a$Recording.name)
      currentName = paste(currentName, ".csv", sep="")
      a %<>%
        filter(!is.na(Presented.Stimulus.name)) %>%
        filter(!str_detect(Presented.Stimulus.name, 'Eyetracker Calibration|bichinhocolorido|bola|fadinha|MASK'))
      if(nrow(a) > 1){
        a$Presented.Stimulus.name = paste(a$Presented.Stimulus.name, fixationIndexer(a$Presented.Stimulus.name), sep = "_")
        write.csv(a, paste(directoryOut, currentName, sep = ""))
        print(currentName)
      } else {
        logFailed = c(i, logFailed)
        print("No data")
      }
    })
    return(logFailed)
}

#Number of output files is lower than number of input files. That's because there are repeated participants, and the output file is named with the name of the participant. That results in no repeated files.
files = list.files()
logFailed = functionMerge(files)
