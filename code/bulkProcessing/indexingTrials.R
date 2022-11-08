source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
directory = "/Users/pdealcan/Documents/github/dataSabara/AllData"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/allIndexed/"
setwd(directory)
library("readxl")

functionMerge = function(l){
    directoryCurrent = paste(directory, "/", l, sep = "")

    setwd(directoryCurrent)

    files = list.files()
    file_list = lapply(files, function(i){
      a = readAndRename(i)
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
        print("No data")
      }
    })
}

folders = list.files()
for(l in folders){
    skip_to_next <- FALSE   
    tryCatch(
      functionMerge(l),
      error = function(e){
        skip_to_next <<- TRUE
        print(e)
      },
      finally = {
        print(paste("Finished", l, sep = " "))
      }
    )
}

