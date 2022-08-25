source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
directory = "/Users/pdealcan/Documents/github/dataSabara/allData"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/processedFundoAll/"
setwd(directory)
library("readxl")

functionMerge = function(l){
    directoryCurrent = paste(directory, "/", l, sep = "")

    setwd(directoryCurrent)

    files = list.files()
    file_list = lapply(files, function(i){readAndRename(i)})

    #Fixing fundo focus
    file_list = lapply(file_list, function(i){detatchFundo(i, directoryOut)})
    
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

