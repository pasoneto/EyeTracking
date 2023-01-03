source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("/Users/pdealcan/Documents/github/sabara/code/verificacoes/verificationFunctions.R")
library("stringr")
library("ggridges")
library("readxl")
directory = "/Users/pdealcan/Documents/github/dataSabara/finalRawData"
setwd(directory)

name = function(x){
  a = unique(x$Recording.name)
  return(a)
}

names = c()
files = list.files(directory)
file_list = lapply(files, function(i){readAndRename(paste(directory, i, sep = "/"))})
names = c(names, lapply(file_list, name))

#Are there repeated names?
names = unlist(names)
length(unique(names)) == length(names)

dNames = c()
duplicatedNames = duplicated(names)
for(k in 1:length(duplicatedNames)){
   if(duplicatedNames[k]){
    dNames = c(names[k], dNames)
   }
}

#Are there repeated file names?
files = c()
directoryCurrent = paste(directory, "/", l, sep = "")
f = list.files(directoryCurrent)
files = c(f, files)
}

length(unique(names)) == length(names)
