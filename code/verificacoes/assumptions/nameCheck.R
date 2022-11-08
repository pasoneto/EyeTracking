source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("/Users/pdealcan/Documents/github/sabara/code/verificacoes/verificationFunctions.R")
library("stringr")
library("ggridges")
library("readxl")
directory = "/Users/pdealcan/Documents/github/dataSabara/AllData"
setwd(directory)

name = function(x){
  a = unique(x$Recording.name)
  return(a)
}

names = c()
for(l in c("0", "1", "2", "3", "4")){
  directoryCurrent = paste(directory, "/", l, sep = "")
  files = list.files(directoryCurrent)
  file_list = lapply(files, function(i){readAndRename(paste(directoryCurrent, i, sep = "/"))})
  names = c(names, lapply(file_list, name))
}

#Are there repeated names?
names = unlist(names)
length(unique(names)) == length(names)

#Are there repeated file names?
files = c()
for(l in c("0", "1", "2", "3", "4")){
  directoryCurrent = paste(directory, "/", l, sep = "")
  f = list.files(directoryCurrent)
  files = c(f, files)
}

length(unique(names)) == length(names)

