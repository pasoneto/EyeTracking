source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("/Users/pdealcan/Documents/github/sabara/code/verificacoes/verificationFunctions.R")
library("stringr")
library("ggridges")
library("readxl")
directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipant"
setwd(directory)

repeatedTime = function(x){
  a = fread(x) %>%
      group_by(Presented.Stimulus.name, Computer.timestamp.begin) %>%
      filter(n()>1) %>%
      nrow()
  if(a != 0){
    return(x)
  } else {
    return(NULL)
  }
}

f = list.files(directory)
file_list = lapply(f, repeatedTime)

#Check if no track has doubled Computer timestamp
is.null(unlist(file_list))
