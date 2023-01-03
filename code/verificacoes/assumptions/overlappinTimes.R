#Calculates the total time spent looking at the screen by different conditions
source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("readxl")
library("ggridges")
directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL/"

setwd(directory)

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)

allParticipants1 = dplyr::bind_rows(file_list)
allParticipants1$condition = sapply(str_split(allParticipants1$Presented.Stimulus.name, "_"), `[`, 1)
allParticipants1 = allParticipants1 %>% filter(condition != "BL")

increaseCheck = function(begin, end){
  increase = c()
  if(length(begin) > 1){
    for(k in 1:(length(begin)-1)){
      i = begin[k+1] > end[k]
      increase = c(increase, i)
    }
    increase = c(increase, TRUE)
    return(increase)
  } else {
    return(TRUE)
  }
}

#Time always increase between fixation
allParticipants1 %>%
  group_by(Recording.name, Presented.Stimulus.name) %>%
  mutate(increasing = increaseCheck(Computer.timestamp.begin, Computer.timestamp.end)) %>%
  filter(increasing == FALSE) %>%
  nrow()

