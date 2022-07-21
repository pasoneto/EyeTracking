source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
library(tidyr)
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")

#(certo-errado)/totalFoco

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
file_list = file_list[1:(length(file_list)-1)] #remove last problematic file

#Processing all participants
allParticipants = lapply(file_list, function(i){processParticipant(i, trials, colunas)})
allParticipants = bind_rows(allParticipants)

allParticipants = allParticipants %>%
  group_by(Recording.name, Presented.Stimulus.name, variable) %>%
  summarise(count = n(),
            target = unique(target)) %>%
  ungroup() %>%
  pivot_wider(names_from = variable, values_from = c("count"), names_sep="")

allParticipants$condition = substr(allParticipants$Presented.Stimulus.name,1,3)
allParticipants[is.na(allParticipants)] = 0

allParticipants %>% dvisu()

indexCalc = function(esquerda, direita, rosto, target){
   if(target == "D"){
     right = sum(direita)
     wrong = sum(esquerda)
     index = (right-wrong)/(right+wrong+rosto)
     return(index)
   } else{
     right = sum(esquerda)
     wrong = sum(direita)
     index = (right-wrong)/(right+wrong+rosto)
     return(index)
   }
}

allParticipants %>%
  group_by(Presented.Stimulus.name, Recording.name) %>%
  summarise(index = indexCalc(E, D, R, unique(target)),
            condition = unique(condition)) %>%
  ggplot(aes(x = index, fill = condition))+
    geom_density(alpha = 0.5)

ggsave('/Users/pdealcan/Documents/github/sabara/reports/report11/countIndex.png')
