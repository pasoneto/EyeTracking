source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
file_list = file_list[1:(length(file_list)-1)] #remove last problematic file

#Processing all participants
allParticipants = lapply(file_list, function(i){processParticipant(i, trials, colunas)})
allParticipants = bind_rows(allParticipants)

###############################################
######### Total fixation duration #############
###############################################
tfd = allParticipants %>%
  group_by(Presented.Stimulus.name, trialIndex, Recording.name, variable) %>%
  filter(NROW(variable) > 1) %>% 
  summarise(total_fixation_duration = sum(Gaze.event.duration),
            condition = stri_sub(unique(Presented.Stimulus.name), 1, 3),
            target = unique(target),
            Recording.name = unique(Recording.name))

dodge <- position_dodge(width = 1)
#Add condition
tfd %>%
  ggplot(aes(x = variable, y = total_fixation_duration, fill = variable)) +
    facet_wrap(~condition+target)+
    geom_violin(position = dodge)+
    geom_boxplot(width=.1, position = dodge) 

ggsave('/Users/pdealcan/Documents/github/sabara/reports/report11/totalFixationDuration.png')

tfd %>%
  group_by(target, variable, condition) %>%
  summarise(stder = sd(total_fixation_duration)/sqrt(length(total_fixation_duration)),
            total_fixation_duration = mean(total_fixation_duration)) %>%
  ggplot(aes(x = variable, y = total_fixation_duration, group = variable, color = variable)) +
    facet_wrap(~condition+target)+
    geom_point(position=position_dodge(width = 0.5))+
    geom_errorbar(aes(ymin = total_fixation_duration - stder, ymax = total_fixation_duration + stder), position=position_dodge(width = 0.5))
  
ggsave('/Users/pdealcan/Documents/github/sabara/reports/report11/totalFixationDuration2.png')

