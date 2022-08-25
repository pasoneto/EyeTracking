source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipantAll/"
directory2 = "/Users/pdealcan/Documents/github/dataSabara/processedFundoAll/"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/processedTotalTime/"
library("readxl")

setwd(directory)

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)

library(plyr)
allParticipants1 <- ldply(file_list, data.frame)
filterColumns = c("Presented.Stimulus.name", "trialIndex", "Eye.movement.type.index", "Computer.timestamp.begin", "Computer.timestamp.end", "Recording.time.begin", "Recording.time.end", "Gaze.event.duration", "Eye.movement.type", "variable", "value", "pupil.right", "pupil.left", "target", "Recording.name")
allParticipants1 = allParticipants1 %>% select(filterColumns)

detach(package:plyr)
allParticipants1$condition = stri_sub(allParticipants1$Presented.Stimulus.name, 1, 3)

allParticipants1 = allParticipants1 %>%
  group_by(Presented.Stimulus.name, Recording.name, trialIndex) %>%
  summarise(totalFixation = sum(Gaze.event.duration)) %>%
  arrange(Recording.name, Presented.Stimulus.name) %>%
  filter(!is.na(totalFixation))

##Adicionando diagnostico
diags = c("FS9IP", "FS24IP", "FS65IP", "FS76IP", "FS93IP", "RP100IP", "SM114IP", "SM118IP", "MR135IP", "MR136IP", "MR140IP", "SF142IP", "SF234IP", "SF246IP", "MR281IP", "MR285IP", "MR299IP", "CR348IP", "CR356IP", "RP373IP", "SI429IP", "SI430IP", "SM462IP", "MP466IP", "CR475IP", "MR534IP", "CR559IP", "SM581IP", "MR589IP", "SI619IP", "RP657IP", "CR683IP", "MR688IP", "MR691IP", "SF728IP", "SI776IP", "SM787IP")
allParticipants1$tea = FALSE
allParticipants1$tea[allParticipants1$Recording.name %in% diags] <- TRUE

#N tea vs non tea
allParticipants1 %>%
  ungroup() %>%
  distinct(Recording.name, .keep_all= TRUE) %>%
  group_by(tea) %>%
  summarise(count = length(tea))

allParticipants1$condition = stri_sub(allParticipants1$Presented.Stimulus.name, 1, 3)

#Descriptive
descriptive1 = allParticipants1 %>%
  group_by(tea) %>%
  summarise(meanRatio = mean(totalFixation),
            stder = sd(totalFixation)/sqrt(length(totalFixation)))

library(xtable)
xtable(descriptive1, type = "latex")

allParticipants1 %>%
  ggplot(aes(x = tea, y = totalFixation, fill = tea)) +
    geom_violin() +
    geom_boxplot(width = 0.2, fill = "white")

ggsave(paste("/Users/pdealcan/Documents/github/sabara/reports/report13/", "boxplot.png", sep = ""))

#Descriptive
descriptive2 = allParticipants1 %>%
  group_by(tea, condition) %>%
  summarise(meanRatio = mean(totalFixation),
            stder = sd(totalFixation)/sqrt(length(totalFixation)))

xtable(descriptive2, type = "latex")

allParticipants1 %>%
  ggplot(aes(x = tea, y = totalFixation, fill = tea)) +
    facet_wrap(~condition)+
    geom_violin() +
    geom_boxplot(width = 0.2, fill = "white")

ggsave(paste("/Users/pdealcan/Documents/github/sabara/reports/report13/", "boxplot2.png", sep = ""))
