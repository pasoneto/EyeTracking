source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
directory = "/Users/pdealcan/Documents/github/dataSabara/processedPupil/"
library("readxl")

#######################################################
########### Fetch processed pupil dilation ############
#######################################################
setwd(directory)

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)
setwd("../")

library(plyr)
allParticipants <- ldply(file_list, data.frame)
filterColumns = c("Eye.movement.type.index", "Pupil.diameter.left", "Pupil.diameter.right", "Recording.timestamp", "Presented.Stimulus.name", "trialIndex", "variable", "target", "Recording.name", "pupil")
allParticipants = allParticipants %>% select(filterColumns)
detach(package:plyr)
allParticipants$condition = stri_sub(allParticipants$Presented.Stimulus.name, 1, 3)

##Adicionando diagnostico
diags = c("FS9IP", "FS24IP", "FS65IP", "FS76IP", "FS93IP", "RP100IP", "SM114IP", "SM118IP", "MR135IP", "MR136IP", "MR140IP", "SF142IP", "SF234IP", "SF246IP", "MR281IP", "MR285IP", "MR299IP", "CR348IP", "CR356IP", "RP373IP", "SI429IP", "SI430IP", "SM462IP", "MP466IP", "CR475IP", "MR534IP", "CR559IP", "SM581IP", "MR589IP", "SI619IP", "RP657IP", "CR683IP", "MR688IP", "MR691IP", "SF728IP", "SI776IP", "SM787IP")
allParticipants$tea = FALSE
allParticipants$tea[allParticipants$Recording.name %in% diags] <- TRUE

#######################################
########### Pupil dilation ############
#######################################
#allParticipants %>%
#  filter(Recording.name == "CR359IP") %>%
#  ggplot(aes(x = Recording.timestamp, y = pupil, color = variable))+
#    facet_wrap(~Presented.Stimulus.name+trialIndex, scale = "free")+
#    geom_point(size=0.5)

#ggsave('/Users/pdealcan/Documents/github/sabara/reports/report10/oneParticipant.png')

allParticipants$condition = stri_sub(allParticipants$Presented.Stimulus.name, 1, 3)

comparisons = allParticipants %>%
  filter(!is.na(pupil)) %>%
  group_by(Recording.name, trialIndex, Presented.Stimulus.name, variable, target, condition, tea) %>%
  summarise(pupil = mean(pupil, na.rm = TRUE))

comparisons = comparisons %>%
  group_by(condition, tea) %>%
  summarise(meanPupil = mean(pupil),
            stder = sd(pupil)/sqrt(length(pupil))
  )

library(xtable)
xtable(comparisons, type = "latex")

comparisons %>%
  ggplot(aes(x = condition, y = meanPupil, color = tea)) +
    facet_wrap(~tea) +
    geom_point() +
    geom_errorbar(aes(ymin = meanPupil - stder, ymax = meanPupil + stder))

ggsave('/Users/pdealcan/Documents/github/sabara/reports/report13/pupil.png')
