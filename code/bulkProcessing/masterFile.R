source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)

#Pré processamento
setwd("/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL")

source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsVideoDuration.R")
source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsProportion.R")
source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsNConditions.R")

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)
a = bind_rows(file_list) %>% select(!numberDuplicated)
a$tea = unlist(lapply(a$Recording.name, tagDiagnostico))

filterBase = paste(a$Presented.Stimulus.name, a$Recording.name, sep="")
durations = filterOutDurations$filterOutDurations #anomalous videos duration
cutoff = filterOutNames05$filterOutTrials #0.5 cutoff
conditions = filterOutConditions$filterOutConditions #Participants not having at least 1 trial in each condition. Excluding baseline

filterDurations = c()
filterCutoffs = c()
filterConditions = c()
for(k in 1:length(filterBase)){
    filterDurations = c(filterDurations, filterBase[k] %in% durations)
    filterCutoffs = c(filterCutoffs, filterBase[k] %in% cutoff)
    filterConditions = c(filterConditions, a$Recording.name[k] %in% conditions)
}

a$filterDurations = filterDurations
a$filterCutoffs = filterCutoffs
a$filterConditions = filterConditions
a$target = substr(a$target, 1, 1) #Fixing target names
a$condition = substr(a$Presented.Stimulus.name, 1, 3) #Adding condition back


#Total fixation duration
a = checkFocus(a) #Check if participant is looking to the target, distractor, rosto ou fundo.
a = a %>%
  group_by(Recording.name, Presented.Stimulus.name) %>%
  mutate(totalFixation = sum(Gaze.event.duration)) %>%
  group_by(Recording.name, Presented.Stimulus.name, focus) %>%
  mutate(proportionFixation = Gaze.event.duration/totalFixation) #Proportion of current fixation duration in relation to the total time of fixations

proportionsTarget = a %>% 
  group_by(Recording.name, Presented.Stimulus.name, focus) %>%
  summarise(sum(proportionFixation)) %>%
  dcast(Recording.name+Presented.Stimulus.name~focus) %>%
  replace(is.na(.), 0)

colnames(proportionsTarget) = c("Recording.name", "Presented.Stimulus.name", "distractorProportion", "fundoProportion", "rostoProportion", "targetProportion")

a = merge(proportionsTarget, a)

#Adding alternancias

#Atenção! O filtro filterConditions só faz sentido caso aplicado APÓS os filtros filterDurations e filterCutoffs.
#Para bater com o relatório 18, deve-se também retirar os BASELINE.
a = a %>%
  group_by(Presented.Stimulus.name, Recording.name) %>%
  filter(NROW(variable) > 2) %>% 
  mutate(RD = alternanciaCount(variable, "R", "D"),
         RE = alternanciaCount(variable, "R", "E"),
         DR = alternanciaCount(variable, "D", "R"),
         ER = alternanciaCount(variable, "E", "R"),
         ER = alternanciaCount(variable, "E", "R"),

         RT = alternanciaCount(focus, "R", "target"),
         TR = alternanciaCount(focus, "target", "R"),
         RD = alternanciaCount(focus, "R", "distractor"),
         DR = alternanciaCount(focus, "distractor", "R"),
  )

#File final elaborada
a = a %>%
  select(Recording.name, 
         Presented.Stimulus.name, 
         condition, 
         tea, 
         target, 
         variable, 
         focus,
         Recording.time.begin, 
         Recording.time.end, 
         Gaze.event.duration, 
         pupil.right, 
         pupil.left,
         totalFixation, 
         proportionFixation, 
         targetProportion,
         distractorProportion,
         fundoProportion, 
         rostoProportion, 
         RD, 
         RE, 
         DR, 
         ER,
         RT,
         TR,
         RD,
         DR,
         filterDurations, 
         filterCutoffs, 
         filterConditions)

write.csv(a, "/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
