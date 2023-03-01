source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Primeiro são filtrados os participantes conforme o critério de inclusão
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE)

dfNonTEA = df %>% filter(tea == FALSE)
dfTEA = df %>% filter(tea == TRUE)

nTEA = length(unique(dfTEA$Recording.name))

permutateFunction = function(df, N, analyseFunction){
  participants = unique(df$Recording.name)
  participantsKeep = sample(participants, size = N, replace = FALSE)

  df = df %>%
    filter(Recording.name %in% participantsKeep)
  
  df = analyseFunction(df)

  return(df)
}

#Alternancias 
computeAlternancias = function(df){
  df = df %>%
    group_by(Recording.name, Presented.Stimulus.name, condition, tea) %>%
    summarise(distractorProportion = unique(distractorProportion),
              fundoProportion = unique(fundoProportion), 
              targetProportion = unique(targetProportion), 
              rostoProportion = unique(rostoProportion)) %>%
    group_by(condition) %>%
    summarise(distractorProportion = mean(distractorProportion),
              fundoProportion = mean(fundoProportion), 
              targetProportion = mean(targetProportion), 
              rostoProportion = mean(rostoProportion)) %>%
    melt(id.vars = c("condition"))

  return(df)
}

sampleTEA = dfTEA %>%
  group_by(Recording.name, Presented.Stimulus.name, condition, tea) %>%
  summarise(distractorProportion = unique(distractorProportion),
            fundoProportion = unique(fundoProportion), 
            targetProportion = unique(targetProportion), 
            rostoProportion = unique(rostoProportion)) %>%
  group_by(condition) %>%
  summarise(distractorProportion = mean(distractorProportion),
            fundoProportion = mean(fundoProportion),
            targetProportion = mean(targetProportion), 
            rostoProportion = mean(rostoProportion))

samplesSet = c()
for(k in 1:10000){
  samplesSet[[k]] = permutateFunction(dfNonTEA, nTEA, computeAlternancias)
}

samplesSet = bind_rows(samplesSet)
sampleTEA = sampleTEA %>% melt()

vlines = geom_vline(data=sampleTEA, aes(xintercept=value), linetype="dashed", color="blue")
samplesSet %>%
  ggplot(aes(x = value)) +
    geom_histogram()+
    vlines+
    facet_wrap(~condition+variable, scale = "free")
