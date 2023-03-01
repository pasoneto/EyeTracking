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

#Proportions 
computeProportions = function(df){
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


#Compute alternancias
computeAlternancias = function(df){
  df = df %>%
    group_by(Recording.name, Presented.Stimulus.name, condition, tea) %>%
    summarise(targetRosto = sum(TR),
              rostoTarget = sum(RT), 
              distractorRosto = sum(DR), 
              rostoDistractor = sum(RD)) %>%
    group_by(condition) %>%
    summarise(targetRosto = mean(targetRosto),
              rostoTarget = mean(rostoTarget), 
              distractorRosto = mean(distractorRosto), 
              rostoDistractor = mean(rostoDistractor)) %>%
    melt(id.vars = c("condition"))

  return(df)
}

############################
## Sample TEA proportions ##
############################
sampleTEA = computeProportions(dfTEA)

samplesSet = c()
for(k in 1:1000){
  samplesSet[[k]] = permutateFunction(dfNonTEA, nTEA, computeProportions)
}

samplesSet = bind_rows(samplesSet)

vlines = geom_vline(data=sampleTEA, aes(xintercept=value), linetype="dashed", color="blue")
samplesSet %>%
  ggplot(aes(x = value)) +
    geom_histogram()+
    vlines+
    facet_wrap(~condition+variable, scale = "free")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/report21/proportions.png")

#############################
## Sample TEA alternâncias ##
#############################
sampleTEA = computeAlternancias(dfTEA)

samplesSet = c()
for(k in 1:1000){
  samplesSet[[k]] = permutateFunction(dfNonTEA, nTEA, computeAlternancias)
}

samplesSet = bind_rows(samplesSet)

vlines = geom_vline(data=sampleTEA, aes(xintercept=value), linetype="dashed", color="blue")
samplesSet %>%
  ggplot(aes(x = value)) +
    geom_histogram()+
    vlines+
    facet_wrap(~condition+variable, scale = "free")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/report21/alternancias.png")
