source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Primeiro são filtrados os participantes conforme o critério de inclusão
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE)

dfTD = df %>% filter(tea == "TD")
dfTEA = df %>% filter(tea == "TEA")
dfNonTD = df %>% filter(tea == "nonTD")

nTEA = length(unique(dfTEA$Recording.name))
nNonTD = length(unique(dfNonTD$Recording.name))

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
sampleNonTD = computeProportions(dfNonTD)

samplesSet = c()
for(k in 1:1000){
  samplesSet[[k]] = permutateFunction(dfTD, nTEA, computeProportions)
}

samplesSet2 = c()
for(k in 1:1000){
  samplesSet2[[k]] = permutateFunction(dfTD, nNonTD, computeProportions)
}

samplesSet = bind_rows(samplesSet)
samplesSet2 = bind_rows(samplesSet2)

vlinesTEA = geom_vline(data=sampleTEA, aes(xintercept=value), linetype="dashed", color="blue")
vlinesNonTD = geom_vline(data=sampleNonTD, aes(xintercept=value), linetype="dashed", color="red")

samplesSet %>%
  ggplot(aes(x = value)) +
    geom_histogram()+
    vlinesTEA+ #blue
    vlinesNonTD+ #red
    facet_wrap(~condition+variable, scale = "free")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report3/proportionsTEAandNonTD.png")

samplesSet2 %>%
  ggplot(aes(x = value, fill = variable)) +
    geom_density(alpha = 0.5)+
    facet_wrap(~condition)

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report3/proportionsTD.png")


#############################
## Sample TEA alternâncias ##
#############################
sampleTEA = computeAlternancias(dfTEA)
sampleNonTD = computeAlternancias(dfNonTD)

samplesSet = c()
for(k in 1:1000){
  samplesSet[[k]] = permutateFunction(dfTD, nTEA, computeAlternancias)
}

samplesSet2 = c()
for(k in 1:1000){
  samplesSet2[[k]] = permutateFunction(dfTD, nNonTD, computeAlternancias)
}

samplesSet = bind_rows(samplesSet)
samplesSet2 = bind_rows(samplesSet2)

vlinesTEA = geom_vline(data=sampleTEA, aes(xintercept=value), linetype="dashed", color="blue")
vlinesNonTD = geom_vline(data=sampleNonTD, aes(xintercept=value), linetype="dashed", color="red")

samplesSet2 %>%
  ggplot(aes(x = value)) +
    geom_histogram()+
    vlinesTEA+
    vlinesNonTD+
    facet_wrap(~condition+variable, scale = "free")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report3/alternanciasBlueTEA.png")
