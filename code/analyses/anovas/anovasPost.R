source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("/Users/pdealcan/Documents/github/sabara/code/bulkProcessing/mergePrePost.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)
library(rstatix)

#Primeiro são filtrados os participantes conforme o critério de inclusão
df = dfPrePost

df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_'))

df %>% 
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  distinct(Recording.name, prePos, .keep_all = TRUE) %>%
  group_by(sexo, tea, prePos) %>%
  summarise(sdAgeJA = sd(ageJA, na.rm = TRUE)/12,
            meanAgeJA = mean(ageJA, na.rm = TRUE)/12,
            N = length(ageJA)
  ) %>%
  xtable(type = "latex")

#General info about all participants
infoParticipant = fread("/Users/pdealcan/Documents/github/sabara/details_experiment/infoParticipants.csv")
colnames(infoParticipant) = c("codigo", "Recording.name", "Codinome2", "dataNascimento", "sexo", "CARS", "dataCARS", "ageCARS",     "pontuacaoCARS", "diagnostico", "Consulta", "grupo",    "JA", "dataJA",  "ageJA",     "Obs.JA",   "GeoPref",  "dataGeo",      "Obs.GeoPref",       "pontABEP", "clasABEP", "idadeMAE", "escolPAI", "escolMAE", "idadeCRIANCA",      "regiaoCRIANCA",     "regiaoResponsavel", "RENDA", "numPESSOAS", "qtdePESSOAStrabalham", "tipoMORADIA", "saudeCRIANCA", "moradia", "GEOPREF2", "IntervalDurationAverage", "IntervalDurationMedian", "IntervalDurationCount", "IntervalDurationTotal Time of Interest Duration", "IntervalDurationTotal Recording Duration", "TotalFixDurationEllipse", "TotalFixDurationNon-social", "TotalFixDurationSocial", "TotalFixDurationAverage", "TotalFixDurationMedian", "TotalFixDurationSum", "TotalFixDurationTotal Time of Interest Duration", "TotalFixDurationTotal Recording Duration", "AvgFixationDurEllipse", "AvgFixationDurNon-social", "AvgFixationDurSocial", "AvgFixationDurAverage", "AvgFixationDurMedian", "AvgFixationDurTotal Time of Interest Duration", "AvgFixationDurTotal Recording Duration")

infoParticipant = infoParticipant %>%
  select(Recording.name, grupo, dataNascimento, dataCARS, dataJA, pontuacaoCARS, dataGeo, sexo)

#Fetching matched samples
#source("./matchedSample.R")
#matchedParticipants = subSample
#df = df %>%
  #filter(Recording.name %in% matchedParticipants)

dfPre = computeAlternancias(df %>% filter(prePos == "pre"))
dfPos = computeAlternancias(df %>% filter(prePos == "post"))
dfPre$prePos = "pre"
dfPos$prePos = "post"
dfA = bind_rows(dfPre, dfPos)

dfA$condition = as.factor(dfA$condition)
dfA$tea = as.factor(dfA$tea)
dfA$prePos = as.factor(dfA$prePos)

dfA = dfA %>%
  filter(tea %in% c("TEA", "TD", "nonTD"))

#mixed-designs ANOVA results
res.aov <- anova_test(
  data = dfA, dv = value, wid = Recording.name,
  between = tea, within = c(condition, variable, prePos))

aovResult = get_anova_table(res.aov)

#Pre pos. Mais alternância no pós
dfA %>%
  group_by(prePos) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = prePos, y = mean, color = prePos))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))+
    theme_minimal()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report16/prePos.png")

dfA %>%
  group_by(prePos) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value)))  %>%
  xtable(type = "latex")

#Variable*prePos
dfA %>%
  group_by(variable, prePos) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = prePos, y = mean, color = prePos))+
    facet_wrap(~variable, scale = "free")+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))+
    theme_minimal()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report16/prePosVar.png")

dfA %>%
  group_by(prePos, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value)))  %>%
  xtable(type = "latex")


#condition:variable:prePos
dfA %>%
  group_by(tea, prePos, condition) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = prePos, y = mean, color = tea))+
    facet_wrap(~condition+tea)+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))+
    theme_minimal()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report16/prePosCondition.png")

dfA %>%
  group_by(tea, prePos, condition) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  xtable(type = "latex")

#Propotion
#dfPre = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
#dfPost = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFilePost.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TEA", "TD", "nonTD"))

dfPre = computeProportions(df %>% filter(prePos == "pre"))
dfPost = computeProportions(df %>% filter(prePos == "post"))

dfPre$prePos = "pre"
dfPost$prePos = "post"

df = bind_rows(dfPre, dfPos)

df$condition = as.factor(df$condition)
df$tea = as.factor(df$tea)
df$prePos = as.factor(df$prePos)

#mixed-designs ANOVA results
res.aov <- anova_test(
  data = df, dv = value, wid = Recording.name,
  between = tea, within = c(condition, variable, prePos))

aovResult = get_anova_table(res.aov)

#variable, tea*variable, condition*variable
#New graph
pd = position_dodge(width = 0.8, preserve = "total")

df %>%
  group_by(variable, prePos) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = prePos, y = mean, color = prePos))+
    facet_wrap(~variable)+
    geom_point(position = pd)+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = pd)+
    theme_minimal()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report16/prePosVarProportion.png")

df %>%
  group_by(variable, prePos) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  xtable(type = "latex")

df %>%
  group_by(tea, prePos) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = prePos, y = mean, color = tea))+
    geom_point(position = pd)+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = pd)+
    theme_bw()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report16/teaPrePosProportion.png")

df %>%
  group_by(prePos, tea) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  xtable(type = "latex")

df %>%
  group_by(condition, tea, prePos) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = prePos, y = mean, color = tea))+
    facet_wrap(~condition)+
    geom_point(position = pd)+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = pd)+
    theme_bw()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report16/teaConditionProportion.png")

df %>%
  group_by(condition, tea, prePos) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  xtable(type = "latex")
