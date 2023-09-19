source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)
library(rstatix)
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

#Primeiro são filtrados os participantes conforme o critério de inclusão
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE)

df %>% 
  filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
  distinct(Recording.name, .keep_all = TRUE) %>%
  group_by(sexo, tea) %>%
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

df = computeAlternancias(df)
df$condition = as.factor(df$condition)
df$tea = as.factor(df$tea)

df = df %>%
  filter(tea %in% c("TEA", "TD", "nonTD"))

#mixed-designs ANOVA results
res.aov <- anova_test(
  data = df, dv = value, wid = Recording.name,
  between = tea, within = c(condition, variable))

aovResult = get_anova_table(res.aov)

#aovResult <- aov(value ~ condition*tea*variable, data = df)
#summary(aovResult)

#Visualizing TEA effect on alternancias. 
#Mean number of alternancias per diagnostic and trial
df %>%
  group_by(tea) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))+
    theme_minimal()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report13/teaMainAlternancia.png")

#Visualizing effect of variable
df %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = variable, y = mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))+
    theme_minimal()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report13/alternanciaVariable.png")

#Visualizing effect of condition
df %>%
  group_by(condition) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = condition, y = mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report13/alternanciaCondition.png")

#Visualizing interaction of condition and variable
df %>%
  group_by(condition, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = condition, y = mean, color = condition))+
    facet_wrap(~variable)+
    geom_point(position = position_dodge(0.5))+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = position_dodge(0.5))+
    theme_bw()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report13/conditionVariable.png")

#Visualizing interaction of condition, variable and TEA
df %>%
  group_by(condition, variable, tea) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean, color = condition))+
    facet_wrap(~variable)+
    geom_point(position = position_dodge(1))+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = position_dodge(1))+
    theme_bw()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report13/conditionVariableTea.png")

#Visualizing interaction between tea and variable
df %>%
  group_by(tea, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean))+
    facet_wrap(~variable)+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report9/teaVariableAlternancia.png")


#Propotion
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TEA", "TD", "nonTD"))

#source("./matchedSample.R")
#matchedParticipants = subSample
#df = df %>%
#  filter(Recording.name %in% matchedParticipants)

#df = df %>%
#  filter(tea %in% c("TEA", "TD"))

#df = df %>%
#  filter(Recording.name %in% matchedParticipants)

df = computeProportions(df)

df$condition = as.factor(df$condition)
df$tea = as.factor(df$tea)
#aovResult <- aov(value ~ condition*tea*variable, data = df)
#summary(aovResult)
#print(xtable(summary(aovResult), type = "latex"))

#mixed-designs ANOVA results
res.aov <- anova_test(
  data = df, dv = value, wid = Recording.name,
  between = tea, within = c(condition, variable))

aovResult = get_anova_table(res.aov)


#variable, tea*variable, condition*variable
#New graph
pd = position_dodge(width = 0.8, preserve = "total")

df %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = variable, y = mean))+
    geom_point(position = pd)+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = pd)+
    theme_minimal()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report13/variableProportion.png")

df %>%
  group_by(variable, tea) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = tea, y = mean))+
    facet_wrap(~variable)+
    geom_point(position = pd)+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = pd)+
    theme_bw()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report13/teaVariableProportion.png")

df %>%
  group_by(condition, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = condition, y = mean))+
    facet_wrap(~variable)+
    geom_point(position = pd)+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder), position = pd)+
    theme_bw()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report13/conditionVariableProportion.png")

#Visualizing variable effect 
df %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = variable, y = mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report9/variableProportion.png")

#Visualizing interaction between condition and variable 
df %>%
  group_by(condition, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = condition, y = mean))+
    facet_wrap(~variable, scale = "free")+
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder))

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report9/conditionVariableProportion.png")
