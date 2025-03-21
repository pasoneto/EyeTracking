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

first = df %>%
  group_by(tea, Recording.name) %>%
  filter(tea %in% c("TEA", "nonTD")) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value)))

model = lm(mean~tea, data = first)
ANOVA=aov(model)

TukeyHSD(ANOVA, conf.level=.95) 

#Post-hoc for TEA-TD on main effect of group on alternancia
#          diff        lwr         upr    p adj
#TEA-TD   -0.1733103 -0.2912578 -0.05536281 0.004076
#TEA-nonTD -0.2351097 -0.424746 -0.04547347 0.016615

#Visualizing TEA effect on alternancias. 
#Mean number of alternancias per diagnostic and trial


#Propotion
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TEA", "TD", "nonTD"))

df = computeProportions(df)

df$condition = as.factor(df$condition)
df$tea = as.factor(df$tea)

df$tea = str_replace_all(df$tea, "TEA", "ASD")
df$variable = str_replace_all(df$variable, "distractorProportion", "Distractor Toy")
df$variable = str_replace_all(df$variable, "targetProportion", "Target Toy")
df$variable = str_replace_all(df$variable, "rostoProportion", "Face")
df$variable = str_replace_all(df$variable, "fundoProportion", "Background")

df %>%
  group_by(tea, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(x = factor(tea, level=c('ASD', 'TD', 'nonTD')), y = mean))+
    #facet_wrap(~variable)+
    facet_grid(~factor(variable, levels=c('Face', 'Target Toy', 'Distractor Toy', 'Background'))) +
    geom_point()+
    geom_errorbar(aes(ymin = mean-stder, ymax = mean+stder)) +
    xlab("")+
    theme_bw()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report14/teaVariableAlternancia.png")

second = df %>%
  filter(tea %in% c("ASD", "TD")) %>%
  filter(variable == "Face") %>%
  group_by(Recording.name, tea, variable) %>%
  summarise(mean = mean(value),
            stder = sd(value)/sqrt(length(value)))

model = lm(mean~tea, data = second)
ANOVA=aov(model)

TukeyHSD(ANOVA, conf.level=.95) 
