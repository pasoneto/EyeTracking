source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)
library(rstatix)
library(pROC)

dfGeo = fread("/Users/pdealcan/Documents/github/sabara/details_experiment/fixationDurGeopref/totalFixation.csv")
colnames(dfGeo) = c("Recording.name", "Recording.name2", "tea", "ellipse", "nonSocial", "social", "average", "median", "sum", "totalTime", "rawTotalTime")
dfGeo = dfGeo %>%
  select(Recording.name, nonSocial, social, totalTime)

dfGeo$Recording.name = str_replace_all(dfGeo$Recording.name, "-2", "")
dfGeo$Recording.name = str_replace_all(dfGeo$Recording.name, "-3", "")

dfGeo = dfGeo %>% na.omit()

#General info about all participants
infoParticipant = fread("/Users/pdealcan/Documents/github/sabara/details_experiment/infoParticipants.csv")
colnames(infoParticipant) = c("codigo", "Recording.name", "Codinome2", "dataNascimento", "sexo", "CARS", "dataCARS", "ageCARS",     "pontuacaoCARS", "tea", "Consulta", "grupo",    "JA", "dataJA",  "ageJA",     "Obs.JA",   "GeoPref",  "dataGeo",      "Obs.GeoPref",       "pontABEP", "clasABEP", "idadeMAE", "escolPAI", "escolMAE", "idadeCRIANCA",      "regiaoCRIANCA",     "regiaoResponsavel", "RENDA", "numPESSOAS", "qtdePESSOAStrabalham", "tipoMORADIA", "saudeCRIANCA", "moradia", "GEOPREF2", "IntervalDurationAverage", "IntervalDurationMedian", "IntervalDurationCount", "IntervalDurationTotal Time of Interest Duration", "IntervalDurationTotal Recording Duration", "TotalFixDurationEllipse", "TotalFixDurationNon-social", "TotalFixDurationSocial", "TotalFixDurationAverage", "TotalFixDurationMedian", "TotalFixDurationSum", "TotalFixDurationTotal Time of Interest Duration", "TotalFixDurationTotal Recording Duration", "AvgFixationDurEllipse", "AvgFixationDurNon-social", "AvgFixationDurSocial", "AvgFixationDurAverage", "AvgFixationDurMedian", "AvgFixationDurTotal Time of Interest Duration", "AvgFixationDurTotal Recording Duration")

geoNames = unique(dfGeo$Recording.name)
jaNames = unique(infoParticipant$Recording.name)
geoNames[!geoNames %in% jaNames]

#Analysis
df = merge(dfGeo, infoParticipant, by = "Recording.name", all.x = FALSE, all.y = FALSE)

#Filter. At least 50%
df$grupo = tagDiagnostico(df$grupo)
df$grupo = str_replace_all(df$grupo, "TEA", "ASD")

df %>%
  na.omit() %>%
  filter(grupo != "other") %>%
  mutate(nonSocial = nonSocial/(social + nonSocial)) %>%
  ggplot(aes(x = factor(grupo, level=c('ASD', 'TD', 'nonTD')), y = nonSocial, fill = grupo)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9, width = 0.2) +
    xlab("")+
    ylab("Proporcão - Estímulo não-social")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report15/nonSocial.png")


threshClassifier = function(x, rocObj, thresh){
  if(rocObj$direction == ">"){
    if(x >= thresh){
      return("ASD")
    } else {
      return("TD")
    }
  }
  if(rocObj$direction == "<"){
    if(x <= thresh){
      return("ASD")
    } else {
      return("TD")
    }
  }
}

finalClassifier = function(df, thresh){
  rocObj = roc(df$grupo, df$nonSocial, plot = FALSE) 
  value = df$nonSocial
  predicted = unlist(lapply(value, function(x){threshClassifier(x, rocObj, thresh)}))
  a = c()
  a$predicted = predicted
  a$true = df$grupo
  acc = sum(a$predicted == a$true)/length(a$predicted)
  conf_matrix = table(a$predicted,a$true)
  sensitivity = sensitivity(conf_matrix)
  specificity = specificity(conf_matrix)
  a$acc = (sensitivity+specificity)/2
  a$rocObj = rocObj
  a$sensitivity = sensitivity
  a$specificity = specificity
  a$direction = rocObj$direction
  return(a)
}

decisionVisualizer = function(df){
  rocObj = roc(df$grupo, df$nonSocial, plot=FALSE)
  accs = c()
  senss = c()
  specs = c()
  threshs = rocObj$thresh[is.finite(rocObj$thresh)]
  thresh = seq(0, 1, 0.01)
  for(i in threshs){
    accs = c(accs, finalClassifier(df, i)$acc)
    senss = c(senss, finalClassifier(df, i)$sensitivity)
    specs = c(specs, finalClassifier(df, i)$specificity)
  }
  dfFinal = data.frame(threshs = threshs,
                       trueN = specs,
                       trueP = senss,
                       accs = accs,
                       direction = rocObj$direction
  )
  dfFinal = dfFinal
  return(dfFinal)
}

dfa = df %>%
  filter(!is.na(grupo)) %>%
  mutate(nonSocial = nonSocial/(social + nonSocial)) %>%
  filter(!grupo %in% c("other", "nonTD"))

rocObj = roc(dfa$grupo, dfa$nonSocial, plot=TRUE)
auc = 0.78

ggroc(rocObj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('AUC = ', auc)) +
  theme_minimal()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report15/rocCurve.jpg")

dfFinal = decisionVisualizer(dfa)

#ROC geopref. Threshold of .69
finalClassifier(dfa, 0.69)$acc
finalClassifier(dfa, 0.69)$sensitivity
finalClassifier(dfa, 0.69)$specificity

#threshold: 0.69	
#specificity: 0.96	
#sensitivity: 0.24
#accuracy: 60%

#Best threshold (in terms of accuracy)
finalClassifier(dfa, 0.36)$acc
finalClassifier(dfa, 0.36)$sensitivity
finalClassifier(dfa, 0.36)$specificity

#threshold: 0.36
#accuracy: 0.76%
#sensitivity: 0.82
#specificity: 0.69

#ANOVA
df = df %>%
  mutate(nonSocial = nonSocial/(social + nonSocial)) %>%
  filter(grupo != "other")

df %>%
  group_by(grupo) %>%
  summarise(mean = mean(nonSocial), 
            sd = sd(nonSocial))

library(effectsize)
res.aov <- aov(nonSocial ~ grupo, data = df)
m <- lm(nonSocial~grupo, data = df)
eta_squared(m, partial = FALSE)

TukeyHSD(res.aov, conf.level=.95)

###Comparison percentage and CARS
df$pontuacaoCARS = str_replace_all(df$pontuacaoCARS, ",", ".")
df$pontuacaoCARS = as.numeric(df$pontuacaoCARS)

df %>%
  filter(!is.na(pontuacaoCARS)) %>%
  ggplot(aes(x = nonSocial, y = pontuacaoCARS, colour = grupo))+
    geom_smooth(method = "lm", se=FALSE)+
    geom_point() + 
    xlab("CARS score")+
    ylab("% fixation on non-social stimuli")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report15/correlationCARS.png")

df = df %>% na.omit()
cor.test(df$pontuacaoCARS, df$nonSocial)
