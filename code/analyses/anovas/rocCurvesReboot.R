source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(pROC)
library(caret)
library(pROC)
library(data.table)
library(dplyr)
library(ggridges)
library(xtable)

otherTEA = "TD"

#Alternancia Non matched
df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")
df = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE) %>%
    filter(tea %in% c("TEA", otherTEA))

nOut = c("CR518IP", "CR736IP", "SM770IP", "SM179IP", "SM175IP", "SM183IP", "SM759IP", "SM761IP", "SM764IP", "SM758IP", "SM765IP", "SM763IP", "SM773IP", "SM766IP", "SM196IP", "FS73IP", "FS84IP", "FS467IP", "FS64IP", "MR268IP", "MR294IP", "MR286IP", "RP104IP", "RP96IP", "RP110IP", "RP99IP", "SI421IP", "SI422IP", "SF141IP", "SF224IP", "SF144IP", "SF214IP", "SF380IP", "SF146IP", "SF147IP")
df = df %>%
  filter(!Recording.name %in% nOut)

df = computeProportions(df)

df = df %>% 
  group_by(variable, Recording.name, tea) %>%
  summarise(value = mean(value)) %>%
  arrange(Recording.name) %>%
  filter(tea %in% c("TEA", "TD"))
  
df = df %>%
  dcast(Recording.name+tea ~ variable)

df$tea <- factor(df$tea)
mylogit <- glm(tea ~ fundoProportion + rostoProportion + distractorProportion, data = df, family = "binomial")

#td-asd #Face #Background #Distractor toy
df$pred <- predict(mylogit, newdata = df, type = "response")

rocObj = roc(df$tea, df$pred, plot = TRUE)
rocObj
#Both proportion

#0.652
ggroc(rocObj) +
  geom_abline(intercept = c(0, 1), color = "darkgrey", linetype = "dashed")+
  theme_bw()

ggsave("/Users/pdealcan/Documents/github/sabara/reports/2023/report17/roc.png")
