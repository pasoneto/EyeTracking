source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(ggridges)
library(dplyr)

df = fread("../../details_experiment/fixationDurGeopref/totalFixation.csv")
colnames(df) = c("recordingName", "participant", "grupo", "ellipse", "nonSocial", "social", "average", "median", "sum", "totalTimeInterestDur", "totalRecordingDuration")

#Problems: code names don't exist
#Duplicated names of the same participant
df %>%
  filter(duplicated(recordingName))

df$recordingName = str_replace_all(df$recordingName, "-3", "")
df$recordingName = str_replace_all(df$recordingName, "-2", "")

df$tea = unlist(lapply(df$recordingName, tagDiagnostico))

nonSocial = df %>%
  mutate(nonSocial = as.numeric(gsub(",", ".", nonSocial)),
         social = as.numeric(gsub(",", ".", social)),
         totalTimeInterestDur = as.numeric(gsub(",", ".", totalTimeInterestDur))) %>%
  select(recordingName, nonSocial, totalTimeInterestDur, tea) %>%
  mutate(percentNonSocial = nonSocial/totalTimeInterestDur)

df = fread("../../details_experiment/fixationDurGeopref/fixationCount.csv")
colnames(df) = c("recordingName", "participant", "grupo", "ellipse", "nonSocial", "social", "average", "median", "sum", "totalTimeInterestCount", "totalTimeInterestDur", "totalRecordingDur")

df$recordingName = str_replace_all(df$recordingName, "-3", "")
df$recordingName = str_replace_all(df$recordingName, "-2", "")

fixations = df %>%
  mutate(nonSocial = as.numeric(gsub(",", ".", nonSocial)),
         social = as.numeric(gsub(",", ".", social)),
         totalTimeInterestDur = as.numeric(gsub(",", ".", totalTimeInterestDur)),
         totalNFixation = (nonSocial+social)-1,
         fixationPerSec = totalNFixation/totalTimeInterestDur) %>%
  select(recordingName, fixationPerSec)

#Final dataset
df = merge(fixations, nonSocial, by.x="recordingName", by.y="recordingName")

#69% fixation threshold: (ROC) curve, 
#Area Under the Curve (AUC), 
#sensitivity, 
#specificity, 
#positive predictive value (PPV)
#negative predictive value (NPV).

thresholdClass = function(x, thresh){
  output = c()
  for(k in x){
    if(k >= thresh){
      output = c(output, "tea")
    } else {
      output = c(output, "other")
    }
  }
  return(output)
}

teaNonTea = function(x){
  output = c()
  for(k in x){
    if(k == "TEA"){
      output = c(output, "tea")
    } else {
      output = c(output, "other")
    }
  }
  return(output)
}

library(pROC)
library(caret)

  predicted = df %>%
    filter(is.numeric(percentNonSocial)) %>%
    filter(!is.na(percentNonSocial)) %>%
    mutate(teaPredict = thresholdClass(percentNonSocial, 0.75)) %>%
    mutate(tea = teaNonTea(tea))

true = as.factor(predicted$tea)
pred = as.factor(predicted$teaPredict)

specificity(true, pred) #0.75
sensitivity(true, pred) #0.91

true = as.factor(predicted$tea)
pred = predicted$percentNonSocial
plot(roc(true, pred))

data.frame(n = seq(0, 1, 0.01),
           sensitivity = sen, 
           specificity = spe) %>%
  ggplot(aes(x = specificity, y = sensitivity)) +
  geom_point()

#tea tea tea TD  TD
#0.5 0.5 0.5 0.4 0.4
#tea tea tea TD  TD

3/(3+0)

0.5





posPredValue(true, pred) #0.99
negPredValue(true, pred) #0.06

#Pierce:
#98% specificity
#17% sensitivity, 
#81% PPV
#65% NPV

#low false positive rate, 69%
