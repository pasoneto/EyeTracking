trainTestSplit = function(df){
  #Getting names of participants for stratified sampling
  dfNonTEA = df %>% filter(tea == 'TD')
  dfTEA = df %>% filter(tea == 'TEA')

  nonTEANames = unique(dfNonTEA$Recording.name) 
  TEANames = unique(dfTEA$Recording.name) 

  N_nonTEA_train = round(length(nonTEANames)*0.7)
  N_TEA_train = round(length(TEANames)*0.7)

  N_nonTEA_test = round(length(nonTEANames)*0.3)
  N_TEA_test = round(length(TEANames)*0.3)

  trainTEANames = sample(TEANames, N_TEA_train, replace = FALSE)
  testTEANames = TEANames[!TEANames %in% trainTEANames]

  trainNonTEANames = sample(nonTEANames, N_nonTEA_train, replace = FALSE)
  testNonTEANames = nonTEANames[!nonTEANames %in% trainNonTEANames]

  #Filtering original dataset
  train = df %>%
    filter(Recording.name %in% c(trainTEANames, trainNonTEANames))

  test = df %>%
    filter(Recording.name %in% c(testTEANames, testNonTEANames))
  
  return(list("train" = train, "test" = test))
}

trainTestSplit2 = function(df){
  #Getting names of participants for stratified sampling
  dfNonTEA = df %>% filter(tea == 'TD')
  dfTEA = df %>% filter(tea == 'TEA')

  nonTEANames = unique(dfNonTEA$Recording.name) 
  TEANames = unique(dfTEA$Recording.name) 

  N_nonTEA_train = round(length(nonTEANames)*0.7)
  N_TEA_train = round(length(TEANames)*0.7)

  N_nonTEA_test = round(length(nonTEANames)*0.3)
  N_TEA_test = round(length(TEANames)*0.3)

  trainTEANames = sample(TEANames, N_TEA_train, replace = FALSE)
  testTEANames = TEANames[!TEANames %in% trainTEANames]

  trainNonTEANames = sample(nonTEANames, N_TEA_train, replace = FALSE)
  testNonTEANames = nonTEANames[!nonTEANames %in% trainNonTEANames]

  #Filtering original dataset
  train = df %>%
    filter(Recording.name %in% c(trainTEANames, trainNonTEANames))

  test = df %>%
    filter(Recording.name %in% c(testTEANames, testNonTEANames))
  
  return(list("train" = train, "test" = test))
}

#Test results is the original dataframe containing the groundTruth and the predicted probabilities
#as $tea and $teaProb
extractROCs = function(testResults){
  rocs = c()
  for(k in 1:length(testResults)){
    roc_obj <- roc(testResults[[k]]$tea, testResults[[k]]$teaProb, direction = "<")
    auc_score = auc(roc_obj)
    ## Area under the curve
    roc_df <- data.frame(
      TPR=rev(roc_obj$sensitivities), 
      FPR=rev(1 - roc_obj$specificities)
    )
    roc_df$run = k
    roc_df$auc = auc_score
    rocs[[k]] = roc_df
  }
  rocs = bind_rows(rocs)
  return(rocs)
}

