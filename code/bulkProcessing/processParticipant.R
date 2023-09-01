source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
library("readxl")
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")
directory = "/Users/pdealcan/Documents/github/dataSabara/allIndexedFINAL/"
directoryOut = "/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL/"
library("tidyselect")

setwd(directory)

functionMerge = function(i){
  nameParticipant = unique(i$Recording.name)
  a = processParticipant(i, trials, colunas) #Count duration of each fixation
  if(!is.null(a)){
    a = duplicatedFundoWrap(a) #Remove duplicated fundo
    print(unique(a$Recording.name))
    write.csv(a, paste(directoryOut, nameParticipant, '.csv', sep=""))
    return("fine")
  } else {
    print("No data")
    return(nameParticipant)
  }
}

file_list = list.files()
file_list = lapply(file_list, fread)

logErrorsList = c()
for(l in file_list){
  logErrors = functionMerge(l)
  if(logErrors != "fine"){
    logErrorsList = c(logErrors, logErrorsList)
  }
}

#for(l in file_list){
#  dif = length(unique(colnames(l))) == length(colnames(l))
#  if(!dif){
#    print(unique(l$Recording.name))
#  }
#}
#fread("/Users/pdealcan/Documents/github/dataSabara/allIndexedFINAL/CR736IP.csv")
print("Problematic participants below. They have no data at all. Only unclassified/eyes not found. Some fixations, but only during calibration")
print(logErrorsList)

#pFiles = c("SM462IP",   "SI421IP",   "SF226IP",   "MT706IP-2", "MR324IP",  "MR313IP", ""FS12IP", "CR632IP", "CR397IP")
#fread(paste(pFiles[1], ".csv", sep=""))

setwd(directoryOut)

participantsToTrim = c("FS1IP", "FS2IP", "FS3IP", "FS4IP", "FS5IP", "FS6IP", "FS7IP", "FS8IP", "FS9IP", "FS10IP", "FS11IP", "FS13IP", "FS14IP", "FS15IP", "FS16IP", "FS17IP", "FS18IP", "FS19IP", "FS20IP", "FS21IP", "FS22IP", "FS23IP", "FS25IP", "FS26IP", "FS27IP", "MP28IP", "MP30IP", "MP31IP", "MP32IP", "MP33IP", "MP34IP", "MP35IP", "MP36IP", "MP37IP", "MP38IP", "MP40IP", "MP41IP", "MP42IP", "MP43IP", "MP44IP", "MP45IP", "MP46IP", "MP47IP", "MP48IP", "MP49IP", "MP51IP", "MP52IP", "MP53IP", "MP55IP", "MP56IP", "FS58IP", "FS59IP", "FS61IP", "FS62IP", "FS63IP", "FS64IP", "FS65IP", "FS66IP", "FS67IP", "FS68IP", "FS69IP", "MP70IP", "FS72IP", "FS73IP", "FS75IP", "FS77IP", "MP78IP", "FS81IP", "FS82IP", "FS83IP", "FS84IP", "FS85IP", "FS86IP", "FS87IP", "MP88IP", "FS89IP", "FS90IP", "FS91IP", "FS92IP", "FS94IP", "SM112IP", "SM113IP", "SM114IP", "SM117IP", "SM119IP", "SM120IP", "SM121IP", "SM122IP", "CR131IP", "MP164IP", "SF149IP", "RP163IP", "FS60IP", "MP39IP", "SM115IP")
videosToTrim = c("RJA_A2_B1_E", "RJA_A1_B2_D", "RJA_A2_B2_E", "RJA_A2_B1_D")
timesToTrim = c(7, 6.18, 7, 7)

for(k in participantsToTrim){
  pName = paste(k, ".csv", sep = "")
  a = fread(pName)
  a = a %>% select(!V1)
  for(i in 1:length(videosToTrim)){
    a = trimDurations(a, videosToTrim[i], timesToTrim[i])
  }
  print(pName)
  write.csv(a, paste(directoryOut, pName, sep = ""))
}

#Check duplicated fixations
#"FS2IP" 
#"FS12IP"
