library("dplyr")
library("data.table")
library("ggplot2")
source("/Users/pdealcan/Documents/github/doc_suomi/code/utils.R")

fixationIndexer = function(x){
  count = 0
  indexes = c()
  for(i in 1:(length(x))){
    if(i == length(x)){
      if(x[i] == x[i-1]){
        indexes = c(indexes, count)
      } else{
        count = count+1
        indexes = c(indexes, count)
      }
      return(indexes)
    } else{
      a = x[i] == x[i+1]
      if(a == TRUE){
        indexes = c(indexes, count) 
      } else{
        indexes = c(indexes, count) 
        count = count+1
      }
    }
  }
  return(indexes)
}

nextFixationCalc = function(eventEnd, NextEventStart){
  nextFix = c()
    for(i in 1:(length(eventEnd)-1)){
      nextFix = c(nextFix, NextEventStart[i+1] - eventEnd[i])
    }
  nextFix = c(nextFix, NA)
  return(nextFix)
  }
