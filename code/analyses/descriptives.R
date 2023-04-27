source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library(data.table)
library(dplyr)
library(ggridges)

df = fread("/Users/pdealcan/Documents/github/dataSabara/masterFile/masterFile.csv")

df %>%
  distinct(Recording.name, .keep_all = TRUE) %>%
  summarise(minTimeJACARS = min(timeBetweenJAandCARS, na.rm = TRUE),
            maxTimeJACARS = max(timeBetweenJAandCARS, na.rm = TRUE),
            timeBetweenJAandCARS = mean(timeBetweenJAandCARS, na.rm = TRUE))

df %>%
  filter(timeBetweenJAandCARS == 849) %>%
  select(Recording.name)

df %>% 
  distinct(Recording.name, .keep_all = TRUE) %>%
  filter(timeBetweenJAandCARS == 323) %>% select(Recording.name)
#Primeiro são filtrados os participantes conforme o critério de inclusão
dfFiltered = df %>%
    filter(!str_detect(Presented.Stimulus.name, 'BL_')) %>%
    filter(filterDurations == FALSE) %>%
    filter(filterCutoffs == FALSE) %>%
    filter(filterConditions == FALSE)


dfSummarise = function(df){
  df %>%
      
}
