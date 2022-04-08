#alvo – rosto e rosto – alvo
#distrator – rosto e rosto – distrator
#alvo – distrator 

source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
setwd("/Users/pdealcan/Documents/github/sabara/data/dataFilter")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
path_out = "/Users/pdealcan/Documents/github/sabara/reports/report6/"

a = visualizer(file_list[[1]], plot = FALSE)

#Definindo alternancias
RBE = c('....R.', '....B.E.')
RBD = c('....R.', '....B.D.')
BER = c('....B.E.', '....R.')
BDR = c('....B.D.', '....R.')
BEBD = c('....B.E.', '....B.D.')
BDBE = c('....B.D.', '....B.E.')

a %>%
  group_by(Presented.Stimulus.name) %>%
  summarise(RBE = computeAlternancia(variable, RBE),
         RBD = computeAlternancia(variable, RBD),
         BER = computeAlternancia(variable, BER),
         BDR = computeAlternancia(variable, BDR),
         BEBD = computeAlternancia(variable, BEBD),
         BDBE = computeAlternancia(variable, BDBE)
  ) %>% dvisu()
computeAlternancia(a$variable, RBE)

