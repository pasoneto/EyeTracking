source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
setwd("/Users/pdealcan/Documents/github/sabara/data/dataFilter")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
path_out = "/Users/pdealcan/Documents/github/sabara/reports/report5/"

visualizer(file_list[[1]])

for(i in 1:length(file_list)){
  visualizer(file_list[[i]])
  file_name = paste(path_out, "graph_visu", i, ".png", sep="")
  ggsave(file_name, width = 30, height = 20, units = "cm")
}

