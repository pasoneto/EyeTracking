source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
setwd("/Users/pdealcan/Documents/github/sabara/data/newest")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
path_out = "/Users/pdealcan/Documents/github/sabara/reports/report5/"

visualizer(file_list[[1]])

file_list[[2]] %>%
  filter(Presented.Stimulus.name == "RJA_A2_B2_E") %>%
  select(Presented.Stimulus.name, Recording.timestamp, matches("RJA_A2_B2_E")) %>%
  dvisu()

for(i in 1:length(file_list)){
  visualizer(file_list[[i]])
  file_name = paste(path_out, "graph_visu", i, ".png", sep="")
  #ggsave(file_name, width = 30, height = 20, units = "cm")
}

#Filtering, computing trial index
file_list[[4]] %>%
  filter(Presented.Stimulus.name == "RJA_A2_B2_E") %>%
  select(Presented.Stimulus.name, Computer.timestamp, matches("RJA_A2_B2_E")) %>%
  arrange(AOI.hit..RJA_A2_B2_E...RJA_A2_B2_E1.) %>%
  dvisu()
