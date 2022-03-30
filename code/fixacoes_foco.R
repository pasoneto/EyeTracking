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

visualizeTotalTime = function(df){
  df %>%
    group_by(variable, Presented.Stimulus.name) %>%
    summarise(total = sum(fixationDuration)/1000) %>%
    arrange(Presented.Stimulus.name) %>%
    mutate(target = str_sub(Presented.Stimulus.name, -1)) %>%
    ggplot(aes(x = variable, y = total, fill = variable))+
      facet_wrap(~target) +
      geom_bar(stat="identity")
}

#Time spent on each object, by trial
for(i in 1:length(file_list)){
  a = visualizer(file_list[[i]])
  visualizeTotalTime(a) 
  file_name = paste(path_out, "graph_visu", i, ".png", sep="")
  ggsave(file_name, width = 30, height = 20, units = "cm")
}

