source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
file_list = file_list[1:(length(file_list)-1)] #remove last problematic file

#Processing all participants
allParticipants = lapply(file_list, function(i){processParticipant(i, trials, colunas)})
allParticipants = bind_rows(allParticipants)

#Olhou pra fundo? sim nao
#Quanto tempo passou entre fixação? Limite x
#Alternancia é quando ha transicao direta, com tempo menor que x
alternancias = allParticipants %>%
  group_by(Presented.Stimulus.name, trialIndex, Recording.name) %>%
  filter(NROW(variable) > 1) %>% 
  summarise(RD = alternanciaCount(variable, "R", "D"),
            RE = alternanciaCount(variable, "R", "E"),
            DR = alternanciaCount(variable, "D", "R"),
            ER = alternanciaCount(variable, "E", "R"),
            condition = stri_sub(unique(Presented.Stimulus.name), 1, 3),
            target = unique(target),
            Recording.name = unique(Recording.name))

alternancias %>%
  filter(condition == "IJA") %>%
  group_by(Recording.name, condition, target) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>%
  melt(id.vars = c("condition", "target", "Recording.name")) %>%
  ggplot(aes(y = target, x = value, fill = target)) +
    facet_wrap(~variable)+
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

alternancias %>%
  filter(condition == "RJA") %>%
  group_by(Recording.name, condition, target) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>%
  melt(id.vars = c("condition", "target", "Recording.name")) %>%
  ggplot(aes(y = target, x = value, fill = target)) +
    facet_wrap(~variable)+
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

#Incluir fundo
#Verificar numero de fixacoes dentro de uma fixacao
