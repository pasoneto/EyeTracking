source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
setwd("/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL")
library(dplyr)

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)

alternancias = bind_rows(file_list) %>% select(!numberDuplicated)
a$tea = unlist(lapply(a$Recording.name, tagDiagnostico))
#Olhou pra fundo? sim nao
#Quanto tempo passou entre fixação? Limite x
#Alternancia é quando ha transicao direta, com tempo menor que x
alternancias = alternancias %>%
  group_by(Presented.Stimulus.name, Recording.name) %>%
  filter(NROW(variable) > 2) %>% 
  summarise(RD = alternanciaCount(variable, "R", "D"),
            RE = alternanciaCount(variable, "R", "E"),
            DR = alternanciaCount(variable, "D", "R"),
            ER = alternanciaCount(variable, "E", "R"),
            ER = alternanciaCount(variable, "E", "R"),
            tea = unique(tea),
            condition = stri_sub(unique(Presented.Stimulus.name), 1, 3),
            target = unique(target),
            Recording.name = unique(Recording.name))

#alternancias %>%
#  filter(condition == "RJA") %>%
#  group_by(Recording.name, condition, target, tea) %>%
#  summarise(RD_count = sum(RD), 
#            RE_count = sum(RE),
#            DR_count = sum(DR),
#            ER_count = sum(ER)) %>%
#  melt(id.vars = c("condition", "target", "Recording.name", "tea")) %>%
#  ggplot(aes(y = target, x = value, fill = target)) +
#    facet_wrap(~variable+tea)+
#    geom_density_ridges() +
#    theme_ridges() + 
#    theme(legend.position = "none")
#
#alternancias %>%
#  filter(condition == "IJA") %>%
#  group_by(Recording.name, condition, target, tea) %>%
#  summarise(RD_count = sum(RD), 
#            RE_count = sum(RE),
#            DR_count = sum(DR),
#            ER_count = sum(ER)) %>%
#  melt(id.vars = c("condition", "target", "Recording.name", "tea")) %>%
#  ggplot(aes(y = target, x = value, fill = target)) +
#    facet_wrap(~variable+tea)+
#    geom_density_ridges() +
#    theme_ridges() + 
#    theme(legend.position = "none")
#
#descriptive = alternancias %>%
#  group_by(tea, condition, target) %>%
#  summarise(RD_count = sum(RD), 
#            RE_count = sum(RE),
#            DR_count = sum(DR),
#            ER_count = sum(ER))
#
#library(xtable)
#xtable(descriptive, type = "latex")
#
#alternancias %>%
#  filter(condition == "IJA") %>%
#  group_by(tea, condition, target) %>%
#  summarise(RD_count = sum(RD), 
#            RE_count = sum(RE),
#            DR_count = sum(DR),
#            ER_count = sum(ER)) %>% 
#  melt(id.vars = c("condition", "target", "tea")) %>%
#  ggplot(aes(y = value, x = target, fill = target)) +
#    facet_wrap(~variable+tea, scale = "free")+
#    geom_bar(stat="identity")
#
#ggsave('/Users/pdealcan/Documents/github/sabara/reports/report13/alternanciaIJA.png')
#
#alternancias %>%
#  filter(condition == "RJA") %>%
#  group_by(tea, condition, target) %>%
#  summarise(RD_count = sum(RD), 
#            RE_count = sum(RE),
#            DR_count = sum(DR),
#            ER_count = sum(ER)) %>% 
#  melt(id.vars = c("condition", "target", "tea")) %>%
#  ggplot(aes(y = value, x = target, fill = target)) +
#    facet_wrap(~variable+tea, scale = "free")+
#    geom_bar(stat="identity")
#
#ggsave('/Users/pdealcan/Documents/github/sabara/reports/report13/alternanciaRJA.png')
#
##Pegar outras timelines 
##Analisar alternancias, numero e tempo de fixacão por timeline order (video 1, 2, etc...).
##Colocar diagnóstico
##1a pergunta: Criancas com tea tem pouca fixação => Qual é a porcentagem de tempo que cada grupo (tea nao tea) olha para a tela. (tempo olhando tarefa/tempo total tarefa; dividir por tipo de crianca e tarefa)
##Ranking das rois mais olhadas para cada tipo de crianca
#
##Depois:
##Computar tempo médio de transição entre cada objeto: para estabelecer threshold
##Verificar numero de fixacoes dentro de uma fixacao
