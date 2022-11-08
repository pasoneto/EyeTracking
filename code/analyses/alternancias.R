source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
setwd("/Users/pdealcan/Documents/github/dataSabara/processedParticipantAll")
library(plyr)
########## Lendo e limpando o banco de dados
files = list.files()

file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)

allParticipants <- ldply(file_list, data.frame)
filterColumns = c("Presented.Stimulus.name", "trialIndex", "Eye.movement.type.index", "Computer.timestamp.begin", "Computer.timestamp.end", "Recording.time.begin", "Recording.time.end", "Gaze.event.duration", "Eye.movement.type", "variable", "value", "pupil.right", "pupil.left", "target", "Recording.name")
allParticipants = allParticipants %>% select(filterColumns)

library(dplyr)
detach(package:plyr)

##Adicionando diagnostico
diags = c("FS9IP", "FS24IP", "FS65IP", "FS76IP", "FS93IP", "RP100IP", "SM114IP", "SM118IP", "MR135IP", "MR136IP", "MR140IP", "SF142IP", "SF234IP", "SF246IP", "MR281IP", "MR285IP", "MR299IP", "CR348IP", "CR356IP", "RP373IP", "SI429IP", "SI430IP", "SM462IP", "MP466IP", "CR475IP", "MR534IP", "CR559IP", "SM581IP", "MR589IP", "SI619IP", "RP657IP", "CR683IP", "MR688IP", "MR691IP", "SF728IP", "SI776IP", "SM787IP")
allParticipants$tea = FALSE
allParticipants$tea[allParticipants$Recording.name %in% diags] <- TRUE


unique(allParticipants$variable)

#Olhou pra fundo? sim nao
#Quanto tempo passou entre fixação? Limite x
#Alternancia é quando ha transicao direta, com tempo menor que x
alternancias = allParticipants %>%
  group_by(Presented.Stimulus.name, trialIndex, Recording.name) %>%
  filter(!is.na(trialIndex)) %>%
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

dadosAnova = alternancias %>%
  group_by(Recording.name, condition, target, tea) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>%
  melt(id.vars = c("condition", "target", "Recording.name", "tea")) %>%
  group_by(condition, Recording.name, tea, variable) %>%
    summarise(count = sum(value))

write.csv(dadosAnova, '../../dataSabara/alternanciaDadosANOVA.csv')


alternancias %>%
  filter(condition == "RJA") %>%
  group_by(Recording.name, condition, target, tea) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>%
  melt(id.vars = c("condition", "target", "Recording.name", "tea")) %>%
  ggplot(aes(y = target, x = value, fill = target)) +
    facet_wrap(~variable+tea)+
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

alternancias %>%
  filter(condition == "IJA") %>%
  group_by(Recording.name, condition, target, tea) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>%
  melt(id.vars = c("condition", "target", "Recording.name", "tea")) %>%
  ggplot(aes(y = target, x = value, fill = target)) +
    facet_wrap(~variable+tea)+
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

#allParticipants %>% 
#  filter(Recording.name == "SI694IP") %>%
#  ggplot(aes(y = variable, x = 0, color = variable))+
#    facet_wrap(~Presented.Stimulus.name+trialIndex, scale = "free")+
#    geom_errorbar(aes(xmin = Recording.time.begin, xmax = Recording.time.end), width = 0, size = 3)+
#    theme(legend.position = "None", strip.text.y = element_blank()) +
#    ylab("Stimulus focused")+
#    xlab("Elapsed time (ms)")

descriptive = alternancias %>%
  group_by(tea, condition, target) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER))

library(xtable)
xtable(descriptive, type = "latex")

alternancias %>%
  filter(condition == "IJA") %>%
  group_by(tea, condition, target) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>% 
  melt(id.vars = c("condition", "target", "tea")) %>%
  ggplot(aes(y = value, x = target, fill = target)) +
    facet_wrap(~variable+tea, scale = "free")+
    geom_bar(stat="identity")

ggsave('/Users/pdealcan/Documents/github/sabara/reports/report13/alternanciaIJA.png')

alternancias %>%
  filter(condition == "RJA") %>%
  group_by(tea, condition, target) %>%
  summarise(RD_count = sum(RD), 
            RE_count = sum(RE),
            DR_count = sum(DR),
            ER_count = sum(ER)) %>% 
  melt(id.vars = c("condition", "target", "tea")) %>%
  ggplot(aes(y = value, x = target, fill = target)) +
    facet_wrap(~variable+tea, scale = "free")+
    geom_bar(stat="identity")

ggsave('/Users/pdealcan/Documents/github/sabara/reports/report13/alternanciaRJA.png')

#Pegar outras timelines 
#Analisar alternancias, numero e tempo de fixacão por timeline order (video 1, 2, etc...).
#Colocar diagnóstico
#1a pergunta: Criancas com tea tem pouca fixação => Qual é a porcentagem de tempo que cada grupo (tea nao tea) olha para a tela. (tempo olhando tarefa/tempo total tarefa; dividir por tipo de crianca e tarefa)
#Ranking das rois mais olhadas para cada tipo de crianca

#Depois:
#Computar tempo médio de transição entre cada objeto: para estabelecer threshold
#Verificar numero de fixacoes dentro de uma fixacao
