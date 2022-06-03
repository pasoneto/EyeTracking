source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
library("stringr")
library("ggridges")
setwd("/Users/pdealcan/Documents/github/dataSabara/allData")

########## Lendo e limpando o banco de dados
files = list.files()
file_list = lapply(files, function(i){read.table(file = i, sep = '\t', header = TRUE)})
file_list = file_list[1:(length(file_list)-1)] #remove last problematic file

#Selecting columns of interest by trial type
trials = c("IJA_A3_B1_D","IJA_A1_B2_E","RJA_A2_B1_E","RJA_A1_B2_D","RJA_A2_B2_E","IJA_A1_B1_D","IJA_A3_B1_E","RJA_A2_B1_D","IJA_A1_B1_E")
colunas = c()
foco = c("Rosto.", "Brinquedo.Direita.", "Brinquedo.Esquerda.")
for(i in trials){
 for(k in foco){
  colunas = c(colunas, paste("AOI.hit..", i, "...", k, sep = ""))
 }
}
addedNames = c("AOI.hit..RJA_A2_B1_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B1_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B1_E...Rosto..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B2_E...Rosto..1")
colunas = c(colunas, addedNames)

processParticipant(file_list[[1]], trials, colunas)

#Processing all participants
allParticipants = lapply(file_list, function(i){processParticipant(i, trials, colunas)})
allParticipants = bind_rows(allParticipants)

#Check 1 child
allParticipants %>%
  filter(Recording.name == "RP105IP") %>%
  ggplot(aes(y = variable, x = 0, color = variable))+
    facet_wrap(~Presented.Stimulus.name+trialIndex, scale = "free")+
    geom_errorbar(aes(xmin = Recording.time.begin, xmax = Recording.time.end), width = 0, size = 3)+
    theme(legend.position = "None", strip.text.y = element_blank()) +
    ylab("Stimulus focused")+
    xlab("Elapsed time (ms)")

op = allParticipants %>%
  filter(Recording.name == "RP105IP") %>%
  group_by(Presented.Stimulus.name, trialIndex) %>%
  filter(NROW(variable) > 1) %>% 
  summarise(RD = alternanciaCount(variable, "R", "D"),
            RE = alternanciaCount(variable, "R", "E"),
            DR = alternanciaCount(variable, "D", "R"),
            ER = alternanciaCount(variable, "E", "R"),
            target = unique(target),
            Recording.name = unique(Recording.name)) %>%
  ungroup() %>%
  select(Presented.Stimulus.name, target, RD, RE, DR, ER)

op[op==0]<-NA

op %>% dvisu()
  select(Presented.Stimulus.name, RD, RE, DR, ER) %>%
  heatmap(scale="column", labRow = op$target, Colv = NA, Rowv = NA)

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

#Visualização geral de alternancias
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
