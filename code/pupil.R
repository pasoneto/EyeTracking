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
addedNames = c("Fixation.point.X", "AOI.hit..RJA_A2_B1_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B1_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B1_E...Rosto..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Direita..1", "AOI.hit..RJA_A2_B2_E...Brinquedo.Esquerda..1", "AOI.hit..RJA_A2_B2_E...Rosto..1")
colunas = c(colunas, addedNames)


#######################################
########### Pupil dilation ############
#######################################
allParticipants = lapply(file_list, function(i){processPupil(i, trials, colunas)})

allParticipants[[5]] %>%
  ggplot(aes(x = Recording.timestamp, y = pupil, color = variable))+
    facet_wrap(~Presented.Stimulus.name+trialIndex, scale = "free")+
    geom_point(size=0.5)

ggsave('/Users/pdealcan/Documents/github/sabara/reports/report10/oneParticipant.png')


allParticipants = bind_rows(allParticipants)
allParticipants$condition = stri_sub(allParticipants$Presented.Stimulus.name, 1, 3)

comparisons = allParticipants %>%
  group_by(Recording.name, trialIndex, Presented.Stimulus.name, variable, target, condition) %>%
  summarise(mean = mean(pupil, na.rm = TRUE))

comparisons %>%
  filter(variable != 'X') %>%
  filter(condition == 'IJA') %>%
  ggplot(aes(y = variable, x = mean, fill = variable)) +
    facet_wrap(~target) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

ggsave('/Users/pdealcan/Documents/github/sabara/reports/report10/totalIJA.png')

comparisons %>%
  filter(variable != 'X') %>%
  filter(condition == 'RJA') %>%
  ggplot(aes(y = variable, x = mean, fill = variable)) +
    facet_wrap(~target) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

ggsave('/Users/pdealcan/Documents/github/sabara/reports/report10/totalRJA.png')


#Dilatacao carga cognitiva
#comparar IJA com RJA. Within subject.
#Trending
