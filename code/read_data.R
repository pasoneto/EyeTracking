library("dplyr")
library("data.table")
library("ggplot2")
source("/Users/pdealcan/Documents/github/doc_suomi/code/utils.R")
setwd("/Users/pdealcan/Documents/github/sabara/data")

########## Lendo e limpando o banco de dados
a = read_tsv("./sample_data3.tsv") #Participante sample
b = read_excel("./Documents/Ciencia/sabara/data/sample_data2.xlsx") #Participante sample

c = read.table(file = './sample_data3.tsv', sep = '\t', header = TRUE)
d = read.table(file = './sample_data3.tsv', sep = '\t', header = TRUE)

consecutive = function(x){
  consecutives = c()
  for(i in 1:length(x)){
    n = x[i] == x[i+1]
    p = x[i] == x[i-1]
    consec = n+p >= 1
    consecutives = c(consecutives, any(n, p))
  }
  return(consecutives)
}

matcher = function(x, y){
  indexer = 0
  clas = c()
  for(i in 1:(length(x))-1){
    a = x[i] == x[i+1]
    b = y[i] == y[i+1]
    if( all(a, b) ) {
     clas = c(clas, indexer)
    } else{
      indexer = indexer+1
      clas = c(clas, indexer)
    }
  }
  return(clas)
}

classifier = function(x){
  y = consecutive(x)
  a = matcher(x, y)
  return(a)
}

#Baseline
d %>%
  filter(Presented.Stimulus.name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
  filter(!is.na(Presented.Stimulus.name)) %>% ##Removento partes sem estimulo
  filter(Presented.Stimulus.name == "IJA_A3_B1_D") %>% #Selecionando video 
  select(Presented.Stimulus.name, Eye.movement.type,  Gaze.event.duration, Eyetracker.timestamp, AOI.hit..IJA_A3_B1_D...Rosto., AOI.hit..IJA_A3_B1_D...Brinquedo.Direita.) %>%
  mutate(consecutive_brinquedoD = classifier(AOI.hit..IJA_A3_B1_D...Brinquedo.Direita.),
         consecutive_rosto = classifier(AOI.hit..IJA_A3_B1_D...Rosto.)) %>%
  group_by(consecutive_brinquedoD, consecutive_rosto) %>%
  summarise(fix_dur_R = sum(Gaze.event.duration),
            fix_dur_b = sum(Gaze.event.duration)) %>%
  dvisu()

#b nao tem presented media name. Nomes de colunas diferentes do a
b %>%
  filter(Presented Stimulus name != "Eyetracker Calibration") %>% ##Removendo partes de calibração
  filter(!is.na(Presented Media name)) %>% dvisu()
  filter(Presented Stimulus name == "IJA_A3_B1_D") %>% dvisu() colnames()#Selecionando video
  select(Presented Stimulus name, Eye movement type,  Gaze event duration [ms], AOI hit [IJA_A32_B1_D - Rosto], AOI hit [IJA_A3_B1_D - Brinquedo Direita], AOI hit [IJA_A3_B1_D - Brinquedo Esquerda]) %>%
  dvisu()
  
#  ggplot(aes(x = seq(1,length(Pupil diameter right)), y = Pupil diameter right))+
#    geom_path()

#Pergunta:
  #Event gaze duration só tem um valor para participante a
  #

unique(a$Average calibration accuracy [mm])
unique(a$IJA) #Vazio. Só NA
unique(a$RJA) #Vazio. Só NA 



