library("dplyr")
library("data.table")
library("ggplot2")
source("/Users/pdealcan/Documents/github/doc_suomi/code/utils.R")
library(readxl)

########## Lendo e limpando o banco de dados
a = read_excel("./Documents/Ciencia/sabara/data/sample_data.xlsx") #Participante sample
b = read_excel("./Documents/Ciencia/sabara/data/sample_data2.xlsx") #Participante sample
#Base line 

a %>%
  filter(`Presented Stimulus name` != "Eyetracker Calibration") %>% ##Removendo partes de calibração
  filter(!is.na(`Presented Stimulus name`)) %>% ##Removento partes sem estimulo
  filter(`Presented Stimulus name` == "IJA_A3_B1_D") %>% #Selecionando video
  select(`Presented Stimulus name`, `Eye movement type`,  `Gaze event duration`, `Eyetracker timestamp`, `AOI hit [IJA_A3_B1_D - Rosto]`, `AOI hit [IJA_A3_B1_D - Brinquedo Direita]`, `AOI hit [IJA_A3_B1_D - Brinquedo Esquerda]`) %>%
  dvisu()

#b nao tem presented media name. Nomes de colunas diferentes do a
b %>%
  filter(`Presented Stimulus name` != "Eyetracker Calibration") %>% ##Removendo partes de calibração
  filter(!is.na(`Presented Media name`)) %>% dvisu()
  filter(`Presented Stimulus name` == "IJA_A3_B1_D") %>% dvisu() colnames()#Selecionando video
  select(`Presented Stimulus name`, `Eye movement type`,  `Gaze event duration [ms]`, `AOI hit [IJA_A32_B1_D - Rosto]`, `AOI hit [IJA_A3_B1_D - Brinquedo Direita]`, `AOI hit [IJA_A3_B1_D - Brinquedo Esquerda]`) %>%
  dvisu()
  
#  ggplot(aes(x = seq(1,length(`Pupil diameter right`)), y = `Pupil diameter right`))+
#    geom_path()

#Pergunta:
  #Event gaze duration só tem um valor para participante a
  #

unique(a$`Average calibration accuracy [mm]`)
unique(a$`IJA`) #Vazio. Só NA
unique(a$`RJA`) #Vazio. Só NA 



