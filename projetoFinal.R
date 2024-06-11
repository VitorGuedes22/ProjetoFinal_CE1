library(ggplot2)
library(dplyr)

# Definir o diretório de trabalho (coloque o caminho que voce usa no seu PC)
setwd("C:/Users/vitor/OneDrive/Documentos/Materias_UNB/CE1/ProjetoFinal_CE1")

#### Questão 1 
rawWages <- read.csv("raw_wages.csv")
wagesClaned <- read.csv("wages_cleaned.csv")

