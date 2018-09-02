#### Download dos dados na página do Caged ####

# Carrega os pacotes necessários:
library(tidyverse)


# Caminho para a base de dados:

path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/"

ano <- 2017
mes <- str_pad(seq(1:12),2, pad="0")
files <- paste0("CAGEDEST_",mes, ano, ".7z")

for (i in 1:12){
  download.file(paste0(path,ano,"/",files[i]), files)
}


