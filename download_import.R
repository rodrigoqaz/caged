#### Download dos dados na página do Caged ####

# Carrega os pacotes necessários:
library(tidyverse)


# Caminho para a base de dados:

path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/"

# Parâmetros paa extração dos dados:
ano <- 2017
mes <- str_pad(seq(1:12),2, pad="0")
files <- paste0("CAGEDEST_", mes, ano, ".7z")
dados <- NULL

# Loop para fazer o download, extração e importação dos dados:
for (i in 1:12){
  #download:
  download.file(url = paste0(path,ano,"/",files[i]), destfile = files[i], mode='wb', method='libcurl')
  
  #descompacta do arquivo. Deve ter instalado o 7Zip no pc e colocar o caminho para o executável abaixo
  system(paste0('"C:\\Program Files\\7-Zip\\7z.exe" e ', files[i]))
  
  #importa e junta dos arquivos em uma único data_frame:
  temp <- read_delim(file = str_replace(files[i], pattern = ".7z", replacement = ".txt"), delim = ";", locale = locale(encoding = "latin1"))
  dados <- rbind(dados,temp)
  rm(temp)
  gc(reset = T) #limpa memoria
}

#salva o conjunto de dados em um arquivo rda:
save(dados, file = "dados.rda")
