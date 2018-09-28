#### Download dos dados na página do Caged ####

# Carrega os pacotes necessários:
library(tidyverse)

# Caminho para a base de dados:
path <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/"

# Parâmetros paa extração dos dados:
ano <- 2018
mes <- str_pad(seq(1:8),2, pad="0")
files <- paste0("CAGEDEST_", mes, ano, ".7z")
files_path <- paste0("CAGEDEST_", mes, ano)
dados <- NULL

# Loop para fazer o download, extração e importação dos dados:
for (i in 1:8){
  #download:
  download.file(url = paste0(path,ano,"/",files[i]), destfile = files[i], mode='wb', method='libcurl')
  
  #descompacta do arquivo. Deve ter instalado o 7Zip no pc e colocar o caminho para o executável abaixo
  system(paste0('"C:\\Program Files\\7-Zip\\7z.exe" e ', files[i], ' -o', files_path[i], ' -y'))
  
  #importa e junta dos arquivos em uma único data_frame:
  temp <- read_delim(file = paste0(files_path[i], '/', dir(files_path[i])), delim = ";", locale = locale(encoding = "latin1"))
  temp <- temp[1:23] #seleciona apenas as colunas que são importantes para o estudo
  dados <- rbind(dados,temp)
  rm(temp)
  gc(reset = T) #limpa memoria
}


#salva o conjunto de dados em um arquivo rda:
save(dados, file = "dados_2017.rda")
