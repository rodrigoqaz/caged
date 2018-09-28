# Verifica se os pacotes estão instalados na máquina, senão, instala e depois carrega:
list.of.packages <- c("tidyverse", "plotly", "janitor", "rgdal", "RColorBrewer", "leaflet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=TRUE)

# Carrega as bases de dados:
load("dados_2017.rda")
ibge_subsetor <- read_delim(file = "ibge_subsetor.txt", delim = ";", col_types = "cc")

# Limpa o nome das variáveis:
names(dados) <- dados %>% clean_names() %>% names

#verifica a quantidade de registros por mes:
dados %>% 
  group_by(competencia_declarada, admitidos_desligados) %>% 
  summarise(qtd = n()) %>% 
  spread(key = admitidos_desligados, value = qtd)

# 1. Gráfico de Pirâmide

# Gera base para o gráfico
base <- dados %>% 
  select(ibge_subsetor, sexo, salario_mensal) %>% 
  filter(ibge_subsetor != "{ñ") %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", ".")),
         sexo = ifelse(sexo=="01", "Masculino", "Feminino")) %>% 
  inner_join(ibge_subsetor) %>% 
  group_by(ibge_subsetor_descricao, sexo) %>% 
  summarise(mediana = median(salario_mensal)) %>% 
  mutate(mediana = if_else(sexo == "Masculino", -mediana, mediana)) %>% 
  spread(key = sexo, value = mediana) %>% 
  arrange(Feminino) %>% 
  ungroup() %>% 
  mutate(ibge_subsetor_descricao = factor(ibge_subsetor_descricao, levels = ibge_subsetor_descricao))

# Gera o gráfico:

base %>%
  plot_ly(x=~Feminino, 
          y=~ibge_subsetor_descricao, 
          type = 'bar', 
          orientation = 'h', 
          name = "Feminino", 
          sizes = c(1800,1800),
          marker = list(color = 'rgba(246, 78, 139, 0.6)'), 
          hoverinfo = 'text', 
          text = ~paste0("R$ ", formatC(abs(Feminino), format = 'f', digits = 2, big.mark = ".", decimal.mark = ","))) %>% 
  add_trace(x=~Masculino, 
            name = "Masculino", 
            marker = list(color = 'rgba(158, 202, 225, 0.6)'),
            hoverinfo = 'text', 
            text = ~paste0("R$ ", formatC(abs(Masculino), format = 'f', digits = 2, big.mark = ".", decimal.mark = ","))) %>% 
  layout(barmode = 'overlay',
         title = "Remuneração mediana por sexo e subsetor do IBGE",
         xaxis = list(title = "Remuneração mediana"),
         yaxis = list(title = ""),
         margin = list(l = 250))


# 2. Variação de remuneração por idade e sexo:
base2 <- dados %>% 
  select(idade, sexo, salario_mensal) %>% 
  mutate(idade = as.numeric(idade),
         salario_mensal = as.numeric(str_replace(salario_mensal, ",", ".")),
         sexo = ifelse(sexo=="01", "Masculino", "Feminino")) %>% 
  filter(between(idade, 16, 75)) %>% 
  group_by(idade, sexo) %>% 
  summarise(media = median(salario_mensal)) %>% 
  spread(key = sexo, value = media)


base2 %>% 
  plot_ly(x=~idade, 
          y=~Feminino, 
          type = 'scatter', 
          mode = 'markers', 
          name = 'Feminino', 
          marker = list(color = 'rgba(246, 78, 139, 0.6)'),
          hoverinfo = 'text', 
          text = ~paste0("R$ ", formatC(abs(Feminino), format = 'f', digits = 2, big.mark = ".", decimal.mark = ","))) %>% 
  add_markers(y=~Masculino, 
              name = 'Masculino',
              marker = list(color = 'rgba(158, 202, 225, 0.6)'),
              hoverinfo = 'text', 
              text = ~paste0("R$ ", formatC(abs(Masculino), format = 'f', digits = 2, big.mark = ".", decimal.mark = ","))) %>% 
  layout(title = 'Remuneração mediana x idade por sexo',
         xaxis = list(title = "Idade"),
         yaxis = list(title = "Remuneração Mediana"))

  
  
 # 3. Mapa
base_mapa <- dados %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", ".")),
         sexo = ifelse(sexo=="01", "Masculino", "Feminino"),
         uf = str_sub(municipio,1,2)) %>% 
  select(uf, sexo, salario_mensal) %>% 
  group_by(uf, sexo) %>% 
  summarise(mediana = mean(salario_mensal)) %>% 
  spread(key = sexo, value = mediana) %>% 
  ungroup() %>% 
  mutate(mediana_maior = ifelse(Feminino > Masculino, "Feminino", "Masculino"))  %>%
  mutate(percentual_dif = ((Masculino/Feminino)-1)*100) %>%
  mutate(percentual_dif = round(percentual_dif,2))


# Carrega o shapefile
shp <- readOGR("Mapa\\.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")

base_grafico <- merge(shp,base_mapa, by.x = "CD_GEOCUF", by.y = "uf")

proj4string(base_grafico) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

Encoding(base_grafico$NM_ESTADO) <- "UTF-8"
base_grafico$percentual_dif[is.na(base_grafico$percentual_dif)] <- 0

pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa

popup <- paste0("<strong>Estado: </strong>", 
                base_grafico$NM_ESTADO, 
                "<br><strong>Sexo com maior salário: </strong>", 
                base_grafico$mediana_maior,
                "<br><strong>Percentual da diferença: </strong>",
                base_grafico$percentual_dif, "%")

leaflet(data = base_grafico) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(base_grafico$percentual_dif), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup) %>%
  addLegend("bottomright", pal = pal, values = ~base_grafico$percentual_dif,
            title = "Diferença da remuneração média entre homens e mulheres",
            opacity = 1)

# 4. Escolaridade

