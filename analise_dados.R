# Verifica se os pacotes estão instalados na máquina, senão, instala e depois carrega:
list.of.packages <- c("tidyverse", "plotly", "janitor", "rgdal", "RColorBrewer", "leaflet", "kableExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=TRUE)

# Carrega as bases de dados:
load("dados_2017.rda")
ibge_subsetor <- read_delim(file = "ibge_subsetor.txt", delim = ";", col_types = "cc")
cbo <- read_delim(file = "cbo_ocupacao.txt", delim = ":", col_types = "ic")
# Limpa o nome das variáveis:
names(dados) <- dados %>% clean_names() %>% names

dados %>%
  select(sexo, salario_mensal) %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", "."))) %>% 
  group_by(sexo) %>% 
  summarize(min = min(salario_mensal),
            q1 = quantile(salario_mensal,0.25),
            md = median(salario_mensal),
            q3 = quantile(salario_mensal,0.75),
            max = max(salario_mensal),
            media = mean(salario_mensal),
            desv.pad = sd(salario_mensal),
            qtd = n())
  
  
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


# 4. Box plot
aux <- dados %>% 
  select(salario_mensal) %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", ".")))

sigma <- sd(aux$salario_mensal)
n <- (qnorm(0.95) * sigma)^2/50

amostra <- dados[sample(nrow(dados), 197810), ]

teste %>% 
  select(sexo)


teste %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", "."))) %>% 
  filter(between(salario_mensal,300,10000), admitidos_desligados == "02") %>% 
plot_ly(y = ~salario_mensal[sexo=="01"], type = "box", name = "Masculino") %>% 
  add_trace(y = ~salario_mensal[sexo=="02"], name = "Feminino") %>% 
  layout(title = 'Box-Plot da variável salário mensal por sexo', yaxis = list(title = "Salário"))


teste %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", "."))) %>% 
  plot_ly(x=~log(salario_mensal), type = 'histogram', histnorm = 'probability', nbinsx = 40)

teste %>%
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", "."))) %>%
  plot_ly(alpha = 0.7) %>%
  add_histogram(x = ~log(salario_mensal[sexo=='01']), 
                histnorm = 'probability', 
                nbinsx = 70, 
                name = "Masculino", 
                marker = list(color = 'rgba(158, 202, 225, 0.6)')) %>%
  add_histogram(x = ~log(salario_mensal[sexo=='02']), 
                histnorm = 'probability', 
                nbinsx = 70, 
                name = "Feminino", 
                marker = list(color = 'rgba(246, 78, 139, 0.6)')) %>%
  layout(barmode = "overlay", xaxis = list(title = "Ln do salário"))


# 5. quantidade de cargos

top_10 <- dados %>% 
  group_by(cbo_2002_ocupacao) %>% 
  summarise(qtd = n()) %>% 
  ungroup() %>% 
  arrange(desc(qtd)) %>% 
  top_n(10, wt = qtd)

dados %>% 
  filter(cbo_2002_ocupacao %in% top_10$cbo_2002_ocupacao) %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", ".")),
       sexo = ifelse(sexo=="01", "Masculino", "Feminino")) %>% 
  inner_join(cbo, by = c("cbo_2002_ocupacao"="codigo")) %>% 
  group_by(cbo_2002_ocupacao, ocupacao, sexo) %>% 
  summarise(media = mean(salario_mensal)) %>% 
  spread(key = sexo, value = media) %>% 
  arrange(factor(cbo_2002_ocupacao, levels = top_10$cbo_2002_ocupacao)) %>% 
  ungroup() %>%
  select(-cbo_2002_ocupacao) %>% 
  mutate(diferenca = paste0(formatC((Masculino / Feminino - 1)*100, format = 'f', digits = 2, big.mark = ".", decimal.mark = ","), "%")) %>% 
  kable(align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



# 5. valor de cargos

top_10_v <- dados %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", "."))) %>% 
  group_by(cbo_2002_ocupacao) %>% 
  summarise(valor = mean(salario_mensal)) %>% 
  ungroup() %>% 
  arrange(desc(valor)) %>% 
  top_n(11, wt = valor)

dados %>% 
  filter(cbo_2002_ocupacao %in% top_10_v$cbo_2002_ocupacao) %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", ".")),
         sexo = ifelse(sexo=="01", "Masculino", "Feminino")) %>% 
  inner_join(cbo, by = c("cbo_2002_ocupacao"="codigo")) %>% 
  group_by(cbo_2002_ocupacao, ocupacao, sexo) %>% 
  summarise(media = mean(salario_mensal)) %>% 
  spread(key = sexo, value = media) %>% 
  arrange(factor(cbo_2002_ocupacao, levels = top_10_v$cbo_2002_ocupacao)) %>% 
  filter(cbo_2002_ocupacao != 215110) %>% 
  ungroup() %>% 
  mutate(diferenca = paste0(formatC((Masculino / Feminino - 1)*100, format = 'f', digits = 2, big.mark = ".", decimal.mark = ","), "%")) %>% 
  kable(align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12)

# 6. Faixa de salario


faixas <- c(0, 1000, 2000, 3000)

x <- dados %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", ".")),
         sexo = ifelse(sexo=="01", "Masculino", "Feminino"),
         faixa = cut(salario_mensal, breaks = faixas)) %>% 
  group_by(faixa, sexo) %>% 
  summarise(qtd = n()) %>% 
  spread(sexo, qtd) %>% 
  ungroup() %>% 
  mutate(perc_feminino = -Feminino / (Feminino+Masculino),
         perc_masculino = Masculino / (Feminino+Masculino))


x$faixa <- factor(c("até 1.000", "1.000 a 2.000", "2.000 a 3.000", "acima de 3.000"), levels = c("até 1.000", "1.000 a 2.000", "2.000 a 3.000", "acima de 3.000"))

x %>%
  plot_ly(x=~perc_feminino, 
          y=~faixa, 
          type = 'bar', 
          orientation = 'h', 
          name = "Feminino", 
          sizes = c(1800,1800),
          marker = list(color = 'rgba(246, 78, 139, 0.6)'), 
          hoverinfo = 'text', 
          text = ~paste0(formatC(abs(perc_feminino*100), format = 'f', digits = 2, big.mark = ".", decimal.mark = ","), "%")) %>% 
  add_trace(x=~perc_masculino, 
            name = "Masculino", 
            marker = list(color = 'rgba(158, 202, 225, 0.6)'),
            hoverinfo = 'text', 
            text = ~paste0(formatC(abs(perc_masculino*100), format = 'f', digits = 2, big.mark = ".", decimal.mark = ","), "%")) %>% 
  layout(barmode = 'overlay',
         title = "Proporção de pessoas por faixa salarial e sexo",
         xaxis = list(title = "Proporção", range = c(-1,1)),
         yaxis = list(title = ""))





# 999. Sankey

graf <- dados %>% 
  group_by(sexo, grau_instrucao, raca_cor) %>% 
  summarise(value = n()) %>% 
  ungroup()

graf <- rbind(graf %>%
                select(sexo,grau_instrucao, value) %>%
                rename(source = sexo, target = grau_instrucao),
              graf %>%
                select(grau_instrucao, raca_cor, value) %>%
                rename(source = grau_instrucao, target = raca_cor)
)

graf <- graf %>% 
  group_by(source, target) %>% 
  summarise(value = sum(value))
graf <- graf %>%  filter(source != "{ñ", target != "{ñ")

node_names <- factor(sort(unique(as.character(unname(unlist(graf[1:2]))))))
nodes <- data.frame(name = node_names)

links <- data.frame(source = match(graf$source, node_names) - 1, 
                    target = match(graf$target, node_names) - 1,
                    value = graf$value)
links <- links[!is.na(links$source), ]


library(plotly)

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = node_names,
    color = rep("blue", length(node_names)),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = links$source,
    target = links$target,
    value =  links$value
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 5
    )
  )
p

