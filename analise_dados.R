# Carrega a base de dados:

load("dados_2017.rda")
ibge_subsetor <- read_delim(file = "ibge_subsetor.txt", delim = ";", col_types = "cc")

# Carrega pacotes necessários
require(tidyverse)
require(plotly)
require(janitor)

# Limpa o nome das variáveis:
names(dados) <- dados %>% clean_names() %>% names

glimpse(dados)

#verificar a quantidade de registros por mes:
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
  summarise(media = median(salario_mensal)) %>% 
  mutate(media = if_else(sexo == "Masculino", -media, media)) %>% 
  spread(key = sexo, value = media) %>% 
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
         xaxis = list(title = "Remuneração média"),
         yaxis = list(title = ""),
         margin = list(l = 250))


# 2. Variação de remuneração por idade e sexo:

x <- dados %>% 
  group_by(idade) %>% 
  summarise(count = n())

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

  
  
# Análise CBO
  
cod_cbo <- read_delim("cod_cbo.txt", ":")

base_cbo <- dados %>% 
  select(cbo_2002_ocupacao, salario_mensal, sexo) %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", "."))) %>% 
  group_by(cbo_2002_ocupacao, sexo) %>% 
  summarise(qtd_total = n(),
            media = mean(salario_mensal)) %>% 
  rename(cod = cbo_2002_ocupacao) %>% 
  gather(var, val, qtd_total:media) %>% 
  unite(var2, sexo, var) %>% 
  spread(var2, val) %>% 
  inner_join(cod_cbo) %>%
  mutate(qtd_total_geral = `01_qtd_total`+`02_qtd_total`) %>% 
  arrange(desc(qtd_total_geral)) %>% 


df <- frame_data(
  ~id, ~type,     ~transactions, ~amount,
  20,  "income",  20,            100,
  20,  "expense", 25,            95,
  30,  "income",  50,            300,
  30,  "expense", 45,            250
)

df %>%
  gather(var, val, transactions:amount) %>%
  unite(var2, type, var) %>%
  spread(var2, val)
