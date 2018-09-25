# Carrega a base de dados:

load("dados_2017.rda")

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


base <- dados %>% 
  select(ibge_subsetor, sexo, salario_mensal) %>% 
  filter(ibge_subsetor != "{ñ") %>% 
  mutate(salario_mensal = as.numeric(str_replace(salario_mensal, ",", "."))) %>% 
  group_by(ibge_subsetor, sexo) %>% 
  summarise(qtd_total = n(),
                media = mean(salario_mensal))


base %>% 
  ggplot(aes(x = reorder(ibge_subsetor, media, mean), y = media, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=format(media/1000, digits = 3)), position = position_dodge(width=1)) +
  coord_flip() +
  labs(title = "Remuneração média por sexo e subsetor do IBGE", x = "Subsetor IBGE", y = "Remuneração média")






base %>% 
  mutate(media = if_else(sexo == "01", -media, media)) %>% 
ggplot(aes(x = reorder(ibge_subsetor, abs(media), mean), y = media, group = sexo, fill = sexo)) +
  geom_bar(stat = "identity", width = 0.75) +
  scale_y_continuous(labels = abs) +
  geom_text(aes(label=format(media/1000, digits = 3)), position = position_dodge(width=1)) +
  coord_flip() +
  labs(y = "Remuneração Média", x = "Subsetor Econômico - IBGE", title = "Remuneração Média por Sexo e Subsetor") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c("blue", "red"),
                    name="",
                    breaks=c("Masculino", "Feminino"),
                    labels=c("Masculino", "Feminino")) +
  scale_y_continuous(breaks = seq(-5000, 5000, 500), 
                     labels = abs(seq(-5000, 5000, 500))) 

  



#análise para fezer
#saldo de empregos por estado (separar masculino e feminino)
# média salarial homens e mulheres por grau de instrução, tamanho estabelecimento, estado, subsetor, raça, idade e tempo de emprego


  
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
