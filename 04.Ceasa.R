# Algumas bibliotecas utilizadas nesta demo. 

## Todas foram instaladas na instalação do tidyverse, mas nem todas são carregadas quando carregamos o tidyverse.
library(tidyverse)
### ggplot - gráficos
### tibble - dados tabulares (extensão de data.frame)
### tidyr  - normalização e desnormalização de modelos de dados
### readr  - leitura/escrita de arquivos
### purrr  - programação funcional
### dplyr  - operações relacionais
### stringr- biblioteca para strings
### forcats- biblioteca para factors

## Biblioteca para data e hora
library(lubridate)

## Operações de encadeamento entre funções
library(magrittr)


# Arquivo que gerei extraíndo dados do site da CEASA - RS
ceasa <- read_csv("Data/ceasa.csv")

# Visualização na Console
ceasa

# Visualização no RStudio
View(ceasa)

# Um resumo dos dados em cada variável da tabela
summary(ceasa)

# Modificação de um tipo de dado
ceasa %<>% mutate(unid_medida = factor(unid_medida))

summary(ceasa)

# Todos os nomes dos produtos
distinct(ceasa, produto) %>% 
  arrange(produto) %>% 
  View()

# Exibição dos tipos das variáveis atualizada
ceasa

# Temos muitos produtos, mas todos na mesma data?
count(ceasa, data)
count(ceasa, produto)

# Ajuste de nome de dois produtos e remoção de uma variável que não utilizaremos
ceasa %<>%
  mutate(produto = case_when(
    str_detect(produto, "BATATA BRANCA") ~ "BATATA INGLESA",
    str_detect(produto, "NESPERA - RS") ~ "NESPERA",
    TRUE ~ produto )) %>%
  select(-unit)

# Uma forma de analisar a primeira e a última data com preços de cada produto comercializado
intervalo_data_produtos <- 
  group_by(ceasa, produto) %>%
  summarise(primeira_data = min(data), ultima_data = max(data)) %>%
  ungroup() %>%
  mutate(duracao = interval(primeira_data, ultima_data) %>% as.numeric("days"))

# Comparar duracao com quantidade de registros por produto. Qual a diferença?
arrange(intervalo_data_produtos, desc(ultima_data, duracao)) 

# Remoção de produtos que não estavam presentes no intervalo de extração do site
produtos_recentes <- 
  filter(intervalo_data_produtos, (ultima_data == max(ceasa$data)) & (primeira_data == min(ceasa$data)))

ceasa_produtos_recentes <-
  semi_join(ceasa, produtos_recentes, by="produto")

scales::percent(nrow(ceasa_produtos_recentes) / nrow(ceasa))

# Parece que alguns produtos possuem "buraco" maior no intervalo de datas
count(ceasa_produtos_recentes, produto) %>%
  summary()

# Vamos manter somente os produtos presentes em todos os dias do período de extração dos dados do site
produtos_presentes_todos_dias <- 
  count(ceasa_produtos_recentes, produto) %>%
  arrange(n) %>%
  filter(n == 786)

ceasa_produtos_recentes %<>%
  semi_join(produtos_presentes_todos_dias, by="produto")

scales::percent(nrow(ceasa_produtos_recentes) / nrow(ceasa))

# Criação de registros para as datas sem preços (finais de semana, feriados, datas com erro de extração)
intervalo_datas_completo_produto <- 
  ceasa_produtos_recentes %>%
  group_by(produto) %>%
  expand(datas = full_seq(c(min(data), max(data)), 1)) %>%
  ungroup()

ceasa_produtos_recentes %<>%
  right_join(intervalo_datas_completo_produto, by=c("produto", "data" = "datas"))

ceasa_produtos_recentes

# Completa registros criados utilizando os dados de preço da próxima data com preços capturados
ceasa_produtos_intervalo_completo <-
  ceasa_produtos_recentes %>%
  group_by(produto) %>%
  arrange(data) %>%
  fill(everything(), .direction = "up") %>%
  ungroup()

ceasa_produtos_intervalo_completo

summary(ceasa_produtos_intervalo_completo)

# Qual o tamanho dos objetos?
format(object.size(ceasa), units = "Mb")
rm(ceasa, ceasa_produtos_recentes, produtos_presentes_todos_dias, intervalo_datas_completo_produto, intervalo_data_produtos, produtos_recentes)
format(object.size(ceasa_produtos_intervalo_completo), units = "Mb")

# Visualização rápida da variação dos preços de um produto ao longo do tempo
filter(ceasa_produtos_intervalo_completo, produto == "ABACATE") %>%
  ggplot(aes(x=data)) +
  geom_line(aes(y=preco_min), color="green") +
  geom_line(aes(y=preco_med), color="yellow") +
  geom_line(aes(y=preco_max), color="red")

# Visualização rápida da variação com linha de tendência. Atentar para a transformação nos dados e legenda
ceasa_produtos_intervalo_completo %>%
  select(produto, data, preco_min:preco_max) %>%
  filter(produto == "ABACATE") %>%
  gather(key=tipo_preco, value=valor, -produto, -data) %>%
  ggplot(aes(x=data, y=valor, group=tipo_preco, color=tipo_preco)) +
  geom_line() +
  geom_smooth(method = "lm")

estacoes <- estacoes_periodo( year(min(ceasa_produtos_intervalo_completo$data))
                              , year(max(ceasa_produtos_intervalo_completo$data)))

# Algumas modificações para formatação do gráfico tornam a sua composição mais extensa
ceasa_produtos_intervalo_completo %>%
  select(produto, data, preco_min:preco_max) %>%
  filter(produto == "ABACATE") %>%
  gather(key=tipo_preco, value=valor, -produto, -data) %>%
  mutate(tipo_preco = factor( tipo_preco
                              , levels = c("preco_min", "preco_med", "preco_max")
                              , labels = c("Mínimo"   , "Médio"    , "Máximo")) ) %>%
  ggplot(aes(x = data, y = valor, group = tipo_preco, color = tipo_preco)) +
  geom_vline(xintercept = estacoes$data, linetype = "dotted", alpha=.6) +
  geom_point(alpha = .4, size = 1) +
  geom_line(alpha = .3) +
  geom_text(data = estacoes, aes(x=data, label=estacao), y = 9.8, inherit.aes = FALSE, size=3.5) +
  geom_smooth(method = "lm", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous( breaks = seq(from = 1, to = 10, by = .5)
                      , minor_breaks = seq(from = 1, to = 10, by = .1)
                      , labels = scales::dollar_format(prefix = "R$ ") ) +
  scale_color_manual( values = c("Mínimo" = "darkgreen", "Médio" = "orange", "Máximo" = "red")) +
  labs(x = "Mês", y = "Preço", color = "Tipo de Preço") + 
  theme_minimal() +
  theme( axis.text   = element_text(size = 10)
         , axis.text.x = element_text(angle = 45, hjust = 1)
         , axis.title  = element_text(size = 12))

# Uso dos índices de IPCA para deflação do período histórico
ipca_index <- 
  readxl::read_xls( "Data/ipca_201802SerieHist.xls"
                    , col_names = c("year", "month", "index")
                    , range = "A300:C353"
                    , col_types = c("text", "text", "text")) %>%
  filter(!is.na(month)) %>%
  fill(year, .direction = "down") %>%
  mutate( anomes = dmy(paste("01", month, year, sep="/"))
          , index = as.numeric(index)) %>%
  select(anomes, index)

last_index_range <- ipca_index %>% tail(1) %>% pull(index)

deflaciona_ipca <- function(x, indice_origem, indice_atual) {
  x / indice_origem * indice_atual
}

# Novo dataset com preços corrigidos pelo IPCA
ceasa_produtos_intervalo_completo %>%
  mutate(anomes = floor_date(data, unit = "month")) %>%
  left_join(ipca_index, by="anomes") %>%
  mutate_at( vars( preco_max:preco_min )
             , funs( ipca = deflaciona_ipca
                     , .args = list(indice_origem = .$index, indice_atual = last_index_range))) %>%
  select(produto, data, unid_medida, preco_max:preco_min, preco_max_ipca:preco_min_ipca) ->
  ceasa_produtos_intervalo_completo_com_ipca

# Últimos dias devem estar sem preço corrigido
tail(ceasa_produtos_intervalo_completo_com_ipca)

format(object.size(ceasa_produtos_intervalo_completo_com_ipca), units = "Mb")

# Mesma visualização que a anterior, com preços corrigidos 
ceasa_produtos_intervalo_completo_com_ipca %>%
  select(produto, data, preco_min_ipca:preco_max_ipca) %>%
  filter(produto == "ABACATE") %>%
  gather(key=tipo_preco, value=valor, -produto, -data) %>%
  mutate(tipo_preco = factor( tipo_preco
                              , levels = c("preco_min_ipca", "preco_med_ipca", "preco_max_ipca")
                              , labels = c("Mínimo"   , "Médio"    , "Máximo")) ) %>%
  ggplot(aes(x = data, y = valor, group = tipo_preco, color = tipo_preco)) +
  geom_vline(xintercept = estacoes$data, linetype = "dotted", alpha=.6) +
  geom_point(alpha = .4, size = 1) +
  geom_line(alpha = .3) +
  geom_text(data = estacoes, aes(x=data, label=estacao), y = 9.8, inherit.aes = FALSE, size=3.5) +
  geom_smooth(method = "lm", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous( breaks = seq(from = 1, to = 10, by = .5)
                      , minor_breaks = seq(from = 1, to = 10, by = .1)
                      , labels = scales::dollar_format(prefix = "R$ ") ) +
  scale_color_manual( values = c("Mínimo" = "darkgreen", "Médio" = "orange", "Máximo" = "red")) +
  labs(x = "Mês", y = "Preço", color = "Tipo de Preço (corrigido)") + 
  theme_minimal() +
  theme( axis.text   = element_text(size = 10)
         , axis.text.x = element_text(angle = 45, hjust = 1)
         , axis.title  = element_text(size = 12))

# Análise visual da variação dos preços ao longo dos meses
ceasa_produtos_intervalo_completo %>%
  mutate(anomes = floor_date(data, unit = "month")) %>%
  rename(valor = preco_med) %>%
  inner_join(ipca_index, by="anomes") %>%
  mutate(valor = valor / index * last_index_range) %>%
  group_by(produto) %>%
  mutate(perc = round(valor / mean(valor), 2)) %>%
  ungroup() %>%
  mutate(mes = month(data, label = TRUE)) -> variacao_percentual_preco_produtos

variacao_percentual_preco_produtos %>%  
  ggplot(aes(x=perc)) +
  geom_vline(xintercept = 1, color = "green", linetype = "dashed") +
  geom_bar(aes(fill = ..count..)) + 
  scale_x_continuous(breaks = seq(from = 0, to = 4, by = 0.25)) +
  facet_wrap(~ mes, nrow = 4, ncol = 3)

filter(variacao_percentual_preco_produtos, (perc >= 2)) %>%
  count(produto, mes) %>%
  spread(key = mes, value=n) %>%
  View()

# produtos com maiores variações (metade (ou mais) que média e o dobro (ou mais) que a média)
variacao_percentual_preco_produtos %>%
  semi_join(variacao_percentual_preco_produtos %>% filter(perc >= 2 | perc <= 0.5), by="produto") %>%
  ggplot(aes(x = mes, y = produto, fill = perc)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "red4", midpoint = 1) +
  theme_bw()

# armazena o resultado final em formato próprio do R
write_rds(ceasa_produtos_intervalo_completo_com_ipca, path = "Data/dados_finais_produtos_ceasa.rds.gz", compress = "gz")

