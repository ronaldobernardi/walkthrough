# Ajustes para formatação de caracteres da língua portuguesa
Sys.setlocale("LC_ALL", "pt_BR")
options(encoding = "UTF-8")

#Librarys
library(tidyverse)
library(lubridate)

# Inclusão das estações do ano
"03-21" -> OUTONO
"06-22" -> INVERNO
"09-22" -> PRIMAVERA
"12-22" -> VERAO

estacoes_periodo <- function(ano_inicio, ano_fim) {
  intervalo_em_anos <- ano_inicio:ano_fim
  
  # Aqui utilizamos algumas funções da biblioteca de programação funcional
  datas_estacoes <- cross( list( year   = intervalo_em_anos
                                 , season = c(OUTONO, INVERNO, PRIMAVERA, VERAO))) %>%
    map(~ paste0(.$year, "-", .$season)) %>%
    unlist() %>%
    ymd()
  
  nomes_estacoes <- rep(c("Outono", "Inverno", "Primavera", "Verão"), times=4)
  
  # Tabela com as estações do ano até a próxima estação (Inverno 2018)
  tibble( data    = sort(datas_estacoes)
          , estacao = nomes_estacoes) %>% 
    filter(data <= today()) %>%
    mutate(estacao = paste(estacao, format(data, "%y"), sep = "/"))
}
