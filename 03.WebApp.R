library(shiny)
library(tidyverse)
library(forecast)
library(timetk)

source("../project/01.Estacoes.R")

ceasa_data <- read_rds(path = "../project/Data/dados_finais_produtos_ceasa.rds.gz")
produtos <- unique(ceasa_data$produto) %>% sort()
tipos_precos <- c("Mínimo"   , "Médio"    , "Máximo")

Sys.setlocale("LC_ALL", "pt_BR")
options(encoding = "UTF-8")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Preços de Hortifrutigrangeiros na CEASA - RS"),
  
  # Seleção de produto
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectizeInput(inputId = "Produto", label = "Produto", choices = c("Escolha um produto" = "", produtos)),
                 checkboxInput(inputId = "CorrigirIPCA", label = "Preços Corrigidos(IPCA)", value = FALSE),
                 selectizeInput(inputId = "Forecast", label = "Forecast Preço", choices = c("Sem Forecast" = "", tipos_precos))
    ),
    
    # Área de saída para gráfico
    mainPanel( width = 9, 
               plotOutput("SerieProduto", height = 550),
               plotOutput("PrevisaoProduto", height = 550)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  precos_escolhidos <- reactive({
    req(input$Produto) 
    
    if(input$CorrigirIPCA) {
      precos_ <- c("preco_min_ipca", "preco_med_ipca", "preco_max_ipca")
    } else {
      precos_ <- c("preco_min", "preco_med", "preco_max")
    }
    precos_
  })
  
  precos_produto_selecionado <- reactive({
    req(input$Produto)
    
    precos <- precos_escolhidos()
    
    ceasa_data %>%
      select_(.dots = c("produto", "data", precos)) %>%
      filter(produto == input$Produto) %>%
      gather(key=tipo_preco, value=valor, -produto, -data) %>%
      mutate(tipo_preco = factor( tipo_preco
                                  , levels = precos
                                  , labels = tipos_precos) )
  })
  
  estacoes_periodo_produtos <- reactive({
    estacoes_periodo(year(min(ceasa_data$data)), year(max(ceasa_data$data)))
  })
  
  output$SerieProduto <- renderPlot({
    req(input$Produto) 
    
    precos_produto <- precos_produto_selecionado()
    
    maior_preco_praticado <- ceiling(max(precos_produto$valor, na.rm = TRUE)) + 1
    
    estacoes <- estacoes_periodo_produtos()
    
    precos_produto %>%
      ggplot(aes(x = data, y = valor, group = tipo_preco, color = tipo_preco)) +
      geom_vline(xintercept = estacoes$data, linetype = "dotted", alpha=.6) +
      geom_point(alpha = .4, size = 1) +
      geom_line(alpha = .3) +
      geom_label(data = estacoes, aes(x=data, label=estacao), y = 0, inherit.aes = FALSE, size=3.5) +
      geom_smooth(method = "lm", show.legend = FALSE) +
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
      scale_y_continuous( breaks = seq(from = 0, to = maior_preco_praticado, by = .5)
                          , minor_breaks = seq(from = 0, to = maior_preco_praticado, by = .1)
                          , limits = c(0, maior_preco_praticado)
                          , labels = scales::dollar_format(prefix = "R$ ") ) +
      scale_color_manual( values = c("Mínimo" = "darkgreen", "Médio" = "orange", "Máximo" = "red")) +
      labs(x = "Mês", y = "Preço", color = "Tipo de Preço") + 
      theme_bw() +
      theme( axis.text   = element_text(size = 10)
             , axis.text.x = element_text(angle = 45, hjust = 1)
             , axis.title  = element_text(size = 12))
  })
  
  output$PrevisaoProduto  <- renderPlot({
    req(input$Produto, input$Forecast) 
    
    precos <- precos_escolhidos()
    
    precos_produto <- precos_produto_selecionado()
    
    maior_preco_praticado <- precos_produto %>%
      filter(tipo_preco == input$Forecast) %>%
      pull(valor) %>%
      max(na.rm = TRUE)
    
    precos_produto %>%
      filter(tipo_preco == input$Forecast) %>%
      tk_ts(select = valor, frequency = 365, start = 2015.16) %>%
      auto.arima() %>%
      forecast(h=30) %>%
      autoplot() + 
      geom_forecast(h=30) +
      scale_y_continuous( breaks = seq(from = 0, to = maior_preco_praticado, by = .5)
                          , minor_breaks = seq(from = 0, to = maior_preco_praticado, by = .1)
                          , limits = c(0, maior_preco_praticado)
                          , labels = scales::dollar_format(prefix = "R$ ") ) +
      scale_color_manual( values = c("Mínimo" = "darkgreen", "Médio" = "orange", "Máximo" = "red")) +
      labs(x = "Mês", y = "Preço", color = "Tipo de Preço") + 
      theme_bw() +
      theme( axis.text   = element_text(size = 10)
             , axis.text.x = element_text(angle = 45, hjust = 1)
             , axis.title  = element_text(size = 12))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

