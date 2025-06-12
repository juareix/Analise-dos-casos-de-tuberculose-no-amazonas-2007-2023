library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)

# Dados demo
set.seed(123)
sinan <- tibble(
  dt_notific = seq.Date(from = as.Date("2018-01-01"), by = "month", length.out = 100),
  no_munic = sample(c("Manaus", "Parintins", "Itacoatiara"), 100, replace = TRUE),
  cs_sexo = sample(c("Masculino", "Feminino"), 100, replace = TRUE),
  forma = sample(c("Pulmonar", "Extrapulmonar"), 100, replace = TRUE),
  faixa_etaria = sample(c("0-9", "10-19", "20-29", "30-39", "40-49", "50+"), 100, replace = TRUE),
  cs_escol_n = sample(c("Fundamental", "Médio", "Superior"), 100, replace = TRUE),
  cs_raca = sample(c("Branca", "Preta", "Parda", "Indígena"), 100, replace = TRUE),
  hiv = sample(c("Positivo", "Negativo", NA), 100, replace = TRUE)
)

gal <- tibble(
  resultado = sample(c("Positivo", "Negativo", "Inconclusivo"), 100, replace = TRUE),
  dna_para_o_complexo_mycobacterium_tuberculosis = sample(c("Detectado", "Não detectado"), 100, replace = TRUE)
)

anos <- sort(unique(year(sinan$dt_notific)))
municipios <- sort(unique(sinan$no_munic))
sexos <- sort(unique(sinan$cs_sexo))

ui <- dashboardPage(
  skin = "green",  # skin verde para combinar com Fiocruz
  dashboardHeader(
    titleWidth = 600,
    title = tags$div(
      style = "display: flex; align-items: center; justify-content: center; width: 100%;",
      # Espaço para logo - substituir src pelo caminho da sua logo
      tags$div(
        style = "flex: 0 0 auto; margin-right: 15px;",
        tags$img(src = "fiocruz_logo.png", height = "50px", alt = "Logo Fiocruz")
      ),
      tags$div(
        style = "flex: 1 1 auto; font-weight: bold; font-size: 22px; color: white; text-align: center;",
        "Projeto Tuberculose - ILMD/FIOCRUZ"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "geral", icon = icon("chart-pie")),
      menuItem("Perfil Sociodemográfico", tabName = "perfil", icon = icon("users")),
      menuItem("Exames (GAL)", tabName = "gal", icon = icon("flask"))
    ),
    hr(),
    selectInput("filtro_ano", "Filtrar por Ano", choices = c("Todos", anos)),
    selectInput("filtro_municipio", "Filtrar por Município", choices = c("Todos", municipios)),
    selectInput("filtro_sexo", "Filtrar por Sexo", choices = c("Todos", sexos))
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Paleta Fiocruz - verde e vermelho */
        .small-box.bg-green { background-color: #007A4D !important; color: white !important; }
        .small-box.bg-red { background-color: #A71D31 !important; color: white !important; }
        .small-box.bg-darkred { background-color: #7A3030 !important; color: white !important; }
        .small-box.bg-orange { background-color: #D13438 !important; color: white !important; }
        
        /* Fundo */
        .content-wrapper, .right-side { background-color: #F4F8F5; }
        
        /* Sidebar com verde escuro */
        .main-sidebar { background-color: #00472B; }
        .sidebar-menu > li.active > a { background-color: #007A4D !important; color: white !important; }
        .sidebar-menu > li > a { color: white !important; }
        .sidebar-menu > li > a:hover { background-color: #007A4D !important; }
      "))
    ),
    tabItems(
      tabItem(tabName = "geral",
              fluidRow(
                valueBoxOutput("n_casos", width = 3),
                valueBoxOutput("ano_recente", width = 3),
                valueBoxOutput("casos_gal", width = 3)
              ),
              fluidRow(
                box(title = "Casos por Ano", width = 6, plotOutput("plot_ano")),
                box(title = "Forma Clínica por Sexo", width = 6, plotOutput("plot_forma_sexo"))
              ),
              fluidRow(
                box(title = "Mapa de Casos (Placeholder)", width = 12, plotOutput("mapa_am"))
              )
      ),
      tabItem(tabName = "perfil",
              fluidRow(
                box(title = "Distribuição por Faixa Etária", width = 6, plotOutput("plot_idade")),
                box(title = "Escolaridade dos Casos", width = 6, plotOutput("plot_escolaridade"))
              ),
              fluidRow(
                box(title = "Distribuição por Raça/Cor", width = 6, plotOutput("plot_raca")),
                box(title = "Status HIV", width = 6, plotOutput("plot_hiv"))
              )
      ),
      tabItem(tabName = "gal",
              fluidRow(
                box(title = "Resultado do Exame", width = 6, plotOutput("plot_resultado")),
                box(title = "DNA para TB", width = 6, plotOutput("plot_dna"))
              )
      )
    )
  )
)

server <- function(input, output) {
  dados_filtrados <- reactive({
    df <- sinan
    if (input$filtro_ano != "Todos") df <- df %>% filter(year(dt_notific) == as.numeric(input$filtro_ano))
    if (input$filtro_municipio != "Todos") df <- df %>% filter(no_munic == input$filtro_municipio)
    if (input$filtro_sexo != "Todos") df <- df %>% filter(cs_sexo == input$filtro_sexo)
    df
  })
  
  output$n_casos <- renderValueBox({
    valueBox(nrow(dados_filtrados()), "Casos SINAN", icon = icon("notes-medical"), color = "green")
  })
  
  
  output$casos_gal <- renderValueBox({
    valueBox(nrow(gal), "Exames GAL", icon = icon("vial"), color = "red")
  })
  
  output$plot_ano <- renderPlot({
    dados_filtrados() %>%
      mutate(ano = year(dt_notific)) %>%
      count(ano) %>%
      ggplot(aes(x = factor(ano), y = n)) +
      geom_col(fill = "#007A4D") +
      theme_minimal() +
      labs(x = "Ano", y = "Número de Casos")
  })
  
  output$plot_forma_sexo <- renderPlot({
    dados_filtrados() %>%
      filter(!is.na(cs_sexo) & !is.na(forma)) %>%
      count(cs_sexo, forma) %>%
      ggplot(aes(x = cs_sexo, y = n, fill = forma)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("#007A4D", "#7A3030")) +
      theme_minimal() +
      labs(x = "Sexo", y = "Número de Casos", fill = "Forma Clínica")
  })
  
  output$plot_idade <- renderPlot({
    dados_filtrados() %>%
      filter(!is.na(faixa_etaria)) %>%
      count(faixa_etaria) %>%
      ggplot(aes(x = faixa_etaria, y = n)) +
      geom_col(fill = "#007A4D") +
      theme_minimal() +
      labs(x = "Faixa Etária", y = "Número de Casos")
  })
  
  output$plot_escolaridade <- renderPlot({
    dados_filtrados() %>%
      filter(!is.na(cs_escol_n)) %>%
      count(cs_escol_n) %>%
      ggplot(aes(x = reorder(cs_escol_n, n), y = n)) +
      geom_col(fill = "#7A3030") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Escolaridade", y = "Número de Casos")
  })
  
  output$plot_raca <- renderPlot({
    dados_filtrados() %>%
      filter(!is.na(cs_raca)) %>%
      count(cs_raca) %>%
      ggplot(aes(x = cs_raca, y = n, fill = cs_raca)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("#007A4D", "#7A3030", "#D13438", "#B84B4B")) +
      theme_minimal() +
      labs(x = "Raça/Cor", y = "Número de Casos")
  })
  
  output$plot_hiv <- renderPlot({
    dados_filtrados() %>%
      filter(!is.na(hiv)) %>%
      count(hiv) %>%
      ggplot(aes(x = hiv, y = n)) +
      geom_col(fill = "#D13438") +
      theme_minimal() +
      labs(x = "Status HIV", y = "Número de Casos")
  })
  
  output$plot_resultado <- renderPlot({
    gal %>%
      filter(!is.na(resultado)) %>%
      count(resultado) %>%
      ggplot(aes(x = resultado, y = n, fill = resultado)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("#007A4D", "#7A3030", "#D13438")) +
      theme_minimal() +
      labs(x = "Resultado", y = "Número de Exames")
  })
  
  output$plot_dna <- renderPlot({
    gal %>%
      filter(!is.na(dna_para_o_complexo_mycobacterium_tuberculosis)) %>%
      count(dna_para_o_complexo_mycobacterium_tuberculosis) %>%
      ggplot(aes(x = dna_para_o_complexo_mycobacterium_tuberculosis, y = n)) +
      geom_col(fill = "#7A3030") +
      theme_minimal() +
      labs(x = "DNA para TB", y = "Número de Exames")
  })
  
  output$mapa_am <- renderPlot({
    ggplot() +
      geom_point(aes(x = runif(50, -65, -60), y = runif(50, -5, 0)), color = "#007A4D") +
      labs(title = "Mapa de Casos (exemplo simples)", x = "Longitude", y = "Latitude") +
      theme_minimal()
  })
}

shinyApp(ui, server)
