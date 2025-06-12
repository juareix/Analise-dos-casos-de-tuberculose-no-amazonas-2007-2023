# ==========================================================================
# APP SHINY - Visualização Interativa das Bases SINAN e GAL
# Projeto: Avaliação da Qualidade dos Dados de Tuberculose - ILMD/FIOCRUZ
# Autor: Juarez Maciel da Silva
# Data: junho/2025
# ==========================================================================

if (!require("shiny")) install.packages("shiny");library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard");library(shinydashboard)
if (!require("tidyverse")) install.packages("tidyverse");library(tidyverse)
if (!require("DT")) install.packages("DT");library(DT)
if (!require("lubridate")) install.packages("lubridate");library(lubridate)
if (!require("shinyWidgets")) install.packages("shinyWidgets");library(shinyWidgets)
if (!require("geobr")) install.packages("geobr");library(geobr)
if (!require("sf")) install.packages("sf");library(sf)

# -----------------------------
# 1. Carregar dados
# -----------------------------
# -----------------------------
# 1. Carregar dados com proteção contra erro
# -----------------------------
sinan <- read_csv("sinan_tratado.csv")
if (!inherits(sinan$dt_notific, "Date") && !inherits(sinan$dt_notific, "POSIXct")) {
  sinan$dt_notific <- suppressWarnings(parse_date_time(sinan$dt_notific, orders = c("ymd", "dmy", "mdy")))
}
gal   <- read_csv("gal_tratado.csv")

if (file.exists("relatorio_inconsistencias.csv")) {
  relatorio_incons <- read_csv("relatorio_inconsistencias.csv")
} else {
  relatorio_incons <- tibble(inconsistencia = character(), total_casos = numeric())
}

if (file.exists("completude_sinan_tabela.csv")) {
  completude_sinan <- read_csv("completude_sinan_tabela.csv")
} else {
  completude_sinan <- tibble(variavel = character(), completude = numeric(), classificacao = character())
}

if (file.exists("completude_gal.csv")) {
  completude_gal <- read_csv("completude_gal.csv")
} else {
  completude_gal <- tibble(variavel = character(), completude = numeric(), classificacao = character())
}

anos <- sort(unique(year(as.Date(sinan$dt_notific))))
municipios <- sort(unique(sinan$no_munic))
sexos <- sort(unique(sinan$cs_sexo))

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    titleWidth = "100%",
    title = tags$div(
      style = "width:100%; text-align:center; font-size:22px; font-weight:bold; color:#FFFFFF;",
      "Painel Interativo - Tuberculose (SINAN e GAL) - ILMD/FIOCRUZ"
    ),
    tags$li(
      class = "dropdown",
      tags$img(src = "fiocruz_logo.png", height = "45px", style = "margin-top:10px; margin-right:15px; float:right;")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "geral", icon = icon("chart-pie")),
      menuItem("Perfil Sociodemográfico", tabName = "perfil", icon = icon("users")),
      menuItem("Exames (GAL)", tabName = "gal", icon = icon("flask")),
      menuItem("Completude", tabName = "completude", icon = icon("check-double")),
      menuItem("Inconsistências", tabName = "incons", icon = icon("exclamation-triangle"))
      #menuItem("Resumo", tabName = "tabelas", icon = icon("table"))
    ),
    hr(),
    selectInput("filtro_ano", "Filtrar por Ano", choices = c("Todos", anos)),
    selectInput("filtro_municipio", "Filtrar por Município", choices = c("Todos", municipios)),
    selectInput("filtro_sexo", "Filtrar por Sexo", choices = c("Todos", sexos))
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Cabeçalho e barra superior */
        .main-header, .main-header .logo, .main-header .navbar {
          background-color: #003366 !important;
        }
        .main-sidebar {
          background-color: #f4f4f4;
        }

        /* Caixas e bordas superiores */
        .box {
          border-top: 3px solid #003366 !important;
        }

        /* Fundo da aplicação */
        body, .content-wrapper {
          background-color: #f8f9fa !important;
        }

        /* Personalização dos botões de download */
        .btn {
          background-color: #003366;
          color: #fff;
          border: none;
        }

        .btn:hover {
          background-color: #002244;
        }

        /* Título centralizado */
        .main-header .navbar .sidebar-toggle {
          display: none;
        }

        /* Fontes legíveis */
        body {
          font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "geral",
              fluidRow(
                valueBoxOutput("n_casos"),
                valueBoxOutput("casos_gal"),
                valueBoxOutput("n_municipios")
              ),
              fluidRow(
                box(title = "Casos por Ano", width = 6, plotOutput("plot_ano"), downloadButton("baixar_grafico_ano", "Baixar")),
                box(title = "Forma Clínica por Sexo", width = 6, plotOutput("plot_forma_sexo"), downloadButton("baixar_grafico_forma", "Baixar"))
              ),
              fluidRow(
                box(title = "Mapa de Casos por Município (AM)", width = 12, plotOutput("mapa_am"))
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
                valueBoxOutput("n_exames_gal"),
                valueBoxOutput("media_prazo")
              ),
              fluidRow(
                box(title = "Resultado dos Exames (Top 5)", width = 6, plotOutput("plot_resultado"), downloadButton("baixar_resultado_gal", "Baixar")),
                box(title = "DNA para TB", width = 6, plotOutput("plot_dna"), downloadButton("baixar_dna_gal", "Baixar"))
              ),
              fluidRow(
                box(title = "Intervalo: Coleta → Liberação (dias)", width = 6, plotOutput("plot_prazo")),
                box(title = "Resumo dos Exames", width = 6, DTOutput("tabela_gal_resumo"))
              )
      ),
      tabItem(tabName = "completude",
              fluidRow(
                box(title = "Completude das Variáveis (SINAN)", width = 12, DTOutput("table_comp_sinan"))
              ),
              fluidRow(
                box(title = "Completude das Variáveis (GAL)", width = 12, DTOutput("table_comp_gal"))
              )
      ),
      tabItem(tabName = "incons",
              fluidRow(
                box(title = "Resumo de Inconsistências (SINAN + GAL)", width = 12, DTOutput("table_incons"))
              )
      ),
      tabItem(tabName = "tabelas",
              fluidRow(
                box(title = "Base SINAN", width = 12, downloadButton("download_sinan", "Download CSV"), DTOutput("table_sinan"))
              ),
              fluidRow(
                box(title = "Base GAL", width = 12, downloadButton("download_gal", "Download CSV"), DTOutput("table_gal"))
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
    valueBox(nrow(dados_filtrados()), "Casos SINAN", icon = icon("notes-medical"), color = "navy")
  })
  
  output$casos_gal <- renderValueBox({
    valueBox(nrow(gal), "Exames GAL", icon = icon("vial"), color = "aqua")
  })
  
  output$n_municipios <- renderValueBox({
    n_mun <- n_distinct(dados_filtrados()$no_munic)
    valueBox(n_mun, "Municípios distintos", icon = icon("map-marker-alt"), color = "teal")
  })
  
  
  output$plot_ano <- renderPlot({
    dados_filtrados() %>%
      mutate(ano = year(dt_notific)) %>%
      count(ano) %>%
      ggplot(aes(x = ano, y = n)) + geom_col(fill = "steelblue") + theme_minimal()
  })
  
  output$plot_forma_sexo <- renderPlot({
    dados_filtrados() %>%
      filter(!is.na(cs_sexo) & !is.na(forma)) %>%
      count(cs_sexo, forma) %>%
      ggplot(aes(x = cs_sexo, y = n, fill = forma)) + geom_col(position = "dodge") + theme_minimal()
  })
  
  output$plot_idade <- renderPlot({
    dados_filtrados() %>% filter(!is.na(faixa_etaria)) %>% count(faixa_etaria) %>%
      ggplot(aes(x = faixa_etaria, y = n)) + geom_col(fill = "#009E73") + theme_minimal()
  })
  
  output$plot_escolaridade <- renderPlot({
    dados_filtrados() %>%
      filter(!is.na(escolaridade)) %>%
      count(escolaridade) %>%
      ggplot(aes(x = reorder(escolaridade, n), y = n)) +
      geom_col(fill = "#F0E442") +
      coord_flip() +
      theme_minimal()
  })
  
  output$plot_raca <- renderPlot({
    dados_filtrados() %>% filter(!is.na(cs_raca)) %>% count(cs_raca) %>%
      ggplot(aes(x = cs_raca, y = n, fill = cs_raca)) + geom_col(show.legend = FALSE) + theme_minimal()
  })
  
  output$plot_hiv <- renderPlot({
    dados_filtrados() %>% filter(!is.na(hiv)) %>% count(hiv) %>%
      ggplot(aes(x = hiv, y = n)) + geom_col(fill = "#D55E00") + theme_minimal()
  })
  
  
  
  output$mapa_am <- renderPlot({
    map_am <- geobr::read_municipality(code_muni = "AM", year = 2020, simplified = TRUE) %>%
      mutate(name_muni = str_to_lower(name_muni))
    
    casos_por_munic <- sinan %>%
      filter(!is.na(no_munic)) %>%
      mutate(no_munic = str_to_lower(no_munic)) %>%
      count(no_munic)
    
    mapa_dados <- left_join(map_am, casos_por_munic, by = c("name_muni" = "no_munic"))
    
    ggplot(mapa_dados) +
      geom_sf(aes(fill = n), color = "white", size = 0.3) +
      scale_fill_gradient(
        name = "Nº de Casos",
        low = "#48D1CC", high = "#008080", na.value = "#00FFFF"
      ) +
      labs(
        title = "Distribuição de Casos de Tuberculose por Município (AM)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  
  
  
  
  
  output$plot_resultado <- renderPlot({
    gal %>%
      filter(!is.na(resultado)) %>%
      count(resultado, sort = TRUE) %>%
      mutate(resultado = if_else(row_number() > 5, "Outros", resultado)) %>%
      group_by(resultado) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      ggplot(aes(x = reorder(resultado, n), y = n, fill = resultado)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Frequência", title = "Principais Resultados de Exames") +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal()
  })
  
  
  output$plot_dna <- renderPlot({
    gal %>%
      filter(!is.na(dna_para_o_complexo_mycobacterium_tuberculosis)) %>%
      mutate(dna_tb = case_when(
        str_detect(tolower(dna_para_o_complexo_mycobacterium_tuberculosis), "detectado") ~ "Detectado",
        str_detect(tolower(dna_para_o_complexo_mycobacterium_tuberculosis), "não") ~ "Não detectado",
        TRUE ~ "Outros"
      )) %>%
      count(dna_tb) %>%
      ggplot(aes(x = dna_tb, y = n, fill = dna_tb)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "Frequência", title = "DNA para TB") +
      scale_fill_manual(values = c("Detectado" = "#20B2AA", "Não detectado" = "#E69F00", "Outros" = "#999999")) +
      theme_minimal()
  })
  
  output$n_exames_gal <- renderValueBox({
    valueBox(nrow(gal), "Total de exames", icon = icon("flask"), color = "navy")
  })
  
  output$media_prazo <- renderValueBox({
    gal <- gal %>%
      mutate(dias = as.numeric(difftime(data_da_liberacao, data_da_coleta, units = "days")))
    media <- round(mean(gal$dias, na.rm = TRUE), 1)
    valueBox(paste(media, "dias"), "Prazo médio (liberação)", icon = icon("clock"), color = "aqua")
  })
  
  output$plot_prazo <- renderPlot({
    gal %>%
      mutate(dias = as.numeric(difftime(data_da_liberacao, data_da_coleta, units = "days"))) %>%
      filter(!is.na(dias) & dias >= 0 & dias <= 60) %>%
      ggplot(aes(x = dias)) +
      geom_histogram(fill = "#20B2AA", bins = 30) +
      labs(x = "Dias entre coleta e liberação", y = "Frequência") +
      theme_minimal()
  })
  
  output$tabela_gal_resumo <- renderDT({
    gal %>%
      select(municipio = municipio_de_residencia, sexo, idade, resultado, dna = dna_para_o_complexo_mycobacterium_tuberculosis,
             coleta = data_da_coleta, liberacao = data_da_liberacao) %>%
      datatable(options = list(pageLength = 5, scrollX = TRUE))
  })
  
  
  output$table_sinan <- renderDT({
    datatable(dados_filtrados(), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('csv', 'pdf')))
  })
  
  output$table_gal <- renderDT({
    datatable(gal, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('csv', 'pdf')))
  })
  
  # ---------------------------
  # Completude - Tabelas
  # ---------------------------
  output$table_comp_sinan <- renderDT({
    datatable(completude_sinan, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$table_comp_gal <- renderDT({
    datatable(completude_gal, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---------------------------
  # Inconsistências - Resumo
  # ---------------------------
  output$table_incons <- renderDT({
    datatable(relatorio_incons, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
