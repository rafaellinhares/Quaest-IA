# app.R (Dentro de inst/app/)

# UI do aplicativo Shiny
ui <- fluidPage(
  title = "Quaest IA",
  theme = shinytheme("flatly"),

  shinyjs::useShinyjs(),

  tags$head(
    tags$style(HTML("
      .logo-container {
        position: absolute;
        top: 15px;
        left: 20px;
        right: auto;
        z-index: 1000;
      }
      .logo-container img {
        height: 50px;
        width: auto;
      }
      body {
        padding-top: 40px;
      }
    "))
  ),

  tags$div(class = "logo-container",
           tags$img(src = "quaest_azul_logo.png", alt = "Logo Quaest")
  ),

  titlePanel(
    tags$h1(
      "Análise de Popularidade Digital (IPD) com IA",
      style = "color: #2c3e50; text-align: center; font-weight: bold;"
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      tags$h4("Configurações e Dados", style = "color: #34495e;"),

      textInput(
        inputId = "gemini_api_key",
        label = tags$b("Chave da API do Google Gemini:"),
        value = "",
        placeholder = "Insira sua chave de API aqui"
      ),
      helpText("Obtenha sua chave em: ",
               tags$a(href="https://aistudio.google.com/app/apikey", "Google AI Studio", target="_blank")),

      hr(),

      fileInput(
        inputId = "excel_file",
        label = tags$b("Carregar Arquivo IPD (dif_dimensoes_14jul.xlsx ou .csv):"),
        accept = c(".xlsx", ".csv"),
        buttonLabel = "Procurar...",
        placeholder = "Nenhum arquivo selecionado"
      ),
      helpText("Este arquivo deve conter as colunas 'profile', 'ipd', 'periodo' e 'dif_ranking'."),

      hr(),

      fileInput(
        inputId = "historical_analysis_file",
        label = tags$b("Carregar Base de Análises Históricas (RELATORIOS+QUAEST+IA.xlsx):"),
        accept = c(".xlsx"),
        buttonLabel = "Procurar...",
        placeholder = "Nenhum arquivo selecionado"
      ),
      helpText("Este arquivo é opcional e deve conter uma coluna chamada 'analise' com exemplos de análises anteriores."),

      hr(),

      actionButton(
        inputId = "generate_button",
        label = "Gerar Análise com IA",
        icon = icon("robot"),
        class = "btn-primary btn-lg btn-block"
      ),

      br(),

      downloadButton(
        outputId = "download_report",
        label = "Baixar Relatório Word",
        icon = icon("file-word"),
        class = "btn-success btn-lg btn-block"
      ),

      br(),
      uiOutput("status_message")
    ),

    mainPanel(
      width = 8,
      tags$h3("Análise Gerada", style = "color: #34495e;"),

      wellPanel(
        style = "background-color: #ecf0f1; border-color: #bdc3c7; min-height: 500px; overflow-y: auto;",
        verbatimTextOutput("analysis_output")
      )
    )
  )
)

# Server do aplicativo Shiny
server <- function(input, output, session) {

  analysis_result <- reactiveVal(NULL)

  observeEvent(input$generate_button, {

    analysis_result(NULL)
    output$analysis_output <- renderText("Gerando análise... Por favor, aguarde.")
    output$status_message <- renderUI(NULL)

    shinyjs::disable("generate_button")
    shinyjs::disable("download_report")

    withProgress(message = 'Processando Análise', value = 0, {

      incProgress(0.1, detail = "Verificando API Key e arquivos...")

      api_key <- input$gemini_api_key
      if (is.null(api_key) || nchar(api_key) == 0 || api_key == "AIzaSyA8S8GGTcv23bOBS12kAGD4JQwmghsn52A") {
        output$status_message <- renderUI({
          tags$div(class = "alert alert-danger", role = "alert",
                   "Erro: Por favor, insira sua chave de API Gemini real.")
        })
        shinyjs::enable("generate_button")
        return()
      }

      req(input$excel_file)
      file_info_uploaded <- input$excel_file

      historical_analyses_text <- ""
      if (!is.null(input$historical_analysis_file)) {
        incProgress(0.2, detail = "Lendo análises históricas...")
        historical_file_path <- input$historical_analysis_file$datapath

        historical_file_ext <- tolower(tools::file_ext(input$historical_analysis_file$name))

        historical_data <- NULL
        if (historical_file_ext == "xlsx") {
          historical_data <- readxl::read_excel(historical_file_path)
        } else {
          output$status_message <- renderUI({
            tags$div(class = "alert alert-warning", role = "alert",
                     "Aviso: O arquivo de análises históricas deve ser .xlsx. A IA não usará exemplos anteriores.")
          })
        }

        if (!is.null(historical_data) && "analise" %in% tolower(names(historical_data))) {
          col_name_analise <- names(historical_data)[tolower(names(historical_data)) == "analise"]
          historical_analyses_text <- paste(historical_data[[col_name_analise]], collapse = "\n\n--- Exemplo Anterior ---\n\n")
        } else if (!is.null(historical_data)) {
          output$status_message <- renderUI({
            tags$div(class = "alert alert-warning", role = "alert",
                     "Aviso: O arquivo de análises históricas nao contem a coluna 'analise'. A IA nao usara exemplos anteriores.")
          })
        }
      } else {
        output$status_message <- renderUI({
          tags$div(class = "alert alert-info", role = "alert",
                   "Nenhum arquivo de analises historicas carregado. A IA gerara a analise sem exemplos anteriores.")
        })
      }

      incProgress(0.4, detail = "Lendo dados IPD e preparando prompt...")

      tryCatch({
        # CHAMADA QUALIFICADA DA FUNÇÃO DO PACOTE: generate_ipd_analysis
        result <- quaestia::generate_ipd_analysis(file_info_uploaded, api_key, historical_analyses_text) # <-- AQUI ESTÁ A MUDANÇA
        analysis_result(result)

        incProgress(0.9, detail = "Analise concluida!")

        output$analysis_output <- renderText({
          analysis_result()
        })

        output$status_message <- renderUI({
          tags$div(class = "alert alert-success", role = "alert",
                   "Analise gerada com sucesso! Voce pode baixar o relatorio Word.")
        })
        shinyjs::enable("download_report")

      }, error = function(e) {
        analysis_result(NULL)
        error_message_display <- paste("Erro ao gerar analise:", e$message, "\nPor favor, verifique a API Key, os arquivos (nomes das colunas 'profile', 'ipd', 'periodo', 'dif_ranking' no IPD, e 'analise' no historico) ou sua conexao com a internet.")

        output$analysis_output <- renderText(error_message_display)
        output$status_message <- renderUI({
          tags$div(class = "alert alert-danger", role = "alert",
                   paste("Erro ao gerar analise:", e$message))
        })
        shinyjs::disable("download_report")
      }, finally = {
        shinyjs::enable("generate_button")
      })
    })
  })

  output$analysis_output <- renderText({
    if (is.null(analysis_result())) {
      "A analise sera exibida aqui apos a geracao."
    } else {
      analysis_result()
    }
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Relatorio_IPD_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      if (is.null(analysis_result())) {
        stop("Nao ha analise para exportar. Por favor, gere a analise primeiro.")
      }

      officer::read_docx() %>%
        officer::body_add_par("Relatorio de Analise de Popularidade Digital", style = "heading 1") %>%
        officer::body_add_par(paste("Data da Analise:", format(Sys.Date(), "%d/%m/%Y")), style = "Normal") %>%
        officer::body_add_par("", style = "Normal") %>%
        officer::body_add_par(analysis_result(), style = "Normal") %>%
        print(target = file)
    }
  )
}

shinyApp(ui = ui, server = server)
