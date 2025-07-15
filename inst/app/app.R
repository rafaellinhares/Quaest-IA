# app.R (Dentro de inst/app/)

# UI do aplicativo Shiny
ui <- fluidPage(
  title = "Quaest IA",
  theme = shinytheme("flatly"),

  shinyjs::useShinyjs(), # Necessário para o botão de copiar

  tags$head(
    # Adicionar Font Awesome para o ícone de copiar
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
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
      /* Estilo para o container da análise e botão de copiar */
      .analysis-container {
        position: relative;
        padding-right: 50px; /* Espaço para o botão */
      }
      .copy-button-container {
        position: absolute;
        top: 10px; /* Ajuste a posição vertical */
        right: 10px; /* Ajuste a posição horizontal */
        z-index: 10;
      }
      .copy-button {
        background-color: #28a745; /* Cor verde */
        color: white;
        border: none;
        padding: 5px 10px;
        border-radius: 5px;
        cursor: pointer;
        font-size: 14px;
      }
      .copy-button:hover {
        background-color: #218838; /* Verde mais escuro no hover */
      }
      /* Ajuste para as caixas de texto */
      .form-group.shiny-input-container {
        margin-bottom: 5px; /* Reduz o espaço entre os inputs, de 8px para 5px */
      }
      .form-group.shiny-input-container label {
        margin-bottom: 1px; /* Reduz espaço entre label e input, de 2px para 1px */
        font-size: 13px; /* Mantém o tamanho da fonte dos rótulos */
      }
      .form-control {
        padding: 6px 8px; /* Mantém o padding interno dos inputs */
        height: auto; /* Garante que a altura seja ajustada ao conteúdo */
        font-size: 13px; /* Mantém o tamanho da fonte do texto dentro dos inputs */
      }
      .btn { /* Mantém o tamanho dos botões */
        padding: 6px 12px;
        font-size: 14px;
      }
      hr { /* Ajusta o espaçamento das linhas divisórias */
        margin-top: 10px;
        margin-bottom: 10px;
      }
      .help-block { /* Ajusta o espaçamento do texto de ajuda */
        margin-top: 5px;
        margin-bottom: 5px;
        font-size: 11px; /* Pode diminuir a fonte se quiser economizar mais espaço */
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

      # Caixa de texto para contexto adicional
      textAreaInput(
        inputId = "context_input",
        label = tags$b("Contexto Adicional para a Análise (Opcional):"),
        value = "",
        placeholder = "Ex: 'Focar na campanha de marketing do Banco X.' ou 'A análise deve ser mais formal.'",
        rows = 3
      ),
      helpText("Informações adicionais para guiar a IA na geração da análise tem prioridade sobre o prompt padrão."),

      hr(),

      actionButton(
        inputId = "generate_button",
        label = "Gerar Análise com IA",
        icon = icon("robot"),
        class = "btn-primary btn-lg btn-block"
      ),

      br(),

      br(),
      uiOutput("status_message")
    ),

    mainPanel(
      width = 8,
      tags$h3("Análise Gerada", style = "color: #34495e;"),

      tags$div(class = "analysis-container",
               tags$div(class = "copy-button-container",
                        actionButton(
                          inputId = "copy_button",
                          label = "",
                          icon = icon("copy"),
                          class = "copy-button"
                        )
               ),
               wellPanel(
                 style = "background-color: #ecf0f1; border-color: #bdc3c7; min-height: 500px; overflow-y: auto;",
                 htmlOutput("analysis_output_paragraph")
               )
      )
    )
  )
)

# Server do aplicativo Shiny
server <- function(input, output, session) {

  analysis_result <- reactiveVal(NULL)

  observeEvent(input$generate_button, {

    analysis_result(NULL)
    output$analysis_output_paragraph <- renderText("Gerando análise... Por favor, aguarde.")
    output$status_message <- renderUI(NULL)

    shinyjs::disable("generate_button")

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

      user_context_input <- input$context_input

      incProgress(0.4, detail = "Lendo dados IPD e preparando prompt...")

      tryCatch({
        result <- quaestia::generate_ipd_analysis(
          file_info_uploaded,
          api_key,
          historical_analyses_text,
          user_context = user_context_input
        )
        analysis_result(result)

        incProgress(0.9, detail = "Analise concluida!")

        output$analysis_output_paragraph <- renderUI({
          tags$p(analysis_result())
        })

        output$status_message <- renderUI({
          tags$div(class = "alert alert-success", role = "alert",
                   "Analise gerada com sucesso! Você pode copiar o texto.")
        })

      }, error = function(e) {
        analysis_result(NULL)
        error_message_display <- paste("Erro ao gerar analise:", e$message, "\nPor favor, verifique a API Key, os arquivos (nomes das colunas 'profile', 'ipd', 'periodo', 'dif_ranking' no IPD, e 'analise' no historico) ou sua conexao com a internet.")

        output$analysis_output_paragraph <- renderText(error_message_display)
        output$status_message <- renderUI({
          tags$div(class = "alert alert-danger", role = "alert",
                   paste("Erro ao gerar analise:", e$message))
        })
      }, finally = {
        shinyjs::enable("generate_button")
      })
    })
  })

  observeEvent(input$copy_button, {
    req(analysis_result())

    shinyjs::runjs(paste0("
      var textToCopy = document.getElementById('analysis_output_paragraph').innerText;
      var dummy = document.createElement('textarea');
      document.body.appendChild(dummy);
      dummy.value = textToCopy;
      dummy.select();
      document.execCommand('copy');
      document.body.removeChild(dummy);
      alert('Texto copiado para a área de transferência!');
    "))
  })

}

shinyApp(ui = ui, server = server)
