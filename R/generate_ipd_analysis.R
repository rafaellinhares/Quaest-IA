# quaestia/R/generate_ipd_analysis.R

#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr filter arrange
#' @importFrom lubridate parse_date_time
generate_ipd_analysis <- function(file_info, api_key, historical_analyses_text = "") {
  if (is.null(file_info)) {
    stop("Por favor, carregue um arquivo (Excel ou CSV).")
  }

  file_path <- file_info$datapath
  file_ext <- tolower(tools::file_ext(file_info$name))

  ipd_data <- NULL

  if (file_ext == "xlsx") {
    ipd_data <- readxl::read_excel(file_path) # Use readxl::read_excel explicitamente
  } else if (file_ext == "csv") {
    ipd_data <- read.csv(file_path, header = TRUE, sep = ",")
  } else {
    stop("Formato de arquivo não suportado. Por favor, carregue um arquivo .xlsx ou .csv.")
  }

  current_colnames <- tolower(names(ipd_data))

  idx_profile <- which(current_colnames %in% c("profile", "nome", "time", "equipe", "clube"))
  idx_ipd <- which(current_colnames %in% c("ipd", "indice_ipd", "popularidade"))
  idx_periodo <- which(current_colnames %in% c("periodo"))
  idx_dif_ranking <- which(current_colnames %in% c("dif_ranking", "diferenca_ranking", "variacao_ranking"))

  if (length(idx_profile) == 1 && length(idx_ipd) == 1 && length(idx_periodo) == 1 && length(idx_dif_ranking) == 1) {
    names(ipd_data)[idx_profile] <- "Profile"
    names(ipd_data)[idx_ipd] <- "IPD"
    names(ipd_data)[idx_periodo] <- "Periodo"
    names(ipd_data)[idx_dif_ranking] <- "Dif_Ranking"
  } else {
    stop("Erro: Nao foi possivel identificar as colunas 'profile', 'ipd', 'periodo' e 'dif_ranking' no seu arquivo.
          Por favor, verifique se as colunas estao nomeadas corretamente (ou variacoes comuns) no arquivo.")
  }

  ipd_data$IPD <- as.numeric(ipd_data$IPD)
  ipd_data$Dif_Ranking <- as.numeric(ipd_data$Dif_Ranking)

  ipd_data$Periodo_Inicio <- lubridate::parse_date_time(sapply(strsplit(ipd_data$Periodo, " a "), `[`, 1),
                                                        orders = c("d/m/Y", "d/m"),
                                                        truncated = 2)

  latest_period <- max(ipd_data$Periodo_Inicio, na.rm = TRUE)
  ipd_data_current_period <- dplyr::filter(ipd_data, Periodo_Inicio == latest_period)

  ipd_data_filtered <- dplyr::filter(ipd_data_current_period, !grepl("caixa", tolower(Profile)))

  ipd_data_sorted <- dplyr::arrange(ipd_data_filtered, desc(IPD))

  top_5_teams <- head(ipd_data_sorted, 5)

  # Preparar o prompt para o Gemini
  prompt_template <- paste0(
    "Olá Gemini, tudo bem? Então, vou te passar o seguinte prompt.\n\n",
    "[Papel e Estilo]\n",
    "# Você é um analista de mídias sociais, especializado em pesquisas envolvendo publicidade, marketing e métricas de redes sociais. Sua escrita apresenta um tom analítico com um pouco de tom jornalístico.\n\n",
    "[Propósito e Contexto]\n",
    "# Estamos analisando os dados semanais do nosso ranking de popularidade digital (IPD) para o período mais recente. Precisamos de um parágrafo indicando os principais destaques da semana, incluindo as variações de posição no ranking.\n",
    "* Contexto: A análise é sobre o ranking de popularidade digital de marcas/entidades.\n\n",
    "[Exemplos ilustrativos]\n",
    "Aqui estão alguns exemplos de análises anteriores que você pode usar como base para o estilo, tom e estrutura do conteúdo. Cada exemplo é separado por '--- Exemplo Anterior ---':\n",
    "%HISTORICAL_ANALYSES%\n\n",
    "[Controle de Qualidade]\n",
    "Você deve se basear nos exemplos ilustrativos fornecidos para entender o estilo e o tipo de informação qualitativa a ser incluída.\n",
    "O novo texto deve ser original e focar nos dados do ranking atual, destacando a posição atual e a variação de ranking (dif_ranking).\n",
    "A cada pedido, você deve gerar um novo texto.\n\n",
    "[Tarefa]\n",
    "Gerar um parágrafo de texto a partir das orientações elencadas anteriormente e do seguinte ranking de popularidade digital para o período %CURRENT_PERIOD_DISPLAY% (Nome do Player, IPD, Variação de Posição em relação à semana anterior):\n",
    "%RANKING_DATA%"
  )

  final_prompt_with_examples <- gsub("%HISTORICAL_ANALYSES%", historical_analyses_text, prompt_template)

  current_period_display <- unique(top_5_teams$Periodo)
  if (length(current_period_display) > 1 || !grepl("\\d{4}", current_period_display[1])) {
    current_period_display <- current_period_display[1]
  } else {
    current_period_display <- format(latest_period, "%d/%m/%Y")
  }

  ranking_text <- ""
  for (i in 1:nrow(top_5_teams)) {
    dif_ranking_text <- ""
    if (!is.na(top_5_teams$Dif_Ranking[i])) {
      if (top_5_teams$Dif_Ranking[i] > 0) {
        dif_ranking_text <- paste0(" (subiu ", top_5_teams$Dif_Ranking[i], " posicoes)")
      } else if (top_5_teams$Dif_Ranking[i] < 0) {
        dif_ranking_text <- paste0(" (caiu ", abs(top_5_teams$Dif_Ranking[i]), " posicoes)")
      } else {
        dif_ranking_text <- " (manteve a posicao)"
      }
    }
    ranking_text <- paste0(ranking_text,
                           top_5_teams$Profile[i], " (", top_5_teams$IPD[i], ")", dif_ranking_text,
                           ifelse(i < nrow(top_5_teams), ", ", "."))
  }

  full_prompt <- gsub("%RANKING_DATA%", ranking_text, final_prompt_with_examples)
  full_prompt <- gsub("%CURRENT_PERIOD_DISPLAY%", current_period_display, full_prompt)

  # CHAMADA QUALIFICADA: quaestia::query_gemini
  analysis_text <- quaestia::query_gemini(full_prompt, api_key = api_key)

  return(analysis_text)
}
