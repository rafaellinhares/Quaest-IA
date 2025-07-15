# quaestia/R/run_app.R

#' @title Iniciar o Dashboard de Analise de Popularidade Digital (IPD)
#'
#' @description Esta funcao inicia o aplicativo Shiny para a analise de Popularidade Digital.
#' Ele permite que analistas carreguem dados de IPD e gerem analises usando a API do Google Gemini.
#'
#' @details O dashboard permite carregar um arquivo com dados de IPD (profile, ipd, periodo, dif_ranking),
#' filtrar pelo periodo mais recente, excluir o player 'Caixa' e gerar um paragrafo de analise
#' com IA, alem de permitir o carregamento de exemplos de analises anteriores.
#'
#' @return Uma aplicacao Shiny rodando em seu navegador.
#' @export
#' @importFrom shinythemes shinytheme
#'
#' @examples
#' if (interactive()) {
#'   quaestia()
#' }
quaestia <- function() {
  appDir <- system.file("app", package = "quaestia")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `quaestia`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
