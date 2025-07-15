# quaestia/R/query_gemini.R

#' @export
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @importFrom httr content
#' @importFrom httr status_code
query_gemini <- function(prompt, model = "gemini-2.0-flash", api_key) {
  if (is.null(api_key) || nchar(api_key) == 0 || api_key == "") {
    stop("Por favor, insira uma chave de API Gemini válida.")
  }

  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent?key=", api_key)

  body <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt)
        )
      )
    ),
    generationConfig = list(
      temperature = 0.7,
      maxOutputTokens = 500
    )
  )

  # Chamada qualificada para add_headers
  response <- httr::POST( # Já está qualificado
    url,
    httr::add_headers(`Content-Type` = "application/json"), # <--- ADICIONE httr:: AQUI
    body = jsonlite::toJSON(body, auto_unbox = TRUE) # Já está qualificado
  )

  if (httr::status_code(response) == 200) { # Já está qualificado
    res <- httr::content(response, as = "parsed") # Já está qualificado
    if (!is.null(res$candidates[[1]]$content$parts[[1]]$text)) {
      return(res$candidates[[1]]$content$parts[[1]]$text)
    } else {
      warning("Resposta da API Gemini não contém texto esperado.")
      print(httr::content(response, as = "text")) # Já está qualificado
      return(NA)
    }
  } else {
    warning(paste("Erro na API Gemini:", httr::status_code(response))) # Já está qualificado
    print(httr::content(response, as = "text")) # Já está qualificado
    stop(paste("Erro na API Gemini:", httr::status_code(response), " - ", httr::content(response, as = "text"))) # Já está qualificado
  }
}
