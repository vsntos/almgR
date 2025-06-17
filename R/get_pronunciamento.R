#' Coleta detalhes de pronunciamentos dado um vetor de IDs
#'
#' @param numCont_vec Vetor de IDs dos pronunciamentos (numéricos ou caracteres)
#' @return Tibble com os dados dos pronunciamentos e coluna texto limpa (texto sem HTML)
#' @export
get_pronunciamentos <- function(numCont_vec) {
  # Garantir que numCont_vec é numérico, removendo NAs
  numCont_vec <- as.numeric(as.character(numCont_vec))
  if (any(is.na(numCont_vec))) {
    warning("Alguns IDs não são numéricos e foram removidos.")
    numCont_vec <- numCont_vec[!is.na(numCont_vec)]
  }

  safe_get <- function(x, field, default = NA_character_) {
    if (!is.null(x[[field]])) x[[field]] else default
  }

  resultados <- vector("list", length(numCont_vec))
  total <- length(numCont_vec)

  for (i in seq_along(numCont_vec)) {
    numCont <- numCont_vec[i]
    message(sprintf("Coletando pronunciamento %d (%d de %d)...", numCont, i, total))

    url <- paste0("https://dadosabertos.almg.gov.br/api/v2/pronunciamentos/", numCont)
    resp <- httr::GET(url, httr::accept("application/json"))

    if (httr::status_code(resp) != 200) {
      warning(sprintf("Erro %d no pronunciamento %d", httr::status_code(resp), numCont))
      next
    }

    dados <- httr::content(resp, as = "parsed", type = "application/json")
    registro <- dados$pronunciamento

    texto_bruto <- safe_get(registro, "texto", NA_character_)
    texto_limpo <- if (!is.na(texto_bruto)) {
      texto_html <- xml2::read_html(texto_bruto)
      xml2::xml_text(texto_html, trim = TRUE)
    } else {
      NA_character_
    }

    resultados[[i]] <- tibble::tibble(
      controle = safe_get(registro, "controle"),
      autor = safe_get(registro, "autor"),
      partido = safe_get(registro, "partido"),
      tipo = safe_get(registro, "tipo"),
      resumo = safe_get(registro, "resumo"),
      assunto = safe_get(registro, "assunto"),
      data = as.Date(safe_get(registro, "dataPronunciamento")),
      texto = texto_limpo
    )
  }

  dplyr::bind_rows(resultados)
}



