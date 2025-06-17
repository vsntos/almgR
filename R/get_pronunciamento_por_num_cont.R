#' Coleta detalhes de um pronunciamento pelo número de controle
#'
#' Consulta a API da Assembleia Legislativa de Minas Gerais para obter os detalhes de um pronunciamento específico identificado pelo número de controle.
#'
#' @param numCont Integer. Número de controle do pronunciamento.
#' @param formato String. Formato da resposta: "json" (padrão) ou "xml".
#'
#' @return Um tibble com os detalhes do pronunciamento, ou tibble vazio em caso de erro.
#' @export
#'
#' @examples
#' \dontrun{
#' pronunciamento <- get_pronunciamento_por_num_cont(3047)
#' }
get_pronunciamento_por_num_cont <- function(numCont, formato = "json") {
  stopifnot(is.numeric(numCont), formato %in% c("json", "xml"))

  base_url <- sprintf("https://dadosabertos.almg.gov.br/api/v2/pronunciamentos/%d", numCont)
  url <- httr::modify_url(base_url, query = list(formato = formato))

  resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao acessar pronunciamento com número de controle %d", httr::status_code(resp), numCont))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    pron <- dados$pronunciamento

    if (is.null(pron)) {
      warning("Pronunciamento não encontrado ou vazio.")
      return(tibble::tibble())
    }

    safe_get <- function(x, field, default = NA_character_) {
      if (!is.null(x[[field]])) x[[field]] else default
    }

    tibble::tibble(
      controle = safe_get(pron, "controle"),
      autor = safe_get(pron, "autor"),
      partido = safe_get(pron, "partido"),
      tipo = safe_get(pron, "tipo"),
      resumo = safe_get(pron, "resumo"),
      assunto = safe_get(pron, "assunto"),
      data = as.Date(safe_get(pron, "dataPronunciamento"))
    )
  } else {
    warning("Formato XML não implementado.")
    return(tibble::tibble())
  }
}


