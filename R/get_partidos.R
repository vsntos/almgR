#' Coleta informações das Partidos parlamentares da ALMG
#'
#' Consulta a API da Assembleia Legislativa de Minas Gerais para obter dados das Partidos parlamentares
#' da legislatura atual.
#'
#' @param formato String. Formato da resposta: "json" (padrão) ou "xml".
#' @return Um tibble com as informações das Partidos, ou tibble vazio em caso de erro.
#' @export
#'
#' @examples
#' \dontrun{
#' partidos <- get_partidos()
#' }
get_partidos <- function(formato = "json") {
  stopifnot(formato %in% c("json", "xml"))

  base_url <- "https://dadosabertos.almg.gov.br/api/v2/representacao_partidaria/partidos"
  url <- httr::modify_url(base_url, query = list(formato = formato))

  resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao acessar dados das Partidos parlamentares", httr::status_code(resp)))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    lista <- dados$list

    if (is.null(lista)) {
      warning("Nenhuma Partido encontrada.")
      return(tibble::tibble())
    }

    tibble::tibble(
      id = vapply(lista, function(x) x$id %||% NA_integer_, integer(1)),
      nome = vapply(lista, function(x) x$nome %||% NA_character_, character(1)),
      sigla = vapply(lista, function(x) x$sigla %||% NA_character_, character(1)),
      descricao = vapply(lista, function(x) x$descricao %||% NA_character_, character(1)),
      dataInicio = vapply(lista, function(x) x$dataInicio %||% NA_character_, character(1)),
      dataFim = vapply(lista, function(x) x$dataFim %||% NA_character_, character(1)),
      quantidadeMembros = vapply(lista, function(x) x$quantidadeMembros %||% NA_integer_, integer(1))
    )
  } else {
    warning("Formato XML não implementado.")
    return(tibble::tibble())
  }
}

partidos <- get_partidos()
