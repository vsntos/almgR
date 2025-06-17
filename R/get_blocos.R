#' Coleta lista de Blocos de Representação Partidária da ALMG
#'
#' Consulta a API da Assembleia Legislativa de Minas Gerais para obter a lista
#' de blocos de representação partidária da legislatura atual.
#'
#' @param formato String. Formato da resposta, "json" (padrão) ou "xml".
#' @return Um tibble com informações dos blocos partidários, ou tibble vazio em caso de erro.
#' @export
#'
#' @examples
#' \dontrun{
#' blocos <- get_blocos()
#' }
#'
#' @importFrom httr GET content status_code accept modify_url
#' @importFrom tibble tibble
get_blocos <- function(formato = "json") {
  stopifnot(formato %in% c("json", "xml"))

  base_url <- "https://dadosabertos.almg.gov.br/api/v2/representacao_partidaria/blocos"
  url <- httr::modify_url(base_url, query = list(formato = formato))

  resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao acessar lista de blocos", httr::status_code(resp)))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    lista <- dados$list

    if (is.null(lista) || length(lista) == 0) {
      warning("Nenhum bloco encontrado.")
      return(tibble::tibble())
    }

    tibble::tibble(
      id = vapply(lista, function(x) x$id %||% NA_integer_, integer(1)),
      nome = vapply(lista, function(x) x$nome %||% NA_character_, character(1)),
      sigla = vapply(lista, function(x) x$sigla %||% NA_character_, character(1)),
      tipoBloco = vapply(lista, function(x) {
        if (!is.null(x$tipoBloco)) {
          if (is.list(x$tipoBloco)) x$tipoBloco[[2]] else NA_character_
        } else NA_character_
      }, character(1)),
      legislatura = vapply(lista, function(x) x$legislatura %||% NA_integer_, integer(1)),
      situacao = vapply(lista, function(x) x$situacao %||% NA_character_, character(1))
    )
  } else {
    warning("Formato XML não implementado.")
    return(tibble::tibble())
  }
}


