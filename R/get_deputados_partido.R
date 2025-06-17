#' Coleta lista de Deputados de um Partido na ALMG
#'
#' Consulta a API da Assembleia Legislativa de Minas Gerais para obter a lista
#' de deputados pertencentes a um partido político específico da legislatura atual.
#'
#' @param id_partido Inteiro. Identificador do partido.
#' @param formato String. Formato da resposta, "json" (padrão) ou "xml".
#' @return Um tibble com informações dos deputados do partido, ou tibble vazio em caso de erro.
#' @export
#'
#' @examples
#' \dontrun{
#' deputados_partido <- get_deputados_partido(45)
#' }
#'
#' @importFrom httr GET content status_code accept modify_url
#' @importFrom tibble tibble
get_deputados_partido <- function(id_partido, formato = "json") {
  stopifnot(is.numeric(id_partido), formato %in% c("json", "xml"))

  base_url <- sprintf("https://dadosabertos.almg.gov.br/api/v2/representacao_partidaria/partidos/%d/deputados", id_partido)
  url <- httr::modify_url(base_url, query = list(formato = formato))

  resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao acessar lista de deputados do partido %d", httr::status_code(resp), id_partido))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    lista <- dados$list

    if (is.null(lista) || length(lista) == 0) {
      warning("Nenhum deputado encontrado para o partido informado.")
      return(tibble::tibble())
    }

    safe_get <- function(x, field, default = NA) {
      if (!is.null(x[[field]])) x[[field]] else default
    }

    tibble::tibble(
      id = vapply(lista, function(x) safe_get(x, "id", NA_integer_), integer(1)),
      nome = vapply(lista, function(x) safe_get(x, "nome", NA_character_), character(1)),
      partido = vapply(lista, function(x) safe_get(x, "partido", NA_character_), character(1)),
      sexo = vapply(lista, function(x) safe_get(x, "sexo", NA_character_), character(1)),
      tagLocalizacao = vapply(lista, function(x) safe_get(x, "tagLocalizacao", NA_integer_), integer(1)),
      cargoRepresentacaoPartidaria = vapply(lista, function(x) safe_get(x, "cargoRepresentacaoPartidaria", NA_character_), character(1))
    )

  } else {
    warning("Formato XML não implementado.")
    return(tibble::tibble())
  }
}

