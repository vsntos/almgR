#' Pesquisa deputados em uma legislatura por situação
#'
#' Consulta a API da Assembleia Legislativa de Minas Gerais para obter deputados
#' de uma legislatura específica filtrando pela situação (ex: em exercício, renunciou, etc.).
#'
#' @param num_legislatura Integer. Número da legislatura (ex: 16).
#' @param situacao Integer. Código da situação:
#'   \itemize{
#'     \item 1 = em exercício
#'     \item 2 = exerceu mandato
#'     \item 3 = renunciou
#'     \item 4 = afastado
#'     \item 5 = perdeu mandato
#'   }
#' @param expr String. Expressão para busca no nome (opcional).
#' @param formato String. Formato da resposta: "json" (padrão) ou "xml".
#'
#' @return Um tibble com os dados dos deputados que correspondem à pesquisa.
#' Retorna tibble vazio em caso de erro ou resposta vazia.
#'
#' @examples
#' \dontrun{
#' # Deputados em exercício na 16ª legislatura
#' deputados_exercicio <- pesquisar_deputados_legislatura_situacao(16, 1)
#' }
#'
#' @importFrom httr GET content status_code accept_json modify_url
#' @importFrom tibble tibble
#' @export
pesquisar_deputados_legislatura_situacao <- function(num_legislatura, situacao, expr = NULL, formato = "json") {
  stopifnot(is.numeric(num_legislatura), is.numeric(situacao), formato %in% c("json", "xml"))

  base_url <- sprintf("https://dadosabertos.almg.gov.br/api/v2/legislaturas/%d/deputados/situacao/%d", num_legislatura, situacao)
  query_list <- list(formato = formato)
  if (!is.null(expr)) query_list$expr <- expr

  url <- httr::modify_url(base_url, query = query_list)
  resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao consultar deputados na legislatura %d com situação %d", httr::status_code(resp), num_legislatura, situacao))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    lista <- dados$listaDeputado

    if (is.null(lista) || length(lista) == 0) {
      warning("Nenhum deputado encontrado para os critérios especificados")
      return(tibble::tibble())
    }

    safe_get <- function(x, field, default = NA) {
      if (!is.null(x[[field]])) x[[field]] else default
    }

    tibble::tibble(
      id = vapply(lista, function(x) safe_get(x, "id", NA_integer_), integer(1)),
      nome = vapply(lista, function(x) safe_get(x, "nome", NA_character_), character(1)),
      partido = vapply(lista, function(x) safe_get(x, "partido", NA_character_), character(1)),
      temDetalhe = vapply(lista, function(x) safe_get(x, "temDetalhe", NA), logical(1)),
      tagLocalizacao = vapply(lista, function(x) safe_get(x, "tagLocalizacao", NA_integer_), integer(1)),
      cargoRepresentacaoPartidaria = vapply(lista, function(x) safe_get(x, "cargoRepresentacaoPartidaria", NA_character_), character(1)),
      dataCargoRepresentacaoPartidaria = vapply(lista, function(x) safe_get(x, "dataCargoRepresentacaoPartidaria", NA_character_), character(1)),
      cargoMesa = vapply(lista, function(x) safe_get(x, "cargoMesa", NA_character_), character(1)),
      dataCargoMesa = vapply(lista, function(x) safe_get(x, "dataCargoMesa", NA_character_), character(1)),
      sexo = vapply(lista, function(x) safe_get(x, "sexo", NA_character_), character(1))
    )
  } else {
    warning("Formato XML não implementado.")
    return(tibble::tibble())
  }
}


