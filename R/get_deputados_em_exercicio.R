#' Lista os deputados em exercício na legislatura atual da ALMG
#'
#' Consulta a API de dados abertos da Assembleia Legislativa de Minas Gerais (ALMG)
#' e retorna uma tabela com informações dos deputados em exercício.
#'
#' @param formato Formato da resposta da API, pode ser "json" (padrão) ou "xml".
#'
#' @return Um `tibble` com as colunas:
#' \describe{
#'   \item{id}{Número identificador do deputado}
#'   \item{nome}{Nome completo do deputado}
#'   \item{partido}{Sigla do partido político}
#'   \item{temDetalhe}{Indica se o deputado possui detalhes adicionais (boolean)}
#'   \item{tagLocalizacao}{Código da localização do deputado}
#'   \item{cargoRepresentacaoPartidaria}{Cargo de representação partidária (se existir)}
#'   \item{liderancas}{Lista de lideranças associadas ao deputado (se existir)}
#'   \item{dataCargoRepresentacaoPartidaria}{Data do cargo de representação partidária (se existir)}
#'   \item{cargoMesa}{Cargo na mesa diretora (se existir)}
#'   \item{dataCargoMesa}{Data do cargo na mesa diretora (se existir)}
#'   \item{sexo}{Sexo do deputado}
#' }
#'
#' @examples
#' \dontrun{
#' deputados <- get_deputados_em_exercicio()
#' print(deputados)
#' }
#'
#' @export
#' @importFrom httr GET content status_code accept modify_url
#' @importFrom tibble tibble
get_deputados_em_exercicio <- function(formato = "json") {
  stopifnot(formato %in% c("json", "xml"))

  base_url <- "https://dadosabertos.almg.gov.br/api/v2/deputados/em_exercicio"
  url <- httr::modify_url(base_url, query = list(formato = formato))

  resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao acessar API de deputados", httr::status_code(resp)))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    lista <- dados$list

    if (is.null(lista) || length(lista) == 0) {
      warning("Nenhum deputado encontrado.")
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
      liderancas = I(lapply(lista, function(x) safe_get(x, "liderancas", list()))),
      dataCargoRepresentacaoPartidaria = vapply(lista, function(x) safe_get(x, "dataCargoRepresentacaoPartidaria", NA_character_), character(1)),
      cargoMesa = vapply(lista, function(x) safe_get(x, "cargoMesa", NA_character_), character(1)),
      dataCargoMesa = vapply(lista, function(x) safe_get(x, "dataCargoMesa", NA_character_), character(1)),
      sexo = vapply(lista, function(x) safe_get(x, "sexo", NA_character_), character(1))
    )

  } else {
    warning("Formato XML ainda não implementado.")
    return(tibble::tibble())
  }
}




