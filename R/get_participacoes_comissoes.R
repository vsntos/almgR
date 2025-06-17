#' Coleta participações em comissões de um deputado na legislatura atual
#'
#' Consulta a API da ALMG para obter as participações de um deputado em comissões na legislatura atual,
#' retornando uma tabela limpa e pronta para análise.
#'
#' @param id_deputado Inteiro. Identificador do deputado.
#' @param formato Formato da resposta, `"json"` (padrão) ou `"xml"`.
#'
#' @return Um tibble com as colunas:
#' \describe{
#'   \item{dataInicio}{Data de início da participação (classe Date)}
#'   \item{dataTermino}{Data de término da participação (classe Date ou NA)}
#'   \item{participacao}{Descrição da participação (ex: Presidente, Efetivo)}
#'   \item{nomeComissao}{Nome da comissão}
#'   \item{tipoComissao}{Tipo da comissão (ex: Comissão Permanente)}
#' }
#'
#' @examples
#' \dontrun{
#' participacoes <- get_participacoes_comissoes(18846)
#' }
#'
#' @export
get_participacoes_comissoes <- function(id_deputado, formato = "json") {
  stopifnot(is.numeric(id_deputado), formato %in% c("json", "xml"))

  url_base <- sprintf("https://dadosabertos.almg.gov.br/api/v2/deputados/%d/participacoes_comissoes", id_deputado)
  url <- httr::modify_url(url_base, query = list(formato = formato))
  resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao acessar participações do deputado %d", httr::status_code(resp), id_deputado))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    lista <- dados$list

    safe_get <- function(x, field, default = NA) if (!is.null(x[[field]])) x[[field]] else default

    # Extrair nome e tipo da comissão
    nome_comissao <- vapply(lista, function(x) safe_get(x$comissao, "nome", NA_character_), character(1))
    tipo_comissao <- vapply(lista, function(x) {
      tc <- safe_get(x$comissao, "tipoComissao", list())
      if (length(tc) >= 2) tc[[2]] else NA_character_
    }, character(1))

    tibble::tibble(
      dataInicio = as.Date(vapply(lista, function(x) safe_get(x, "dataInicio", NA_character_), character(1))),
      dataTermino = as.Date(vapply(lista, function(x) safe_get(x, "dataTermino", NA_character_), character(1))),
      participacao = vapply(lista, function(x) safe_get(x, "participacao", NA_character_), character(1)),
      nomeComissao = nome_comissao,
      tipoComissao = tipo_comissao
    )
  } else {
    warning("Formato XML não implementado.")
    tibble::tibble()
  }
}
