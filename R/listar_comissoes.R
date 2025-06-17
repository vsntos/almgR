#' Lista as comissões da ALMG
#'
#' Consulta a API pública da Assembleia Legislativa de Minas Gerais (ALMG)
#' para obter a lista de comissões, ordenadas por identificador.
#'
#' @param formato Formato da resposta, "json" (padrão) ou "xml".
#'
#' @return Um `tibble` com as colunas principais das comissões, incluindo:
#' \describe{
#'   \item{id}{Identificador da comissão}
#'   \item{nome}{Nome da comissão}
#'   \item{dataInicio}{Data de início da comissão}
#'   \item{dataTermino}{Data de término da comissão, se houver}
#'   \item{ementa}{Ementa ou descrição}
#'   \item{email}{E-mail de contato}
#'   \item{diaReuniao}{Dia da semana para reunião}
#'   \item{horaReuniao}{Hora da reunião}
#'   \item{nomeCompleto}{Nome completo da comissão}
#'   \item{nomeReduzido}{Nome reduzido}
#'   \item{playlist}{URL da playlist de mídias}
#'   \item{tagLocalizacao}{Identificador de localização}
#' }
#'
#' @examples
#' \dontrun{
#' comissoes <- listar_comissoes()
#' print(comissoes)
#' }
#'
#' @importFrom httr GET content status_code accept_json modify_url
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
listar_comissoes <- function(formato = "json") {
  stopifnot(formato %in% c("json", "xml"))

  base_url <- "https://dadosabertos.almg.gov.br/api/v2/comissoes/lista"
  url <- httr::modify_url(base_url, query = list(formato = formato))

  resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao acessar API de comissões", httr::status_code(resp)))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    lista <- dados$list

    if (is.null(lista) || length(lista) == 0) {
      warning("Nenhuma comissão encontrada.")
      return(tibble::tibble())
    }

    safe_get <- function(x, field, default = NA) {
      if (!is.null(x[[field]])) x[[field]] else default
    }

    tibble::tibble(
      id = vapply(lista, function(x) safe_get(x, "id", NA_integer_), integer(1)),
      nome = vapply(lista, function(x) safe_get(x, "nome", NA_character_), character(1)),
      dataInicio = vapply(lista, function(x) safe_get(x, "dataInicio", NA_character_), character(1)),
      dataTermino = vapply(lista, function(x) safe_get(x, "dataTermino", NA_character_), character(1)),
      ementa = vapply(lista, function(x) safe_get(x, "ementa", NA_character_), character(1)),
      email = vapply(lista, function(x) safe_get(x, "email", NA_character_), character(1)),
      diaReuniao = vapply(lista, function(x) safe_get(x, "diaReuniao", NA_integer_), integer(1)),
      horaReuniao = vapply(lista, function(x) safe_get(x, "horaReuniao", NA_character_), character(1)),
      nomeCompleto = vapply(lista, function(x) safe_get(x, "nomeCompleto", NA_character_), character(1)),
      nomeReduzido = vapply(lista, function(x) safe_get(x, "nomeReduzido", NA_character_), character(1)),
      playlist = vapply(lista, function(x) safe_get(x, "playlist", NA_character_), character(1)),
      tagLocalizacao = vapply(lista, function(x) safe_get(x, "tagLocalizacao", NA_integer_), integer(1))
    )
  } else {
    warning("Formato XML ainda não implementado.")
    return(tibble::tibble())
  }
}


