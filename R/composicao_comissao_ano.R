#' Coleta a composição de uma comissão em um ano específico
#'
#' Consulta a API da ALMG para obter a composição da comissão no primeiro dia do ano de referência informado.
#'
#' @param id Integer identificador da comissão (obrigatório).
#' @param ano Integer ano de referência para a composição (obrigatório).
#' @param formato String. Formato da resposta, "json" (padrão) ou "xml".
#'
#' @return Tibble com informações da composição da comissão, incluindo nomes, cargos e datas.
#'
#' @examples
#' \dontrun{
#' comp <- composicao_comissao_ano(8, 2005)
#' }
#'
#' @importFrom httr GET content status_code accept
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @export
composicao_comissao_ano <- function(id, ano, formato = "json") {
  # Validação simples de argumentos
  if (!is.numeric(id) || length(id) != 1) stop("'id' deve ser um número único")
  if (!is.numeric(ano) || length(ano) != 1) stop("'ano' deve ser um número único")
  if (!formato %in% c("json", "xml")) stop("'formato' deve ser 'json' ou 'xml'")

  base_url <- sprintf("https://dadosabertos.almg.gov.br/api/v2/comissoes/%d/composicoes/%d", id, ano)
  resp <- httr::GET(base_url, httr::accept(paste0("application/", formato)))

  # Verifica sucesso HTTP
  if (httr::status_code(resp) != 200) {
    warning(sprintf("Erro %d ao acessar composição da comissão %d no ano %d", httr::status_code(resp), id, ano))
    return(tibble::tibble())
  }

  if (formato == "json") {
    dados <- httr::content(resp, as = "parsed", type = "application/json")
    lista <- dados$list

    if (is.null(lista) || length(lista) == 0) {
      message("Nenhum dado retornado para essa comissão e ano.")
      return(tibble::tibble())
    }

    safe_get <- function(x, field, default = NA_character_) {
      if (!is.null(x[[field]])) x[[field]] else default
    }

    purrr::map_dfr(lista, function(membro) {
      tibble::tibble(
        dataReferencia = safe_get(membro, "dataReferencia"),
        idComissao = safe_get(membro, "idComissao"),
        idCargo = safe_get(membro, "idCargo"),
        dataInicioTitular = safe_get(membro, "dataInicioTitular"),
        dataInicioSuplente = safe_get(membro, "dataInicioSuplente"),
        nomeParlamentarTitular = safe_get(membro, "nomeParlamentarTitular"),
        nomeParlamentarSuplente = safe_get(membro, "nomeParlamentarSuplente"),
        partidoDeputadoTitular = safe_get(membro, "partidoDeputadoTitular"),
        partidoDeputadoSuplente = safe_get(membro, "partidoDeputadoSuplente"),
        idDeputadoTitular = safe_get(membro, "idDeputadoTitular", NA_integer_),
        idDeputadoSuplente = safe_get(membro, "idDeputadoSuplente", NA_integer_),
        ordenacao = safe_get(membro, "ordenacao", NA_integer_),
        cargoComissao = safe_get(membro, "cargoComissao"),
        sexoDeputadoTitular = safe_get(membro, "sexoDeputadoTitular"),
        sexoDeputadoSuplente = safe_get(membro, "sexoDeputadoSuplente")
      )
    })
  } else {
    warning("Formato XML ainda não implementado.")
    return(tibble::tibble())
  }
}

