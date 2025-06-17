#' Obtém os tipos de pronunciamentos da ALMG
#'
#' Consulta o endpoint de tipos de pronunciamentos da Assembleia Legislativa de Minas Gerais
#' e retorna um tibble com identificadores e descrições.
#'
#' @return Um tibble com colunas `id` e `descricao`.
#'
#' @examples
#' \dontrun{
#' tipos <- get_tipos_pronunciamentos()
#' print(tipos)
#' }
#'
#' @importFrom httr GET accept_json status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
get_tipos_pronunciamentos <- function() {
  url <- "https://dadosabertos.almg.gov.br/api/v2/pronunciamentos/tipos"
  resp <- httr::GET(url, httr::accept_json())

  if (httr::status_code(resp) != 200) {
    stop("Erro na requisição: ", httr::status_code(resp))
  }

  conteudo <- httr::content(resp, as = "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(conteudo, simplifyDataFrame = TRUE)

  if (is.null(json$listaTipoPronunciamento)) {
    stop("Resposta inesperada: campo 'listaTipoPronunciamento' ausente.")
  }

  tibble::as_tibble(json$listaTipoPronunciamento)
}


