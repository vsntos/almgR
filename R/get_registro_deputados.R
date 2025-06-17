#' Obtém registros detalhados de deputados pelo(s) ID(s)
#'
#' Consulta a API da Assembleia Legislativa de Minas Gerais para retornar os dados detalhados de um ou mais deputados
#' pelo(s) seu(s) identificador(es) único(s).
#'
#' @param ids_deputados Vetor numérico com um ou mais identificadores de deputados.
#' @param formato String. Formato da resposta: "json" (padrão) ou "xml".
#'
#' @return Um `tibble` com os principais campos dos registros dos deputados, incluindo lideranças desaninhadas.
#' Retorna tibble vazio em caso de erro ou resposta vazia.
#'
#' @examples
#' \dontrun{
#' registros <- get_registro_deputados(c(9673, 7752, 28859))
#' }
#'
#' @export
get_registro_deputados <- function(ids_deputados, formato = "json") {
  stopifnot(is.numeric(ids_deputados), formato %in% c("json", "xml"))

  extrair_liderancas <- function(x) {
    if (length(x) == 0 || is.null(x[[1]])) {
      return(tibble::tibble(
        categoria = NA_character_,
        cargo = NA_character_,
        id_lideranca = NA_integer_,
        partido_lideranca = NA_character_
      ))
    }

    purrr::map_dfr(x, function(elem) {
      elems <- as.character(elem)
      n <- length(elems)
      if (n < 4) elems <- c(elems, rep(NA_character_, 4 - n))

      tibble::tibble(
        categoria = elems[1],
        cargo = elems[2],
        id_lideranca = as.integer(elems[3]),
        partido_lideranca = elems[4]
      )
    })
  }

  resultados <- purrr::map_dfr(ids_deputados, function(id) {
    # message(sprintf("Coletando deputado %d...", id)) # opcional, pode ser ativado

    base_url <- sprintf("https://dadosabertos.almg.gov.br/api/v2/deputados/%d", id)
    url <- httr::modify_url(base_url, query = list(formato = formato))
    resp <- httr::GET(url, httr::accept(paste0("application/", formato)))

    if (httr::status_code(resp) != 200) {
      warning(sprintf("Erro %d ao acessar registro do deputado %d", httr::status_code(resp), id))
      return(tibble::tibble())
    }

    if (formato == "json") {
      dados <- httr::content(resp, as = "parsed", type = "application/json")
      registro <- dados$deputado

      if (is.null(registro)) {
        warning(sprintf("Registro do deputado %d não encontrado ou vazio", id))
        return(tibble::tibble())
      }

      registro_df <- tibble::tibble(
        id = registro$id %||% NA_integer_,
        nome = registro$nome %||% NA_character_,
        partido = registro$partido %||% NA_character_,
        sexo = registro$sexo %||% NA_character_,
        dataNascimento = as.Date(registro$dataNascimento %||% NA_character_),
        emailPessoal = registro$emailPessoal %||% NA_character_,
        atividadeProfissional = registro$atividadeProfissional %||% NA_character_,
        cargoAssembleia = registro$cargoAssembleia %||% NA_character_
      )

      liderancas_df <- extrair_liderancas(registro$liderancas %||% list())

      tidyr::crossing(registro_df, liderancas_df)

    } else {
      warning("Formato XML não implementado.")
      tibble::tibble()
    }
  })

  # Corrigir datas inválidas (anos antes de 1900)
  resultados <- resultados %>%
    dplyr::mutate(dataNascimento = dplyr::if_else(lubridate::year(dataNascimento) < 1900, as.Date(NA), dataNascimento))

  resultados
}




