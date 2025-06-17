#' Coletar agenda diária da ALMG
#'
#' Esta função consulta a API pública da Assembleia Legislativa de Minas Gerais
#' para coletar os eventos agendados em uma dada seção legislativa, data específica.
#' Retorna um tibble com os principais campos da agenda diária.
#'
#' @param secao String com a sigla da seção legislativa (ex: "home", "PL", "PEC").
#' @param ano Número inteiro do ano (ex: 2023).
#' @param mes Número inteiro do mês (ex: 6 para junho).
#' @param dia Número inteiro do dia do mês (ex: 16).
#' @return Um tibble com os eventos da agenda diária. Colunas: codigoInterno, titulo,
#' subtitulo, local, descricao, diaInicial, diaFinal, horaInicial, horaFinal, situacao, urlDetalhe.
#' Retorna tibble vazio se não houver eventos ou NULL se erro na consulta.
#' @import httr
#' @importFrom dplyr tibble
#' @importFrom purrr map_dfr
#' @export
#' @examples
#' \dontrun{
#' agenda <- coletar_agenda_diaria(secao = "home", ano = 2010, mes = 6, dia = 11)
#' print(agenda)
#' }
coletar_agenda_diaria <- function(secao, ano, mes, dia) {
  url <- sprintf("https://dadosabertos.almg.gov.br/api/v2/agenda/diaria/%s/%d/%02d/%02d",
                 secao, ano, mes, dia)

  res <- httr::GET(url)

  if (httr::status_code(res) != 200) {
    warning("Erro ao acessar a agenda: ", httr::status_code(res))
    return(NULL)
  }

  conteudo <- httr::content(res, "parsed")

  if (!"agendaDiaria" %in% names(conteudo)) {
    warning("Resposta sem agendaDiaria")
    return(NULL)
  }

  itens <- conteudo$agendaDiaria$itens

  if (length(itens) == 0) {
    message("Nenhum evento para esta data.")
    return(dplyr::tibble())
  }

  df <- purrr::map_dfr(itens, ~{
    dplyr::tibble(
      codigoInterno = .x$codigoInterno %||% NA_character_,
      titulo = .x$titulo %||% NA_character_,
      subtitulo = .x$subtitulo %||% NA_character_,
      local = .x$local %||% NA_character_,
      descricao = .x$descricao %||% NA_character_,
      diaInicial = .x$diaInicial %||% NA_character_,
      diaFinal = .x$diaFinal %||% NA_character_,
      horaInicial = .x$horaInicial %||% NA_character_,
      horaFinal = .x$horaFinal %||% NA_character_,
      situacao = .x$situacao %||% NA_character_,
      urlDetalhe = .x$urlDetalhe %||% NA_character_
    )
  })

  return(df)
}
