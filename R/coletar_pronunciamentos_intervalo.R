#' Coleta pronunciamentos da ALMG em um intervalo de datas
#'
#' Consulta a API de dados abertos da Assembleia Legislativa de Minas Gerais (ALMG)
#' e retorna uma tabela com os metadados de pronunciamentos realizados no período especificado.
#' Os resultados são paginados automaticamente.
#'
#' @param ini Data inicial da consulta no formato "AAAAMMDD". Ex: "19950101".
#' @param fim Data final da consulta no formato "AAAAMMDD". Ex: "19951231".
#' @param por_pagina Número de registros por página da API. Máximo recomendado: 100.
#'
#' @return Um `tibble` contendo os pronunciamentos no período com as colunas:
#' \describe{
#'   \item{controle}{Número de controle do pronunciamento}
#'   \item{autor}{Nome do parlamentar}
#'   \item{partido}{Sigla do partido}
#'   \item{tipo}{Tipo do pronunciamento (Discurso, Questão de Ordem, etc.)}
#'   \item{resumo}{Resumo textual}
#'   \item{assunto}{Assuntos indexados}
#'   \item{data}{Data do pronunciamento}
#' }
#'
#' @examples
#' \dontrun{
#' # Coletar pronunciamentos do ano de 2000
#' dados_2000 <- coletar_pronunciamentos_intervalo("20000101", "20001231")
#' }
#'
#' @export
coletar_pronunciamentos_intervalo <- function(ini, fim, por_pagina = 100) {
  # Validação básica dos parâmetros de data
  if (!grepl("^\\d{8}$", ini)) stop("Parâmetro 'ini' deve estar no formato 'AAAAMMDD'")
  if (!grepl("^\\d{8}$", fim)) stop("Parâmetro 'fim' deve estar no formato 'AAAAMMDD'")
  if (!is.numeric(por_pagina) || por_pagina <= 0 || por_pagina > 100) {
    stop("Parâmetro 'por_pagina' deve ser numérico entre 1 e 100")
  }

  base_url <- "https://dadosabertos.almg.gov.br/api/v2/pronunciamentos/pesquisa/direcionada"
  resultados <- list()
  pagina <- 1
  repetir <- TRUE

  message(sprintf("Coletando pronunciamentos de %s a %s...", ini, fim))

  while (repetir) {
    # Monta URL com query
    url <- httr::modify_url(base_url, query = list(tp = por_pagina, p = pagina, ini = ini, fim = fim))

    resp <- httr::GET(url, httr::add_headers(Accept = "application/xml"))

    # Verifica erro HTTP, interrompe se erro
    if (httr::status_code(resp) != 200) {
      warning(sprintf("Erro HTTP %d na página %d (período %s–%s). Parando coleta.",
                      httr::status_code(resp), pagina, ini, fim))
      break
    }

    # Parse XML
    xml <- xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))
    itens <- xml2::xml_find_all(xml, ".//pronunciamento")

    if (length(itens) == 0) {
      message("Nenhum pronunciamento encontrado na página ", pagina, ". Finalizando coleta.")
      break
    }

    # Extrai dados
    dados <- purrr::map_dfr(itens, function(item) {
      tibble::tibble(
        controle = xml2::xml_text(xml2::xml_find_first(item, "controle")),
        autor = xml2::xml_text(xml2::xml_find_first(item, "autor")),
        partido = xml2::xml_text(xml2::xml_find_first(item, "partido")),
        tipo = xml2::xml_text(xml2::xml_find_first(item, "tipo")),
        resumo = xml2::xml_text(xml2::xml_find_first(item, "resumo")),
        assunto = xml2::xml_text(xml2::xml_find_first(item, "assunto")),
        data = xml2::xml_text(xml2::xml_find_first(item, "dataPronunciamento"))
      )
    })

    resultados[[pagina]] <- dados
    message(sprintf("Página %d coletada: %d registros", pagina, nrow(dados)))

    # Parar se menos que por_pagina retornado (última página)
    if (nrow(dados) < por_pagina) {
      repetir <- FALSE
    } else {
      pagina <- pagina + 1
    }
  }

  # Combina resultados
  if (length(resultados) == 0) {
    message("Nenhum dado coletado para o intervalo especificado.")
    return(tibble::tibble())
  }

  dplyr::bind_rows(resultados)
}

