#' Pesquisa direcionada de pronunciamentos da ALMG
#'
#' Realiza uma pesquisa direcionada na API da Assembleia Legislativa de Minas Gerais para obter pronunciamentos
#' conforme filtros de data, expressão e paginação.
#'
#' @param ini Data inicial da pesquisa no formato "AAAAMMDD". Ex: "20220101".
#' @param fim Data final da pesquisa no formato "AAAAMMDD". Ex: "20221231".
#' @param expressao String. Termo ou expressão para busca no texto dos pronunciamentos.
#' @param por_pagina Número de registros por página (máximo recomendado: 100).
#' @param max_paginas Número máximo de páginas para coletar (para limitar a busca).
#'
#' @return Um tibble com os dados dos pronunciamentos encontrados, ou tibble vazio se não houver resultados.
#' @export
#'
#' @examples
#' \dontrun{
#' resultados <- pesquisar_pronunciamentos_direcionada(
#"20220101", "20221231",
#expressao = "saúde", por_pagina = 50, max_paginas = 2
# )

#' }
pesquisar_pronunciamentos_direcionada <- function(ini, fim, expressao = NULL, por_pagina = 100, max_paginas = 5) {
  stopifnot(is.character(ini), is.character(fim), is.numeric(por_pagina), is.numeric(max_paginas))
  stopifnot(por_pagina > 0 && por_pagina <= 100)
  stopifnot(max_paginas > 0)

  base_url <- "https://dadosabertos.almg.gov.br/api/v2/pronunciamentos/pesquisa/direcionada"
  resultados <- list()
  pagina <- 1

  repeat {
    query <- list(
      ini = ini,
      fim = fim,
      tp = por_pagina,
      p = pagina
    )
    if (!is.null(expressao)) {
      query$expressao <- expressao
    }

    url <- httr::modify_url(base_url, query = query)
    resp <- httr::GET(url, httr::accept("application/xml"))

    if (httr::status_code(resp) != 200) {
      warning(sprintf("Erro %d na página %d da pesquisa direcionada.", httr::status_code(resp), pagina))
      break
    }

    xml <- xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))
    itens <- xml2::xml_find_all(xml, ".//pronunciamento")
    if (length(itens) == 0) break

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
    pagina <- pagina + 1
    if (pagina > max_paginas) break
  }

  dplyr::bind_rows(resultados)
}

