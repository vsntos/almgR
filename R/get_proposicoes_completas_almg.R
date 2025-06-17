#' Baixa proposições da ALMG com textos completos para um intervalo de anos
#'
#' Esta função baixa os dados de proposições da ALMG para cada ano no intervalo informado,
#' baixa os textos (original e outros textos) de cada proposição e retorna um tibble
#' combinando as informações.
#'
#' @param anos Vetor numérico com os anos desejados, ex: 2015:2023
#' @param tipo Tipo do arquivo para download (padrão: "CSV")
#' @param incluir_outros_textos Booleano, se TRUE inclui os textos adicionais (default FALSE)
#' @return Tibble com as proposições e seus textos completos.
#' @importFrom utils download.file read.csv
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select all_of bind_cols mutate
#' @importFrom tibble as_tibble
#' @importFrom purrr map map_df
#' @export
get_proposicoes_completas_almg <- function(anos, tipo = "CSV", incluir_outros_textos = FALSE) {

  baixar_dados_almg <- function(anos, tipo) {
    base_url <- "https://dadosabertos.almg.gov.br/arquivo/proposicoes/download"

    baixar_ano <- function(ano) {
      message(sprintf("Baixando dados do ano %d...", ano))
      url <- paste0(base_url, "?ano=", ano, "&tipo=", tipo)
      tmp <- tempfile(fileext = ".csv")
      tryCatch({
        download.file(url, tmp, quiet = TRUE)
        df <- utils::read.csv(tmp, stringsAsFactors = FALSE)
        df$ano <- ano
        message("Download prévio concluído.")
        return(df)
      }, error = function(e) {
        warning(sprintf("Erro ao baixar dados do ano %d: %s", ano, e$message))
        return(NULL)
      }, finally = {
        if (file.exists(tmp)) file.remove(tmp)
      })
    }

    dados_anos <- lapply(anos, baixar_ano)
    dados_anos <- Filter(Negate(is.null), dados_anos)
    dados <- do.call(rbind, dados_anos)
    rownames(dados) <- NULL
    return(dados)
  }

  baixar_texto <- function(link) {
    if (is.na(link) || link == "") return(NULL)
    tryCatch({
      resp <- jsonlite::fromJSON(link)
      if (incluir_outros_textos) {
        list(
          textoOriginal = resp$textos$textoOriginal$texto,
          outrosTextos = resp$textos$outrosTextos
        )
      } else {
        list(textoOriginal = resp$textos$textoOriginal$texto)
      }
    }, error = function(e) {
      warning(sprintf("Erro ao baixar texto: %s", e$message))
      return(list(textoOriginal = NA, outrosTextos = NULL))
    })
  }

  dados <- baixar_dados_almg(anos, tipo)

  # Seleciona colunas básicas da proposição
  vars_proposicao <- c("Codigo", "TipoProposicao", "SiglaTipoProposicao", "Numero", "Ano",
                       "Ementa", "Indexacao", "Situacao", "DataPublicacao", "DataAtualizacao",
                       "DataUltimaAcao", "Regime", "Resumo", "Origem", "Local", "NomeFaseAtual",
                       "Legislatura", "LinkTextos", "ano")

  dados_proposicoes <- dados %>%
    dplyr::select(dplyr::all_of(vars_proposicao)) %>%
    tibble::as_tibble()

  # Baixa os textos para cada proposição
  textos_list <- purrr::map(dados_proposicoes$LinkTextos, baixar_texto)

  # Extrai colunas dos textos em um data.frame
  if (incluir_outros_textos) {
    textos_df <- purrr::map_df(textos_list, ~ tibble(
      textoOriginal = .x$textoOriginal,
      outrosTextos = list(.x$outrosTextos)
    ))
  } else {
    textos_df <- purrr::map_df(textos_list, ~ tibble(
      textoOriginal = .x$textoOriginal
    ))
  }

  # Junta proposições e textos
  dados_completos <- dplyr::bind_cols(dados_proposicoes, textos_df)

  return(dados_completos)
}

