#' Baixa e empilha autores das proposições da ALMG para um intervalo de anos
#'
#' Esta função baixa os dados de proposições da ALMG para cada ano no intervalo informado,
#' extrai e "explode" os autores das proposições (um autor por linha),
#' retornando um tibble com dados dos autores e identificação da proposição.
#'
#' @param anos Vetor numérico com os anos desejados, ex: 2015:2023
#' @param tipo Tipo de arquivo para download (padrão: "CSV")
#' @return Tibble com dados dos autores de proposições da ALMG.
#' @importFrom utils download.file read.csv
#' @importFrom dplyr select mutate bind_rows all_of
#' @importFrom tidyr unnest
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' \dontrun{
#' autores <- get_autores_almg_intervalo(2015:2023)
#' }
get_autores_almg_intervalo <- function(anos, tipo = "CSV") {

  if (getRversion() >= "2.15.1")  utils::globalVariables(c("Autores"))

  baixar_dados_almg <- function(anos, tipo) {
    base_url <- "https://dadosabertos.almg.gov.br/arquivo/proposicoes/download"

    baixar_ano <- function(ano) {
      message(sprintf("Baixando dados do ano %d...", ano))
      url <- paste0(base_url, "?ano=", ano, "&tipo=", tipo)
      tmp <- tempfile(fileext = ".csv")
      tryCatch({
        download.file(url, tmp, quiet = TRUE)
        df <- read.csv(tmp, stringsAsFactors = FALSE)
        df$ano <- ano
        message("Download concluído.")
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

  dados <- baixar_dados_almg(anos, tipo)

  # Selecionar variáveis de identificação e autores
  vars_id <- c("Codigo", "TipoProposicao", "SiglaTipoProposicao", "Numero", "Ano", "ano")
  dados_autores <- dados %>%
    dplyr::select(dplyr::all_of(c(vars_id, "Autores"))) %>%
    dplyr::mutate(
      Autores = lapply(Autores, function(x) {
        # Parse JSON da coluna Autores (que é string JSON)
        jsonlite::fromJSON(x)
      })
    ) %>%
    tidyr::unnest(cols = c(Autores)) %>%
    tibble::as_tibble()

  return(dados_autores)
}
