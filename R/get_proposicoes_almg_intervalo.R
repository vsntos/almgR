#' Obtem dados de proposições da ALMG para um intervalo de anos
#'
#' Baixa dados de proposições da ALMG para cada ano no intervalo informado,
#' seleciona as colunas principais relacionadas à proposição e retorna um tibble.
#'
#' @param anos Vetor numérico com os anos desejados.
#' @param tipo Tipo do arquivo para download ("CSV" por padrão).
#' @return Tibble com dados das proposições.
#' @importFrom utils download.file read.csv
#' @importFrom dplyr select all_of
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' \dontrun{
#' # Baixar proposições dos anos de 2015 a 2023
#' proposicoes <- get_proposicoes_almg_intervalo(2015:2023)
#' head(proposicoes)
#' }
get_proposicoes_almg_intervalo <- function(anos, tipo = "CSV") {

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

  vars_proposicao <- c("Codigo", "TipoProposicao", "SiglaTipoProposicao", "Numero", "Ano",
                       "Ementa", "Indexacao", "Situacao", "DataPublicacao", "DataAtualizacao",
                       "DataUltimaAcao", "Regime", "Resumo", "Origem", "Local", "NomeFaseAtual",
                       "Legislatura", "LinkTextos", "ano")

  dados_proposicoes <- dados %>%
    dplyr::select(dplyr::all_of(vars_proposicao)) %>%
    tibble::as_tibble()

  return(dados_proposicoes)
}
