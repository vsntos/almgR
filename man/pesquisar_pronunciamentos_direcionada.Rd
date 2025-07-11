% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pesquisar_pronunciamentos_direcionada.R
\name{pesquisar_pronunciamentos_direcionada}
\alias{pesquisar_pronunciamentos_direcionada}
\title{Pesquisa direcionada de pronunciamentos da ALMG}
\usage{
pesquisar_pronunciamentos_direcionada(
  ini,
  fim,
  expressao = NULL,
  por_pagina = 100,
  max_paginas = 5
)
}
\arguments{
\item{ini}{Data inicial da pesquisa no formato "AAAAMMDD". Ex: "20220101".}

\item{fim}{Data final da pesquisa no formato "AAAAMMDD". Ex: "20221231".}

\item{expressao}{String. Termo ou expressão para busca no texto dos pronunciamentos.}

\item{por_pagina}{Número de registros por página (máximo recomendado: 100).}

\item{max_paginas}{Número máximo de páginas para coletar (para limitar a busca).}
}
\value{
Um tibble com os dados dos pronunciamentos encontrados, ou tibble vazio se não houver resultados.
}
\description{
Realiza uma pesquisa direcionada na API da Assembleia Legislativa de Minas Gerais para obter pronunciamentos
conforme filtros de data, expressão e paginação.
}
\examples{
\dontrun{
resultados <- pesquisar_pronunciamentos_direcionada(
}
}
