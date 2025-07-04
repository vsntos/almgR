% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_deputados_partido.R
\name{get_deputados_partido}
\alias{get_deputados_partido}
\title{Coleta lista de Deputados de um Partido na ALMG}
\usage{
get_deputados_partido(id_partido, formato = "json")
}
\arguments{
\item{id_partido}{Inteiro. Identificador do partido.}

\item{formato}{String. Formato da resposta, "json" (padrão) ou "xml".}
}
\value{
Um tibble com informações dos deputados do partido, ou tibble vazio em caso de erro.
}
\description{
Consulta a API da Assembleia Legislativa de Minas Gerais para obter a lista
de deputados pertencentes a um partido político específico da legislatura atual.
}
\examples{
\dontrun{
deputados_partido <- get_deputados_partido(45)
}

}
