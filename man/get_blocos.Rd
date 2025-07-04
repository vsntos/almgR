% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_blocos.R
\name{get_blocos}
\alias{get_blocos}
\title{Coleta lista de Blocos de Representação Partidária da ALMG}
\usage{
get_blocos(formato = "json")
}
\arguments{
\item{formato}{String. Formato da resposta, "json" (padrão) ou "xml".}
}
\value{
Um tibble com informações dos blocos partidários, ou tibble vazio em caso de erro.
}
\description{
Consulta a API da Assembleia Legislativa de Minas Gerais para obter a lista
de blocos de representação partidária da legislatura atual.
}
\examples{
\dontrun{
blocos <- get_blocos()
}

}
