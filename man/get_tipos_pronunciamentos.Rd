% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tipos_pronunciamentos.R
\name{get_tipos_pronunciamentos}
\alias{get_tipos_pronunciamentos}
\title{Obtém os tipos de pronunciamentos da ALMG}
\usage{
get_tipos_pronunciamentos()
}
\value{
Um tibble com colunas \code{id} e \code{descricao}.
}
\description{
Consulta o endpoint de tipos de pronunciamentos da Assembleia Legislativa de Minas Gerais
e retorna um tibble com identificadores e descrições.
}
\examples{
\dontrun{
tipos <- get_tipos_pronunciamentos()
print(tipos)
}

}
