% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coletar_agenda_diaria.R
\name{coletar_agenda_diaria}
\alias{coletar_agenda_diaria}
\title{Coletar agenda diária da ALMG}
\usage{
coletar_agenda_diaria(secao, ano, mes, dia)
}
\arguments{
\item{secao}{String com a sigla da seção legislativa (ex: "home", "PL", "PEC").}

\item{ano}{Número inteiro do ano (ex: 2023).}

\item{mes}{Número inteiro do mês (ex: 6 para junho).}

\item{dia}{Número inteiro do dia do mês (ex: 16).}
}
\value{
Um tibble com os eventos da agenda diária. Colunas: codigoInterno, titulo,
subtitulo, local, descricao, diaInicial, diaFinal, horaInicial, horaFinal, situacao, urlDetalhe.
Retorna tibble vazio se não houver eventos ou NULL se erro na consulta.
}
\description{
Esta função consulta a API pública da Assembleia Legislativa de Minas Gerais
para coletar os eventos agendados em uma dada seção legislativa, data específica.
Retorna um tibble com os principais campos da agenda diária.
}
\examples{
\dontrun{
agenda <- coletar_agenda_diaria(secao = "home", ano = 2010, mes = 6, dia = 11)
print(agenda)
}
}
