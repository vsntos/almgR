
Vinicius Santos <vsntos.github.io/almgR>

Doutor em Ciência Política, Universidade Federal de Minas Gerais (UFMG). Ciêntista de Dados e Cientista de Redes.

*almgR* é um pacote R para coletar dados abertos da Assembleia Legislativa de Minas Gerais (ALMG) via sua API REST pública.

## Funcionalidades principais

- Coleta de dados legislativos: pronunciamentos, deputados, comissões, proposições e mais.

- Tratamento automático de paginação e erros de API.

- Estruturação e organização dos dados para análise.

- Automatização de rotinas periódicas de coleta e atualização.

## Instalação

Instale a versão de desenvolvimento diretamente do GitHub com:

## Instale o pacote devtools caso ainda não tenha

install.packages("devtools")

## Instale o almgR da sua conta GitHub

devtools::install_github("vsntos/almgR")

## Exemplo básico de uso

Coletar pronunciamentos entre 1º de janeiro e 31 de dezembro de 2024

pronunciamentos <- coletar_pronunciamentos_intervalo("20240101", "20241231")

## Visualizar os primeiros registros

head(pronunciamentos)

Buscar deputados em exercício

deputados <- get_deputados_em_exercicio()

## Visualizar estrutura dos dados

glimpse(deputados)


## [Contribuição](#Contribuição) {#Contribuição}

Contribuições são bem-vindas! Abra issues e pull requests no repositório almgR.

## [Citação](#Citação) {#Citação}

Santos, Vinicius (2025). *almgR*: pacote R para coletar dados abertos da Assembleia Legislativa de Minas Gerais (ALMG). R package. <https://github.com/vsntos/almgR>

