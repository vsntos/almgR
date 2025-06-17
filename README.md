
<!-- README.md is generated from README.Rmd. Please edit that file -->

almgR <!-- badges: start --> <!-- badges: end --> O pacote almgR fornece
funções para coletar e manipular dados abertos da Assembleia Legislativa
de Minas Gerais (ALMG) via sua API REST pública.

Com o almgR você pode:

Buscar pronunciamentos, deputados, comissões, proposições e outros dados
legislativos.

Trabalhar com paginação e tratamentos automáticos de erros.

Salvar e organizar dados para análises em R.

Automatizar rotinas periódicas de coleta.

Instalação Instale a versão de desenvolvimento diretamente do GitHub
com:

r Copiar \# Instale o pacote devtools caso não tenha
install.packages(“devtools”)

# Instale almgR da sua conta GitHub

devtools::install_github(“seu_usuario/almgR”) Exemplo básico r Copiar
library(almgR)

# Coletar pronunciamentos de um intervalo de datas

pronunciamentos \<- coletar_pronunciamentos_intervalo(“20240101”,
“20241231”)

# Visualizar os primeiros registros

head(pronunciamentos)

# Buscar deputados em exercício

deputados \<- get_deputados_em_exercicio()

# Visualizar estrutura dos dados

glimpse(deputados)
