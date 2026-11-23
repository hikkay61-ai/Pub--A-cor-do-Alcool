# --- ETAPA 1: REINSTALAÇÃO DE PACOTES (Executar apenas uma vez) ---

# Como o PC foi formatado, precisamos reinstalar tudo.
pacotes_necessarios <- c(
  "tidyverse",
  "microdatasus",
  "survival",
  "ggsurvfit",
  "writexl",
  "remotes"
)

# Instala apenas os que ainda não estão instalados
novos_pacotes <- pacotes_necessarios[
  !(pacotes_necessarios %in% installed.packages()[, "Package"])
]

if (length(novos_pacotes)) {
  install.packages(novos_pacotes)
}


# --- ETAPA 2: CONFIGURAÇÃO DO PROJETO ---

# Define o caminho para a pasta principal do projeto
caminho_projeto <- "C:/Users/Micro/Documents/Projeto_Pub_Alcool_RacaCor-main/Felipe Nascimento"

# Verifica se a pasta existe; se não, cria ela
if (!dir.exists(caminho_projeto)) {
  dir.create(caminho_projeto, recursive = TRUE)
  message("Pasta criada com sucesso em: ", caminho_projeto)
}

# Define essa pasta como o local de trabalho
setwd(caminho_projeto)

message("Diretório de trabalho definido para: ", getwd())


# --- ETAPA 3: CARREGAMENTO DOS PACOTES ---

library(tidyverse)
library(microdatasus)
library(survival)
library(ggsurvfit)
library(writexl)


# --- ETAPA 4: CONFIGURAÇÕES DE DOWNLOAD ---

# Configuração para download estável
options(download.file.method = "libcurl")

# Aumenta o tempo limite de download
options(timeout = 600)
