# ==============================================================================
# ARQUIVO: 05_mapa_CAPS.R
# OBJETIVO: Cruzar atendimentos do CAPS (SIA-PS) com nomes das unidades (CNES)
# ==============================================================================

caminho_projeto <- path.expand("~/Projeto_Pub_Alcool_RacaCor-main/Felipe Nascimento")
setwd(caminho_projeto)

library(tidyverse)
library(microdatasus)
library(writexl)

options(timeout = 1200) # 20 minutos de limite para arquivos pesados

# 1. VERIFICAÇÃO DA BASE DE PACIENTES (SIA-PS 2024)
# ------------------------------------------------------------------------------
if (!exists("sia_raw")) {
  message("1/4: Baixando dados do SIA-PS (2024)...")
  Sys.setlocale("LC_TIME", "C")
  sia_raw <- fetch_datasus(
    year_start = 2024, month_start = 1,
    year_end = 2024, month_end = 12,
    uf = "SP",
    information_system = "SIA-PS"
  )
  Sys.setlocale("LC_TIME", "")
} else {
  message("1/4: Aproveitando o 'sia_raw' já carregado na memória...")
}

# 2. PREPARAR A BASE DE PACIENTES DA CAPITAL
# ------------------------------------------------------------------------------
message("2/4: Organizando planilha de atendimentos na capital de SP...")

planilha_caps_sp <- sia_raw %>%
  mutate(
    RACACOR_NUM = as.numeric(RACACOR),
    RACA_COR_TEXTO = case_when(
      RACACOR_NUM == 1 ~ "Branca",
      RACACOR_NUM == 2 ~ "Preta",
      RACACOR_NUM == 3 ~ "Parda",
      RACACOR_NUM == 4 ~ "Amarela",
      RACACOR_NUM == 5 ~ "Indígena",
      TRUE ~ "Sem Informação"
    ),
    MUNPAC = as.character(MUNPAC),
    GRUPO_CID = str_sub(CIDPRI, 1, 3)
  ) %>%
  filter(MUNPAC == "355030") %>%
  filter(GRUPO_CID %in% c("F10", "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19")) %>%
  arrange(CNES_EXEC) %>%
  select(
    CODIGO_CAPS = CNES_EXEC,
    CID_PRINCIPAL = CIDPRI,
    RACA_COR = RACA_COR_TEXTO,
    SEXO = SEXOPAC,
    IDADE = IDADEPAC,
    DATA_ATENDIMENTO = DT_ATEND
  )

# 3. BAIXAR DICIONÁRIO DE NOMES DO CNES (FTP DATASUS)
# ------------------------------------------------------------------------------
message("3/4: Baixando base nacional do CNES (545 MB). Aguarde...")

url_cnes_zip <- "ftp://ftp.datasus.gov.br/cnes/BASE_DE_DADOS_CNES_202401.ZIP"
arquivo_zip <- tempfile(fileext = ".zip")

download.file(url_cnes_zip, destfile = arquivo_zip, mode = "wb")

message("Extraindo tabela de estabelecimentos...")
arquivos_no_zip <- unzip(arquivo_zip, list = TRUE)$Name
nome_arquivo_estab <- arquivos_no_zip[grep("tbEstabelecimento", arquivos_no_zip, ignore.case = TRUE)][1]

pasta_temp <- tempdir()
unzip(arquivo_zip, files = nome_arquivo_estab, exdir = pasta_temp)

dicionario_cnes <- read_delim(
  file.path(pasta_temp, nome_arquivo_estab),
  delim = ";",
  locale = locale(encoding = "latin1"), 
  col_types = cols(.default = "c"),     
  show_col_types = FALSE
) %>%
  select(
    CODIGO_CAPS = CO_CNES,     
    NOME_DO_CAPS = NO_FANTASIA 
  ) %>%
  distinct(CODIGO_CAPS, .keep_all = TRUE)

# 4. CRUZAMENTO FINAL E EXPORTAÇÃO PARA EXCEL
# ------------------------------------------------------------------------------
message("4/4: Cruzando códigos com os nomes dos CAPS e salvando Excel...")

planilha_final_caps <- planilha_caps_sp %>%
  left_join(dicionario_cnes, by = "CODIGO_CAPS") %>%
  relocate(NOME_DO_CAPS, .after = CODIGO_CAPS)

caminho_excel_caps <- file.path(caminho_projeto, "atendimentos_CAPS_AD_SP_2024.xlsx")
write_xlsx(planilha_final_caps, caminho_excel_caps)

# Limpa arquivos temporários da memória
unlink(arquivo_zip)
gc()

message("--- SUCESSO! Planilha com nomes dos CAPS salva em: ", caminho_excel_caps, " ---")