# ============================================================
# 02_SIM_obitos.R
# Dados de mortalidade SIM — São Paulo, 2023-2024
# ============================================================

library(readxl)

message("Carregando dados de óbitos de 2023...")

dados_2023 <- readxl::read_excel(
  file.path(caminho_projeto, "dadosfaa_GERAL_2023.xlsx")
)

message("Carregando dados de óbitos de 2024...")

dados_2024 <- readxl::read_excel(
  file.path(caminho_projeto, "dadosfaa_GERAL_2024.xlsx")
)

message("--- SUCESSO! DADOS DE ÓBITOS CARREGADOS ---")
