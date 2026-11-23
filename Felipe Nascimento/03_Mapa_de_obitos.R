# ============================================================
# 03_mapa_obitos.R
# Mapa de óbitos atribuíveis ao álcool — São Paulo, 2023
# ============================================================

library(tidyverse)
library(microdatasus)
library(lubridate)
library(geobr)
library(scales)
library(showtext)
library(sf)

message("Preparando os dados para o mapa...")
# ============================================================
# ETAPA 3 — PREPARAÇÃO DOS DADOS
# ============================================================

dados_mapa <- dados_2023 %>%
  mutate(CODMUNOCOR = as.character(CODMUNOCOR)) %>%
  filter(
    substr(CODMUNOCOR, 1, 2) == "35",
    RACACOR %in% c("Branca", "Preta", "Parda", "Amarela", "Indígena")
  )

message("Dados de óbitos associados ao mapa.")

# ============================================================
# ETAPA 4 — MALHA MUNICIPAL DE SÃO PAULO
# ============================================================

message("Carregando a malha dos municípios de São Paulo...")

mapa_sp <- read_municipality(
  code_muni = "SP",
  year = 2020,
  showProgress = FALSE
)

message("Malha municipal carregada.")
# ============================================================
# ETAPA 5 — AGREGAÇÃO DOS ÓBITOS
# ============================================================

obitos_mun <- dados_mapa %>%
  group_by(CODMUNOCOR, RACACOR) %>%
  summarise(
    N_OBITOS = n(),
    .groups = "drop"
  ) %>%
  complete(
    CODMUNOCOR,
    RACACOR,
    fill = list(N_OBITOS = 0)
  )

message("Óbitos agregados por município e raça/cor.")
# ============================================================
# ETAPA 6 — ASSOCIAÇÃO COM A MALHA
# ============================================================

mapa_obitos <- mapa_sp %>%
  mutate(
    code_muni6 = substr(as.character(code_muni), 1, 6)
  ) %>%
  left_join(
    obitos_mun %>%
      mutate(
        CODMUNOCOR = as.character(CODMUNOCOR)
      ),
    by = c("code_muni6" = "CODMUNOCOR")
  )

message("Dados de óbitos associados à malha municipal.")

# ============================================================
# ETAPA 7 — ESCALA DE CORES
# ============================================================

mapa_obitos <- mapa_obitos %>%
  mutate(
    N_OBITOS_ESCALA = if_else(
      N_OBITOS > 0,
      N_OBITOS,
      NA_real_
    )
  )

message("Escala de cores preparada.")
# ============================================================
# ETAPA 8 — GERAÇÃO DO MAPA
# ============================================================

mapa_final <- ggplot(mapa_obitos) +
  geom_sf(
    aes(fill = N_OBITOS_ESCALA),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "rocket",
    direction = -1,
    name = "Nº de Óbitos"
  ) +
  labs(
    title = "Óbitos Atribuíveis ao Álcool por Raça/Cor",
    subtitle = "Municípios do Estado de São Paulo (2023)",
    caption = "Fonte: SIM/SUS (DATASUS)."
  ) +
  facet_wrap(~RACACOR, ncol = 3) +
  theme_void()

message("Mapa gerado.")
# 1. Exibe o mapa na aba Plots do RStudio
print(mapa_final)

# 2. Salva o arquivo PNG em alta resolução na pasta do projeto
caminho_mapa <- file.path(caminho_projeto, "mapa_obitos_alcool_sp_2023.png")
ggsave(filename = caminho_mapa, plot = mapa_final, width = 12, height = 8, dpi = 300, bg = "white")

message("--- Mapa salvo em: ", caminho_mapa, " ---")
