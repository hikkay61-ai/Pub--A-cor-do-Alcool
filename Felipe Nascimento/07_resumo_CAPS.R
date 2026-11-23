# ==============================================================================
# ARQUIVO: 06_resumo_CAPS.R
# OBJETIVO: Ranking geral e composição racial nos 10 maiores CAPS (SP - 2024)
# ==============================================================================

caminho_projeto <- path.expand("~/Projeto_Pub_Alcool_RacaCor-main/Felipe Nascimento")
setwd(caminho_projeto)

library(tidyverse)
library(readxl)
library(writexl)
library(scales)
library(showtext)

# 1. LEITURA DA PLANILHA GERADA NO SCRIPT 05
# ------------------------------------------------------------------------------
message("1/4: Lendo planilha atendimentos_CAPS_AD_SP_2024.xlsx...")

dados_caps <- read_excel(file.path(caminho_projeto, "atendimentos_CAPS_AD_SP_2024.xlsx")) %>%
  mutate(
    NOME_DO_CAPS = if_else(
      is.na(NOME_DO_CAPS) | NOME_DO_CAPS == "",
      paste0("Unidade CNES ", CODIGO_CAPS),
      NOME_DO_CAPS
    )
  )

# 2. TABELA 1: RANKING GERAL DE ATENDIMENTOS POR UNIDADE
# ------------------------------------------------------------------------------
message("2/4: Gerando ranking geral por unidade CAPS...")

ranking_geral <- dados_caps %>%
  group_by(CODIGO_CAPS, NOME_DO_CAPS) %>%
  summarise(
    TOTAL_ATENDIMENTOS = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(TOTAL_ATENDIMENTOS)) %>%
  mutate(
    POSICAO = row_number(),
    PERC_DO_TOTAL_SP = round((TOTAL_ATENDIMENTOS / sum(TOTAL_ATENDIMENTOS)) * 100, 2)
  ) %>%
  relocate(POSICAO, .before = CODIGO_CAPS)

# Exibe o Top 10 direto no console do RStudio
print(head(ranking_geral, 10))

# 3. TABELA 2: DISTRIBUIÇÃO POR RAÇA/COR EM CADA CAPS (FORMATO LARGO)
# ------------------------------------------------------------------------------
message("3/4: Calculando distribuição percentual por Raça/Cor em cada CAPS...")

distribuicao_raca_caps <- dados_caps %>%
  filter(RACA_COR %in% c("Branca", "Preta", "Parda", "Amarela", "Indígena")) %>%
  group_by(CODIGO_CAPS, NOME_DO_CAPS, RACA_COR) %>%
  summarise(QTD = n(), .groups = "drop") %>%
  group_by(CODIGO_CAPS, NOME_DO_CAPS) %>%
  mutate(
    TOTAL_VALIDO = sum(QTD),
    PERC = round((QTD / TOTAL_VALIDO) * 100, 1)
  ) %>%
  ungroup() %>%
  select(-QTD) %>%
  pivot_wider(
    names_from = RACA_COR,
    values_from = PERC,
    values_fill = 0
  ) %>%
  arrange(desc(TOTAL_VALIDO))

caminho_saida <- file.path(caminho_projeto, "ranking_e_perfil_CAPS_SP_2024.xlsx")
write_xlsx(
  list(
    "Ranking_Geral_CAPS" = ranking_geral,
    "Perfil_Raca_Por_CAPS" = distribuicao_raca_caps
  ),
  path = caminho_saida
)

# 4. GRÁFICO: TOP 10 UNIDADES POR RAÇA/COR (ENQUADRAMENTO AJUSTADO)
# ------------------------------------------------------------------------------
message("4/4: Gerando gráfico dos 10 primeiros CAPS...")

font_add_google("Ubuntu", "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)

top10_nomes <- ranking_geral %>%
  slice_head(n = 10) %>%
  pull(NOME_DO_CAPS)

dados_grafico_top10 <- dados_caps %>%
  filter(
    NOME_DO_CAPS %in% top10_nomes,
    RACA_COR %in% c("Branca", "Preta", "Parda", "Amarela", "Indígena")
  ) %>%
  group_by(NOME_DO_CAPS, RACA_COR) %>%
  summarise(QTD = n(), .groups = "drop") %>%
  group_by(NOME_DO_CAPS) %>%
  mutate(
    TOTAL = sum(QTD),
    PERC = QTD / TOTAL
  ) %>%
  ungroup()

grafico_top10 <- ggplot(dados_grafico_top10, aes(x = reorder(NOME_DO_CAPS, TOTAL), y = PERC, fill = RACA_COR)) +
  geom_col(color = "white", linewidth = 0.6, width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Amarela" = "#f1c40f",
    "Branca" = "#2c3e50",
    "Indígena" = "#27ae60",
    "Parda" = "#e67e22",
    "Preta" = "#e74c3c"
  )) +
  scale_y_continuous(labels = percent_format(), expand = c(0.01, 0.01)) +
  labs(
    title = "Composição Racial nos 10 CAPS com Maior Volume de Atendimentos",
    subtitle = "Município de São Paulo (2024) - Diagnósticos F10 a F19",
    caption = "Fonte: Ministério da Saúde - SIA/SUS e CNES.",
    x = NULL,
    y = "Proporção de Atendimentos (%)",
    fill = "Raça/Cor"
  ) +
  theme_minimal(base_family = "ubuntu") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    plot.title = element_text(size = 19, face = "bold", color = "black", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13.5, color = "grey30", margin = margin(b = 18)),
    plot.caption = element_text(size = 11, color = "black", hjust = 1, margin = margin(t = 12)),
    
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "grey25"),
    axis.title.x = element_text(size = 13.5, color = "black", margin = margin(t = 8)),
    
    legend.position = "bottom",
    legend.title = element_text(size = 13, color = "black", margin = margin(r = 10)),
    legend.text = element_text(size = 11.5, color = "black"),
    legend.key.size = unit(0.65, "cm"),
    legend.margin = margin(t = 12),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(18, 25, 15, 18)
  )

print(grafico_top10)

caminho_grafico_top10 <- file.path(caminho_projeto, "grafico_top10_CAPS_raca_sp_2024.png")
ggsave(filename = caminho_grafico_top10, plot = grafico_top10, width = 12, height = 5.4, dpi = 300, bg = "white")

top4 <- ranking_geral %>%
  slice_head(n = 4) %>%
  mutate(
    DIF_PARA_O_1_LUGAR = first(TOTAL_ATENDIMENTOS) - TOTAL_ATENDIMENTOS,
    DIF_PARA_O_ANTERIOR = lag(TOTAL_ATENDIMENTOS) - TOTAL_ATENDIMENTOS
  ) %>%
  select(POSICAO, NOME_DO_CAPS, TOTAL_ATENDIMENTOS, DIF_PARA_O_ANTERIOR, DIF_PARA_O_1_LUGAR)

print(top4)
