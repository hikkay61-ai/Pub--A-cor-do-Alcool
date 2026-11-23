nome_arquivo_sih <- "dados_sp_pandemia_2020-2023.xlsx"

caminho_arquivo_sih <- file.path(
  caminho_projeto,
  nome_arquivo_sih
)

if (file.exists(caminho_arquivo_sih)) {
  
  message("Arquivo SIH já processado encontrado.")
  message("Carregando dados de: ", caminho_arquivo_sih)
  
  dados_sp_analise <- readxl::read_excel(
    caminho_arquivo_sih
  )
  
  message("--- SUCESSO! DADOS SIH CARREGADOS ---")
  
} else {
  
# ============================================================
# 01_SIH_internacoes.R
# Internações hospitalares relacionadas ao álcool
# São Paulo | 2020-2023
#
# Depende de:
#   00_config.R
# ============================================================


# --- ETAPA 1: COLETA DOS DADOS SIH-RD ---

message("Iniciando download dos dados de SP (2020-2023)...")

dados_sp <- fetch_datasus(
  information_system = "SIH-RD",
  uf = "SP",
  year_start = 2020,
  year_end = 2023,
  month_start = 1,
  month_end = 12,
  vars = c(
    "NASC",
    "DT_INTER",
    "DIAS_PERM",
    "DIAG_PRINC",
    "MORTE",
    "CID_MORTE",
    "CAR_INT",
    "MUNIC_RES",
    "RACA_COR",
    "SEXO"
  )
)


# --- ETAPA 2: FILTRAGEM DOS CASOS ---

codigos_faa <- c(
  "E244",
  "F100", "F101", "F102", "F103", "F104",
  "F105", "F106", "F107", "F108", "F109",
  "G312",
  "G621",
  "G721",
  "I426",
  "K292",
  "K700", "K701", "K702", "K703", "K704", "K709",
  "K852",
  "K860",
  "O354",
  "P043",
  "Q860",
  "R780",
  "T510", "T511", "T512", "T513", "T518", "T519",
  "X45",
  "X65",
  "Y15",
  "Y90",
  "Y910", "Y911", "Y912", "Y913"
)

message("Filtrando internações relacionadas ao álcool...")

dados_sp_faa <- dados_sp %>%
  filter(DIAG_PRINC %in% codigos_faa)


# --- ETAPA 3: PROCESSAMENTO DOS DADOS ---

message("Processando e limpando os dados...")

dados_sp_processados <- process_sih(dados_sp_faa)


# --- ETAPA 4: PREPARAÇÃO DAS VARIÁVEIS PARA ANÁLISE ---

message("Preparando dados para análise...")

dados_sp_analise <- dados_sp_processados %>%
  mutate(
    DT_INTER = ymd(DT_INTER),
    
    ANO_INTER = year(DT_INTER),
    
    PERIODO = case_when(
      ANO_INTER %in% c(2020, 2021) ~ "Pandemia (2020-2021)",
      ANO_INTER %in% c(2022, 2023) ~ "Pós-Pandemia (2022-2023)"
    ),
    
    DIAS_PERM2 = as.numeric(DIAS_PERM),
    
    MORTE_NUMERICO = case_when(
      MORTE == "Não" ~ "0",
      MORTE == "Sim" ~ "1"
    )
  ) %>%
  filter(
    !is.na(PERIODO),
    !is.na(DIAS_PERM2),
    !is.na(MORTE_NUMERICO)
  )


# --- ETAPA 5: SALVAMENTO DOS DADOS ---

nome_arquivo_sih <- "dados_sp_pandemia_2020-2023.xlsx"

caminho_arquivo_sih <- file.path(
  caminho_projeto,
  nome_arquivo_sih
)

write_xlsx(
  dados_sp_analise,
  caminho_arquivo_sih
)

message(
  "Arquivo SIH salvo em: ",
  caminho_arquivo_sih
)
}
