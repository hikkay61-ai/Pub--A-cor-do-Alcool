# ==============================================================================
# ARQUIVO: 06_mapa_CAPS_CEP.R
# OBJETIVO: Mapa de CAPS AD com Coordenadas Reais + Busca por CEP (HTML Estático)
# ==============================================================================

caminho_projeto <- path.expand("~/Projeto_Pub_Alcool_RacaCor-main/Felipe Nascimento")
setwd(caminho_projeto)

QTD_CAPS_PROXIMOS <- 3
TIMEOUT_SEGUNDOS  <- 8
URL_API_CEP_1     <- "https://cep.awesomeapi.com.br/json/"
URL_API_CEP_2     <- "https://brasilapi.com.br/api/cep/v2/"
URL_API_GEOCODE   <- "https://nominatim.openstreetmap.org/search"

library(tidyverse)
library(leaflet)
library(scales)
library(readxl)
library(htmlwidgets)

# 1. CARREGAMENTO DA BASE DE ATENDIMENTOS
# ------------------------------------------------------------------------------
if (!exists("planilha_turbinada")) {
  if (exists("planilha_final_caps")) {
    planilha_turbinada <- planilha_final_caps
  } else {
    caminho_planilha <- file.path(caminho_projeto, "atendimentos_CAPS_AD_SP_2024.xlsx")
    if (!file.exists(caminho_planilha)) {
      stop("Arquivo 'atendimentos_CAPS_AD_SP_2024.xlsx' não encontrado em: ", caminho_projeto)
    }
    message("Carregando planilha de atendimentos salva...")
    planilha_turbinada <- read_excel(caminho_planilha)
  }
}

message("1/4: Calculando a porcentagem de Raça/Cor por unidade...")

# 2. CÁLCULO ESTRUTURAL DAS PORCENTAGENS
# ------------------------------------------------------------------------------
perfil_mapa <- planilha_turbinada %>%
  mutate(CODIGO_CAPS = as.character(CODIGO_CAPS)) %>%
  filter(!is.na(NOME_DO_CAPS)) %>%
  count(CODIGO_CAPS, NOME_DO_CAPS, RACA_COR, name = "total_atendimentos") %>%
  group_by(CODIGO_CAPS, NOME_DO_CAPS) %>%
  mutate(
    porcentagem = total_atendimentos / sum(total_atendimentos),
    porc_visual = percent(porcentagem, accuracy = 0.1)
  ) %>%
  ungroup() %>%
  select(CODIGO_CAPS, NOME_DO_CAPS, RACA_COR, porc_visual) %>%
  pivot_wider(
    names_from = RACA_COR,
    values_from = porc_visual,
    values_fill = "0%"
  )

cols_raca <- c("Branca", "Preta", "Parda", "Amarela", "Indígena", "Sem Informação")
for (col in cols_raca) {
  if (!col %in% names(perfil_mapa)) perfil_mapa[[col]] <- "0%"
}

message("2/4: Carregando coordenadas reais das unidades...")

# 3. CARREGAMENTO DE COORDENADAS (EXCEL LOCAL OU EXTRAÇÃO DO CNES)
# ------------------------------------------------------------------------------
caminho_seu_excel <- file.path(caminho_projeto, "coordenadas_caps.xlsx")

if (file.exists(caminho_seu_excel)) {
  message("Lendo coordenadas do arquivo 'coordenadas_caps.xlsx'...")
  coordenadas_reais <- read_excel(caminho_seu_excel) %>%
    mutate(
      CODIGO_CAPS = as.character(CODIGO_CAPS),
      LAT = as.numeric(str_replace(as.character(LAT), ",", ".")),
      LON = as.numeric(str_replace(as.character(LON), ",", "."))
    )
} else {
  message("Buscando coordenadas na tabela CNES...")
  arq_cnes <- list.files(tempdir(), pattern = "tbEstabelecimento", full.names = TRUE, ignore.case = TRUE)[1]
  
  if (is.na(arq_cnes) || !file.exists(arq_cnes)) {
    options(timeout = 1200)
    url_cnes_zip <- "ftp://ftp.datasus.gov.br/cnes/BASE_DE_DADOS_CNES_202401.ZIP"
    arquivo_zip <- tempfile(fileext = ".zip")
    download.file(url_cnes_zip, destfile = arquivo_zip, mode = "wb")
    arquivos_no_zip <- unzip(arquivo_zip, list = TRUE)$Name
    nome_arq <- arquivos_no_zip[grep("tbEstabelecimento", arquivos_no_zip, ignore.case = TRUE)][1]
    unzip(arquivo_zip, files = nome_arq, exdir = tempdir())
    arq_cnes <- file.path(tempdir(), nome_arq)
    unlink(arquivo_zip)
  }
  
  coordenadas_reais <- read_delim(
    arq_cnes,
    delim = ";",
    locale = locale(encoding = "latin1"),
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ) %>%
    transmute(
      CODIGO_CAPS = CO_CNES,
      LAT = suppressWarnings(as.numeric(str_replace(NU_LATITUDE, ",", "."))),
      LON = suppressWarnings(as.numeric(str_replace(NU_LONGITUDE, ",", "."))),
      ENDERECO = paste0(NO_LOGRADOURO, ", ", NU_ENDERECO, " - ", NO_BAIRRO)
    ) %>%
    distinct(CODIGO_CAPS, .keep_all = TRUE)
}

dados_finais_mapa <- perfil_mapa %>%
  left_join(coordenadas_reais, by = "CODIGO_CAPS") %>%
  filter(!is.na(LAT), !is.na(LON))

message("3/4: Renderizando o mapa interativo (", nrow(dados_finais_mapa), " unidades)...")

# 4. DESENHANDO O MAPA INTERATIVO (LEAFLET)
# ------------------------------------------------------------------------------
mapa_caps_real <- leaflet(dados_finais_mapa) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = ~LAT,
    lng = ~LON,
    radius = 8,
    color = "#e74c3c",
    fillOpacity = 0.8,
    popup = ~paste0(
      "<div style='font-family: Arial; font-size: 13px;'>",
      "<b>", NOME_DO_CAPS, "</b><br>",
      "<small>CNES: ", CODIGO_CAPS, "</small><br>",
      "<hr style='margin: 5px 0;'>",
      "<b>Branca:</b> ", Branca, "<br>",
      "<b>Preta:</b> ", Preta, "<br>",
      "<b>Parda:</b> ", Parda, "<br>",
      "<b>Amarela:</b> ", Amarela, "<br>",
      "<b>Indígena:</b> ", Indígena, "<br>",
      "<b>Sem Informação:</b> ", `Sem Informação`,
      "</div>"
    )
  )

# 5. MÓDULO DE BUSCA POR CEP (INJEÇÃO LIMPA SEM QUEBRA DE BUFFER)
# ------------------------------------------------------------------------------
message("4/4: Acoplando busca por CEP e salvando HTML estático...")

col_end <- intersect(c("ENDERECO", "LOGRADOURO", "END"), names(dados_finais_mapa))[1]

dados_caps_js <- dados_finais_mapa %>%
  mutate(
    ENDERECO_EXIBICAO = if (!is.na(col_end)) {
      as.character(.data[[col_end]])
    } else {
      paste0("CNES: ", CODIGO_CAPS)
    }
  ) %>%
  select(CODIGO_CAPS, NOME_DO_CAPS, LAT, LON, ENDERECO_EXIBICAO)

parametros_modulo_cep <- list(
  qtd_proximos       = QTD_CAPS_PROXIMOS,
  timeout_ms         = TIMEOUT_SEGUNDOS * 1000,
  url_api1           = URL_API_CEP_1,
  url_api2           = URL_API_CEP_2,
  url_geocode        = URL_API_GEOCODE,
  qtd_ignorados_base = max(0, nrow(perfil_mapa) - nrow(dados_finais_mapa)),
  caps               = dados_caps_js
)

js_codigo <- '
function(el, x, data) {
  var map = this;
  var camadaBuscaCep = L.layerGroup().addTo(map);
  var style = document.createElement("style");
  style.innerHTML = ".painel-cep-static{position:absolute;top:12px;right:12px;z-index:1000;background:rgba(255,255,255,0.96);padding:14px 16px;border-radius:8px;box-shadow:0 3px 14px rgba(0,0,0,0.22);font-family:Arial,sans-serif;width:340px;max-width:calc(100vw - 24px);max-height:calc(100vh - 24px);overflow-y:auto;box-sizing:border-box;}.painel-cep-static h4{margin:0 0 8px 0;color:#2c3e50;font-size:15px;font-weight:bold;}.linha-input-cep-static{display:flex;gap:6px;align-items:center;}.input-cep-campo{flex:1;padding:7px 10px;border:1px solid #bdc3c7;border-radius:4px;font-size:13px;outline:none;}.btn-buscar-cep-static{background:#e74c3c;color:#fff;border:none;font-weight:bold;padding:8px 12px;border-radius:4px;cursor:pointer;font-size:13px;}.btn-limpar-cep-static{background:#ecf0f1;color:#2c3e50;border:1px solid #bdc3c7;width:100%;margin-top:8px;padding:6px;border-radius:4px;font-size:12px;cursor:pointer;}.card-caps-item{background:#f8f9fa;border-left:4px solid #2c3e50;padding:8px 10px;margin-top:8px;border-radius:4px;font-size:12px;line-height:1.4;}.card-caps-destaque{background:#eafaf1;border-left:4px solid #27ae60;}.msg-erro-cep{background:#fdecea;border-left:4px solid #e74c3c;color:#922b21;padding:8px 10px;margin-top:8px;border-radius:4px;font-size:12px;}.nota-ignorados{color:#7f8c8d;font-size:11px;margin-top:6px;display:block;}";
  document.head.appendChild(style);

  var painel = document.createElement("div");
  painel.className = "painel-cep-static";
  painel.innerHTML = "<h4>Localizar CAPS AD por CEP</h4><div class=\\"linha-input-cep-static\\"><input type=\\"text\\" id=\\"campo_cep_usuario\\" class=\\"input-cep-campo\\" placeholder=\\"Digite seu CEP (ex: 01001-000)\\" maxlength=\\"9\\" autocomplete=\\"off\\" /><button type=\\"button\\" id=\\"btn_acao_cep\\" class=\\"btn-buscar-cep-static\\">Encontrar CAPS</button></div><div id=\\"saida_resultado_cep_static\\"></div>";
  el.style.position = "relative";
  el.appendChild(painel);

  L.DomEvent.disableClickPropagation(painel);
  L.DomEvent.disableScrollPropagation(painel);

  var inputCep = painel.querySelector("#campo_cep_usuario");
  var btnBuscar = painel.querySelector("#btn_acao_cep");
  var divSaida = painel.querySelector("#saida_resultado_cep_static");

  function calcularHaversineKm(lat1, lon1, lat2, lon2) {
    var R = 6371.0, toRad = Math.PI / 180.0;
    var dLat = (lat2 - lat1) * toRad, dLon = (lon2 - lon1) * toRad;
    var a = Math.sin(dLat/2)*Math.sin(dLat/2) + Math.cos(lat1*toRad)*Math.cos(lat2*toRad)*Math.sin(dLon/2)*Math.sin(dLon/2);
    return R * (2 * Math.atan2(Math.sqrt(a), Math.sqrt(Math.max(0, 1 - a))));
  }

  async function fetchComTimeout(url
  js_parte1 <- '
function(el, x, data) {
  var map = this;
  var camadaBuscaCep = L.layerGroup().addTo(map);
  var style = document.createElement("style");
  style.innerHTML = ".painel-cep-static{position:absolute;top:12px;right:12px;z-index:1000;background:rgba(255,255,255,0.96);padding:14px 16px;border-radius:8px;box-shadow:0 3px 14px rgba(0,0,0,0.22);font-family:Arial,sans-serif;width:340px;max-width:calc(100vw - 24px);max-height:calc(100vh - 24px);overflow-y:auto;box-sizing:border-box;}.painel-cep-static h4{margin:0 0 8px 0;color:#2c3e50;font-size:15px;font-weight:bold;}.linha-input-cep-static{display:flex;gap:6px;align-items:center;}.input-cep-campo{flex:1;padding:7px 10px;border:1px solid #bdc3c7;border-radius:4px;font-size:13px;outline:none;}.btn-buscar-cep-static{background:#e74c3c;color:#fff;border:none;font-weight:bold;padding:8px 12px;border-radius:4px;cursor:pointer;font-size:13px;}.btn-limpar-cep-static{background:#ecf0f1;color:#2c3e50;border:1px solid #bdc3c7;width:100%;margin-top:8px;padding:6px;border-radius:4px;font-size:12px;cursor:pointer;}.card-caps-item{background:#f8f9fa;border-left:4px solid #2c3e50;padding:8px 10px;margin-top:8px;border-radius:4px;font-size:12px;line-height:1.4;}.card-caps-destaque{background:#eafaf1;border-left:4px solid #27ae60;}.msg-erro-cep{background:#fdecea;border-left:4px solid #e74c3c;color:#922b21;padding:8px 10px;margin-top:8px;border-radius:4px;font-size:12px;}.nota-ignorados{color:#7f8c8d;font-size:11px;margin-top:6px;display:block;}";
  document.head.appendChild(style);
  
  var painel = document.createElement("div");
  painel.className = "painel-cep-static";
  painel.innerHTML = "<h4>Localizar CAPS AD por CEP</h4><div class=\\"linha-input-cep-static\\"><input type=\\"text\\" id=\\"campo_cep_usuario\\" class=\\"input-cep-campo\\" placeholder=\\"Digite seu CEP (ex: 01001-000)\\" maxlength=\\"9\\" autocomplete=\\"off\\" /><button type=\\"button\\" id=\\"btn_acao_cep\\" class=\\"btn-buscar-cep-static\\">Encontrar CAPS</button></div><div id=\\"saida_resultado_cep_static\\"></div>";
  el.style.position = "relative";
  el.appendChild(painel);
  
  L.DomEvent.disableClickPropagation(painel);
  L.DomEvent.disableScrollPropagation(painel);
  
  var inputCep = painel.querySelector("#campo_cep_usuario");
  var btnBuscar = painel.querySelector("#btn_acao_cep");
  var divSaida = painel.querySelector("#saida_resultado_cep_static");
  
  function calcularHaversineKm(lat1, lon1, lat2, lon2) {
    var R = 6371.0, toRad = Math.PI / 180.0;
    var dLat = (lat2 - lat1) * toRad, dLon = (lon2 - lon1) * toRad;
    var a = Math.sin(dLat/2)*Math.sin(dLat/2) + Math.cos(lat1*toRad)*Math.cos(lat2*toRad)*Math.sin(dLon/2)*Math.sin(dLon/2);
    return R * (2 * Math.atan2(Math.sqrt(a), Math.sqrt(Math.max(0, 1 - a))));
  }
  
  async function fetchComTimeout(url) {
    var controller = new AbortController();
    var timer = setTimeout(function() { controller.abort(); }, data.timeout_ms);
    try {
      var resposta = await fetch(url, { signal: controller.signal });
      clearTimeout(timer);
      return resposta;
    } catch (erro) { clearTimeout(timer); throw erro; }
  }
  '
  