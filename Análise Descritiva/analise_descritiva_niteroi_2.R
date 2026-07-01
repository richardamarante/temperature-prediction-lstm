# Setup ----

# Rode o inicio do analise_descritiva_niteroi

library(tidyverse)
library(patchwork)
library(plotly)
library(lubridate)

# Univariada ----

## Função auxiliar ----

plot_univariado <- function(var, label_y, titulo_base, cor, pasta) {

  # Horário
  p_horario <- ggplot(dados, aes(x = Data, y = .data[[var]])) +
    geom_line(color = cor, linewidth = 0.22, alpha = 0.75) +
    geom_hline(aes(yintercept = -Inf)) +
    scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
    labs(title = paste(titulo_base, "Horária em Niterói"),
         x = "Ano", y = label_y) +
    ggthemes::theme_hc() +
    theme(
      plot.title  = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
      axis.title  = element_text(size = 16),
      axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
      axis.text.y = element_text(size = 13)
    )

  ggsave(file.path("trabalho/Análise Descritiva/resultados", pasta, paste0("00_", tolower(pasta), "_horario.png")),
         plot = p_horario, dpi = 600, width = 12, height = 7, units = "in", bg = "white")

  # Diário
  p_diario <- ggplot(dados_diarios, aes(x = Data, y = .data[[var]])) +
    geom_line(color = cor, linewidth = 1.00) +
    geom_hline(aes(yintercept = -Inf)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
    labs(title = paste(titulo_base, "Média Diária em Niterói"),
         x = "Ano", y = label_y) +
    ggthemes::theme_hc() +
    theme(
      plot.title  = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
      axis.title  = element_text(size = 16),
      axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
      axis.text.y = element_text(size = 13)
    )

  ggsave(file.path("trabalho/Análise Descritiva/resultados", pasta, paste0("01_", tolower(pasta), "_diario.png")),
         plot = p_diario, dpi = 600, width = 12, height = 7, units = "in", bg = "white")

  # Semanal
  p_semanal <- ggplot(dados_semanais, aes(x = Data, y = .data[[var]])) +
    geom_line(color = cor, linewidth = 1.00) +
    geom_hline(aes(yintercept = -Inf)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
    labs(title = paste(titulo_base, "Média Semanal em Niterói"),
         x = "Ano", y = label_y) +
    ggthemes::theme_hc() +
    theme(
      plot.title  = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
      axis.title  = element_text(size = 16),
      axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
      axis.text.y = element_text(size = 13)
    )

  ggsave(file.path("trabalho/Análise Descritiva/resultados", pasta, paste0("02_", tolower(pasta), "_semanal.png")),
         plot = p_semanal, dpi = 600, width = 12, height = 7, units = "in", bg = "white")

  # Mensal
  p_mensal <- ggplot(dados_mensais, aes(x = Data, y = .data[[var]])) +
    geom_line(color = cor, linewidth = 1.00) +
    geom_hline(aes(yintercept = -Inf)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
    labs(title = paste(titulo_base, "Média Mensal em Niterói"),
         x = "Ano", y = label_y) +
    ggthemes::theme_hc() +
    theme(
      plot.title  = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
      axis.title  = element_text(size = 16),
      axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
      axis.text.y = element_text(size = 13)
    )

  ggsave(file.path("trabalho/Análise Descritiva/resultados", pasta, paste0("03_", tolower(pasta), "_mensal.png")),
         plot = p_mensal, dpi = 600, width = 12, height = 7, units = "in", bg = "white")

  message("✔ ", titulo_base, " — 4 gráficos salvos em resultados/", pasta)
}

## Plots ----

plot_univariado(
  var         = "TempSuperficie",
  label_y     = "Temperatura da Superfície (°C)",
  titulo_base = "Temperatura da Superfície",
  cor         = vermelho,
  pasta       = "TempSuperficie"
)

plot_univariado(
  var         = "TempOrvalho",
  label_y     = "Temperatura do Ponto de Orvalho (°C)",
  titulo_base = "Temperatura do Ponto de Orvalho",
  cor         = verde,
  pasta       = "TempOrvalho"
)

plot_univariado(
  var         = "RadiacaoSolarLiquida",
  label_y     = "Radiação Solar Líquida (W/m²)",
  titulo_base = "Radiação Solar Líquida",
  cor         = "#F2C500",
  pasta       = "RadiacaoSolar"
)

plot_univariado(
  var         = "RadiacaoTermicaLiq",
  label_y     = "Radiação Térmica Líquida (W/m²)",
  titulo_base = "Radiação Térmica Líquida",
  cor         = vermelho,
  pasta       = "RadiacaoTermica"
)

plot_univariado(
  var         = "FluxoCalorSensivel",
  label_y     = "Fluxo de Calor Sensível (W/m²)",
  titulo_base = "Fluxo de Calor Sensível",
  cor         = ciano,
  pasta       = "FluxoCalorSensivel"
)

plot_univariado(
  var         = "FluxoCalorLatente",
  label_y     = "Fluxo de Calor Latente (W/m²)",
  titulo_base = "Fluxo de Calor Latente",
  cor         = "#E66100",
  pasta       = "FluxoCalorLatente"
)

plot_univariado(
  var         = "PressaoSuperficial",
  label_y     = "Pressão Superficial (Pa)",
  titulo_base = "Pressão Superficial",
  cor         = cinza,
  pasta       = "Pressao"
)

plot_univariado(
  var         = "VentoU_LesteOeste",
  label_y     = "Componente U do Vento (m/s)",
  titulo_base = "Vento Leste-Oeste",
  cor         = ciano,
  pasta       = "VentoU"
)

plot_univariado(
  var         = "VentoV_NorteSul",
  label_y     = "Componente V do Vento (m/s)",
  titulo_base = "Vento Norte-Sul",
  cor         = roxo,
  pasta       = "VentoV"
)

## histograma + boxplot ----

plot_distribuicao <- function(var, label_x, titulo_base, cor, pasta) {

  p_hist <- ggplot(dados, aes(x = .data[[var]])) +
    geom_histogram(bins = 35, fill = cor, color = "white", linewidth = 0.20) +
    scale_x_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
    scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
    labs(title = "Histograma", x = label_x, y = "Frequência") +
    ggthemes::theme_hc() +
    theme(
      plot.title  = element_text(size = 14, hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title  = element_text(size = 13),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
    )

  p_box <- ggplot(dados, aes(y = .data[[var]])) +
    geom_boxplot(fill = cor, color = "gray20", alpha = 0.80,
                 outlier.size = 0.80, outlier.alpha = 0.40) +
    scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
    labs(title = "Boxplot", x = NULL, y = label_x) +
    ggthemes::theme_hc() +
    theme(
      plot.title  = element_text(size = 14, hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title  = element_text(size = 13),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 11)
    )

  p_combinado <- (p_hist | p_box) +
    patchwork::plot_annotation(
      title = paste("Distribuição —", titulo_base),
      theme = theme(
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold",
                                  margin = margin(b = 12))
      )
    )

  ggsave(
    file.path("trabalho/Análise Descritiva/resultados", pasta,
              paste0("04_", tolower(pasta), "_distribuicao.png")),
    plot   = p_combinado,
    dpi    = 600, width = 12, height = 6, units = "in", bg = "white"
  )

  message("✔ ", titulo_base, " — distribuição salva em resultados/", pasta)
}

### Plots ----

plot_distribuicao("Temperatura",          "Temperatura (°C)",                    "Temperatura do Ar",               laranja,          "Temperatura")
plot_distribuicao("TempSuperficie",       "Temperatura da Superfície (°C)",      "Temperatura da Superfície",       vermelho,         "TempSuperficie")
plot_distribuicao("TempOrvalho",          "Temperatura do Ponto de Orvalho (°C)","Temperatura do Ponto de Orvalho", verde,            "TempOrvalho")
plot_distribuicao("UmidadeRelativa",      "Umidade Relativa (%)",                "Umidade Relativa",                ciano,            "UmidadRel")
plot_distribuicao("PrecipTotal",          "Precipitação",                        "Precipitação",                    azul_principal,   "Precipitacao")
plot_distribuicao("VelocVento",           "Velocidade do Vento (m/s)",           "Velocidade do Vento",             verde_vento,      "VelocVento")
plot_distribuicao("VentoU_LesteOeste",    "Componente U do Vento (m/s)",         "Vento Leste-Oeste",               ciano,            "VentoU")
plot_distribuicao("VentoV_NorteSul",      "Componente V do Vento (m/s)",         "Vento Norte-Sul",                 roxo,             "VentoV")
plot_distribuicao("PressaoSuperficial",   "Pressão Superficial (Pa)",            "Pressão Superficial",             cinza,            "Pressao")
plot_distribuicao("RadiacaoSolarLiquida", "Radiação Solar Líquida (W/m²)",       "Radiação Solar Líquida",          "#F2C500",        "RadiacaoSolar")
plot_distribuicao("RadiacaoTermicaLiq",   "Radiação Térmica Líquida (W/m²)",     "Radiação Térmica Líquida",        vermelho,         "RadiacaoTermica")
plot_distribuicao("FluxoCalorSensivel",   "Fluxo de Calor Sensível (W/m²)",      "Fluxo de Calor Sensível",         ciano,            "FluxoCalorSensivel")
plot_distribuicao("FluxoCalorLatente",    "Fluxo de Calor Latente (W/m²)",       "Fluxo de Calor Latente",          "#E66100",        "FluxoCalorLatente")

# Plotly ----

## Setup ----

library(tidyverse)
library(plotly)
library(lubridate)

dados <- readRDS("trabalho/Análise Descritiva/dados_niteroi.rds")

variaveis_clima <- c("Temperatura", "TempSuperficie", "RadiacaoSolarLiquida",
                     "RadiacaoTermicaLiq", "FluxoCalorSensivel", "FluxoCalorLatente",
                     "TempOrvalho", "PrecipTotal", "VentoU_LesteOeste", "VentoV_NorteSul",
                     "PressaoSuperficial", "VelocVento", "UmidadeRelativa")

nomes_variaveis <- c(
  Temperatura          = "Temperatura do Ar",
  TempSuperficie       = "Temperatura da Superfície",
  RadiacaoSolarLiquida = "Radiação Solar Líquida",
  RadiacaoTermicaLiq   = "Radiação Térmica Líquida",
  FluxoCalorSensivel   = "Fluxo de Calor Sensível",
  FluxoCalorLatente    = "Fluxo de Calor Latente",
  TempOrvalho          = "Temperatura do Ponto de Orvalho",
  PrecipTotal          = "Precipitação",
  VentoU_LesteOeste    = "Vento Leste-Oeste",
  VentoV_NorteSul      = "Vento Norte-Sul",
  PressaoSuperficial   = "Pressão Superficial",
  VelocVento           = "Velocidade do Vento",
  UmidadeRelativa      = "Umidade Relativa"
)

cores_variaveis <- c(
  "Temperatura do Ar"               = "#FF7F0E",
  "Temperatura da Superfície"       = "#D7263D",
  "Temperatura do Ponto de Orvalho" = "#00A676",
  "Umidade Relativa"                = "#00A6D6",
  "Precipitação"                    = "#0072CE",
  "Vento Leste-Oeste"               = "#00A6D6",
  "Vento Norte-Sul"                 = "#7B2CBF",
  "Velocidade do Vento"             = "#009E73",
  "Pressão Superficial"             = "#4D4D4D",
  "Radiação Solar Líquida"          = "#F2C500",
  "Radiação Térmica Líquida"        = "#D7263D",
  "Fluxo de Calor Sensível"         = "#00A6D6",
  "Fluxo de Calor Latente"          = "#E66100"
)

variaveis_media <- setdiff(variaveis_clima, "PrecipTotal")

dados_horarios <- dados %>%
  mutate(Data = as.POSIXct(Data)) %>%
  select(Data, all_of(variaveis_clima))

dados_diarios <- dados %>%
  mutate(Data = as.POSIXct(as_date(Data))) %>%
  group_by(Data) %>%
  summarise(across(all_of(variaveis_media), ~ mean(.x, na.rm = TRUE)),
            PrecipTotal = sum(PrecipTotal, na.rm = TRUE), .groups = "drop")

dados_semanais <- dados %>%
  mutate(Data = as.POSIXct(as_date(floor_date(Data, unit = "week", week_start = 1)))) %>%
  group_by(Data) %>%
  summarise(across(all_of(variaveis_media), ~ mean(.x, na.rm = TRUE)),
            PrecipTotal = sum(PrecipTotal, na.rm = TRUE), .groups = "drop")

dados_mensais <- dados %>%
  mutate(Data = as.POSIXct(as_date(floor_date(Data, unit = "month")))) %>%
  group_by(Data) %>%
  summarise(across(all_of(variaveis_media), ~ mean(.x, na.rm = TRUE)),
            PrecipTotal = sum(PrecipTotal, na.rm = TRUE), .groups = "drop")

# Padronização min-max para permitir sobreposição
padronizar <- function(df) {
  df %>%
    pivot_longer(-Data, names_to = "variavel", values_to = "valor") %>%
    group_by(variavel) %>%
    mutate(valor_pad = (valor - min(valor, na.rm = TRUE)) /
             (max(valor, na.rm = TRUE) - min(valor, na.rm = TRUE)),
           nome = nomes_variaveis[variavel]) %>%
    ungroup()
}

h <- padronizar(dados_horarios)
d <- padronizar(dados_diarios)
s <- padronizar(dados_semanais)
m <- padronizar(dados_mensais)

# Função para construir as traces de uma escala
build_traces <- function(df, visible = FALSE) {
  vars <- unique(df$variavel)
  lapply(vars, function(v) {
    sub <- filter(df, variavel == v)
    nome <- unique(sub$nome)
    list(
      x       = sub$Data,
      y       = sub$valor_pad,
      type    = "scatter",
      mode    = "lines",
      name    = nome,
      visible = visible,
      line    = list(color = cores_variaveis[nome], width = 1.2),
      hovertemplate = paste0("<b>", nome, "</b><br>Data: %{x}<br>",
                             "Valor padronizado: %{y:.3f}<extra></extra>")
    )
  })
}

n <- length(variaveis_clima)

traces_h <- build_traces(h, visible = TRUE)   # horário visível por padrão
traces_d <- build_traces(d, visible = FALSE)
traces_s <- build_traces(s, visible = FALSE)
traces_m <- build_traces(m, visible = FALSE)

todas_traces <- c(traces_h, traces_d, traces_s, traces_m)

# Visibilidade por botão: cada grupo tem n traces
vis_h <- c(rep(TRUE,  n), rep(FALSE, n), rep(FALSE, n), rep(FALSE, n))
vis_d <- c(rep(FALSE, n), rep(TRUE,  n), rep(FALSE, n), rep(FALSE, n))
vis_s <- c(rep(FALSE, n), rep(FALSE, n), rep(TRUE,  n), rep(FALSE, n))
vis_m <- c(rep(FALSE, n), rep(FALSE, n), rep(FALSE, n), rep(TRUE,  n))

botoes <- list(
  list(label  = "Horário",
       method = "update",
       args   = list(list(visible = vis_h),
                     list(title = "Séries Temporais — Escala Horária"))),
  list(label  = "Diário",
       method = "update",
       args   = list(list(visible = vis_d),
                     list(title = "Séries Temporais — Escala Diária"))),
  list(label  = "Semanal",
       method = "update",
       args   = list(list(visible = vis_s),
                     list(title = "Séries Temporais — Escala Semanal"))),
  list(label  = "Mensal",
       method = "update",
       args   = list(list(visible = vis_m),
                     list(title = "Séries Temporais — Escala Mensal")))
)

p <- plot_ly()

for (tr in todas_traces) {
  p <- add_trace(p,
    x              = tr$x,
    y              = tr$y,
    type           = tr$type,
    mode           = tr$mode,
    name           = tr$name,
    visible        = tr$visible,
    line           = tr$line,
    hovertemplate  = tr$hovertemplate
  )
}

p <- layout(p,
  title = list(text = "Séries Temporais — Escala Horária", x = 0.5),
  xaxis = list(title = "Data", rangeslider = list(visible = TRUE)),
  yaxis = list(title = "Valor Padronizado (min-max)", range = c(0, 1)),
  legend = list(orientation = "v", x = 1.02, y = 0.5),
  updatemenus = list(list(
    type       = "buttons",
    direction  = "right",
    x          = 0.0,
    y          = 1.15,
    showactive = TRUE,
    buttons    = botoes
  )),
  hovermode = "x unified",
  paper_bgcolor = "white",
  plot_bgcolor  = "white"
)

# Salvar como HTML
htmlwidgets::saveWidget(p,
  file            = "trabalho/Análise Descritiva/resultados/explorar_series.html",
  selfcontained   = TRUE,
  title           = "Exploração — Séries Temporais Niterói"
)
