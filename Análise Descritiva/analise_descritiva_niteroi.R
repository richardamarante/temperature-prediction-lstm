# Análise descritiva - ERA5-Land Niterói
# Richard Amarante Melo e Caio Vinicius Araujo de Oliveira Salviano

options(scipen = 999) # Tirar notação científica

library(tidyverse)
library(lubridate)
library(scales)

# Carregando dados
dados <- readRDS("trabalho/Análise Descritiva/dados_niteroi.rds")

variaveis_clima <- c("Temperatura", "TempSuperficie", "RadiacaoSolarLiquida", "RadiacaoTermicaLiq", "FluxoCalorSensivel", "FluxoCalorLatente", "TempOrvalho", "PrecipTotal", "VentoU_LesteOeste", "VentoV_NorteSul", "PressaoSuperficial", "VelocVento", "UmidadeRelativa")
variaveis_media_periodo <- c("Temperatura", "TempSuperficie", "RadiacaoSolarLiquida", "RadiacaoTermicaLiq", "FluxoCalorSensivel", "FluxoCalorLatente", "TempOrvalho", "VentoU_LesteOeste", "VentoV_NorteSul", "PressaoSuperficial", "VelocVento", "UmidadeRelativa")

# Cores utilizadas nos gráficos
azul_principal <- "#0072CE"
laranja <- "#FF7F0E"
verde <- "#00A676"
vermelho <- "#D7263D"
roxo <- "#7B2CBF"
verde_vento <- "#009E73"
amarelo <- "#F9A602"
ciano <- "#00A6D6"
cinza <- "#4D4D4D"

# Já predefinindo o nome que vai aparecer no eixo da variável nos gráficos, assim como suas cores
nomes_variaveis <- c(
  Temperatura = "Temperatura do Ar",
  TempSuperficie = "Temperatura da Superfície",
  RadiacaoSolarLiquida = "Radiação Solar Líquida",
  RadiacaoTermicaLiq = "Radiação Térmica Líquida",
  FluxoCalorSensivel = "Fluxo de Calor Sensível",
  FluxoCalorLatente = "Fluxo de Calor Latente",
  TempOrvalho = "Temperatura do Ponto de Orvalho",
  PrecipTotal = "Precipitação",
  VentoU_LesteOeste = "Vento Leste-Oeste",
  VentoV_NorteSul = "Vento Norte-Sul",
  PressaoSuperficial = "Pressão Superficial",
  VelocVento = "Velocidade do Vento",
  UmidadeRelativa = "Umidade Relativa"
)

cores_variaveis <- c(
  "Temperatura do Ar" = laranja,
  "Temperatura da Superfície" = vermelho,
  "Temperatura do Ponto de Orvalho" = verde,
  "Umidade Relativa" = ciano,
  "Precipitação" = azul_principal,
  "Vento Leste-Oeste" = ciano,
  "Vento Norte-Sul" = roxo,
  "Velocidade do Vento" = verde_vento,
  "Pressão Superficial" = cinza,
  "Radiação Solar Líquida" = "#F2C500",
  "Radiação Térmica Líquida" = vermelho,
  "Fluxo de Calor Sensível" = ciano,
  "Fluxo de Calor Latente" = "#E66100"
)

# Corrigindo bug de label que estava tendo em alguns gráficos

label_decimal <- function(x) {
  x <- round(x, 2)
  
  saida <- character(length(x))
  saida[is.na(x)] <- NA_character_

  sem_decimal <- !is.na(x) & x == round(x)
  uma_casa <- !is.na(x) & !sem_decimal & x * 10 == round(x * 10)
  duas_casas <- !is.na(x) & !sem_decimal & !uma_casa

  saida[sem_decimal] <- formatC(x[sem_decimal], format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
  saida[uma_casa] <- formatC(x[uma_casa], format = "f", digits = 1, big.mark = ".", decimal.mark = ",")
  saida[duas_casas] <- formatC(x[duas_casas], format = "f", digits = 2, big.mark = ".", decimal.mark = ",")

  saida
}

# Personalizando as legendas para elas ficarem sempre com a label centralizada em cima dela ou ao lado, dependendo da orientação, porque acho que ficou mais bonito

legenda_vertical <- guide_colorbar(
  title.position = "top",
  title.hjust = 0.5,
  barheight = 5,
  barwidth = 0.8
)

legenda_horizontal <- guide_colorbar(
  title.position = "top",
  title.hjust = 0.5,
  barwidth = 14,
  barheight = 0.8
)

resumo_descritivo <- summary(dados[variaveis_clima])

# Separando a versão da base de dados que é agregada diariamente, semanalmente e mensalmente (sendo que isso tudo acontece pelas médias, com exceção da Precipitação, que somamos para pegar a acumulada)

dados_diarios <- dados %>%
  mutate(Data = as_date(Data)) %>%
  group_by(Data) %>%
  summarise(
    across(all_of(variaveis_media_periodo), ~ mean(.x, na.rm = TRUE)),
    PrecipTotal = sum(PrecipTotal, na.rm = TRUE), .groups = "drop")

dados_semanais <- dados %>%
  mutate(Data = as_date(floor_date(Data, unit = "week", week_start = 1))) %>%
  group_by(Data) %>%
  summarise(
    across(all_of(variaveis_media_periodo), ~ mean(.x, na.rm = TRUE)),
    PrecipTotal = sum(PrecipTotal, na.rm = TRUE), .groups = "drop")

dados_mensais <- dados %>%
  mutate(Data = as_date(floor_date(Data, unit = "month"))) %>%
  group_by(Data) %>%
  summarise(
    across(all_of(variaveis_media_periodo), ~ mean(.x, na.rm = TRUE)),
    PrecipTotal = sum(PrecipTotal, na.rm = TRUE), .groups = "drop")


### - Gráficos - ###

temperatura_horaria <- ggplot(dados, aes(x = Data, y = Temperatura)) +
  geom_line(color = laranja, linewidth = 0.22, alpha = 0.75) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4),
                     labels = label_decimal) +
  labs(
    title = "Temperatura do Ar Horária em Niterói",
    x = "Ano",
    y = "Temperatura (°C)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "00_temperatura_horaria.png"),
       plot = temperatura_horaria, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

temperatura_diaria <- ggplot(dados_diarios, aes(x = Data, y = Temperatura)) +
  geom_line(color = laranja, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Temperatura Média Diária em Niterói",
    x = "Ano",
    y = "Temperatura (°C)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "01_temperatura_media_diaria.png"),
       plot = temperatura_diaria, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

temperatura_semanal <- ggplot(dados_semanais, aes(x = Data, y = Temperatura)) +
  geom_line(color = laranja, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Temperatura Média Semanal em Niterói",
    x = "Ano",
    y = "Temperatura (°C)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "02_temperatura_media_semanal.png"),
       plot = temperatura_semanal, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

temperatura_mensal <- ggplot(dados_mensais, aes(x = Data, y = Temperatura)) +
  geom_line(color = laranja, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Temperatura Média Mensal em Niterói",
    x = "Ano",
    y = "Temperatura (°C)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "03_temperatura_media_mensal.png"),
       plot = temperatura_mensal, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

umidade_horaria <- ggplot(dados, aes(x = Data, y = UmidadeRelativa)) +
  geom_line(color = ciano, linewidth = 0.22, alpha = 0.75) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Umidade Relativa Horária em Niterói",
    x = "Ano",
    y = "Umidade Relativa (%)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "04_umidade_relativa_horaria.png"),
       plot = umidade_horaria, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

umidade_diaria <- ggplot(dados_diarios, aes(x = Data, y = UmidadeRelativa)) +
  geom_line(color = ciano, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Umidade Relativa Média Diária em Niterói",
    x = "Ano",
    y = "Umidade Relativa (%)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "05_umidade_relativa_media_diaria.png"),
       plot = umidade_diaria, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

umidade_semanal <- ggplot(dados_semanais, aes(x = Data, y = UmidadeRelativa)) +
  geom_line(color = ciano, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Umidade Relativa Média Semanal em Niterói",
    x = "Ano",
    y = "Umidade Relativa (%)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "06_umidade_relativa_media_semanal.png"),
       plot = umidade_semanal, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

umidade_mensal <- ggplot(dados_mensais, aes(x = Data, y = UmidadeRelativa)) +
  geom_line(color = ciano, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Umidade Relativa Média Mensal em Niterói",
    x = "Ano",
    y = "Umidade Relativa (%)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "07_umidade_relativa_media_mensal.png"),
       plot = umidade_mensal, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

precipitacao_horaria <- ggplot(dados, aes(x = Data, y = PrecipTotal)) +
  geom_line(color = azul_principal, linewidth = 0.25, alpha = 0.85) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Precipitação Horária em Niterói",
    x = "Ano",
    y = "Precipitação Horária"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "08_precipitacao_horaria.png"),
       plot = precipitacao_horaria, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

precipitacao_diaria <- ggplot(dados_diarios, aes(x = Data, y = PrecipTotal)) +
  geom_line(color = azul_principal, linewidth = 0.80) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Precipitação Acumulada Diária em Niterói",
    x = "Ano",
    y = "Precipitação Total Diária"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "09_precipitacao_total_diaria.png"),
       plot = precipitacao_diaria, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

precipitacao_semanal <- ggplot(dados_semanais, aes(x = Data, y = PrecipTotal)) +
  geom_line(color = azul_principal, linewidth = 0.90) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Precipitação Acumulada Semanal em Niterói",
    x = "Ano",
    y = "Precipitação Total Semanal"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "10_precipitacao_total_semanal.png"),
       plot = precipitacao_semanal, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

precipitacao_mensal <- ggplot(dados_mensais, aes(x = Data, y = PrecipTotal)) +
  geom_line(color = azul_principal, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Precipitação Acumulada Mensal em Niterói",
    x = "Ano",
    y = "Precipitação Total Mensal"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "11_precipitacao_total_mensal.png"),
       plot = precipitacao_mensal, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

vento_horario <- ggplot(dados, aes(x = Data, y = VelocVento)) +
  geom_line(color = verde_vento, linewidth = 0.22, alpha = 0.75) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Velocidade do Vento Horária em Niterói",
    x = "Ano",
    y = "Velocidade do Vento (m/s)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "12_velocidade_vento_horaria.png"),
       plot = vento_horario, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

vento_diario <- ggplot(dados_diarios, aes(x = Data, y = VelocVento)) +
  geom_line(color = verde_vento, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Velocidade Média Diária do Vento em Niterói",
    x = "Ano",
    y = "Velocidade do Vento (m/s)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "13_velocidade_vento_media_diaria.png"),
       plot = vento_diario, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

vento_semanal <- ggplot(dados_semanais, aes(x = Data, y = VelocVento)) +
  geom_line(color = verde_vento, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Velocidade Média Semanal do Vento em Niterói",
    x = "Ano",
    y = "Velocidade do Vento (m/s)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "14_velocidade_vento_media_semanal.png"),
       plot = vento_semanal, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

vento_mensal <- ggplot(dados_mensais, aes(x = Data, y = VelocVento)) +
  geom_line(color = verde_vento, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Velocidade Média Mensal do Vento em Niterói",
    x = "Ano",
    y = "Velocidade do Vento (m/s)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "15_velocidade_vento_media_mensal.png"),
       plot = vento_mensal, dpi = 600, width = 12, height = 7,
       units = "in", bg = "white")

series_temperatura_umidade <- dados_diarios %>%
  select(Data, Temperatura, TempSuperficie, TempOrvalho, UmidadeRelativa) %>%
  pivot_longer(-Data, names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_temperatura_umidade <- ggplot(series_temperatura_umidade, aes(x = Data, y = valor, color = variavel)) +
  geom_line(linewidth = 0.70) +
  geom_hline(aes(yintercept = -Inf)) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  scale_color_manual(values = cores_variaveis, guide = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Temperatura, Superfície e Umidade",
    x = "Ano",
    y = NULL
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 12)
  )

ggsave(file.path("resultados", "16_series_temperatura_umidade.png"),
       plot = grafico_temperatura_umidade, dpi = 600, width = 12,
       height = 8, units = "in", bg = "white")

series_radiacao_fluxos <- dados_diarios %>%
  select(Data, RadiacaoSolarLiquida, RadiacaoTermicaLiq,
         FluxoCalorSensivel, FluxoCalorLatente) %>%
  pivot_longer(-Data, names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_radiacao_fluxos <- ggplot(series_radiacao_fluxos, aes(x = Data, y = valor, color = variavel)) +
  geom_line(linewidth = 0.70) +
  geom_hline(aes(yintercept = -Inf)) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  scale_color_manual(values = cores_variaveis, guide = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Radiação e Fluxos de Calor",
    x = "Ano",
    y = NULL
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 12)
  )

ggsave(file.path("resultados", "17_series_radiacao_fluxos.png"),
       plot = grafico_radiacao_fluxos, dpi = 600, width = 12,
       height = 8, units = "in", bg = "white")

series_chuva_vento_pressao <- dados_diarios %>%
  select(Data, PrecipTotal, VentoU_LesteOeste, VentoV_NorteSul,
         VelocVento, PressaoSuperficial) %>%
  pivot_longer(-Data, names_to = "variavel", values_to = "valor") %>%
  mutate(
    variavel = nomes_variaveis[variavel],
    variavel = factor(variavel, levels = c(
      "Precipitação",
      "Pressão Superficial",
      "Vento Norte-Sul",
      "Vento Leste-Oeste",
      "Velocidade do Vento"
    ))
  )

grafico_chuva_vento_pressao <- ggplot(series_chuva_vento_pressao, aes(x = Data, y = valor, color = variavel)) +
  geom_line(linewidth = 0.70) +
  geom_hline(aes(yintercept = -Inf)) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  scale_color_manual(values = cores_variaveis, guide = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Precipitação, Vento e Pressão",
    x = "Ano",
    y = NULL
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 12)
  )

ggsave(file.path("resultados", "18_series_chuva_vento_pressao.png"),
       plot = grafico_chuva_vento_pressao, dpi = 600, width = 12,
       height = 9, units = "in", bg = "white")

hist_temperatura_umidade <- dados %>%
  select(Temperatura, TempSuperficie, TempOrvalho, UmidadeRelativa) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_hist_temperatura <- ggplot(hist_temperatura_umidade, aes(x = valor, fill = variavel)) +
  geom_histogram(bins = 35, color = "white", linewidth = 0.20) +
  facet_wrap(~ variavel, scales = "free", ncol = 2) +
  scale_fill_manual(values = cores_variaveis, guide = "none") +
  scale_x_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Distribuição de Temperatura e Umidade",
    x = NULL,
    y = "Frequência"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ggsave(file.path("resultados", "19_histogramas_temperatura_umidade.png"),
       plot = grafico_hist_temperatura, dpi = 600, width = 12,
       height = 8, units = "in", bg = "white")

hist_radiacao_fluxos <- dados %>%
  select(RadiacaoSolarLiquida, RadiacaoTermicaLiq,
         FluxoCalorSensivel, FluxoCalorLatente) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_hist_radiacao <- ggplot(hist_radiacao_fluxos, aes(x = valor, fill = variavel)) +
  geom_histogram(bins = 35, color = "white", linewidth = 0.20) +
  facet_wrap(~ variavel, scales = "free", ncol = 2) +
  scale_fill_manual(values = cores_variaveis, guide = "none") +
  scale_x_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Distribuição da Radiação e dos Fluxos de Calor",
    x = NULL,
    y = "Frequência"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ggsave(file.path("resultados", "20_histogramas_radiacao_fluxos.png"),
       plot = grafico_hist_radiacao, dpi = 600, width = 12,
       height = 8, units = "in", bg = "white")

hist_chuva_vento_pressao <- dados %>%
  select(PrecipTotal, VentoU_LesteOeste, VentoV_NorteSul,
         VelocVento, PressaoSuperficial) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_hist_chuva <- ggplot(hist_chuva_vento_pressao, aes(x = valor, fill = variavel)) +
  geom_histogram(bins = 35, color = "white", linewidth = 0.20) +
  facet_wrap(~ variavel, scales = "free", ncol = 2) +
  scale_fill_manual(values = cores_variaveis, guide = "none") +
  scale_x_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Distribuição da Precipitação, do Vento e da Pressão",
    x = NULL,
    y = "Frequência"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ggsave(file.path("resultados", "21_histogramas_chuva_vento_pressao.png"),
       plot = grafico_hist_chuva, dpi = 600, width = 12,
       height = 9, units = "in", bg = "white")

grafico_temp_superficie <- ggplot(dados_diarios, aes(x = TempSuperficie, y = Temperatura)) +
  geom_point(aes(color = RadiacaoSolarLiquida), alpha = 0.60, size = 1.60) +
  scale_color_gradient(low = "#FDD9A0", high = "#D95F02",
                       breaks = breaks_pretty(n = 4),
                       labels = label_decimal,
                       guide = legenda_vertical) +
  scale_x_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  labs(
    title = "Temperatura da Superfície e Radiação Solar",
    x = "Temperatura da Superfície (°C)",
    y = "Temperatura do Ar (°C)",
    color = "Radiação"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    legend.text = element_text(size = 11)
  )

ggsave(file.path("resultados", "22_dispersao_temperatura_superficie_radiacao.png"),
       plot = grafico_temp_superficie, dpi = 600, width = 10,
       height = 7, units = "in", bg = "white")

grafico_temp_orvalho <- ggplot(dados_diarios, aes(x = TempOrvalho, y = Temperatura)) +
  geom_point(aes(color = UmidadeRelativa), alpha = 0.60, size = 1.60) +
  scale_color_gradient(low = "#CFEAFF", high = "#0057B8",
                       breaks = breaks_pretty(n = 4),
                       labels = label_decimal,
                       guide = legenda_vertical) +
  scale_x_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  labs(
    title = "Temperatura do Ar e Umidade Relativa",
    x = "Temperatura do Ponto de Orvalho (°C)",
    y = "Temperatura do Ar (°C)",
    color = "Umidade"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    legend.text = element_text(size = 11)
  )

ggsave(file.path("resultados", "23_dispersao_temperatura_orvalho_umidade.png"),
       plot = grafico_temp_orvalho, dpi = 600, width = 10,
       height = 7, units = "in", bg = "white")

grafico_radiacao_superficie <- ggplot(dados_diarios,
                                      aes(x = RadiacaoSolarLiquida, y = TempSuperficie)) +
  geom_point(aes(color = FluxoCalorSensivel), alpha = 0.60, size = 1.60) +
  scale_color_viridis_c(option = "C", end = 0.90,
                        breaks = breaks_pretty(n = 4),
                        labels = label_decimal,
                        guide = legenda_vertical) +
  scale_x_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  labs(
    title = "Radiação Solar e Temperatura da Superfície",
    x = "Radiação Solar Líquida Média Diária",
    y = "Temperatura da Superfície (°C)",
    color = "Calor sensível"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    legend.text = element_text(size = 11)
  )

ggsave(file.path("resultados", "24_dispersao_radiacao_superficie_calor.png"),
       plot = grafico_radiacao_superficie, dpi = 600, width = 10,
       height = 7, units = "in", bg = "white")

grafico_umidade_chuva <- ggplot(dados_diarios,
                                aes(x = UmidadeRelativa, y = PrecipTotal)) +
  geom_point(aes(color = Temperatura), alpha = 0.60, size = 1.60) +
  scale_color_gradient(low = "#FFE0B2", high = laranja,
                       breaks = breaks_pretty(n = 4),
                       labels = label_decimal,
                       guide = legenda_vertical) +
  scale_x_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  labs(
    title = "Precipitação Diária e Umidade Relativa",
    x = "Umidade Relativa Média Diária (%)",
    y = "Precipitação Total Diária",
    color = "Temperatura"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    legend.text = element_text(size = 11)
  )

ggsave(file.path("resultados", "25_dispersao_umidade_chuva_temperatura.png"),
       plot = grafico_umidade_chuva, dpi = 600, width = 10,
       height = 7, units = "in", bg = "white")

grafico_vento_temperatura <- ggplot(dados_diarios,
                                    aes(x = VentoU_LesteOeste, y = VentoV_NorteSul)) +
  geom_point(aes(color = Temperatura), alpha = 0.55, size = 1.50) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.35) +
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.35) +
  scale_color_gradient(low = "#FFE0B2", high = laranja,
                       breaks = breaks_pretty(n = 4),
                       labels = label_decimal,
                       guide = legenda_vertical) +
  scale_x_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  labs(
    title = "Componentes do Vento e Temperatura",
    x = "Vento Leste-Oeste Médio Diário (m/s)",
    y = "Vento Norte-Sul Médio Diário (m/s)",
    color = "Temperatura"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    legend.text = element_text(size = 11)
  )

ggsave(file.path("resultados", "26_dispersao_vento_temperatura.png"),
       plot = grafico_vento_temperatura, dpi = 600, width = 10,
       height = 7, units = "in", bg = "white")

matriz_cor <- dados %>%
  select(all_of(variaveis_clima)) %>%
  cor(use = "pairwise.complete.obs")

matriz_cor_longa <- as.data.frame(as.table(matriz_cor)) %>%
  as_tibble() %>%
  rename(variavel_1 = Var1, variavel_2 = Var2, correlacao = Freq) %>%
  mutate(
    variavel_1 = nomes_variaveis[as.character(variavel_1)],
    variavel_2 = nomes_variaveis[as.character(variavel_2)]
  )

label_correlacao <- number_format(accuracy = 0.01, decimal.mark = ",", big.mark = ".", trim = TRUE)

grafico_correlacao <- ggplot(matriz_cor_longa, aes(x = variavel_1, y = variavel_2, fill = correlacao)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = label_correlacao(correlacao)),
            size = 3.0, color = "black") +
  scale_fill_gradient2(low = "#2C7BB6", mid = "#F7F7F7", high = "#D7191C",
                       midpoint = 0, limits = c(-1, 1),
                       breaks = seq(-1, 1, 0.5),
                       labels = label_decimal,
                       guide = legenda_horizontal) +
  labs(
    title = "Correlação Entre as Variáveis Meteorológicas",
    x = NULL,
    y = NULL,
    fill = "Correlação"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.title.align = 0.5,
    legend.text = element_text(size = 11),
    plot.margin = margin(10, 18, 10, 10)
  ) +
  coord_fixed()

ggsave(file.path("resultados", "27_matriz_correlacao.png"),
       plot = grafico_correlacao, dpi = 600, width = 14,
       height = 11, units = "in", bg = "white")

acf_temperatura <- acf(dados$Temperatura, lag.max = 24 * 14,
                       plot = FALSE, na.action = na.pass)

autocorrelacao <- tibble(
  defasagem_horas = as.numeric(acf_temperatura$lag),
  correlacao = as.numeric(acf_temperatura$acf)
)

grafico_autocorrelacao <- ggplot(autocorrelacao, aes(x = defasagem_horas, y = correlacao)) +
  geom_col(fill = azul_principal, width = 0.80) +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_vline(xintercept = c(24, 48, 72, 96), linetype = "dashed",
             color = "gray25", linewidth = 0.60) +
  scale_x_continuous(breaks = seq(0, 24 * 14, by = 24),
                     labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = breaks_pretty(n = 5),
                     labels = label_decimal) +
  labs(
    title = "Autocorrelação da Temperatura Horária",
    x = "Defasagem (horas)",
    y = "Autocorrelação"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "28_autocorrelacao_temperatura.png"),
       plot = grafico_autocorrelacao, dpi = 600, width = 12,
       height = 7, units = "in", bg = "white")

temperatura_treino_teste <- dados_diarios %>%
  arrange(Data) %>%
  mutate(
    indice_temporal = row_number(),
    proporcao_temporal = indice_temporal / n(),
    base = case_when(
      proporcao_temporal <= 0.70 ~ "Treino",
      proporcao_temporal <= 0.85 ~ "Validação",
      TRUE ~ "Teste"
    )
  )

y_posicao_split <- max(temperatura_treino_teste$Temperatura, na.rm = TRUE)
inicio_base <- min(temperatura_treino_teste$Data)
fim_base <- max(temperatura_treino_teste$Data)
limite_validacao <- min(temperatura_treino_teste$Data[temperatura_treino_teste$base == "Validação"])
limite_teste <- min(temperatura_treino_teste$Data[temperatura_treino_teste$base == "Teste"])
centros_split <- temperatura_treino_teste %>%
  group_by(base) %>%
  summarise(Data = min(Data) + (max(Data) - min(Data)) / 2,
            .groups = "drop")

grafico_treino_teste <- ggplot(temperatura_treino_teste, aes(x = Data, y = Temperatura)) +
  geom_line(color = laranja, linewidth = 1.00) +
  geom_hline(aes(yintercept = -Inf)) +
  geom_vline(xintercept = c(inicio_base, limite_validacao, limite_teste, fim_base),
             linetype = "dashed",
             color = "gray25", linewidth = 0.70) +
  geom_text(data = centros_split, aes(x = Data, y = y_posicao_split, label = base),
            inherit.aes = FALSE, size = 4.2, fontface = "bold") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 4), labels = label_decimal) +
  labs(
    title = "Divisão Cronológica da Base de Modelagem",
    subtitle = "70% Treino, 15% Validação e 15% Teste",
    x = "Ano",
    y = "Temperatura Média Diária (°C)"
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13)
  )

ggsave(file.path("resultados", "29_treino_teste_temperatura.png"),
       plot = grafico_treino_teste, dpi = 600, width = 12,
       height = 7, units = "in", bg = "white")

temperaturas_sobrepostas <- dados_diarios %>%
  select(Data, Temperatura, TempSuperficie, TempOrvalho) %>%
  pivot_longer(-Data, names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_temperaturas_sobrepostas <- ggplot(temperaturas_sobrepostas,
                                           aes(x = Data, y = valor, color = variavel)) +
  geom_line(linewidth = 0.85, alpha = 0.90) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_color_manual(values = c(
    "Temperatura do Ar" = laranja,
    "Temperatura da Superfície" = vermelho,
    "Temperatura do Ponto de Orvalho" = verde
  )) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  labs(
    title = "Comparação Entre as Medidas de Temperatura",
    x = "Ano",
    y = "Temperatura Média Diária (°C)",
    color = NULL
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

ggsave(file.path("resultados", "30_series_sobrepostas_temperaturas.png"),
       plot = grafico_temperaturas_sobrepostas, dpi = 600, width = 12,
       height = 7, units = "in", bg = "white")

componentes_vento <- dados_diarios %>%
  select(Data, VentoU_LesteOeste, VentoV_NorteSul) %>%
  pivot_longer(-Data, names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_componentes_vento <- ggplot(componentes_vento,
                                    aes(x = Data, y = valor, color = variavel)) +
  geom_line(linewidth = 0.80, alpha = 0.90) +
  geom_hline(aes(yintercept = -Inf)) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.40) +
  scale_color_manual(values = c(
    "Vento Leste-Oeste" = ciano,
    "Vento Norte-Sul" = roxo
  )) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  labs(
    title = "Componentes Médias do Vento",
    x = "Ano",
    y = "Componente Média Diária do Vento (m/s)",
    color = NULL
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

ggsave(file.path("resultados", "31_series_sobrepostas_componentes_vento.png"),
       plot = grafico_componentes_vento, dpi = 600, width = 12,
       height = 7, units = "in", bg = "white")

series_padronizadas_temperatura_umidade_vento <- dados_diarios %>%
  select(Data, Temperatura, UmidadeRelativa, VelocVento) %>%
  pivot_longer(-Data, names_to = "variavel", values_to = "valor") %>%
  group_by(variavel) %>%
  mutate(valor_minmax = (valor - min(valor, na.rm = TRUE)) /
           (max(valor, na.rm = TRUE) - min(valor, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_padronizado_temperatura_umidade_vento <- ggplot(series_padronizadas_temperatura_umidade_vento,
                                                       aes(x = Data, y = valor_minmax, color = variavel)) +
  geom_line(linewidth = 0.45, alpha = 0.75) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_color_manual(values = c(
    "Temperatura do Ar" = laranja,
    "Umidade Relativa" = ciano,
    "Velocidade do Vento" = verde_vento
  )) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 5), limits = c(0, 1),
                     labels = label_decimal) +
  labs(
    title = "Temperatura, Umidade e Vento em Escala Comparável",
    x = "Ano",
    y = "Valor Padronizado (min-max)",
    color = NULL
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

ggsave(file.path("resultados", "32_series_padronizadas_temperatura_umidade_vento.png"),
       plot = grafico_padronizado_temperatura_umidade_vento, dpi = 600, width = 12,
       height = 7.5, units = "in", bg = "white")

series_joule_sobrepostas <- dados_diarios %>%
  select(Data, RadiacaoSolarLiquida, RadiacaoTermicaLiq,
         FluxoCalorSensivel, FluxoCalorLatente) %>%
  pivot_longer(-Data, names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = nomes_variaveis[variavel])

grafico_joule_sobrepostas <- ggplot(series_joule_sobrepostas,
                                   aes(x = Data, y = valor, color = variavel)) +
  geom_line(linewidth = 0.85, alpha = 0.90) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.40) +
  geom_hline(aes(yintercept = -Inf)) +
  scale_color_manual(values = c(
    "Radiação Solar Líquida" = "#F2C500",
    "Radiação Térmica Líquida" = vermelho,
    "Fluxo de Calor Sensível" = ciano,
    "Fluxo de Calor Latente" = "#E66100"
  )) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = breaks_pretty(n = 5), labels = label_decimal) +
  labs(
    title = "Radiação e Fluxos de Calor em Escala Diária",
    x = "Ano",
    y = "Média Diária",
    color = NULL
  ) +
  ggthemes::theme_hc() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

ggsave(file.path("resultados", "33_series_sobrepostas_radiacao_fluxos.png"),
       plot = grafico_joule_sobrepostas, dpi = 600, width = 12,
       height = 7.5, units = "in", bg = "white")
