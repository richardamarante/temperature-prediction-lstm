library(dplyr)
library(ggplot2)
library(readr)
library(scales)

tema <- list(
  fundo = "#0A1620",
  texto = "#DCE6EA",
  grade = "#304653",
  teal = "#5FA8AE",
  cobre = "#E8935C"
)

cores_janelas <- c(
  `24` = "#E8935C",
  `48` = "#00A676",
  `72` = "#2A9DF4",
  `96` = "#9B5DE5"
)

selecionar_melhores_modelos <- function(resultados) {
  resultados |>
    filter(alvo == "Temperatura") |>
    group_by(k_h) |>
    slice_min(rmse_C, n = 1, with_ties = FALSE) |>
    ungroup() |>
    arrange(k_h)
}

tema_apresentacao <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = tema$fundo, colour = NA),
      panel.background = element_rect(fill = tema$fundo, colour = NA),
      panel.grid.major = element_line(colour = alpha(tema$grade, 0.65), linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.title = element_blank(),
      axis.title = element_text(colour = tema$texto, face = "plain", size = 15),
      axis.text = element_text(colour = tema$texto, face = "plain", size = 12),
      axis.line = element_line(colour = alpha(tema$texto, 0.45)),
      axis.ticks = element_line(colour = alpha(tema$texto, 0.45)),
      legend.position = "bottom",
      legend.title = element_text(colour = tema$texto, face = "plain", size = 12),
      legend.text = element_text(colour = tema$texto, face = "plain", size = 12),
      legend.background = element_rect(fill = tema$fundo, colour = NA),
      legend.key = element_rect(fill = tema$fundo, colour = NA),
      plot.margin = margin(12, 18, 10, 12)
    )
}

rotulos_hora <- function(datas) {
  format(as.POSIXct(datas, tz = "America/Sao_Paulo"), "%Hh", tz = "America/Sao_Paulo")
}

grafico_metrica <- function(resultados, metrica = c("rmse_C", "r2")) {
  metrica <- match.arg(metrica)
  eixo_y <- if (metrica == "rmse_C") "RMSE (°C)" else "R²"
  base <- resultados |>
    filter(alvo == "Temperatura") |>
    mutate(janela = factor(janela_h, levels = c(24, 48, 72, 96)))

  ggplot(base, aes(x = k_h, y = .data[[metrica]], colour = janela, group = janela)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.4) +
    scale_colour_manual(values = cores_janelas, name = "Janela (h)") +
    scale_x_continuous(breaks = 1:24) +
    scale_y_continuous(labels = label_number(decimal.mark = ",", accuracy = 0.01)) +
    labs(
      x = "Horizonte k (horas à frente)",
      y = eixo_y
    ) +
    tema_apresentacao()
}

grafico_previsoes_janelas <- function(previsoes) {
  base <- previsoes |>
    filter(alvo == "Temperatura") |>
    mutate(
      Data = as.POSIXct(Data, tz = "UTC"),
      janela = factor(janela_h, levels = c(24, 48, 72, 96))
    )

  ggplot(base, aes(Data, TemperaturaPrevista, colour = janela, group = janela)) +
    geom_line(linewidth = 0.95) +
    geom_point(size = 2.3) +
    scale_colour_manual(values = cores_janelas, name = "Janela (h)") +
    scale_x_datetime(
      breaks = sort(unique(base$Data)),
      labels = rotulos_hora,
      expand = expansion(mult = c(0.015, 0.015))
    ) +
    scale_y_continuous(labels = label_number(decimal.mark = ",", accuracy = 0.1)) +
    labs(
      x = "Horário previsto",
      y = "Temperatura prevista (°C)"
    ) +
    tema_apresentacao()
}

grafico_previsao_vencedores <- function(previsoes, melhores) {
  chaves <- melhores |>
    select(alvo, tipo_modelo, janela_h, k_h)

  base <- previsoes |>
    inner_join(chaves, by = c("alvo", "tipo_modelo", "janela_h", "k_h")) |>
    mutate(Data = as.POSIXct(Data, tz = "UTC")) |>
    arrange(k_h)

  ggplot(base, aes(Data, TemperaturaPrevista)) +
    geom_line(colour = tema$cobre, linewidth = 1.05) +
    geom_point(colour = tema$cobre, size = 2.7) +
    scale_x_datetime(
      breaks = base$Data,
      labels = rotulos_hora,
      expand = expansion(mult = c(0.015, 0.015))
    ) +
    scale_y_continuous(labels = label_number(decimal.mark = ",", accuracy = 0.1)) +
    labs(
      x = "Horário previsto",
      y = "Temperatura prevista (°C)"
    ) +
    tema_apresentacao()
}

salvar_grafico <- function(grafico, caminho) {
  ggsave(
    caminho,
    plot = grafico,
    width = 12,
    height = 7,
    units = "in",
    dpi = 600,
    bg = tema$fundo
  )
}

gerar_graficos_resultados <- function(
    base_resultados = Sys.getenv(
      "LSTM_RESULTADOS_DIR",
      "/tmp/regeraGraficos/resultados_lstm"
    ),
    pasta_saida = "Apresentação/media") {
  resultados <- read_csv(
    file.path(base_resultados, "lstm_resultados_gerais.csv"),
    show_col_types = FALSE
  )
  previsoes <- read_csv(
    file.path(base_resultados, "lstm_previsoes_futuras.csv"),
    show_col_types = FALSE
  )
  melhores <- selecionar_melhores_modelos(resultados)

  stopifnot(
    nrow(melhores) == 24L,
    identical(sort(as.integer(melhores$k_h)), 1:24)
  )

  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

  saidas <- c(
    rmse = file.path(pasta_saida, "lstm_rmse_por_k_e_janela.png"),
    r2 = file.path(pasta_saida, "lstm_r2_por_k_e_janela.png"),
    janelas = file.path(pasta_saida, "lstm_previsoes_por_janela.png"),
    vencedores = file.path(pasta_saida, "lstm_previsao_modelos_vencedores.png")
  )

  salvar_grafico(grafico_metrica(resultados, "rmse_C"), saidas[["rmse"]])
  salvar_grafico(grafico_metrica(resultados, "r2"), saidas[["r2"]])
  salvar_grafico(grafico_previsoes_janelas(previsoes), saidas[["janelas"]])
  salvar_grafico(grafico_previsao_vencedores(previsoes, melhores), saidas[["vencedores"]])

  invisible(saidas)
}

if (sys.nframe() == 0L) {
  gerar_graficos_resultados()
}
