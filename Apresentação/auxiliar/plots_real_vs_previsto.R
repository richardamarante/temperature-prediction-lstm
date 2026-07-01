library(dplyr)
library(gganimate)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)

tema <- list(
  fundo = "#0A1620",
  texto = "#DCE6EA",
  grade = "#304653",
  teal = "#5FA8AE",
  cobre = "#E8935C",
  real = "#DCE6EA"
)

carregar_real_vs_previsto_vencedores <- function(base_resultados, n_horas = 720) {
  melhores <- read_csv(
    file.path(base_resultados, "lstm_melhores_modelos_por_alvo_k.csv"),
    show_col_types = FALSE
  ) |>
    filter(alvo == "Temperatura") |>
    arrange(k_h)

  stopifnot(
    nrow(melhores) == 24L,
    identical(sort(as.integer(melhores$k_h)), 1:24)
  )

  pasta_tabelas <- file.path(base_resultados, "tabelas", "teste_real_vs_previsto")

  bases <- lapply(seq_len(nrow(melhores)), function(i) {
    modelo <- melhores[i, ]
    arquivo <- sprintf(
      "teste_%s_j%03d_k%02d_real_vs_previsto.csv",
      modelo$tipo_modelo,
      modelo$janela_h,
      modelo$k_h
    )
    caminho <- file.path(pasta_tabelas, arquivo)
    stopifnot(file.exists(caminho))

    dados <- read_csv(caminho, show_col_types = FALSE) |>
      arrange(DataPrevista)

    if (nrow(dados) > n_horas) {
      dados <- tail(dados, n_horas)
    }

    dados |>
      mutate(
        k_h = as.integer(modelo$k_h),
        janela_h = as.integer(modelo$janela_h),
        rmse_C = as.numeric(modelo$rmse_C),
        mse_C = as.numeric(modelo$rmse_C)^2,
        r2 = as.numeric(modelo$r2),
        DataPrevista = as.POSIXct(DataPrevista, tz = "UTC")
      )
  })

  bind_rows(bases) |>
    arrange(k_h, DataPrevista)
}

tema_real_vs_previsto <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = tema$fundo, colour = NA),
      panel.background = element_rect(fill = tema$fundo, colour = NA),
      panel.grid.major = element_line(colour = alpha(tema$grade, 0.65), linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_text(
        colour = tema$teal,
        face = "plain",
        size = 18,
        hjust = 0.5,
        margin = margin(b = 12)
      ),
      axis.title = element_text(colour = tema$texto, face = "plain", size = 15),
      axis.text = element_text(colour = tema$texto, face = "plain", size = 12),
      axis.line = element_line(colour = alpha(tema$texto, 0.45)),
      axis.ticks = element_line(colour = alpha(tema$texto, 0.45)),
      strip.background = element_rect(fill = "#16303D", colour = NA),
      strip.text = element_text(colour = tema$texto, face = "plain", size = 12),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(colour = tema$texto, face = "plain", size = 12),
      legend.background = element_rect(fill = tema$fundo, colour = NA),
      legend.key = element_rect(fill = tema$fundo, colour = NA),
      plot.caption = element_text(
        colour = tema$texto,
        face = "plain",
        family = "mono",
        size = 11,
        hjust = 1,
        lineheight = 1.1,
        margin = margin(t = -30, r = 4)
      ),
      plot.margin = margin(12, 16, 8, 12)
    )
}

formatar_decimal <- function(x, casas = 3) {
  sub("\\.", ",", formatC(x, format = "f", digits = casas))
}

rotulo_metricas <- function(rmse, r2) {
  linhas <- c(
    paste0("RMSE = ", formatar_decimal(rmse), " °C"),
    paste0("MSE  = ", formatar_decimal(rmse^2), " °C"),
    paste0("R²   = ", formatar_decimal(r2))
  )
  paste(sprintf("%-18s", linhas), collapse = "\n")
}

preparar_series <- function(base) {
  base |>
    mutate(
      faceta = ifelse(k_h == 1, "k = 1 hora", sprintf("k = %d horas", k_h))
    ) |>
    pivot_longer(
      cols = c(Real, Previsto),
      names_to = "Serie",
      values_to = "Temperatura"
    ) |>
    mutate(Serie = factor(Serie, levels = c("Real", "Previsto")))
}

grafico_painel <- function(base, ks) {
  dados <- preparar_series(filter(base, k_h %in% ks))
  metricas <- dados |>
    distinct(faceta, rmse_C, r2) |>
    mutate(rotulo = mapply(rotulo_metricas, rmse_C, r2))

  ggplot(dados, aes(DataPrevista, Temperatura, colour = Serie)) +
    geom_line(
      data = filter(dados, Serie == "Real"),
      linewidth = 0.55,
      alpha = 0.95
    ) +
    geom_line(
      data = filter(dados, Serie == "Previsto"),
      linewidth = 0.85,
      alpha = 1
    ) +
    geom_text(
      data = metricas,
      aes(x = Inf, y = Inf, label = rotulo),
      inherit.aes = FALSE,
      colour = tema$texto,
      size = 3.6,
      hjust = 1.08,
      vjust = 1.25,
      lineheight = 1.1
    ) +
    facet_wrap(vars(faceta), ncol = if (length(ks) == 4L) 2 else 1) +
    scale_colour_manual(values = c(Real = tema$real, Previsto = tema$cobre)) +
    scale_x_datetime(
      date_breaks = "5 days",
      date_labels = "%d/%m",
      expand = expansion(mult = c(0.005, 0.005))
    ) +
    scale_y_continuous(
      limits = range(c(base$Real, base$Previsto), na.rm = TRUE),
      labels = label_number(decimal.mark = ",", accuracy = 1)
    ) +
    labs(
      x = "Tempo (h)",
      y = "Temperatura (°C)"
    ) +
    tema_real_vs_previsto(if (length(ks) == 4L) 12 else 14)
}

grafico_individual <- function(base, k, mostrar_k = FALSE) {
  dados_k <- filter(base, k_h == k)
  dados <- preparar_series(dados_k)
  rmse <- unique(dados_k$rmse_C)
  r2 <- unique(dados_k$r2)

  ggplot(dados, aes(DataPrevista, Temperatura, colour = Serie)) +
    geom_line(
      data = filter(dados, Serie == "Real"),
      linewidth = 0.7,
      alpha = 0.95
    ) +
    geom_line(
      data = filter(dados, Serie == "Previsto"),
      linewidth = 1.05,
      alpha = 1
    ) +
    scale_colour_manual(values = c(Real = tema$real, Previsto = tema$cobre)) +
    scale_x_datetime(
      date_breaks = "3 days",
      date_labels = "%d/%m",
      expand = expansion(mult = c(0.005, 0.005))
    ) +
    scale_y_continuous(
      limits = range(c(base$Real, base$Previsto), na.rm = TRUE),
      labels = label_number(decimal.mark = ",", accuracy = 1)
    ) +
    labs(
      subtitle = if (mostrar_k) sprintf("k = %d horas", k) else NULL,
      x = "Tempo (h)",
      y = "Temperatura (°C)",
      caption = rotulo_metricas(rmse, r2)
    ) +
    tema_real_vs_previsto()
}

salvar_png <- function(grafico, caminho, width = 12, height = 7) {
  ggsave(
    caminho,
    plot = grafico,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = tema$fundo
  )
}

gerar_gif <- function(base, caminho) {
  pasta_quadros <- tempfile("real_vs_previsto_quadros_")
  dir.create(pasta_quadros)
  on.exit(unlink(pasta_quadros, recursive = TRUE), add = TRUE)

  caminhos <- character(24)
  for (k in 1:24) {
    caminhos[[k]] <- file.path(pasta_quadros, sprintf("quadro_%02d.png", k))
    salvar_png(
      grafico_individual(base, k, mostrar_k = TRUE),
      caminhos[[k]],
      width = 12.8,
      height = 7.2
    )
  }

  quadros <- magick::image_read(caminhos) |>
    magick::image_resize("1280x720!") |>
    magick::image_animate(delay = 80)

  magick::image_write(quadros, path = caminho, format = "gif")
  invisible(caminho)
}

gerar_real_vs_previsto <- function(
    base_resultados = Sys.getenv(
      "LSTM_RESULTADOS_DIR",
      "/tmp/regeraGraficos/resultados_lstm"
    ),
    raiz_saida = "Apresentação/media/real_vs_previsto") {
  base <- carregar_real_vs_previsto_vencedores(base_resultados, n_horas = 720)

  pastas <- c(
    quatro = file.path(raiz_saida, "4_facetas"),
    duas = file.path(raiz_saida, "2_facetas"),
    individuais = file.path(raiz_saida, "individuais")
  )
  invisible(lapply(pastas, dir.create, recursive = TRUE, showWarnings = FALSE))

  grupos_4 <- split(1:24, ceiling((1:24) / 4))
  for (ks in grupos_4) {
    caminho <- file.path(
      pastas[["quatro"]],
      sprintf("real_vs_previsto_k%02d_k%02d.png", min(ks), max(ks))
    )
    salvar_png(grafico_painel(base, ks), caminho, width = 14, height = 9)
  }

  grupos_2 <- split(1:24, ceiling((1:24) / 2))
  for (ks in grupos_2) {
    caminho <- file.path(
      pastas[["duas"]],
      sprintf("real_vs_previsto_k%02d_k%02d.png", min(ks), max(ks))
    )
    salvar_png(grafico_painel(base, ks), caminho)
  }

  for (k in 1:24) {
    caminho <- file.path(
      pastas[["individuais"]],
      sprintf("real_vs_previsto_k%02d.png", k)
    )
    salvar_png(grafico_individual(base, k), caminho)
  }

  gerar_gif(
    base,
    file.path(raiz_saida, "real_vs_previsto_k01_k24.gif")
  )

  invisible(raiz_saida)
}

if (sys.nframe() == 0L) {
  gerar_real_vs_previsto()
}
