library(tidyverse)
library(scales)
library(ggthemes)

dados <- readRDS("Apresentação/data/dados_niteroi.rds")

# Paleta da apresentação
azul_principal <- "#0072CE"
teal <- "#5FA8AE"
cobre <- "#E8935C"
texto <- "#DCE6EA"
fundo <- "#0A1620"

label_decimal <- function(x) {

  x <- round(x, 2)

  saida <- character(length(x))
  saida[is.na(x)] <- NA_character_

  sem_decimal <- !is.na(x) & x == round(x)
  uma_casa <- !is.na(x) & !sem_decimal & x * 10 == round(x * 10)
  duas_casas <- !is.na(x) & !sem_decimal & !uma_casa

  saida[sem_decimal] <-
    formatC(
      x[sem_decimal],
      format = "f",
      digits = 0,
      big.mark = ".",
      decimal.mark = ","
    )

  saida[uma_casa] <-
    formatC(
      x[uma_casa],
      format = "f",
      digits = 1,
      big.mark = ".",
      decimal.mark = ","
    )

  saida[duas_casas] <-
    formatC(
      x[duas_casas],
      format = "f",
      digits = 2,
      big.mark = ".",
      decimal.mark = ","
    )

  saida
}

acf_temperatura <- acf(
  dados$Temperatura,
  lag.max = 24 * 14,
  plot = FALSE,
  na.action = na.pass
)

autocorrelacao <- tibble(
  defasagem_horas = as.numeric(acf_temperatura$lag),
  correlacao = as.numeric(acf_temperatura$acf)
)

grafico_autocorrelacao <-

  ggplot(
    autocorrelacao,
    aes(defasagem_horas, correlacao)
  ) +

  geom_col(
    fill = azul_principal,
    width = .85
  ) +

  geom_hline(
    yintercept = 0,
    colour = texto,
    linewidth = .45
  ) +

  geom_vline(
    xintercept = seq(24, 96, 24),
    colour = cobre,
    linetype = "22",
    linewidth = .5,
    alpha = .8
  ) +

  scale_x_continuous(
    breaks = seq(0, 24 * 14, by = 24),
    labels = label_number()
  ) +

  scale_y_continuous(
    breaks = breaks_pretty(6),
    labels = label_decimal
  ) +

  labs(
    x = "Defasagem (horas)",
    y = "Autocorrelação"
  ) +

  theme_minimal(base_size = 13) +

  theme(

    plot.background = element_rect(
      fill = fundo,
      colour = NA
    ),

    panel.background = element_rect(
      fill = fundo,
      colour = NA
    ),

    panel.grid.major = element_line(
      colour = scales::alpha(texto, .12),
      linewidth = .3
    ),

    panel.grid.major.x = element_blank(),

    panel.grid.minor = element_blank(),

    plot.title = element_text(
      colour = teal,
      face = "bold",
      size = 18,
      hjust = .5,
      margin = margin(b = 12)
    ),

    axis.title = element_text(
      colour = texto,
      size = 15
    ),

    axis.text = element_text(
      colour = texto,
      size = 12
    ),

    axis.text.x = element_text(
      angle = 90,
      vjust = .5
    ),

    axis.line = element_line(
      colour = scales::alpha(texto, .35)
    ),

    axis.ticks = element_line(
      colour = scales::alpha(texto, .35)
    ),

    plot.margin = margin(8, 8, 8, 8)
  )

grafico_autocorrelacao

ggsave(
  "Apresentação/media/plot_autocorrelacao.png",
  plot = grafico_autocorrelacao,
  dpi = 600,
  width = 12,
  height = 7,
  units = "in",
  bg = "transparent"
)
