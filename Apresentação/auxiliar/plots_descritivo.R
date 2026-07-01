# Setup -----

library(tidyverse)
library(lubridate)
library(ggthemes)
library(patchwork)
library(scales)
library(ggtext)

dados <- readRDS("Apresentação/data/dados_niteroi.rds")

dados <- dados %>%
  mutate(Data = as.POSIXct(Data))

# Cores -----

azul_principal <- "#0072CE"
laranja <- "#FF7F0E"
verde <- "#00A676"
vermelho <- "#D7263D"
roxo <- "#7B2CBF"
verde_vento <- "#009E73"
amarelo <- "#F9A602"
ciano <- "#00A6D6"
cinza <- "#AEB7BF"

# Temperatura de referência -----

temperatura <- dados %>%
  filter(
    Data >= ymd_hms("2021-06-01 00:00:00"),
    Data <= ymd_hms("2021-06-28 23:00:00")
  ) %>%
  select(Data, Temperatura)

# Função -----

grafico_variavel <- function(var,
                             titulo,
                             cor){

  base <- dados %>%
    filter(
      Data >= ymd_hms("2021-06-01 00:00:00"),
      Data <= ymd_hms("2021-06-28 23:00:00")
    ) %>%
    select(Data, Valor = all_of(var)) %>%
    left_join(temperatura, by = "Data")

  ymin1 <- min(base$Valor, na.rm = TRUE)
  ymax1 <- max(base$Valor, na.rm = TRUE)

  ymin2 <- min(base$Temperatura, na.rm = TRUE)
  ymax2 <- max(base$Temperatura, na.rm = TRUE)

  base$TempEscalada <-
    (base$Temperatura - ymin2) /
    (ymax2 - ymin2) *
    (ymax1 - ymin1) +
    ymin1

  ggplot(base, aes(Data)) +

    geom_line(
      aes(y = Valor),
      colour = cor,
      linewidth = 0.8
    ) +

    geom_line(
      aes(y = TempEscalada),
      colour = "#F2EDE4",
      linewidth = 0.45,
      alpha = 0.5,
      linetype = "22"
    ) +

    scale_y_continuous(

      name = NULL,

      sec.axis = sec_axis(

        ~ (. - ymin1) /
          (ymax1 - ymin1) *
          (ymax2 - ymin2) +
          ymin2,

        name = NULL

      )

    ) +

    labs(
      title = titulo,
      x = NULL
    ) +

    theme_minimal(base_size = 10) +

    theme(

      # Fundo
      plot.background = element_rect(
        fill = "#0A1620",
        colour = NA
      ),

      panel.background = element_rect(
        fill = "#0A1620",
        colour = NA
      ),

      # Grade
      panel.grid.major = element_blank(),

      panel.grid.minor = element_blank(),

      # Título
      plot.title = element_text(
        colour = "#5FA8AE",
        face = "bold",
        size = 11,
        hjust = .5
      ),

      # Texto
      axis.title = element_blank(),

      axis.text.y = element_text(
        colour = "#DCE6EA",
        size = 7
      ),

      axis.text.y.right = element_text(
        colour = "#DCE6EA",
        size = 7
      ),

      # Remove eixo x
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),

      axis.line = element_line(
        colour = alpha("#DCE6EA", .35)
      ),

      axis.ticks = element_line(
        colour = alpha("#DCE6EA", .35)
      ),

      plot.margin = margin(5,5,5,5)
    )
}

# Gráficos -----

p1  <- grafico_variavel("TempSuperficie","Temp. Superfície",vermelho)
p2  <- grafico_variavel("RadiacaoSolarLiquida","Rad. Solar",amarelo)
p3  <- grafico_variavel("RadiacaoTermicaLiq","Rad. Térmica",vermelho)
p4  <- grafico_variavel("FluxoCalorSensivel","Fluxo Sensível",ciano)

p5  <- grafico_variavel("FluxoCalorLatente","Fluxo Latente","#E66100")
p6  <- grafico_variavel("TempOrvalho","Ponto Orvalho",verde)
p7  <- grafico_variavel("PrecipTotal","Precipitação",azul_principal)
p8  <- grafico_variavel("PressaoSuperficial","Pressão",cinza)

p9  <- grafico_variavel("VelocVento","Vel. Vento",verde_vento)
p10 <- grafico_variavel("UmidadeRelativa","Umidade",ciano)
p11 <- grafico_variavel("VentoU_LesteOeste","Vento U",ciano)
p12 <- grafico_variavel("VentoV_NorteSul","Vento V",roxo)

# Figura final -----

p_parte1 <-
  (p1 | p2) /
  (p3 | p4)+

  plot_annotation(

    caption =
      "<span style='float:left;'>01/06/2021 00:00 até 28/06/2021 23:00</span>
       <span style='float:right;'>
       Temperatura do Ar: <span style='color:#F2EDE4;'>- - -</span>
       </span>",

    theme = theme(

      plot.background = element_rect(
        fill = "#0A1620",
        colour = NA
      ),

      plot.caption = ggtext::element_markdown(
        colour = "#8FA8B5",
        size = 11,
        hjust = 0
      )

    )

  )

p_parte2 <-
  (p5 | p6) /
  (p7 | p8)+

  plot_annotation(

    caption =
      "<span style='float:left;'>01/06/2021 00:00 até 28/06/2021 23:00</span>
       <span style='float:right;'>
       Temperatura do Ar: <span style='color:#F2EDE4;'>- - -</span>
       </span>",

    theme = theme(

      plot.background = element_rect(
        fill = "#0A1620",
        colour = NA
      ),

      plot.caption = ggtext::element_markdown(
        colour = "#8FA8B5",
        size = 11,
        hjust = 0
      )

    )

  )

p_parte3 <-
  (p9 | p10) /
  (p11 | p12)+

  plot_annotation(

    caption =
      "<span style='float:left;'>01/06/2021 00:00 até 28/06/2021 23:00</span>
       <span style='float:right;'>
       Temperatura do Ar: <span style='color:#F2EDE4;'>- - -</span>
       </span>",

    theme = theme(

      plot.background = element_rect(
        fill = "#0A1620",
        colour = NA
      ),

      plot.caption = ggtext::element_markdown(
        colour = "#8FA8B5",
        size = 11,
        hjust = 0
      )

    )

  )

p_parte1
p_parte2
p_parte3

# Salvar -----

ggsave(
  "Apresentação/media/plot_descritivo_parte1.png",
  plot = p_parte1,
  dpi = 600,
  width = 12,
  height = 7,
  units = "in",
  bg = "transparent"
)
ggsave(
  "Apresentação/media/plot_descritivo_parte2.png",
  plot = p_parte2,
  dpi = 600,
  width = 12,
  height = 7,
  units = "in",
  bg = "transparent"
)
ggsave(
  "Apresentação/media/plot_descritivo_parte3.png",
  plot = p_parte3,
  dpi = 600,
  width = 12,
  height = 7,
  units = "in",
  bg = "transparent"
)
