# Setup -----

library(tidyverse)
library(lubridate)
library(ggthemes)

dados <- readRDS("Relatório/data/dados_niteroi.rds")

dados <- dados %>%
  mutate(Data = as.POSIXct(Data))

# Plot ----

variaveis <- c(
  "Temperatura",
  "TempSuperficie",
  "RadiacaoSolarLiquida",
  "RadiacaoTermicaLiq",
  "FluxoCalorSensivel",
  "FluxoCalorLatente",
  "TempOrvalho",
  "PrecipTotal",
  "VentoU_LesteOeste",
  "VentoV_NorteSul",
  "PressaoSuperficial",
  "VelocVento",
  "UmidadeRelativa"
)

nomes <- c(
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

azul_principal <- "#0072CE"
laranja <- "#FF7F0E"
verde <- "#00A676"
vermelho <- "#D7263D"
roxo <- "#7B2CBF"
verde_vento <- "#009E73"
amarelo <- "#F9A602"
ciano <- "#00A6D6"
cinza <- "#4D4D4D"

cores <- c(
  "Temperatura do Ar"               = laranja,
  "Temperatura da Superfície"       = vermelho,
  "Temperatura do Ponto de Orvalho" = verde,
  "Umidade Relativa"                = ciano,
  "Precipitação"                    = azul_principal,
  "Vento Leste-Oeste"               = ciano,
  "Vento Norte-Sul"                 = roxo,
  "Velocidade do Vento"             = verde_vento,
  "Pressão Superficial"             = cinza,
  "Radiação Solar Líquida"          = "#F2C500",
  "Radiação Térmica Líquida"        = vermelho,
  "Fluxo de Calor Sensível"         = ciano,
  "Fluxo de Calor Latente"          = "#E66100"
)

temperatura <- dados %>%
  filter(
    Data >= ymd_hms("2021-06-21 00:00:00"),
    Data <= ymd_hms("2021-06-28 23:00:00")
  ) %>%
  select(Data, Temperatura)

grafico_variavel <- function(var,
                             titulo,
                             cor){

  base <- dados %>%
    filter(
      Data >= ymd_hms("2021-06-21 00:00:00"),
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
      linewidth = .25
    ) +

    geom_line(
      aes(y = TempEscalada),
      colour = "black",
      linewidth = .35,
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

    ggthemes::theme_hc() +

    theme(

      plot.title = element_text(
        size = 13,
        face = "bold",
        hjust = .5
      ),

      axis.title.y.left = element_blank(),
      axis.title.y.right = element_blank(),

      axis.text.y = element_text(size = 8),
      axis.text.y.right = element_text(size = 8),

      # Remove completamente o eixo X
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()

    )

}

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

p <-
  (p1 | p2 | p3 | p4) /
  (p5 | p6 | p7 | p8) /
  (p9 | p10 | p11 | p12) + 
  plot_annotation(caption = "0h 21/06/2021 até 23h 28/06/2021")

p

ggsave("Relatório/media/plot_descritivo.png",
       plot = p,
      dpi = 600, width = 12, height = 7, units = "in", bg = "white")
