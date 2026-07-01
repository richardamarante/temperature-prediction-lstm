source("Apresentação/auxiliar/plots_resultados.R", encoding = "UTF-8")

base_resultados <- Sys.getenv(
  "LSTM_RESULTADOS_DIR",
  "/tmp/regeraGraficos/resultados_lstm"
)

resultados <- readr::read_csv(
  file.path(base_resultados, "lstm_resultados_gerais.csv"),
  show_col_types = FALSE
)

melhores_arquivo <- readr::read_csv(
  file.path(base_resultados, "lstm_melhores_modelos_por_alvo_k.csv"),
  show_col_types = FALSE
)

melhores_calculados <- selecionar_melhores_modelos(resultados)

stopifnot(
  nrow(melhores_calculados) == 24L,
  identical(sort(as.integer(melhores_calculados$k_h)), 1:24),
  !anyDuplicated(melhores_calculados$k_h)
)

comparacao <- melhores_calculados |>
  dplyr::select(k_h, janela_calculada = janela_h, rmse_calculado = rmse_C) |>
  dplyr::left_join(
    melhores_arquivo |>
      dplyr::filter(alvo == "Temperatura") |>
      dplyr::select(k_h, janela_arquivo = janela_h, rmse_arquivo = rmse_C),
    by = "k_h"
  )

stopifnot(
  all(comparacao$janela_calculada == comparacao$janela_arquivo),
  isTRUE(all.equal(comparacao$rmse_calculado, comparacao$rmse_arquivo))
)

message("Seleção dos 24 modelos vencedores validada.")

grafico_rmse <- grafico_metrica(resultados, "rmse_C")
stopifnot(
  is.null(grafico_rmse$labels$title),
  identical(grafico_rmse$theme$axis.title$face, "plain"),
  identical(grafico_rmse$theme$axis.title$size, 15),
  identical(grafico_rmse$theme$axis.text$size, 12)
)

message("Tema dos gráficos de resultados validado.")

artefatos <- file.path(
  "Apresentação/media",
  c(
    "lstm_rmse_por_k_e_janela.png",
    "lstm_r2_por_k_e_janela.png",
    "lstm_previsoes_por_janela.png",
    "lstm_previsao_modelos_vencedores.png"
  )
)

stopifnot(
  all(file.exists(artefatos)),
  all(file.info(artefatos)$size > 100000)
)

message("Os quatro gráficos temáticos foram encontrados e validados.")
