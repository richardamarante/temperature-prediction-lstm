source("Apresentação/auxiliar/plots_real_vs_previsto.R", encoding = "UTF-8")

base_resultados <- Sys.getenv(
  "LSTM_RESULTADOS_DIR",
  "/tmp/regeraGraficos/resultados_lstm"
)

base <- carregar_real_vs_previsto_vencedores(base_resultados, n_horas = 720)

stopifnot(
  identical(sort(unique(as.integer(base$k_h))), 1:24),
  nrow(dplyr::distinct(base, k_h, janela_h)) == 24L,
  all(dplyr::count(base, k_h)$n == 720L)
)

melhores <- readr::read_csv(
  file.path(base_resultados, "lstm_melhores_modelos_por_alvo_k.csv"),
  show_col_types = FALSE
) |>
  dplyr::filter(alvo == "Temperatura") |>
  dplyr::select(k_h, janela_esperada = janela_h)

stopifnot(
  all(
    dplyr::distinct(base, k_h, janela_h) |>
      dplyr::left_join(melhores, by = "k_h") |>
      dplyr::transmute(ok = janela_h == janela_esperada) |>
      dplyr::pull(ok)
  )
)

message("Base real × previsto dos 24 vencedores validada.")

grafico_teste <- grafico_individual(base, 10, mostrar_k = FALSE)
stopifnot(
  is.null(grafico_teste$labels$title),
  is.null(grafico_teste$labels$subtitle),
  identical(grafico_teste$labels$x, "Tempo (h)"),
  identical(grafico_teste$theme$axis.title$face, "plain"),
  identical(grafico_teste$theme$axis.title$size, 15),
  identical(grafico_teste$theme$axis.text$size, 12),
  grepl("RMSE.*MSE.*R²", grafico_teste$labels$caption),
  !grepl("°C²", grafico_teste$labels$caption, fixed = TRUE),
  identical(grafico_teste$theme$plot.caption$family, "mono"),
  grafico_teste$layers[[2]]$geom_params$linewidth >
    grafico_teste$layers[[1]]$geom_params$linewidth
)

message("Textos e destaque da linha prevista validados.")

raiz_saida <- "Apresentação/media/real_vs_previsto"
arquivos_4 <- list.files(file.path(raiz_saida, "4_facetas"), "\\.png$", full.names = TRUE)
arquivos_2 <- list.files(file.path(raiz_saida, "2_facetas"), "\\.png$", full.names = TRUE)
arquivos_1 <- list.files(file.path(raiz_saida, "individuais"), "\\.png$", full.names = TRUE)
arquivo_gif <- file.path(raiz_saida, "real_vs_previsto_k01_k24.gif")

stopifnot(
  length(arquivos_4) == 6L,
  length(arquivos_2) == 12L,
  length(arquivos_1) == 24L,
  all(file.info(c(arquivos_4, arquivos_2, arquivos_1))$size > 100000),
  file.exists(arquivo_gif),
  file.info(arquivo_gif)$size > 100000,
  length(magick::image_read(arquivo_gif)) == 24L,
  all(as.integer(strsplit(system2(
    "identify",
    c("-format", shQuote("%T "), shQuote(arquivo_gif)),
    stdout = TRUE
  ), " +")[[1]]) == 80L)
)

message("42 PNGs e GIF com 24 quadros validados.")
