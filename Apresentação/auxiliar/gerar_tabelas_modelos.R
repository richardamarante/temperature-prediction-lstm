library(dplyr)
library(readr)

formatar_numero <- function(x) {
  sub("\\.", ",", formatC(x, format = "f", digits = 3))
}

montar_tabela_html <- function(dados) {
  linhas <- apply(dados, 1, function(linha) {
    paste0(
      "<tr>",
      paste0("<td>", linha, "</td>", collapse = ""),
      "</tr>"
    )
  })

  paste0(
    '<table class="tabela-tema tabela-scroll tabela-modelos">\n',
    "<thead><tr>",
    "<th>Horizonte (k)</th>",
    "<th>Janela (h)</th>",
    "<th>RMSE (°C)</th>",
    "<th>MAE (°C)</th>",
    "<th>R²</th>",
    "</tr></thead>\n",
    "<tbody>\n",
    paste(linhas, collapse = "\n"),
    "\n</tbody>\n</table>"
  )
}

preparar_tabela <- function(dados) {
  dados |>
    arrange(k_h, janela_h) |>
    transmute(
      horizonte = paste0(k_h, "h"),
      janela = as.character(janela_h),
      rmse = formatar_numero(rmse_C),
      mae = formatar_numero(mae_C),
      r2 = formatar_numero(r2)
    )
}

gerar_tabelas_modelos <- function(
    base_resultados = Sys.getenv(
      "LSTM_RESULTADOS_DIR",
      "/tmp/regeraGraficos/resultados_lstm"
    ),
    pasta_saida = "Apresentação/includes") {
  resultados <- read_csv(
    file.path(base_resultados, "lstm_resultados_gerais.csv"),
    show_col_types = FALSE
  ) |>
    filter(alvo == "Temperatura")

  melhores <- resultados |>
    group_by(k_h) |>
    slice_min(rmse_C, n = 1, with_ties = FALSE) |>
    ungroup()

  stopifnot(nrow(resultados) == 96L, nrow(melhores) == 24L)

  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  saidas <- c(
    todos = file.path(pasta_saida, "todos_modelos.html"),
    melhores = file.path(pasta_saida, "melhores_modelos.html")
  )

  writeLines(
    montar_tabela_html(preparar_tabela(resultados)),
    saidas[["todos"]],
    useBytes = TRUE
  )
  writeLines(
    montar_tabela_html(preparar_tabela(melhores)),
    saidas[["melhores"]],
    useBytes = TRUE
  )

  invisible(saidas)
}

if (sys.nframe() == 0L) {
  gerar_tabelas_modelos()
}
