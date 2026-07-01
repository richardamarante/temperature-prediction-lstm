source("Apresentação/auxiliar/gerar_tabelas_modelos.R", encoding = "UTF-8")

base_resultados <- Sys.getenv(
  "LSTM_RESULTADOS_DIR",
  "/tmp/regeraGraficos/resultados_lstm"
)

pasta_teste <- tempfile("tabelas_modelos_")
dir.create(pasta_teste)
on.exit(unlink(pasta_teste, recursive = TRUE), add = TRUE)

saidas <- gerar_tabelas_modelos(base_resultados, pasta_teste)
todos <- paste(readLines(saidas[["todos"]], encoding = "UTF-8"), collapse = "\n")
melhores <- paste(readLines(saidas[["melhores"]], encoding = "UTF-8"), collapse = "\n")

contar_linhas <- function(html) {
  comprimentos <- gregexpr("<tr>", html, fixed = TRUE)[[1]]
  sum(comprimentos > 0) - 1L
}

stopifnot(
  contar_linhas(todos) == 96L,
  contar_linhas(melhores) == 24L,
  !grepl(">Alvo<", todos, fixed = TRUE),
  grepl(">Horizonte (k)<", todos, fixed = TRUE),
  grepl("tabela-scroll", todos, fixed = TRUE),
  grepl("tabela-scroll", melhores, fixed = TRUE)
)

message("Tabelas de 96 e 24 modelos validadas.")
