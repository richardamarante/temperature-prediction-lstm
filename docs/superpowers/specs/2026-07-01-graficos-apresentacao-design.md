# Gráficos dos modelos vencedores na apresentação

## Objetivo

Integrar à apresentação Quarto quatro gráficos já existentes em conteúdo, regenerados com a identidade visual escura da apresentação:

1. RMSE por horizonte e janela;
2. R² por horizonte e janela;
3. previsões futuras das quatro janelas;
4. previsão futura definitiva, formada pelos 24 modelos vencedores — para cada horizonte `k`, a janela com menor RMSE.

## Fonte dos dados

Os gráficos serão gerados a partir das tabelas em `/tmp/regeraGraficos/resultados_lstm`. A seleção dos vencedores será validada contra `lstm_melhores_modelos_por_alvo_k.csv`.

## Aparência

Os gráficos manterão conteúdo, escalas e significado dos PNGs atuais, adotando fundo `#0A1620`, texto `#DCE6EA`, títulos em `#5FA8AE` e cores de janela coerentes com a apresentação. Serão exportados em alta resolução e com fundo transparente ou escuro compatível com os slides.

## Integração

Os quatro PNGs serão copiados para a árvore `Apresentação/media` e referenciados em slides próprios. O gráfico `real vs. previsto` restrito à janela de 72 horas será removido da apresentação, pois não representa a seleção vencedora por horizonte.

## Validação

- confirmar 24 vencedores, um para cada `k` de 1 a 24;
- confirmar que cada ponto definitivo usa a janela de menor RMSE no respectivo `k`;
- confirmar existência e dimensões dos quatro PNGs;
- renderizar o projeto Quarto e verificar o HTML gerado.
