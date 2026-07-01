# Real × previsto por horizonte — desenho

## Objetivo

Gerar e integrar à apresentação a evolução do ajuste real × previsto para todos os horizontes `k = 1, …, 24`, sempre usando, em cada `k`, a janela de observação com menor RMSE.

## Saídas estáticas

- 6 painéis com quatro facetas: `1–4`, `5–8`, …, `21–24`;
- 12 painéis com duas facetas: `1–2`, `3–4`, …, `23–24`;
- 24 imagens individuais, uma para cada horizonte.

As 42 imagens usarão o mesmo trecho final do conjunto de teste, limites comparáveis, linha real e linha prevista, fundo `#0A1620` e identidade visual da apresentação.

## Saída animada

Um GIF gerado com `gganimate` percorrerá os 24 horizontes em ordem. Cada quadro permanecerá por aproximadamente 0,4 segundo e a animação será repetida continuamente no slide.

## Integração no Quarto

Cada imagem estática será colocada em um slide próprio. O GIF ficará em um slide adicional. A seção será inserida após as métricas e antes das previsões futuras.

## Validação

- confirmar a cobertura exata de `k = 1, …, 24` em cada família;
- confirmar que cada série usa a janela vencedora registrada em `lstm_melhores_modelos_por_alvo_k.csv`;
- confirmar 42 PNGs e um GIF animado;
- renderizar o Quarto e verificar os 43 slides novos no HTML.
