<div align="center">
  <h1>🌡️ Previsão de Temperatura Horária em Niterói</h1>
  <p>
    Aplicação de Redes Neurais Recorrentes do tipo <strong>LSTM</strong><br>
    para previsão de temperatura horária do ar em Niterói, RJ<br>
    <strong>Redes Neurais</strong> — Universidade Federal Fluminense
  </p>
  <a href="https://richardamarante.github.io/temperature-prediction-lstm/">
    <img src="https://img.shields.io/badge/▶  Ver%20Apresentação-5FA8AE?style=for-the-badge&logoColor=white" alt="Ver Apresentação"/>
  </a>
  <br/><br/>
  <img src="https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r&logoColor=white"/>
  <img src="https://img.shields.io/badge/Python-3776AB?style=flat-square&logo=python&logoColor=white"/>
  <img src="https://img.shields.io/badge/Quarto-75AADB?style=flat-square&logo=quarto&logoColor=white"/>
  <img src="https://img.shields.io/badge/LSTM-E8935C?style=flat-square&logoColor=white"/>
  <img src="https://img.shields.io/badge/ERA5--Land-0A1620?style=flat-square&logoColor=white"/>
</div>

---

Trabalho desenvolvido para a disciplina **Redes Neurais** da Universidade Federal Fluminense, período 2026.1.

**Autores:** Caio Vinicius Araujo de Oliveira Salviano (`caiosalviano@id.uff.br`) & Richard Amarante Melo (`richardmelo@id.uff.br`)

---

## 🎯 Objetivo

Desenvolver e comparar modelos **LSTM** (*Long Short-Term Memory*) para prever a **temperatura horária do ar** em Niterói, RJ, utilizando variáveis meteorológicas da base **ERA5-Land** (Copernicus / ECMWF).

Além da série horária, o mesmo framework foi aplicado a três agregações diárias derivadas da temperatura do ar: **máxima**, **média** e **mínima** diárias.

---

## 🗄️ Dados

| Atributo | Descrição |
|----------|-----------|
| **Fonte principal** | [ERA5-Land — Copernicus Climate Data Store](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land) |
| **Fontes complementares** | Meteostat API, NASA POWER |
| **Cobertura** | Niterói, RJ — 0h 31/12/2009 → 0h 01/07/2026 |
| **Resolução** | Horária |
| **Variáveis** | 12 variáveis meteorológicas numéricas contínuas |
| **Dados faltantes** | Nenhum |

> O ERA5-Land apresenta atraso de até **6 dias** em relação à data atual.
> As fontes complementares cobrem esse período de defasagem via API,
> viabilizando previsões para datas futuras.

---

## 📐 Metodologia

### Divisão da Base

| Conjunto | Proporção | Início |
|----------|:---------:|--------|
| Treino | **70%** | 31/12/2009 |
| Validação | **15%** | 29/06/2021 |
| Teste | **15%** | 16/12/2023 |

Divisão **cronológica** — sem embaralhamento, preservando a ordem temporal da série.

### Arquitetura LSTM

- **64 unidades** na camada LSTM
- *Dropout* de **0,2**
- Otimizador **Adam** — taxa de aprendizado 0,001
- *Early stopping* monitorado pela perda no conjunto de validação

### Hiperparâmetros Avaliados

| | Temperatura Horária |
|---|---|
| **Janelas** | 24h, 48h, 72h, 96h |
| **Horizontes (k)** | 1h a 24h |
| **Total de modelos** | 96 |

**Total: 26 modelos treinados e comparados.**

### Métricas de Comparação

| Métrica | Papel |
|---------|-------|
| **RMSE** | Métrica principal — penaliza erros grandes |
| **MAE** | Métrica complementar — erro médio absoluto |
| **R²** | Proporção da variância explicada pelo modelo |

---

## Principais Achados

- A **temperatura horária com 1h de horizonte** foi o melhor resultado geral — R² de **0,96** e RMSE de apenas **0,81 °C**
- O desempenho se deteriora progressivamente com o aumento do horizonte de previsão, em todos os alvos
- Em todos os grupos, o **menor horizonte** (1h ou 1 dia) produziu o melhor modelo

---

<div align="center">
  <sub>Universidade Federal Fluminense · Redes Neurais · 2026.1</sub>
</div>
