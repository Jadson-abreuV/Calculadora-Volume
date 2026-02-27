# regressaoFacil

Pacote em **R** para ajuste e avaliação de modelos de regressão linear e não linear, com **31 modelos-base florestais** aplicados para **volume** e **biomassa** (62 combinações no catálogo).

## Funcionalidades

- Ajuste de modelos lineares com `ajustar_modelo_linear()`.
- Ajuste de modelos não lineares com `ajustar_modelo_nlinear()`.
- Avaliação com `avaliar_modelo()`:
  - RMSE
  - MAE
  - R²
  - R² ajustado
  - AIC
  - BIC
- Análise gráfica de resíduos com `analisar_residuos()`.
- Catálogo com 31 modelos para cada alvo (`volume` e `biomassa`), incluindo **Schumacher-Hall não linear**.
- Ajuste automático dos modelos disponíveis com `ajustar_modelos_florestais()`.
- Inclusão de modelo fora da lista com `criar_modelo_custom()` e `modelos_custom`.
- Para modelos linearizados em log, recálculo da predição na escala original (com correção de viés) e métricas nessa escala.
- Flexibilização de nomes de variáveis independentes via `mapa_variaveis` (ex.: usar `D`, `hm` ou `hc` no lugar de `dap` e `ht`).

## Exemplo: ajuste automático + diagnóstico

```r
library(regressaoFacil)

set.seed(123)
dados <- data.frame(
  dap = runif(80, 10, 45),
  ht = runif(80, 8, 30),
  rho = runif(80, 0.35, 0.75)
)

dados$volume <- 0.00006 * (dados$dap^2) * dados$ht + rnorm(80, 0, 0.08)

# 31 modelos para volume
nrow(catalogo_modelos_florestais("volume"))

# ajuste automático
ajuste <- ajustar_modelos_florestais(
  data = dados,
  resposta = "volume",
  alvo = "volume",
  criterio = "aic",
  n_top = 10,
  mapa_variaveis = list(dap = c("dap", "D"), ht = c("ht", "hm", "hc"))
)

head(ajuste$ranking)

# gráfico de resíduos do melhor modelo
top_id <- ajuste$ranking$id[1]
modelo_top <- ajuste$modelos_ajustados[[top_id]]
analisar_residuos(modelo_top, titulo = top_id)
```
