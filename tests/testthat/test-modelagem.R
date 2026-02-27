test_that("catalogo possui 31 modelos para volume e 31 para biomassa", {
  cat_todos <- catalogo_modelos_florestais("todos")
  cat_vol <- catalogo_modelos_florestais("volume")
  cat_bio <- catalogo_modelos_florestais("biomassa")

  expect_s3_class(cat_todos, "data.frame")
  expect_equal(nrow(cat_vol), 31)
  expect_equal(nrow(cat_bio), 31)
  expect_equal(nrow(cat_todos), 62)
  expect_true(any(grepl("Schumacher-Hall nao linear", cat_todos$nome, fixed = TRUE)))
})

test_that("avaliacao inclui r2 ajustado", {
  modelo <- ajustar_modelo_linear(mpg ~ wt + hp, data = mtcars)
  met <- avaliar_modelo(modelo)

  expect_true("r2_ajustado" %in% names(met))
  expect_true(is.numeric(met$r2_ajustado))
})

test_that("ajuste automatico aceita aliases de dap e ht", {
  set.seed(123)
  dados <- data.frame(
    D = runif(70, 10, 40),
    hm = runif(70, 10, 30),
    densidade = runif(70, 0.4, 0.8)
  )
  dados$volume <- 0.00006 * (dados$D^2) * dados$hm + rnorm(70, 0, 0.05)

  ajuste <- ajustar_modelos_florestais(
    data = dados,
    resposta = "volume",
    alvo = "volume",
    criterio = "aic",
    n_top = 10,
    mapa_variaveis = list(
      dap = c("dap", "D"),
      ht = c("ht", "hm", "hc"),
      rho = c("rho", "densidade")
    )
  )

  expect_true(nrow(ajuste$ranking) >= 1)
  expect_true(all(c("id", "id_base", "r2_ajustado", "rmse", "mae", "r2", "aic", "bic") %in% names(ajuste$ranking)))
})

test_that("ajuste automatico aceita custom e retorna r2 ajustado", {
  set.seed(123)
  dados <- data.frame(
    dap = runif(70, 10, 40),
    ht = runif(70, 10, 30),
    rho = runif(70, 0.4, 0.8)
  )
  dados$volume <- 0.00006 * (dados$dap^2) * dados$ht + rnorm(70, 0, 0.05)

  mod_extra <- criar_modelo_custom(
    id = "CUSTOM_01",
    nome = "Custom linear",
    alvo = "volume",
    tipo = "linear",
    formula = ".y ~ dap + ht + I(dap * ht)",
    variaveis = "dap,ht"
  )

  ajuste <- ajustar_modelos_florestais(
    data = dados,
    resposta = "volume",
    alvo = "volume",
    criterio = "aic",
    n_top = 35,
    modelos_custom = mod_extra
  )

  expect_true(any(ajuste$ranking$id == "CUSTOM_01"))
  expect_true(all(c("id", "id_base", "r2_ajustado", "rmse", "mae", "r2", "aic", "bic") %in% names(ajuste$ranking)))
})
