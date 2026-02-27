#' Ajusta um modelo de regressao linear
#'
#' @param formula Formula do modelo.
#' @param data Data frame com os dados.
#'
#' @return Objeto de classe `lm`.
#' @export
ajustar_modelo_linear <- function(formula, data) {
  stats::lm(formula = formula, data = data)
}

#' Ajusta um modelo de regressao nao linear
#'
#' @param formula Formula do modelo nao linear.
#' @param data Data frame com os dados.
#' @param start Lista com valores iniciais dos parametros.
#' @param ... Argumentos adicionais repassados para `stats::nls()`.
#'
#' @return Objeto de classe `nls`.
#' @export
ajustar_modelo_nlinear <- function(formula, data, start, ...) {
  stats::nls(formula = formula, data = data, start = start, ...)
}

.metricas_vetores <- function(y_true, y_pred, modelo = NULL, p = NULL) {
  if (!is.numeric(y_true) || !is.numeric(y_pred)) {
    stop("`y_true` e `y_pred` devem ser numericos.", call. = FALSE)
  }

  if (length(y_true) != length(y_pred)) {
    stop("`y_true` e `y_pred` devem ter o mesmo tamanho.", call. = FALSE)
  }

  residuos <- y_true - y_pred
  ss_res <- sum(residuos^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  r2 <- if (ss_tot == 0) NA_real_ else 1 - (ss_res / ss_tot)

  n <- length(y_true)
  if (is.null(p) && !is.null(modelo)) {
    p <- length(stats::coef(modelo))
  }
  if (is.null(p)) {
    p <- 1
  }

  r2_ajustado <- if (is.na(r2) || (n - p - 1) <= 0) {
    NA_real_
  } else {
    1 - ((1 - r2) * (n - 1) / (n - p - 1))
  }

  aic <- if (is.null(modelo)) NA_real_ else stats::AIC(modelo)
  bic <- if (is.null(modelo)) NA_real_ else stats::BIC(modelo)

  list(
    rmse = sqrt(mean(residuos^2)),
    mae = mean(abs(residuos)),
    r2 = r2,
    r2_ajustado = r2_ajustado,
    aic = aic,
    bic = bic
  )
}

#' Avalia um modelo de regressao
#'
#' @param modelo Objeto de classe `lm` ou `nls`.
#' @param y_true Vetor numerico opcional com valores observados.
#' @param y_pred Vetor numerico opcional com predicoes. Quando `NULL`, usa
#'   `fitted(modelo)`.
#'
#' @return Lista com `rmse`, `mae`, `r2`, `r2_ajustado`, `aic` e `bic`.
#' @export
avaliar_modelo <- function(modelo, y_true = NULL, y_pred = NULL) {
  if (!inherits(modelo, c("lm", "nls"))) {
    stop("`modelo` deve ser um objeto das classes 'lm' ou 'nls'.", call. = FALSE)
  }

  if (is.null(y_pred)) {
    y_pred <- stats::fitted(modelo)
  }

  if (is.null(y_true)) {
    y_true <- y_pred + stats::residuals(modelo)
  }

  .metricas_vetores(y_true = y_true, y_pred = y_pred, modelo = modelo)
}

#' Analise grafica de residuos
#'
#' Gera quatro graficos base: residuos vs ajustados, QQ-plot, histograma dos
#' residuos e escala-localizacao.
#'
#' @param modelo Objeto `lm` ou `nls`.
#' @param y_true Vetor observado opcional (escala original).
#' @param y_pred Vetor predito opcional (escala original).
#' @param titulo Titulo geral opcional.
#'
#' @return Vetor de residuos invisivel.
#' @export
analisar_residuos <- function(modelo, y_true = NULL, y_pred = NULL, titulo = NULL) {
  if (is.null(y_pred)) {
    y_pred <- stats::fitted(modelo)
  }
  if (is.null(y_true)) {
    y_true <- y_pred + stats::residuals(modelo)
  }

  residuos <- y_true - y_pred

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mfrow = c(2, 2))
  graphics::plot(y_pred, residuos,
    xlab = "Valores ajustados", ylab = "Residuos",
    main = "Residuos vs Ajustados"
  )
  graphics::abline(h = 0, lty = 2, col = "red")

  stats::qqnorm(residuos, main = "QQ-plot dos residuos")
  stats::qqline(residuos, col = "red", lty = 2)

  graphics::hist(residuos, main = "Histograma dos residuos", xlab = "Residuos")

  graphics::plot(y_pred, sqrt(abs(residuos)),
    xlab = "Valores ajustados", ylab = "sqrt(|residuos|)",
    main = "Escala-Localizacao"
  )

  if (!is.null(titulo)) {
    graphics::mtext(titulo, outer = TRUE, line = -2, cex = 1.1)
  }

  invisible(residuos)
}

#' Compara multiplos modelos
#'
#' @param ... Objetos de modelo `lm` e/ou `nls`.
#'
#' @return `data.frame` com metricas por modelo.
#' @export
comparar_modelos <- function(...) {
  modelos <- list(...)

  if (length(modelos) == 0) {
    stop("Informe ao menos um modelo para comparacao.", call. = FALSE)
  }

  classes_validas <- vapply(modelos, function(m) inherits(m, c("lm", "nls")), logical(1))
  if (!all(classes_validas)) {
    stop("Todos os objetos devem ser modelos 'lm' ou 'nls'.", call. = FALSE)
  }

  nomes <- names(modelos)
  if (is.null(nomes) || any(nomes == "")) {
    nomes <- paste0("modelo_", seq_along(modelos))
  }

  metricas <- lapply(modelos, avaliar_modelo)

  data.frame(
    modelo = nomes,
    rmse = vapply(metricas, `[[`, numeric(1), "rmse"),
    mae = vapply(metricas, `[[`, numeric(1), "mae"),
    r2 = vapply(metricas, `[[`, numeric(1), "r2"),
    r2_ajustado = vapply(metricas, `[[`, numeric(1), "r2_ajustado"),
    aic = vapply(metricas, `[[`, numeric(1), "aic"),
    bic = vapply(metricas, `[[`, numeric(1), "bic"),
    stringsAsFactors = FALSE
  )
}

.modelos_base_florestais <- function() {
  data.frame(
    id_base = paste0("M", sprintf("%02d", 1:31)),
    nome = c(
      "Spurr linear", "Meyer", "Schumacher-Hall log", "Stoate", "Takata",
      "Honer", "Combined variable", "Brenac", "Berkhout", "Kopezky-Gehrhardt",
      "Curtis", "Polynomial DAP", "Polynomial DAP+H", "Power DAP", "Power DAP+H",
      "Log-log DAP", "Log-log DAP+H", "Log-log DAP+H+rho", "Linear DAP2H",
      "Linear DAP+H", "Linear DAP2+H", "Quadratico DAP", "Quadratico DAP+H",
      "Exp DAP", "Exp DAP+H", "Power DAP (2)", "Power DAP+H (2)",
      "Weibull DAP", "Logistica DAP", "Chapman-Richards", "Schumacher-Hall nao linear"
    ),
    tipo = c(rep("linear", 13), rep("nlinear", 2), rep("linear", 8), rep("nlinear", 8)),
    formula = c(
      ".y ~ I(dap^2 * ht)",
      ".y ~ dap + I(dap^2) + dap * ht + I(dap^2 * ht) + ht",
      "log(.y) ~ log(dap) + log(ht)",
      ".y ~ dap + I(dap^2) + I(dap^2 * ht) + ht",
      ".y ~ I(dap^2 * ht) + I(dap * ht)",
      ".y ~ I(dap^2) + I(dap^2 * ht)",
      ".y ~ I(dap^2) + I(dap^2 * ht) + ht",
      "log(.y) ~ I(1 / dap) + log(ht)",
      "log(.y) ~ log(dap)",
      ".y ~ I(dap^2)",
      "log(.y) ~ log(dap) + I(1 / dap)",
      ".y ~ dap + I(dap^2) + I(dap^3)",
      ".y ~ dap + I(dap^2) + I(dap^3) + ht + I(ht^2)",
      ".y ~ b0 * (dap^b1)",
      ".y ~ b0 * (dap^b1) * (ht^b2)",
      "log(.y) ~ log(dap)",
      "log(.y) ~ log(dap) + log(ht)",
      "log(.y) ~ log(dap) + log(ht) + log(rho)",
      ".y ~ I(dap^2 * ht)",
      ".y ~ dap + ht",
      ".y ~ I(dap^2) + ht",
      ".y ~ dap + I(dap^2)",
      ".y ~ dap + I(dap^2) + ht + I(ht^2)",
      ".y ~ exp(b0 + b1 * dap)",
      ".y ~ exp(b0 + b1 * dap + b2 * ht)",
      ".y ~ b0 * (dap^b1)",
      ".y ~ b0 * (dap^b1) * (ht^b2)",
      ".y ~ b0 * (1 - exp(-b1 * dap^b2))",
      ".y ~ b0 / (1 + exp(-(b1 + b2 * dap)))",
      ".y ~ b0 * (1 - exp(-b1 * dap))^b2",
      ".y ~ b0 * (dap^b1) * (ht^b2)"
    ),
    variaveis = c(
      "dap,ht", "dap,ht", "dap,ht", "dap,ht", "dap,ht", "dap,ht", "dap,ht",
      "dap,ht", "dap", "dap", "dap", "dap", "dap,ht", "dap", "dap,ht",
      "dap", "dap,ht", "dap,ht,rho", "dap,ht", "dap,ht", "dap,ht", "dap",
      "dap,ht", "dap", "dap,ht", "dap", "dap,ht", "dap", "dap", "dap", "dap,ht"
    ),
    start = c(
      rep("", 13), "b0=0.01;b1=2", "b0=0.01;b1=2;b2=1",
      rep("", 8), "b0=0;b1=0.01", "b0=0;b1=0.01;b2=0.01", "b0=0.01;b1=2",
      "b0=0.01;b1=2;b2=1", "b0=1;b1=0.01;b2=1", "b0=1;b1=0;b2=0.1", "b0=1;b1=0.01;b2=1",
      "b0=0.01;b1=2;b2=1"
    ),
    stringsAsFactors = FALSE
  )
}

.catalogo_modelos_florestais <- function() {
  base <- .modelos_base_florestais()
  volume <- base
  biomassa <- base

  volume$id <- paste0("VOL_", base$id_base)
  volume$alvo <- "volume"

  biomassa$id <- paste0("BIO_", base$id_base)
  biomassa$alvo <- "biomassa"

  out <- rbind(volume, biomassa)
  out[, c("id", "id_base", "nome", "alvo", "tipo", "formula", "variaveis", "start")]
}

.parse_start <- function(x) {
  if (!nzchar(x)) {
    return(NULL)
  }

  partes <- strsplit(x, ";", fixed = TRUE)[[1]]
  nomes <- sub("=.*$", "", partes)
  valores <- as.numeric(sub("^.*=", "", partes))
  out <- as.list(valores)
  names(out) <- nomes
  out
}

.validar_modelos_custom <- function(modelos_custom) {
  if (is.null(modelos_custom)) {
    return(NULL)
  }

  obrig <- c("id", "nome", "alvo", "tipo", "formula", "variaveis", "start")

  if (!is.data.frame(modelos_custom)) {
    stop("`modelos_custom` deve ser um data.frame.", call. = FALSE)
  }

  if (!all(obrig %in% names(modelos_custom))) {
    stop("`modelos_custom` precisa conter colunas: id, nome, alvo, tipo, formula, variaveis, start.", call. = FALSE)
  }

  if (!all(modelos_custom$alvo %in% c("volume", "biomassa"))) {
    stop("Coluna `alvo` de `modelos_custom` deve conter apenas 'volume' ou 'biomassa'.", call. = FALSE)
  }

  if (!all(modelos_custom$tipo %in% c("linear", "nlinear"))) {
    stop("Coluna `tipo` de `modelos_custom` deve conter apenas 'linear' ou 'nlinear'.", call. = FALSE)
  }

  modelos_custom
}

#' Cria uma linha de modelo customizado para incluir no ajuste automatico
#'
#' @param id Identificador unico do modelo customizado.
#' @param nome Nome amigavel.
#' @param alvo `"volume"` ou `"biomassa"`.
#' @param tipo `"linear"` ou `"nlinear"`.
#' @param formula Formula como texto, usando `.y` para resposta.
#' @param variaveis Variaveis independentes em texto separadas por virgula.
#' @param start Parametros iniciais para nlinear no formato `"b0=1;b1=0.1"`.
#'
#' @return `data.frame` de uma linha pronto para `modelos_custom`.
#' @export
criar_modelo_custom <- function(id,
                                nome,
                                alvo = c("volume", "biomassa"),
                                tipo = c("linear", "nlinear"),
                                formula,
                                variaveis,
                                start = "") {
  alvo <- match.arg(alvo)
  tipo <- match.arg(tipo)

  data.frame(
    id = as.character(id),
    id_base = "CUSTOM",
    nome = as.character(nome),
    alvo = alvo,
    tipo = tipo,
    formula = as.character(formula),
    variaveis = as.character(variaveis),
    start = as.character(start),
    stringsAsFactors = FALSE
  )
}

#' Lista os modelos florestais pre-programados no pacote
#'
#' Sao 31 modelos-base replicados para volume e biomassa (62 no total).
#'
#' @param alvo Tipo de modelo: `"todos"`, `"volume"` ou `"biomassa"`.
#'
#' @return `data.frame` com identificador, tipo, formula e variaveis requeridas.
#' @export
catalogo_modelos_florestais <- function(alvo = c("todos", "volume", "biomassa")) {
  alvo <- match.arg(alvo)
  catalogo <- .catalogo_modelos_florestais()

  if (alvo == "todos") {
    return(catalogo)
  }

  catalogo[catalogo$alvo == alvo, , drop = FALSE]
}

.ajuste_linearizado_original <- function(modelo_lm, y_obs_original) {
  pred_log <- stats::fitted(modelo_lm)
  mse_log <- mean(stats::residuals(modelo_lm)^2)
  exp(pred_log) * exp(mse_log / 2)
}

.normalizar_colunas_florestais <- function(data, mapa_variaveis) {
  out <- data
  nomes <- names(out)
  nomes_lower <- tolower(nomes)

  for (canonica in names(mapa_variaveis)) {
    aliases <- unique(tolower(c(canonica, mapa_variaveis[[canonica]])))
    idx <- which(nomes_lower %in% aliases)

    if (length(idx) == 0) {
      next
    }

    idx <- idx[1]
    names(out)[idx] <- canonica
    nomes <- names(out)
    nomes_lower <- tolower(nomes)
  }

  out
}

#' Ajusta automaticamente modelos florestais de volume e biomassa
#'
#' Ajusta os modelos do catalogo compativeis com as variaveis presentes no
#' `data.frame`. Tambem permite incluir modelos fora da lista oficial.
#'
#' Para modelos linearizados do tipo `log(y)`, o pacote recalcula as predicoes
#' na escala original e computa as metricas nessa escala.
#'
#' @param data Data frame com os dados.
#' @param resposta Nome da coluna resposta (ex.: `"volume"` ou `"biomassa"`).
#' @param alvo Tipo de modelos a considerar: `"todos"`, `"volume"` ou `"biomassa"`.
#' @param criterio Criterio para ordenar (`"aic"`, `"bic"`, `"rmse"`, `"mae"`).
#' @param n_top Quantidade maxima de modelos no ranking final.
#' @param modelos_custom `data.frame` opcional com modelos extras.
#' @param mapa_variaveis Lista nomeada opcional para alias de colunas.
#'   Exemplo: `list(dap = c("dap", "d", "D"), ht = c("ht", "hm", "hc"))`.
#'
#' @return Lista com `ranking`, `modelos_ajustados` e `falhas`.
#' @export
ajustar_modelos_florestais <- function(data,
                                       resposta,
                                       alvo = c("todos", "volume", "biomassa"),
                                       criterio = c("aic", "bic", "rmse", "mae"),
                                       n_top = 30,
                                       modelos_custom = NULL,
                                       mapa_variaveis = list(
                                         dap = c("dap", "d", "D"),
                                         ht = c("ht", "h", "hm", "hc", "altura_comercial"),
                                         rho = c("rho", "densidade", "dens")
                                       )) {
  alvo <- match.arg(alvo)
  criterio <- match.arg(criterio)

  if (!is.data.frame(data)) {
    stop("`data` deve ser um data.frame.", call. = FALSE)
  }

  data_norm <- .normalizar_colunas_florestais(data, mapa_variaveis)

  if (!resposta %in% names(data_norm)) {
    stop("A coluna de resposta informada nao existe em `data`.", call. = FALSE)
  }

  catalogo <- catalogo_modelos_florestais(alvo)
  custom <- .validar_modelos_custom(modelos_custom)

  if (!is.null(custom)) {
    if (alvo != "todos") {
      custom <- custom[custom$alvo == alvo, , drop = FALSE]
    }
    if (nrow(custom) > 0) {
      faltantes <- setdiff(names(catalogo), names(custom))
      for (col in faltantes) {
        custom[[col]] <- ""
      }
      custom <- custom[, names(catalogo), drop = FALSE]
      catalogo <- rbind(catalogo, custom)
    }
  }

  metricas <- list()
  modelos <- list()
  falhas <- list()

  for (i in seq_len(nrow(catalogo))) {
    linha <- catalogo[i, ]
    vars <- strsplit(linha$variaveis, ",", fixed = TRUE)[[1]]
    vars <- trimws(vars)
    cols_necessarias <- unique(c(resposta, vars))

    if (!all(cols_necessarias %in% names(data_norm))) {
      falhas[[linha$id]] <- "Variaveis requeridas nao encontradas no conjunto de dados"
      next
    }

    dados_ok <- data_norm[stats::complete.cases(data_norm[, cols_necessarias, drop = FALSE]), cols_necessarias, drop = FALSE]

    if (nrow(dados_ok) < 5) {
      falhas[[linha$id]] <- "Poucas observacoes completas para ajuste"
      next
    }

    f_txt <- gsub("\\.y", resposta, linha$formula)
    f_obj <- stats::as.formula(f_txt)

    if (linha$tipo == "linear") {
      tentativa <- try(stats::lm(formula = f_obj, data = dados_ok), silent = TRUE)
    } else {
      start <- .parse_start(linha$start)
      tentativa <- try(
        stats::nls(
          formula = f_obj,
          data = dados_ok,
          start = start,
          control = stats::nls.control(warnOnly = TRUE)
        ),
        silent = TRUE
      )
    }

    if (inherits(tentativa, "try-error")) {
      falhas[[linha$id]] <- as.character(tentativa)
      next
    }

    y_obs_original <- dados_ok[[resposta]]
    formula_usa_log <- grepl("^\\s*log\\(", linha$formula)

    if (linha$tipo == "linear" && formula_usa_log) {
      y_pred_original <- .ajuste_linearizado_original(tentativa, y_obs_original)
      met <- avaliar_modelo(tentativa, y_true = y_obs_original, y_pred = y_pred_original)
    } else {
      met <- avaliar_modelo(tentativa)
    }

    metricas[[linha$id]] <- data.frame(
      id = linha$id,
      id_base = linha$id_base,
      nome = linha$nome,
      alvo = linha$alvo,
      tipo = linha$tipo,
      rmse = met$rmse,
      mae = met$mae,
      r2 = met$r2,
      r2_ajustado = met$r2_ajustado,
      aic = met$aic,
      bic = met$bic,
      stringsAsFactors = FALSE
    )
    modelos[[linha$id]] <- tentativa
  }

  if (length(metricas) == 0) {
    stop("Nenhum modelo do catalogo conseguiu ser ajustado com os dados informados.", call. = FALSE)
  }

  ranking <- do.call(rbind, metricas)
  ranking <- ranking[order(ranking[[criterio]], decreasing = FALSE), , drop = FALSE]

  if (n_top < nrow(ranking)) {
    ranking <- ranking[seq_len(n_top), , drop = FALSE]
  }

  list(
    ranking = ranking,
    modelos_ajustados = modelos,
    falhas = falhas
  )
}
