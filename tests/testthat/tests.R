library(testthat)

set.seed(123)
X <- matrix(rnorm(100), ncol = 2)
beta <- c(1, 2)
Y <- as.vector(X %*% beta + rnorm(50))

test_that("Função falha com dados inválidos", {
  matriz_constante <- matrix(c(1, 1, NA, 2, 2, 2), nrow = 3)
  expect_error(regressao_linear(matriz_constante, Y = c(1, 2, NA)),
               "X e Y não podem conter valores NA.", fixed = TRUE)

  expect_error(regressao_linear(matrix(1:6, nrow = 3), Y = c(1, 2)),
               "O número de linhas em X deve ser igual ao comprimento de Y.", fixed = TRUE)

  # Teste: matriz sem posto completo
  matriz_incompleta <- matrix(c(1, 2, 3, 2, 4, 6), nrow = 3)
  expect_error(regressao_linear(matriz_incompleta, Y = c(5, 6, 5)))
})

test_that("Resultados da regressão linear são válidos", {

  # Resíduos são zero quando Y é perfeitamente descrito por Xβ
  X_perfeito <- matrix(c(1, 2, 3), nrow = 3)
  Y_perfeito <- c(5, 10, 15)
  resultado <- regressao_linear(X_perfeito, Y_perfeito)
  expect_equal(sum(resultado$residuos^2), 0)

  # Intervalo de confiança dos coeficientes
  expect_true(all(resultado$IC[, 2] <= resultado$coeficientes & resultado$IC[, 3] >= resultado$coeficientes))

  # Verificação da estatística F e valor p
  X <- matrix(rnorm(100), ncol = 2)
  beta <- c(1, 2)
  Y <- as.vector(X %*% beta + rnorm(50))

  resultado_F <- regressao_linear(X, Y)
  expect_true(resultado_F$teste_F$F_estatistica >= 0)
  expect_true(resultado_F$teste_F$valor_p >= 0 && resultado_F$teste_F$valor_p <= 1)

  # Modelo se ajusta corretamente com dados aleatórios
  resultado_aleatorio <- regressao_linear(X, Y, intercepto = FALSE)
  expect_equal(nrow(resultado_aleatorio$IC), length(beta))
  expect_equal(nrow(resultado_aleatorio$coeficientes), length(beta))
})


test_that("Modelos específicos funcionam corretamente", {
  # Modelo ridge não falha
  modelo_ridge <- regressao_linear(X, Y, type = "ridge", lambda = 1)
  expect_s3_class(modelo_ridge, "regressao_resultado")

  # Função falha com argumento de tipo não adequado
  expect_error(regressao_linear(X, Y, type = "a"),
               "Tipo de modelo não reconhecido. Use 'lm' ou 'ridge'.")
})

test_that("predicao funciona corretamente", {
  modelo_lm <- regressao_linear(X = as.matrix(mtcars[1:25, c("mpg", "hp")]) , Y = as.vector(mtcars[1:25, c("wt")]))
  X_novo <- as.matrix(mtcars[26:30, c("mpg", "hp")])

  expect_type(predicao(modelo_lm, X_novo), "double")

  # Verificação de erro em casos inválidos
  expect_error(predicao(list(), matrix(1:3, nrow = 1)),
               "O argumento 'modelo' deve ser uma lista resultante da função 'regressao_linear'.", fixed = TRUE)

  X_novo_invalido <- data.frame(mpg = c("a", "b"), hp = c(100, 110), wt = c(2.5, 2.6))
  expect_error(predicao(modelo_lm, X_novo_invalido),
               "X_novo deve ser uma matriz numérica.", fixed = TRUE)

  X_novo_incompatível <- as.matrix(mtcars[1:5, c("mpg", "wt", "hp")])
  expect_error(predicao(modelo_lm, X_novo_incompatível),
               "O número de colunas em X_novo deve ser compatível com os coeficientes do modelo.")

  modelo_lm_intercepto <- regressao_linear(X = as.matrix(mtcars[1:25, c("mpg", "hp")]) , Y = as.vector(mtcars[1:25, c("wt")]), intercepto = FALSE)
  X_novo <- as.matrix(mtcars[26:30, c("mpg", "hp")])
  expect_equal(length(predicao(modelo_lm_intercepto, X_novo)), nrow(X_novo))
})

test_that("analise_residuos retorna resultados corretos", {
  X <- matrix(rnorm(100), ncol = 2)
  beta <- c(1, 2)
  Y <- as.vector(X %*% beta + rnorm(50))
  resultados <- regressao_linear(X, Y)
  graficos_resultados <- analise_residuos(resultados)

  expect_type(graficos_resultados, "list")
  expect_true("shapiro_wilks" %in% names(graficos_resultados))
  expect_s3_class(graficos_resultados$shapiro_wilks, "htest")

  expect_s3_class(graficos_resultados$grafico_res_ajustados, "gg")
  expect_s3_class(graficos_resultados$grafico_histograma_res, "gg")
  expect_s3_class(graficos_resultados$grafico_qq_res, "gg")
})

test_that("analise_residuos falha com entrada inválida", {
  expect_error(analise_residuos(NULL),
               "O argumento 'modelo' deve ser uma lista resultante da função 'regressao_linear'.", fixed = TRUE)
})

# n <- 100
# dados <- data.frame(idade = sample(18:65, n, replace = TRUE),
#                     renda_anual = round(rgamma(n, scale = 1200, shape = 12), 3),
#                     ocupacao = sample(c("Empregado", "Desempregado", "Autônomo"), n, replace = TRUE),
#                     sexo = sample(c("Homem", "Mulher"), n, replace = TRUE),
#                     altura = round(rnorm(n, mean = 173, sd = 6), 3),
#                     anos_exp = sample(18:35, n, replace = TRUE) - sample(0:18, n, replace = TRUE),
#                     satisfacao =  sample(seq(0, 10, by = 0.1), n, replace = TRUE))

