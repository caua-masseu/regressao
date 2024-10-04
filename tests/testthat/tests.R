test_that("Função falha com coluna constante", {
  matriz_constante <- matrix(c(1, 1, 1, 2, 2, 2), nrow = 3)
  expect_error(regressao_linear(matriz_constante, Y = c(1,2,2)))
})

test_that("Função falha com X ou Y possuindo valores faltantes", {
  matriz_constante <- matrix(c(1, 1, NA, 2, 2, 2), nrow = 3)
  expect_error(regressao_linear(matriz_constante, Y = c(1,2,NA)))
})

test_that("Função falha com X ou Y possuindo tamanhos diferentes", {
  matriz_constante <- matrix(c(1, 1, 1, 2, 2, 2), nrow = 3)
  expect_error(regressao_linear(matriz_constante, Y = c(1,2)))
})

# test_that("Função falha com matriz sem posto completo", {
#   matriz_incompleta <- matrix(c(1, 2, 1, 2, 3, 6), nrow = 3)
#   expect_error(regressao_linear(matriz_incompleta, Y = c(5,6,5)))
# })

test_that("Resíduos são zero quando Y é perfeitamente descrito por Xβ", {
  X <- matrix(c(1, 2, 3), nrow = 3)
  Y <- c(5, 10, 15)
  resultado <- regressao_linear(X, Y)
  expect_equal(sum(resultado$residuos^2), 0)
})

# n <- 100
# dados <- data.frame(idade = sample(18:65, n, replace = TRUE),
#                     renda_anual = round(rgamma(n, scale = 1200, shape = 12), 3),
#                     ocupacao = sample(c("Empregado", "Desempregado", "Autônomo"), n, replace = TRUE),
#                     sexo = sample(c("Homem", "Mulher"), n, replace = TRUE),
#                     altura = round(rnorm(n, mean = 173, sd = 6), 3),
#                     anos_exp = sample(18:35, n, replace = TRUE) - sample(0:18, n, replace = TRUE),
#                     satisfacao =  sample(seq(0, 10, by = 0.1), n, replace = TRUE))

