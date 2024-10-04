library(testthat)

# Teste: Função falha com coluna constante
test_that("Função falha com coluna constante", {
  matriz_constante <- matrix(c(1, 1, 1, 2, 2, 2), nrow = 3)
  expect_error(regressao_linear(matriz_constante, Y = c(1, 2, 2)),
               "O número de linhas em X deve ser igual ao comprimento de Y.")
})

# Teste: Função falha com X ou Y possuindo valores faltantes
test_that("Função falha com X ou Y possuindo valores faltantes", {
  matriz_constante <- matrix(c(1, 1, NA, 2, 2, 2), nrow = 3)
  expect_error(regressao_linear(matriz_constante, Y = c(1, 2, NA)),
               "X e Y não podem conter valores NA.")
})

# Teste: Função falha com X ou Y possuindo tamanhos diferentes
test_that("Função falha com X ou Y possuindo tamanhos diferentes", {
  matriz_constante <- matrix(c(1, 1, 1, 2, 2, 2), nrow = 3)
  expect_error(regressao_linear(matriz_constante, Y = c(1, 2)),
               "O número de linhas em X deve ser igual ao comprimento de Y.")
})

# Teste: Função falha com matriz sem posto completo
test_that("Função falha com matriz sem posto completo", {
  matriz_incompleta <- matrix(c(1, 2, 1, 2, 3, 6), nrow = 3)
  expect_error(regressao_linear(matriz_incompleta, Y = c(5, 6, 5)),
               "X deve ter pelo menos uma coluna de preditores.") # Substitua pela mensagem adequada.
})

# Teste: Resíduos são zero quando Y é perfeitamente descrito por Xβ
test_that("Resíduos são zero quando Y é perfeitamente descrito por Xβ", {
  X <- matrix(c(1, 2, 3), nrow = 3)
  Y <- c(5, 10, 15)
  resultado <- regressao_linear(X, Y)
  expect_equal(sum(resultado$residuos^2), 0)
})

# Teste: Intervalo de confiança dos coeficientes está correto
test_that("Intervalo de confiança dos coeficientes está correto", {
  X <- matrix(c(1, 2, 3), nrow = 3)
  Y <- c(5, 10, 15)
  resultado <- regressao_linear(X, Y)
  expect_true(all(resultado$IC[, 2] <= resultado$coeficientes & resultado$IC[, 3] >= resultado$coeficientes))
})

# Teste: Modelo ridge não falha
test_that("Modelo ridge não falha com dados válidos", {
  X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  Y <- c(1, 2, 3)
  resultado <- regressao_linear(X, Y, type = "ridge", lambda = 1)
  expect_s3_class(resultado, "list")
})

# Teste: Verifica se a estatística F e valor p estão corretos
test_that("Estatística F e valor p são calculados corretamente", {
  X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  Y <- c(1, 2, 3)
  resultado <- regressao_linear(X, Y)
  expect_true(resultado$teste_F$F_estatistica >= 0)
  expect_true(resultado$teste_F$valor_p >= 0 && resultado$teste_F$valor_p <= 1)
})

# Teste: Verifica se o modelo se ajusta corretamente com dados aleatórios
test_that("Modelo se ajusta corretamente com dados aleatórios", {
  set.seed(123)
  X <- matrix(rnorm(100), ncol = 2)
  beta <- c(1, 2)
  Y <- X %*% beta + rnorm(50)
  resultado <- regressao_linear(X, Y)
  expect_equal(nrow(resultado$IC), length(beta))
  expect_equal(nrow(resultado$coeficientes), length(beta))
})

# Teste: Verifica se o R² está entre 0 e 1
test_that("R² está entre 0 e 1", {
  X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  Y <- c(1, 2, 3)
  resultado <- regressao_linear(X, Y)
  expect_true(resultado$R2 >= 0 && resultado$R2 <= 1)
})

# Teste: Verifica se a função lida com dados sem variação
test_that("Função falha com Y sem variação", {
  X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  Y <- c(1, 1, 1)
  expect_error(regressao_linear(X, Y),
               "X deve ter pelo menos uma coluna de preditores.") # Substitua pela mensagem adequada.
})

# Teste: Verifica a saída com dados que têm uma coluna de preditor
test_that("Saída correta com uma única coluna de preditor", {
  X <- matrix(c(1, 2, 3), ncol = 1)
  Y <- c(1, 2, 3)
  resultado <- regressao_linear(X, Y)
  expect_equal(length(resultado$coeficientes), 2)  # Incluindo o intercepto
})


# n <- 100
# dados <- data.frame(idade = sample(18:65, n, replace = TRUE),
#                     renda_anual = round(rgamma(n, scale = 1200, shape = 12), 3),
#                     ocupacao = sample(c("Empregado", "Desempregado", "Autônomo"), n, replace = TRUE),
#                     sexo = sample(c("Homem", "Mulher"), n, replace = TRUE),
#                     altura = round(rnorm(n, mean = 173, sd = 6), 3),
#                     anos_exp = sample(18:35, n, replace = TRUE) - sample(0:18, n, replace = TRUE),
#                     satisfacao =  sample(seq(0, 10, by = 0.1), n, replace = TRUE))

