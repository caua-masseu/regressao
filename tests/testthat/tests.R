library(testthat)

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
  expect_error(regressao_linear(matriz_incompleta, Y = c(5, 6, 5)))
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
  expect_error(regressao_linear(X, Y)) # Substitua pela mensagem adequada.
})

# Teste: Verifica a saída com dados que têm uma coluna de preditor
test_that("Saída correta com uma única coluna de preditor", {
  X <- matrix(c(1, 2, 3), ncol = 1)
  Y <- c(1, 2, 3)
  resultado <- regressao_linear(X, Y)
  expect_equal(length(resultado$coeficientes), 2)
})

# Teste: Função falha com argumento tipo nao adequado
test_that("Função falha com argumento tipo nao adequado", {
  X <- matrix(c(5, 6, 8), ncol = 1)
  Y <- c(1, 2, 3)
  resultado <-
  expect_error(regressao_linear(X, Y, type = "a"))
})


# Criar um exemplo de modelo de regressão
X <- as.matrix(mtcars[, c("mpg", "hp", "wt")])
Y <- mtcars$drat
modelo_lm <- regressao_linear(X, Y, type = "lm")

# Testes para a função `predicao`
test_that("predicao retorna um vetor numérico ao receber um modelo válido e dados novos", {
  X_novo <- as.matrix(mtcars[1:5, c("mpg", "hp", "wt")])
  Y_pred_novo <- predicao(modelo_lm, X_novo)
  expect_type(Y_pred_novo, "double") # Verifica se o retorno é numérico
  expect_equal(length(Y_pred_novo), nrow(X_novo)) # Verifica se o comprimento do vetor de predição corresponde ao número de linhas de X_novo
})

test_that("predicao falha com modelo inválido", {
  expect_error(predicao(list(), matrix(1:3, nrow = 1)),
               "O argumento 'modelo' deve ser uma lista resultante da função 'regressao_linear'.")

  expect_error(predicao(list(coeficientes = c(1, 2)), matrix(1:3, nrow = 1)),
               "O argumento 'modelo' deve ser uma lista resultante da função 'regressao_linear'.")
})

test_that("predicao falha com X_novo não numérico", {
  X_novo_invalido <- data.frame(mpg = c(21, 22), hp = c(100, 110), wt = c(2.5, 2.6))
  expect_error(predicao(modelo_lm, X_novo_invalido),
               "X_novo deve ser uma matriz numérica.")
})

test_that("predicao falha quando X_novo não tem colunas compatíveis", {
  X_novo_incompatível <- as.matrix(mtcars[1:5, c("mpg", "hp")])
  expect_error(predicao(modelo_lm, X_novo_incompatível),
               "O número de colunas em X_novo deve ser compatível com os coeficientes do modelo.")
})

test_that("predicao inclui intercepto quando necessário", {
  modelo_lm_intercepto <- regressao_linear(X, Y, type = "lm", intercepto = TRUE)
  X_novo_intercepto <- as.matrix(mtcars[1:5, c("mpg", "hp", "wt")])
  Y_pred_novo_intercepto <- predicao(modelo_lm_intercepto, X_novo_intercepto)

  expect_equal(length(Y_pred_novo_intercepto), nrow(X_novo_intercepto))
})

test_that("predicao não altera o resultado quando o intercepto não é considerado", {
  modelo_lm_sem_intercepto <- regressao_linear(X, Y, type = "lm", intercepto = FALSE)
  X_novo_sem_intercepto <- as.matrix(mtcars[1:5, c("mpg", "hp", "wt")])
  Y_pred_novo_sem_intercepto <- predicao(modelo_lm_sem_intercepto, X_novo_sem_intercepto)

  expect_equal(length(Y_pred_novo_sem_intercepto), nrow(X_novo_sem_intercepto))
})


# Criar um exemplo de modelo de regressão
set.seed(123)  # Para reprodutibilidade
X <- as.matrix(mtcars[, c("mpg", "hp", "wt")])
Y <- mtcars$drat
modelo_lm <- regressao_linear(X, Y, type = "lm")  # Supondo que essa função já esteja definida

# Testes para a função `analise_residuos`
test_that("analise_residuos retorna uma lista com os gráficos e o resultado do teste de Shapiro-Wilk", {
  resultados <- list(
    coeficientes = modelo_lm$coeficientes,
    residuos = modelo_lm$residuos,
    valores_preditos = modelo_lm$valores_preditos
  )

  graficos_resultados <- analise_residuos(resultados)

  # Verifica se o resultado é uma lista
  expect_type(graficos_resultados, "list")

  # Verifica se a lista contém os gráficos e o resultado do teste de Shapiro-Wilk
  expect_true("grafico_res_ajustados" %in% names(graficos_resultados))
  expect_true("grafico_histograma_res" %in% names(graficos_resultados))
  expect_true("grafico_qq_res" %in% names(graficos_resultados))
  expect_true("grafico_acf_residuos" %in% names(graficos_resultados))
  expect_true("shapiro_wilks" %in% names(graficos_resultados))

  # Verifica se o resultado do teste de Shapiro-Wilk é um objeto de teste
  expect_s3_class(graficos_resultados$shapiro_wilks, "htest")
})

test_that("analise_residuos falha com entrada inválida", {
  expect_error(analise_residuos(NULL),
               "resultados deve ser uma lista como resultado da funcao regressao linear.")

  expect_error(analise_residuos(list(coeficientes = c(1, 2, 3))),
               "resultados deve ser uma lista como resultado da funcao regressao linear.")
})

test_that("analise_residuos cria gráficos com dados válidos", {
  resultados <- list(
    coeficientes = modelo_lm$coeficientes,
    residuos = modelo_lm$residuos,
    valores_preditos = modelo_lm$valores_preditos
  )

  graficos_resultados <- analise_residuos(resultados)

  # Verifica se os gráficos podem ser gerados (não testa a visualização)
  expect_s3_class(graficos_resultados$grafico_res_ajustados, "gg")
  expect_s3_class(graficos_resultados$grafico_histograma_res, "gg")
  expect_s3_class(graficos_resultados$grafico_qq_res, "gg")
  expect_s3_class(graficos_resultados$grafico_acf_residuos, "gg")
})





# n <- 100
# dados <- data.frame(idade = sample(18:65, n, replace = TRUE),
#                     renda_anual = round(rgamma(n, scale = 1200, shape = 12), 3),
#                     ocupacao = sample(c("Empregado", "Desempregado", "Autônomo"), n, replace = TRUE),
#                     sexo = sample(c("Homem", "Mulher"), n, replace = TRUE),
#                     altura = round(rnorm(n, mean = 173, sd = 6), 3),
#                     anos_exp = sample(18:35, n, replace = TRUE) - sample(0:18, n, replace = TRUE),
#                     satisfacao =  sample(seq(0, 10, by = 0.1), n, replace = TRUE))

