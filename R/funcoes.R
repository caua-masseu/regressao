#' Ajustar um modelo de regressão linear
#'
#' Esta função ajusta um modelo de regressão linear ou ridge a um conjunto de dados, permitindo a
#' análise e previsão de uma variável resposta com base em variáveis preditoras.
#'
#' @param X Uma matriz de variáveis preditoras (apenas valores numéricos).
#' @param Y Um vetor da variável resposta.
#' @param type Tipo de modelo a ser ajustado: "lm" para regressão linear simples ou "ridge"
#' para regressão ridge. O valor padrão é "lm".
#' @param lambda Parâmetro de penalização para a regressão ridge. O valor padrão é 0.5.
#' @param intercepto Lógica indicando se deve ser adicionado um intercepto à matriz X.
#' TRUE para incluir o intercepto, FALSE caso contrário (padrão é TRUE).
#' @param nivel_confianca Nível de significância a ser utilizado no cálculo do intervalo de
#' confiança para os coeficientes. O padrão é 0.95.
#'
#' @return Uma lista contendo:
#' \item{coeficientes}{Coeficientes estimados do modelo.}
#' \item{residuos}{Resíduos do ajuste.}
#' \item{valores_preditos}{Valores preditos pelo modelo.}
#' \item{obs_vs_pred}{Gráfico de valores preditos vs. valores observados.}
#' \item{IC}{Intervalo de confiança dos coeficientes calculados.}
#' \item{R2}{Coeficiente de determinação do modelo.}
#' \item{teste_F}{Resultados do teste F, incluindo estatística F e valor p.}
#'
#' @importFrom ggplot2 ggplot geom_point labs
#' @examples
#' # Criar a matriz de preditores e vetor de respostas
#' X <- as.matrix(human[, c("idade", "renda_anual", "altura", "anos_exp")])
#' Y <- dados$satisfacao
#'
#' # Ajustar um modelo de regressão linear
#' modelo_lm <- regressao_linear(X, Y, type = "lm", intercepto = TRUE)
#'
#' # Ajustar um modelo de regressão ridge
#' modelo_ridge <- regressao_linear(X, Y, type = "ridge", lambda = 0.5)
#'
#' # Exibir os coeficientes
#' print("Coeficientes do modelo linear:")
#' print(modelo_lm$coeficientes)
#' print("Coeficientes do modelo ridge:")
#' print(modelo_ridge$coeficientes)
#' @export
regressao_linear <- function(X, Y, type = "lm", lambda = 0.5, intercepto = TRUE, nivel_confianca = 0.95) {

  if (!is.matrix(X) || !is.numeric(X) || !is.numeric(Y)) {
    stop("X deve ser uma matriz numérica e Y deve ser um vetor numérico.")
  }
  if (nrow(X) != length(Y)) {
    stop("O número de linhas em X deve ser igual ao comprimento de Y.")
  }
  if (!is.vector(Y) || length(dim(Y)) > 1) {
    stop("Y deve ser um vetor unidimensional.")
  }
  if (nrow(X) == 0 || ncol(X) == 0) {
    stop("X não pode ser vazio.")
  }
  if (any(is.na(X)) || any(is.na(Y))) {
    stop("X e Y não podem conter valores NA.")
  }
  if (ncol(X) < 1) {
    stop("X deve ter pelo menos uma coluna de preditores.")
  }
  # if(rank(X) != ncol(X)){
  #  stop("X deve ter posto completo")
  # }

  if(intercepto){
    X <- cbind(1, X)
  }
  if (type == "lm") {
    coeficientes <- solve(t(X) %*% X) %*% t(X) %*% Y
  } else if (type == "ridge") {
    coeficientes <- solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% Y
  } else {
    stop("Tipo de modelo não reconhecido. Use 'lm' ou 'ridge'.")
  }

  Y_pred <- X %*% coeficientes
  residuos <- Y - Y_pred

  grafico_observado_vs_predito <- ggplot2::ggplot(data = data.frame(Y, Y_pred), ggplot2::aes(x = Y, y = Y_pred)) +
    ggplot2::geom_point(color = "blue") + ggplot2::labs(title = "Observado vs Predito", x = "Valores Observados", y = "Valores Preditos")

  SSE <- sum(residuos^2)
  SST <- sum((Y - mean(Y))^2)
  R2 <- 1 - (SSE / SST)

  n <- nrow(X)
  p <- ncol(X)
  sigma2 <- SSE / (n - p)
  var_beta <- sigma2 * solve(t(X) %*% X)
  erro_padrao <- sqrt(diag(var_beta))

  alfa <- 1 - nivel_confianca
  t_critico <- qt(1 - alfa / 2, df = n - p)

  limite_inferior <- coeficientes - t_critico * erro_padrao
  limite_superior <- coeficientes + t_critico * erro_padrao
  intervalo_confianca <- cbind(coeficientes, limite_inferior, limite_superior)
  colnames(intervalo_confianca) <- c("Coeficientes", "Limite Inferior", "Limite Superior")

  F_estatistica <- ((SST - SSE) / (p - 1)) / (SSE / (n - p))
  valor_p <- 1 - pf(F_estatistica, df1 = p - 1, df2 = n - p)

  teste_F_resultado <- list(F_estatistica = F_estatistica, valor_p = valor_p)

  return(list(coeficientes = coeficientes,
              residuos = residuos,
              valores_preditos = Y_pred,
              obs_vs_pred = grafico_observado_vs_predito,
              IC = intervalo_confianca,
              R2 = R2,
              teste_F = teste_F_resultado, intercepto = intercepto))
}


#' Predizer novos valores usando um modelo de regressão ajustado
#'
#' @param modelo Uma lista contendo os resultados do modelo ajustado, como os coeficientes.
#' Deve ser o resultado de uma chamada à função `regressao_linear`.
#' @param X_novo Uma matriz de novas variáveis preditoras para as quais se deseja fazer previsões.
#' A matriz deve ter o mesmo número de colunas e a mesma ordem que o modelo ajustado.
#' @return Um vetor contendo os valores preditos.
#' @examples
#'
#' # Ajustar um modelo de regressão linear
#' modelo_lm <- regressao_linear(X, Y, type = "lm")
#'
#' # Criar uma nova matriz de preditores para predição
#' X_novo <- as.matrix(novos_dados[, c("idade", "renda_anual", "altura", "anos_exp")])
#'
#' # Fazer predições para os novos dados
#' Y_pred_novo <- predicao(modelo_lm, X_novo)
#'
#' @export
predicao <- function(modelo, X_novo) {
  if (!is.list(modelo) || !all(c("coeficientes") %in% names(modelo))) {
    stop("O argumento 'modelo' deve ser uma lista resultante da função 'regressao_linear'.")
  }
  if (!is.matrix(X_novo) || !is.numeric(X_novo)) {
    stop("X_novo deve ser uma matriz numérica.")
  }
  if (ncol(X_novo) + as.numeric(modelo$intercepto) != length(modelo$coeficientes)) {
    stop("O número de colunas em X_novo deve ser compatível com os coeficientes do modelo.")
  }

  if (modelo$intercepto) {
    X_novo <- cbind(1, X_novo)
  }
  Y_pred_novo <- X_novo %*% modelo$coeficientes
  return(as.vector(Y_pred_novo))
}


#' Análise de Resíduos de um Modelo de Regressão
#'
#' Esta função gera gráficos para analisar os resíduos de um modelo de regressão linear.
#' Ela plota o gráfico de resíduos vs. valores ajustados, um histograma dos resíduos,
#' um gráfico Q-Q para verificar a normalidade dos resíduos, e um gráfico de autocorrelação
#' dos resíduos.
#'
#' @param resultados Uma lista contendo os resultados do ajuste do modelo.
#' Deve incluir:
#' \itemize{
#'   \item \code{coeficientes}: vetor de coeficientes do modelo.
#'   \item \code{residuos}: vetor de resíduos do modelo.
#'   \item \code{valores_preditos}: vetor de valores preditos pelo modelo.
#' }
#'
#' @return Uma lista de gráficos gerados, contendo:
#' \itemize{
#'   \item \code{grafico_res_ajustados}: gráfico de resíduos vs. valores ajustados.
#'   \item \code{grafico_histograma_res}: histograma dos resíduos.
#'   \item \code{grafico_qq_res}: gráfico Q-Q dos resíduos.
#'   \item \code{grafico_acf_residuos}: gráfico de autocorrelação dos resíduos.
#'   \item \code{shapiro_wilks}: resultado do teste de Shapiro-Wilk para testar a normalidade dos resíduos.
#' }
#'
#' @importFrom ggplot2 ggplot geom_point geom_hline labs theme_bw geom_histogram stat_qq_line geom_bar
#' @examples
#' # Ajustar o modelo
#' resultado <- regressao_linear(X, Y)
#'
#' # Analisar os resíduos
#' graficos_resultados <- analise_residuos(resultado)
#'
#' # Exibir gráficos dos resíduos
#' print(graficos_resultados$grafico_res_ajustados)
#' print(graficos_resultados$grafico_histograma_res)
#' print(graficos_resultados$grafico_qq_res)
#' print(graficos_resultados$grafico_acf_residuos)
#' @export
analise_residuos <- function(resultados) {
  if (!is.list(resultados)) {
    stop("resultados deve ser uma lista como resultado da funcao regressao linear.")
  }

  # Extrai os componentes da lista
  coeficientes <- resultados$coeficientes
  residuos <- resultados$residuos
  Y_pred <- resultados$valores_preditos

  graficos <- list()

  # Gráfico de Resíduos vs Valores Ajustados
  graficos$grafico_res_ajustados <- ggplot2::ggplot(data = data.frame(Y_pred, residuos), ggplot2::aes(x = Y_pred, y = residuos)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Gráfico de Resíduos vs. Ajustados",
                  x = "Valores Ajustados",
                  y = "Resíduos") +
    ggplot2::theme_bw()

  # Histograma dos Resíduos
  graficos$grafico_histograma_res <- ggplot2::ggplot(data = data.frame(residuos), ggplot2::aes(x = residuos)) +
    ggplot2::geom_histogram(binwidth = 0.5, color = "black", fill = "blue") +
    ggplot2::labs(title = "Histograma dos Resíduos",
                  x = "Resíduos") +
    ggplot2::theme_bw()

  # Gráfico Q-Q dos Resíduos
  graficos$grafico_qq_res <- ggplot2::ggplot(data = data.frame(residuos), ggplot2::aes(sample = residuos)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Gráfico Q-Q da Normalidade dos Resíduos") +
    ggplot2::theme_bw()

  # Gráfico de Autocorrelação dos Resíduos
  acf_data <- acf(residuos, plot = FALSE, lag.max = 30)
  acf_df <- data.frame(
    Lag = acf_data$lag[-1],
    ACF = acf_data$acf[-1]
  )

  # Calcula os limites de confiança
  n <- length(residuos)
  ci_upper <- 1.96 / sqrt(n)
  ci_lower <- -1.96 / sqrt(n)

  graficos$grafico_acf_residuos <- ggplot2::ggplot(acf_df, ggplot2::aes(x = Lag, y = ACF)) +
    ggplot2::geom_bar(stat = "identity", fill = "blue", color = "black", width = 0.7) +
    ggplot2::labs(title = "Autocorrelação dos Resíduos",
                  x = "Lag",
                  y = "Autocorrelação") +
    ggplot2::geom_hline(yintercept = ci_upper, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = ci_lower, linetype = "dashed", color = "red") +
    ggplot2::theme_bw()

  # Teste de Shapiro-Wilk para normalidade
  graficos$shapiro_wilks <- shapiro.test(residuos)

  return(graficos)
}


#' Conjunto de Dados Exemplo
#'
#' Um conjunto de dados simulado contendo informações sobre indivíduos,
#' incluindo idade, renda anual, ocupação, sexo, altura, anos de experiência
#' e satisfação. Este conjunto de dados pode ser usado para ajustar modelos
#' de regressão e realizar análises estatísticas.
#'
#' @format Um data frame com 100 observações e 7 variáveis:
#' \describe{
#'   \item{idade}{Idade do indivíduo (numérico, entre 18 e 65).}
#'   \item{renda_anual}{Renda anual do indivíduo (numérico, distribuído conforme uma distribuição gamma).}
#'   \item{ocupacao}{Tipo de ocupação do indivíduo (categórico: "Empregado", "Desempregado", "Autônomo").}
#'   \item{sexo}{Sexo do indivíduo (categórico: "Homem", "Mulher").}
#'   \item{altura}{Altura do indivíduo em centímetros (numérico, distribuído normalmente).}
#'   \item{anos_exp}{Anos de experiência do indivíduo (numérico, valor entre 0 e 17).}
#'   \item{satisfacao}{Nível de satisfação do indivíduo (numérico, varia de 0 a 10).}
#' }
#'
#' @source Arquivo gerado aleatoriamente com a função `sample`, `rgamma` e `rnorm`.
"human"



