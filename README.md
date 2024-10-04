---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# regressao

<!-- badges: start -->
<!-- badges: end -->

regressao é um pacote R desenvolvido para facilitar o ajuste de modelos de regressão linear e ridge, bem como realizar análises estatísticas dos resíduos dos modelos. Ele inclui funções para ajustar modelos de regressão, gerar predições e analisar a qualidade do ajuste por meio de gráficos de resíduos. Além disso, um conjunto de dados de exemplo é fornecido para que os usuários possam testar e explorar as funcionalidades do pacote.

## Instalação

Você pode instalar a versão de desenvolvimento do regressao como:

``` r
# install.packages("devtools")
devtools::install_github("usuario/regressao")
```

## Funcionalidades

- **`regressao_linear()`**: Ajusta um modelo de regressão linear simples ou ridge. Retorna os coeficientes, resíduos, valores preditos e um gráfico de valores observados vs. preditos.
- **`analise_residuos()`**: Gera gráficos para análise dos resíduos de um modelo de regressão.


## Exemplos

Este é um exemplo básico que mostra como resolver um problema comum de ajuste de modelo de regressão linear e ridge:

```{r example}
library(regressao)
# Criar a matriz de preditores e vetor de respostas
X <- as.matrix(human[, c("idade", "renda_anual", "altura", "anos_exp")])
X <- cbind(1, X)  # Adiciona uma coluna de 1 para o intercepto
Y <- human$satisfacao

# Ajustar um modelo de regressão linear
modelo_lm <- regressao_linear(X, Y, type = "lm")
```

Você também pode gerar gráficos, como o gráfico "Observado vs Predito":

```{r}
print(modelo_lm$obs_vs_pred)
```

O pacote inclui um conjunto de dados simulado chamado human, que contém informações sobre indivíduos, incluindo idade, renda anual, ocupação, sexo, altura, anos de experiência e satisfação.

```{r}
head(human)
```

## Contribuindo

Se você tiver sugestões ou encontrar problemas, sinta-se à vontade para abrir uma *issue* ou enviar um *pull request*.

## Licença
Este pacote é distribuído sob a licença MIT.
