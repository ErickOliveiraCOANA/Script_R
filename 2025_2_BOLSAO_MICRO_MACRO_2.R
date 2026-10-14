# Instalando pacotes necessários
if(!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
if(!require(car)) install.packages("car", dependencies=TRUE)
if(!require(readxl)) install.packages("readxl", dependencies=TRUE)
if(!require(AgroR)) install.packages("AgroR", dependencies=TRUE)

library(AgroR)
library(tidyverse)
library(car)
library(readxl)


dados <- read_excel("R:/Controle de Qualidade/TESTES/2025/BOLSÃO MICROPERFURADO  E MACRO - MARÇO 2025/DADOS.xlsx", sheet = "ESTATÍSTICA - CONDEN.")

# Convertendo variáveis categóricas
dados$BOLSÃO <- as.factor(dados$BOLSÃO)
dados$POSIÇÃO <- as.factor(dados$POSIÇÃO)
dados$CONDENSAÇÃO <- as.factor(dados$CONDENSAÇÃO)  # Importante para regressão logística

# 📌 2. Teste Qui-Quadrado para associação entre variáveis categóricas
tabela_contingencia <- table(dados$BOLSÃO, dados$CONDENSAÇÃO)
qui_quadrado_bolsao <- chisq.test(tabela_contingencia)
print(qui_quadrado_bolsao)

tabela_contingencia2 <- table(dados$POSIÇÃO, dados$CONDENSAÇÃO)
qui_quadrado_posicao <- chisq.test(tabela_contingencia2)
print(qui_quadrado_posicao)

# Se houver células com frequência < 5, use o Teste Exato de Fisher
if(any(qui_quadrado_bolsao$expected < 5)){
  fisher_bolsao <- fisher.test(tabela_contingencia)
  print(fisher_bolsao)
}
if(any(qui_quadrado_posicao$expected < 5)){
  fisher_posicao <- fisher.test(tabela_contingencia2)
  print(fisher_posicao)
}

# 📌 3. Regressão Logística Binária
modelo_logistico <- glm(CONDENSAÇÃO ~ BOLSÃO * POSIÇÃO, data = dados, family = binomial)
summary(modelo_logistico)

# 📌 4. Obter razões de chances (Odds Ratio)
exp(cbind(OddsRatio = coef(modelo_logistico), confint(modelo_logistico)))


anova(modelo_logistico, test="Chisq")
