
library(pacman)

pacman::p_load(dplyr, car, lmtest, QuantPsyc, psych,scatterplot3d)

dados <- read.csv('tabela.csv')
View(dados)
glimpse(dados)


## definição do modelo observando Valor Unitario
mod <- lm(Valor.Unitário ~ Área.total + Desempenho.Agropecuário + Aptidão + Proximidade.com.a.rodovia, dados)


##Define a grid dos graficos
par(mfrow=c(2,2))
plot(mod)

#par(mfrow=c(1,1))

## Normalidade dos residuos

## distribuição dos dos dados = normal -> p-value > 0,05 (5%)
## distribuição dos dos dados != normal -> p-value <= 0,05 (5%)
shapiro.test(mod$residuals)

# neste caso a distribuição é normal

## Outlier dos residuos por meio da função summary, que checa os maiores e menores valores
## Vamos checar por meio do residuo padronizado
summary(rstandard(mod))

# como a mediana esta proxima de 0, não aparenta ter nenhum outlier

## Homocedasticidade

bptest(mod)

# multilinearidade, checar se há relação entre as variaveis independentes, os
# valores de correlação não podem passar de 0.9

pairs.panels(dados)

# Analise do modelo

summary(mod)

## como f-statistic foi menor que 5%, esse modelo possui uma maior capacidade de predição

# normalizando os coeficientes

lm.beta(mod)
## dado o resultado da normalização, podemos observar uma maior relação entre a 
## aptidão e o desempenho com o valor do que as outras variaveis


# Checar o Intervalo de Confiança 95%
confint(mod)


