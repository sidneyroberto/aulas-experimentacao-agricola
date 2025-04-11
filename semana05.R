# Começando com Delineamento por Quadrado Latino

if (!require("ExpDes.pt")) install.packages("ExpDes.pt", repos = "http://cran.rstudio.com/")
library(ExpDes.pt)

boxplot(dados_producao$Producao ~ dados_producao$Bioestimulante)
anova = aov(dados_producao$Producao ~ dados_producao$Umidade+dados_producao$Argila+dados_producao$Bioestimulante)
summary(anova)

erros = residuals(anova)
erros

hist(erros)
shapiro.test(erros)

bartlett.test(erros ~ dados_producao$Bioestimulante)

dql(
  trat=dados_producao$Bioestimulante,
  linha=dados_producao$Umidade,
  coluna=dados_producao$Argila,
  resp=dados_producao$Producao,
  quali = TRUE,
  mcomp = "tukey",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#-------------------------------------------------------------------------------
boxplot(dados_sacas_hectare$Sacas_Hectare ~ dados_sacas_hectare$Especie)
anova = aov(dados_sacas_hectare$Sacas_Hectare ~ dados_sacas_hectare$Textura+dados_sacas_hectare$Umidade+dados_sacas_hectare$Especie)
summary(anova)

erros = residuals(anova)
erros

hist(erros)
shapiro.test(erros)

bartlett.test(erros ~ dados_sacas_hectare$Especie)

# Pr>Fc deve ser menor que 0.05 para que as médias sejam consideradas diferentes
dql(
  trat=dados_sacas_hectare$Especie,
  linha=dados_sacas_hectare$Textura,
  coluna=dados_sacas_hectare$Umidade,
  resp=dados_sacas_hectare$Sacas_Hectare,
  quali = TRUE,
  mcomp = "tukey",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#-------------------------------------------------------------------------------
# Agora, Regressão Linear
#-------------------------------------------------------------------------------

# Calcula a correlação entre as variáveis
cor(dados_massa_altura$`Massa(kg)`, dados_massa_altura$`Altura (cm)`)

# Calcula a correlação, porém dando detalhes extras
cor.test(dados_massa_altura$`Massa(kg)`, dados_massa_altura$`Altura (cm)`)

# Exibe o gráfico, onde o primeiro argumento é o eixo x e o segundo o eixo y
plot(dados_massa_altura$`Massa(kg)`, dados_massa_altura$`Altura (cm)`)

# Realiza a análise de regressão linear
regressao = lm(dados_massa_altura$`Altura (cm)` ~ dados_massa_altura$`Massa(kg)`)
summary(regressao)

# LEMBRETE: o somatório dos erros deve ser igual ou próximo a 0

erros = regressao$residuals
sum(erros)
sd(erros)

# Testando a normalidade dos dados
shapiro.test(erros)

if (!require("lmtest")) install.packages("lmtest", repos = "http://cran.rstudio.com/")
library(lmtest)

# Se p-value > 0.05, logo os meus resíduos são homocedásticos ao longo do eixo X
gqtest(erros ~ dados_massa_altura$`Altura (cm)`)

if (!require("ggplot2")) install.packages("ggplot2", repos = "http://cran.rstudio.com/")
library(ggplot2)

ggplot(dados_massa_altura, aes(x = dados_massa_altura$`Massa(kg)`, y = dados_massa_altura$`Altura (cm)`)) +
  geom_point() +  # Plota os pontos
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Linha de regressão
  labs(x = "Nome do eixo X", y = "Nome do eixo Y", title = "Regressão Linear") +
  theme_minimal()

#-------------------------------------------------------------------------------

# Calcula a correlação entre as variáveis
cor(dados_argila_produtividade$`Teor de Argila`, dados_argila_produtividade$Produtividade)

# Calcula a correlação, porém dando detalhes extras
cor.test(dados_argila_produtividade$`Teor de Argila`, dados_argila_produtividade$Produtividade)

# Exibe o gráfico, onde o primeiro argumento é o eixo x e o segundo o eixo y
plot(dados_argila_produtividade$`Teor de Argila`, dados_argila_produtividade$Produtividade)

# Realiza a análise de regressão linear
# Intercept -> Beta 0
# dados_argila_produtividade$`Teor de Argila` -> Beta 1
regressao = lm(dados_argila_produtividade$Produtividade ~ dados_argila_produtividade$`Teor de Argila`)
summary(regressao)

# LEMBRETE: o somatório dos erros deve ser igual ou próximo a 0

erros = regressao$residuals
sum(erros)
sd(erros)

# Testando a normalidade dos dados
shapiro.test(erros)

# Se p-value > 0.05, logo os meus resíduos são homocedásticos ao longo do eixo X
gqtest(erros ~ dados_argila_produtividade$Produtividade)

ggplot(dados_argila_produtividade, aes(x = dados_argila_produtividade$`Teor de Argila`, y = dados_argila_produtividade$Produtividade)) +
  geom_point() +  # Plota os pontos
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Linha de regressão
  labs(x = "Nome do eixo X", y = "Nome do eixo Y", title = "Regressão Linear") +
  theme_minimal()

# Fazendo análise de regressão linear quadrática
# Intercept -> Beta 0
# dados_argila_produtividade$`Teor de Argila` -> Beta 1
# I(dados_argila_produtividade$`Teor de Argila`^2) -> Beta 2
quadratico = lm(
  dados_argila_produtividade$Produtividade 
  ~ dados_argila_produtividade$`Teor de Argila` 
  + I(dados_argila_produtividade$`Teor de Argila`^2))
summary(quadratico)

#-------------------------------------------------------------------------------

# Calcula a correlação entre as variáveis
cor(dados_aula_pratica$DAP, dados_aula_pratica$Altura)

# Calcula a correlação, porém dando detalhes extras
cor.test(dados_aula_pratica$DAP, dados_aula_pratica$`Área Basal`)

# Exibe o gráfico, onde o primeiro argumento é o eixo x e o segundo o eixo y
plot(log(dados_aula_pratica$DAP), log(dados_aula_pratica$Volume))

# Realiza a análise de regressão linear
# Intercept -> Beta 0
# dados_argila_produtividade$`Teor de Argila` -> Beta 1
regressao = lm(dados_aula_pratica$DAP ~ dados_aula_pratica$Volume)
summary(regressao)

# LEMBRETE: o somatório dos erros deve ser igual ou próximo a 0

erros = regressao$residuals
sum(erros)
sd(erros)

# Testando a normalidade dos dados
shapiro.test(erros)

# Se p-value > 0.05, logo os meus resíduos são homocedásticos ao longo do eixo X
gqtest(erros ~ dados_aula_pratica$Produtividade)

if (!require("rgl")) install.packages("rgl", repos = "http://cran.rstudio.com/")
library(rgl)
plot3d(x = dados_aula_pratica$DAP, y = dados_aula_pratica$Altura, dados_aula_pratica$Volume)

quadratico = lm(
  log(dados_aula_pratica$Volume) 
  ~ log(dados_aula_pratica$DAP)
  + log(dados_aula_pratica$Altura))
summary(quadratico)




















