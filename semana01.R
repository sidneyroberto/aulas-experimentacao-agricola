# Instalando dependências necessárias

#if (!require("modeest")) install.packages("modeest", repos = "http://cran.rstudio.com/")
#library(modeest)

#if (!require("e1071")) install.packages("modeest", repos = "http://cran.rstudio.com/")
#library(e1071)

#if (!require("pastecs")) install.packages("modeest", repos = "http://cran.rstudio.com/")
#library(pastecs)

# toto = c(39, 23, 25, 34, 28, 24, 23, 25, 61, 44, 56, 24, 26, 29, 39, 40, 39, 3540)
toto = c(10.5, 9.17, 9.84, 9.68, 9.95, 10.1, 9.94, 9.68, 10, 10.16, 9.43, 8.75, 10.51, 9.69, 10.55, 9.65, 9.2, 9.28, 10.43, 10.54, 10.24, 9.85, 9.08, 9.44, 10.29, 9.81, 10, 10.12, 10.32, 10.03, 9.28, 10.28, 10.35, 9.81, 9.87, 10.02, 9.25, 10.46, 10.12, 9.66, 10.51, 10.04, 10.64, 10.2, 9.79, 9.67, 9.84, 9.64, 9.92, 9.74)


# Gera um banco de dados aleatório
# No exemplo, gera 5000 dados, com média de 22 e desvio padrão de 7
# toto <- rnorm(5000, 22, 7)

# Medidas de posição

media <- mean(toto) # média
cat("Média:", media)
mediana <- median(toto) # mediana
cat("\nMediana:", mediana)
moda <- mfv(toto) # moda
moda


# Medidas de dispersão

variancia <- var(toto) # variância
cat("\nVariância:", variancia)
desvio_padrao <- sd(toto) # desvio padrão
cat("\nDesvio padrão:", desvio_padrao)

coeficiente_de_variacao <- desvio_padrao / media * 100 # coeficiente de variação
cat("\nCoeficiente de variação:", coeficiente_de_variacao)

png("boxplot_semana01.png")
boxplot(toto)
dev.off()

png("histogram_semana01.png")
hist(toto)
dev.off()

teste <- shapiro.test(toto) # Teste de normalidade de Shapiro Wilks
# Ho -> Normal (p-value >= 0,05)
# H1 -> Não normal (p-value < 0,05)
print(teste)

# Calcula a curtose
# curtose == 0 -> mesocúrtica (curva normal gaussiana)
# curtose > 0 -> leptocúrtica (curva acentuada para cima)
# curtose < 0 -> platicúrtica (curva acentuada para baixo)
curtose <- kurtosis(toto)
cat("\nCurtose:", curtose)

# Calcula a assimetria
# assimetria == 0 -> simétrica
# assimetria > 0 -> assimétrica à direita
# assimetria < 0 -> assimétrica à esquerda
assimetria <- skewness(toto)
cat("\nAssimetria:", assimetria)

# Calcula a estatística descritiva de forma automática
stat.desc(toto)

summary(toto)
