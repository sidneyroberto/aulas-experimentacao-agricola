#toto = c(39, 23, 25, 34, 28, 24, 23, 25, 61, 44, 56, 24, 26, 29, 39, 40, 39, 3540)
#toto = c(39, 23, 25, 34, 28, 24, 23, 25, 61, 44, 56, 24, 26, 29, 39, 40, 39)

# Gera um banco de dados aleatório
# No exemplo, gera 30000 dados, com média de 22 e desvio padrão de 7
toto = rnorm(5000, 22, 7)

# Medidas de posição

media <- mean(toto) # média
media
median(toto) # mediana
mfv(toto) # moda

# Medidas de dispersão

var(toto) # variância
desvio_padrao <- sd(toto) # desvio padrão
desvio_padrao

coeficiente_de_variacao <- desvio_padrao / media * 100 # coeficiente de variação
coeficiente_de_variacao

boxplot(toto)

hist(toto)

shapiro.test(toto) # Teste de normalidade de Shapiro Wilks
# Ho -> Normal (p-value >= 0,05)
# H1 -> Não normal (p-value < 0,05)

# Calcula a curtose
# curtose == 0 -> mesocúrtica (curva normal gaussiana)
# curtose > 0 -> leptocúrtica (curva acentuada para cima)
# curtose < 0 -> platicúrtica (curva acentuada para baixo)
kurtosis(toto) 

# Calcula a assimetria
# assimetria == 0 -> simétrica 
# assimetria > 0 -> assimétrica à direita
# assimetria < 0 -> assimétrica à esquerda
skewness(toto)

# Calcula a estatística descritiva de forma automática
stat.desc(toto)

summary(toto)


