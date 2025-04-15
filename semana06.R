if (!require("ExpDes.pt")) install.packages("ExpDes.pt", repos = "http://cran.rstudio.com/")
library(ExpDes.pt)

doses <- as.character(dadosSemana06$`Doses_AIA(ppm)`)

# Análise de pressupostos dos resíduos
anova = aov(dadosSemana06$Germinacao ~ doses)
summary(anova)

erros = anova$residuals
hist(erros)
boxplot(erros ~ doses)
shapiro.test(erros)
bartlett.test(erros ~ doses)

dic(
  trat = dadosSemana06$`Doses_AIA(ppm)`,
  resp = dadosSemana06$Germinacao,
  quali = FALSE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

# Primeiramente, utilizando o modelo linear simples
regressao = lm(dadosSemana06$Germinacao ~ dadosSemana06$`Doses_AIA(ppm)`)
summary(regressao)

plot(dadosSemana06$`Doses_AIA(ppm)`, dadosSemana06$Germinacao)
lines(dadosSemana06$`Doses_AIA(ppm)`, fitted.values(regressao), col="red")

# Agora, utilizando o modelo quadrático
regressao = lm(dadosSemana06$Germinacao ~ dadosSemana06$`Doses_AIA(ppm)` + I(dadosSemana06$`Doses_AIA(ppm)` ^ 2))
summary(regressao)

plot(dadosSemana06$`Doses_AIA(ppm)`, dadosSemana06$Germinacao)
lines(dadosSemana06$`Doses_AIA(ppm)`, fitted.values(regressao), col="red")

# Agora, utilizando o modelo cúbico
regressao = lm(dadosSemana06$Germinacao ~ dadosSemana06$`Doses_AIA(ppm)` + I(dadosSemana06$`Doses_AIA(ppm)` ^ 2) + I(dadosSemana06$`Doses_AIA(ppm)` ^ 3))
summary(regressao)

cor.test(dadosSemana06$`Doses_AIA(ppm)`, dadosSemana06$Germinacao)

erros_regressao = residuals(regressao)
shapiro.test(erros_regressao)

if (!require("lmtest")) install.packages("lmtest", repos = "http://cran.rstudio.com/")
library(lmtest)

gqtest(erros_regressao ~ dadosSemana06$`Doses_AIA(ppm)`)
sum(erros_regressao)
sd(erros_regressao)

plot(dadosSemana06$`Doses_AIA(ppm)`, dadosSemana06$Germinacao)
lines(dadosSemana06$`Doses_AIA(ppm)`, fitted.values(regressao), col="red", type="p")




















