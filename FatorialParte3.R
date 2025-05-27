library(readxl)
dadosFat3 <- read_excel("dados_biocarvao_dbc.xlsx")
View(dadosFat3)

# y = M + B= F1 + F2 + F1.F2 + e
# altura = bloco + biocarvao + npk + biocarvao.npk + e

boxplot(dadosFat3$altura ~ dadosFat3$tratamento)

dadosFat3$npk = as.character(dadosFat3$npk)
anova = aov(dadosFat3$altura ~ dadosFat3$bloco + dadosFat3$biocarvao + dadosFat3$npk + dadosFat3$npk * dadosFat3$biocarvao)
summary(anova)

erros = anova$residuals
shapiro.test(erros)

bartlett.test(erros ~ dadosFat3$tratamento)

if (!require("ExpDes.pt")) install.packages("ExpDes.pt", repos = "http://cran.rstudio.com/")
library(ExpDes.pt)

dadosFat3$npk = as.numeric(dadosFat3$npk)

fat2.dbc(
  dadosFat3$biocarvao,
  dadosFat3$npk,
  dadosFat3$bloco,
  dadosFat3$altura,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Biocarvão", "NPK"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

# Pega apenas os com biocarvão pois são os significatvos
com_biocarvao = subset(dadosFat3, dadosFat3$biocarvao == "Com")

# Aplicando o modelo estatístico quadrático
regressao = lm(com_biocarvao$altura ~ com_biocarvao$npk + I(com_biocarvao$npk ^ 2))
summary(regressao)






