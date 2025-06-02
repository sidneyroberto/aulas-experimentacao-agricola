if (!require("readxl")) install.packages("readxl", repos = "http://cran.rstudio.com/")
library(readxl)
dadosSub2 <- read_excel("DadosParcelaSubdividida2.xlsx")
View(dadosSub2)

boxplot(dadosPsub2$Produção ~ dadosPsub2$Tratamento)

if (!require("ExpDes.pt")) install.packages("ExpDes.pt", repos = "http://cran.rstudio.com/")
library(ExpDes.pt)

# Exemplo de fator quantitativo e com desdobramento
psub2.dic(
  fator1=dadosPsub2$Soja,
  fator2=dadosPsub2$Dose,
  repet=dadosPsub2$Repetição,
  resp=dadosPsub2$Produção,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Soja", "Dose"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

sojaC = subset(dadosPsub2, dadosPsub2$Soja == "Soja C")
View(sojaC)

regressao = lm(sojaC$Produção ~ sojaC$Dose)
summary(regressao)

