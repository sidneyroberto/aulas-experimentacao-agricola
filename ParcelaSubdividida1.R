if (!require("readxl")) install.packages("readxl", repos = "http://cran.rstudio.com/")
library(readxl)
dadosSub1 <- read_excel("ParcelaSubdivididas.xlsx")
View(dadosSub1)

boxplot(dadosSub1$Produção ~ dadosSub1$Tratamento)

if (!require("ExpDes.pt")) install.packages("ExpDes.pt", repos = "http://cran.rstudio.com/")
library(ExpDes.pt)

psub2.dbc(
  dadosSub1$Manejo,
  dadosSub1$Soja,
  dadosSub1$Bloco,
  dadosSub1$Produção,
  quali = c(TRUE, TRUE),
  mcomp = "tukey",
  fac.names = c("Manejo", "Soja"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)




