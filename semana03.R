library(readxl)
dados <- read_excel("dados_semana3.xlsx", sheet = "Sheet1")
View(dados)

boxplot(dados$Altura ~ dados$Tratamento)

anova <- aov(dados$Altura ~ dados$Bloco + dados$Tratamento)
summary(anova)

erros <- residuals(anova)
erros

shapiro.test(erros)
hist(erros)

boxplot(erros ~ dados$Tratamento)

bartlett.test(erros ~ dados$Tratamento)

dbc(
  dados$Tratamento,
  dados$Bloco,
  dados$Altura,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

anova2 <- aov(dados2$Produtividade ~ dados2$Bloco + dados2$Cultivar)
summary(anova2)

boxplot(dados2$Produtividade ~ dados2$Cultivar)

erros2 <- residuals(anova2)
hist(erros2)
boxplot(erros2 ~ dados2$Cultivar)

shapiro.test(erros2)

bartlett.test(erros2 ~ dados2$Cultivar)

dbc(
  dados2$Cultivar,
  dados2$Bloco,
  dados2$Produtividade,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

outros_dados <- dados2[-96, ];dados2
View(outros_dados)

anova3 <- aov(outros_dados$Produtividade ~ outros_dados$Bloco + outros_dados$Cultivar)
summary(anova3)

boxplot(outros_dados$Produtividade ~ outros_dados$Cultivar)

erros3 <- residuals(anova3)
hist(erros3)
boxplot(erros3 ~ outros_dados$Cultivar)

shapiro.test(erros3)

bartlett.test(erros3 ~ outros_dados$Cultivar)

dbc(
  outros_dados$Cultivar,
  outros_dados$Bloco,
  outros_dados$Produtividade,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#------------------------------------------------------------------------------#
View(dadosExercicio)
anovaEx <- aov(dadosExercicio$Resposta ~ dadosExercicio$BLOCO + dadosExercicio$Tratamento)
summary(anovaEx)

boxplot(dadosExercicio$Resposta ~ dadosExercicio$Tratamento)

erros4 <- residuals(anovaEx)
hist(erros4)
boxplot(erros4 ~ dadosExercicio$Tratamento)

shapiro.test(erros4)

bartlett.test(erros4 ~ dadosExercicio$Tratamento)

dbc(
  dadosExercicio$Tratamento,
  dadosExercicio$BLOCO,
  dadosExercicio$Resposta,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)








