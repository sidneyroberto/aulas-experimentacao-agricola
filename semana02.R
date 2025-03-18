# library(readxl)
# dados_semana02 <- read_excel("dados_semana02.xlsx")

# 1º -> Fazer a ANAVA
# 2º -> Extrair os erros
# 3º -> Testes (normalidade e homogeneidade de variância)

# Fazendo a ANAVA
ANAVA = aov(dados_semana02$`Massa 100 grãos (g)` ~ dados_semana02$Fungicida)
summary(ANAVA)

# Extraindo os erros (desvios)
DESVIOS = residuals(ANAVA)
summary(DESVIOS)

# Teste de normalidade de Shapiro Wilks
# Ho -> Normal (p-value >= 0,05)
# H1 -> Não normal (p-value < 0,05)
shapiro.test(DESVIOS)

# Homocedasticidade (ou Homogeneidade de Variância)
# Ho -> homocedástico
# H1 -> Heterocedástico
bartlett.test(DESVIOS~dados_semana02$Fungicida)

################################################################################

?dic

dic(
  dados_semana02$Fungicida, # tratamento
  dados_semana02$`Massa 100 grãos (g)`, # resposta
  quali = TRUE, # o tratamento é qualitativo
  mcomp = "tukey", 
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

################################################################################

anava_estudantes = aov(estudantes$Massa ~ estudantes$Genero)
summary(anava_estudantes)

erros = residuals(anava_estudantes)
shapiro.test(erros)

bartlett.test(erros ~ estudantes$Genero)

boxplot(estudantes$Massa ~ estudantes$Genero)

dic(
  estudantes$Genero, # tratamento
  estudantes$Massa, # resposta
  quali = TRUE, # o tratamento é qualitativo
  mcomp = "tukey", 
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)



