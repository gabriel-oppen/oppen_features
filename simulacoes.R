# Descrição

# Este código simula uma base de dados para um ensaio clínico randomizado (RCT) 
# para investigar o efeito de um tratamento sobre o desempenho em matemática ao longo do tempo, 
# utilizando uma abordagem longitudinal. 

# Definição da estrutura dos dados:
# - São definidas variáveis como nota_matematica_base, sexo, raca e escolaridade_pais, 
#   sendo a escolaridade dos pais correlacionada com as notas de matemática.

# Geração de dados para escolas e alunos:
# - São criadas escolas fictícias com um número específico de alunos por escola.
# - É incorporada correlação intraescolar através de efeitos escolares aleatórios.

# Aplicação do tratamento:
# - A variável sorteio_tratamento determina se um aluno foi designado para o tratamento.
# - A variável tratamento_aplicado indica se o tratamento foi aplicado, seguindo uma distribuição predefinida.

# Definição do efeito do tratamento ao longo do tempo:
# - O efeito médio do tratamento (ATE) é de 10% no tempo 1 e 25% no tempo 2 para a população geral.
# - Para alunos com raca = 1, os efeitos do tratamento são ampliados para 20% no tempo 1 e 50% no tempo 2.

# Estruturação dos dados no formato longo:
# - Os dados são organizados no formato longo (longitudinal), onde cada linha representa um aluno em um tempo específico (-2 a 2).
# - São geradas notas de matemática com base na nota base do aluno, considerando o efeito do tratamento e variações aleatórias.

# Simulação do atrito nos dados:
# - O atrito é simulado para refletir a perda de seguimento de alguns alunos nos tempos 1 e 2, influenciado pela raça.

# Filtragem dos dados:
# - Os dados são filtrados para incluir apenas os alunos que responderam nos tempos 1 e 2, removendo casos de atrito.

# Resultado final:
# - A base de dados final (dtFinal) contém informações detalhadas sobre cada aluno, pronta para análises estatísticas.

# Importante: Este código simula um estudo e pode ser ajustado conforme necessidades específicas de pesquisa.


# Instalar pacotes necessários
#install.packages(c("dplyr", "tidyr", "simstudy"))

# Carregar pacotes
library(dplyr)
library(tidyr)
library(simstudy)

# Preparando ambiente

rm(list = ls(all.names = TRUE)) # clear objects
gc()                            # free up memory
set.seed(1234)                  # making it reproducible
#options(error = utils::recover) # para o código na linha antes de dar um erro

# Definir o efeito do tratamento
ATE_general_time1 <- 0.10 # Efeito médio em t1 = 10pp
ATE_general_time2 <- 0.25 # Efeito médio em t2 = 25pp
ATE_raca1_time1   <- 0.20 # Efeito médio para negros em t1 = 20pp
ATE_raca1_time2   <- 0.50 # Efeito médio para negros em t2 = 50pp

# Definir o número de escolas e alunos por escola
num_schools <- 50
students_per_school <- 20
total_students <- num_schools * students_per_school

# Criar id de escola

school_ids <- rep(1:num_schools, each = students_per_school)

# Definir a estrutura da simulação
def <- defData(varname = "nota_matematica_base", formula = 50, variance = 10, id = "id_aluno")
def <- defData(def, varname = "sexo", formula = 0.5, dist = "binary")
def <- defData(def, varname = "raca", formula = 0.25, dist = "binary")

# Adicionar correlação intra-classe
defSchool <- defDataAdd(varname = "school_effect_math", formula = 0, variance = 2)

# Gerar dados para as escolas
dtSchool <- genData(num_schools)
dtSchool <- addColumns(defSchool, dtSchool)
dtSchool <- dtSchool %>%
  rename(id_escola = id)

# Gerar dados para os alunos
dtStudent <- genData(total_students, def)

# Adicionar a coluna de id_escola aos alunos
dtStudent <- dtStudent %>%
  mutate(id_escola = school_ids) %>%
  left_join(dtSchool, by = "id_escola")

# Adicionar escolaridade dos pais correlacionada com as notas

dtStudent <- dtStudent %>%
  mutate(
    escolaridade_pais = case_when(
      nota_matematica_base + rnorm(n(), 0, 5) > 65 ~ sample(6:7, n(), replace = TRUE),
      nota_matematica_base + rnorm(n(), 0, 5) > 55 ~ sample(4:5, n(), replace = TRUE),
      nota_matematica_base + rnorm(n(), 0, 5) > 45 ~ sample(3:4, n(), replace = TRUE),
      nota_matematica_base + rnorm(n(), 0, 5) > 35 ~ sample(2:3, n(), replace = TRUE),
      TRUE ~ sample(1:2, n(), replace = TRUE)
    )
  )

# Adicionar sorteio de tratamento
dtStudent <- dtStudent %>%
  mutate(sorteio_tratamento = sample(rep(0:1, each = 500)))

# Adicionar a variável tratamento_aplicado

dtStudent <- dtStudent %>%
  mutate(tratamento_aplicado = ifelse(sorteio_tratamento == 1, rbinom(n(), 1, 0.7), 0))

# Criar dados no formato long com tempos [-2, -1, 0, 1, 2]
dtLong <- dtStudent %>%
  crossing(tempo = c(-2, -1, 0, 1, 2)) %>%
  group_by(id_aluno) %>%
  mutate(
    nota_matematica = case_when(
      tempo == -2 ~ nota_matematica_base + school_effect_math + rnorm(n(), 0, 5),
      tempo == -1 ~ nota_matematica_base + school_effect_math + rnorm(n(), 0, 3),
      tempo == 0 ~ nota_matematica_base + school_effect_math,
      tempo == 1 ~ nota_matematica_base + school_effect_math + 
        ifelse(tratamento_aplicado == 1, 
               ifelse(raca == 1, nota_matematica_base * ATE_raca1_time1, nota_matematica_base * ATE_general_time1), 0) + 
        rnorm(n(), 0, 2),
      tempo == 2 ~ nota_matematica_base + school_effect_math + 
        ifelse(tratamento_aplicado == 1, 
               ifelse(raca == 1, nota_matematica_base * ATE_raca1_time2, nota_matematica_base * ATE_general_time2), 0) + 
        rnorm(n(), 0, 2)
    )
  ) %>%
  ungroup()

# Simular atrito maior para negros (que também têm efeito maior do tratamento)
dtLong <- dtLong %>%
  mutate(
    atrito_t1 = ifelse(tempo == 1, rbinom(n(), 1, ifelse(raca == 1 & sorteio_tratamento == 0, 0.4, 0.2)), NA),
    atrito_t2 = ifelse(tempo == 2, rbinom(n(), 1, ifelse(raca == 1 & sorteio_tratamento == 0, 0.3, 0.1)), NA)
  )

# Propagar informações de atrito para t0
dtLong <- dtLong %>%
  group_by(id_aluno) %>%
  mutate(
    atrito_t1 = max(atrito_t1, na.rm = TRUE),
    atrito_t2 = max(atrito_t2, na.rm = TRUE)
  ) %>%
  ungroup()

# Adicionar variáveis de não atrito
dtLong <- dtLong %>%
  mutate(
    naoatrito_t1 = ifelse(atrito_t1 == 0, 1, 0),
    naoatrito_t2 = ifelse(atrito_t2 == 0, 1, 0)
  )

# Filtrar os dados para incluir apenas os que responderam em cada tempo
dtFinal <- dtLong %>%
  filter(!(tempo == 1 & atrito_t1 == 1), !(tempo == 2 & atrito_t2 == 1)) %>%
  select(id_aluno, sorteio_tratamento, tratamento_aplicado, nota_matematica, tempo, sexo, raca, escolaridade_pais, id_escola, naoatrito_t1, naoatrito_t2)

# Ver a estrutura final dos dados
str(dtFinal)

# Salvar a base de dados em um arquivo CSV
write.csv(dtFinal, "base_RCT_simulada_long.csv", row.names = FALSE)

# Análises da base
library(skimr)
if(!require(lmtest)) install.packages("lmtest")       # erro padrão robusto e cluster
if(!require(sandwich)) install.packages("sandwich")   # testes robustez e cluster
if(!require(ivreg)) install.packages("ivreg")         # testes late

df <- dtFinal

## Balanceamento no Baseline
model <- lm(nota_matematica ~ sorteio_tratamento,data = df[df$tempo == 0, ])
summary(model) 
model <- lm(raca ~ sorteio_tratamento,data = df[df$tempo == 0, ])
summary(model) 
model <- lm(escolaridade_pais ~ sorteio_tratamento,data = df[df$tempo == 0, ])
summary(model) 
model <- lm(sexo ~ sorteio_tratamento,data = df[df$tempo == 0, ])
summary(model) 

skim(df[df$tempo == 0,]) 

######## Variáveis balanceadas na linha de base ########

## Estimando Impacto

### ATE

#### Forma mais simples

model1 <- lm(log(nota_matematica) ~ sorteio_tratamento,data = df[df$tempo == 1, ])
summary(model1) # efeito de 8%, um pouco abaixo dos 10% reais

model2 <- lm(log(nota_matematica) ~ sorteio_tratamento,data = df[df$tempo == 2, ])
summary(model2) # efeito de 19%, um pouco abaixo dos 25% reais

model3 <- lm(log(nota_matematica) ~ sorteio_tratamento*as.factor(tempo),data = df) # dif-in-dif
summary(model3) # resultado semelhante do dif-medias

model1_LATE <- ivreg(log(nota_matematica) ~ tratamento_aplicado*raca | sorteio_tratamento*raca, data = df[df$tempo == 1, ])
summary(model1_LATE) # efeito dos 10% reais com o efeito heterogêneo próximo da realidade

model2_LATE <- ivreg(log(nota_matematica) ~ tratamento_aplicado*raca | sorteio_tratamento*raca, data = df[df$tempo == 2, ])
summary(model2_LATE) # efeito dos 10% reais com o efeito heterogêneo próximo da realidade

#### Adicionando controles da linha de base

model4 <- lm(log(nota_matematica) ~ sorteio_tratamento + raca + escolaridade_pais + sexo, data = df[df$tempo == 1, ])
summary(model4) # praticamente o mesmo resultado, o que faz sentido já que as características estavam balanceadas

model5 <- lm(log(nota_matematica) ~ sorteio_tratamento + raca + escolaridade_pais + sexo, data = df[df$tempo == 2, ])
summary(model5) # praticamente o mesmo resultado, o que faz sentido já que as características estavam balanceadas

model6 <- lm(log(nota_matematica) ~ sorteio_tratamento*as.factor(tempo) + raca + escolaridade_pais + sexo, data = df)
summary(model6) # resultado semelhante do dif-medias

#### Incluindo cluster a nível de escola

model1_rob <- coeftest(model1, vcov = vcovCL(model1, "HC1", cluster = df$id_escola[df$tempo == 1]), save = TRUE)
model1_rob # não mudou muito
summary(model1)

model2_rob <- coeftest(model2, vcov = vcovCL(model2, "HC1", cluster = df$id_escola[df$tempo == 2]), save = TRUE)
model2_rob # não mudou muito

#### Considerando atrito pelo IPW

## Testes de atrito

### Diferença da taxa de atrito por grupo

model <- lm(naoatrito_t1 ~ sorteio_tratamento, data = df[df$tempo == 0, ])
summary(model)

model <- lm(naoatrito_t2 ~ sorteio_tratamento, data = df[df$tempo == 0, ])
summary(model)

### Balanceamento no Baseline dos grupos remanescentes
model <- lm(nota_matematica ~ sorteio_tratamento, data = df[df$tempo == 0 & df$naoatrito_t1 == 1, ])
summary(model) 
model <- lm(raca ~ sorteio_tratamento,data = df[df$tempo == 0 & df$naoatrito_t1 == 1, ])
summary(model) 
model <- lm(escolaridade_pais ~ sorteio_tratamento,data = df[df$tempo == 0 & df$naoatrito_t1 == 1, ])
summary(model) 
model <- lm(sexo ~ sorteio_tratamento, data = df[df$tempo == 0 & df$naoatrito_t1 == 1, ])
summary(model) 

model <- lm(nota_matematica ~ sorteio_tratamento, data = df[df$tempo == 0 & df$naoatrito_t2 == 1, ])
summary(model) 
model <- lm(raca ~ sorteio_tratamento,data = df[df$tempo == 0 & df$naoatrito_t2 == 1, ])
summary(model) # ou seja, na amostra que permaneceu em t2, há mais negros no tratamento do que no controle. O que faz sentido porque defini que negro+controle atritariam 3vzs mais.  
model <- lm(escolaridade_pais ~ sorteio_tratamento,data = df[df$tempo == 0 & df$naoatrito_t2 == 1, ])
summary(model) 
model <- lm(sexo ~ sorteio_tratamento, data = df[df$tempo == 0 & df$naoatrito_t2 == 1, ])
summary(model) 



## Criando modelo de regressão logística para estimar a probabilidade de não atritar

model_IPW1       <- glm(naoatrito_t1 ~ sorteio_tratamento + raca, data = df[df$tempo == 0, ], family = binomial)
summary(model_IPW1) # aqui os coeficientes não fazem sentido com o que defini

model_IPW2       <- glm(naoatrito_t2 ~ sorteio_tratamento + raca, data = df[df$tempo == 0, ], family = binomial)
summary(model_IPW2) # aqui os coeficientes FAZEM sentido com o que defini

## Criando variável com a probabilidade
df_t0 <- df[df$tempo == 0, ]
df_t0$phat1 = predict(model_IPW1, type = "response")
df_t0$phat2 = predict(model_IPW2, type = "response")

## Criando pesos
df_t0 <- df_t0 %>%
  mutate(psweight1 = 1 / phat1,
         psweight2 = 1 / phat2) %>%   # Ou seja, quanto maior a probabilidade da pessoa atritar maior o seu peso. Assim, pessoas que tinham altas chances de atritar e não atritaram ficam com mais peso na amostra
  reframe(id_aluno, psweight1, psweight2)

## Mergindo
df     <- left_join(df, df_t0, by = "id_aluno")

weight1     = df$psweight1[df$tempo == 1]
weight2     = df$psweight2[df$tempo == 2]

## Rodando regressões com peso
model1_peso <- lm(log(nota_matematica) ~ sorteio_tratamento, data = df[df$tempo == 1, ], weights = weight1)
summary(model1_peso) # não mudou muito

model2_peso <- lm(log(nota_matematica) ~ sorteio_tratamento, data = df[df$tempo == 2, ], weights = weight2)
summary(model2_peso) # não mudou muito

model1_peso_LATE <- ivreg(log(nota_matematica) ~ tratamento_aplicado*raca | sorteio_tratamento*raca, data = df[df$tempo == 1, ], weights = weight1)
summary(model1_peso_LATE) 

model2_peso_LATE <- ivreg(log(nota_matematica) ~ tratamento_aplicado*raca | sorteio_tratamento*raca, data = df[df$tempo == 2, ], weights = weight2)
summary(model2_peso_LATE) 
summary(model2)

# Análise de heterogeneidade

model1_het <- lm(log(nota_matematica) ~ sorteio_tratamento*raca,data = df[df$tempo == 1, ])
summary(model1_het) # efeito de 14%, um pouco abaixo dos 20% reais

model2_het <- lm(log(nota_matematica) ~ sorteio_tratamento*raca,data = df[df$tempo == 2, ])
summary(model2_het) # efeito de 32%, abaixo dos 50% reais


