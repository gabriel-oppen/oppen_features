# Descrição do programa

# Abrindo e/ou instalando bibs

if(!require(tidyverse)) install.packages("tidyverse") # manipulação de dados
if(!require(writexl)) install.packages("writexl")     # salva em .xlsx
if(!require(lmtest)) install.packages("lmtest")      # testes late
if(!require(estimatr)) install.packages("estimatr")    # testes late
if(!require(ivreg)) install.packages("ivreg")       # testes late
if(!require(sandwich)) install.packages("sandwich")    # testes late

total_regs <- 0 # Inicializando o contador de gráficos - para dizer no final quantas vc criou :)

# Função para estimar e salvar resultados
f_oppen_estima_heterogeneo_ITT_LATE     <- function(dados,
                                                    vars_controle,
                                                    vars_resultado,
                                                    var_tratamento_sorteado, # tratamento atribuído pelo sorteio (para calcular o ITT)
                                                    var_tratamento_recebido, # tratamento de fato (para calcular o LATE)
                                                    var_heterogeneo = NULL,         # variável para definir efeito heterogêneo 
                                                    tempo_final = NULL,      # último tempo do impacto
                                                    output_path
) {
  
  # Criando controles
  
  controles <- character(length(vars_controle)) # criando lista de controles vazia
  
  for (i in seq_along(vars_controle)) {
    controles[i] <- paste0("df$", vars_controle[i], "[tempo == 0]") # adicionando os controles na lista
  }
  
  vars_controle <- paste(controles, collapse = " + ") # putting a '+' between the variables
  
  # Criando df vazio para armazenar resultados do loop
  dados_final <- data.frame() 
  
  
  for (i in seq_along(vars_resultado)) {
    
    for (t in 1:tempo_final) {
      
      for(w in seq_along(var_tratamento_recebido)) {
        
        for (h in seq_along(var_heterogeneo)) {
          
          # Definindo parâmetros do loop
          var            <- vars_resultado[i]
          var_trat_receb <- var_tratamento_recebido[w]
          var_hete       <- var_heterogeneo[h]
          
          total_regs <- total_regs + 1
          
          df <- dados
          
          df$hetero_dummy        <- df[[var_hete]] 
          df$tratamento_dummy    <- df[[var_tratamento_sorteado]]
          df$tratamento_sorteado <- df[[var_tratamento_sorteado]]*df[[var_hete]] 
          df$tratamento_recebido <- df[[var_trat_receb]]*df[[var_hete]]
          
          
          
          
          # Estimando ITT com variáveis e com erro padrão simples
          model_ITT <- lm(df[[var]][tempo == t] ~ df$tratamento_sorteado[tempo == t] + df$tratamento_dummy[tempo == t] + df$hetero_dummy[tempo == 0], data = df)
          
          formula_ITT_control <- formula(paste0(var, "[tempo == ", t, "] ~ tratamento_sorteado[tempo == ", t, "] + tratamento_dummy[tempo == ", t, "] + hetero_dummy[tempo == ", 0, "] + ", vars_controle))
          model_ITT_control <- lm(formula_ITT_control, data = df)
          
          # Estimando ITT com variáveis e com erro padrão robusto
          model_ITT_rob <- coeftest(model_ITT, vcov = vcovHC(model_ITT, "HC1"), save = TRUE)
          model_ITT_control_rob <- coeftest(model_ITT_control, vcov = vcovHC(model_ITT_control, "HC1"), save = TRUE)
          
          # Estimando LATE com variáveis e com erro padrão simples
          model_LATE <- ivreg(df[[var]][tempo == t] ~ df$tratamento_recebido[tempo == t] + df$tratamento_dummy[tempo == t] + df$hetero_dummy[tempo == 0] | df$tratamento_sorteado[tempo == t] + df$tratamento_dummy[tempo == t] + df$hetero_dummy[tempo == 0], data = df) #Para o teste Weak-instrument um p-valor baixo indica que há forte evidência contra a hipótese nula de que os instrumentos são fracos. Isso sugere que os instrumentos são relevantes para a variável instrumental, o que é desejável. O teste de Wu-Hausman é usado para testar a consistência dos estimadores IV em relação aos estimadores OLS. Um p-valor alto indica que não há evidências significativas para rejeitar a hipótese nula de consistência entre os estimadores IV e OLS. Isso sugere que o modelo IV pode ser consistente com o modelo OLS.
          
          formula_LATE_control <- formula(paste0(var, "[tempo == ", t, "] ~ tratamento_recebido[tempo == ", t, "] +  tratamento_dummy[tempo == ", t, "] + hetero_dummy[tempo == ", 0, "] + ", vars_controle, " | ", "df$tratamento_sorteado[tempo == ", t, "] + tratamento_dummy[tempo == ", t, "] + hetero_dummy[tempo == ", 0, "] + ", vars_controle))
          model_LATE_control <- ivreg(formula_LATE_control, data = df)
          
          # Estimando LATE com variáveis e com erro padrão robusto
          model_LATE_rob <- coeftest(model_LATE, vcov = vcovHC(model_LATE, "HC1"), save = TRUE)
          model_LATE_control_rob <- coeftest(model_LATE_control, vcov = vcovHC(model_LATE_control, "HC1"), save = TRUE)
          
          # Adicionando resultados em um dataframe
          dados_resultados <- data.frame(
            variavel = c(var, var, var, var),
            efeito = c(model_ITT$coefficients[[2]], model_LATE$coefficients[[2]], model_ITT_control$coefficients[[2]], model_LATE_control$coefficients[[2]]),
            erro_padrao = c(model_ITT_rob[nrow(model_ITT_rob) + 2], model_LATE_rob[nrow(model_LATE_rob) + 2], model_ITT_control_rob[nrow(model_ITT_control_rob) + 2], model_LATE_control_rob[nrow(model_LATE_control_rob) + 2]),
            n_obs = c(nrow(model_ITT[["model"]]), nrow(model_LATE[["model"]]), nrow(model_ITT_control[["model"]]),  nrow(model_LATE_control[["model"]])),
            p_val = c(model_ITT_rob[nrow(model_ITT_rob) * 3 + 2], model_LATE_rob[nrow(model_LATE_rob) * 3 + 2], model_ITT_control_rob[nrow(model_ITT_control_rob) * 3 + 2], model_LATE_control_rob[nrow(model_LATE_control_rob) * 3 + 2]),
            estimador = c("ITT", "LATE", "ITT", "LATE"),
            controles  = c("não", "não", "sim", "sim"),
            metodo = c("Diferença de Médias", "Diferença de Médias", "Diferença de Médias", "Diferença de Médias"),
            tempo =  t
          )
          
          dados_resultados <- dados_resultados %>%
            mutate(tratamento_completo = var_trat_receb,
                   heterogeneidade = var_hete)
          
          dados_final <- bind_rows(dados_final, dados_resultados) # juntando dataframes
          
          # Tendando calcular intervalos de confiança
            dados_final <- dados_final %>% 
              #group_by(variavel, tempo, controles, estimador) %>% 
              mutate(ic_baixo = efeito - qt(0.95, n_obs - 1) * erro_padrao,
                     ic_cima  = efeito + qt(0.95, n_obs - 1) * erro_padrao,
                     ic = paste0("[",ic_baixo," , ",ic_cima,"]"))
          
          # Organizando
          dados_final <- dados_final %>% reframe(tempo,variavel,estimador,n_obs,efeito, ic_baixo, ic_cima, ic, erro_padrao,p_val, metodo, controles, tratamento_completo, heterogeneidade)
          dados_final <- dados_final %>% arrange(desc(tempo), desc(controles), desc(variavel), estimador)
          dados_final$metodo_atrito_nao_aleatorio <- "Nenhum"
          total_regs <- total_regs + 2
        }
      }
    }
  }
  # Salvando
  write_xlsx(dados_final, path = paste0(output_path, "/tabelas/coef_hetero_ITT_LATE.xlsx"))
  # gran finale
  cat("Ihhaaaaaa! Você gerou uma tabela top com resultados de", total_regs , "regressões!\n") 
}

