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
f_oppen_estima_ITT_LATE     <- function(dados,
                                        vars_controle,
                                        vars_resultado,
                                        var_tratamento_sorteado, # tratamento atribuído pelo sorteio (para calcular o ITT)
                                        var_tratamento_recebido, # tratamento de fato (para calcular o LATE)
                                        tempo_final = NULL,      # último tempo do impacto
                                        output_path
) {
  
  dados_final <- data.frame() # dataframe vazio para armazenar resultados do loop
  
 # for (tipo_metodo in c("Nenhum","Lee Bounds - Upper","Lee Bounds - Lower")) {
    
    for (i in seq_along(vars_resultado)) {
      
      for (t in 1:tempo_final) {
        
        for(w in seq_along(var_tratamento_recebido)) {
          
          # Definindo parâmetros do loop
          var            <- vars_resultado[i]
          var_trat_receb <- var_tratamento_recebido[w]
          
          # Filtrando base para o Lee Bounds
          
          
     #    
     #    if (tipo_metodo != "Nenhum") {
     #      dados2 <- dados %>% filter(trimming = 0)
     #    }
     #    
     #    else {
           dados2 <- dados
     #    }
          
          
          dados2$tratamento_sorteado <- dados2[[var_tratamento_sorteado]] # criando variável
          dados2$tratamento_recebido <- dados2[[var_trat_receb]] # criando variável
          
          total_regs <- total_regs + 1
          
          
          # Estimando ITT com variáveis e com erro padrão simples
          model_ITT     <- lm(dados2[[var]][tempo == t] ~ dados2$tratamento_sorteado[tempo == t] , data = dados2)
          
          if (var %in% c("tx_ocupacao", "renda_trabalho_semna", "log_renda_trabalho_semna")) {
            model_ITT_DID <- lm(dados2[[var]][tempo == t] ~ dados2$tratamento_sorteado[tempo == t] + dados2[[paste0(var)]][tempo == 0], data = dados2)
          }
          else {
            model_ITT_DID <- lm(dados2[[var]][tempo == t] ~ dados2$tratamento_sorteado[tempo == t] + dados2[[paste0(var, "_ipt")]][tempo == 0] + dados2[[paste0(var, "_mi")]][tempo == 0] , data = dados2)
          }
          
          formula_ITT_control <- formula(paste0(var, "[tempo == ", t, "] ~ tratamento_sorteado[tempo == ", t, "] + ", vars_controle))
          model_ITT_control <- lm(formula_ITT_control, data = dados2)
          
          # Estimando ITT com variáveis e com erro padrão robusto
          model_ITT_rob             <- coeftest(model_ITT, vcov = vcovHC(model_ITT, "HC1"), save = TRUE)
          model_ITT_DID_rob         <- coeftest(model_ITT_DID, vcov = vcovHC(model_ITT_DID, "HC1"), save = TRUE)
          model_ITT_control_rob     <- coeftest(model_ITT_control, vcov = vcovHC(model_ITT_control, "HC1"), save = TRUE)
          
          # Estimando LATE com variáveis e com erro padrão simples
          model_LATE     <- ivreg(dados2[[var]][tempo == t] ~ dados2$tratamento_recebido[tempo == t] | dados2$tratamento_sorteado[tempo == t] , data = dados2) #Para o teste Weak-instrument um p-valor baixo indica que há forte evidência contra a hipótese nula de que os instrumentos são fracos. Isso sugere que os instrumentos são relevantes para a variável instrumental, o que é desejável. O teste de Wu-Hausman é usado para testar a consistência dos estimadores IV em relação aos estimadores OLS. Um p-valor alto indica que não há evidências significativas para rejeitar a hipótese nula de consistência entre os estimadores IV e OLS. Isso sugere que o modelo IV pode ser consistente com o modelo OLS.
          if (var %in% c("tx_ocupacao", "renda_trabalho_semna", "log_renda_trabalho_semna")) {
            model_LATE_DID <- ivreg(dados2[[var]][tempo == t] ~ dados2$tratamento_recebido[tempo == t] + dados2[[var]][tempo == 0]  | dados2$tratamento_sorteado[tempo == t] + dados2[[var]][tempo == 0] , data = dados2) #Para o teste Weak-instrument um p-valor baixo indica que há forte evidência contra a hipótese nula de que os instrumentos são fracos. Isso sugere que os instrumentos são relevantes para a variável instrumental, o que é desejável. O teste de Wu-Hausman é usado para testar a consistência dos estimadores IV em relação aos estimadores OLS. Um p-valor alto indica que não há evidências significativas para rejeitar a hipótese nula de consistência entre os estimadores IV e OLS. Isso sugere que o modelo IV pode ser consistente com o modelo OLS.
          }
          else {
            model_LATE_DID <- ivreg(dados2[[var]][tempo == t] ~ dados2$tratamento_recebido[tempo == t] + dados2[[paste0(var,"_ipt")]][tempo == 0] + dados2[[paste0(var,"_mi")]][tempo == 0]  | dados2$tratamento_sorteado[tempo == t] + dados2[[paste0(var,"_ipt")]][tempo == 0] + dados2[[paste0(var,"_mi")]][tempo == 0] , data = dados2)          }
          
          
          
          formula_LATE_control <- formula(paste0(var, "[tempo == ", t, "] ~ tratamento_recebido[tempo == ", t, "] + ", vars_controle, " | ", "dados2$tratamento_sorteado[tempo == ", t, "] + ", vars_controle))
          model_LATE_control   <- ivreg(formula_LATE_control, data = dados2)
          
          # Estimando LATE com variáveis e com erro padrão robusto
          model_LATE_rob             <- coeftest(model_LATE, vcov = vcovHC(model_LATE, "HC1"), save = TRUE)
          model_LATE_DID_rob         <- coeftest(model_LATE_DID, vcov = vcovHC(model_LATE_DID, "HC1"), save = TRUE)
          model_LATE_control_rob     <- coeftest(model_LATE_control, vcov = vcovHC(model_LATE_control, "HC1"), save = TRUE)
          
          # Adicionando resultados em um dataframe
          dados_resultados <- data.frame(
            variavel = c(var, var, var, var, var, var),
            efeito = c(model_ITT$coefficients[[2]], model_LATE$coefficients[[2]], model_ITT_control$coefficients[[2]], model_LATE_control$coefficients[[2]], model_ITT_DID$coefficients[[2]], model_LATE_DID$coefficients[[2]]),
            erro_padrao = c(model_ITT_rob[nrow(model_ITT_rob) + 2], model_LATE_rob[nrow(model_LATE_rob) + 2], model_ITT_control_rob[nrow(model_ITT_control_rob) + 2], model_LATE_control_rob[nrow(model_LATE_control_rob) + 2], model_ITT_DID_rob[nrow(model_ITT_DID_rob) + 2], model_LATE_DID_rob[nrow(model_LATE_DID_rob) + 2]),
            n_obs = c(nrow(model_ITT[["model"]]), nrow(model_LATE[["model"]]), nrow(model_ITT_control[["model"]]),  nrow(model_LATE_control[["model"]]), nrow(model_ITT_DID[["model"]]), nrow(model_LATE_DID[["model"]])),
            p_val = c(model_ITT_rob[nrow(model_ITT_rob) * 3 + 2], model_LATE_rob[nrow(model_LATE_rob) * 3 + 2], model_ITT_control_rob[nrow(model_ITT_control_rob) * 3 + 2], model_LATE_control_rob[nrow(model_LATE_control_rob) * 3 + 2], model_ITT_DID_rob[nrow(model_ITT_DID_rob) * 3 + 2], model_LATE_DID_rob[nrow(model_LATE_DID_rob) * 3 + 2]),
            estimador = c("ITT", "LATE", "ITT", "LATE", "ITT", "LATE"),
            metodo    = c("Diferença de Médias", "Diferença de Médias", "Diferença em Diferenças", "Diferença em Diferenças", "Diferença em Diferenças", "Diferença em Diferenças"),
            controles = c("não", "não", "sim", "sim", "não", "não"),
            tempo =  t
          )
          
          dados_resultados <- dados_resultados %>%
            mutate(tratamento_completo = var_trat_receb,
                   heterogeneidade = "não")
          
          dados_final <- bind_rows(dados_final, dados_resultados) # juntando dataframes
          
          # Tendando calcular intervalos de confiança
          #  dados_final <- dados_final %>% 
          #    #group_by(variavel, tempo, controles, estimador) %>% 
          #    mutate(ic_baixo = efeito - qt(0.95, n_obs - 1) * erro_padrao / sqrt(n_obs),
          #           ic_cima  = efeito + qt(0.95, n_obs - 1) * erro_padrao / sqrt(n_obs))
          
          # Organizando
          dados_final <- dados_final %>% reframe(tempo,variavel,estimador,n_obs,efeito, erro_padrao,p_val, metodo, controles,tratamento_completo, heterogeneidade)
          dados_final <- dados_final %>% arrange(desc(tempo), desc(controles), desc(variavel), estimador)
          
          total_regs <- total_regs + 2
        }
      }
    }
  #}
  
  # Salvando
  write_xlsx(dados_final, path = paste0(output_path, "/tabelas/coef_medios_ITT_LATE.xlsx"))
  # gran finale
  cat("Ihhaaaaaa! Você gerou uma tabela top com resultados de", total_regs , "regressões!\n") 
}
