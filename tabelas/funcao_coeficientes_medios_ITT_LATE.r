# Descrição do programa

library(tidyverse)
#library(car)
library(writexl)      # salva em .xlsx
library(lmtest)       # cálculo de erro padrão robusto
library(estimatr)     # efeitos robustos
library(ivreg)        # variaveis instrumentais

# Função para estimar e salvar resultados
f_oppen_estima_ITT_LATE     <- function(dados,
										vars_controle,
										vars_resultado,
										var_tratamento_sorteado, # tratamento atribuído pelo sorteio (para calcular o ITT)
										var_tratamento_recebido, # tratamento de fato (para calcular o LATE)
										tempo = NULL # tempo do impacto
) {
  
  dados_final <- data.frame() # dataframe vazio para armazenar resultados do loop
  
  for (i in seq_along(vars_resultado)) {
  
    var <- vars_resultado[i]
	
	dados$tratamento_sorteado <- dados[[var_tratamento_sorteado]] # criando variável
	dados$tratamento_recebido <- dados[[var_tratamento_recebido]] # criando variável
    
    # Estimando ITT com variáveis e com erro padrão simples
    model_ITT <- lm(dados[[var]][tempo == 2] ~ dados$tratamento_sorteado[tempo == 2] + dados[[var]][tempo == 0], data = dados)
    
    formula_ITT_control <- formula(paste0(var, "[tempo == 2] ~ tratamento_sorteado[tempo == 2] + ", vars_controle))
    model_ITT_control <- lm(formula_ITT_control, data = dados)
    
    # Estimando ITT com variáveis e com erro padrão robusto
    model_ITT_rob <- coeftest(model_ITT, vcov = vcovHC(model_ITT, "HC1"), save = TRUE)
    model_ITT_control_rob <- coeftest(model_ITT_control, vcov = vcovHC(model_ITT_control, "HC1"), save = TRUE)
    
    # Estimando LATE com variáveis e com erro padrão simples
    model_LATE <- ivreg(dados[[var]][tempo == 2] ~ dados$tratamento_recebido[tempo == 2] + dados[[var]][tempo == 0] | dados$tratamento_sorteado[tempo == 2] + dados[[var]][tempo == 0], data = dados)
    
    formula_LATE_control <- formula(paste0(var, "[tempo == 2] ~ tratamento_recebido[tempo == 2] + ", vars_controle, " | ", "dados$tratamento_sorteado[tempo == 2] + ", vars_controle))
    model_LATE_control <- ivreg(formula_LATE_control, data = dados)
    
    # Estimando LATE com variáveis e com erro padrão robusto
    model_LATE_rob <- coeftest(model_LATE, vcov = vcovHC(model_LATE, "HC1"), save = TRUE)
    model_LATE_control_rob <- coeftest(model_LATE_control, vcov = vcovHC(model_LATE_control, "HC1"), save = TRUE)
    
    # Adicionando resultados em um dataframe
    dados_resultados <- data.frame(
      variavel = c(var, var, var, var),
      efeito = c(round(model_ITT$coefficients[[2]], 2), round(model_LATE$coefficients[[2]], 2), round(model_ITT_control$coefficients[[2]], 2), round(model_LATE_control$coefficients[[2]], 2)),
      desvio_padrao = c(round(model_ITT_rob[nrow(model_ITT_rob) + 2], 2), round(model_LATE_rob[nrow(model_LATE_rob) + 2], 2), round(model_ITT_control_rob[nrow(model_ITT_control_rob) + 2], 2), round(model_LATE_control_rob[nrow(model_LATE_control_rob) + 2], 2)),
      n_obs = c(nrow(model_ITT[["model"]]), nrow(model_LATE[["model"]]), nrow(model_ITT_control[["model"]]),  nrow(model_LATE_control[["model"]])),
      p_val = c(round(model_ITT_rob[nrow(model_ITT_rob) * 3 + 2], 2), round(model_LATE_rob[nrow(model_LATE_rob) * 3 + 2], 2), round(model_ITT_control_rob[nrow(model_ITT_control_rob) * 3 + 2], 2), round(model_LATE_control_rob[nrow(model_LATE_control_rob) * 3 + 2], 2)),
      estimador = c("ITT", "LATE", "ITT + controles LB", "LATE + controles LB")
    )
    
    dados_final <- bind_rows(dados_final, dados_resultados) # juntando dataframes
    
  }
  
  # Salvando
  write_xlsx(dados_final, path = paste0(output_dir, "/tabelas/coef_medios_ITT_LATE.xlsx"))
}

