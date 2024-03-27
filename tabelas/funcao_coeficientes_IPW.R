# Descrição - Esse programa cria função que gera tabela com resultados usando IPW para lidar com atrito não-aleatório

# Abrindo e/ou instalando bibs

if(!require(tidyverse)) install.packages("tidyverse")             # manipulação de dados
if(!require(writexl)) install.packages("writexl")                 # salva em .xlsx
if(!require(causalweight)) install.packages("causalweight")       # IPW


total_regs <- 0 # Inicializando o contador de gráficos - para dizer no final quantas vc criou :)

# Função para estimar e salvar resultados
f_oppen_estima_IPW     <- function(dados,
                                   vars_controle_atrito,     # variáveis que determinaram o atrito
                                   vars_resultado,
                                   var_tratamento_sorteado, # tratamento atribuído pelo sorteio (para calcular o ITT)
                                   var_tratamento_recebido, # tratamento de fato (para calcular o LATE)
                                   tempo_final = NULL,      # último tempo do impacto
                                   output_path
) {
  
  
  
  df_ipw <- data.frame() # Dataframe vazio para armazenar resultados
  
  
  for (t in 1:tempo_final) { 
    for(w in seq_along(var_tratamento_recebido)) {  
      for (i in seq_along(vars_resultado)) { # variáveis de resultados
        for (tipo_estimador in c("ITT", "LATE")) { # loop para os tipos de modelo
          
          total_regs <- total_regs + 1
          
          # Definindo parâmetros do loop
          var            <- vars_resultado[i]
          var_trat_receb <- var_tratamento_recebido[w]
          dados$tratamento_sorteado <- dados[[var_tratamento_sorteado]] # criando variável
          dados$tratamento_recebido <- dados[[var_trat_receb]] # criando variável
          
          # Tirando valores nulos de y e definindo o tempo
          dados_filter <- dados %>%
            filter(!is.na(dados[[var]]) & dados[["tempo"]] == t)
          # Definindo a matrix de controles
          vars_control <- dados_filter[, vars_controle_atrito]
          vars_control <- as.matrix(sapply(vars_control, as.numeric))
          
          model <- lateweight(y = dados_filter[[var]], d = dados_filter$tratamento_recebido, z = dados_filter$tratamento_sorteado, x = vars_control)
          
          
          df_resultados <- data.frame(
            tempo = t,
            variavel = var,
            n_obs = nrow(dados_filter),
            estimador = tipo_estimador,
            tratamento_completo = var_trat_receb,
            controles = "sim",
            heterogeneidade = "não",
            metodo_atrito_nao_aleatorio = "IPW"
          )
          
          df_resultados <- df_resultados %>%
            mutate(erro_padrao = case_when(
              estimador == "ITT" ~ model$se.ITT,
              estimador == "LATE" ~ model$se.effect,
              .default = NA),
              p_val = case_when(
                estimador == "ITT" ~ model$pval.ITT,
                estimador == "LATE" ~ model$pval.effect,
                .default = NA),
              efeito = case_when(
                estimador == "ITT" ~ model$ITT,
                estimador == "LATE" ~ model$effect,
                .default = NA)
            )
          
          
          df_ipw <- rbind(df_ipw, df_resultados) # juntando dfs
          
        }
      }
    }
  }
  
  # Escrevendo os resultados em um arquivo Excel
  write_xlsx(df_ipw, path = paste0(output_path,"tabelas/coef_ipw_ITT_LATE.xlsx"))
  # gran finale
  cat("Ihhaaaaaa! Você gerou uma tabela top com resultados de", total_regs , "regressões!\n") 
  
}