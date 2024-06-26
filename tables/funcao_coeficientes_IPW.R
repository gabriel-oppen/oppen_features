# Descrição - Esse programa cria função que gera tabela com resultados usando IPW para lidar com atrito não-aleatório

# Abrindo e/ou instalando bibs

if(!require(tidyverse)) install.packages("tidyverse")             # manipulação de dados
if(!require(writexl)) install.packages("writexl")                 # salva em .xlsx
if(!require(causalweight)) install.packages("causalweight")       # IPW
if(!require(fastDummies)) install.packages("fastDummies")         # Cria dummies de factors



total_regs <- 0 # Inicializando o contador de gráficos - para dizer no final quantas vc criou :)

# Função para estimar e salvar resultados
f_oppen_estima_IPW     <- function(dados,
                                   vars_controle_atrito,     # variáveis que determinaram o atrito
                                   vars_resultado,
                                   var_tratamento_sorteado, # tratamento atribuído pelo sorteio (para calcular o ITT)
                                   var_tratamento_recebido, # tratamento de fato (para calcular o LATE)
                                   tempo_final = NULL,      # último tempo do impacto,
                                   vars_cluster,
                                   output_path
) {
  
  
  
  df_ipw <- data.frame() # Dataframe vazio para armazenar resultados
  
  
  for (t in 1:tempo_final) { 
    for(w in seq_along(var_tratamento_recebido)) {  
      for (i in seq_along(vars_resultado)) { # variáveis de resultados
        for (tipo_estimador in c("ITT", "LATE")) { # loop para os tipos de modelo
          for (c in seq_along(vars_cluster)) {
            
            total_regs <- total_regs + 1
            
            # Definindo parâmetros do loop
            var            <- vars_resultado[i]
            var_trat_receb <- var_tratamento_recebido[w]
            var_cluster    <- vars_cluster[c]
            
            # Criando variáveis
            dados$tratamento_sorteado <- dados[[var_tratamento_sorteado]]
            dados$tratamento_recebido <- dados[[var_trat_receb]] 
            dados$var_cluster         <- dados[[var_cluster]]
            
            # Tirando valores nulos de y e definindo o tempo
            dados_filter <- dados %>%
              filter(!is.na(dados[[var]]) & dados[["tempo"]] == t)
            
            
            # Cria dummies para os factors
            vars_control <- dados_filter[, vars_controle_atrito]
            factor_vars <- names(vars_control)[sapply(vars_control, is.factor)]  # Identify factor variables
            controls_df <- dummy_cols(vars_control, select_columns = factor_vars) # Create dummy variables for each level of factor variable
            controls_df <- select(controls_df, -all_of(factor_vars)) # Remove original factor variables
            
            
            # Rodando modelo
            model <- lateweight(y = dados_filter[[var]], d = dados_filter$tratamento_recebido, z = dados_filter$tratamento_sorteado, x = controls_df, cluster = dados_filter$var_cluster)
            
            
            df_resultados <- data.frame(
              tempo = t,
              variavel = var,
              n_obs = nrow(dados_filter),
              estimador = tipo_estimador,
              tratamento_completo = var_trat_receb,
              controles = "sim",
              metodo = "Diferença de Médias",
              heterogeneidade = "não",
              metodo_atrito_nao_aleatorio = "IPW",
              cluster = var_cluster
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
            
            # Calculando intervalos de confiança
            df_resultados <- df_resultados %>% 
              mutate(ic_baixo = efeito - qt(0.95, n_obs - 1) * erro_padrao,
                     ic_cima  = efeito + qt(0.95, n_obs - 1) * erro_padrao,
                     ic = paste0("[",ic_baixo," , ",ic_cima,"]"))
            
            
            df_ipw <- rbind(df_ipw, df_resultados) # juntando dfs
            
          }
        }
      }
    }
  }
  # Escrevendo os resultados em um arquivo Excel
  write_xlsx(df_ipw, path = paste0(output_path,"tabelas/coef_ipw_ITT_LATE.xlsx"))
  # gran finale
  cat("Ihhaaaaaa! Você gerou uma tabela top com resultados de", total_regs , "regressões!\n") 
  
}