# Descrição do programa

# Abrindo e/ou instalando bibs

if(!require(tidyverse)) install.packages("tidyverse") # manipulação de dados
if(!require(writexl)) install.packages("writexl")     # salva em .xlsx
if(!require(lmtest)) install.packages("lmtest")       # erro padrão robusto e cluster
if(!require(estimatr)) install.packages("estimatr")   # testes late
if(!require(ivreg)) install.packages("ivreg")         # testes late
if(!require(sandwich)) install.packages("sandwich")   # testes late

total_regs <- 0 # Inicializando o contador de gráficos - para dizer no final quantas vc criou :)

# Função para estimar e salvar resultados
f_oppen_estima_ITT_LATE     <- function(dados,
                                        vars_controle,
                                        vars_resultado,
                                        var_tratamento_sorteado, # tratamento atribuído pelo sorteio (para calcular o ITT)
                                        var_tratamento_recebido, # tratamento de fato (para calcular o LATE)
                                        tempo_final = NULL,      # último tempo do impacto
                                        vars_cluster,            # variável de cluster
                                        output_path
) {
  
  
  
  
  # Criando controles
  
  controles <- character(length(vars_controle)) # criando lista de controles vazia
  
  for (i in seq_along(vars_controle)) {
    controles[i] <- paste0( vars_controle[i], "[tempo == 0]") # adicionando os controles na lista
  }
  
  vars_controle <- paste(controles, collapse = " + ") # putting a '+' between the variables
  
  
  
  dados_final <- data.frame() # dataframe vazio para armazenar resultados do loop
  for (tipo_metodo in c("Nenhum", "Lee Bounds - Upper", "Lee Bounds - Lower", "IPW - manual")) { 
    for (i in seq_along(vars_resultado)) {
      
      for (t in 1:tempo_final) {
        
        for(w in seq_along(var_tratamento_recebido)) {
          
          for(c in seq_along(vars_cluster)) {
            
            # Definindo parâmetros do loop
            var              <- vars_resultado[i]
            var_trat_receb   <- var_tratamento_recebido[w]
            
            
            # Filtrando base para o Lee Bounds
            
            if (tipo_metodo %in% c("Lee Bounds - Upper","Lee Bounds - Lower") & length(dados[[var]][(dados$tempo == 0) & (!is.na(dados[[var]]))]) != 0)  { # Aplicando Lee Bounds quando o nº de observações da variável de interesse em t0 não é igual a 0
              
              # Mantendo apenas a pesquisa do loop e o baseline
              df <- dados %>% 
                filter(tempo %in% c(t,0))
              
              # Criar um df com nº de observações sem missings em cada tempo
              df_fractions <- df %>% 
                group_by(tratamento, tempo) %>% 
                filter(!is.na(!!sym(var)) & tempo %in% c(t, 0)) %>%  # quando você usa !!sym(var), o sym(var) converte a string var em um símbolo, e !! então avalia esse símbolo dentro da expressão
                reframe(n_obs = n())
              
              if (length(unique(df_fractions$tempo)) == 2) { # aplicando somente para as variáveis de resultado que têm dados no baseline
                
                # Gerando percentual de atrito e percentual a ser cortado do grupo que menos atritou  
                remaining_treatment <- df_fractions[df_fractions$tratamento == 1 & df_fractions$tempo == t, ]["n_obs"][[1]] / df_fractions[df_fractions$tratamento == 1 & df_fractions$tempo == 0, ]["n_obs"][[1]]
                remaining_control   <- df_fractions[df_fractions$tratamento == 0 & df_fractions$tempo == t, ]["n_obs"][[1]] / df_fractions[df_fractions$tratamento == 0 & df_fractions$tempo == 0, ]["n_obs"][[1]]
                trimming_fraction   <- abs((remaining_treatment - remaining_control) / max(remaining_treatment, remaining_control))
                
                # Criando subset do grupo com menor taxa de atrito
                if (remaining_treatment > remaining_control) {
                  menor_atrito = 1
                }
                if (remaining_treatment < remaining_control) {
                  menor_atrito = 0
                }
                
                dados_menoratrito <- subset(df, tratamento == menor_atrito & tempo == t)
                
                # Definindo número para remover
                n_remover <- round(nrow(dados_menoratrito) * trimming_fraction, 0)
                
                if (tipo_metodo == "Lee Bounds - Upper" & length(unique(df[[var]][!is.na(df[[var]])])) > 2){ # Segunda condição checar se variável é binária
                  # Gerando ranking e removendo   
                  df_remover <- dados_menoratrito %>% 
                    arrange(desc(!!sym(var))) %>% 
                    mutate(rank = row_number()) %>% 
                    filter(rank <= n_remover)
                }
                
                if (tipo_metodo == "Lee Bounds - Upper" & length(unique(df[[var]][!is.na(df[[var]])])) == 2) {
                  n_remover <- dados_menoratrito %>%
                    summarise(n_obs = round(n() * trimming_fraction))
                  
                  n_remover <- n_remover$n_obs
                  
                  df_remover <- dados_menoratrito %>%
                    filter(!!sym(var) == 1)
                  
                  if (n_remover <= nrow(df_remover)) {
                    df_remover <- df_remover %>% 
                      sample_n(size = n_remover, replace = FALSE) # removendo aleatóriamente
                  }
                  if (n_remover > nrow(df_remover)) {
                    n_remover = nrow(df_remover) # CUIDADO! Isso serve para quebrar um galho, mas o nº de observações do Lee bound ficará diferen no Upper e no Lower, o que pode prejudicar a interpretação.
                    df_remover <- df_remover %>% 
                      sample_n(size = n_remover, replace = FALSE) # removendo aleatóriamente
                  }
                  
                }
                # Gerando ranking e removendo  
                if (tipo_metodo == "Lee Bounds - Lower" & length(unique(df[[var]][!is.na(df[[var]])])) > 2) { # Segunda condição checar se variável não é binária
                  df_remover <- dados_menoratrito %>% 
                    arrange(!!sym(var)) %>% # invertendo a ordem 
                    mutate(rank = row_number()) %>% 
                    filter(rank <= n_remover)
                }
                
                if (tipo_metodo == "Lee Bounds - Lower" & length(unique(df[[var]][!is.na(df[[var]])])) == 2) { 
                  n_remover <- dados_menoratrito %>%
                    summarise(n_obs = round(n() * trimming_fraction))
                  
                  n_remover <- n_remover$n_obs
                  
                  df_remover <- dados_menoratrito %>%
                    filter(!!sym(var) == 0)
                  
                  if (n_remover <= nrow(df_remover)) {
                    df_remover <- df_remover %>% 
                      sample_n(size = n_remover, replace = FALSE) # removendo aleatóriamente
                  }
                  if (n_remover > nrow(df_remover)) {
                    n_remover = nrow(df_remover)
                    df_remover <- df_remover %>% 
                      sample_n(size = n_remover, replace = FALSE) # removendo aleatóriamente
                  }
                }  
                
                # Removendo
                df <- df %>% filter(!id %in% df_remover$id)
              }
            }# Fim do Lee bounds
            
            if (tipo_metodo %in% c("Nenhum","IPW - manual")) {
              df <- dados %>%
                filter(tempo %in% c(0,t))
            }
            
            # Criando variáveis
            
            df$tratamento_sorteado <- df[[var_tratamento_sorteado]]
            df$tratamento_recebido <- df[[var_trat_receb]] 
            df$var_cluster         <- df[[vars_cluster[c]]] 
            total_regs <- total_regs + 1
            
            
            # Criando df_did com  Painel Balanceado (importante apenas para dif-in-dif)
            
            df_did <- df %>% 
              group_by(id) %>% 
              mutate(n_respostas = sum(!is.na(!!sym(var)))) %>%
              ungroup() %>%
              mutate(max_respostas = max(n_respostas),
                     painel_balanceado = case_when(n_respostas == max_respostas ~ 1,
                                                   n_respostas < max_respostas ~ 0,
                                                   .default = NULL)) %>% 
              ungroup() %>% 
              filter(painel_balanceado == 1)
            
            # Gerando pesos do IPW
            
            ## Fit a logistic regression model
            formula_ipw     <- formula(paste("tratamento_sorteado[df$tempo == 0] ~ ", vars_controle))
            model_IPW       <- glm(formula_ipw, data = df, family = binomial)
            
            ## Predict probabilities
            df_t0 <- df %>% filter(tempo == 0) # mantendo só a linha de base nesse dataframe
            df_t0 <- df_t0 %>% 
              mutate(prob_treatment = predict(model_IPW, type = "response"))
            
            df_t0 <- df_t0 %>%
              mutate(psweight = case_when(
                tratamento == 1 ~ 1 / prob_treatment,
                tratamento == 0 ~ 1 / (1 - prob_treatment),
                TRUE ~ NA_real_  # Default case for any other condition
              )) %>% 
              reframe(id, psweight)
            
            df     <- left_join(df, df_t0, by = "id")
            df_did <- left_join(df_did, df_t0, by = "id")
            
            # Definindo pesos
            if (tipo_metodo == "IPW - manual") {
              weight     = df$psweight[df$tempo == t]
              weight_did = df_did$psweight
            }
            else {
              weight = NULL
              weight_did = NULL
            }
            
            
            # Criando variáveis de interação do dif-in-dif
            df_did$tratamento_sorteado_x_tempo <- as.factor(df_did$tratamento_sorteado*df_did$tempo)
            df_did$tratamento_recebido_x_tempo <- as.factor(df_did$tratamento_recebido*df_did$tempo)
            
            if (!(tipo_metodo %in% c("Lee Bounds - Upper","Lee Bounds - Lower") & length(unique(df[!is.na(df[[var]]), ]$tempo)) < 2)) { # não rodar as regressões do Lee Bounds quando variável não tive linha de base
              # Estimando ITT com variáveis e com erro padrão simples
              model_ITT     <- lm(df[[var]][tempo == t] ~ df$tratamento_sorteado[tempo == t], data = df, weights = weight)
              model_ITT_DID <- lm(df_did[[var]] ~ tratamento_sorteado_x_tempo + tratamento_sorteado + tempo, data = df_did, weights = weight_did)
              
              formula_ITT_control <- formula(paste0(var, "[tempo == ", t, "] ~ tratamento_sorteado[tempo == ", t, "] + ", vars_controle))
              model_ITT_control   <- lm(formula_ITT_control, data = df, weights = weight)
              
              # Estimando ITT com variáveis e com erro padrão robusto
              model_ITT_rob             <- coeftest(model_ITT, vcov = vcovCL(model_ITT, "HC1", cluster = df$var_cluster[df$tempo == 0]), save = TRUE)
              model_ITT_DID_rob         <- coeftest(model_ITT_DID, vcov = vcovCL(model_ITT_DID, "HC1", cluster = df_did$var_cluster), save = TRUE)
              model_ITT_control_rob     <- coeftest(model_ITT_control, vcov = vcovCL(model_ITT_control, "HC1", cluster = df$var_cluster[df$tempo == 0]), save = TRUE)
              
              # Estimando LATE com variáveis e com erro padrão simples
              model_LATE     <- ivreg(df[[var]][tempo == t] ~ df$tratamento_recebido[tempo == t] | df$tratamento_sorteado[tempo == t] , data = df, weights = weight) # Para o teste Weak-instrument um p-valor baixo indica que há forte evidência contra a hipótese nula de que os instrumentos são fracos. Isso sugere que os instrumentos são relevantes para a variável instrumental, o que é desejável. O teste de Wu-Hausman é usado para testar a consistência dos estimadores IV em relação aos estimadores OLS. Um p-valor alto indica que não há evidências significativas para rejeitar a hipótese nula de consistência entre os estimadores IV e OLS. Isso sugere que o modelo IV pode ser consistente com o modelo OLS.
              model_LATE_DID <- ivreg(df_did[[var]] ~ tratamento_recebido_x_tempo + tratamento_recebido + tempo  | tratamento_sorteado_x_tempo + tratamento_sorteado + tempo, data = df_did, weights = weight_did)
              
              formula_LATE_control <- formula(paste0(var, "[tempo == ", t, "] ~ tratamento_recebido[tempo == ", t, "] + ", vars_controle, " | ", "tratamento_sorteado[tempo == ", t, "] + ", vars_controle))
              model_LATE_control   <- ivreg(formula_LATE_control, data = df, weights = weight)
              
              # Estimando LATE com variáveis e com erro padrão robusto
              model_LATE_rob             <- coeftest(model_LATE, vcov = vcovCL(model_LATE, "HC1", cluster = df$var_cluster[df$tempo == 0]), save = TRUE)
              model_LATE_DID_rob         <- coeftest(model_LATE_DID, vcov = vcovCL(model_LATE_DID, "HC1", cluster = df_did$var_cluster), save = TRUE)
              model_LATE_control_rob     <- coeftest(model_LATE_control, vcov = vcovCL(model_LATE_control, "HC1", cluster = df$var_cluster[df$tempo == 0]), save = TRUE)
              
              # Adicionando resultados em um dataframe
              dados_resultados <- data.frame(
                variavel = c(var, var, var, var, var, var),
                efeito = c(model_ITT$coefficients[[2]], model_LATE$coefficients[[2]], model_ITT_control$coefficients[[2]], model_LATE_control$coefficients[[2]], model_ITT_DID$coefficients[[2]], model_LATE_DID$coefficients[[2]]),
                erro_padrao = c(model_ITT_rob[nrow(model_ITT_rob) + 2], model_LATE_rob[nrow(model_LATE_rob) + 2], model_ITT_control_rob[nrow(model_ITT_control_rob) + 2], model_LATE_control_rob[nrow(model_LATE_control_rob) + 2], model_ITT_DID_rob[nrow(model_ITT_DID_rob) + 2], model_LATE_DID_rob[nrow(model_LATE_DID_rob) + 2]),
                n_obs = c(nrow(model_ITT[["model"]]), nrow(model_LATE[["model"]]), nrow(model_ITT_control[["model"]]),  nrow(model_LATE_control[["model"]]), nrow(model_ITT_DID[["model"]]), nrow(model_LATE_DID[["model"]])),
                p_val = c(model_ITT_rob[nrow(model_ITT_rob) * 3 + 2], model_LATE_rob[nrow(model_LATE_rob) * 3 + 2], model_ITT_control_rob[nrow(model_ITT_control_rob) * 3 + 2], model_LATE_control_rob[nrow(model_LATE_control_rob) * 3 + 2], model_ITT_DID_rob[nrow(model_ITT_DID_rob) * 3 + 2], model_LATE_DID_rob[nrow(model_LATE_DID_rob) * 3 + 2]),
                estimador = c("ITT", "LATE", "ITT", "LATE", "ITT", "LATE"),
                metodo    = c("Diferença de Médias", "Diferença de Médias", "Diferença de Médias", "Diferença de Médias", "Diferença em Diferenças", "Diferença em Diferenças"),
                controles = c("não", "não", "sim", "sim", "não", "não"),
                tempo =  t,
                cluster = vars_cluster[c]
              )
              
              dados_resultados <- dados_resultados %>%
                mutate(tratamento_completo = var_trat_receb,
                       heterogeneidade = "não",
                       metodo_atrito_nao_aleatorio = tipo_metodo
                       
                )
              
              dados_final <- bind_rows(dados_final, dados_resultados) # juntando dataframes
              
              # Calculando intervalos de confiança
              dados_final <- dados_final %>% 
                mutate(ic_baixo = efeito - qt(0.95, n_obs - 1) * erro_padrao,
                       ic_cima  = efeito + qt(0.95, n_obs - 1) * erro_padrao,
                       ic = paste0("[",ic_baixo," , ",ic_cima,"]"))
              
              # Organizando
              dados_final <- dados_final %>% reframe(tempo,variavel,estimador,n_obs,efeito, ic_baixo, ic_cima, ic, erro_padrao,p_val, metodo, controles,tratamento_completo, heterogeneidade, metodo_atrito_nao_aleatorio, cluster)
              dados_final <- dados_final %>% arrange(desc(tempo), desc(controles), desc(variavel), estimador)
              
              total_regs <- total_regs + 2
            }
          }
        }
      }
    }
  }
  # Salvando
  write_xlsx(dados_final, path = paste0(output_path, "/tabelas/coef_medios_ITT_LATE.xlsx"))
  # gran finale
  cat("Ihhaaaaaa! Você gerou uma tabela top com resultados de", total_regs , "regressões!\n") 
}