# Descrição do programa

# Abrindo e/ou instalando bibs

if(!require(tidyverse)) install.packages("tidyverse") # manipulação de dados
if(!require(writexl)) install.packages("writexl")     # salva em .xlsx
if(!require(lmtest)) install.packages("lmtest")       # erro padrão robusto e cluster
if(!require(estimatr)) install.packages("estimatr")   # testes late
if(!require(ivreg)) install.packages("ivreg")         # testes late
if(!require(sandwich)) install.packages("sandwich")   # testes late

# Inicializando o contador de gráficos - para dizer no final quantas vc criou :)
total_regs <- 0 

# Iniciando relógio para saber quanto tempo demorou para rodar
start_time <- Sys.time()

# Função para estimar e salvar resultados
f_oppen_estima_ITT_LATE     <- function(dados,
                                        vars_controle,
                                        var_estrato = NULL,
                                        vars_resultado,
                                        var_tratamento_sorteado,        # tratamento atribuído pelo sorteio. Tem que ser Dummy 
                                        var_tratamento_recebido = NULL, # tratamento de fato. Tem que ser Dummy
                                        var_tempo = NULL,               # variável de tempo da coleta. Tem que ser numérica
                                        vars_cluster = NULL,            # variável de cluster
                                        var_heterogeneo = NULL,         # variável efeitos heterogêneos
                                        var_id          = NULL,         # variável de ID do nível da observação
                                        output_path,
                                        nome_arquivo                    # nome do arquivo gerado
) {
  
  # Definindo tempos
  
  if (is.null(var_tempo)) {
    baseline    <- 1
    dados$tempo <- 1
  }
  if (!is.null(var_tempo)) {
    baseline <- min(unique(dados$tempo))
    dados$tempo <- dados[[var_tempo]]
  }
  
  # Definindo id
  if (is.null(var_id)) {
    dados$id <- row_number(dados$tratamento) # criando id para cada observação em que tratamento não é missing
  }
  if (!is.null(var_id)) {
    dados$id <- dados[[var_id]]
  }
  
  # Apendando variável "não" à lista de variáveis heterogênas
  var_heterogeneo <- c(var_heterogeneo, "não")
  
  # Definindo variável de tratamento efetivo
  var_tratamento_recebido <- c(var_tratamento_recebido, "não")
  
  # Criando controles
  
  controles <- character(length(vars_controle)) # criando lista de controles vazia
  
  for (i in seq_along(vars_controle)) {
    controles[i] <- paste0( vars_controle[i], "[tempo == baseline]") # adicionando os controles na lista
  }
  
  vars_controle <- paste(controles, collapse = " + ") # putting a '+' between the variables
  
  dados_final <- data.frame() # dataframe vazio para armazenar resultados do loop
  for (tipo_metodo in c("Nenhum", "IPW - manual",  "Lee Bounds - Upper", "Lee Bounds - Lower")) { 
    for (t in unique(dados$tempo)) {
      
      for (i in seq_along(vars_resultado)) {
        
        for(w in seq_along(var_tratamento_recebido)) {
          
          for (h in seq_along(var_heterogeneo)) {
            
            for(c in seq_along(vars_cluster)) {
              
              for(tipo_estimador in c("ITT","LATE")) {
                
                for(tipo_regressao in c("Diferença de Médias", "Diferença em Diferenças")) {
                  
                  for(tipo_controle in c("não", "sim")) {
                    
                    for(tipo_estrato in c("não", "sim")) {
                      
                      # Definindo parâmetros do loop
                      var              <- vars_resultado[i]
                      if (var_tratamento_recebido[w] == "não"){
                        var_trat_receb <- var
                      }
                      if (var_tratamento_recebido[w] != "não"){
                        var_trat_receb <- var_tratamento_recebido[w]
                      }
                      var_hete         <- var_heterogeneo[h]
                      
                      dados$var_resultado <- dados[[var]] # variável de resultado do loop
                      
                      # Filtrando base para o Lee Bounds
                      
                      if (tipo_metodo %in% c("Lee Bounds - Upper","Lee Bounds - Lower") & length(dados[[var]][(dados$tempo == baseline) & (!is.na(dados[[var]]))]) != 0 )  { # & !is.na(var_tempo). Aplicando Lee Bounds quando o nº de observações da variável de interesse no baseline não é igual a 0 e quando existe variável de tempo (ou seja, quando temos informações anteriores ao tratamento)
                        
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
                          remaining_treatment <- df_fractions[df_fractions$tratamento == 1 & df_fractions$tempo == t, ]["n_obs"][[1]] / df_fractions[df_fractions$tratamento == 1 & df_fractions$tempo == baseline, ]["n_obs"][[1]]
                          remaining_control   <- df_fractions[df_fractions$tratamento == 0 & df_fractions$tempo == t, ]["n_obs"][[1]] / df_fractions[df_fractions$tratamento == 0 & df_fractions$tempo == baseline, ]["n_obs"][[1]]
                          trimming_fraction   <- abs((remaining_treatment - remaining_control) / max(remaining_treatment, remaining_control))
                          
                          # Criando subset do grupo com menor taxa de atrito
                          if (remaining_treatment > remaining_control) {
                            menor_atrito = 1
                          }
                          if (remaining_treatment < remaining_control) {
                            menor_atrito = 0
                          }
                          
                          dados_menoratrito <- subset(df, tratamento == menor_atrito & tempo == t & !is.na(var_resultado))
                          
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
                      }
                      
                      # IPW e Nenhum
                      
                      if (tipo_metodo %in% c("Nenhum","IPW - manual")) {
                        df <- dados %>%
                          filter(tempo %in% c(0,t))
                      }
                      
                      # Criando variáveis
                      
                      df$tratamento_sorteado <- df[[var_tratamento_sorteado]]
                      df$tratamento_recebido <- df[[var_trat_receb]] 
                      df$var_cluster         <- df[[vars_cluster[c]]]
                      
                      ## Variáveis de efeito heterogêno
                      if (var_hete == "não") {
                        df$hetero_dummy <- 1
                      }
                      else {
                        df$hetero_dummy <- df[[var_hete]]
                      }
                      
                      if (!is.null(var_tempo)) {
                        
                        df <- df %>%
                          group_by(id) %>%
                          arrange(id, tempo) %>% 
                          mutate(hetero_dummy = first(hetero_dummy),              # substituindo variável pelo seu valor no tempo 0
                                 tratamento_recebido = last(tratamento_recebido)) %>%  # substituindo variável pelo seu valor no tempo 2
                          ungroup()
                      }
                      
                      ## Variáveis de tratamento heterogêneo
                      
                      df$tratamento_dummy_sorteado    <- df[[var_tratamento_sorteado]]
                      df$tratamento_sorteado          <- df[[var_tratamento_sorteado]]*df$hetero_dummy
                      df$tratamento_dummy_recebido    <- df$tratamento_recebido
                      df$tratamento_recebido          <- df$tratamento_recebido*df$hetero_dummy
                      
                      # Definindo estrato
                      
                      if(tipo_estrato == "sim" & !is.null(var_estrato)){
                        df$estrato_definido <- df[[var_estrato]]
                        lista_controles <- paste0(vars_controle, " + ", var_estrato, "[tempo == baseline]")
                      }
                      if(tipo_estrato == "não" | is.null(var_estrato)){
                        df$estrato_definido <- 1
                        lista_controles <- vars_controle
                      }
                      
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
                      if (tipo_metodo == "IPW - manual" & nrow(df[(df$tempo == baseline) & !is.na(df$var_resultado), ]) > 0) { # não rodando quando a variável só tem missings
                        
                        ## Mantendo só a linha de base nesse dataframe
                        df_t0 <- df %>% filter(tempo == baseline & !is.na(var_resultado)) 
                        
                        ## Criando modelo de regressão logística para estimar a probabilidade de receber o tratamento no baseline com base em observáveis
                        formula_ipw     <- formula(paste("df_t0[[var_tratamento_sorteado]][df_t0$tempo == baseline] ~ ", lista_controles))
                        model_IPW       <- glm(formula_ipw, data = df_t0, family = binomial)
                        
                        ## Criando variável com a probabilidade
                        df_t0$prob_treatment = predict(model_IPW, type = "response")
                        ## Criando pesos
                        df_t0 <- df_t0 %>%
                          mutate(psweight = case_when(
                            tratamento == 1 ~ 1 / prob_treatment,
                            tratamento == 0 ~ 1 / (1 - prob_treatment),
                            TRUE ~ NA_real_  # Default case for any other condition
                          )) %>% 
                          reframe(id, psweight)
                        
                        df     <- left_join(df, df_t0, by = "id")#, relationship = "many-to-one")
                        df_did <- left_join(df_did, df_t0, by = "id")
                        
                        # Definindo pesos
                        
                        weight     = df$psweight[df$tempo == t]
                        weight_did = df_did$psweight
                      }
                      #if (tipo_metodo != "IPW - manual" | nrow(df[(df$tempo == baseline) & !is.na(df$var_resultado), ]) == 0) {
                      else {
                        weight = NULL
                        weight_did = NULL
                      }
                      
                      
                      # Criando variáveis de interação do dif-in-dif
                      df_did$tratamento_sorteado_x_tempo <- as.factor(df_did$tratamento_sorteado*df_did$tempo)
                      df_did$tratamento_recebido_x_tempo <- as.factor(df_did$tratamento_recebido*df_did$tempo)
                      
                      # Definindo casos em que não iremos rodar os modelos
                      
                      ## caso 1: se é VERDADEIRO que, sendo Lee Bounds, a variável NÂO tem observações válidas na linha de base
                      if (tipo_metodo %in% c("Lee Bounds - Upper", "Lee Bounds - Lower") &
                          length(unique(df[!is.na(df[[var]]), ]$tempo)) < 2) {
                        
                        condicao_1 = 1
                      }
                      else {
                        condicao_1 = 0
                      }
                      
                      ## caso 2: se é VERDADEIRO que a variável de resultado NÂO tem valores válidos quando o tempo for igual ao baseline
                      
                      if (all(is.na(mean(unique(df[df$tempo == baseline, ][[var]]), na.rm =  TRUE)) & t == baseline)) {
                        
                        condicao_2 = 1
                      }
                      else{
                        condicao_2 = 0
                      }
                      
                      # Rodando modelos
                      
                      if (condicao_1 + condicao_2 == 0) { 
                        
                        if (tipo_regressao == "Diferença de Médias") {
                          if (tipo_estimador == "ITT") {
                            if(tipo_controle == "não"){
                              model <- lm(df[[var]][tempo == t] ~ df$tratamento_sorteado[tempo == t] + df$tratamento_dummy_sorteado[tempo == baseline] + df$hetero_dummy[tempo == baseline] + df$estrato_definido[tempo == baseline], data = df, weights = weight)
                            }
                            if(tipo_controle == "sim"){
                              formula <- formula(paste0(var, "[tempo == ", t, "] ~ tratamento_sorteado[tempo == ", t, "] + tratamento_dummy_sorteado[tempo == baseline] + hetero_dummy[tempo == baseline] + ", lista_controles))
                              model   <- lm(formula, data = df, weights = weight)
                            }
                          }
                          if (tipo_estimador == "LATE" & t != 0){
                            if (var_trat_receb != var) {
                              if(tipo_controle == "não"){
                                model <- ivreg(df[[var]][tempo == t] ~ tratamento_recebido[tempo == t] + tratamento_dummy_recebido[tempo == baseline] + hetero_dummy[tempo == baseline] + estrato_definido[tempo == baseline] | tratamento_sorteado[tempo == t] + tratamento_dummy_sorteado[tempo == baseline] + hetero_dummy[tempo == baseline] + estrato_definido[tempo == baseline], data = df, weights = weight) # Para o teste Weak-instrument um p-valor baixo indica que há forte evidência contra a hipótese nula de que os instrumentos são fracos. Isso sugere que os instrumentos são relevantes para a variável instrumental, o que é desejável. O teste de Wu-Hausman é usado para testar a consistência dos estimadores IV em relação aos estimadores OLS. Um p-valor alto indica que não há evidências significativas para rejeitar a hipótese nula de consistência entre os estimadores IV e OLS. Isso sugere que o modelo IV pode ser consistente com o modelo OLS.
                              }
                              if(tipo_controle == "sim"){
                                formula <- formula(paste0(var, "[tempo == ", t, "] ~ tratamento_recebido[tempo == ", t, "] + tratamento_dummy_recebido[df$tempo == baseline] + hetero_dummy[df$tempo == baseline] + ", lista_controles, " | ", "tratamento_sorteado[tempo == ", t, "] + tratamento_dummy_sorteado[df$tempo == baseline] + hetero_dummy[df$tempo == baseline] + ", lista_controles))
                                model   <- ivreg(formula, data = df, weights = weight)
                              }                            
                            }
                            if (var_trat_receb == var) {
                              model <- NULL
                            }
                            
                          }
                          
                          # erro padrão robusto
                          if (!is.null(model)) {
                            model_rob <- coeftest(model, vcov = vcovCL(model, "HC1", cluster = df$var_cluster[df$tempo == baseline]), save = TRUE)  
                          }
                          
                        }
                        if (tipo_regressao == "Diferença em Diferenças" & length(unique(df$tempo)) >= 2)  {  # apenas aplica dif-in-dif para dataframes que têm mais de um período de tempo
                          if(tipo_estimador == "ITT") {
                            model <- lm(df_did[[var]] ~ tratamento_sorteado_x_tempo + tratamento_sorteado + tempo + tratamento_dummy_sorteado + hetero_dummy + estrato_definido, data = df_did, weights = weight_did)
                          }
                          if(tipo_estimador == "LATE" & t != 0) {
                            if (var_trat_receb != var) {
                              model <- ivreg(df_did[[var]] ~ tratamento_recebido_x_tempo + tratamento_recebido + tempo + tratamento_dummy_recebido + hetero_dummy + estrato_definido | tratamento_sorteado_x_tempo + tratamento_sorteado + tempo + tratamento_dummy_sorteado + hetero_dummy + estrato_definido, data = df_did, weights = weight_did)
                            }
                            if (var_trat_receb == var) {
                              model <- NULL
                            }
                          }
                          # erro padrão robusto
                          if (!is.null(model)) {
                            model_rob <- coeftest(model, vcov = vcovCL(model, "HC1", cluster = df_did$var_cluster), save = TRUE)  
                          }
                          
                        }
                        if (tipo_regressao == "Diferença em Diferenças" & (length(unique(df$tempo)) < 2 | tipo_controle == "sim")){ # não rodar modelo dif-in-dif quando tem controles e quando tem menos de um tempo
                          model <- NULL
                        }
                        
                        # Adicionando resultados em um dataframe
                        if(!is.null(model)){
                          dados_resultados <- data.frame(
                            variavel    = var,
                            efeito      = model$coefficients[[2]],
                            erro_padrao = model_rob[nrow(model_rob) + 2],
                            n_obs       = nrow(model[["model"]]),
                            p_val       = model_rob[nrow(model_rob) * 3 + 2],
                            estimador   = tipo_estimador,
                            metodo      = tipo_regressao,
                            controles   = tipo_controle,
                            tempo       = t,
                            cluster     = vars_cluster[c],
                            tratamento_completo = var_tratamento_recebido[w],
                            heterogeneidade = var_hete,
                            metodo_atrito_nao_aleatorio = tipo_metodo,
                            estratificado = tipo_estrato
                          )
                          
                          # adicionando intervalo de confiança
                          dados_resultados <- dados_resultados %>% 
                            mutate(
                              ic_baixo = efeito - qt(0.95, n_obs - 1) * erro_padrao,
                              ic_cima  = efeito + qt(0.95, n_obs - 1) * erro_padrao
                            )
                          
                          # adicionando variáveis para análise de heterogeneidade
                          if (var_hete != "não") {
                            dados_resultados <- dados_resultados %>% 
                              mutate(
                                efeito_tratamento = model$coefficients[[3]],
                                efeito_interacao  = efeito,
                                efeito_caracteristica = model$coefficients[[4]],
                                p_val_tratamento = model_rob[nrow(model_rob) * 3 + 3],
                                p_val_interacao  = p_val,
                                p_val_caracteristica = model_rob[nrow(model_rob) * 3 + 4],
                                prop_amostra = nrow(df[df$hetero_dummy == 1, ]) / nrow(df)
                              )
                          }
                          
                          dados_final <- bind_rows(dados_final, dados_resultados) # juntando dataframes
                          total_regs <- total_regs + 1
                          
                        }
                        model <- NULL
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Organizando
  dados_final <- dados_final %>% arrange(desc(tempo), desc(controles), desc(variavel), estimador)
  # Salvando
  write_xlsx(dados_final, path = paste0(output_path,nome_arquivo,".xlsx"))
  
  # Terminando relógio para saber quanto tempo demorou para rodar
  end_time <- Sys.time()
  execution_time <- end_time - start_time
  
  # gran finale
  cat("Ihhaaaaaa! Em ", execution_time," segundos você gerou uma tabela top com resultados de", total_regs , "regressões!\n") 
}
