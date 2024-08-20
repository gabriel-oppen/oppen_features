# Descrição: Essa função tem como input o arquivo "input_wide" e gera os outputs 2.0, 2.1 e 2.2
## Definição dos arquivos e fluxo: https://opesociais2017.sharepoint.com/:u:/r/sites/48.FI-12.Av.NoExperimental-MelhoriadaEducao/_layouts/15/Doc.aspx?sourcedoc=%7B6169BFCE-8E44-471F-8609-994BB02A26D6%7D&file=Fluxograma%20nivel%20escola%20-%20pareamento.vsdx&action=default&mobileredirect=true

# Configurações iniciais --------------------------------------------------

## Abrindo e/ou instalando bibliotecas usadas
if(!require(data.table)) install.packages("data.table") # manipulação de dados
if(!require(writexl)) install.packages("writexl")       # salva em .xlsx
if(!require(MatchIt)) install.packages("MatchIt")       # propensity score matching (pareamento)
if(!require(ggplot2)) install.packages("ggplot2")       # gráficos
# Definição da função -----------------------------------------------------

f_oppen_psm_did     <- function(dados,                          # base de dados
                                listas_vars.psm,                # lista de variáveis a serem usadas no psm
                                vars_resultado,                 # variáveis de resultado (indicadores)
                                #var_tratamento,                # variável que indica alocação do tratamento
                                var_intervencao,                # variável que indica a modalidade da intervenção
                                lista_tipos_intervencao,        # lista dos tipos de intervenção (municipal, regional, município X, etc)
                                output_path,                    # diretório a ser salvo o arquivo
                                nome_arquivo                    # nome do arquivo criado
) {
  
  # Convertendo para data.table
  dados <- as.data.table(dados)
  
  # Criando cópia de variáveis especificadas na função para facilitar menção no código
  dados[, var_intervencao := get(var_intervencao)] # comando get pega uma string e considera como um objeto ("teste" vira teste)
  
  # Criando dataframe vazio para armazenar resultados do loop
  dados_output_2.0 <- data.table()
  dados_output_2.1 <- data.table() 
  dados_output_2.2 <- data.table() 
  
  # Loops
  for(ti in seq_along(lista_tipos_intervencao)) { # loop do tipo de intervenção
    for (vr in seq_along(vars_resultado)) {     # loop de variáveis de resultado
      for (metodo in c("glm", "mahalanobis")) {
        for (tipo_reposicao in c("sem reposição")) {
          for(n_vizinhos in c(1,2)){            # loop do número de escolas alocadas como controle por escola de tratamento
            for(pacote in c("MatchIt")){ # loop do pacote usado (depois incluir "MatchingFrontier")
              
              # Definindo valores do loop
              tipos_intervencao_loop      <- as.character(lista_tipos_intervencao[ti])
              vars_resultado_loop         <- as.character(vars_resultado[vr])
              tipo_reposicao_loop         <- ifelse(tipo_reposicao == "sem reposição", FALSE, TRUE)
              
              # Mantendo apenas municípios de um tipo de intervenção e o pool de municípios de controle
              
              if(tipos_intervencao_loop %in% c("regional","municipal")) {
                df <- dados[dados$var_intervencao %in% c(NA, "controle", tipos_intervencao_loop), ]
              }
              
              if(tipos_intervencao_loop %in% unique(dados$id_municipio)) {
                df <- dados[(var_intervencao %in% c(NA, "controle")) | (id_municipio == tipos_intervencao_loop), ]
              }
              
              if(tipos_intervencao_loop %in% unique(dados$id_consorcio)) {
                df <- dados[(var_intervencao %in% c(NA, "controle")) | (id_consorcio == tipos_intervencao_loop), ]
              }
              
              # Gerando variável dummy de tratamento
              df[, tratamento := fcase((tipos_intervencao_loop %in% c("regional","municipal")) & (var_intervencao == tipos_intervencao_loop), 1,
                                       (tipos_intervencao_loop %in% c("regional","municipal")) & (var_intervencao != tipos_intervencao_loop), 0,
                                       (tipos_intervencao_loop %in% unique(id_municipio)) & (id_municipio == tipos_intervencao_loop), 1,
                                       (tipos_intervencao_loop %in% unique(id_municipio)) & (id_municipio != tipos_intervencao_loop), 0,
                                       (tipos_intervencao_loop %in% unique(id_consorcio)) & (id_consorcio == tipos_intervencao_loop), 1,
                                       (tipos_intervencao_loop %in% unique(id_consorcio)) & (id_consorcio != tipos_intervencao_loop), 0
              )]
              
              # Rodando Pareamento -----------------------------------------------------------
              if(pacote == "MatchIt"){
                
                # Gerando a fórmula para o PSM
                formula_psm <- as.formula(paste("tratamento ~", paste(c(listas_vars.psm, vars_resultado), collapse = " + ")))
                
                # Rodando o MatchIt para pareamento
                modelo = matchit(formula  = formula_psm,    # ver se o parametro é isso memso
                                 method   = "nearest",      # 
                                 ratio    = n_vizinhos,              # k number for the k-neighbours method 
                                 data     = df,
                                 verbose  = FALSE,
                                 distance = metodo,
                                 link     = "probit",
                                 replace  = tipo_reposicao_loop
                )    
                
                # Criando data.table com variável de propensity_score, weights e subclass (matched_group)
                df_matching = data.table(match.data(modelo,
                                                    drop.unmatched = FALSE,         # Não dropar observaçoes que não fizeram match (importante para depois ver a distribuição antes do pareamento)
                                                    distance = "matching_score",    # renomeia variável do propensity score
                                                    subclass = "matching_grupo",
                                                    weights  = "matching_peso"))    # renomeia variável de grupo
                
              }
              
              # Adicionando variáveis do loop
              df_matching[, var_intervencao := tipos_intervencao_loop]
              #df_matching[, matching_n_vizinhos := n_vizinhos]
              #df_matching[, matching_tipo := pacote]
              #df_matching[, matching_metodo := metodo]
              df_matching[, modelo := paste0(pacote, " - ", metodo, " - ", n_vizinhos," vizinhos", " - ", tipo_reposicao)]
              
              df_matching[metodo == "mahalanobis", matching_score := NA_real_] # Como mahalanobis não calculos PSM, atribuímos um valor missing
              
              # Removendo variáveis não necessárias
              #df_matching[, matching_tipo := NULL]
              #df_matching[, matching_metodo := NULL]
              
              #  # Criando Figura 2.4 (Suporte Comum)
              #  # Gráfico 1: Antes do Pareamento (usando todas as observações)
              #  grafico_antes <- ggplot(df_matching, aes(x = matching_score, color = as.factor(tratamento), linetype = as.factor(tratamento))) +
              #    geom_density(size = 1.2) +
              #    labs(y = "Densidade", color = NULL, linetype = NULL) +
              #    scale_color_manual(values = c("0" = "#E77200", "1" = "#117ABA"), labels = c("0" = "Controle", "1" = "Tratado")) +
              #    scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"), labels = c("0" = "Controle", "1" = "Tratado")) +
              #    theme_minimal() +
              #    theme(
              #      panel.grid = element_blank(),  # Remove as linhas de grade
              #      axis.title.x = element_blank(),  # Remove o título do eixo X
              #      plot.title = element_blank()     # Remove o título principal
              #    )
              #  
              #  # Gráfico 2: Depois do Pareamento (usando apenas observações onde !is.na(matching_grupo))
              #  grafico_depois <- ggplot(df_matching[!is.na(df_matching$matching_grupo), ], aes(x = matching_score, color = as.factor(tratamento), linetype = as.factor(tratamento))) +
              #    geom_density(size = 1.2) +
              #    labs(y = "Densidade", color = NULL, linetype = NULL) +
              #    scale_color_manual(values = c("0" = "#E77200", "1" = "#117ABA"), labels = c("0" = "Controle", "1" = "Tratado")) +
              #    scale_linetype_manual(values = c("0" = "dashed", "1" = "solid"), labels = c("0" = "Controle", "1" = "Tratado")) +
              #    theme_minimal() +
              #    theme(
              #      panel.grid = element_blank(),  # Remove as linhas de grade
              #      axis.title.x = element_blank(),  # Remove o título do eixo X
              #      plot.title = element_blank()     # Remove o título principal
              #    )
              #  
              #  
              #  grafico_antes
              #  
              #  grafico_depois
              
              # Adicionando dados do loop na tabela final (output 2.1)
              dados_output_2.1 <- rbindlist(list(dados_output_2.1, df_matching), use.names = TRUE) # juntando dataframes
              
              # Criando output 2.2 (Testes de balanceamento antes e depois do matching)
              
              ## rodando modelo
              for (var in c(vars_resultado_loop, listas_vars.psm)){
                for(momento in c("Antes do matching", "Depois do matching")){
                  if(momento == "Antes do matching"){
                    modelo_balanceamento <- lm(get(var) ~ tratamento, data = df_matching)
                  }
                  if(momento == "Depois do matching"){
                    modelo_balanceamento <- lm(get(var) ~ tratamento + matching_grupo, data = df_matching, weights = df_matching$matching_peso)
                  }
                  
                  ## criando variáveis
                  output_2.2 <- as.data.table(broom::tidy(modelo_balanceamento))
                  output_2.2[, variavel_caracteristica := var]
                  output_2.2[, nobs := nobs(modelo_balanceamento)]
                  output_2.2[, momento := momento]
                  output_2.2[, var_intervencao := tipos_intervencao_loop]
                  #output_2.2[, matching_n_vizinhos := n_vizinhos]
                  #output_2.2[, matching_tipo := pacote]
                  #output_2.2[, matching_metodo := metodo]
                  output_2.2[, modelo := paste0(pacote, " - ", metodo, " - ", n_vizinhos, " vizinhos", " - ", tipo_reposicao)]
                  ## salvando
                  dados_output_2.2 <- rbindlist(list(dados_output_2.2, output_2.2), use.names = TRUE)
                }
              }
              
            }
          }
        }
      }
    }
    
    # Criando output 2.2
    write_xlsx(dados_output_2.2, path = paste0(output_path,nome_arquivo,"_output_2.2.xlsx"))
    
    # Criando output 2.1
    write_xlsx(dados_output_2.1, path = paste0(output_path,nome_arquivo,"_output_2.1.xlsx"))
    
    # Criando output 2.0
    ## rodando modelo
    modelo_probit <- glm(formula = formula_psm,
                         family = binomial(link = "probit"), 
                         data = df)
    
    ## criando variáveis
    output_2.0 <- as.data.table(broom::tidy(modelo_probit))
    output_2.0[, nobs := nobs(modelo_probit)]
    output_2.0[, tipo_intervencao := tipos_intervencao_loop]
    
    ## salvando
    dados_output_2.0 <- rbindlist(list(dados_output_2.0, output_2.0), use.names =  TRUE)
    write_xlsx(dados_output_2.0, path = paste0(output_path,nome_arquivo,"_output_2.0.xlsx"))
    
    
  }
}
