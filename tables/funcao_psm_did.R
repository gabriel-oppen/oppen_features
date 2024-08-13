# Configurações iniciais --------------------------------------------------

## Abrindo e/ou instalando bibliotecas usadas
if(!require(data.table)) install.packages("data.table") # manipulação de dados
if(!require(writexl)) install.packages("writexl")       # salva em .xlsx
if(!require(MatchIt)) install.packages("MatchIt")       # propensity score matching (pareamento)

## Criando dataframe vazio para armazenar resultados do loop
dados_final <- data.table() 

# Definição da função -----------------------------------------------------

f_oppen_psm_did     <- function(dados,                          # base de dados
                                listas_vars.psm,                # lista de variáveis a serem usadas no psm
                                vars_resultado,                 # variáveis de resultado (indicadores)
                                #var_tratamento,                # variável que indica alocação do tratamento
                                var_intervencao,                # variável que indica a modalidade da intervenção
                                lista_tipos_intervencao,        # lista dos tipos de intervenção (municipal, regional,)
                                
                                output_path,                    # diretório a ser salvo o arquivo
                                nome_arquivo                    # nome do arquivo criado
) {
  
  # Convertendo para data.table
  dados <- as.data.table(dados)
  
  # Criando cópia de variáveis especificadas na função para facilitar menção no código
  dados[, var_intervencao := get(var_intervencao)] # comando get pega uma string e considera como um objeto ("teste" vira teste)

  # Loops
  
  for (vr in seq_along(vars_resultado)) { # loop de variáveis de resultado
    for(ti in seq_along(tipos_intervencao)) { # loop do tipo de intervenção
      for (metodo in c("full", "nearest")) {
        k_values <- if(metodo == "full") NA else c(1,2) # Definindo valores de k com base no método (para não rodar loops desnecessários)
        
        for(k in k_values){ # loop do número de escolas alocadas como controle por escola de tratamento
          for(pacote in c("MatchIt")){ # loop do pacote usado (depois incluir "MatchingFrontier")
            
            # Definindo valores do loop
            tipos_intervencao_loop <- as.character(tipos_intervencao[ti])
            vars_resultado_loop    <- as.character(vars_resultado[vr])
            
            # Mantendo apenas municípios de um tipo de intervenção e o pool de municípios de controle
            
            if(tipos_intervencao_loop %in% c("regional","municipal")) {
              df <- dados[dados$var_intervencao %in% c(NA, "controle", tipos_intervencao_loop), ]
            }
            
            if(tipos_intervencao_loop %in% unique(dados$id_escola)) {
              df <- dados[(var_intervencao == "controle") | (id_escola == tipos_intervencao_loop), ]
            }
            
            if(tipos_intervencao_loop %in% unique(dados$id_consorcio)) {
              df <- dados[(var_intervencao == "controle") | (id_consorcio == tipos_intervencao_loop), ]
            }
            
            
            # Gerando variável dummy de tratamento
            df[, tratamento := fcase((tipos_intervencao_loop %in% c("regional","municipal")) & (var_intervencao == tipos_intervencao_loop), 1,
                                     (tipos_intervencao_loop %in% c("regional","municipal")) & (var_intervencao != tipos_intervencao_loop), 0,
                                     (tipos_intervencao_loop %in% unique(id_escola)) & (id_escola == tipos_intervencao_loop), 1,
                                     (tipos_intervencao_loop %in% unique(id_escola)) & (id_escola != tipos_intervencao_loop), 0,
                                     (tipos_intervencao_loop %in% unique(id_consorcio)) & (id_consorcio == tipos_intervencao_loop), 1,
                                     (tipos_intervencao_loop %in% unique(id_consorcio)) & (id_consorcio != tipos_intervencao_loop), 0
                                     )]
            
            # Rodando Pareamento -----------------------------------------------------------
            if(pacote == "MatchIt"){
              
              # Gerando a fórmula para o PSM
              formula_psm <- as.formula(paste("tratamento ~", paste(listas_vars.psm, collapse = " + ")))
              
              # Rodando o MatchIt para pareamento
              if(metodo == "nearest") {
                modelo = matchit(formula = formula_psm, # ver se o parametro é isso memso
                                 method  = metodo,      # método de pareamento
                                 ratio   = k,           # k number for the k-neighbours method 
                                 data    = df,
                                 verbose = FALSE
                )    
              }
              
              if(metodo == "full") {
                modelo = matchit(formula = formula_psm,
                                 method  = metodo, 
                                 data    = df,
                                 verbose = FALSE
                )
              }
              
              
              # Criando data.table com variável de propensity_score, weights e subclass (matched_group)
              df_matching = data.table(match.data(modelo,
                                                  drop.unmatched = FALSE, # Não dropar observaçoes que não fizeram match (importante para depois ver a distribuição antes do pareamento)
                                                  distance = "matching_score",    # renomeia variável do propensity score
                                                  subclass = "matching_grupo",
                                                  weights  = "matching_peso"))   # renomeia variável de grupo
              
            }
            
            # Adicionando variáveis do loop
            df_matching[, var_intervencao := tipos_intervencao_loop]
            df_matching[, matching_n_vizinhos := k]
            df_matching[, matching_tipo := pacote]
            df_matching[, matching_metodo := metodo]
            df_matching[, modelo := fcase( # criando nome do modelo agregando todas as suas especificações
              matching_metodo == "nearest", paste0(matching_tipo, " - ", matching_metodo, " ", matching_n_vizinhos),
              matching_metodo == "full", paste0(matching_tipo, " - ", matching_metodo)
            )]
            
            # Removendo variáveis não necessárias
            df_matching[, matching_tipo := NULL]
            df_matching[, matching_metodo := NULL]
            
            # Adicionando dados do loop na tabela final
            dados_final <- rbindlist(list(dados_final, df_matching)) # juntando dataframes
            
          }
        }
      }
    }
    # Salvando
    write_xlsx(dados_final, path = paste0(output_path,nome_arquivo,".xlsx"))
  }
}
