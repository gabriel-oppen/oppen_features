# Descrição: Essa função tem como input o arquivo "input_wide" e gera os outputs 2.0, 2.1 e 2.2
## Definição dos arquivos e fluxo: https://opesociais2017.sharepoint.com/:u:/r/sites/48.FI-12.Av.NoExperimental-MelhoriadaEducao/_layouts/15/Doc.aspx?sourcedoc=%7B6169BFCE-8E44-471F-8609-994BB02A26D6%7D&file=Fluxograma%20nivel%20escola%20-%20pareamento.vsdx&action=default&mobileredirect=true

# Configurações iniciais --------------------------------------------------

## Abrindo e/ou instalando bibliotecas usadas
if(!require(data.table)) install.packages("data.table") # manipulação de dados
if(!require(writexl)) install.packages("writexl")       # salva em .xlsx
if(!require(MatchIt)) install.packages("MatchIt")       # propensity score matching (pareamento)
if(!require(ggplot2)) install.packages("ggplot2")       # gráficos
if(!require(patchwork)) install.packages("patchwork")   # junta gráficos

# Definição da função -----------------------------------------------------

f_oppen_psm_did     <- function(dados,                          # base de dados
                                listas_vars.psm,                # lista de variáveis a serem usadas no psm
                                vars_resultado,                 # variáveis de resultado (indicadores)
                                #var_tratamento,                # variável que indica alocação do tratamento
                                var_intervencao,                # variável que indica a modalidade da intervenção
                                #lista_tipos_intervencao,        # lista dos tipos de intervenção (municipal, regional, município X, etc)
                                lista_tipos_intervencao_media,
                                lista_tipos_intervencao_inferior,
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
  
  # Criando lista de todos os tipos de intervenção
  lista_tipos_intervencao_todas <- c(lista_tipos_intervencao_media, lista_tipos_intervencao_inferior)
  
  # Loops
  for(ti in seq_along(lista_tipos_intervencao_todas)) { # loop do tipo de intervenção
    for(pacote in c("MatchIt")){     # loop de variáveis de resultado
      for (metodo in c("glm", "mahalanobis")) {
        for (tipo_reposicao in c("com reposição","sem reposição")) {
          for(n_vizinhos in c(1, 3, 5)){            # loop do número de escolas alocadas como controle por escola de tratamento
            for (vr in seq_along(vars_resultado)){ # loop do pacote usado (depois incluir "MatchingFrontier")
              
              # Definindo valores do loop
              tipos_intervencao_loop      <- as.character(lista_tipos_intervencao_todas[ti])
              vars_resultado_loop         <- as.character(vars_resultado[vr])
              tipo_reposicao_loop         <- ifelse(tipo_reposicao == "sem reposição", FALSE, TRUE)
              
              # Mantendo apenas municípios de um tipo de intervenção e o pool de municípios de controle
              
              if(tipos_intervencao_loop %in% lista_tipos_intervencao_media) {
                df <- dados[dados$var_intervencao %in% c(NA, "controle", tipos_intervencao_loop), ]
              }
              
              if(tipos_intervencao_loop %in% unique(dados$id_municipio)) {
                df <- dados[(var_intervencao %in% c(NA, "controle")) | (id_municipio == tipos_intervencao_loop), ]
              }
              
              if(tipos_intervencao_loop %in% unique(dados$id_consorcio)) {
                df <- dados[(var_intervencao %in% c(NA, "controle")) | (id_consorcio == tipos_intervencao_loop), ]
              }
              
              # Gerando variável dummy de tratamento
              df[, tratamento := fcase((tipos_intervencao_loop %in% lista_tipos_intervencao_media) & (var_intervencao == tipos_intervencao_loop), 1,
                                       (tipos_intervencao_loop %in% lista_tipos_intervencao_media) & ((var_intervencao != tipos_intervencao_loop) | is.na(var_intervencao)), 0,
                                       (tipos_intervencao_loop %in% unique(id_municipio)) & (id_municipio == tipos_intervencao_loop), 1,
                                       (tipos_intervencao_loop %in% unique(id_municipio)) & (id_municipio != tipos_intervencao_loop), 0,
                                       (tipos_intervencao_loop %in% unique(id_consorcio)) & (id_consorcio == tipos_intervencao_loop), 1,
                                       (tipos_intervencao_loop %in% unique(id_consorcio)) & ((id_consorcio != tipos_intervencao_loop) | (is.na(id_consorcio))), 0
              )]
              
              # Rodando Pareamento -----------------------------------------------------------
              if(pacote == "MatchIt"){
                
                # Gerando a fórmula para o PSM
                formula_psm <- as.formula(paste("tratamento ~", paste(listas_vars.psm, collapse = " + ")))
                
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
              df_matching[, modelo := paste0(pacote, " - ", metodo, " - ", n_vizinhos," vizinhos", " - ", tipo_reposicao)]
              df_matching[metodo == "mahalanobis", matching_score := NA_real_] # Como mahalanobis não calculos PSM, atribuímos um valor missing
              df_matching[tipo_reposicao_loop == "TRUE", matching_grupo := NA_real_] # Como mahalanobis não calculos PSM, atribuímos um valor missing
              
              # Adicionando dados do loop na tabela final (output 2.1)
              dados_output_2.1 <- rbindlist(list(dados_output_2.1, df_matching), use.names = TRUE) # juntando dataframes
            }
            # Criando output 2.2 (Testes de balanceamento antes e depois do matching)
            ## rodando modelo
            for (var in c(vars_resultado, listas_vars.psm)){
              for(momento in c("Antes do matching", "Depois do matching")){
                if(momento == "Antes do matching"){
                  modelo_balanceamento <- lm(get(var) ~ tratamento, data = df_matching)
                }
                if(momento == "Depois do matching"){
                  modelo_balanceamento <- lm(get(var) ~ tratamento, data = df_matching[df_matching$matching_peso != 0], weights = df_matching[df_matching$matching_peso != 0]$matching_peso)
                }
                
                ## criando variáveis
                output_2.2 <- as.data.table(broom::tidy(modelo_balanceamento, conf.int = TRUE))
                output_2.2[, variavel_caracteristica := var]
                output_2.2[, nobs := nobs(modelo_balanceamento)]
                output_2.2[, momento := momento]
                output_2.2[, var_intervencao := tipos_intervencao_loop]
                output_2.2[, modelo := paste0(pacote, " - ", metodo, " - ", n_vizinhos, " vizinhos", " - ", tipo_reposicao)]
                
                ## filtrando
                output_2.2 = output_2.2[term == "tratamento", ] # mantendo apenas o coeficiente do tratamento
                output_2.2[, term := NULL] # apagando coluna
                
                ## salvando
                dados_output_2.2 <- rbindlist(list(dados_output_2.2, output_2.2), use.names = TRUE)
                
                # Criando Figura 2.4 (Suporte Comum)
                if(metodo == "glm" & tipos_intervencao_loop %in% lista_tipos_intervencao_media){
                  # Gráfico 1: Antes do Pareamento (usando todas as observações)
                  grafico_antes <- ggplot(df_matching, aes(x = matching_score, color = as.factor(tratamento), linetype = as.factor(tratamento))) +
                    geom_density(size = 1.2) +
                    labs(y = "Densidade", color = NULL, linetype = NULL) +
                    scale_color_manual(values = c("0" = "#E77200", "1" = "#117ABA")) +
                    #scale_linewidth_manual(values = c("0" = 1.7, "1" = 1.2)) +
                    ggtitle("Antes do Pareamento") +  
                    theme_minimal() +
                    theme(
                      panel.background = element_rect(fill = "white", color = NA),  
                      plot.background = element_rect(fill = "white", color = NA),  
                      panel.grid = element_blank(),
                      axis.title.x = element_blank(),
                      plot.title = element_text(hjust = 0.5),  
                      legend.position = "none",
                      #text = element_text(family = "Itau Display")
                    ) +
                    xlim(0, 0.05) +
                    ylim(0, 120)
                  
                  # Extraindo os limites automáticos do primeiro gráfico
                  limites_anteriores <- ggplot_build(grafico_antes)$layout$panel_params[[1]]
                  x_limits <- limites_anteriores$x.range
                  y_limits <- limites_anteriores$y.range
                  
                  # Gráfico 2: Depois do Pareamento
                  grafico_depois <- ggplot(df_matching[df_matching$matching_peso != 0], aes(x = matching_score, color = as.factor(tratamento), linetype = as.factor(tratamento))) +
                    geom_density(size = 1.2) +
                    labs(y = NULL, color = NULL, linetype = NULL) +  
                    #ylab("") +
                    scale_color_manual(values = c("0" = "#E77200", "1" = "#117ABA")) +
                    #scale_linewidth_manual(values = c("0" = 1.7, "1" = 1.2)) +
                    ggtitle("Depois do Pareamento") +  
                    theme_minimal() +
                    theme(
                      panel.background = element_rect(fill = "white", color = NA),  
                      plot.background  = element_rect(fill = "white", color = NA),   
                      panel.grid       = element_blank(),
                      axis.title.x     = element_blank(),
                      axis.text.y      = element_blank(),
                      plot.title       = element_text(hjust = 0.5),  
                      legend.position  = "none",
                      #text = element_text(family = "Itau Display")
                    ) +
                    xlim(0, 0.05) +  # Aplicando limites do primeiro gráfico
                    ylim(0, 120)    # Aplicando limites do primeiro gráfico
                  
                  # Combinando os gráficos
                  graficos_juntos <- grafico_antes + grafico_depois
                  
                  ggsave(paste0("matching/output/figuras/figura_2.4_suporte_comum/Suportecomum_", pacote, "_", metodo, "_", n_vizinhos, "_vizinhos", "_", tipo_reposicao, "_", tipos_intervencao_loop, ".jpg"),
                         plot = graficos_juntos,
                         dpi = 600,
                         width = 8,
                         height = 3,
                         units = "in")  
                }
                
         #       # Criando gráfico de balanceamento Antes do Matching
         #      grafico_balanco_antes <- ggplot(dados_output_2.2[momento == "Antes do matching"], aes(x = variavel_caracteristica, y = estimate)) +
         #        geom_point(size = 3, color = "black") +  # Pontos para as estimativas
         #        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Barras de erro para o intervalo de confiança
         #        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Linha tracejada em x = 0
         #        coord_flip() +  # Inverter eixos para ter variáveis na horizontal
         #        labs(x = NULL, y = "Estimativa", title = "Antes do Pareamento") +
         #        theme_minimal() +
         #        theme(
         #          panel.background = element_rect(fill = "white", color = NA),  
         #          plot.background = element_rect(fill = "white", color = NA),  
         #          panel.grid = element_blank(),
         #          plot.title = element_text(hjust = 0.5)  
         #        )
         #      
         #      # Gráfico Depois do Matching
         #      grafico_balanco_depois <- ggplot(dados_output_2.2[momento == "Depois do matching"], aes(x = variavel_caracteristica, y = estimate)) +
         #        geom_point(size = 3, color = "black") +  # Pontos para as estimativas
         #        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Barras de erro para o intervalo de confiança
         #        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Linha tracejada em x = 0
         #        coord_flip() +  # Inverter eixos para ter variáveis na horizontal
         #        labs(x = NULL, y = NULL, title = "Depois do Pareamento") +
         #        theme_minimal() +
         #        theme(
         #          panel.background = element_rect(fill = "white", color = NA),  
         #          plot.background = element_rect(fill = "white", color = NA),  
         #          panel.grid = element_blank(),
         #          axis.text.y = element_blank(),  # Removendo os nomes das variáveis no eixo Y
         #          axis.ticks.y = element_blank(), # Removendo os ticks do eixo Y
         #          plot.title = element_text(hjust = 0.5)  
         #        )
         #      
         #      # Combinando os Gráficos
         #      graficos_balanco_juntos <- grafico_balanco_antes + grafico_balanco_depois
         #      
         #      # Salvando o gráfico final
         #      ggsave(paste0("matching/output/figuras/Balanceameto_", pacote, "_", metodo, "_", n_vizinhos, "_vizinhos", "_", tipo_reposicao, "_", tipos_intervencao_loop, ".jpg"), plot = graficos_balanco_juntos, dpi = 600, width = 8, height = 3, units = "in")
                
                
              
            }
          }
        }
      }
    }
  }
  
  # Criando output 2.2
  write_xlsx(dados_output_2.2, path = paste0(output_path,nome_arquivo,"_output_2.2.xlsx"))
  
  # Criando output 2.1
  #write_xlsx(dados_output_2.1, path = paste0(output_path,nome_arquivo,"_output_2.1.xlsx"))
  
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
