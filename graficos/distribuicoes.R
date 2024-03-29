# Descrição do Código:
# Este script cria  uma função chamada f_oppen_graficos_distribuicao que gera gráficos de densidade (cdf) e
# funções de distribuição acumulada empíricas (ecdf) para um conjunto de dados. A função aceita um conjunto
# de dados, uma lista de variáveis de resultado, uma variável indicadora de tratamento, um diretório para
# salvar as imagens e parâmetros opcionais para personalizar os limites dos eixos y e x nos gráficos.
# Caso a variável de tratamento não seja fornecida, o código gera gráficos para toda a amostra.
# Os gráficos são salvos no diretório especificado e incluem distribuições de densidade e acumuladas para
# cada variável de resultado.

# Bibliotecas --------------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse") # manipulação de dados
if(!require(ggplot2)) install.packages("ggplot2")     # gráficos

# Definindo parâmetros
f_oppen_graficos_distribuicao <- function(dados, 
                                          vars_resultado,
                                          var_tratamento = NULL,
                                          imagens_dir = NULL,
                                          titulo = NULL,
                                          nota = NULL,
                                          ylim = c(),
                                          xlim = c()) {
  
  # Inicializando o contador de gráficos - para dizer no final quando vc criou :)
  total_graficos <- 0
  
  # Loop  
  for (i in seq_along(vars_resultado)) { # seq_along retorna um número que representa o total de elementos no vetor especificado
    
    var <- vars_resultado[i]
    
    # Verifica se variáveis opcionais foram preenchidas
    imagens_dir_final <- ifelse(is.null(imagens_dir), "", imagens_dir)
    titulo_var <- ifelse(is.null(titulo), var, titulos[i]) # se não for especificado o título usa-se o nome da variável
    
    if (!is.null(var_tratamento) && var_tratamento %in% names(dados)) { # && é um operador condicional que avalia apenas a primeira expressão e, se for falsa, não avalia a segunda expressão. Isso é conhecido como "short-circuiting".
      var_tratamento_final <- as.factor(dados[[var_tratamento]])
    } else {
      var_tratamento_final <- NULL
    }
    
    # cumulative distribution function (cdf)
    grafico_cdf <- ggplot(dados, aes(x = !!as.name(var), color = var_tratamento_final)) +
      geom_line(stat = "density") +
      labs(title = paste("Distribuição", titulo_var), y = "densidade",
           caption = nota) +
      theme_minimal(base_size = 10) +
      theme(plot.caption = element_text(size = 4)) + # tamanho da legenda
      scale_color_discrete(name = "Grupo", labels = c("1" = "Tratamento", "0" = "Controle")) +
      coord_cartesian(ylim = ylim, xlim = xlim)
    
    # salvando e incrementando o contador
    suppressWarnings(ggsave(paste0(imagens_dir_final, "distribuicao_cdf_", var, ".png"),
           plot = grafico_cdf, width = 12.7, height = 6.7, units = "cm"))
    total_graficos <- total_graficos + 1 
    
    # empirical cumulative distribution function (ecdf)
    grafico_ecdf <- ggplot(dados, aes(x = !!as.name(var), color = var_tratamento_final)) +
      stat_ecdf(geom = "line") +
      labs(title = paste("Distribuição acumulada", titulo_var), y = "percentil",
           caption = nota) +
      theme_minimal(base_size = 10) +
      theme(plot.caption = element_text(size = 4)) + # tamanho da legenda
      scale_color_discrete(name = "Grupo", labels = c("1" = "Tratamento", "0" = "Controle")) +
      coord_cartesian(ylim = c(0,1), xlim = xlim)

    # salvando e incrementando o contador
    suppressWarnings(ggsave(paste0(imagens_dir_final, "distribuicao_ecdf_", var, ".png"),
           plot = grafico_ecdf, width = 12.7, height = 6.7, units = "cm"))
    total_graficos <- total_graficos + 1
  }
  
  # gran finale
  cat("Você criou um total de", total_graficos, "gráficos lindos! Uhuuuuuul\n") 
}
