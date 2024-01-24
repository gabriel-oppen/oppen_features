
# Descrição do programa
## Este cria gráficos de barras com intervalo de confiança a partir de um conjunto de dados.
## A função f_oppen_graficos_barras aceita parâmetros como variáveis de tempo, resultado, e grupo,
## além de permitir a personalização de rótulos e o armazenamento das imagens geradas em um diretório específico.

# Bibliotecas --------------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse") # manipulação de dados
if(!require(ggplot2)) install.packages("ggplot2")     # gráficos

# Função para criar gráficos de barras com intervalo de confiança
f_oppen_graficos_barras <- function(dados,
                                    tempo_var = NULL,
                                    vars_resultado,
                                    ylim = c(),
                                    grupo_var = NULL,
                                    nota = NULL,
                                    imagens_dir = NULL,
                                    label = NULL
) {
  
  # Inicializando o contador de gráficos - para dizer no final quando vc criou :)
  total_graficos <- 0
  
  # Loop  
  for (i in seq_along(vars_resultado)) { # seq_along retorna um número que representa o total de elementos no vetor especificado
    
    var <- vars_resultado[i]
    
    # Verifica se variáveis opcionais foram preenchidas
    imagens_dir_final <- ifelse(is.null(imagens_dir), "", imagens_dir)
    label_var <- ifelse(is.null(label), var, label[i]) # se não for especificado o título usa-se o nome da variável
    
    
    if (!is.null(grupo_var) && grupo_var %in% names(dados)) { #&& é um operador condicional que avalia apenas a primeira expressão e, se for falsa, não avalia a segunda expressão. Isso é conhecido como "short-circuiting".
      grupo_var_final <- as.factor(dados[[grupo_var]])
    } else {
      grupo_var_final <- NULL
    }
    
    tempo_var_final <- ifelse(is.null(tempo_var), grupo_var_final, tempo_var)
    
    # Criar gráfico de barras
    grafico_barras <- ggplot(dados, aes(x = tempo_var_final, y = !!as.name(var), fill = grupo_var_final)) +
      stat_summary(geom = "bar", position = "dodge", color = "black", fun = "mean") +
      stat_summary(geom = "errorbar", position = position_dodge(0.9), width = 0.25, fun.data = "mean_cl_normal") +
      labs(title = paste0("Diferença entre médias de ", label_var),
           x = "tempo",
           y = label,
           fill = "Grupos",
           caption = nota) +
      theme_minimal(base_size = 10) +
      theme(plot.caption = element_text(size = 4)) + # tamanho da legenda
      scale_fill_discrete(name = "Grupo", labels = c("1" = "Tratamento", "0" = "Controle")) +
      coord_cartesian(ylim = ylim)
    
    # salvando e incrementando o contador
    suppressWarnings(ggsave(paste0(imagens_dir_final, "barras_diferenca_medias_", var, ".png"),
                            plot = grafico_barras, width = 12.7, height = 6.7, units = "cm"))
    total_graficos <- total_graficos + 1 
  }
  
  # gran finale
  cat("Você criou um total de", total_graficos, "gráficos lindos! Uhuuuuuul\n") 
  
}
