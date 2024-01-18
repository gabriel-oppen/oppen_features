# Bibliotecas --------------------------------------------------------------------

library(tidyverse)    # Manipulação de dados
library(ggplot2)      # Gráficos
library(patchwork)    # Combinação de gráficos
library(skimr)        # Estatísticas descritivas


# Diretórios

prepara_basedados_dir = "2_prepara_basedados/output/dados/" # caminho onde está a base de dados principal
output_dir = "3_estatisticas_descritivas/output/"           # caminho onde serão salvos os produtos

# Abrir conjunto de dados tratado

df <- readRDS(file = paste0(prepara_basedados_dir, "dados_dremio.rds"))


# Filtrando só quem preencheu a pesquisa e é controle
df<- df %>% 
  filter(consent == 1 & tratamento == 0)

# Mantendo apenas variáveis necessárias

df <- df %>%
  select(
    caseid,
    tratamento,
    indice_conhecimento_processo_avaliacao_formativa,
    indice_mindset_expectativas,
    indice_autoeficacia,
    indice_praticas_processo_avaliacao_formativa
  )



# Loop de Distribuição de variáveis

## Defina aqui as variáveis que terão um histograma criado ##
variaveis = c("indice_conhecimento_processo_avaliacao_formativa",
              "indice_mindset_expectativas",
              "indice_autoeficacia",
              "indice_praticas_processo_avaliacao_formativa")

for (var in variaveis) {
  
  nome_var = paste0("índice de ", str_split(var, "_")[[1]][2])
  
  # cumulative distribution function (cdf)
  grafico_cdf <- ggplot(df,  aes(x = !!as.name(var), color = as.factor(tratamento))) + # !!as.name(var) is used to unquote the variable var and ensure that it is treated as a variable within the aes() function.
    geom_line(stat = "density") + 
    labs(title = paste("Distribuição do", nome_var), y = "densidade") + # Define o título dinâmico 
    theme_minimal(base_size = 10)  +
    scale_color_discrete(name = "Grupo", labels = c("Controle")) +  # Specify custom legend labels
    coord_cartesian(ylim = c(0, 5), xlim = c(0.4, 1))  # Define os limites dos eixos x e y
  
  ggsave(paste0(output_dir, "/imagens/distribuicao_cdf_", var, ".png"), plot = grafico_cdf, width = 12.7, height = 6.7, units = "cm") # saving
  
  # empirical cumulative distribution function (ecdf)
  
  grafico_ecdf <- ggplot(df, aes(x = !!as.name(var), color = as.factor(tratamento))) + # aes_string takes the string provided in the loop and uses it as the column name.
    stat_ecdf(geom = "line") +  
    labs(title = paste("Distribuição acumulada do", nome_var), y = "percentil") + # Define o título dinâmico 
    theme_minimal(base_size = 10)  +
    scale_color_discrete(name = "Grupo", labels = c("Controle")) +  # Specify custom legend labels
    coord_cartesian(ylim = c(0, 1), xlim = c(0.4, 1))  # Define os limites dos eixos x e y
  
  ggsave(paste0(output_dir, "/imagens/distribuicao_ecdf_", var, ".png"), plot = grafico_ecdf, width = 12.7, height = 6.7, units = "cm") # saving
}

