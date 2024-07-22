# Descrição do programa - Essa função gera planilhas com resultados do Propensity Score Matching (PSM) e uma planilha com resultados das estimações por Diferenças em Diferenças (DiD)


# Configurações iniciais --------------------------------------------------

## Abrindo e/ou instalando bibibliotecas usadas

if(!require(tidyverse)) install.packages("tidyverse") # manipulação de dados
if(!require(writexl)) install.packages("writexl")     # salva em .xlsx
if(!require(MatchIt)) install.packages("MatchIt")     # propensity score matching (pareamento)


## Inicializando o contador de regressões - para dizer no final quantas vc criou :)
total_regs <- 0 

## Iniciando relógio para saber quanto tempo demorou para rodar
start_time <- Sys.time()

## Criando dataframe vazio para armazenar resultados do loop
dados_final <- data.frame() 

# Definição da função -----------------------------------------------------

f_oppen_psm_did     <- function(dados,                          # base de dados
                                listas_vars.psm,                # lista de variáveis a serem usadas no psm
                                vars_resultado,                 # variáveis de resultado (indicadores)
                                var_tratamento,                 # variável que indica alocação do tratamento
                                var_intervencao,                # variável que indica a modalidade da intervenção
                                var_nivel.resultados,           # variável que indica o nível dos resultados das estimações
                                output_path,                    # diretório a ser salvo o arquivo
                                nome_arquivo                    # nome do arquivo gerado
) {
  
  for (vr in seq_along(vars_resultado)) {
    
    dados$var_resultado_loop <- vars_resultado[vr]
    print(vars_resultado[vr])
    
  }
  

}