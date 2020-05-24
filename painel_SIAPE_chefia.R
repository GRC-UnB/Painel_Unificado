library(data.table)
library(tidyverse)
library(readr)

# Parametros inciais
rm(list = ls())
source("parametros.R")
source(funcoes)


# Parâmetros Ano de início e final da base
ano_inicial = 2013
ano_final = 2019
meses = c(paste0(0, 1:9), 10:12)

# Roda loop do ano
for (ano in ano_inicial:ano_final) {
  # Roda o loop dos meses
  for (mes in meses) {
    # Caminhos dos arquivos
    caminho_chefia = paste0(pasta_siape,
                            ano,
                            "_",
                            mes,
                            "_servidores/",
                            ano,
                            mes,
                            "_Cadastro_CHEFIA.csv")
    caminho_chefia_RDS = paste0(pasta_siape,
                                ano,
                                "_",
                                mes,
                                "_servidores/",
                                ano,
                                mes,
                                "_CHEFIA.RDS")
    caminho_cadastro_RDS = paste0(pasta_siape,
                                  ano,
                                  "_",
                                  mes,
                                  "_servidores/",
                                  ano,
                                  mes,
                                  "_Cadastro.RDS")
    
    
    # Execução remunera
    if (file.exists(caminho_chefia)) {
      # Leitura do arquivo de chefia
      chefia = fread(caminho_chefia)
      
      # Transforma a variável 
      chefia = chefia %>%
        mutate(id = as.character(COD_ORG_EXERCICIO)) %>%
        select(-c(COD_ORG_EXERCICIO))
      
      # if(file.exists(caminho_cadastro_RDS)){
      cadastro_rds = readRDS(caminho_cadastro_RDS)
      
      cadastro_rds = cadastro_rds %>%
        filter(ATIVIDADE == 1) %>%
        select(Id_SERVIDOR_PORTAL, id)
      
      cadastro_rds = cadastro_rds %>%
        distinct(Id_SERVIDOR_PORTAL, .keep_all = T)
      
      chefia = chefia %>%
        distinct(Id_SERVIDOR_PORTAL, id, .keep_all = T)
      
      chefia = merge.data.frame(
        x = chefia,
        y = cadastro_rds,
        by = c("Id_SERVIDOR_PORTAL", "id")
      )
      
      write_rds(x = chefia,path = caminho_chefia_RDS)
      
      rm(cadastro_rds)
      # }
      # Contagem de total de itens
      chefia2 = chefia %>%
        mutate(regime_juridico = REGIME_JURIDICO) %>%
        select(-c(REGIME_JURIDICO)) %>%
        group_by(id, regime_juridico) %>%
        summarise(quantidade_chefia = n()) %>%
        spread(key = regime_juridico,
               value = quantidade_chefia,
               fill = 0)
      
      # Total geral de cargos de chefia ocupados
      chefia2$total_cargo_chefia_ocup = rowSums(chefia2[,-c(1)])
      
      # Percentuais
      chefia2 = chefia2 %>%
        mutate(
          servidores_efetivos = `REGIME JURIDICO UNICO`,
          celetistas = `CONSOLIDACAO DAS LEIS DO TRABALHO`,
          perc_efetivos_chefia = servidores_efetivos/ total_cargo_chefia_ocup,
          perc_celetistas_chefia = celetistas / total_cargo_chefia_ocup) %>%
        select(-c(`REGIME JURIDICO UNICO`,`CONSOLIDACAO DAS LEIS DO TRABALHO`))
      
      
      # Mês como número - Correção para o mês de dezembro
      mes_int = as.integer(mes)
      mes_int = ifelse(mes_int == 12, 0, mes_int)
      ano_int = ifelse(mes_int == 0, ano + 1, ano)
      # Servidor tá ativo?
      chefia2 = chefia2 %>%
        mutate(data = as.Date.character(paste0("01-", mes_int + 1, "-", ano_int), format = "%d-%m-%Y"),
               data = data - 1)
      
      chefia2 = chefia2 %>%
        select(
          id,
          data,
          servidores_efetivos,
          celetistas,
          total_cargo_chefia_ocup,
          perc_efetivos_chefia,
          perc_celetistas_chefia
        )
      
      
      
      write_rds(x = chefia2, path = caminho_chefia_RDS)
      
      ## Agrupamento
      
      if (!exists("final_agregado")) {
        final_agregado = chefia2
      }
      else{
        temp_agregado = chefia2
        
        final_agregado = rbindlist(list(final_agregado, temp_agregado))
        # Remove temp_agregado
        rm(temp_agregado)
      }
      # Remove dataframe unificado final
      rm(chefia, chefia2)
      log_mensagem = paste0("Arquivos ", caminho_chefia, " lido com sucesso.")
    }
    else{
      log_mensagem = paste0("Não foi encontrado o arquivo: ", caminho_chefia)
    }
    cat("\n\n OS ARQUIVOS ", mes, "-", ano, " foram lidos")
    cat(
      paste0(date(), " - ", log_mensagem, "\n"),
      file = paste0(pasta_logs, "siape_chefia.log"),
      append = T
    )
    
    rm(log_mensagem)
  }
}
openxlsx::write.xlsx(final_agregado, file = "Paineis/PAINEL_CHEFIA.xlsx")
