library(openxlsx)
library(tidyverse)
library(readr)
library(data.table)

# Parametros inciais
rm(list=ls())
source("parametros.R")
source(funcoes)


# Parâmetros - tempo do loop
ano_inicial = 2013
ano_final = 2019
meses = c(paste0(0,1:9),10:12)

# Roda loop do ano
for(ano in ano_inicial:ano_final){
  # Roda o loop dos meses
  for(mes in meses){
    # Caminhos dos arquivos
    caminho_remuneracao = paste0(pasta_siape,ano,"_",mes,"_servidores/",ano,mes,"_Remuneracao.xlsx")
    caminho_cadastro_RDS = paste0(pasta_siape,ano,"_",mes,"_servidores/",ano,mes,"_Cadastro.RDS")
    caminho_remuneracao_rds = paste0(pasta_siape,ano,"_",mes,"_servidores/",ano,mes,"_Cadastro_Remunera.RDS")
    
    # Execução remunera
    if(file.exists(caminho_cadastro_RDS)){
      if(file.exists(caminho_remuneracao)){
        # Leitura do arquivo de cadastro
        cadastro = read_rds(caminho_cadastro_RDS)
        
        # Alterar nome 
        names(cadastro) = gsub(pattern = "PERIODO",replacement = "data",x = names(cadastro))
        
        cadastro = cadastro %>%
          filter(ATIVIDADE == 1)
        
        # Casos únicos
        unicos = cadastro %>%
          distinct(Id_SERVIDOR_PORTAL,id,.keep_all = T)
        
        # Remove Cadastro
        rm(cadastro)
        
        
        #### EXECUÇÃO REMUNERAÇÃO
        remuneracao = openxlsx::read.xlsx(xlsxFile = caminho_remuneracao)
        
        # Converte tipo
        remuneracao = remuneracao %>%
          mutate(remuneracao_basica_bruta = converter(`REMUNERAÇÃO.BÁSICA.BRUTA`),
                 remuneracao_basica_liquida = converter(`REMUNERAÇÃO.APÓS.DEDUÇÕES.OBRIGATÓRIAS`)) %>%
          select(-c(`REMUNERAÇÃO.BÁSICA.BRUTA`,`REMUNERAÇÃO.APÓS.DEDUÇÕES.OBRIGATÓRIAS`))
        
        # Cria data.frame final
        final = merge.data.frame(x = unicos,y=remuneracao,by.x = "Id_SERVIDOR_PORTAL")
        # Remove remuneração e unicos
        rm(remuneracao,unicos)
        
        write_rds(x = final,path = caminho_remuneracao_rds,compress = "xz")
        
        ## Agrupamento
        
        if(!exists("final_agregado")){
          final_agregado = final %>%
            group_by(id,data) %>%
            summarise(med_remunera_bruta = mean(remuneracao_basica_bruta,na.rm=T),
                      sd_remunera_bruta = sd(remuneracao_basica_bruta,na.rm = T),
                      med_remunera_liquida = mean(remuneracao_basica_liquida,na.rm=T),
                      sd_remunera_liquida = sd(remuneracao_basica_liquida,na.rm = T))
        }
        else{
          
          temp_agregado = final %>%
            group_by(id,data) %>%
            summarise(med_remunera_bruta = mean(remuneracao_basica_bruta,na.rm=T),
                      sd_remunera_bruta = sd(remuneracao_basica_bruta,na.rm = T),
                      med_remunera_liquida = mean(remuneracao_basica_liquida,na.rm=T),
                      sd_remunera_liquida = sd(remuneracao_basica_liquida,na.rm = T))
          
          final_agregado = rbindlist(list(final_agregado, temp_agregado))
          # Remove temp_agregado
          rm(temp_agregado)
        }
        # Remove dataframe unificado final
        rm(final)
        log_mensagem = paste0("Arquivos ",caminho_cadastro_RDS," e ",caminho_remuneracao," lidos com sucesso.")
      }
      else{
        log_mensagem = paste0("Não foi encontrado o arquivo: ",caminho_remuneracao)
      }
    }
    else{
      log_mensagem = paste0("Não foi encontrado o arquivo: ",caminho_cadastro_RDS)
    }
    dir.create(path = pasta_logs,showWarnings = F)
    cat("\n\n OS ARQUIVOS ",mes,"-",ano," foram lidos")
    cat(paste0(date()," - ",log_mensagem,"\n"),file = paste0(pasta_logs,"siape.log"),append = T)
    
    rm(log_mensagem)
  }
}
openxlsx::write.xlsx(final_agregado,file = painel_siape_remuneracao,asTable = T)
