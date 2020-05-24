library(tidyverse)
library(openxlsx)
library(readr)
library(data.table)

# Parametros inciais
rm(list = ls())
source("parametros.R")
source(funcoes)


# Parâmetros
# Periodo analisado
ano_inicial = 2013
ano_final = 2019
meses = c(paste0(0, 1:9), 10:12)
# Roda loop do ano
for (ano in ano_inicial:ano_final) {
  # Roda o loop dos meses
  for (mes in meses) {
    # Caminhos dos arquivos - Cadastro e remuneracao
    caminho_cadastro = paste0(pasta_siape,
                              ano,
                              "_",
                              mes,
                              "_servidores/",
                              ano,
                              mes,
                              "_Cadastro.xlsx")
    caminho_remunera = paste0(pasta_siape,
                              ano,
                              "_",
                              mes,
                              "_servidores/",
                              ano,
                              mes,
                              "_Remuneracao.xlsx")
    caminho_cadastro_RDS = paste0(pasta_siape,
                                  ano,
                                  "_",
                                  mes,
                                  "_servidores/",
                                  ano,
                                  mes,
                                  "_Cadastro.RDS")
    
    # Verifica se existem o cadastro e a remuneração
    if (file.exists(caminho_cadastro)) {
      if (file.exists(caminho_remunera)) {
        # Leitura do arquivo de cadastro
        cadastro = openxlsx::read.xlsx(xlsxFile = caminho_cadastro)
        
        # Ajusta as datas
        cadastro = cadastro %>%
          mutate(
            id = as.character(COD_ORG_EXERCICIO),
            DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO = ajusta_data(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO),
            DATA_INICIO_AFASTAMENTO = ajusta_data(DATA_INICIO_AFASTAMENTO),
            DATA_TERMINO_AFASTAMENTO = ajusta_data(DATA_TERMINO_AFASTAMENTO),
            DATA_INGRESSO_ORGAO = ajusta_data(DATA_INGRESSO_ORGAO)
          ) %>%
          select(-c(COD_ORG_EXERCICIO))
        
        # Mês como número - Correção para o mês de dezembro
        mes_int = as.integer(mes)
        mes_int = ifelse(mes_int == 12, 0, mes_int)
        ano_int = ifelse(mes_int == 0, ano + 1, ano)
        # Servidor tá ativo?
        cadastro = cadastro %>%
          mutate(
            data = as.Date.character(paste0("01-", mes_int + 1, "-", ano_int), format = "%d-%m-%Y"),
            data = data - 1,
            ATIVIDADE = ifelse(
              is.na(DATA_INICIO_AFASTAMENTO) &
                is.na(DATA_TERMINO_AFASTAMENTO),
              1,
              0
            ),
            ATIVIDADE = ifelse(
              !is.na(DATA_INICIO_AFASTAMENTO) &
                data >= DATA_TERMINO_AFASTAMENTO,
              1,
              ATIVIDADE
            ),
            # Contagem de dias de serviço público e dias no órgão
            DIAS_ORGAO = ifelse(
              is.na(DATA_INGRESSO_ORGAO),
              NA,
              as.integer(data - DATA_INGRESSO_ORGAO)
            ),
            DIAS_SERVICO_PUBLICO = ifelse(
              is.na(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO),
              NA,
              as.integer(data - DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO)
            ),
            ANOS_ORGAO = DIAS_ORGAO / 365,
            ANOS_SERVICO_PUBLICO = DIAS_SERVICO_PUBLICO / 365
          )
        
        # Casos únicos
        unicos = cadastro %>%
          distinct(Id_SERVIDOR_PORTAL, id, .keep_all = T)
        
        # Salva o arquivo RDS para ter um backup de leitura mais rápida leitura
        write_rds(x = unicos, path = caminho_cadastro_RDS,compress = 'xz')
        # Remove Cadastro
        rm(cadastro)
        
        
        ## Agrupamentos: Quantidade total de servidores
        ## tempo de serviço público (desvio padrão e média)
        ## Tempo no órgão (desvio padrão e média)
        if (!exists("final_agregado")) {
          final_agregado = unicos %>%
            filter(ATIVIDADE == 1) %>%
            group_by(id, data) %>%
            summarise(
              qtde_servidores = n(),
              med_tempo_serv_orgao = mean(ANOS_ORGAO, na.rm = T),
              sd_tempo_serv_orgao = sd(ANOS_ORGAO, na.rm = T),
              med_tempo_serv_publico = mean(ANOS_SERVICO_PUBLICO, na.rm =
                                              T),
              sd_tempo_serv_publico = sd(ANOS_SERVICO_PUBLICO, na.rm = T)
            )
        }
        else{
          temp_agregado = unicos %>%
            filter(ATIVIDADE == 1) %>%
            group_by(id, data) %>%
            summarise(
              qtde_servidores = n(),
              med_tempo_serv_orgao = mean(ANOS_ORGAO, na.rm = T),
              sd_tempo_serv_orgao = sd(ANOS_ORGAO, na.rm = T),
              med_tempo_serv_publico = mean(ANOS_SERVICO_PUBLICO, na.rm =
                                              T),
              sd_tempo_serv_publico = sd(ANOS_SERVICO_PUBLICO, na.rm = T)
            )
          
          # Junta o dataframe temporário com o at
          final_agregado = rbindlist(list(final_agregado, temp_agregado))
          
          # Remove variável temporária
          rm(temp_agregado)
        }
        # Remove dataframe unificado final
        
        log_mensagem = paste0("Arquivos ",
                              caminho_cadastro,
                              " e ",
                              caminho_remunera,
                              " lidos com sucesso.")
      }
      else{
        log_mensagem = paste0("Não foi encontrado o arquivo: ", caminho_remunera)
      }
    }
    else{
      log_mensagem = paste0("Não foi encontrado o arquivo: ", caminho_cadastro)
    }
    dir.create(path = pasta_logs,showWarnings = F)
    cat("\n\n OS ARQUIVOS ", mes, "-", ano, " foram lidos")
    cat(
      paste0(date(), " - ", log_mensagem, "\n"),
      file = paste0(pasta_logs, "siape.log"),
      append = T
    )
    
    rm(log_mensagem)
  }
}
names(final_agregado) = iconv(names(final_agregado), to = "ASCII//TRANSLIT")
openxlsx::write.xlsx(final_agregado, file = painel_siape, asTable = T)
