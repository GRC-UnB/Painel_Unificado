library(openxlsx)
library(tidyverse)
rm(list = ls())

source("parametros.R")

# DADOS TCU 2018 --------------
# Nomes dos órgãos do TCU
painel_dados2018 <- read.csv2(file = arquivo_tcu_2018,encoding = "latin1") 
painel_nomes = names(painel_dados2018)

# Variáveis numéricas
indices_tcu = c("iGG","iGovTI","iGovPub","iGovContrat","iGovPessoas")


# Variáveis a serem mantidas
painel_nomes = c("idBase","iGG","iGovPub","iGovContrat","iGovPessoas","iGovTI")

# Elimina as variáveis desnecessárias
painel_dados2018 = subset(x = painel_dados2018, select = painel_nomes)

# Ajusta o nome do ID
names(painel_dados2018) = c("id","iGG","iGovPub","iGovContrat","iGovPessoas","iGovTI")

# Ajusta o ano da base
painel_dados2018$ano = "2018"

painel_dados2018 = painel_dados2018 %>%
  mutate_at(.vars = indices_tcu,
            .funs = ~ifelse(!is.na(.) & . <0.15,"inexpressivo",
                            ifelse(!is.na(.) & .>= 0.15 &. <0.40,"iniciando",
                                   ifelse(!is.na(.) & .>= 0.40 &. <=0.70,"intermediario",
                                          ifelse(!is.na(.) & . >0.7,"aprimorado",.)))))

# Escrita da base
openxlsx::write.xlsx(painel_dados2018, file = painel_tcu_2018)

rm(painel_dados2018)
# Leitura painel 2016 --------------
# painel_nomes <- read.xlsx(xlsxFile = arquivo_tcu, sheet = "ID2018")
painel_dados2016 <-
  read.xlsx(xlsxFile = arquivo_tcu_2016,
            sheet = "Respostas",
            startRow = 3)

# Nome das variáveis no painel
painel_nomes = names(painel_dados2016)

# Seleciona os casos que não tenha números na composição
painel_nomes = grep(
  x = painel_nomes,
  pattern = "(^[0-9]|Utilidade)",
  value = T,
  invert = T
)

# Elimina as variáveis desnecessárias
painel_dados2016 = subset(x = painel_dados2016, select = painel_nomes)

# Ano da base
painel_dados2016$ano = "2016"

# Tira numeros da base
names(painel_dados2016) = c("id","iGovTI","ano")

painel_dados2016 = painel_dados2016 %>%
  mutate(iGovTI = ifelse(!is.na(iGovTI) & iGovTI <0.15,"inexpressivo",
                            ifelse(!is.na(iGovTI) & iGovTI>= 0.15 & iGovTI <0.40,"iniciando",
                                   ifelse(!is.na(iGovTI) & iGovTI >= 0.40 & iGovTI <=0.70,"intermediario",
                                          ifelse(!is.na(iGovTI) & iGovTI >0.7,"aprimorado",iGovTI)))))

# Salva a base
openxlsx::write.xlsx(painel_dados2016, file = painel_tcu_2016)

rm(painel_dados2016)
# Leitura dados de 2017 ----------------
painel_dados2017 = read.csv2(file = arquivo_tcu_2017,encoding = "latin1")

# Seleciona os casos que não tenha números na composição
painel_nomes = c("idBase","iGG","iGovPub","iGovContrat","iGovPessoas","iGovTI")
# Elimina as variáveis desnecessárias
painel_dados2017 = subset(x = painel_dados2017, select = painel_nomes)

# Ajusta nomes
names(painel_dados2017) = c("id","iGG","iGovPub","iGovContrat","iGovPessoas","iGovTI")

# Ano do painel
painel_dados2017$ano = "2017"

# Classificaçoes TCU
painel_dados2017 = painel_dados2017 %>%
  mutate_at(.vars = indices_tcu,
            .funs = ~ifelse(!is.na(.) & . <0.15,"inexpressivo",
                            ifelse(!is.na(.) & .>= 0.15 &. <0.40,"iniciando",
                                   ifelse(!is.na(.) & .>= 0.40 &. <=0.70,"intermediario",
                                          ifelse(!is.na(.) & . >0.7,"aprimorado",.)))))


# Salva o arquivo
openxlsx::write.xlsx(painel_dados2017, file = painel_tcu_2017)
