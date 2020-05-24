library(tidyverse)
library(openxlsx)

# Parametros inciais
rm(list = ls())
source("parametros.R")
source(funcoes)

# Abertura da base
base = openxlsx::read.xlsx(xlsxFile = arquivo_lai)

# Delimitação do período
ano_inicial = 2013
ano_finaL = 2019

# Criação de nomes de colunas e linhas
for (i in ano_inicial:ano_finaL) {
  if (i == ano_inicial) {
    nomes_datas = paste0(1:12, "-", i)
  }
  else{
    nomes_datas = c(nomes_datas, paste0(1:12, "-", i))
  }
}
# Nomes dos órgãos para nomear as linhas da matriz
nome_orgaos = base$ORG_EXERCICIO

# Criação de matrizes
Com_GRC = matrix(0,
                 nrow = nrow(base),
                 ncol = (ano_finaL - ano_inicial + 1) * 12)
# Nomeia coluna como data e linhas como órgão:
# Exemplo ---- 
# ----|Data1|Data2|....
# Org1|    0|    0|....
# Org2|    0|    0|.... 

colnames(Com_GRC) = nomes_datas
rownames(Com_GRC) = nome_orgaos

# Removendo variáveis
rm(nomes_datas, nome_orgaos)

# Criação de outras variáveis
Pol_GRC = Com_GRC
Pla_INT = Com_GRC
Com_INT = Com_GRC
Pol_SIC = Com_GRC
Com_SIC = Com_GRC


# Criação de variáveis de data
Com_GRC_Data = openxlsx::convertToDate(base$`Data-Com-GRC`)
Pol_GRC_Data = openxlsx::convertToDate(base$`Data-Pol-GRC`)
Com_INT_Data = openxlsx::convertToDate(base$`Data-Com-Int`)
Pla_INT_Data = openxlsx::convertToDate(base$`Data-Pla-Int`)
Pol_SIC_Data = openxlsx::convertToDate(base$`Data-Com-SIC`)
Com_SIC_Data = openxlsx::convertToDate(base$`Data-Pol-SIC`)

# Criação das matrizes de datas
# Faz o loop ano a ano e mês a mês, comparando cada data do nome da coluna
for (i in ano_inicial:ano_finaL) {
  for (j in 1:12) {
    # Cria o nome da coluna
    coluna = paste0(j, "-", i)
    mes_int = ifelse(j == 12, 0, j)
    ano_int = ifelse(j == 12, i + 1, i)
    # Cria a data para comparação
    data = as.Date.character(x = paste0("1-", mes_int + 1, "-", ano_int),
                             format  = "%d-%m-%Y")
    data = data - 1
    # Se a data de instituição/publicação for maior ou igual à data da coluna,
    # O valor dado é 1, se não, 0.
    Com_GRC[, coluna] = ifelse(Com_GRC_Data <= data &
                                 !is.na(Com_GRC_Data), 1, 0)
    Pol_GRC[, coluna] = ifelse(Pol_GRC_Data <= data &
                                 !is.na(Pol_GRC_Data), 1, 0)
    Com_SIC[, coluna] = ifelse(Com_SIC_Data <= data &
                                 !is.na(Com_SIC_Data), 1, 0)
    Pol_SIC[, coluna] = ifelse(Pol_SIC_Data <= data &
                                 !is.na(Pol_SIC_Data), 1, 0)
    Com_INT[, coluna] = ifelse(Com_INT_Data <= data &
                                 !is.na(Com_INT_Data), 1, 0)
    Pla_INT[, coluna] = ifelse(Pla_INT_Data <= data &
                                 !is.na(Pla_INT_Data), 1, 0)
    rm(coluna, data)
    # Mostra o mês e o ano no console
    cat("\n Ano: ", i, " Mês: ", j)
  }
}

#Remoção das variáveis de data e dos índices do loop
rm(list = grep("ata", ls(), value = T))

# Criação de data.frame
Com_GRC = as.data.frame(Com_GRC)
Pol_GRC = as.data.frame(Pol_GRC)
Com_INT = as.data.frame(Com_INT)
Pla_INT = as.data.frame(Pla_INT)
Com_SIC = as.data.frame(Com_SIC)
Pol_SIC = as.data.frame(Pol_SIC)


# Criação de variável órgão
Com_GRC$nome = row.names(Com_GRC)
Pol_GRC$nome = row.names(Pol_GRC)
Com_INT$nome = row.names(Com_INT)
Pla_INT$nome = row.names(Pla_INT)
Com_SIC$nome = row.names(Com_SIC)
Pol_SIC$nome = row.names(Pol_SIC)

# Criação de cada painel de dados
# Exclui a variável ORGAO do gather para poder separar e juntar para cada caso
Com_GRC = Com_GRC %>%
  gather(key = data, value = "comite_grc", -nome)
Pol_GRC = Pol_GRC %>%
  gather(key = data, value = "politica_grc", -nome)
Com_INT = Com_INT %>%
  gather(key = data, value = "comite_grc", -nome)
Pla_INT = Pla_INT %>%
  gather(key = data, value = "plano_int", -nome)
Com_SIC = Com_SIC %>%
  gather(key = data, value = "comite_posic", -nome)
Pol_SIC = Pol_SIC %>%
  gather(key = data, value = "politica_posic", -nome)

# Juntando cada dataframe acima em um único painel. Ele mescla usando as variáveis de nome igual
painel = merge(Com_GRC, Pol_GRC)
painel = merge(painel, Com_INT)
painel = merge(painel, Pla_INT)
painel = merge(painel, Com_SIC)
painel = merge(painel, Pol_SIC)

# Removendo variáveis não mais utilizadas
rm(Com_GRC, Pol_GRC, Com_INT, Pla_INT, Com_SIC, Pol_SIC)

# Unificando nomes dos órgãos
nomes = base %>%
  mutate(nome = ORG_EXERCICIO,
         id = COD_ORG_EXERCICIO) %>%
  select(id, nome)

painel = merge(nomes, painel, by = "nome")

# Remove nomes
rm(nomes)

# Correção das datas no painel
painel = painel %>%
  mutate(
    data = as.Date.character(paste0("1-", data), "%d-%m-%Y"),
    Ano = as.numeric(format(data, "%Y")),
    Mes = as.numeric(format(data, "%m")),
    Ano = ifelse(Mes == 12, Ano + 1, Ano),
    Mes = ifelse(Mes == 12, 1, Mes + 1),
    data = as.Date.character(paste0("1-", Mes, "-", Ano), "%d-%m-%Y"),
    data = data - 1
  ) %>%
  select(-c("Ano", "Mes"))

#### Momentos em que houve uma imposição
# Data imposição GRC
painel$imp_grc = ifelse(painel$data >= as.Date.character("10-05-2017", "%d-%m-%Y"),
                         1,
                         0)

# Data de adoção INT
painel$imp_int = ifelse(painel$data >= as.Date.character("11-11-2018", "%d-%m-%Y"),
                         1,
                         0)

# Data comite integridade
painel$imp_com_int = ifelse(painel$data >= as.Date.character("11-11-2018", "%d-%m-%Y"),
                             1,
                             0)

# Data comite grc
painel$imp_com_grc = ifelse(painel$data >= as.Date.character("10-05-2017", "%d-%m-%Y"),
                             1,
                             0)

# Momentos em que a imposição foi obrigatória
painel$imp_grc = factor(
  x = painel$imp_grc,
  levels = c(0, 1),
  labels = c("NAO_OBRIGA", "OBRIGATORIA")
)
painel$imp_int = factor(
  x = painel$imp_int,
  levels = c(0, 1),
  labels = c("NAO_OBRIGA", "OBRIGATORIA")
)
painel$imp_com_int = factor(
  x = painel$imp_com_int,
  levels = c(0, 1),
  labels = c("NAO_OBRIGA", "OBRIGATORIA")
)
painel$imp_com_grc = factor(
  x = painel$imp_com_grc,
  levels = c(0, 1),
  labels = c("NAO_OBRIGA", "OBRIGATORIA")
)

names(painel) = iconv(names(painel), to = "ASCII//TRANSLIT")

openxlsx::write.xlsx(painel, file = painel_lai, asTable = T)


rm(list = ls())
