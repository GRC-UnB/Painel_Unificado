# Este código chama os outros
library(tidyverse)
# Abre as funções
rm(list = ls())
source("parametros.R")
source(funcoes)

# Leitura de todos os arquivos a serem mesclados
# ids: dicionário que permite unificação
ids = openxlsx::read.xlsx(xlsxFile = arquivo_ids, sheet = "DADOS")
names(ids) = tolower(gsub(pattern = "[.-]",replacement = "_",x = names(ids)))
painel_SIAPE = openxlsx::read.xlsx(xlsxFile = painel_siape)
painel_SIAPE_remunera =  openxlsx::read.xlsx(xlsxFile = painel_siape_remuneracao)
painel_CHEFIA = openxlsx::read.xlsx(xlsxFile = painel_chefia)
painel_LAI = openxlsx::read.xlsx(xlsxFile = painel_lai)
painel_PEP = openxlsx::read.xlsx(xlsxFile = painel_pep_nvl_sup)
painel_TCU2018 = openxlsx::read.xlsx(xlsxFile = painel_tcu_2018)
painel_TCU2017 = openxlsx::read.xlsx(xlsxFile = painel_tcu_2017)
painel_TCU2016 = openxlsx::read.xlsx(xlsxFile = painel_tcu_2016)
painel_SIOP = openxlsx::read.xlsx(xlsxFile = painel_siop)
painel_SIORG = openxlsx::read.xlsx(xlsxFile = painel_siorg)


#### NOMES SIAPE ####
nomes <- ids %>%
   filter(fonte == "SIAPE") %>%
   select(-c(fonte, sigla, id_siape, nome))


# SIAPE servidores
painel_SIAPE <-
   merge(x = painel_SIAPE,
         y = nomes,
         by = "id")

# SIAPE remunerações
painel_SIAPE_remunera <-
   merge(x = painel_SIAPE_remunera,
         y = nomes,
         by = "id")

# SIAPE cargos
painel_CHEFIA <-
   merge(x = painel_CHEFIA,
         y = nomes,
         by = "id")


#### PAINEL DA LAI ####
nomes = ids %>%
   filter(fonte %in% c("GRC-INT", "INT", "POSIC")) %>%
   select(-c(fonte, id, id_siape,nome)) %>%
distinct(nome_tratado, .keep_all = T)

painel_LAI = merge(y = painel_LAI,
                   x = nomes,
                   by.y = "nome",
                   by.x = "nome_tratado")


#### PAINEL PEP ####

### CRUZEI COM PAINEL SIAPE PORQUE O PEP NÃO FUNCIONA
nomes = ids %>%
   filter(fonte == "SIAPE") %>%
   select(-c(fonte, sigla, id_siape, nome))

painel_PEP = merge(x = nomes,
                   y = painel_PEP,
                   by = "id")


# #### PAINEL TCU 2018 ####
nomes = ids %>%
   filter(fonte == "TCU2018") %>%
   select(-c(fonte, sigla, id_siape,nome))

# painel_TCU2018$ID = as.character(painel_TCU2018$ID)

painel_TCU2018 = merge(x = painel_TCU2018,
                       y = nomes,
                       by = "id")

# #### PAINEL TCU 2017 ####
nomes = ids %>%
   filter(fonte == "TCU2017") %>%
   select(-c(fonte, sigla, id_siape,nome))

painel_TCU2017 = merge(x = painel_TCU2017,
                       y = nomes,
                       by = "id")

#### PAINEL TCU 2016 ####
nomes = ids %>%
   filter(fonte == "TCU2016") %>%
   select(-c(fonte, sigla, id_siape,nome))

painel_TCU2016 = merge(x = painel_TCU2016,
                       y = nomes,
                       by = "id")


# Junta as bases do TCU em uma só

TCU = bind_rows(painel_TCU2018,painel_TCU2017)
TCU = bind_rows(TCU,painel_TCU2016)

###### PAINEIS UNIFICADOS ######
painel_UNIFICADO = merge(
   x = painel_SIAPE,
   y = painel_SIAPE_remunera,
   by.x = c("nome_tratado", "data"),
   by.y = c("nome_tratado", "data"),
   all.x = T,
   all.y = T,
   no.dups = T
)

# CORREÇÃO DE COLUNAS DUPLAS
nomes.y = grep(pattern = "[.]y", names(painel_UNIFICADO))
if (length(nomes.y) > 0) {
   painel_UNIFICADO = painel_UNIFICADO[-nomes.y]
   nomes.x = gsub(
      pattern = "[.]x",
      replacement = "",
      x = names(painel_UNIFICADO)
   )
   names(painel_UNIFICADO) =  nomes.x
}

painel_UNIFICADO = merge(
   x = painel_CHEFIA,
   y = painel_UNIFICADO,
   by.x = c("nome_tratado", "data"),
   by.y = c("nome_tratado", "data"),
   all.x = T,
   all.y = T,
   no.dups = T
)

# CORREÇÃO DE COLUNAS DUPLAS
nomes.y = grep(pattern = "[.]y", names(painel_UNIFICADO))
if (length(nomes.y) > 0) {
   painel_UNIFICADO = painel_UNIFICADO[-nomes.y]
   nomes.x = gsub(
      pattern = "[.]x",
      replacement = "",
      x = names(painel_UNIFICADO)
   )
   names(painel_UNIFICADO) =  nomes.x
}

painel_UNIFICADO = painel_UNIFICADO %>%
   group_by(nome_tratado,data) %>%
   distinct(nome_tratado,.keep_all = T) %>%
   ungroup()

painel_UNIFICADO = merge(
   x = painel_LAI,
   y = painel_UNIFICADO,
   by.x = c("nome_tratado", "data"),
   by.y = c("nome_tratado", "data"),
   all.x = T,
   all.y = T,
   no.dups = T
)

# CORREÇÃO DE COLUNAS DUPLAS
nomes.y = grep(pattern = "[.]y", names(painel_UNIFICADO))
if (length(nomes.y) > 0) {
   painel_UNIFICADO = painel_UNIFICADO[-nomes.y]
   nomes.x = gsub(
      pattern = "[.]x",
      replacement = "",
      x = names(painel_UNIFICADO)
   )
   names(painel_UNIFICADO) =  nomes.x
}

painel_UNIFICADO = merge(
   x = painel_UNIFICADO,
   y = painel_PEP,
   by.x = c("nome_tratado", "data"),
   by.y = c("nome_tratado", "data"),
   all.x = T,
   all.y = T,
   no.dups = T
)

# CORREÇÃO DE COLUNAS DUPLAS
nomes.y = grep(pattern = "[.]y", names(painel_UNIFICADO))
if (length(nomes.y) > 0) {
   painel_UNIFICADO = painel_UNIFICADO[-nomes.y]
   nomes.x = gsub(
      pattern = "[.]x",
      replacement = "",
      x = names(painel_UNIFICADO)
   )
   names(painel_UNIFICADO) =  nomes.x
}
##################### código até aqui unifica bases mensais ##################


# Conversão de variáveis de data
painel_UNIFICADO$data = openxlsx::convertToDate(painel_UNIFICADO$data)

# Cria variável de ano
painel_UNIFICADO$ano = format(painel_UNIFICADO$data, "%Y")

# Seleciona as variáveis da base unificada

###
painel_UNIFICADO = merge(
   x = painel_UNIFICADO,
   y = painel_SIOP,
   by.x = c("nome_tratado", "ano"),
   by.y = c("unidade_orcamentaria", "ano"),
   all.x = T,
   no.dups = T
)


# CORREÇÃO DE COLUNAS DUPLAS
nomes.y = grep(pattern = "[.]y", names(painel_UNIFICADO))
if (length(nomes.y) > 0) {
   painel_UNIFICADO = painel_UNIFICADO[-nomes.y]
   nomes.x = gsub(
      pattern = "[.]x",
      replacement = "",
      x = names(painel_UNIFICADO)
   )
   names(painel_UNIFICADO) =  nomes.x
}

painel_UNIFICADO = merge(
   x = painel_UNIFICADO,
   y = TCU,
   by.x = c("nome_tratado", "ano"),
   by.y = c("nome_tratado", "ano"),
   all.x = T,
   no.dups = T
)


painel_UNIFICADO = painel_UNIFICADO %>%
   select(-c(nome,orgao_orcamentario,id_orgao_orcamentario,id_unidade_orcamentaria))

nrow(painel_UNIFICADO)

openxlsx::write.xlsx(painel_UNIFICADO, file = painel_final)
