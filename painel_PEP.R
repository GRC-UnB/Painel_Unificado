library(tidyverse)
library(openxlsx)

# Abre as funções
rm(list = ls())
source("parametros.R")
source(funcoes)

# Leitura da terceira planilha
pep = openxlsx::read.xlsx(xlsxFile = arquivo_PEP, sheet = "PEP-2013-2019")
# Conversão da data
pep$data = openxlsx::convertToDate(pep$Data)

# Cria a variável de escolaridade e agrupa 
pep2 = pep %>%
  mutate(escolaridade = ifelse(`ESC-PAD` >= 5, "SUP", "MED"),
         id = as.character(`ID-SIAPE`),
         nome = Orgão) %>%
  group_by(data, `ID-SIAPE`, Orgão) %>%
  mutate(total_servidor_por_orgao = sum(Servidores)) %>%
  ungroup()

# Agrupamento por órgão e escolaridade
# Criação de variáveis relativas
pep2 = pep2 %>%
  group_by(data, id, nome,escolaridade) %>%
  summarise(
    med_idade = weighted.mean(Média.de.Idade, Servidores),
    servidores_escol = sum(Servidores),
    serv_escol_niv_super = sum(Servidores / total_servidor_por_orgao),
    total_servidores_pep = mean(total_servidor_por_orgao)
  ) %>%
  ungroup()

# ajuste de variáveis com caracteres com problemas de encoding
names(pep2) = iconv(names(pep2), to = "ASCII//TRANSLIT")

# Painel novo agrupado em xlsx
openxlsx::write.xlsx(pep2, "PEP/painel_PEP_COMPLETO.xlsx")

# Seleciona apenas o nível superior
painel_pep = pep2 %>%
  filter(escolaridade == "SUP") %>%
  select(-c(escolaridade))

# Salvamento das bases em xlsx
openxlsx::write.xlsx(pep2, file = painel_pep_completo)
openxlsx::write.xlsx(painel_pep, file = painel_pep_nvl_sup)
