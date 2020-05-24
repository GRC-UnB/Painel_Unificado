library(data.table)
library(tidyverse)
library(openxlsx)
rm(list = ls())

source("parametros.R")
substituir = read.csv2("substituidor.txt")

# Abertura da base
siorg = fread(input = arquivo_siorg)

# Classificação da base
siorg = siorg %>%
  mutate(nome = Orgao_superior,
         id = codigo_siorg,
         nat_juridica = Natureza_J) %>%
  group_by(nome) %>%
  mutate(qtde_unidades = n()) %>%
  ungroup() %>%
  filter(denominacao_unidade == nome) %>%
  select(
    -c(
      nivel,
      codigo_hierarquico,
      denominacao_unidade,
      nivel_norma,
      Tipo_de_unidade,
      codigo_siorg,
      Natureza_J,
      Orgao_superior
    )
  )



openxlsx::write.xlsx(siorg,file = painel_siorg)
