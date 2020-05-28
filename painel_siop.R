library(openxlsx)
library(tidyverse)
library(stringi)
library(utf8)
rm(list = ls())

# Parametros e funções utilizadas
source("parametros.R")
source("funcoes.R")
# Nomes dos órgãos do SIOP
painel <- read.csv2(file = arquivo_siop, encoding = "UTF-8")
# Converte tudo pra caracter
painel[] <- lapply(painel, as.character)

# Altera os nomes
names(painel) <-
  tolower(iconv(names(painel), to = "ASCII//TRANSLIT"))
names(painel) <- gsub("[ .]", "_", names(painel))

# Remover casos com NA
painel = painel %>%
  mutate(orcamento_aprov_pl = projeto_de_lei) %>%
  select(-c(projeto_de_lei)) %>%
  filter(!(is.na(ano)), ano != "Total", orgao_orcamentario != "") %>%
  mutate(ano = as.integer(ano)) %>%
  filter(ano >= 2013 & ano <= 2019) %>%
  mutate(ano = as.character(ano))

# Variáveis numéricas 
variaveis_numericas = c(
  "orcamento_aprov_pl",
  "dotacao_inicial",
  "dotacao_atual",
  "empenhado",
  "liquidado",
  "pago"
)

# Conserto dos nomes
# Transformação em encoding ASCII para tirar acentos
painel$orgao_orcamentario = tolower(iconv(painel$orgao_orcamentario, from = "utf-8", to = "ASCII//TRANSLIT"))
painel$unidade_orcamentaria = tolower(iconv(painel$unidade_orcamentaria, from = "utf-8", to = "ASCII//TRANSLIT"))

# Cria ids de órgãos e unidades orçamentárias
painel$id_orgao_orcamentario = stri_extract(painel$orgao_orcamentario, regex = "^[0-9]+")
painel$id_unidade_orcamentaria = stri_extract(painel$unidade_orcamentaria, regex = "^[0-9]+")


painel$orgao_orcamentario = stri_replace(painel$orgao_orcamentario,
                                         regex = "[0-9]+ [-] ",
                                         replacement = "")
painel$unidade_orcamentaria = stri_replace(painel$unidade_orcamentaria,
                                           regex = "[0-9]+ [-] ",
                                           replacement = "")

painel$orgao_orcamentario = stri_replace(painel$orgao_orcamentario,
                                         regex = " [-] .*",
                                         replacement = "")
painel$unidade_orcamentaria = stri_replace(painel$unidade_orcamentaria,
                                           regex = " [-] .*",
                                           replacement = "")
# Subsituições:
substituir = read.csv2("substituidor.txt")

# Loop
for (i in 1:nrow(substituir)) {
  painel$orgao_orcamentario = gsub(
    pattern = substituir$orignal[i],
    replacement = substituir$substituido[i],
    x = painel$orgao_orcamentario
  )
  painel$unidade_orcamentaria = gsub(
    pattern = substituir$orignal[i],
    replacement = substituir$substituido[i],
    x = painel$unidade_orcamentaria
  )
}



# Transformar variaveis em numéricas
painel2 <- painel %>%
  mutate_at(
    .vars = vars(variaveis_numericas),
    .funs = ~ gsub(
      pattern = "[.]",
      replacement = "",
      x = .
    )
  ) %>%
  mutate_at(
    .vars = vars(variaveis_numericas),
    .funs = ~ gsub(
      pattern = "[,]",
      replacement = ".",
      x = .
    )
  ) %>%
  mutate_at(.vars = vars(variaveis_numericas),
            .funs = ~ as.numeric(.)/12)

painel2$orgao_orcamentario = toupper(painel2$orgao_orcamentario)
painel2$unidade_orcamentaria = toupper(painel2$unidade_orcamentaria)

# Salvar painel num arquivo xlsx
wb = createWorkbook()
addWorksheet(wb, sheetName = "painel")
writeData(wb = wb, sheet = "painel", x = painel2)
saveWorkbook(wb, file = painel_siop, overwrite = T)

