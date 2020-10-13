library(tidyverse)
library(ggplot2)
library(openxlsx)
library(plotly)
library(htmlwidgets)

rm(list=ls())

source("funcoes.R")

dados = read.xlsx("dados/PAINEL_FINAL.xlsx")


write.csv(dados,file = "GRC_UNB.csv")
dados$Data = convertToDate(dados$Data)
dados = dados %>%
  mutate(politica_grc = factor(politica_grc,levels = c(0,1),labels = c("Não adotou","Adotou")),
         politica_posic = factor(politica_posic,levels = c(0,1),labels = c("Não adotou","Adotou")),
         plano_int = factor(plano_int,levels = c(0,1),labels = c("Não adotou","Adotou")),
         iGG = ifelse(iGG == "inexpressivo","0-Inexpressivo",iGG),
         iGG = ifelse(iGG == "iniciando","1-Iniciando",iGG),
         iGG = ifelse(iGG == "intermediario","2-Intermediário",iGG),
         iGG = ifelse(iGG == "aprimorado","3-Aprimorado",iGG))

#### MEDIA ####
remuneracao_media_grc = dados %>%
  filter(!is.na(politica_grc)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_grc) %>%
  summarise(media = weighted.mean(x = med_remunera_liquida,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

remuneracao_media_posic = dados %>%
  filter(!is.na(politica_posic)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_posic) %>%
  summarise(media = weighted.mean(x = med_remunera_liquida,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

remuneracao_media_int = dados %>%
  filter(!is.na(plano_int)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,plano_int) %>%
  summarise(media = weighted.mean(x = med_remunera_liquida,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

#### MEDIANA ####

remuneracao_mediana_grc = dados %>%
  filter(!is.na(politica_grc)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_grc) %>%
  summarise(media = median(x = med_remunera_liquida,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

remuneracao_mediana_posic = dados %>%
  filter(!is.na(politica_posic)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_posic) %>%
  summarise(media = median(x = med_remunera_liquida,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

remuneracao_mediana_int = dados %>%
  filter(!is.na(plano_int)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,plano_int) %>%
  summarise(media = median(x = med_remunera_liquida,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

#### TEMPO DE SERVIÇO ####

tempo_serv_grc = dados %>%
  filter(!is.na(politica_grc)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_grc) %>%
  summarise(media = weighted.mean(x = med_tempo_serv_publico,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

tempo_serv_posic = dados %>%
  filter(!is.na(politica_posic)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_posic) %>%
  summarise(media = weighted.mean(x = med_tempo_serv_publico,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

tempo_serv_int = dados %>%
  filter(!is.na(plano_int)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,plano_int) %>%
  summarise(media = weighted.mean(x = med_tempo_serv_publico,w = qtde_servidores,na.rm = T)) %>%
  ungroup()

#### Execução orçamentária ####
orcamento_grc = dados %>%
  filter(!is.na(politica_grc)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_grc) %>%
  summarise(orcado = mean(x = dotacao_atual,na.rm = T),
            pago = mean(x = pago,na.rm = T)) %>%
  multi_spread(key = politica_grc,value = c(orcado,pago)) %>%
  ungroup()

orcamento_posic = dados %>%
  filter(!is.na(politica_posic)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_posic) %>%
  summarise(orcado = mean(x = dotacao_atual,na.rm = T),
            pago = mean(x = pago,na.rm = T)) %>%
  multi_spread(key = politica_posic,value = c(orcado,pago))%>%
  ungroup()

orcamento_plaint = dados %>%
  filter(!is.na(plano_int)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,plano_int) %>%
  summarise(orcado = mean(x = dotacao_atual,na.rm = T),
            pago = mean(x = pago,na.rm = T)) %>%
  multi_spread(key = plano_int,value = c(orcado,pago)) %>%
  ungroup()

##### EVOLUÇÃO NO TEMPO ####

quantidade_grc = dados %>%
  filter(!is.na(politica_grc)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_grc) %>%
  summarise(media = n()) %>%
  ungroup()

quantidade_posic = dados %>%
  filter(!is.na(politica_posic)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,politica_posic) %>%
  summarise(media = n()) %>%
  ungroup()

quantidade_int = dados %>%
  filter(!is.na(plano_int)) %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(ano,plano_int) %>%
  summarise(media = n()) %>%
  ungroup()


###### NIVEL DE CAPACIDADE DE GOVERNANÇA #####
igg_serv = dados %>%
  filter(!is.na(iGG)) %>%
  # mutate(ano = as.integer(ano)) %>%
  group_by(ano,iGG) %>%
  summarise(media = median(qtde_servidores,na.rm = T)) %>%
  ungroup()

igg_remuneracao_media = dados %>%
  filter(!is.na(iGG)) %>%
  # mutate(ano = as.integer(ano)) %>%
  group_by(ano,iGG) %>%
  summarise(media = weighted.mean(med_remunera_liquida,qtde_servidores,na.rm = T)) %>%
  ungroup()

igg_tempo_serv = dados %>%
  filter(!is.na(iGG)) %>%
  # mutate(ano = as.integer(ano)) %>%
  group_by(ano,iGG) %>%
  summarise(media = weighted.mean(med_tempo_serv_publico,qtde_servidores,na.rm = T)) %>%
  ungroup()

igg_idade = dados %>%
  filter(!is.na(iGG)) %>%
  # mutate(ano = as.integer(ano)) %>%
  group_by(ano,iGG) %>%
  summarise(media = weighted.mean(med_idade,qtde_servidores,na.rm = T)) %>%
  ungroup()

igg_orcamento = dados %>%
  filter(!is.na(iGG)) %>%
  # mutate(ano = as.integer(ano)) %>%
  group_by(ano,iGG) %>%
  summarise(orcado = mean(dotacao_atual,na.rm = T),
            pago = mean(pago,na.rm = T)) %>%
  ungroup() %>%
  gather(key = conta,value = Valor,-c(ano,iGG)) %>%
  mutate(conta = ifelse(conta == "orcado","Autorizado","Pago"),
         conta = paste0(ano," - ", conta))

##### graficos  #####
#### MEDIA remuneração líquida ####
graph1_grc_remuneracao_media = plot_ly(data = remuneracao_media_grc,
                               x = ~ ano,
                               y = ~ media,
                               color = ~politica_grc,
                               type = "scatter",
                               mode = "lines+markers") %>%
  layout(title = "Remuneração Média dos órgãos por adoção de política de GRC",
         yaxis = list(title = "Remuneração Média em R$"))

graph1_posic_remuneracao_media = plot_ly(data = remuneracao_media_posic,
                                x = ~ ano,
                                y = ~ media,
                                color = ~politica_posic,
                                type = "scatter",
                                mode = "lines+markers") %>%
  layout(title = "Remuneração Média dos órgãos por adoção de POSIC",
         yaxis = list(title = "Remuneração Média em R$"))

graph1_plaint_remuneracao_media = plot_ly(data = remuneracao_media_int,
                                x = ~ ano,
                                y = ~ media,
                                color = ~plano_int,
                                type = "scatter",
                                mode = "lines+markers") %>%
  layout(title = "Remuneração Média dos órgãos por adoção de plano de integridade",
         yaxis = list(title = "Remuneração Média em R$"))

####### grc mediana #######
graph2_grc_remuneracao_mediana = plot_ly(data = remuneracao_mediana_grc,
                               x = ~ ano,
                               y = ~ media,
                               color = ~politica_grc,
                               type = "scatter",
                               mode = "lines+markers") %>%
  layout(title = "Remuneração mediana dos órgãos por adoção de política de GRC",
         yaxis = list(title = "Remuneração Média em R$"))

graph2_posic_remuneracao_mediana = plot_ly(data = remuneracao_mediana_posic,
                                        x = ~ ano,
                                        y = ~ media,
                                        color = ~politica_posic,
                                        type = "scatter",
                                        mode = "lines+markers") %>%
  layout(title = "Remuneração mediana dos órgãos por adoção de POSIC",
         yaxis = list(title = "Remuneração Média em R$"))

graph2_plaint_remuneracao_mediana = plot_ly(data = remuneracao_mediana_int,
                                        x = ~ ano,
                                        y = ~ media,
                                        color = ~plano_int,
                                        type = "scatter",
                                        mode = "lines+markers") %>%
  layout(title = "Remuneração mediana dos órgãos por adoção de plano de integridade",
         yaxis = list(title = "Remuneração Média em R$"))


#### tempo servico ######
graph3_grc_tempo_serv = plot_ly(data = tempo_serv_grc,
                                  x = ~ ano,
                                  y = ~ media,
                                  color = ~politica_grc,
                                  type = "scatter",
                                  mode = "lines+markers") %>%
  layout(title = "Tempo médio de serviços dos servidores por adoção de política de GRC",
         yaxis = list(title = "Tempo médio de serviço em anos"))

graph3_plaint_tempo_serv = plot_ly(data = tempo_serv_int,
                         x = ~ ano,
                         y = ~ media,
                         color = ~plano_int,
                         type = "scatter",
                         mode = "lines+markers") %>%
  layout(title = "Tempo médio de serviços dos servidores por adoção de plano de integridade",
         yaxis = list(title = "Tempo médio de serviço em anos"))

graph3_posic_tempo_serv = plot_ly(data = tempo_serv_posic,
                         x = ~ ano,
                         y = ~ media,
                         color = ~politica_posic,
                         type = "scatter",
                         mode = "lines+markers") %>%
  layout(title = "Tempo médio de serviços dos servidores por adoção da POSIC",
         yaxis = list(title = "Tempo médio de serviço em anos"))


#### Orçamento ####
graph4_grc_orcamento = plot_ly(data = orcamento_grc,
                        x = ~ ano,
                        y = ~ Adotou_orcado,
                        colors = "orange",
                        name = "Orçado - Adotou",
                        type = "scatter",
                        mode = "lines+markers") %>%
  add_trace(y = ~ `Não adotou_orcado`,name = "Orçado - Não adotou", colors = "orange2") %>%
  add_trace(y = ~ `Não adotou_pago`,name = "Pago - Não adotou", colors = "orange2") %>%
  add_trace(y = ~ `Adotou_pago`,name = "Pago - Adotou", colors = "orange5") %>%
  layout(title = "Orçamento médio dos órgãos por adoção de política de GRC",
         yaxis = list(title = "Orçamento médio em R$"))

graph4_posic_orcamento = plot_ly(data = orcamento_posic,
                        x = ~ ano,
                        y = ~ Adotou_orcado,
                        colors = "orange",
                        name = "Orçado - Adotou",
                        type = "scatter",
                        mode = "lines+markers") %>%
  add_trace(y = ~ `Não adotou_orcado`,name = "Orçado - Não adotou", colors = "orange2") %>%
  add_trace(y = ~ `Não adotou_pago`,name = "Pago - Não adotou", colors = "orange2") %>%
  add_trace(y = ~ `Adotou_pago`,name = "Pago - Adotou", colors = "orange5") %>%
  layout(title = "Orçamento médio dos órgãos por adoção da posic",
         yaxis = list(title = "Orçamento médio em R$"))

graph4_plaint_orcamento = plot_ly(data = orcamento_plaint,
                          x = ~ ano,
                          y = ~ Adotou_orcado,
                          colors = "orange",
                          name = "Orçado - Adotou",
                          type = "scatter",
                          mode = "lines+markers") %>%
  add_trace(y = ~ `Não adotou_orcado`,name = "Orçado - Não adotou", colors = "orange2") %>%
  add_trace(y = ~ `Não adotou_pago`,name = "Pago - Não adotou", colors = "orange2") %>%
  add_trace(y = ~ `Adotou_pago`,name = "Pago - Adotou", colors = "orange5") %>%
  layout(title = "Orçamento médio dos órgãos por adoção da posic",
         yaxis = list(title = "Orçamento médio em R$"))

######### EVOLUÇÃO ########
graph5_grc_evolucao = plot_ly(data = quantidade_grc,
                        x = ~ ano,
                        y = ~ media,
                        color = ~politica_grc,
                        type = "scatter",
                        mode = "lines+markers") %>%
  layout(title = "Orçamento médio dos órgãos por adoção de política de GRC",
         yaxis = list(title = "Orçamento médio em R$"))

graph5_posic_evolucao = plot_ly(data = quantidade_posic,
                       x = ~ ano,
                       y = ~ media,
                       color = ~politica_posic,
                       type = "scatter",
                       mode = "lines+markers") %>%
  layout(title = "Orçamento médio dos órgãos por adoção de política de GRC",
         yaxis = list(title = "Orçamento médio em R$"))

graph5_int_evolucao = plot_ly(data = quantidade_int,
                       x = ~ ano,
                       y = ~ media,
                       color = ~plano_int,
                       type = "scatter",
                       mode = "lines+markers") %>%
  layout(title = "Orçamento médio dos órgãos por adoção de política de GRC",
         yaxis = list(title = "Orçamento médio em R$"))

######## IGG #####
graph6_igg_mediana_servidores = 
  plot_ly(data = igg_serv,
          x = ~ iGG,
          y = ~ media,
          color = ~ano,
          type = "bar") %>%
  layout(title = "Mediana da Quantidade de Servidores por\nNível de Capacidade de Governança (IGG)",
         yaxis = list(title = "Quantidade de servidores no órgão"))

graph6_igg_tempo_medio = 
  plot_ly(data = igg_tempo_serv,
          x = ~ iGG,
          y = ~ media,
          color = ~ano,
          type = "bar") %>%
  layout(title = "Tempo médio de serviço dos Servidores por\nNível de Capacidade de Governança (IGG)",
         yaxis = list(title = "Tempo de serviço em anos"))

graph6_igg_idade_media = 
  plot_ly(data = igg_idade,
          x = ~ iGG,
          y = ~ media,
          color = ~ano,
          type = "bar") %>%
  layout(title = "Idade média dos Servidores por\nNível de Capacidade de Governança (IGG)",
         yaxis = list(title = "Idade em anos"))


graph6_igg_orcamento = plot_ly(data = igg_orcamento,
          x = ~ iGG,
          y = ~Valor,
          color = ~conta,
          type = "bar") %>%
  layout(title = "Mediana da Quantidade de Servidores por\nNível de Capacidade de Governança (IGG)",
         yaxis = list(title = "Valores em R$"))


lista = list()
informacoes = ls()
informacoes = grep("graph[0-9]",x = informacoes,value = T)


for(i in informacoes){
  lista[[i]] = get(i)
}

saveRDS(object = lista,file = "graficos.RDS")

for(i in informacoes){
  htmlwidgets::saveWidget(widget = lista[[i]],file = paste0(i,".html"),selfcontained = T)
}
