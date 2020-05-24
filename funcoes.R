# Funcao converter
converter = function(variavel) {
  variavel = as.character(variavel)
  variavel = gsub(pattern = " ", replacement = "", variavel)
  variavel = gsub(pattern = "[.]", replacement = "", variavel)
  variavel = gsub(pattern = ",", replacement = ".", variavel)
  variavel = as.numeric(variavel)
  
  return(variavel)
}

agregador = function(base,variaveis_grupo,variaveis_id,variaveis_calculadas){
  if(missing(variaveis_id)){
    variaveis_id = NULL
  }
  base_final = base %>%
    mutate( # Quantidade de servidores, sem distinção
      Quantidade_diferente = ifelse(is.null(variaveis_id),
                                    n(),
                                    n_distinct(vars(variaveis_id),na.rm = T))) %>%
    group_by_at(.vars = variaveis_grupo) %>%
    summarise(# Quantidade de servidores
      Quantidade_Igual = n(),
      Quantidade_diferente = max(Quantidade_diferente))
  
  
  # medias
  for(i in variaveis_calculadas){
    # Base com valores médios
    base_media = base %>%
      group_by_at(.vars = variaveis_grupo) %>%
      # Médias dos arquivos
      summarise_at(.vars = vars(i),.funs = ~mean(.,na.rm = T))
    # Renomeia base
    
    # medianas
    base_mediana = base %>%
      group_by_at(.vars = variaveis_grupo) %>%
      # Médias dos arquivos
      summarise_at(.vars = vars(i),.funs = ~median(.,na.rm = T))
    # Renomeia base
    
    # mínimos
    base_minima = base %>%
      group_by_at(.vars = variaveis_grupo) %>%
      # Médias dos arquivos
      summarise_at(.vars = vars(i),.funs = ~min(.,na.rm = T))
    
    # máximos
    base_maximo = base %>%
      group_by_at(.vars = variaveis_grupo) %>%
      # Médias dos arquivos
      summarise_at(.vars = vars(i),.funs = ~max(.,na.rm = T))
    
    # desvio padrão
    base_desvios = base %>%
      group_by_at(.vars = variaveis_grupo) %>%
      # Médias dos arquivos
      summarise_at(.vars = vars(i),.funs = ~sd(.,na.rm = T))
    
    # Base com soma
    base_total = base %>%
      group_by_at(.vars = variaveis_grupo) %>%
      # Médias dos arquivos
      summarise_at(.vars = vars(i),.funs = ~sum(.,na.rm = T))
    # Renomeia base
    
    medias = paste0("MEDIA_",i)
    minimos = paste0("MIN_",i)
    maximos = paste0("MAX_",i)
    medianas = paste0("MEDIANA_",i)
    desvios = paste0("SD_",i)
    total = paste0("TOTAL_",i)
    
    # Mesclagem de base
    base_final[medias] = base_media[i]
    base_final[desvios] = base_media[i]
    base_final[medianas] = base_mediana[i]
    base_final[maximos] = base_maximo[i]
    base_final[minimos] = base_minima[i]
    base_final[total] = base_total[i]
    
    
    cat("\n",i)
  }
  
  
  return(base_final)
}

# Função ajusta data
ajusta_data = function(variavel) {
  variavel = as.character(variavel)
  variavel = as.Date.character(variavel, format = "%d/%m/%Y")
  
  return(variavel)
}

# Remoção caracteres nome
remove_nome = function(variavel) {
  variavel = as.character(variavel)
  variavel = gsub(pattern = " ", replacement = "_", variavel)
  variavel = gsub(pattern = "[.]", replacement = "", variavel)
  variavel = gsub(pattern = ",", replacement = ".", variavel)
  variavel = as.numeric(variavel)
  
  return(variavel)
}

correlacionador = function(base, eliminar) {
  # Elimina o desnecessário
  base3 = base %>%
    select(-eliminar)
  
  # Quantidade de linhas
  n_linhas = length(names((base3)))
  n_colunas = n_linhas + 2
  
  # Cria a matriz
  nomes_colunas = c("MEDIA", "SD", names(base3))
  nomes_linhas = names(base3)
  matriz = matrix(
    0,
    nrow = n_linhas,
    ncol = n_colunas,
    dimnames = list(nomes_linhas, nomes_colunas)
  )
  
  
  for (i in nomes_linhas) {
    for (j in nomes_colunas) {
      if (i == j) {
        matriz[i, j] = 1
      }
      else{
        #   # Se for media
        if (j %in% c("MEDIA", "SD")) {
          if (j == "MEDIA") {
            matriz[i, j] = mean(base3[, i], na.rm = T)
          }
          if (j == "SD") {
            matriz[i, j] = sd(base3[, i], na.rm = T)
          }
        }
        else{
          matriz[i, j] = cor(base3[i], base3[j], use = "complete.obs")
        }
      }
      cat("\n Coluna: ", j, " Linha:", i)
    }
    
    
  }
  # matriz = as.data.frame(matriz,row.names = nomes_linhas)
  matriz = cbind.data.frame(nomes_linhas, matriz)
  return(matriz)
}

# função pra transformar
logistic_transform = function(regressao, parametros) {
  coeficientes = regressao$coefficients
  cat("\n\n", sum(coeficientes))
  cat("\n\nOS COEFICIENTES ANTES DA MULTIPLICACAO", coeficientes)
  coeficientes = coeficientes * parametros
  # cat("\n\n",sum(coeficientes))
  cat("\n\nOS COEFICIENTES DEPOIS DA MULTIPLICACAO", coeficientes)
  media_ef_fix = mean(regressao$alpha)
  resultado = 1 / (1 + exp(-sum(coeficientes) - media_ef_fix))
  
  
  # cat('\n\n\n O valor dos coeficientes é:  ', (-sum(coeficientes+media_ef_fix)))
  
  # cat('\n\n\n O valor dos coeficientes fora é:  ', (-sum(coeficientes)-media_ef_fix))
  
  
  cat('\n\nO RESULTADO É: ', resultado * 100, "%\n\n")
  
  
  return(resultado)
  
  
  
}
marginal.effects <- function(x, sims = 1000) {
  set.seed(1984)
  pdf <-
    ifelse(
      as.character(x$call)[3] == "binomial(link = \"probit\")",
      mean(dnorm(predict(x, type = "link"))),
      mean(dlogis(predict(x, type = "link")))
    )
  pdfsd <-
    ifelse(as.character(x$call)[3] == "binomial(link = \"probit\")",
           sd(dnorm(predict(x, type = "link"))),
           sd(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf * coef(x)
  sim <- matrix(rep(NA, sims * length(coef(x))), nrow = sims)
  for (i in 1:length(coef(x))) {
    sim[, i] <- rnorm(sims, coef(x)[i], diag(vcov(x) ^ 0.5)[i])
  }
  pdfsim <- rnorm(sims, pdf, pdfsd)
  sim.se <- pdfsim * sim
  res <- cbind(marginal.effects, sd(sim.se))
  colnames(res)[2] <- "standard.error"
  ifelse(names(x$coefficients[1]) == "(Intercept)",
         return(res[2:nrow(res), ]), return(res))
}

fitted.bife <- function(object, ...) {
  X <-
    model.matrix(object[["formula"]], object[["data"]], rhs = 1L)[,-1L, drop = FALSE]
  x <- as.vector(X %*% object[["coefficients"]])
  x <-
    x + object[["alpha"]][as.integer(object[["data"]][[ncol(object[["data"]])]])]
  object[["family"]][["linkinv"]](x)
}

marginal.effects.fixed <- function(x, nsims = 1000) {
  set.seed(1984)
  pdf <- mean(dlogis(-log((1 - fitted(
    x
  )) / fitted(x))))
  pdfsd <- sd(dlogis(-log((1 - fitted(
    x
  )) / fitted(x))))
  marginal.effects <- pdf * fixed.effects(x)
  sim <- matrix(rep(NA, nsims * length(fixef(x))), nrow = nsims)
  for (i in 1:length(fixef(x))) {
    sim[, i] <- rnorm(nsims, fixef(x)[i], diag(vcov(x) ^ 0.5)[i])
  }
  pdfsim <- rnorm(nsims, pdf, pdfsd)
  sim.se <- pdfsim * sim
  res <- cbind(marginal.effects, sd(sim.se))
  colnames(res)[2] <- "standard.error"
  ifelse(names(fixef(x))[1] == "(Intercept)",
         return(res[2:nrow(res), ]), return(res))
}


# EFEITOS FIXOS
mfx <- function(x, sims = 1000) {
  set.seed(1984)
  pdf <- ifelse(as.character(x$family$link) == "probit",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  pdfsd <- ifelse(as.character(x$family$link) == "probit",
                  sd(dnorm(predict(x, type = "link"))),
                  sd(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf * coef(x)
  sim <- matrix(rep(NA, sims * length(coef(x))), nrow = sims)
  for (i in 1:length(coef(x))) {
    sim[, i] <- rnorm(sims, coef(x)[i], diag(vcov(x) ^ 0.5)[i])
  }
  pdfsim <- rnorm(sims, pdf, pdfsd)
  sim.se <- pdfsim * sim
  res <- cbind(marginal.effects, sd(sim.se))
  colnames(res)[2] <- "standard.error"
  ifelse(names(x$coefficients[1]) == "(Intercept)",
         return(res[2:nrow(res), ]), return(res))
}

# criação meses antes depois imposição
# Essa função gera o número de meses até a origem
quantidade_meses <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
  lt$year * 12 + lt$mon
}
# Faz a diferença
diferenca_mes <- function(data_inicial, data_comparada) {
  quantidade_meses(data_inicial) - quantidade_meses(data_comparada)
}



# Modelo de hausman
hausman_test = function(efeito_fixo, efeito_randomico) {
  cov(efeito_fixo$coefficients[1], )
}