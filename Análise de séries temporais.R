#######################################################################
# Análise de custo-benefício da geração elétrica por diferentes fontes
# Pedro Luiz Rosa Furlan - RA 156997
# Orientação: Alexandre Gori Maia
#######################################################################

# 0. Preâmbulo

 # 0.1. Instalando e carregando pacotes necessários
  install.packages("readxl")
  install.packages("vars")
  install.packages("ggplot2")
  install.packages("tseries")
  install.packages("mFilter")
  install.packages("forecast")
  install.packages("lmtest")
  
  library(readxl)
  library(vars)
  library(ggplot2)
  library(tseries)
  library(mFilter)
  library(forecast)
  library(lmtest)

 # 0.2. Definindo diretorio de trabalho
  setwd("/home/pedro/Documents/Economia/MONOGRAFIA/Dados")

  

# 1. Carregando dados e criando series temporais; 
  # devido aos diferentes períodos disponiveis para os dados, foram criadas quatro variáveis
  dados1 = read_excel("Séries temporais.xlsx", sheet = 1, range = NULL) # A partir de 1965
  dados2 = read_excel("Séries temporais.xlsx", sheet = 2, range = NULL) # A partir de 1966
  dados3 = read_excel("Séries temporais.xlsx", sheet = 3, range = NULL) # A partir de 1970
  dados4 = read_excel("Séries temporais.xlsx", sheet = 4, range = NULL) # A partir de 1971

  pibreal    = ts(dados1$PIBreal,    start = c(1965,1), end = c(2020,1), frequency = 1)
  varpib     = ts(dados2$PIBTxVar,   start = c(1966,1), end = c(2020,1), frequency = 1)
  gerhidro   = ts(dados1$GerHidro,   start = c(1965,1), end = c(2020,1), frequency = 1)
  gertermo   = ts(dados1$GerTermo,   start = c(1965,1), end = c(2020,1), frequency = 1)
  gertotal   = ts(dados3$GerTotal,   start = c(1970,1), end = c(2020,1), frequency = 1)
  varpibagro = ts(dados4$PIBAgro,    start = c(1971,1), end = c(2020,1), frequency = 1)
  varpibind  = ts(dados4$PIBInd,     start = c(1971,1), end = c(2020,1), frequency = 1)
  varpibser  = ts(dados4$PIBServ,    start = c(1971,1), end = c(2020,1), frequency = 1)

  # 1.1. Diferenças
  difpibreal   = diff(pibreal, lag = 1, differences = 1)
  difgerhidro  = diff(gerhidro, lag = 1, differences = 1)
  difgertermo  = diff(gertermo, lag = 1, differences = 1)
  difgertotal  = diff(gertotal, lag = 1, differences = 1)
  dif2gertotal = diff(gertotal, lag = 1, differences = 2)



# 2. Gerando graficos 
  
  # 2.1. PIB real e primeira diferença
  ts.plot(pibreal, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "R$ de Dezembro de 2020", xlab = "Ano", 
          main= "PIB real de 1965 a 2020")
  
  ts.plot(difpibreal, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "R$ de Dezembro de 2020", xlab = "Ano", 
          main= "1ª diferença do PIB real de 1965 a 2020")
  abline(h = 0, lty = "dashed", col = "black", lwd = 1)
  
  # 2.2. Geracao hidreletrica e primeira diferenca
  ts.plot(gerhidro, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "Geração hidrelétrica de 1965 a 2020")
  
  ts.plot(difgerhidro, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "1ª diferença da geração hidrelétrica de 1965 a 2020")
  abline(h = 0, lty = "dashed", col = "black", lwd = 1)
  
  # 2.3. Geracao termeletrica e primeira diferenca
  ts.plot(gertermo, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "Geração termoelétrica de 1965 a 2020")
  
  ts.plot(difgertermo, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "1ª diferença da geração termoelétrica de 1965 a 2020")
  abline(h = 0, lty = "dashed", col = "black", lwd = 1)
  
  # 2.4. Geracao eletrica total, primeira e segunda diferencas
  ts.plot(gertotal, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "Geração elétrica total de 1970 a 2020")
  
  ts.plot(difgertotal, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "1ª diferença da geração elétrica total de 1970 a 2020")
  abline(h = 0, lty = "dashed", col = "black", lwd = 1)
  
  ts.plot(dif2gertotal, 
          type = "l", lwd = "2", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "2ª diferença da geração elétrica total de 1970 a 2020")
  abline(h = 0, lty = "dashed", col = "black", lwd = 1)



# 3. Funções de autocorrelação para verificar estacionariedade das series
  
  # 3.1. Pib real e primeira diferença
  acf(pibreal, lag.max = length(pibreal),
      xlab = "Defasagens", ylab = 'Autocorrelação', 
      main='Função de autocorrelação do PIB real')
  
  acf(difpibreal, lag.max = length(difpibreal),
      xlab = "Defasagens", ylab = 'Autocorrelação', 
      main='Função de autocorrelação da 1ª diferença do PIB real')
  
  # 3.2. Geração hidrelétrica e primeira diferença
  acf(gerhidro, lag.max = length(gerhidro),
      xlab = "Defasagens", ylab = 'Autocorrelação', 
      main='Função de autocorrelação da geração hidrelétrica')
  
  acf(difgerhidro, lag.max = length(difgerhidro),
      xlab = "Defasagens", ylab = 'Autocorrelação', 
      main='Função de autocorrelação da 1ª diferença da geração hidrelétrica')
 
  # 3.3. Geração termelétrica e primeira diferença
  acf(gertermo, lag.max = length(gertermo),
      xlab = "Defasagens", ylab = 'Autocorrelação', 
      main='Função de autocorrelação da geração termelétrica')
  
  acf(difgertermo, lag.max = length(difgertermo),
      xlab = "Defasagens", ylab = 'Autocorrelação', 
      main='Função de autocorrelação da 1ª diferença da geração termelétrica')
  
  # 3.4. Geração elétrica total
  acf(gertotal, lag.max = length(gertotal),
      xlab = "Defasagens", ylab = 'Autocorrelação', 
      main='Função de autocorrelação da geração elétrica total')
  
  acf(difgertotal, lag.max = length(difgertotal),
      xlab = "Defasagens", ylab = 'Autocorrelação', 
      main='Função de autocorrelação da 1ª diferença da geração elétrica total')
  
  
  
# 4. Aplicando o teste KPSS para estacionariedade com tendência
  kpss.test(pibreal, null = "Trend")
  kpss.test(gerhidro, null = "Trend")
  kpss.test(gertermo, null = "Trend")
  kpss.test(gertotal, null = "Trend")

  
  
# 5. Aplicando o teste de Dickey-Fuller para raízes unitárias
  adf.test(pibreal)
  adf.test(gerhidro)
  adf.test(gertermo)
  adf.test(gertotal)
  
  adf.test(difpibreal)
  adf.test(difgerhidro)
  adf.test(difgertermo)
  adf.test(difgertotal)
  
  

# 6. Estimando modelos VAR 
  
  # 6.1. Preparando dados 
    dadosVAR1 = ts.union(difpibreal, difgerhidro)
    dadosVAR2 = ts.union(difpibreal, difgertermo)
    difpibreal1971 = window(difpibreal, start = 1971)
    dadosVAR3 = ts.union(difpibreal1971, difgertotal)
    
  # 6.2. Selecionando numero adequado de defasagens
    VARselect(dadosVAR1, lag.max = 10, type = "const")
    VARselect(dadosVAR2, lag.max = 10, type = "const")
    VARselect(dadosVAR3, lag.max = 10, type = "const")
    
  # 6.3. Estimando modelos  
    modelo1 = VAR(dadosVAR1, p = 1, type = "both")
    modelo2 = VAR(dadosVAR2, p = 1, type = "both")
    modelo3 = VAR(dadosVAR3, p = 1, type = "both")
    
    modelo1
    modelo2
    modelo3
    
    summary(modelo1, equation = "difpibreal")
    summary(modelo2, equation = "difpibreal")
    summary(modelo3, equation = "difpibreal")
  
  
  
# 7. Testes de causalidade de Granger
  grangertest(difgerhidro, difpibreal, order = 10)
  grangertest(difgertermo, difpibreal, order = 10)
  grangertest(difgertotal, difpibreal, order = 10)
  