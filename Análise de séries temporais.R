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
  setwd("/home/pedro/Documents/Economia/Monografia/Dados")

  

# 1. Carregando dados e criando series temporais; 
  # devido aos diferentes períodos disponiveis para os dados, foram criadas duas variáveis
  dados1 = read_excel("Séries temporais.xlsx", sheet = 1, range = NULL) # A partir de 1965
  dados2 = read_excel("Séries temporais.xlsx", sheet = 2, range = NULL) # A partir de 1970

  pibreal    = ts(dados1$PIBreal,    start = c(1965,1), end = c(2020,1), frequency = 1)
  gerhidro   = ts(dados1$GerHidro,   start = c(1965,1), end = c(2020,1), frequency = 1)
  gertermo   = ts(dados1$GerTermo,   start = c(1965,1), end = c(2020,1), frequency = 1)
  gertotal   = ts(dados2$GerTotal,   start = c(1970,1), end = c(2020,1), frequency = 1)
 

  # 1.1. Diferenças
  difpibreal   = diff(pibreal, lag = 1, differences = 1)
  dif2pibreal  = diff(pibreal, lag = 1, differences = 2)
  difgerhidro  = diff(gerhidro, lag = 1, differences = 1)
  difgertermo  = diff(gertermo, lag = 1, differences = 1)
  difgertotal  = diff(gertotal, lag = 1, differences = 1)
  dif2gertotal = diff(gertotal, lag = 1, differences = 2)


# 2. Gerando graficos 
  
  # 2.1. PIB real, primeira e segunda diferenças
  ts.plot(pibreal, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "R$ Tri de Dezembro de 2020", xlab = "Ano", 
          main= "PIB real de 1965 a 2020")
  
  ts.plot(difpibreal, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "R$ Tri de Dezembro de 2020", xlab = "Ano", 
          main= "1ª diferença do PIB real de 1966 a 2020")
  abline(h = mean(difpibreal), lty = "dashed", col = "black", lwd = 1)
  
  ts.plot(dif2pibreal, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "R$ Tri de Dezembro de 2020", xlab = "Ano", 
          main= "2ª diferença do PIB real de 1967 a 2020")
  abline(h = mean(dif2pibreal), lty = "dashed", col = "black", lwd = 1)
  
  # 2.2. Geracao hidreletrica e primeira diferenca
  ts.plot(gerhidro, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "Geração hidrelétrica de 1965 a 2020")
  
  ts.plot(difgerhidro, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "1ª diferença da geração hidrelétrica de 1966 a 2020")
  abline(h = mean(difgerhidro), lty = "dashed", col = "black", lwd = 1)
  
  # 2.3. Geracao termeletrica e primeira diferenca
  ts.plot(gertermo, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "Geração termoelétrica de 1965 a 2020")
  
  ts.plot(difgertermo, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "1ª diferença da geração termoelétrica de 1966 a 2020")
  abline(h = mean(difgertermo), lty = "dashed", col = "black", lwd = 1)
  
  # 2.4. Geracao eletrica total, primeira e segunda diferencas
  ts.plot(gertotal, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "Geração elétrica total de 1970 a 2020")
  
  ts.plot(difgertotal, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "1ª diferença da geração elétrica total de 1971 a 2020")
  abline(h = mean(difgertotal), lty = "dashed", col = "black", lwd = 2)
  
  ts.plot(dif2gertotal, 
          type = "l", lwd = "3", col = "2", 		
          ylab= "TWh", xlab = "Ano", 
          main= "2ª diferença da geração elétrica total de 1972 a 2020")
  abline(h = mean(dif2gertotal), lty = "dashed", col = "black", lwd = 1)



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
  adf.test(difpibreal)
  adf.test(difgerhidro)
  adf.test(difgertermo)
  adf.test(difgertotal)
  
  adf.test(dif2pibreal)
  adf.test(dif2gertotal)
  
  
  
# 6. Estimando modelos VAR 
  
  # 6.0. Realizando corte de variáveis, para garantir sua compatibilidade
  difgerhidro1967 = window(difgerhidro, start = 1967)
  difgertermo1967 = window(difgertermo, start = 1967)
  dif2pibreal1972 = window(dif2pibreal, start = 1972)
  
  # 6.1. Preparando dados 
  dadosVAR1 = ts.union(dif2pibreal,  difgerhidro1967)  # PIB x Geração hidrelétrica
  dadosVAR2 = ts.union(dif2pibreal,  difgertermo1967)  # PIB x Geração termelétrica
  dadosVAR3 = ts.union(dif2pibreal1972, dif2gertotal) # PIB x Geração elétrica total
  
  # 6.2. Selecionando numero adequado de defasagens
  VARselect(dadosVAR1, lag.max = 10, type = "const", season = NULL, exogen = NULL)
  VARselect(dadosVAR2, lag.max = 10, type = "const", season = NULL, exogen = NULL)
  VARselect(dadosVAR3, lag.max = 10, type = "const", season = NULL, exogen = NULL)
  
  # 6.3. Estimando modelos  
  modelo1 = VAR(dadosVAR1, p = 1, type = "const", season = NULL, exogen = NULL)
  modelo2 = VAR(dadosVAR2, p = 1, type = "const", season = NULL, exogen = NULL)
  modelo3 = VAR(dadosVAR3, p = 1, type = "const", season = NULL, exogen = NULL)
  
  modelo1
  modelo2
  modelo3
  
  summary(modelo1, equation = "dif2pibreal")
  summary(modelo2, equation = "dif2pibreal")
  summary(modelo3, equation = "dif2pibreal1972")  

  # 6.4. Testando para autocorrelação dos resíduos
  dwtest(modelo1$varresult$dif2pibreal,     alternative = "two.sided")
  dwtest(modelo2$varresult$dif2pibreal,     alternative = "two.sided")
  dwtest(modelo3$varresult$dif2pibreal1972, alternative = "two.sided")
  
  
  
# 7. Testes de causalidade de Granger
  causality(modelo1, cause = "difgerhidro1967")$Granger
  causality(modelo2, cause = "difgertermo1967")$Granger
  causality(modelo3, cause = "dif2gertotal")$Granger
