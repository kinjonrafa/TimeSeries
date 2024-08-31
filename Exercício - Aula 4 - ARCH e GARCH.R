# Exercício Aula 4 - ARCH e GARCH ----------------------------------------

#Objetivo:
#Calcular métricas de risco - Value at Risk (VaR) e Expected Shortfall (ES)

#Material: Série S&P 500 diária de 1990 até 2024

# Limpando o Ambiente -----------------------------------------------------
rm(list = ls())
cat("\014")

#Instalando Bibliotecas
#install.packages("quantmod")
#install.packages("data.table")


# Carregando as Bibliotecas Necessárias -----------------------------------

library(data.table)
library(tsibble)
library(forecast)
library(tseries)
library(zoo)
library(rugarch)
library(FinTS)
library(stats)
library(lmtest)
library(quantmod)


# Obtendo a Série S&P 500 (Ticker GSPC - Yahoo Finance) ------------------------
sp500_data_raw <- getSymbols("^GSPC" , src = "yahoo", from = "1990-01-02",
                             to = Sys.Date(), auto.assign = FALSE)

sp500_data <- data.table(Date = index(sp500_data_raw) , 
                         Close = coredata(Cl(sp500_data_raw)))



# Convertendo a série em um objeto Time Series ---------------------------------
sp500_ts <- ts(sp500_data$Close.GSPC.Close, frequency = 252, start = c(1990,1))


# Plotando a Série Original em Nível -------------------------------------------
plot(sp500_ts, main = "Índice S&P 500 em Nível", ylab = "Índice" ,
     xlab = "Ano")



# Função de Autocorrelação (FAC) da série em Nível -----------------------------
acf(na.omit(sp500_ts),main = "FAC da Série em Nível (Índice S&P500)",
                       ylab = "Autocorrelação",
                       xlab = "Defasagem (Lags)")



# Função de Autocorrelação Parcial (FACP) da série em Nível --------------------
pacf(na.omit(sp500_ts), main = "FACP da Série em Nível (Índice S&P500)",
     ylab = "Autocorrelação Parcial",
     xlab = "Defasagem (Lags)")


# Teste de Phillips-Perron na série original (Nível) ----------------------

pp_test_nivel <- pp.test(na.omit(sp500_ts))
print(pp_test_nivel)


# Teste ADF na Série Original
adf_test_nivel <- adf.test(na.omit(sp500_ts))
print(adf_test_nivel)


#Seguindo o procedimento para ajuste da série, vamos remover a estacionariedade
#para remover a estacionariedade iremos diferenciar a série

sp500_diff <- diff(sp500_ts)

#plotar o gráfico da série diferenciada
plot(sp500_diff, main = "Índice S&P 500 Diferenciado",
     ylab = "Diferença", xlab = "Ano")


#Analisando a função de autocorrelação da série diferenciada
acf(na.omit(sp500_diff), main = "FAC da Série Diferenciada (Índice S&P 500)",
    ylab = "Autocorrelação", xlab = "Defasagem (LAGS)" )


#Analisando a função de autocorrelação parcial da série diferenciada
pacf(na.omit(sp500_diff), main = "PACF da Série Diferenciada (Índice S&P 500)" ,
     ylab = "Autocorrelação Parcial" , xlab = "Defasagem(LAGS)")


# teste Phillips-Perron na série diferenciada
pp_test_diff <- pp.test(na.omit(sp500_diff))
print(pp_test_diff)


#Teste ADF na série diferenciada
adf_test_diff <- adf.test(na.omit(sp500_diff))
print(adf_test_diff)


#Estimando o modelo ARMA e avaliando a qualidade dos modelos

# Realizar o Teste Ljung-Box (ARIMA)
#residuos_arima_squared <- residuos_arima^2
#ljung_box_test_arima <- Box.test(residuos_arima_squared, lag = 10, type = "Ljung-Box")
#print(ljung_box_test_arima)



#Construindo um Modelo ARMA(p,q) ~ (0,1)
modelo_arma <- arima(sp500_diff, order = c(0,0,1), include.mean = TRUE)
summary(modelo_arma)


#Verificando os resíduos do modelo ARMA(0,1)
checkresiduals(modelo_arma)


#ACF e PACF no modelo ARMA(0,1)
acf(residuals(modelo_arma), main="Modelo ARMA(0,1) - ACF Resíduos",
    col="darkblue")

pacf(residuals(modelo_arma), main="Modelo ARMA(0,1) - PACF Resíduos",
    col="darkblue")



#Realizando o teste AutoRegressivo

#Conditional Heteroskedasticity (Heterocedasticidade Condicional)

#Teste ARCH LM

#Executando a função archtest() nos resíduos dos modelos ARIMA(3,1,0) e ARMA(0,1)


#Executando o teste de Heterocedasticidade Condicional nos resíduos do modelo ARMA(0,1)

residuos_arma <- residuals(modelo_arma)
arch_test_arma <- ArchTest(residuos_arma)
print(arch_test_arma)


# Realizar o Teste Ljung-Box (ARMA)
residuos_arma_squared <- residuos_arma^2
ljung_box_test_arma <- Box.test(residuos_arma_squared, lag = 10, type = "Ljung-Box")
print(ljung_box_test_arma)


#Verificar a FAC dos resíduos ao quadrado
acf(residuals(modelo_arma)^2, main = "FAC dos resíduos do modelo ARMA ao Quadrado")


#Elevar ao Quadrado a série diferenciada
sp500_diff_squared <- sp500_diff^2


#Plot da série diferenciada elevada ao quadrado
plot(sp500_diff_squared, main = "Série S&P500 Diferenciada ao Quadrado",
     ylab = "Diferença ao Quadrado", xlab = "Ano")



#Analisando a função de autocorrelação da série diferenciada ao quadrado
acf(na.omit(sp500_diff_squared),
    main = "FAC da Série S&P500 ao Quadrado",
    ylab = "Autocorrelação", xlab = "Defasagem (LAGS)")



#Analisando a autocorrelação parcial da série diferenciada ao quadrado
pacf(na.omit(sp500_diff_squared),
     main = "FACP da Série S&P500 ao Quadrado", 
     ylab = "Autocorrelação Parcial", xlab = "Defasagem (LAGS)")



#Calcular a média móvel da variância da série S&P500 (volatilidade condicional)
volatilidade_condicional <- zoo::rollapply(sp500_diff_squared,
                                           width = 20, 
                                           FUN = mean, 
                                           fill = NA,
                                           align = "right")

#Plotar a volatilidade condicional
plot(volatilidade_condicional, type = "l", 
     main = "Volatilidade Condicional (Média Móvel da Variância)",
     ylab = "Volatilidade Condicional",
     xlab = "Ano",
     col = "black"
     )





#Especificação do Modelo ARCH (Ordem 1 / Primeira Ordem)
spec_arch <- ugarchspec(variance.model = list(model = "sGARCH",
                                              garchOrder = c(1,0)),
                        
                        mean.model = list(armaOrder = c(0,0),
                                          include.mean = FALSE)
                        )

#Ajustando o modelo ARCH
modelo_arch <- ugarchfit(spec = spec_arch, 
                         data = sp500_diff)

#Resumo do Modelo
print(modelo_arch)



# Plotar a FAC dos resíduos padronizados
acf(residuals(modelo_arch, standardize = TRUE),
    main = "FAC dos Resíduos Padronizados pelo DP do ARCH")


#Plotando a série do Modelo ARCH / Plot Selection - 3 - Conditional SD (vs returns)
plot(modelo_arch, which = 3)


#Seguindo com as análises, verificamos a necessidade de um modelo com maior complexidade
#vamos construir o modelo GARCH e validar seus resultados

#Ajustando o Modelo GARCH de primeira ordem (GARCH de Ordem 1)

#especificando o modelo GARCH
spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0), include.mean = FALSE)
                         )
#Ajustando o modelo GARCH
modelo_garch <- ugarchfit(spec = spec_garch, data = sp500_diff)

#Plotando o resumo do modelo GARCH
print(modelo_garch)


# Plotar a FAC dos resíduos padronizados
acf(residuals(modelo_garch, standardize = TRUE), main = "FAC dos Resíduos Padronizados pelo DP do GARCH")


#Plotando a série do Modelo GARCH
plot(modelo_garch, which = 3)


#Comparando os modelos ARCH(1) e GARCH(1,1) utilizando a função infocriteria()

#Info Critéria - Modelo ARCH(1)
infocriteria(modelo_arch)

#Info Critéria - Modelo GARCH(1,1)
infocriteria(modelo_garch)

#Prevendo a volatilidade futura do S&P500 para os próximos 3 dias - Modelo GARCH
garch_forecast <- ugarchforecast(modelo_garch , n.ahead = 3)

plot(garch_forecast, which = 1)


#Calculando o Value at Risk (VaR)

#Extraindo o desvio padrão (sigma)
sigma_forecast <- sigma(garch_forecast)

#Setando o nível de confiança para o VaR
confidence_level <- 0.95
q <- qnorm(confidence_level)

#Calculando o VaR
var <- -q * sigma_forecast

#Calculando o expected Shortfall (ES)
es <- -dnorm(q) / (1 - confidence_level) * sigma_forecast


# Printando o VaR
print(var)

# Calculando o Expected Shortfall
print(es)






# Código Inutilizado ------------------------------------------------------


#Modelo Arima utilizando a função auto.arima()
#modelo_arima <- auto.arima(sp500_diff)
#summary(modelo_arima)


#Verificando os resíduos do modelo ARIMA(3,1,0)
#checkresiduals(modelo_arima)


#Aplicando a função de Autocorrelação e autocorrelação parcial nos resíduos dos modelos ARIMA(3,1,0) e ARMA(0,1)

#ACF e PACF no modelo ARIMA(3,1,0)
#acf(residuals(modelo_arima), main="Modelo ARIMA(3,1,0) - ACF Resíduos", 
#    col="darkred")

#pacf(residuals(modelo_arima), main="Modelo ARIMA(3,1,0) - PACF Resíduos", 
#     col="darkred")


#Executando o teste de Heterocedasticidade Condicional nos resíduos do modelo ARIMA(3,1,0)

#residuos_arima <- residuals(modelo_arima)
#arch_test_arima <- ArchTest(residuos_arima)
#print(arch_test_arima)

