#Exemplos da Aula 3 - Testes de Estacionariedade

#Limpando o ambiente
rm(list = ls())
cat("\014")

# Importando as Bibliotecas Necessárias

library(GetBCBData)
library(data.table)
library(tsibble)
library(forecast)
library(tseries)
library(urca)

#Para executar os exemplos em aula, vamos obter a série IBC-Br

#Série IBC-Br: Índice de Atividade Econômica do Banco Central
#Mede a evolução contemporânea da atividade econômica do país 

#Obter a série do IBC-Br
ibc_data <- data.table(gbcbd_get_series(id = 24363, first.date = "2015-01-01",last.date = Sys.Date()))


#Muitas vezes para realizar as análises de séries temporais é necessário
#a conversão dos objetos importados em formas de tabelas em objetos de séries temporais

#Converter a série para um objeto de série temporal
ibc_ts <- ts(ibc_data$value, frequency = 12, start = c(2015,01))


#Execucao do Teste ADF - Teste de Raiz Unitaria
adf_test_nivel <- adf.test(na.omit(ibc_ts))

#Apresentando os valores do teste ADF
print(adf_test_nivel)


#Executando a funcao de autocorrelacao para essa mesma serie
acf(na.omit(ibc_ts),main = "FAC da Serie em Nivel (IBC-Br)",ylab = "Autocorrelacao", xlab = "Defasagem (Lags)")


#Aplicar a diferenciacao na serie para remover a nao estacionariedade
ibc_diff <- diff(ibc_ts)

#Aplicar o teste ADF na serie diferenciada
adf_test_diff <- adf.test(na.omit(ibc_diff))
print(adf_test_diff)


#Aplicar a funcao de autocorrelacao na serie diferenciada
acf(na.omit(ibc_diff), main = "FAC da Serie Diferenciada (IBC-Br)",ylab = "Autocorrelacao", xlab = "Defasagem (Lags)")


#Aplicando o teste KPSS na serie original (Nivel)
kpss_test_nivel <- ur.kpss(ibc_ts, type = "mu")
summary(kpss_test_nivel)


#Aplicando o teste KPSS na serie diferenciada
kpss_test_diff <- ur.kpss(ibc_diff, type = "mu")
summary(kpss_test_diff)


#Aplicando o teste PP - Phillips-Perron na Serie Original (Nivel)
pp_test_nivel <- pp.test(na.omit(ibc_ts))
print(pp_test_nivel)

#Aplicando o teste PP - Phillips-Perron na Serie Diferenciada
pp_test_diff <- pp.test(na.omit(ibc_diff))
print(pp_test_diff)


#Plotando as series original e diferenciada
plot(ibc_ts, main = "IBC-Br em Nivel", ylab = "Valor" , xlab = "Ano")


#Plotando as series original e diferenciada
plot(ibc_diff, main = "IBC-Br Diferenciada", ylab = "Valor" , xlab = "Ano")


#Seguindo com o procedimento de Analises de Series Temporais
#Vamos aplicar o modelo ARIMA e verificar seus resultados

#Modelo Arima para a série original
modelo_arima_nivel = auto.arima(ibc_ts, max.P = 0, max.D = 0, max.Q = 0)
summary(modelo_arima_nivel)

#Modelo Arima para a série Diferenciada
modelo_arima_diff = auto.arima(ibc_diff, max.P = 0, max.D = 0, max.Q = 0)
summary(modelo_arima_diff)


#Projeções utilizando os modelos ARIMA

#Projeção para séries em nivel
projecao_nivel = forecast(modelo_arima_nivel, h = 6)
tail(ibc_ts)

projecao_nivel

autoplot(projecao_nivel)


#Projeção da série diferenciada
projecao_diff = forecast(modelo_arima_diff, h = 6)
tail(ibc_diff)
projecao_diff

autoplot(projecao_diff)

