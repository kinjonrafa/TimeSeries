#Exercício da Aula 3 - Testes de Estacionariedade


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
library(ggplot2)
library(scales)
library(dplyr)
library(fpp3)




#Para iniciar o exercício, vamos obter a série IBC-Br

#Série IBC-Br: Índice de Atividade Econômica do Banco Central
#Mede a evolução contemporânea da atividade econômica do país 

#Obter a série do IBC-Br
ibc_data <- data.table(gbcbd_get_series(id = 24363, first.date = "2015-01-01",last.date = Sys.Date()))



#Muitas vezes para realizar as análises de séries temporais é necessário
#a conversão dos objetos importados em formas de tabelas em objetos de séries temporais


#Abaixo seguem dois modos de coagir os dados em um objeto Time Series
#Convertendo a coluna ref.date em data
ibc = ibc_data %>%
      mutate(ref.date = yearmonth(ref.date)) %>%
      as_tsibble(index = ref.date)


#Converter a série para um objeto de série temporal
ibc_ts <- ts(ibc_data$value, frequency = 12, start = c(2015,01))



#plotando os Gráficos para analisar o comportamento da série

#Plotando o gráfico de linhas referente a série IBC-Br
ggplot(ibc_data) +
  geom_line(aes(x = ibc_data$ref.date , y = ibc_data$value), size = 0.5 , colour = "black") +
  labs(x = "", y="") +
  theme(aspect.ratio=0.5) +
  ggtitle( "Dados IBC-Br") +
  theme(plot.title = element_text( size = 18, hjust = 0.5) ) +
  theme(axis.text = element_text(size = 12) ) +
  theme(axis.title.y = element_text(size = 12, angle = 360, vjust = 0.5, hjust=1)) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size= 12) ) +
  labs( caption = "Dados: Banco Central do Brasil")

#Plotando o subplot para analisar os dados de maneira fragmentada
ibc %>%
  gg_subseries(value) +
  labs(title = "Subsérie do Índice IBC-Br - Período de 2015 a 2024")+
  theme(plot.title = element_text(hjust = 0.5))


#Aplicando as funções de autocorrelacao e autocorrelação parcial para a série IBC-Br
#Série IBC-Br em nível - sem nenhum tratamento

#FAC  - Função de Autocorrelação
acf(ibc$value, main = "IBC-Br - ACF", col = "blue",ci.col = "black", lag.max = length(ibc$value) -1)

#FACP - Função de Autocorrelação Parcial
pacf(ibc$value, main = "IBC-Br - PACF", col = "red",ci.col = "black", lag.max = length(ibc$value) -1)


#Decompondo a série para identificar algum padrão de tendência
#Realizando a decomposição sazonal da série temporal
#Para execução desta função precisamos utilizar o objeto Time Series Criado anteriormente

decomposed = stl(ibc_ts, s.window = "periodic")
autoplot(decomposed)




#Execucao do Teste ADF - Teste de Raiz Unitaria
adf_test_nivel_1 <- adf.test(na.omit(ibc_ts))
#Apresentando os valores do teste ADF
print(adf_test_nivel_1)




#Execucao do Teste ADF - Teste de Raiz Unitaria
adf_test_nivel_2 <- adf.test(na.omit(ibc$value))
#Apresentando os valores do teste ADF
print(adf_test_nivel_2)




#Teste Augmented Dickey-Fuller Test Unit Root Test - Teste Dickey-Fuller Aumentado/Raiz Unitária
summary( ur.df( na.omit(ibc_ts)))
summary( ur.df( na.omit(ibc$value)))
#Na próxima aula perguntar porque ao utilizar a função ur.df() os valores são diferentes do teste realizado via função adf.test()



#Aplicando o teste KPSS na serie original (Nivel)
kpss_test_nivel <- ur.kpss(ibc_ts, type = "mu")
summary(kpss_test_nivel)







#Provocando a estacionarieade na série IBC-Br

#Aplicar a diferenciacao na serie para remover a nao estacionariedade
ibc_diff <- diff(ibc_ts)


#Plotando o gráfico de linhas referente a série IBC-Br Diferenciada
plot(ibc_diff, main = "IBC-Br Diferenciado (Taxa de Crescimento)", ylab = "Diferença", xlab = "Ano")



#Aplicar a funcao de autocorrelacao na serie diferenciada
acf(na.omit(ibc_diff), main = "FAC da Serie Diferenciada (IBC-Br)",ylab = "Autocorrelacao", xlab = "Defasagem (Lags)")


#Aplicar o teste ADF na serie diferenciada
adf_test_diff <- adf.test(na.omit(ibc_diff))
print(adf_test_diff)


#Aplicando o teste KPSS na serie diferenciada
kpss_test_diff <- ur.kpss(ibc_diff, type = "mu")
summary(kpss_test_diff)



#Seguindo com o procedimento de Analises de Series Temporais
#Vamos aplicar o modelo ARIMA e verificar seus resultados

#Modelo Arima para a série original
modelo_arima_nivel = auto.arima(ibc_ts, max.P = 0, max.D = 0, max.Q = 0)
summary(modelo_arima_nivel)

#Modelo Arima para a série Diferenciada
modelo_arima_diff = auto.arima(ibc_diff, max.P = 0, max.D = 0, max.Q = 0)
summary(modelo_arima_diff)



#Aplicando a função de Autocorrelação nos resíduos dos modelos ARIMA
acf(residuals(modelo_arima_nivel), main="IBC/BR - ACF Resíduos em Nível", col="darkred", ci.col="black",lag.max=length(ibc_ts) - 1)
acf(residuals(modelo_arima_diff), main="IBC/BR - ACF Resíduos Diferenciada", col="darkred", ci.col="black",lag.max=length(ibc_ts) - 1)


#Aplicando Box test para validar se os resíduos são ruido branco
Box.test(residuals(modelo_arima_nivel))
Box.test(residuals(modelo_arima_diff))


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



#Estimando Modelos SARIMA - Considerando Sazonalidade no modelo


#Criando o Modelo ARIMA sem travas e com sazonalidade
modelo_sarima_nivel = auto.arima(ibc_ts)
summary(modelo_sarima_nivel)


#Criando o Modelo ARIMA sem travas e com sazonalidade
modelo_sarima_diff = auto.arima(ibc_diff)
summary(modelo_sarima_diff)



#Aplicando Box test para validar se os resíduos são ruido branco
Box.test(residuals(modelo_sarima_nivel))
Box.test(residuals(modelo_sarima_diff))





#Projeção para os próximos 6 meses SARIMA - IBC-Br em Nível
projecao_sarima_nivel = forecast(modelo_sarima_nivel, h = 6)
tail(ibc_ts)
projecao_sarima_nivel
autoplot(projecao_sarima_nivel)




#Projeção para os próximos 6 meses SARIMA - IBC-Br Diferenciado
projecao_sarima_diff = forecast(modelo_sarima_diff, h = 6)
tail(ibc_ts)
projecao_sarima_diff
autoplot(projecao_sarima_diff)


#Projeção para os próximos 12 meses SARIMA - IBC-Br em Nível
projecao_sarima_nivel_12 = forecast(modelo_sarima_nivel, h = 12)
tail(ibc_ts)
projecao_sarima_nivel_12
autoplot(projecao_sarima_nivel_12)

#Projeção para os próximos 12 meses SARIMA - IBC-Br Diferenciado
projecao_sarima_diff_12 = forecast(modelo_sarima_diff, h = 12)
tail(ibc_ts)
projecao_sarima_diff_12
autoplot(projecao_sarima_diff_12)














