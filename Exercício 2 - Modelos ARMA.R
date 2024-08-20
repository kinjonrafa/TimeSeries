#Trabalho de Applied Time Series Analysis

#Alunos:
#Murilo Mineo Meira de Castro
#Hugo Luiz Zanotto Alves
#Rafael da Silva

# Atividade:
# 


# Importando as bibliotecas necessárias para a realização das Análises
library(GetBCBData)
library(urca)
library(tsibble)
library(fpp3)
library(ggplot2)
library(dplyr)
library(forecast)



#Obtendo a Série do IPCA
ipca_data <- gbcbd_get_series(id = 433, first.date = "2007-01-01", last.date = Sys.Date() )



# Convertendo a série para um tsibble

ipca =
  ipca_data %>%
  mutate(ref.date = yearmonth(ref.date)) %>%
  as_tsibble(index = ref.date)

#plotando Gráfico de Linhas para analisar a série

ggplot( ipca ) + geom_line(aes(x = ipca$ref.date , y =  ipca$value ), size = 0.5, colour = "black") +
  labs(x = "", y="") +
  theme(aspect.ratio=0.5) +
  ggtitle( "Inflação Mensal") +
  theme(plot.title = element_text( size = 18, hjust = 0.5) ) +
  theme(axis.text = element_text(size = 12) ) +
  theme(axis.title.y = element_text(size = 12, angle = 360, vjust = 0.5, hjust=1)) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size= 12) ) +
  labs( caption = "Dados: Banco Central do Brasil")

# Plotando o gráfico de subseries para visualizar o comportamento mensal

ipca %>%
  gg_subseries(value) +
  labs(title = "Subsérie do IPCA - Período de 2007 a 2025")+
  theme(plot.title = element_text(hjust = 0.5))


#a-) Identificando e descrevendo as principais características do IPCA

#Aplicando o ACF, PACF e Teste ADF
acf(ipca$value, main = "IPCA - ACF", col = "blue",ci.col = "black", lag.max = length(ipca$value) -1)
pacf(ipca$value, main = "IPCA - PACF", col = "red",ci.col = "black", lag.max = length(ipca$value) -1)
summary( ur.df( ipca$value, lags=3, type='none'))



#b-) Construindo os modelos AR, MA e ARMA e comparando

#Transformando a série em um objeto Time Series do R

ts_ipca = ts(ipca_data$value)
ts_ipca

#Construindo o Modelo AR(p) para a serie IPCA

#Modelo AR(p) de primeira ordem para a série
ar1_ipca = ts_ipca %>% as_tsibble() %>% model(ARIMA(ts_ipca ~ pdq(1,0,0)))
report(ar1_ipca)

#Modelo AR(p) de segunda ordem para a série
ar2_ipca = ts_ipca %>% as_tsibble() %>% model(ARIMA(ts_ipca ~ pdq(2,0,0)))
report(ar2_ipca)


#Construindo o modelo MA(q) para a série IPCA

#Modelo MA(q) de primeira ordem para a série IPCA
ma1_ipca = ts_ipca %>% as_tsibble() %>% model(ARIMA(ts_ipca ~ pdq(0,0,1)))
report(ma1_ipca)


#Modelo MA(q) de primeira ordem para a série IPCA
ma2_ipca = ts_ipca %>% as_tsibble() %>% model(ARIMA(ts_ipca ~ pdq(0,0,2)))
report(ma2_ipca)


#Construindo o modelo ARMA(p,q) de primeira ordem
arma_ipca = ipca %>% model(ARIMA(value ~ pdq(1,0,1)))
report(arma_ipca)



#c-) Coletando e analisando os resíduos do modelo


#Resíduos do modelo AR(p) de primeira ordem
residuals_ar1_ipca = residuals(ar1_ipca)

#Resíduos do modelo MA(q) de primeira ordem
residuals_ma1_ipca = residuals(ma1_ipca)

#Resíduos do modelo ARMA(p,q)
residuals_arma_ipca = residuals(arma_ipca)



#Analisando graficamente os resíduos


#plotando Gráfico de Linhas para analisar a série de resíduos do modelo AR
ggplot( residuals_ar1_ipca ) + 
  geom_line(aes(x = residuals_ar1_ipca$index , y =  residuals_ar1_ipca$.resid ), size = 0.5, colour = "darkblue") +
  labs(x = "", y="") +
  theme(aspect.ratio=0.5) +
  ggtitle( "Resíduos do Modelo AR") +
  theme(plot.title = element_text( size = 18, hjust = 0.5) ) +
  theme(axis.text = element_text(size = 12) ) +
  theme(axis.title.y = element_text(size = 12, angle = 360, vjust = 0.5, hjust=1)) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size= 12) ) +
  labs( caption = "Dados: Banco Central do Brasil")



#plotando Gráfico de Linhas para analisar a série de resíduos do modelo MA
ggplot( residuals_ma1_ipca ) + 
  geom_line(aes(x = residuals_ma1_ipca$index , y =  residuals_ma1_ipca$.resid ), size = 0.5, colour = "purple") +
  labs(x = "", y="") +
  theme(aspect.ratio=0.5) +
  ggtitle( "Resíduos do Modelo MA") +
  theme(plot.title = element_text( size = 18, hjust = 0.5) ) +
  theme(axis.text = element_text(size = 12) ) +
  theme(axis.title.y = element_text(size = 12, angle = 360, vjust = 0.5, hjust=1)) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size= 12) ) +
  labs( caption = "Dados: Banco Central do Brasil")


#plotando Gráfico de Linhas para analisar a série de resíduos do modelo ARMA
ggplot( residuals_arma_ipca ) + 
  geom_line(aes(x = residuals_arma_ipca$ref.date , y = residuals_arma_ipca$.resid ), size = 0.5, colour = "red") +
  labs(x = "", y="") +
  theme(aspect.ratio=0.5) +
  ggtitle( "Resíduos do Modelo ARMA") +
  theme(plot.title = element_text( size = 18, hjust = 0.5) ) +
  theme(axis.text = element_text(size = 12) ) +
  theme(axis.title.y = element_text(size = 12, angle = 360, vjust = 0.5, hjust=1)) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size= 12) ) +
  labs( caption = "Dados: Banco Central do Brasil")


#Plotando os gráficos das funções de Autocorrelação dos resíduos dos modelos

#Autocorrelação FAC dos resíduos do modelo AR(p)
ggAcf(residuals_ar1_ipca)


#Autocorrelação FAC dos resíduos do modelo MA(q)
ggAcf(residuals_ma1_ipca)

#Autocorrelação FAC dos resíduos do modelo ARMA(p,q)
ggAcf(residuals_arma_ipca)
