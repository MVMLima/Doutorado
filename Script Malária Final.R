#Abrir o banco de dados e definir diretório de trabalho

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Malária/Banco Malária.RData")
setwd("C:/Users/Lucas/Desktop/Resultados Doutorado/Malária")

#Carregar pacotes para análise e transformação do banco em série temporal
library(forecast)
library(TSA)
library(urca)
library(tseries)
library(ggplot2)
library(seasonal)
library(tsoutliers)
library(expsmooth)
library(fma)
library(nnfor)


#Determinando a série temporal (h=12)
set.seed(123)
x <- ts(malaria$TO, start = c(1997,1), frequency = 12)
x = log(x)
test_x <- window(x, start=c(2016,1))
x <- window(x, end=c(2015,12))


#Fazendo a modelagem e previsão
models <- list(
  mod_arima = auto.arima(x, ic='aicc', stepwise=FALSE),
  mod_exp = ets(x, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(x, p=12, size=25),
  mod_tbats = tbats(x, ic='aicc', seasonal.periods=12),
  mod_bats = bats(x, ic='aicc', seasonal.periods=12),
  mod_stl = stlm(x, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(x),
  mod_elm = elm(x),
  mod_mlp = mlp(x)
)

#Plotando os resultados por modelos
forecasts <- lapply(models, forecast, 12)
forecasts$naive <- naive(x, 12)


#Apresentando o melhor modelo
acc <- lapply(forecasts, function(f){
  accuracy(f, test_x)[2,,drop=FALSE]
})
acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)

#######################################################

#Determinando a série temporal (h=6)
set.seed(123)
x <- ts(malaria$TO, start = c(1997,1), frequency = 12)
x = log(x)
test_x <- window(x, start=c(2016,7))
x <- window(x, end=c(2016,6))


#Fazendo a modelagem e previsão
models <- list(
  mod_arima = auto.arima(x, ic='aicc', stepwise=FALSE),
  mod_exp = ets(x, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(x, p=12, size=25),
  mod_tbats = tbats(x, ic='aicc', seasonal.periods=12),
  mod_bats = bats(x, ic='aicc', seasonal.periods=12),
  mod_stl = stlm(x, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(x),
  mod_elm = elm(x),
  mod_mlp = mlp(x)
)

#Plotando os resultados por modelos
forecasts <- lapply(models, forecast, 6)
forecasts$naive <- naive(x, 6)


#Apresentando o melhor modelo
acc <- lapply(forecasts, function(f){
  accuracy(f, test_x)[2,,drop=FALSE]
})
acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)

#########################################################

#Determinando a série temporal (h=3)
set.seed(123)
x <- ts(malaria$TO, start = c(1997,1), frequency = 12)
x = log(x)
test_x <- window(x, start=c(2016,10))
x <- window(x, end=c(2016,9))


#Fazendo a modelagem e previsão
models <- list(
  mod_arima = auto.arima(x, ic='aicc', stepwise=FALSE),
  mod_exp = ets(x, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(x, p=12, size=25),
  mod_tbats = tbats(x, ic='aicc', seasonal.periods=12),
  mod_bats = bats(x, ic='aicc', seasonal.periods=12),
  mod_stl = stlm(x, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(x),
  mod_elm = elm(x),
  mod_mlp = mlp(x)
)

#Plotando os resultados por modelos
forecasts <- lapply(models, forecast, 3)
forecasts$naive <- naive(x, 3)


#Apresentando o melhor modelo
acc <- lapply(forecasts, function(f){
  accuracy(f, test_x)[2,,drop=FALSE]
})
acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)

############################################################

y = ts(malaria$R, start = c(1997,1), frequency = 12)
y = log(y)

autoplot(y) +
  autolayer(forecasts$mod_exp, series="ETS", PI=FALSE) +
  autolayer(forecasts$mod_arima, series="ARIMA", PI=FALSE) +
  autolayer(forecasts$mod_stl, series="STL", PI=FALSE) +
  autolayer(forecasts$mod_neural, series="NNAR", PI=FALSE) +
  autolayer(forecasts$mod_bats, series="BATS", PI=FALSE) +
  autolayer(forecasts$mod_tbats, series="TBATS", PI=FALSE) +
  autolayer(forecasts$mod_sts, series="STS", PI=FALSE) +
  autolayer(forecasts$mod_elm, series="ELM", PI=FALSE) +
  autolayer(forecasts$mod_mlp, series="MLP", PI=FALSE) +
  autolayer(forecasts$naive, series="NAIVE", PI=FALSE) +
  xlab("Ano") + ylab("Número de casos") +
  ggtitle("Número de Casos de Malária, 1997 a 2016")
