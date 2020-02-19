#Abrir o banco de dados e definir diretório de trabalho

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue/Banco Dengue Base.RData")
setwd("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue")

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


x <- ts(dengue$AC, start = c(2000,1), frequency = 12)
test_x <- window(x, start=c(2014, 1))
x <- window(x, end=c(2013,12))

#Detectando outliers
outlier.x <- tsoutliers::tso(x, types = c("AO","LS","TC"), maxit.iloop=10)
outlier.x
plot(outlier.x)

#Fazendo a modelagem e previsão
models <- list(
  mod_arima = auto.arima(x, ic='aicc', stepwise=FALSE),
  mod_exp = ets(x, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(x, p=12, size=25),
  mod_tbats = tbats(x, ic='aicc', seasonal.periods=12),
  mod_bats = bats(x, ic='aicc', seasonal.periods=12),
  mod_stl = stlm(x, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(x)
)

#Plotando os resultados por modelos
forecasts <- lapply(models, forecast, 12)
forecasts$naive <- naive(x, 12)
par(mfrow=c(2, 2))
for(f in forecasts){
  plot(f)
  lines(test_x, col='red')
}

#Apresentando o melhor modelo
acc <- lapply(forecasts, function(f){
  accuracy(f, test_x)[2,,drop=FALSE]
})
acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)