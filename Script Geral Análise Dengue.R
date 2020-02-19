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


#Usar caso haja 0 no ano 2000
denguets <- dengue
dengueA <- dengue[-(1:12),-(1:2)]
denguets <- ts(denguets, start = c(2001, 1), frequency = 12)
str(denguets)

#Análise descritiva das séries temporais por estado da federação
XX <- ts(dengue$AC, start = c(2000,1), frequency = 12)
plot(decompose(XX))
Pacf(XX)
monthplot(XX)
seas(XX) %>% autoplot

ts_plot_season <- function(x = x) {
  season <- cycle(x)
  season.factor <- factor(season)
  ggplot() + 
    geom_boxplot(mapping = aes(x = season.factor,
                               y = x)) +
    labs(x = "Periodo", y =  "Serie")
}

ts_plot_season(XX)

#Verificação se a série temporal é estacionária
summary(ur.df(XX))
summary(ur.za(XX))
summary(ur.kpss(XX))

#Se nã for estcionária deve-se aplicar uma differeciação
z = diff(XX)
summary(ur.kpss(z))

#Divisão do banco em teste e treino, obtenção dos modelos preditivos, métricas de desempenho e gráfico comparativo
train <- window(XX, end=c(2013,12))
h <- length(XX) - length(train)
ETS <- forecast(ets(train), h=h)
ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),
                  h=h)
STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)
NNAR <- forecast(nnetar(train), h=h)
TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5

autoplot(XX) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(STL, series="STL", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(TBATS, series="TBATS", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Ano") + ylab("Número de casos") +
  ggtitle("Número de Casos de Dengue, 2001 a 2018")

c(ETS = accuracy(ETS, XX)["Test set", "ME"],
  ARIMA = accuracy(ARIMA, XX)["Test set","ME"],
  `STL-ETS` = accuracy(STL, XX)["Test set","ME"],
  NNAR = accuracy(NNAR, XX)["Test set","ME"],
  TBATS = accuracy(TBATS, XX)["Test set","ME"],
  Combination = accuracy(Combination, XX)["Test set","ME"])

c(ETS = accuracy(ETS, XX)["Test set", "RMSE"],
  ARIMA = accuracy(ARIMA, XX)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, XX)["Test set","RMSE"],
  NNAR = accuracy(NNAR, XX)["Test set","RMSE"],
  TBATS = accuracy(TBATS, XX)["Test set","RMSE"],
  Combination = accuracy(Combination, XX)["Test set","RMSE"])

c(ETS = accuracy(ETS, XX)["Test set", "MAE"],
  ARIMA = accuracy(ARIMA, XX)["Test set","MAE"],
  `STL-ETS` = accuracy(STL, XX)["Test set","MAE"],
  NNAR = accuracy(NNAR, XX)["Test set","MAE"],
  TBATS = accuracy(TBATS, XX)["Test set","MAE"],
  Combination = accuracy(Combination, XX)["Test set","MAE"])

c(ETS = accuracy(ETS, XX)["Test set", "MPE"],
  ARIMA = accuracy(ARIMA, XX)["Test set","MPE"],
  `STL-ETS` = accuracy(STL, XX)["Test set","MPE"],
  NNAR = accuracy(NNAR, XX)["Test set","MPE"],
  TBATS = accuracy(TBATS, XX)["Test set","MPE"],
  Combination = accuracy(Combination, XX)["Test set","MPE"])

c(ETS = accuracy(ETS, XX)["Test set", "MAPE"],
  ARIMA = accuracy(ARIMA, XX)["Test set","MAPE"],
  `STL-ETS` = accuracy(STL, XX)["Test set","MAPE"],
  NNAR = accuracy(NNAR, XX)["Test set","MAPE"],
  TBATS = accuracy(TBATS, XX)["Test set","MAPE"],
  Combination = accuracy(Combination, XX)["Test set","MAPE"])