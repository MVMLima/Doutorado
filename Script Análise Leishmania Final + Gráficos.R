#Abrir o banco de dados e definir diretório de trabalho

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Leishmaniose/Banco Leishmania.RData")
setwd("C:/Users/Lucas/Desktop/Resultados Doutorado/Leishmaniose")

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
x.h12 <- ts(leishmania$MS, start = c(2001,1), frequency = 12)
x.h12 = log(x.h12)
test_x.h12 <- window(x.h12, start=c(2017,1))
x.h12 <- window(x.h12, end=c(2016,12))


#Fazendo a modelagem e previsão
models.h12 <- list(
  mod_arima = auto.arima(x.h12, ic='aicc', stepwise=FALSE),
  mod_exp = ets(x.h12, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(x.h12, p=12, size=25),
  mod_tbats = tbats(x.h12, ic='aicc', seasonal.periods=12),
  mod_bats = bats(x.h12, ic='aicc', seasonal.periods=12),
  mod_stl = stlm(x.h12, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(x.h12),
  mod_elm = elm(x.h12),
  mod_mlp = mlp(x.h12)
)

#Plotando os resultados por modelos
forecasts.h12 <- lapply(models.h12, forecast, 12)
forecasts.h12$naive <- naive(x.h12, 12)

#Apresentando o melhor modelo
acc.h12 <- lapply(forecasts.h12, function(f){
  accuracy(f, test_x.h12)[2,,drop=FALSE]
})
acc.h12 <- Reduce(rbind, acc.h12)
row.names(acc.h12) <- names(forecasts.h12)
acc.h12 <- acc.h12[order(acc.h12[,'MASE']),]
round(acc.h12, 2)

##########################################################

#Determinando a série temporal (h=6)
set.seed(123)
x.h6 <- ts(leishmania$MS, start = c(2001,1), frequency = 12)
x.h6 = log(x.h6)
test_x.h6 <- window(x.h6, start=c(2017,7))
x.h6 <- window(x.h6, end=c(2017,6))


#Fazendo a modelagem e previsão
models.h6 <- list(
  mod_arima = auto.arima(x.h6, ic='aicc', stepwise=FALSE),
  mod_exp = ets(x.h6, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(x.h6, p=12, size=25),
  mod_tbats = tbats(x.h6, ic='aicc', seasonal.periods=12),
  mod_bats = bats(x.h6, ic='aicc', seasonal.periods=12),
  mod_stl = stlm(x.h6, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(x.h6),
  mod_elm = elm(x.h6),
  mod_mlp = mlp(x.h6)
)

#Plotando os resultados por modelos
forecasts.h6 <- lapply(models.h6, forecast, 6)
forecasts.h6$naive <- naive(x.h6, 6)

#Apresentando o melhor modelo
acc.h6 <- lapply(forecasts.h6, function(f){
  accuracy(f, test_x.h6)[2,,drop=FALSE]
})
acc.h6 <- Reduce(rbind, acc.h6)
row.names(acc.h6) <- names(forecasts.h6)
acc.h6 <- acc.h6[order(acc.h6[,'MASE']),]
round(acc.h6, 2)

##########################################################

#Determinando a série temporal (h=3)
set.seed(123)
x.h3 <- ts(leishmania$MS, start = c(2001,1), frequency = 12)
x.h3 = log(x.h3)
test_x.h3 <- window(x.h3, start=c(2017,10))
x.h3 <- window(x.h3, end=c(2017,9))


#Fazendo a modelagem e previsão
models.h3 <- list(
  mod_arima = auto.arima(x.h3, ic='aicc', stepwise=FALSE),
  mod_exp = ets(x.h3, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(x.h3, p=12, size=25),
  mod_tbats = tbats(x.h3, ic='aicc', seasonal.periods=12),
  mod_bats = bats(x.h3, ic='aicc', seasonal.periods=12),
  mod_stl = stlm(x.h3, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(x.h3),
  mod_elm = elm(x.h3),
  mod_mlp = mlp(x.h3)
)

#Plotando os resultados por modelos
forecasts.h3 <- lapply(models.h3, forecast, 3)
forecasts.h3$naive <- naive(x.h3, 3)

#Apresentando o melhor modelo
acc.h3 <- lapply(forecasts.h3, function(f){
  accuracy(f, test_x.h3)[2,,drop=FALSE]
})
acc.h3 <- Reduce(rbind, acc.h3)
row.names(acc.h3) <- names(forecasts.h3)
acc.h3 <- acc.h3[order(acc.h3[,'MASE']),]
round(acc.h3, 2)


#########################################################
###########Gráficos Finais###############################
#########################################################
s.realMT <- log(leishmania$MS)

#h12 plot()

forecast_h12_arima <- c(forecasts.h12$mod_arima$fitted, forecasts.h12$mod_arima$mean)
forecast_h12_exp <- c(forecasts.h12$mod_exp$fitted, forecasts.h12$mod_exp$mean)
forecast_h12_neural <- c(forecasts.h12$mod_neural$fitted, forecasts.h12$mod_neural$mean)
forecast_h12_tbats <- c(forecasts.h12$mod_tbats$fitted, forecasts.h12$mod_tbats$mean)
forecast_h12_bats <- c(forecasts.h12$mod_bats$fitted, forecasts.h12$mod_bats$mean)
forecast_h12_stl <- c(forecasts.h12$mod_stl$fitted, forecasts.h12$mod_stl$mean)
forecast_h12_sts <- c(forecasts.h12$mod_sts$fitted, forecasts.h12$mod_sts$mean)
forecast_h12_elm <- c(forecasts.h12$mod_elm$fitted, forecasts.h12$mod_elm$mean)
forecast_h12_mlp <- c(forecasts.h12$mod_mlp$fitted, forecasts.h12$mod_mlp$mean)
forecast_h12_naive <- c(forecasts.h12$naive$fitted, forecasts.h12$naive$mean)

png("Fig_MS_leishmania.png", units="mm", height=170, width=170, res=600)
par(mar=c(4,4,2.2,0.1))
par(mfrow=c(2,2))
st <- 1:12
plot(st, s.realMT[193:204], type = "l", lwd = 2, ylim = c(1,6), ylab = "Número de casos (log-n)", xlab = "2017 (mês)", main="A")
points(st,forecast_h12_arima[193:204], type = "l", lwd = 2, col = 2, lty = "dashed")
points(st,forecast_h12_exp[193:204], type = "l", lwd = 2, col = 3, lty = 3)
points(st,forecast_h12_neural[193:204], type = "l", lwd = 2, col = 4, lty = 4)
points(st,forecast_h12_tbats[193:204], type = "l", lwd = 2, col = 5, lty = 5)
points(st,forecast_h12_bats[193:204], type = "l", lwd = 2, col = 6, lty = 6)
points(st,forecast_h12_stl[193:204], type = "l", lwd = 2, col = 7, lty = 7)
points(st,forecast_h12_sts[193:204], type = "l", lwd = 2, col = 8, lty = 8)
points(st,forecast_h12_elm[181:192], type = "l", lwd = 2, col = 9, lty = 9)
points(st,forecast_h12_mlp[181:192], type = "l", lwd = 2, col = 10, lty = 10)
points(st,forecast_h12_naive[193:204], type = "l", lwd = 2, col = 11, lty = 11)

#h6 plot()

forecast_h6_arima <- c(forecasts.h6$mod_arima$fitted, forecasts.h6$mod_arima$mean)
forecast_h6_exp <- c(forecasts.h6$mod_exp$fitted, forecasts.h6$mod_exp$mean)
forecast_h6_neural <- c(forecasts.h6$mod_neural$fitted, forecasts.h6$mod_neural$mean)
forecast_h6_tbats <- c(forecasts.h6$mod_tbats$fitted, forecasts.h6$mod_tbats$mean)
forecast_h6_bats <- c(forecasts.h6$mod_bats$fitted, forecasts.h6$mod_bats$mean)
forecast_h6_stl <- c(forecasts.h6$mod_stl$fitted, forecasts.h6$mod_stl$mean)
forecast_h6_sts <- c(forecasts.h6$mod_sts$fitted, forecasts.h6$mod_sts$mean)
forecast_h6_elm <- c(forecasts.h6$mod_elm$fitted, forecasts.h6$mod_elm$mean)
forecast_h6_mlp <- c(forecasts.h6$mod_mlp$fitted, forecasts.h6$mod_mlp$mean)
forecast_h6_naive <- c(forecasts.h6$naive$fitted, forecasts.h6$naive$mean)


plot(st[7:12], s.realMT[199:204], type = "l", lwd = 2, ylim = c(1,6), ylab = "", xlab = "2017 (mês)", main="B")
points(st[7:12],forecast_h6_arima[199:204], type = "l", lwd = 2, col = 2, lty = "dashed")
points(st[7:12],forecast_h6_exp[199:204], type = "l", lwd = 2, col = 3, lty = 3)
points(st[7:12],forecast_h6_neural[199:204], type = "l", lwd = 2, col = 4, lty = 4)
points(st[7:12],forecast_h6_tbats[199:204], type = "l", lwd = 2, col = 5, lty = 5)
points(st[7:12],forecast_h6_bats[199:204], type = "l", lwd = 2, col = 6, lty = 6)
points(st[7:12],forecast_h6_stl[199:204], type = "l", lwd = 2, col = 7, lty = 7)
points(st[7:12],forecast_h6_sts[199:204], type = "l", lwd = 2, col = 8, lty = 8)
points(st[7:12],forecast_h6_elm[187:192], type = "l", lwd = 2, col = 9, lty = 9)
points(st[7:12],forecast_h6_mlp[187:192], type = "l", lwd = 2, col = 10, lty = 10)
points(st[7:12],forecast_h6_naive[199:204], type = "l", lwd = 2, col = 11, lty = 11)

#h3 plot()

forecast_h3_arima <- c(forecasts.h3$mod_arima$fitted, forecasts.h3$mod_arima$mean)
forecast_h3_exp <- c(forecasts.h3$mod_exp$fitted, forecasts.h3$mod_exp$mean)
forecast_h3_neural <- c(forecasts.h3$mod_neural$fitted, forecasts.h3$mod_neural$mean)
forecast_h3_tbats <- c(forecasts.h3$mod_tbats$fitted, forecasts.h3$mod_tbats$mean)
forecast_h3_bats <- c(forecasts.h3$mod_bats$fitted, forecasts.h3$mod_bats$mean)
forecast_h3_stl <- c(forecasts.h3$mod_stl$fitted, forecasts.h3$mod_stl$mean)
forecast_h3_sts <- c(forecasts.h3$mod_sts$fitted, forecasts.h3$mod_sts$mean)
forecast_h3_elm <- c(forecasts.h3$mod_elm$fitted, forecasts.h3$mod_elm$mean)
forecast_h3_mlp <- c(forecasts.h3$mod_mlp$fitted, forecasts.h3$mod_mlp$mean)
forecast_h3_naive <- c(forecasts.h3$naive$fitted, forecasts.h3$naive$mean)


plot(st[10:12], s.realMT[202:204], type = "l", lwd = 2, ylim = c(1,6), ylab = "Número de casos (log-n)", xlab = "2017 (mês)", main="C", xaxp = c(10,12,2))
points(st[10:12],forecast_h3_arima[202:204], type = "l", lwd = 2, col = 2, lty = "dashed")
points(st[10:12],forecast_h3_exp[202:204], type = "l", lwd = 2, col = 3, lty = 3)
points(st[10:12],forecast_h3_neural[202:204], type = "l", lwd = 2, col = 4, lty = 4)
points(st[10:12],forecast_h3_tbats[202:204], type = "l", lwd = 2, col = 5, lty = 5)
points(st[10:12],forecast_h3_bats[202:204], type = "l", lwd = 2, col = 6, lty = 6)
points(st[10:12],forecast_h3_stl[202:204], type = "l", lwd = 2, col = 7, lty = 7)
points(st[10:12],forecast_h3_sts[202:204], type = "l", lwd = 2, col = 8, lty = 8)
points(st[10:12],forecast_h3_elm[190:192], type = "l", lwd = 2, col = 9, lty = 9)
points(st[10:12],forecast_h3_mlp[190:192], type = "l", lwd = 2, col = 10, lty = 10)
points(st[10:12],forecast_h3_naive[202:204], type = "l", lwd = 2, col = 11, lty = 11)

#Legenda
plot(193:204, s.realMT[193:204],type='n',axes=F,ann=F)
legend(193,3.3, legend=c("Real", "Arima", "Exponencial", "Neural", "TBATS", "BATS", "STL", "STS", "ELM", "MLP", "Naive"), lty=c(1,2,3,4,5,6,7,8,9,10,11),col=c(1,2,3,4,5,6,7,8,9,10,11), lwd=2, bty="n")

dev.off()