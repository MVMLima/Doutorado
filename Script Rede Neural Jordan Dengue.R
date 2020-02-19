#Abrir o banco de dados e definir diretório de trabalho

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue/Banco Dengue Base.RData")
setwd("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue")

library(forecast)
library(quantmod)
library(RSNNS)


#Criar série temporal a ser estudada conforme o Estado
y = ts(dengue$AC, start = c(2000, 1), frequency = 12)

y = log(y)

#Normalizando os dados
range_data <- function (x) {(x-min(x))/(max(x)-min(x))}
min_data <-min(y)
max_data <-max(y)
y<-range_data(y)

#Carregando os pacotes necessários e preparando o banco para análise
y <- as.zoo(y)
x1 <-Lag(y, k = 1)
x2 <-Lag(y, k = 2)
x3 <-Lag(y, k = 3)
x4 <-Lag(y, k = 4)
x5 <-Lag(y, k = 5)
x6 <-Lag(y, k = 6)
x7 <-Lag(y, k = 7)
x8 <-Lag(y, k = 8)
x9 <-Lag(y, k = 9)
x10 <-Lag(y, k = 10)
x11 <-Lag(y, k = 11)
x12 <-Lag(y, k = 12)

x = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
x = cbind(y,x)
x = x[-(1:12),]
head(x)
n = nrow(x)


n_train=144
train = sample(1:n,n_train,FALSE)
inputs = x[,2:13]
outputs = x[,1]

set.seed(2019)
fit = jordan(inputs[train],outputs[train], size= 100, learnFuncParams = c(0.01), maxit=2000)

plotIterativeError(fit)

pred = predict(fit,inputs[-train])

round(cor(outputs[-train],pred)^2,4)

unscale_data = function(x,max_x,min_x) {x*(max_x-min_x)+min_x}

output_actual = unscale_data(outputs[-train],max_data,min_data)

output_actual = as.matrix(output_actual)
rownames(output_actual) = 1:length(output_actual)
output_pred = unscale_data(pred, max_data, min_data)
result = cbind(as.ts(output_actual),as.ts(output_pred))
plot(result)

