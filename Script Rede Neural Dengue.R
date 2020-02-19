#Abrir o banco de dados e definir diretório de trabalho

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue/Banco Dengue Base.RData")
setwd("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue")

#Criar série temporal a ser estudada
data <- ts(dengue$AC, start = c(2001, 1), frequency = 12)

#Verificar números de lag na ST
pacf(data)

#Carregando os pacotess necessários e preparando o banco para análise
library(quantmod)
data <- as.zoo(data)
x1 <-Lag(data, k = 1)
x2 <-Lag(data, k = 2)
x3 <-Lag(data, k = 3)
x<- cbind(x1,x2,x3,data)
head(x)
x <- x[-(1:3),]
head(x)

#Normalizando os dados
range_data <- function (x) {(x-min(x))/(max(x)-min(x))}
x<-data.matrix(x)
min_data <-min(x)
max_data <-max(x)
x<-range_data(x)

#Criando banco de teste e treinamento

