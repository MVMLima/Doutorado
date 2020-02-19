#Abrir o banco de dados e definir diretório de trabalho

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue/Banco Dengue Base.RData")
setwd("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue")

#Criar série temporal a ser estudada
data <- ts(dengue$AC, start = c(2000, 1), frequency = 12)

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
x1 = as.matrix(x[,1])
x2 = as.matrix(x[,2])
x3 = as.matrix(x[,3])
y = as.matrix(x[,4])

n_train = 144
y_train = as.matrix(y[1:n_train])
x1_train = as.matrix(t(x1[1:n_train,]))
x2_train = as.matrix(t(x2[1:n_train,]))
x3_train = as.matrix(t(x3[1:n_train,]))

nrow(x)
ncol(x1_train)
ncol(x2_train)
ncol(x2_train)
length(y_train)

x_train = array(c(x1_train,
                  x2_train,
                  x3_train),
                dim=c(dim(x1_train),3))

dim(x_train) #Checagem da dimenssão do objeto

require(rnn)

set.seed(2018)
model1 = trainr(Y=t(y_train),
                X = x_train,
                learningrate =0.10,
                hidden_dim = 60,
                numepochs = 300,
                network_type = "lstm",
                sigmoid = "tanh")

#Examinando a velocidade de aprendizagem do modelo
error_1 = (model1$error)
rownames(error_1) = 1:nrow(error_1)
colnames(error_1) = "error"
plot(as.vector(error_1))

#Avaliando a peformance do modelo
pred1_train = t(predictr(model1,x_train))

round(cor(y_train,pred1_train),4)

plot(y_train,pred1_train,ylab="pred1_train")

#Aprimorando o modelo
set.seed(2018)
model2 = trainr(Y=t(y_train),
                X = x_train,
                learningrate =0.05,
                hidden_dim = c(4,3),
                numepochs = 500,
                network_type = "rnn",
                sigmoid = "logistic")

pred2_train = t(predictr(model2,x_train))
round(cor(y_train,pred2_train),4)

#Aprimorando o modelo altando os valores de ajuste
set.seed(2018)
model2 = trainr(Y=t(y_train),
                X = x_train,
                learningrate =0.05,
                hidden_dim = 60,
                numepochs = 1500,
                network_type = "gru",
                sigmoid = "logistic")

pred2_train = t(predictr(model2,x_train))
round(cor(y_train,pred2_train),4)


x1_test = as.matrix(t(x1[(n_train+1):nrow(x1),]))
x2_test = as.matrix(t(x2[(n_train+1):nrow(x2),]))
x3_test = as.matrix(t(x3[(n_train+1):nrow(x3),]))
y_test = as.matrix(y[(n_train+1):nrow(x3)])


x_test = array(c(x1_test,
                 x2_test,
                 x3_test),
               dim=c(dim(x1_test),3))

#Criando as previsões

pred1_test = t(predictr(model1,x_test))
pred2_test = t(predictr(model2,x_test))


unscale_data = function(x,max_x,min_x)
{x*(max_x - min_x) + min_x}

pred1_actual = unscale_data(pred1_test, max_data, min_data)
pred1_actual = exp(pred1_actual)
pred1_actual = ts(matrix(pred1_actual), end=c(2017,12), frequency=12)

pred2_actual = unscale_data(pred2_test, max_data, min_data)
pred2_actual = exp(pred2_actual)
pred2_actual = ts(matrix(pred2_actual), end=c(2017,12), frequency=12)

y_actual = unscale_data(y_test,max_data,min_data)
y_actual = exp(y_actual)
y_actual = ts(matrix(y_actual), end=c(2017,12), frequency=12)

result_all = cbind(y_actual, round(pred1_actual,2), round(pred2_actual,2))
colnames(result_all) = c("actual","Model 1","Model 2")
result_all
