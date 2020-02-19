#Abrir o banco de dados e definir diretório de trabalho

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue/Banco Dengue Base.RData")
setwd("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue")

#Criar série temporal a ser estudada
data <- ts(dengue$AC, start = c(2000, 1), frequency = 12)

library(h2o)
library(timetk)
library(tidyquant)
library(tsbox)
library(tidyverse)

data_tbl <- ts_tbl(data)

data_tbl <- data_tbl %>% tk_augment_timeseries_signature()

data_clean <- data_tbl %>% 
  select(-time) %>% 
  select_if(~!any(is.na(.))) %>% 
  mutate_if(is.ordered, ~ as.character(.) %>%  as.factor)


train_tbl <- data_clean %>% filter(year < 2014)
valid_tbl <- data_clean %>% filter(between(year,2014,2016))
test_tbl <- data_clean %>% filter(year == 2017)

h2o.init()

train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o <- as.h2o(test_tbl)

h2o.no_progress()

y <- "value" 
x <- names(train_tbl) %>% setdiff(y)

set.seed(123)
automl_models_h2o <- h2o.automl(x = x, 
                                y = y, 
                                training_frame = train_h2o, 
                                validation_frame = valid_h2o, 
                                leaderboard_frame = test_h2o, 
                                max_runtime_secs = 120, 
                                stopping_metric = "AUTO")


automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)

error_tbl <- data_tbl %>% 
  filter(lubridate::year(time) == 2017) %>%
  add_column(pred = pred_h2o %>% as.vector()) %>%
  rename(actual = value) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) %>% 
  select(time, actual, pred, error, error_pct)
error_tbl

f_error <- function(data){
  data %>% summarise(
    n=length(error),
    mean = mean(error),
    var = sum((error-mean)^2)/(n-1),
    std = sqrt(var),
    mae = mean(abs(error)),
    rmse = mean(error^2)^0.5,
    mape = mean(abs(error_pct)),
    mpe = mean(error_pct),
    skew = sum(((error - mean)/std)^3)/n,
    kurtosis = sum(((error - mean)/std)^4)/n-3,
    Metrics::mase(actual, pred)
  ) 
}

error_tbl %>% f_error()

#Modelo Ramdom Forest
rf_md <- h2o.randomForest(training_frame = train_h2o, 
                          validation_frame = valid_h2o, 
                          nfolds = 5,
                          x = x,
                          y = y,
                          ntrees = 500,
                          stopping_rounds = 10,
                          stopping_metric = "RMSE",
                          score_each_iteration = TRUE,
                          stopping_tolerance = 0.0001,
                          seed = 1234)



pred_h2o2 <- h2o.predict(rf_md, newdata = test_h2o)

h2o.performance(rf_md, newdata = test_h2o)

error_tbl2 <- data_tbl %>% 
  filter(lubridate::year(time) == 2017) %>%
  add_column(pred = pred_h2o2 %>% as.vector()) %>%
  rename(actual = value) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) %>% 
  select(time, actual, pred, error, error_pct)
error_tbl2

error_tbl2 %>% f_error()

#Modelo GBM
gbm_md <- h2o.gbm(training_frame = train_h2o, 
          validation_frame = valid_h2o,
          nfolds = 5,
          x = x,
          y = y,
          max_depth = 20,
          distribution = "gaussian",
          ntrees = 500,
          learn_rate = 0.1,
          score_each_iteration = TRUE
)

pred_h2o3 <- h2o.predict(gbm_md, newdata = test_h2o)

h2o.performance(gbm_md, newdata = test_h2o)

error_tbl3 <- data_tbl %>% 
  filter(lubridate::year(time) == 2017) %>%
  add_column(pred = pred_h2o3 %>% as.vector()) %>%
  rename(actual = value) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) %>% 
  select(time, actual, pred, error, error_pct)

error_tbl3

error_tbl3 %>% f_error()