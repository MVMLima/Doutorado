#Abrir o banco de dados e definir diretório de trabalho

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue/Banco Dengue Base.RData")
setwd("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue")

#Carregar pacotes para análise e transformação do banco em série temporal
library(TSstudio)

install.packages("TSstudio")

#Usar caso haja 0 no ano 2000
denguets <- dengue
dengueA <- dengue[-(1:12),-(1:2)]
denguets <- ts(denguets, start = c(2001, 1), frequency = 12)
str(denguets)

#Análise descritiva das séries temporais por estado da federação
XX <- ts(dengue$AC, start = c(2000,1), frequency = 12)

ts_info(XX)
ts_plot(XX)
ts_decompose(XX)
XX_detrend <- XX - decompose(XX)$trend
ts_seasonal(XX_detrend, type = "box")
ts_acf(XX)
ts_lags(USVSales, lags = c(1, 2, 3, 4, 5))
ts_lags(USVSales, lags = c(12, 24, 36))

df <- ts_to_prophet(XX)

names(df) <- c("date", "y")

head(df)

library(dplyr)
library(lubridate)

df <- df %>% mutate(month = factor(month(date, label = TRUE), ordered = FALSE), 
                    lag12 = lag(y, n = 12)) %>% filter(!is.na(lag12))

df$trend <- 1:nrow(df)
df$trend_sqr <- df$trend^2

h <- 12
train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]



forecast_df <- data.frame(date = seq.Date(from = max(df$date) + month(1),
              length.out = h, by = "month"),
              trend = seq(from = max(df$trend) + 1,
              length.out = h, by = 1))
forecast_df$trend_sqr <- forecast_df$trend ^ 2
forecast_df$month <- factor(month(forecast_df$date, label = TRUE), ordered = FALSE)
forecast_df$lag12 <- tail(df$y, 12)


lr <- lm(y ~ month + lag12 + trend + trend_sqr, data = train_df)

summary(lr)

test_df$yhat <- predict(lr, newdata = test_df)

mape_lr <- mean(abs(test_df$y - test_df$yhat) / test_df$y)

mape_lr

library(h2o)
h2o.init(max_mem_size = "16G")

train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)

forecast_h <- as.h2o(forecast_df)

x <- c("month", "lag12", "trend", "trend_sqr")

y <- "y"

rf_md <- h2o.randomForest(training_frame = train_h,
                          nfolds = 5,
                          x = x,
                          y = y,
                          ntrees = 500,
                          stopping_rounds = 10,
                          stopping_metric = "RMSE",
                          score_each_iteration = TRUE,
                          stopping_tolerance = 0.0001,
                          seed = 1234)


h2o.varimp_plot(rf_md)
rf_md@model$model_summary

library(plotly)
tree_score <- rf_md@model$scoring_history$training_rmse

plot_ly(x = seq_along(tree_score), y = tree_score,
        type = "scatter", mode = "line") %>%
  layout(title = "The Trained Model Score History",
         yaxis = list(title = "RMSE"),
         xaxis = list(title = "Num. of Trees"))


test_h$pred_rf <- h2o.predict(rf_md, test_h)

test_1 <- as.data.frame(test_h)

mape_rf <- mean(abs(test_1$y - test_1$pred_rf) / test_1$y)
mape_rf

hyper_params_rf <- list(mtries = c(2, 3, 4),
                        sample_rate = c(0.632, 0.8, 0.95),
                        col_sample_rate_per_tree = c(0.5, 0.9, 1.0),
                        max_depth = c(seq(1, 30, 3)),
                        min_rows = c(1, 2, 5, 10))

search_criteria_rf <- list(strategy = "RandomDiscrete",
                           stopping_metric = "rmse",
                           stopping_tolerance = 0.0001,
                           stopping_rounds = 10,
                           max_runtime_secs = 60 * 20)


rf2 <- h2o.grid(algorithm = "randomForest",
                search_criteria = search_criteria_rf,
                hyper_params = hyper_params_rf,
                x = x,
                y = y,
                training_frame = train_h,
                ntrees = 5000,
                nfolds = 5,
                grid_id = "rf_grid",
                seed = 1234)


rf2_grid_search <- h2o.getGrid(grid_id = "rf_grid",
                               sort_by = "rmse",
                               decreasing = FALSE)


rf_grid_model <- h2o.getModel(rf2_grid_search@model_ids[[1]])
test_h$rf_grid <- h2o.predict(rf_grid_model, test_h)
mape_rf2 <- mean(abs(test_1$y - test_1$rf_grid) / test_1$y)
mape_rf2

gbm_md <- h2o.gbm(
  training_frame = train_h,
  nfolds = 5,
  x = x,
  y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)

h2o.varimp_plot(gbm_md)

test_h$pred_gbm <- h2o.predict(gbm_md, test_h)
test_1 <- as.data.frame(test_h)
mape_gbm <- mean(abs(test_1$y - test_1$pred_gbm) / test_1$y)
mape_gbm

autoML1 <- h2o.automl(training_frame = train_h,
                      x = x,
                      y = y,
                      nfolds = 5,
                      max_runtime_secs = 60*20,
                      seed = 1234)

test_h$pred_autoML <- h2o.predict(autoML1@leader, test_h)
test_1 <- as.data.frame(test_h)
mape_autoML <- mean(abs(test_1$y - test_1$pred_autoML) / test_1$y)
mape_autoML

forecast_h$pred_gbm <- h2o.predict(gbm_md, forecast_h)
forecast_h$pred_rf <- h2o.predict(rf_grid_model, forecast_h)
forecast_h$pred_automl <- h2o.predict(autoML1@leader, forecast_h)
final_forecast <- as.data.frame(forecast_h)
