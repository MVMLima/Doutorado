library(dplyr)
library(ggplot2)
library(forecastML)
library(glmnet)
library(randomForest)
library(DT)

load("C:/Users/Lucas/Desktop/Resultados Doutorado/Dengue/Banco Dengue Base.RData")

data = as.data.frame(dengue$AC)

ts_frequency <- 12  # monthly time-series

data_train <- data[1:(nrow(data) - ts_frequency), ]
data_test <- data[(nrow(data) - ts_frequency + 1):nrow(data), ]

data_train = as.data.frame(data_train)
data_test = as.data.frame(data_test)

p <- ggplot(data, aes(x = 1:nrow(data), y = dengue$AC))
p <- p + geom_line()
p <- p + geom_vline(xintercept = nrow(data_train), color = "red", size = 1.1)
p <- p + theme_bw() + xlab("Index")
p

horizons <- c(1, 3, 6, 9, 12)
lookback <- 1:15

data_list <- forecastML::create_lagged_df(data_train, type = "train",
                                          outcome_col = 1, lookback = lookback,
                                          horizons = horizons)

plot(data_list)

windows <- forecastML::create_windows(lagged_df = data_list, window_length = 24, skip = 0,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)
windows

plot(windows, data_list, show_labels = TRUE)


model_function <- function(data) {
  
  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))
  
  model <- glmnet::cv.glmnet(x, y)
  return(model)
}

model_function_2 <- function(data) {
  
  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names,  "~ ."))
  
  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}

model_results <- forecastML::train_model(data_list, windows, model_name = "LASSO",
                                         model_function, use_future = FALSE)

model_results_2 <- forecastML::train_model(data_list, windows, model_name = "RF", 
                                           model_function_2, use_future = FALSE)


prediction_function <- function(model, data_features) {
  
  x <- as.matrix(data_features, ncol = ncol(data_features))
  
  data_pred <- data.frame("y_pred" = predict(model, x, s = "lambda.min"))
  return(data_pred)
}

# Example 2 - Random Forest
prediction_function_2 <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

data_results <- predict(model_results, model_results_2,
                        prediction_function = list(prediction_function, prediction_function_2), data = data_list)

plot(data_results, type = "prediction", horizons = c(1, 6, 12))


plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 5:7)

plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

plot(data_results, type = "forecast_stability", valid_indices = attributes(data_list)$row_indices[1:3])

plot(data_results, type = "forecast_variability", valid_indices = 30:80)

data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"),
                                       models = NULL)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE), )

plot(data_error, data_results, type = "time", horizons = c(1, 6, 12), windows = 5:7)

plot(data_error, data_results, type = "horizon", horizons = c(1, 6, 12))

plot(data_error, data_results, type = "global")

hyper_function <- function(model) {
  
  lambda_min <- model$lambda.min
  lambda_1se <- model$lambda.1se
  
  data_hyper <- data.frame("lambda_min" = lambda_min, "lambda_1se" = lambda_1se)
  return(data_hyper)
}

data_hyper <- forecastML::return_hyper(model_results, hyper_function)

plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))

plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))

data_forecast_list <- forecastML::create_lagged_df(data_train, type = "forecast", 
                                                   lookback = lookback,  horizon = horizons)

DT::datatable(head(data_forecast_list$horizon_6), options = list(scrollX = TRUE))


data_forecast <- predict(model_results, model_results_2,
                         prediction_function = list(prediction_function, prediction_function_2), 
                         data = data_forecast_list)

DT::datatable(head(data_forecast, 10), options = list(scrollX = TRUE))

plot(data_forecast, data_actual = data_train[-(1:12), ],
     actual_indices = as.numeric(row.names(data_train[-(1:12), ])),
     horizons = c(1, 6, 12), facet_plot = c("model", "model_forecast_horizon"))


plot(data_forecast, data_actual = data_test, 
     actual_indices = as.numeric(row.names(data_test)),
     facet_plot = "model", horizons = c(1, 6, 12))

data_error <- forecastML::return_error(data_forecast, data_test = data_test,
                                       test_indices = as.numeric(row.names(data_test)),
                                       metrics = c("mae", "mape", "smape", "mdape"))

DT::datatable(head(data_error$error_by_horizon, 10), options = list(scrollX = TRUE))

data_list <- forecastML::create_lagged_df(data_train, type = "train", 
                                          lookback = lookback, 
                                          horizon = horizons)

windows <- forecastML::create_windows(data_list, window_length = 0)

plot(windows, data_list, show_labels = TRUE)


model_results <- forecastML::train_model(data_list, windows,  model_name = "LASSO", model_function)

data_results <- predict(model_results, prediction_function = list(prediction_function), data = data_list)

DT::datatable(head(data_results, 10), options = list(scrollX = TRUE))

plot(data_results, type = "prediction", horizons = c(1, 6, 12))

plot(data_results, type = "residual", horizons = c(1, 6, 12))

plot(data_results, type = "forecast_stability", valid_indices = 109:120)


data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "mdape", "smape"),
                                       models = NULL)

DT::datatable(head(data_error$error_global), options = list(scrollX = TRUE))

plot(data_error, data_results, type = "horizon")

data_hyper <- forecastML::return_hyper(model_results, hyper_function)

plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))

plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))

data_forecast_list <- forecastML::create_lagged_df(data_train, type = "forecast", 
                                                   lookback = lookback,  horizon = horizons)

data_forecast <- predict(model_results, prediction_function = list(prediction_function), 
                         data = data_forecast_list)

plot(data_forecast, data_actual = data[-(1:16), ],
     actual_indices = as.numeric(row.names(data[-(1:16), ])),
     horizons = c(1, 6, 12), 
     facet_plot = c("model", "model_forecast_horizon")) + ggplot2::theme(legend.position = "none")

plot(data_forecast, data_actual = data_test, actual_indices = as.numeric(row.names(data_test)),
     facet_plot = NULL, horizons = c(1, 6, 12))

data_error <- forecastML::return_error(data_forecast, data_test = data_test, 
                                       test_indices = as.numeric(row.names(data_test)),
                                       metrics = c("mae", "mape", "mdape", "smape"))

DT::datatable(data_error$error_by_horizon, options = list(scrollX = TRUE))

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))
