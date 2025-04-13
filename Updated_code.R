library(forecast)
library(data.table)
library(zoo)

funcfcastSmoothAllOutput <- function(x, h, d) {
  
  train.x <- window(x, end = c(1, floor(length(x) * 0.8)))
  test.x <- window(x, start = c(1, floor(length(x) * 0.8) + 1))
  test.h <- length(test.x)
  
  # List of model functions
  model_funcs <- list(
    mean = function(y, h) meanf(y, h = h),
    naive = function(y, h) naive(y, h = h),
    snaive = function(y, h) snaive(y, h = h),
    tslm = function(y, h) forecast(tslm(y ~ trend + season), h = h),
    stlf = function(y, h) forecast(stlf(y), h = h),
    HoltWinters = function(y, h) forecast(HoltWinters(y), h = h),
    ets = function(y, h) forecast(ets(y), h = h),
    autoarima = function(y, h) forecast(auto.arima(y), h = h),
    nn = function(y, h) forecast(nnetar(y, lambda = 0.5), h = h)
  )
  
  # Evaluate all models on train set
  forecasts <- lapply(model_funcs, function(f) tryCatch(f(train.x, test.h), error = function(e) NULL))
  
  # Calculate accuracy (safely)
  acc_results <- lapply(forecasts, function(f) {
    if (!is.null(f)) round(accuracy(f$mean, test.x), 2) else NA
  })
  
  # Convert to data.table
  acc.table <- rbindlist(lapply(names(acc_results), function(name) {
    row <- acc_results[[name]]
    if (is.null(row) || any(is.na(row))) return(NULL)
    data.table(Model = name, t(row[1, ]))
  }), fill = TRUE)
  
  # Determine best model
  acc_rmse <- sapply(acc_results, function(x) if (!is.null(x)) x[2] else Inf)
  best_model_name <- names(which.min(acc_rmse))
  best_model_func <- model_funcs[[best_model_name]]
  
  # Forecast using best model on full data
  best_forecast <- best_model_func(x, h)
  
  # Forecast from all methods for output table
  all_forecasts <- lapply(model_funcs, function(f) tryCatch(f(x, h), error = function(e) NULL))
  
  # Build fcastAll table
  make_forecast_row <- function(fcast, d) {
    if (is.null(fcast)) return(rep(NA, 3 * h + 1))
    c(fcast$method,
      round(fcast$mean * d, 1),
      round(fcast$upper[, 1] * d, 1),
      round(fcast$lower[, 1] * d, 1))
  }
  
  fcastAll <- as.data.table(do.call(rbind, lapply(all_forecasts, make_forecast_row, d = d)))
  
  # Naming columns dynamically
  periods <- as.character(yearmon(seq(
    from = time(best_forecast$mean)[1],
    to = time(best_forecast$mean)[length(best_forecast$mean)],
    by = 1/frequency(best_forecast$mean)
  )))
  
  colnames(fcastAll) <- c("Model",
                          paste0(periods, "_Mean"),
                          paste0(periods, "_Upper80"),
                          paste0(periods, "_Lower80"))
  
  fcastMean <- list(
    mean = round(best_forecast$mean, 1),
    lower80 = round(best_forecast$lower[,1], 1),
    upper80 = round(best_forecast$upper[,1], 1),
    method = best_forecast$method,
    rmse = round(min(acc_rmse), 2),
    accuracyTable = acc.table,
    allForecasts = fcastAll
  )
  
  plot(best_forecast)
  print(acc.table)
  
  return(fcastMean)
}
