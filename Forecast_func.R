library(fpp2)
library(zoo) # For function yearmon
library(urca) # For kpss test
library(tseries) # For ADF test
library(dplyr) # 

# Define the function
forecast_school_data <- function(dataClean, startDate, endDate, schoolMonthDays) {
  # Convert data to ts object
  dataCleanTs <- ts(dataClean[, c(9, 10, 11)], frequency = 12, start = startDate)
  
  # Initialize tables
  fcastTable <- data.table(yearMon = character(), schoolName = character(), mean = numeric(),
                           lower80 = numeric(), upper80 = numeric(), method = numeric(), rmse = character())
  fcastAccTable <- data.table(schoolName = character(), Model = character(), ME = numeric(),
                              RMSE = numeric(), MAE = numeric())
  fcastAll <- data.table()
  
  # Forecasting loop
  for (i in 1:ncol(dataCleanTs)) {
    if (i == 2) {
      timeSeries <- window(dataCleanTs[, i], start = c(2014, 5), end = endDate)
    } else {
      timeSeries <- window(dataCleanTs[, i], end = endDate)
    }
    
    fcast <- funcfcastSmoothAllOutput(timeSeries, 6, schoolMonthDays[, i, with = FALSE][[1]])
    
    fcastTable <- rbind(fcastTable, data.table(yearMon = as.character(zoo::as.yearmon(time(dataCleanTs[, 1]))),
                                               schoolName = colnames(dataCleanTs)[i],
                                               mean = c(window(dataCleanTs[, i], end = endDate), as.numeric(fcast$mean)),
                                               lower80 = c(rep(NA, length(window(dataCleanTs[, i], end = endDate))), as.numeric(fcast$lower80)),
                                               upper80 = c(rep(NA, length(window(dataCleanTs[, i], end = endDate))), as.numeric(fcast$upper80)),
                                               method = fcast$method,
                                               rmse = fcast$rmse))
    
    fcastAccTable <- rbind(fcastAccTable, data.table(schoolName = colnames(dataCleanTs)[i],
                                                     Model = fcast$accuracyTable$Model,
                                                     ME = fcast$accuracyTable$ME,
                                                     RMSE = fcast$accuracyTable$RMSE,
                                                     MAE = fcast$accuracyTable$MAE))
    
    fcastAll <- rbind(fcastAll, cbind(schoolName = colnames(dataCleanTs)[i],
                                      bestModel = fcast$method,
                                      bestModelRMSE = fcast$rmse,
                                      fcast$allForecasts))
  }
  
  # Adjust forecast table
  fcastTableAdj <- fcastTable
  fcastTableAdj[, c("meanAct", "upper80Act", "lower80Act") := list(
    case_when(schoolName == "adjTom" ~ mean * dataClean$TomlinDays,
              schoolName == "adjWelling" ~ mean * dataClean$WellingDays,
              schoolName == "adjNower" ~ mean * dataClean$NowerDays),
    case_when(schoolName == "adjTom" ~ upper80 * dataClean$TomlinDays,
              schoolName == "adjWelling" ~ upper80 * dataClean$WellingDays,
              schoolName == "adjNower" ~ upper80 * dataClean$NowerDays),
    case_when(schoolName == "adjTom" ~ lower80 * dataClean$TomlinDays,
              schoolName == "adjWelling" ~ lower80 * dataClean$WellingDays,
              schoolName == "adjNower" ~ lower80 * dataClean$NowerDays))]
  
  # Export data
  write.csv(fcastAll, file = "fcastAll.csv")
  write.csv(fcastAccTable, file = "fcastAccTable.csv")
  write.csv(fcastTableAdj, file = "fcastTableAdj.csv")
  
  return(list(fcastTable = fcastTable, fcastAccTable = fcastAccTable, fcastAll = fcastAll, fcastTableAdj = fcastTableAdj))
}

# Example usage
startDate <- c(2013, 10)
endDate <- c(2018, 9)
schoolMonthDays <- dataClean[Date > as.Date("2018-9-01"), .(TomlinDays, NowerDays, WellingDays)]

result <- forecast_school_data(dataClean, startDate, endDate, schoolMonthDays)
