library(dplyr)
library(forecast)

forcastingWithArima <- function(selectedData){
  
  data <- select(selectedData, usd, month_year)

  Dates_unique <- distinct(data ,month_year, keep_all=FALSE)
  Dates_unique <- Dates_unique[order(as.Date(Dates_unique$month_year)),]
  
  foodData_Median <- data.frame(Date=as.Date(character()), Median=double())

  for (row in 1:nrow(Dates_unique)){
    thisDate <- Dates_unique[row,1]
    foodData_with_Date <- subset(data, data$month_year == thisDate)
  
    foodData_Median_newRow <- c(toString(thisDate),mean(foodData_with_Date$usd))
    foodData_Median <- rbind(foodData_Median, foodData_Median_newRow)
  }
  
  names(foodData_Median) <- c("Date","Median")

  start_year <- substring(Dates_unique[1,1],1,4)
  start_month <- substring(Dates_unique[1,1],6,7)
  
  stop_year <- substring(tail(Dates_unique$month_year,1),1,4)
  stop_month <- substring(tail(Dates_unique$month_year,1),6,7)
  
  ts_test <- ts(as.numeric(foodData_Median$Median), start=c(start_year,start_month),end=c(stop_year,stop_month), frequency = 12)
  fit_test <- auto.arima(ts_test)

  arima_result <- forecast(fit_test, 36)
  
  year_forcast <- year_forcast <- data.frame(year = seq(as.Date("2018-01-01"), by="1 month", length.out=36))
  price_forcast <- arima_result$mean
  
  result <- data.frame(price = price_forcast, year = year_forcast)
  
  return(result)
}