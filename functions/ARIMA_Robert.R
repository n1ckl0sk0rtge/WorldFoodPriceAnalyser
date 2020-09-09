library(dplyr)
library(forecast)

#Ich habe foodData_Bread

foodData_Bread_good <- select(foodData_Bread, product, country, usd, month_year, year)

Dates_unique <- distinct(foodData_Bread,month_year,keep_all=FALSE)

for (row in 1:nrow(Dates_unique)){
  thisDate <- Dates_unique[row,1]
  foodData_with_Date <- subset(foodData_Bread, foodData_Bread$month_year == thisDate)
  
  foodData_Median_newRow <- c(toString(thisDate),mean(foodData_with_Date$usd))
  foodData_Median <- rbind(foodData_Median, foodData_Median_newRow)
}

#Überschreibe start und enddatum mit Variablen

ts_test <- ts(as.numeric(foodData_Median$Median), start=c(2006,01),end=c(2017,12), frequency = 12)

fit_test <- auto.arima(ts_test)

plot(forecast(fit_test, 36), col = "darkblue")