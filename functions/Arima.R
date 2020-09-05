library(dplyr) 
library(ggplot2)
library(forecast)
library(modelr)

forcastingWithArima <- function(selectedData){
  
  data <- select(selectedData, usd, month_year)
  
  # plot(price_test_arima, axes=F, xlab="Entwicklung Preise 2006 bis 2017", col="darkblue")
  # Arima Modell erstellen
  fit_arima_test <- auto.arima(data$usd, seasonal=FALSE)
  # Plot Prognose fuer die naechsten 36 Monate
  print(forecast(fit_arima_test, 36))
  # Trendlinie:
  #lines(lowess(price_test_arima), col="red")
}
