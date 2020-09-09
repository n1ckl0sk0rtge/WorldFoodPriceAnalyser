library(dplyr) 
library(ggplot2)

forcastingWithLinearRegression <- function(selectedData){

  data <- select(selectedData, usd, year)
  
  model <- lm(usd~year, data=data)
  
  year_forcast <- data.frame(year = seq(as.Date("2018-01-01"), by="1 month", length.out=36))
  price_forcast <- predict.lm(model, year_forcast)
  
  result <- data.frame(price = price_forcast, year = year_forcast)

  return(result)
}