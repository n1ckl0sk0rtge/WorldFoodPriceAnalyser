library(dplyr) 
library(ggplot2)

forcastingWithLinearRegression <- function(selectedData){
  
  data <- select(selectedData, product, country, usd, year)
  
  model <- lm(usd~year, data=data)
  
  year_forcast <- data.frame(year = c("2018-01-01", "2019-01-01", "2020-01-01"))
  price_forcast <- predict.lm(model, data.frame(year = c(as.Date("2018-01-01"), as.Date("2019-01-01"), as.Date("2020-01-01"))))

  result <- data.frame(price = price_forcast, year_forcast, type = "forcast")
  
  return(result)
}