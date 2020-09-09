library(neuralnet)
library(dplyr) 
library(ggplot2)

forcastingWithNN <- function(selectedData){
  model <- readRDS("./../data/neuralnetworkForPricePrediction5.RDS")
  
  product_value <- unique(select(selectedData, product_ID))$product_ID[1]
  counrty_value <- unique(select(selectedData, country_ID))$country_ID[1]
  
  county <- c(counrty_value, counrty_value, counrty_value) #- 1) / 70000
  year <- (as.numeric(seq(as.Date("2018-01-01"), by="1 month", length.out=36)) - 16071) / 132
  product <- c(product_value, product_value, product_value) #- 50) / 440
  
  testData <- data.frame(year, county, product)
  
  predict <- compute(model, testData)
  
  #print(predict)
  
  year_forcast <- year_forcast <- data.frame(year = seq(as.Date("2018-01-01"), by="1 month", length.out=36))
  price_forcast <- predict$net.result
  
  result <- data.frame(price = price_forcast, year = year_forcast)
  
  return(result)
  
}