library(neuralnet)
library(dplyr) 
library(ggplot2)

forcastingWithNN <- function(selectedData){
  model <- readRDS("./../data/neuralnetworkForPricePrediction.RDS")
  
  product_value <- unique(select(selectedData, product_ID))$product_ID[1]
  counrty_value <- unique(select(selectedData, country_ID))$country_ID[1]
  
  county <- (c(counrty_value, counrty_value, counrty_value) - 1) / 70000
  year <- (c(2018,2019,2020) - 2006) / 11
  product <- (c(product_value, product_value, product_value) - 50) / 440
  testData <- data.frame(year, county, product)
  
  predict <- compute(model, testData)
  
  year_forcast <- data.frame(year = c("2018-01-01", "2019-01-01", "2020-01-01"))
  price_forcast <- predict$net.result
  
  result <- data.frame(price = price_forcast, year_forcast, type = "forcast")
  
  return(result)
  
}