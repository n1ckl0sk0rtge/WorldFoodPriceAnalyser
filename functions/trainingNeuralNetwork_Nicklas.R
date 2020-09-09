library(neuralnet)
library(dplyr) 

data <- readRDS("/Users/nkoertge/Documents/001_StudienDocs/08_SoSe\ 2020/Anwendungsentwicklung/Application/data/wfp_data.RDS")

initNeuronalNetwork <- function(fooddata){

  price <- fooddata$usd
  
  year <- (as.numeric(as.Date(fooddata$month_year)) - 16071) / 132
  country <- (fooddata$country_ID)
  product <- (fooddata$product_ID)
  
  #data <- data.frame(price, year, country, product)
  data <- data.frame(price, year, country, product)
  trainData <- sample_n(data, 500)
  
  View(trainData)
                        # +country+product
  network = neuralnet(price~year+product+country, data=trainData, hidden=c(5,4), algorithm="rprop+",
                      err.fct = "sse", act.fct = "logistic", lifesign = "full", rep=1, stepmax = 5e+6,
                      linear.output = TRUE, threshold = 1)
  
  saveRDS(network, "/Users/nkoertge/Documents/001_StudienDocs/08_SoSe\ 2020/Anwendungsentwicklung/Application/data/neuralnetworkForPricePrediction5.RDS")
  
  print(network$result.matrix)
}

initNeuronalNetwork(data)

