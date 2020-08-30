library(ggplot2)

foodPriceDevelopmentLineGraph <- function(foodData, input, countries) {
  
  if (length(countries) == 0) {
    productData <- filter(foodData, product == input$selectProducts)
  } else {
    productData <- filter(foodData, product == input$selectProducts & country %in% countries)
  }
  
  startYear <- paste(input$sliderYears[1], "-01-01", sep="")
  endYear <- paste(input$sliderYears[2], "-01-01", sep="")
  
  productDataInYears <- filter(productData, year >= startYear & year <= endYear)
  
  dataframe <- select(productDataInYears, price_USD, year)
  dataframe$year <- as.Date(dataframe$year)
  
  years <- unique(dataframe$year)
  
  price <- c()
  
  for (i in 1:length(years)){
    yearData <- filter(dataframe, year == years[i])
    price <- c(price, mean(yearData$price_USD))
  }
  
  if(length(price) == length(years)){
    dataframeWithMeanData <- data.frame(price, years)
    #plot
    graph <- ggplot(data=dataframeWithMeanData, aes(x=years, y=price)) +
      geom_bar(stat="identity", fill = "#FF6666")
    
    print(graph)
  }
}