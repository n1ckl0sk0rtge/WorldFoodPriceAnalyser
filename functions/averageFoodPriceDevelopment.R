library(ggplot2)

averageFoodPriceDevelopment <- function(selectedData, extraYearData) {
  
  dataframe <- select(selectedData, year, usd)
  
  years <- unique(dataframe$year)
  price <- c()

  for (i in 1:length(years)){
    yearData <- filter(dataframe, year == years[i])
    price <- c(price, mean(yearData$usd))
  }

  if(length(price) == length(years)){
    dataframeWithMeanData <- data.frame(price, year = years, type = "origin")
    
    if(! is.null(extraYearData)){
      dataframeWithMeanData <- rbind(dataframeWithMeanData, extraYearData)
    }
    
    graph <- ggplot(data=dataframeWithMeanData, aes(x=year, y=price, fill=type)) +
      geom_bar(stat="identity")

    print(graph)
  }
  
}