library(ggplot2)
library(reshape2)

averageFoodPriceDevelopment <- function(selectedData, extraYearData) {
  
  dataframe <- select(selectedData, month_year, usd)
  
  years <- unique(dataframe$month_year)
  price <- c()
  # 
  for (i in 1:length(years)){
    yearData <- filter(dataframe, month_year == years[i])
    price <- c(price, mean(yearData$usd))
  }

  if(length(price) == length(years)){
    dataframeWithMeanData <- data.frame(price = price, year = as.Date(years))
    
    if(! is.null(extraYearData)){
      dataframeWithMeanData <- rbind(dataframeWithMeanData, extraYearData)
      
      graph <- ggplot(data=dataframeWithMeanData, aes(x=year)) +
        geom_line(aes(y = price), color = "#2C3E50", size=1.5) +
        geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="dashed", 
                   color = "#2C3E50", size=0.5) +
        xlab("year") + ylab("price in USD") +
        theme(legend.position="none")
    } else {
      graph <- ggplot(data=dataframeWithMeanData, aes(x=year)) +
        geom_line(aes(y = price), color = "#2C3E50", size=1.5) + 
        xlab("year") + ylab("price in USD") +
        theme(legend.position="none")
    }
  
    print(graph)
  }
  
}