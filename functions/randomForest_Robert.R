library(randomForest)
library(ggplot2)
library(magrittr)
library(dplyr)
library(caret)
library(forecast)
library(zoo)
library(tibble)


foodData <- readRDS("./data/wfp_data.RDS")

foodData$month_year <- as.Date(foodData$month_year)

#Auf Food, Land filtern möglich vl
#gibt es monate in denen das produkt billiger ist?
foodData_Bread <- subset(foodData, product_ID == 145)

foodData_Bread[is.na(foodData_Bread)] <- 0

############ New Approach
plot_org <- foodData_Bread %>% ggplot(aes(month_year, usd)) + geom_line() + theme_minimal() + labs(title = "Brot", x= "Datum", y = "Preis")
View(plot_org)

foodData_Bread <- foodData_Bread %>% select(month_year, everything()) #Date nach vorne

Dates_unique <- distinct(foodData_Bread,month_year,keep_all=FALSE)

foodData_Median <- data.frame(Date=as.Date(character()), Median=double())
foodData_Median_newRow <- data.frame(Date=as.Date(character()), Median=double())


for (row in 1:nrow(Dates_unique)){
  thisDate <- Dates_unique[row,1]
  foodData_with_Date <- subset(foodData_Bread, foodData_Bread$month_year == thisDate)
  
  foodData_Median_newRow <- c(toString(thisDate),mean(foodData_with_Date$usd))
  foodData_Median <- rbind(foodData_Median, foodData_Median_newRow)
}

colnames(foodData_Median) <- c("Date","Median")

#foodData_Median <- data.frame(foodData_Median[,1],foodData_Median[,2])
#names(foodData_Median) <- c('month_year','median')

#foodData_Median$month_year <- as.Date(foodData_Median$month_year) 


#Mit Zoo 

z <- read.zoo(foodData_Median, format = "%Y-%m-%d")

foodData_Bread_ts <- as.ts(foodData_Median) #Date wird kaputtgemacht... was dagegen machen?



test_2 <- embed(test_1, lag_order +1)

y_train <- test_2[,1]
x_train <- test_2[,-1]


y_test <- window(test_1, start=c("2018-01-01"),end=c("2018-12-01"))
x_test <- test_2[nrow(test_2), c(1:1)]

forecasts_rf <- numeric(horizon)


for(i in 1:horizon){ 
  set.seed(2019) 
  fit_rf <- randomForest(x_train, y_train) 
  forecasts_rf[i] <- predict(fit_rf, x_test) 
  y_train <- y_train[-1] 
  x_train <- x_train[-nrow(x_train), ]
}








foodData_Bread_org <- window(foodData_Bread_ts, end =c(2016,12))
n_diffs <- nsdiffs(foodData_Bread_org) #Fehler Non seasonal data




