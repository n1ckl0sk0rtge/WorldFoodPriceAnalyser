library(ggplot2)

testrenderer <- function(selectedData, forcastData){
  
  data <- select(selectedData, year, usd)
  
  plot <- ggplot(data=data, aes(year, usd)) + 
    geom_jitter(color="darkblue", size=1) + 
    theme_bw()
  
  print(plot)
  
}