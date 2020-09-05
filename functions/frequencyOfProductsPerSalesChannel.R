library(ggplot2)

frequencyOfProductsPerSalesChannel <- function(selectedData){
  
  dataframe <- select(selectedData, sales_channel, year, usd)
  
  palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
  
  plot_price_sales_channel_year <- ggplot(data=dataframe, mapping=aes(x=sales_channel, y=usd, color=sales_channel, shape=sales_channel)) + 
      geom_jitter(size=1, alpha=0.7) + 
      scale_colour_manual(values = palette) + 
      theme_bw()
  
  print(plot_price_sales_channel_year)
}