library(ggplot2)

frequencyOfProductsPerSalesChannel <- function(selectedData){
  
  dataframe <- select(selectedData, sales_channel, year, usd)
  
  plot_price_sales_channel_year <- ggplot(data=dataframe, mapping=aes(x=sales_channel, y=usd, shape=sales_channel)) + 
      geom_jitter(size=1, alpha=0.7) + 
      scale_fill_manual(values = c("#2C3E50")) +
      xlab("sales channels") + ylab("price in USD") +
      theme(legend.position="none")
  
  print(plot_price_sales_channel_year)
}