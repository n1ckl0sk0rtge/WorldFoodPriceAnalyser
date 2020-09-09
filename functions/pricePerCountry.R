library(ggplot2)

pricePerCountry <- function(selectedData){
  
  dataframe <- select(selectedData, product, year, usd, country)
  
  plot_price_product_country <- ggplot(data=dataframe, mapping=aes(x=country, y=usd)) + 
      geom_boxplot(size=0.8, alpha=0.3) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      xlab("countries") + ylab("price in USD") +
      scale_fill_manual(values = c("#2C3E50")) +
      theme(legend.position="none")
  
  print(plot_price_product_country)
}