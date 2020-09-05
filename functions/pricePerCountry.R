library(ggplot2)

pricePerCountry <- function(selectedData){
  
  dataframe <- select(selectedData, product, year, usd, country)
  
  palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
  
  plot_price_product_country <- ggplot(data=dataframe, mapping=aes(x=country, y=usd, color=country)) + 
      geom_boxplot(size=0.8, alpha=0.3) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      theme(legend.position="none")
  
  print( plot_price_product_country)
}