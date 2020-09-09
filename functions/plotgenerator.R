
foodData <- readRDS("/Users/nkoertge/Documents/001_StudienDocs/08_SoSe 2020/Anwendungsentwicklung/Application/data/wfp_data.RDS")

plot_price_sales_channel_product <- foodData%>%select(usd, product, sales_channel)%>%ggplot(mapping=aes(x=usd, y=product, color=sales_channel, shape=sales_channel)) + 
  geom_jitter(width=0.1, size=0.5, alpha=0.7) + 
  scale_colour_manual(values=c("#2C3E50", "#517394")) + 
  theme_bw(base_size = 9) + 
  ggtitle("Price per Sales Channel on products")

ggsave(filename = "plot_price_sales_channel_product.png", plot = plot_price_sales_channel_product)

# count_country_product <- foodData%>%select(country, product)%>%ggplot(aes(x=country, y=product)) + 
#   theme_bw(base_size = 8) + 
#   geom_count(col="#2C3E50") + 
#   ggtitle("Counts per product per country") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# plot(count_country_product)

# ggsave(filename="count_country_product.png", plot=count_country_product)
