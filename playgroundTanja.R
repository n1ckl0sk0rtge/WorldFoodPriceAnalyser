#Libraries:
#library(dplyr)
#library(ggplot2)
#library(magrittr)
#library(ggmap)
#library(ggiraph)
#library(leaflet)
#library(shiny)
#library(forecast)
#library(mosaic)
#library(GGally)


# Datentabelle laden




# Variable mit Spaltennamen erstellen und in "wfp_sortiert" anpassen
cnames_wfp<-c("country_ID", "country", "city_ID", "city", "market_ID", "market_city", "product_ID", "product", "currency_ID", "currency", "sales_channel_ID", "sales_channel", "unit_ID", "unit", "month", "year", "price_local_currency", "commodity_source")
colnames(wfp_sortiert)<-cnames_wfp


# Daten anschauen
class(wfp_sortiert) # Anzeige Kategorie des Datensatzes
head(wfp_sortiert)
tails(wfp_sortiert, 15) # Anzeige letzte 15 Zeilen des Datensatzes
#data(wfp_sortiert) #?
names(wfp_sortiert) # Anzeige Spaltennamen
dim(wfp_sortiert) # Summe Zeilen und Spalten des Datensatzes
summary(wfp_sortiert) # min, 1st qu., median, mean, 3rd qu., max pro Spalte
object.size(wfp_sortiert) # Groesse Datensatz
str(wfp_sortiert) # Struktur der Daten
ls(wfp_sortiert) # Liste der Spaltennamen
table(wfp_sortiert$product) #
nrow(wfp_sortiert) # Anzahl der Zeilen
ncol(wfp_sortiert) # Anzahl der Spalten
#tapply(wfp_sortiert$price, mean) #
tapply(wfp_sortiert$country, wfp_sortiert$product=="Apples", summary) # Anzahl der Spalten von "Apples" and gesamten Datensatz
wfp_sortiert[wfp_sortiert$product == 'Apples',] # Anzeige Datensatz gefiltert auf "Apples"




# Zusammenfuegen der Datensaetze "wfp_sortiert" und "wfp_market_food_prices_inUSD"
wfp_USD<-cbind(wfp_sortiert, wfp_market_food_prices_inUSD[c('mp_price')])


# Spaltennamen in wfp_USD anpassen - Variable "cnames_USD" mit Spaltennamen erstellen und in "wfp_USD" anpassen
cnames_USD<-c("country_ID", "country", "city_ID", "city", "market_ID", "market_city", "product_ID", "product", "currency_ID", "currency", "sales_channel_ID", "sales_channel", "unit_ID", "unit", "month", "year", "price_local_currency", "commodity_source", "price_USD")
colnames(wfp_USD)<-cnames_USD


# Filtern: Jahre >2005, Produkte mit wenig Datensaetzen (z.B. nur fuer ein Land) oder nicht relevante Produkte und in "wfp_USD_2006_2017" speichern
wfp_USD_2006_2017<-subset(wfp_USD, year>2005 & product!="Livestock animals" & product!="Exchange rate" & product!="Exchange rate (unofficial)" & product!="Charcoal" & product!="Passion fruit" & product!="Cotton" & product!="Blackberry" & product!="Broccoli" & product!="Cashew" & product!="Fonio" & product!="Gari" & product!="Ghee" & product!="Labaneh" & product!="Poultry" & product!="Pumpkin" & product!="Tortilla (maize)" & product!="Transport (public)" & product!="Beetroots" & product!="Noodles" & product!="Sesame" & product!="Water (drinking)" & product!="Peanut" & product!="Cornstarch" & product!="Buckwheat grits" & product!="Cassava leaves" & product!="Curd" & product!="Dates" & product!="Guava" & product!="Lettuce" & product!="Sour cream" & product!="Parsley")
# Datei speichern
write.csv(wfp_USD_2006_2017,"wfp_USD_2006_2017.csv")


# Erstellen neuer Datensatz ohne 0,0 Preis und ohne "Wage", "Producer", "Farm Gate"
wfp_market_food_prices_USD_neu<-subset(wfp_market_food_prices_inUSD_changed_units_and_prices, usd>0.0000000000 & product!="Wage" & sales_channel!="Farm Gate" & sales_channel!="Producer")
# Datensatz speichern als csv
write.csv(wfp_market_food_prices_USD_neu, "wfp_USD_2006_2017_neu.csv")

# Datensatz bereiningt um "MT" und neu speichern als csv
wfp_market_food_prices_final<-subset(wfp_market_food_prices_inUSD_changed_units_and_prices_neu, unit!="MT")
write.csv(wfp_market_food_prices_final, "wfp_food_prices_2006_2017_neu_neu.csv")

# Weiterverarbeitung mit angepassten Einheiten und Preisen:
wfp_market_food_prices_final1<-subset(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu)
write.csv(wfp_market_food_prices_final1, "wfp_food_prices_2006_2017_final1.csv")


# Umwandlung Datumsformat:
wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change$month_year <- format(as.Date(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change$month_year, format = "%Y-%m-%d"))




# Subsets product:
Apples<-subset(wfp_market_food_prices_final1, product=="Apples")
Bananas<-subset(wfp_market_food_prices_final1, product=="Bananas")


# Subsets country:
Afghanistan<-subset(wfp_market_food_prices_USD_neu, country=="Afghanistan")



# PLOTS:

# Farbpalette f�r Plots erstellen
palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")



### Plots auf Produktebene:

# Boxplot Preise pro Land *
plot_price_product_country<-ggplot(data=Apples, mapping=aes(x=country, y=usd, color=country))+geom_boxplot(size=0.8, alpha=0.3)+ggtitle("Prices Apples per country 2006-2017")
ggsave(filename = "plot_price_product_country.png", plot = plot_price_product_country)

# Verteilung Preise pro Sales Channel pro Produkt 2006-2017 *
Apples%>%select(year, usd, sales_channel)%>%ggplot(mapping=aes(x=year, y=usd, color=sales_channel, shape=sales_channel))+geom_jitter(size=1.5, alpha=0.9)+scale_colour_manual(values=palette)+theme_bw()+ggtitle("Price range per product 2006-2017")

# Verteilung Preise pro Sales Channel pro Produkt per Monat *
Apples%>%select(month, usd, sales_channel)%>%ggplot(mapping=aes(x=month, y=usd, color=sales_channel, shape=sales_channel))+geom_jitter(width=0.2, size=1.5, alpha=1)+scale_colour_manual(values=palette)+theme_bw()+ggtitle("Price range per product per month")

# Preisverteilung pro Land �ber alle Jahre im Datensatz "Apples" *
ggplot(data=Apples, mapping=aes(x=country, y=usd, color=country))+geom_boxplot(size=0.8, alpha=0.3)+ggtitle("Prices Apples per country 2006-2017")

#Histogramme - Haeufigkeit der Laenderpositionen pro Jahr im Datensatz "Apples" ??
Apples%>%select(year, country)%>%ggplot(mapping=aes(x=year, fill=country))+facet_wrap(~country)+geom_histogram(binwidth=0.8)+ggtitle("Count countries per year (Apples)")+theme_bw()

# Preise Apples alle Laender 2006-2017 - Jahresebene Linien - andere Darstellung mit boxplot oben
ggplot(data=Apples, mapping=aes(x=year, y=usd, color=year))+geom_jitter(width=0.1, alpha=0.6)+theme_bw()+ggtitle("Price Apples all countries")

# Preisverteilung "Apples" in USD Laenderebene - andere Darstellung in Linie und Boxplot oben
ggplot(data=Apples, mapping=aes(x=country, y=usd))+geom_jitter(size=1, col="brown")+theme_light()+ggtitle("Prices Apples in USD over all countries")
ggplot(data=Apples, mapping=aes(x=country, y=usd, color=country))+geom_line(width=0.2, size=2, alpha=1)+ggtitle("Preise pro Land (2006-2017)")

#Apples%>%select(year, usd, country)%>%ggplot(mapping=aes(x=year, y=usd, color=country, shape=country))+geom_jitter(size=3, alpha=0.9)+scale_colour_manual(values=palette)+theme_bw()




### Plots Gesamt�bersicht:

# Preise alle Produkte alle Laender auf Jahresebene *
plot_price_all_year<-ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=year, y=usd, color=year))+geom_jitter(width=0.1, alpha=0.6)+theme_bw()+ggtitle("Prices all products over all countries per year")
ggsave(filename = "plot_price_all_year.png", plot = plot_price_all_year)

# Preise alle Produkte alle Laender auf Monatsebene *
plot_price_all_month<-ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=month, y=usd, color=month))+geom_jitter(width=0.1, alpha=0.6)+theme_bw()+ggtitle("Prices all products over all countries per month")
ggsave(filename = "plot_price_all_month.png", plot = plot_price_all_month)

# ! Preise 2006-2017 gesamt - zeigt keine Linie an --> Durchschnittspreis pro Jahr ausrechnen
wfp_market_food_prices_final1%>%ggplot(aes(x=year, y=usd))+geom_line()+geom_point(size=3)

# Haeufigkeit pro Produkt und Land *
count_country_product<-wfp_market_food_prices_final1%>%select(country, product)%>%ggplot(aes(x=country, y=product))+theme_bw(base_size = 8)+geom_count(col="brown")+ggtitle("Counts per product per country")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="count_country_product.png", plot=count_country_product)



### Plots pro Sales Channel:

# Verteilung Preise pro Sales Channel *
plot_price_sales_channel<-ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=sales_channel, y=usd, color=sales_channel))+geom_jitter(size=0.8, alpha=0.3)+ggtitle("Prices USD per Sales Channel 2006-2017")
ggsave(filename = "plot_price_sales_channel.png", plot = plot_price_sales_channel)

#ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=sales_channel, y=usd, color=sales_channel))+geom_boxplot(size=0.8, alpha=0.3)+ggtitle("Prices USD per Sales Channel 2006-2017")

# Verteilung Preise pro Sales Channel 2006-2017 *zweite oder dritte Plot - Unterschied Farbpalette*
ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=year, y=usd, color=sales_channel))+geom_jitter(size=1, alpha=0.5)+ggtitle("Price range per Sales Channel 2006-2017")
plot_price_sales_channel_year<-wfp_market_food_prices_final1%>%select(year, usd, sales_channel)%>%ggplot(mapping=aes(x=year, y=usd, color=sales_channel, shape=sales_channel))+geom_jitter(size=1, alpha=0.7)+scale_colour_manual(values = palette)+theme_bw()+ggtitle("Price range per Sales Channel 2006-2017")
ggsave(filename = "plot_price_sales_channel_year.png", plot=plot_price_sales_channel_year)
wfp_market_food_prices_final1%>%select(year, usd, sales_channel)%>%ggplot(mapping=aes(x=year, y=usd, color=sales_channel, shape=sales_channel))+geom_jitter(size=1.5, alpha=0.9)+theme_bw()+scale_fill_hue()+ggtitle("Price range per Sales Channel 2006-2017")
#ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=year, y=usd, color=sales_channel))+geom_boxplot(size=0.8, alpha=0.3)+ggtitle("Prices USD all countries 2006-2017")

# Verteilung Preise pro Sales Channel pro Monat *
plot_price_sales_channel_month<-ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=month, y=usd, color=sales_channel))+geom_jitter(width=0.2, size=1, alpha=0.5)+theme_bw()+ggtitle("Price range per Sales Channel per month")
ggsave(filename = "plot_price_sales_channel_month.png", plot = plot_price_sales_channel_month)

# Boxplot Preise pro Sales Channel (nicht so gut geeignet)
ggplot(data=wfp_market_food_prices_final1, mapping=aes(y=usd, x=sales_channel, color=sales_channel))+geom_boxplot(size=0.8, alpha=0.3)+theme_bw()+ggtitle("Prices per Sales Channel")

# Linie mit Durchschnittspreis in plot auf Jahr-und Monatsebene einbringen -??
#abline(v=median(wfp_market_food_prices_final1$usd), col="magenta", lwd=4)

# Histogram count pro Sales Channel 2006-2017 - nebeneinander gestapelt
#wfp_market_food_prices_final1%>%select(year, sales_channel)%>%ggplot(mapping=aes(x=year, fill=sales_channel))+facet_wrap(~sales_channel)+geom_histogram(binwidth=0.8)

# Haeufigkeit der Produkte pro Sales Channel (gestapelte Punkte) *
count_product_sales_channel<-wfp_market_food_prices_final1%>%select(sales_channel, product)%>%ggplot(aes(x=sales_channel, y=product))+geom_count(col="brown")+theme_bw(base_size = 9)+ggtitle("Counts per product on Sales Channel")
ggsave(filename = "count_product_sales_channel.png", plot = count_product_sales_channel)

# Verteilung Preis Retail & Wholesale auf Produkte - nicht geeignet
plot_price_sales_channel_product<-wfp_market_food_prices_final1%>%select(usd, product, sales_channel)%>%ggplot(mapping=aes(x=usd, y=product, color=sales_channel, shape=sales_channel))+geom_jitter(width=0.1, size=0.5, alpha=0.7)+scale_colour_manual(values=palette)+theme_bw(base_size = 9)+ggtitle("Price per Sales Channel on products")
ggsave(filename = "plot_price_sales_channel_product.png", plot = plot_price_sales_channel_product)


#Scatterplot Matrix *
ggpairs(Apples, columns=c("usd", "country"), aes(fill=country))+theme_bw(base_size = 7)
