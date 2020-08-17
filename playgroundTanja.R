# R Script Tanja

#Libraries:
#library(dplyr)
#library(ggplot2)
#library(magrittr)
#library(ggmap)
#library(ggiraph)
#library(leaflet)
#library(shiny)
#library(forecast)


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
wfp_USD_2006_2017<-wfp_USD_2006_2017[-c(229061, 228444, 257168, 263535, 263536, 263537, 612296, 641976, 641977, 641978, 641979, 641980, 642102, 642103, 642104, 642105, 642106, 641976, 641977, 641978, 641979, 641980, 228444, 257168, 263535, 263536, 263537, 612296, 641976, 641977, 641978, 641979, 641980, 642102, 642103, 642104, 642105, 642106),]





# Subsets product:
Apples<-subset(wfp_USD, product=="Apples")
Bananas<-subset(wfp_USD, product=="Bananas")


# Subsets country:
Afghanistan<-subset(wfp_USD_2006_2017, country=="Afghanistan")



# Plots:

#Histogramme - Haeufigkeit der Laenderpositionen pro Jahr im Datensatz "Apples"
Apples%>%select(year, country)%>%ggplot(mapping=aes(x=year, fill=country))+facet_wrap(~country)+geom_histogram(binwidth=0.8)+ggtitle("Count countries per year (Apples)")+theme_bw()
# Preise Apples alle Laender alle Jahre
ggplot(data=Apples, mapping=aes(x=year, y=price_USD, color=year))+geom_jitter(width=0.1, alpha=0.6)+theme_bw()+ggtitle("Price Apples all countries")
# Preise alle Produkte alle Laender
ggplot(data=wfp_USD_2006_2017, mapping=aes(x=year, y=price_USD, color=year))+geom_jitter(width=0.1, alpha=0.6)+theme_bw()+ggtitle("Prices all products over all countries")
