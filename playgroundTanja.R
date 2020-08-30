# R Script Tanja

#Libraries:
library(dplyr)
library(ggplot2)
library(magrittr)
library(ggmap)
library(ggiraph)
library(leaflet)
library(shiny)
library(forecast)
library(mosaic)
library(GGally)
library(splines)
#library(gapminder)
library(modelr)
library(purrr)
library(tidyr)
library(broom)



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

wfp_market_food_prices_final2<-subset(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change)


# Subsets product:
Apples<-subset(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, product=="Apples")
Bananas<-subset(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, product=="Bananas")
Rice<-subset(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, product=="Rice")

# Subsets country:
Afghanistan<-subset(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, country=="Afghanistan")



# PLOTS:

# Farbpalette f?r Plots erstellen
palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")



### Plots auf Produktebene:

# Boxplot Preise pro Land *
plot_price_product_country<-ggplot(data=Apples, mapping=aes(x=country, y=usd, color=country))+geom_boxplot(size=0.8, alpha=0.3)+ggtitle("Prices Apples per country 2006-2017")
ggsave(filename = "plot_price_product_country.png", plot = plot_price_product_country)

# Verteilung Preise pro Sales Channel pro Produkt 2006-2017 *
Apples%>%select(year, usd, sales_channel)%>%ggplot(mapping=aes(x=year, y=usd, color=sales_channel, shape=sales_channel))+geom_jitter(size=1.5, alpha=0.9)+scale_colour_manual(values=palette)+theme_bw()+ggtitle("Price range per product 2006-2017")

# Verteilung Preise pro Sales Channel pro Produkt per Monat *
Apples%>%select(month, usd, sales_channel)%>%ggplot(mapping=aes(x=month, y=usd, color=sales_channel, shape=sales_channel))+geom_jitter(width=0.2, size=1.5, alpha=1)+scale_colour_manual(values=palette)+theme_bw()+ggtitle("Price range per product per month")

# Preisverteilung pro Land ?ber alle Jahre im Datensatz "Apples" *
ggplot(data=Apples, mapping=aes(x=country, y=usd, color=country))+geom_boxplot(size=0.8, alpha=0.3)+ggtitle("Prices Apples per country 2006-2017")

#Histogramme - Haeufigkeit der Laenderpositionen pro Jahr im Datensatz "Apples" ??
Apples%>%select(year, country)%>%ggplot(mapping=aes(x=year, fill=country))+facet_wrap(~country)+geom_histogram(binwidth=0.8)+ggtitle("Count countries per year (Apples)")+theme_bw()

# Preise Apples alle Laender 2006-2017 - Jahresebene Linien - andere Darstellung mit boxplot oben
ggplot(data=Apples, mapping=aes(x=year, y=usd, color=year))+geom_jitter(width=0.1, alpha=0.6)+theme_bw()+ggtitle("Price Apples all countries")

# Preisverteilung "Apples" in USD Laenderebene - andere Darstellung in Linie und Boxplot oben
ggplot(data=Apples, mapping=aes(x=country, y=usd))+geom_jitter(size=1, col="brown")+theme_light()+ggtitle("Prices Apples in USD over all countries")
ggplot(data=Apples, mapping=aes(x=country, y=usd, color=country))+geom_line(width=0.2, size=2, alpha=1)+ggtitle("Preise pro Land (2006-2017)")

#Apples%>%select(year, usd, country)%>%ggplot(mapping=aes(x=year, y=usd, color=country, shape=country))+geom_jitter(size=3, alpha=0.9)+scale_colour_manual(values=palette)+theme_bw()




### Plots Gesamt?bersicht:

# Preise alle Produkte alle Laender auf Jahresebene *
plot_price_all_year<-ggplot(data=wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, mapping=aes(x=year, y=usd, color=year))+geom_jitter(width=0.1, alpha=0.6)+theme_bw()+ggtitle("Prices all products over all countries per year")
ggsave(filename = "plot_price_all_year.png", plot = plot_price_all_year)

# Preise alle Produkte alle Laender auf Monatsebene *
plot_price_all_month<-ggplot(data=wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, mapping=aes(x=month, y=usd, color=month))+geom_jitter(width=0.1, alpha=0.6)+theme_bw()+ggtitle("Prices all products over all countries per month")
ggsave(filename = "plot_price_all_month.png", plot = plot_price_all_month)

# ! Preise 2006-2017 gesamt - zeigt keine Linie an --> Durchschnittspreis pro Jahr ausrechnen
wfp_market_food_prices_final1%>%ggplot(aes(x=year, y=usd))+geom_line()+geom_point(size=3)

# Haeufigkeit pro Produkt und Land *
count_country_product<-wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change%>%select(country, product)%>%ggplot(aes(x=country, y=product))+theme_bw(base_size = 8)+geom_count(col="brown")+ggtitle("Counts per product per country")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(filename="count_country_product.png", plot=count_country_product)



### Plots pro Sales Channel:

# Verteilung Preise pro Sales Channel *
plot_price_sales_channel<-ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=sales_channel, y=usd, color=sales_channel))+geom_jitter(size=0.8, alpha=0.3)+ggtitle("Prices USD per Sales Channel 2006-2017")
ggsave(filename = "plot_price_sales_channel.png", plot = plot_price_sales_channel)

#ggplot(data=wfp_market_food_prices_final1, mapping=aes(x=sales_channel, y=usd, color=sales_channel))+geom_boxplot(size=0.8, alpha=0.3)+ggtitle("Prices USD per Sales Channel 2006-2017")

# Verteilung Preise pro Sales Channel 2006-2017 *zweite oder dritte Plot - Unterschied Farbpalette*
ggplot(data=wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, mapping=aes(x=year, y=usd, color=sales_channel))+geom_jitter(size=1, alpha=0.5)+ggtitle("Price range per Sales Channel 2006-2017")
plot_price_sales_channel_year<-wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change%>%select(year, usd, sales_channel)%>%ggplot(mapping=aes(x=year, y=usd, color=sales_channel, shape=sales_channel))+geom_jitter(size=1, alpha=0.7)+scale_colour_manual(values = palette)+theme_bw()+ggtitle("Price range per Sales Channel 2006-2017")
ggsave(filename = "plot_price_sales_channel_year.png", plot=plot_price_sales_channel_year)
wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change%>%select(year, usd, sales_channel)%>%ggplot(mapping=aes(x=year, y=usd, color=sales_channel, shape=sales_channel))+geom_jitter(size=1.5, alpha=0.9)+theme_bw()+scale_fill_hue()+ggtitle("Price range per Sales Channel 2006-2017")
#ggplot(data=wfp_market_..., mapping=aes(x=year, y=usd, color=sales_channel))+geom_boxplot(size=0.8, alpha=0.3)+ggtitle("Prices USD all countries 2006-2017")

# Verteilung Preise pro Sales Channel pro Monat *
plot_price_sales_channel_month<-ggplot(data=wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, mapping=aes(x=month, y=usd, color=sales_channel))+geom_jitter(width=0.2, size=1, alpha=0.5)+theme_bw()+ggtitle("Price range per Sales Channel per month")
ggsave(filename = "plot_price_sales_channel_month.png", plot = plot_price_sales_channel_month)

# Boxplot Preise pro Sales Channel (nicht so gut geeignet)
ggplot(data=wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, mapping=aes(y=usd, x=sales_channel, color=sales_channel))+geom_boxplot(size=0.8, alpha=0.3)+theme_bw()+ggtitle("Prices per Sales Channel")

# Linie mit Durchschnittspreis in plot auf Jahr-und Monatsebene einbringen -??
#abline(v=median(wfp_market_food_prices_final1$usd), col="magenta", lwd=4)

# Histogram count pro Sales Channel 2006-2017 - nebeneinander gestapelt
#wfp_market_food_prices_final1%>%select(year, sales_channel)%>%ggplot(mapping=aes(x=year, fill=sales_channel))+facet_wrap(~sales_channel)+geom_histogram(binwidth=0.8)

# Haeufigkeit der Produkte pro Sales Channel (gestapelte Punkte) *
count_product_sales_channel<-wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change%>%select(sales_channel, product)%>%ggplot(aes(x=sales_channel, y=product))+geom_count(col="brown")+theme_bw(base_size = 9)+ggtitle("Counts per product on Sales Channel")
ggsave(filename = "count_product_sales_channel.png", plot = count_product_sales_channel)

# Verteilung Preis Retail & Wholesale auf Produkte - nicht geeignet
plot_price_sales_channel_product<-wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change%>%select(usd, product, sales_channel)%>%ggplot(mapping=aes(x=usd, y=product, color=sales_channel, shape=sales_channel))+geom_jitter(width=0.1, size=0.5, alpha=0.7)+scale_colour_manual(values=palette)+theme_bw(base_size = 9)+ggtitle("Price per Sales Channel on products")
ggsave(filename = "plot_price_sales_channel_product.png", plot = plot_price_sales_channel_product)


#Scatterplot Matrix *
ggpairs(Apples, columns=c("usd", "country"), aes(fill=country))+theme_bw(base_size = 7)

#ungeeignet
#ggplot(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, aes(x=month, y=usd)) + geom_step(colour="blue")


# Mittelwert Preise alle Laender 2006 bis 2017
mean_all_prices<-mean(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change$usd)
head(mean_all_prices)
#Standardabweichung
sd(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change$usd)


# Plots speichern:
#ggsave(Dateiname="my_plot.png", plot=my_plot)









# Datensatz Arima Modell und Lineare Regression
Dataset_Arima<-select(wfp_market_food_prices_inUSD_changed_units_and_prices_neu_neu_date_change, product, country, usd, month_year, year)


#Arima Modell:

# Erstellung der Zeitreihe (ts Funktion); Frequency=12 Monate, ab 01/2006 bis 12/2017
price_test_arima<-ts(Dataset_Arima$usd, start=c(2006,1),end=c(2017,12), frequency = 12)
# Plot Zeitreihe - Preisentwicklung 2006 bis 2017
plot(price_test_arima, axes=F, xlab="Entwicklung Preise 2006 bis 2017", col="darkblue")
# Arima Modell erstellen
fit_arima_test<-auto.arima(price_test_arima)
# Plot Prognose f?r die n?chsten 24 Monate
plot(forecast(fit_arima_test, 24), xlab = "Entwicklung Preise 2006 bis 2020", col = "darkblue")
# Trendlinie:
lines(lowess(price_test_arima), col="red")

# Zerlegung der Zeitreihe (seasonal decomposition):
# data: spiegelt den Verlauf der Preise wieder; seasonal: periodischer Anteil, der sich alle 12 Monate wiederholt; trend: Trendwerte abzgl. dem periodischen Anteil; remainder: Differenz aus trend und periodischem Anteil
fit_arima_all_stl<-stl(price_test_arima, s.window = "periodic")
plot(fit_arima_all_stl)

# Seasonplot:
seasonplot(window(price_test_arima, start=c(2012,1), end=c(2017,12)), col = "brown", year.labels=TRUE, main="Prices per month vs year")

# Monthplot - Verlauf der Preise inkl. Durchschnittswerte:
monthplot(price_test_arima, ylab="Price in USD", xlab="Month")+title(main="Prices and mean prices per month")

# Autokorrelation: Korrelation innerhalb der Zeitreihe durch Verschiebung (lag)
acf(price_test_arima)






#Arima Modell am Beispiel Apples:

# Erstellung der Zeitreihe (ts Funktion); Frequency=12 Monate, ab 01/2006 bis 12/2017
price_apples_arima<-ts(Apples$usd, start=c(2006,1),end=c(2017,12), frequency = 12)
# Plot Zeitreihe - Preisentwicklung 2006 bis 2017
plot(price_apples_arima, axes=F, xlab="Entwicklung Preise 2006 bis 2017", col="darkblue")
# Auswahl Zeitfenster der Zeitreihe
Zeitfenster_Apples<-window(price_apples_arima, start=c(2012,1), end=c(2017,12))
# Plot Zeitfenster 2012 bis 2017 und Trendlinie
plot(Zeitfenster_Apples)
lines(lowess(Zeitfenster_Apples), col="red")
# Arima Modell erstellen
fit_arima_apples<-auto.arima(price_apples_arima)
# Plot Prognose f?r die n?chsten 24 Monate
plot(forecast(fit_arima_apples, 24), xlab = "Entwicklung Preise 2006 bis 2020", col = "darkblue")
# Trendlinie:
lines(lowess(price_apples_arima), col="red")

# --> Ergebnis: Prognose der "Apples" Preise in den naechsten 24 Monate liegt bei ca. 17/18 USD (blaue Linie). Dunkle Schattierung: Der Preis wird mit einer Wahrscheinlichkeit von 80% in diesem Bereich liegen. Helle Schattierung: Der Preis wird mit einer Wahrscheinlichkeit von 95% in diesem Bereich liegen.

# Zerlegung der Zeitreihe (seasonal decomposition):
# data: spiegelt den Verlauf der Preise wieder; seasonal: periodischer Anteil, der sich alle 12 Monate wiederholt; trend: Trendwerte abz?glich dem periodischen Anteil; remainder: Differenz aus trend und periodischem Anteil
fit_apples_stl<-stl(price_apples_arima, s.window = "periodic")
plot(fit_apples_stl)

# Seasonplot:
seasonplot(window(price_apples_arima, start=c(2012,1), end=c(2017,12)), col="brown", year.labels=TRUE, main="Prices per month vs year")

# Monthplot - Verlauf der Preise inkl. Durchschnittswerte:
monthplot(price_apples_arima, ylab="Price in USD", xlab="Month")+title(main="Prices and mean prices per month")

# Autocorrelation: Korrelation innerhalb der Zeitreihe durch Verschiebung (lag); blaue Linie= Vertrauensintervalle
acf(price_apples_arima)

# Crosscorrelation: --> macht kein Sinn, da nur eine Zeitreihe vorhanden
# Korrelation zwischen zwei verschiedenen Zeitreihen; Betrachtung zeitversetzt (lag); blaue Linie= Vertrauensintervalle
ccf(window(price_apples_arima, start=c(2017,1), end=c(2017,12)), window(price_apples_arima, start=c(2017,1), end=c(2017,12)), main="Crosscorelation 2017")








# Lineare Regression:
lm<-lm(usd~month_year, data=Dataset_Arima)
summary(lm)
plot(Dataset_Arima$month_year, Dataset_Arima$usd)
abline(lm, lwd=3, col="red")
residuals(lm)
mean(lm$residuals) # Mittelwert lm und residuals (muss nahe 0 liegen)
boxplot(residuals(lm))
hist(residuals(lm))
plot(lm)
shapiro.test(residuals(lm))


# lm Funktion braucht immer die Form (dependent~independent), z.b. Y-Achse: USD dependent variable (predicted); X-Achse independent (predictor)
# Steigung der Linie ist estimate des Koeffizienten von "product" (= unabh?ngige Variable der Daten) -> Wert in Spalte Estimate ab zweiter Zeile
# Standard error der Steigung in Spalte Std.error ab zweiter Zeile
# Ein Koeffizient wird zu 95% zwischen 2 standard errors von seinem Estimate liegen. 
# Mittelwert als horizontale Linie einf?gen, Triangle (Steigung ab dem Mittelpunkt) evtl mit wage und price
# Variance residuals berechnen (var(lm$residuals)); variance Variable (var(data$usd)); variance slope und ic (var(est(ols.slope, ols.ic))
# all.equal(varusd, varRes + varEst)
# var(data)=var(estimate)+var(residuals), d.h. variance of estimate is always less than variane of the data, also variance of residuals
# cov(lm$residuals, data$variablexy) - zeigt dass die residuals nicht mit der Variablen xy korrelieren
# Regression LIne geht durch die DAten, welche den minimum squared error (MSE) haben, die vertikale Distanz zwischen actuals usd und predicted usd durch die Linie
# Schnittpunkt horizontale und vertikale Linie (jeweils Mittelwert der X und Y-Achse); die Steigung der Regression Line ist die Korrelation zwischen den zwei datasets * ratio standard deviation (USD zu Jahr, oder outcomes zu predictors)
# MSE berechnen anhand der Steigung (manipulate Funktion)



# ***Test Lineare Regression am Beispiel Rice***

# Test Beispiel gapminder
# Preisentwicklung pro Produkt pro Jahr
Dataset_Arima%>%ggplot(aes(year, usd, group=product))+geom_line(alpha=1/3) # Linienplot gesamt - evtl. in Jitter aendern
# Einteilung in ein Produkt "Rice" - Verteilung full data, linearer Trend, remaining pattern
Rice<-filter(Dataset_Arima, product=="Rice")
Rice%>%ggplot(aes(year, usd))+geom_jitter(color="darkblue", size=0.8)+theme_bw()+ggtitle("Full data = ")
Rice_mod<-lm(usd~year, data=Rice)
Rice%>%add_predictions(Rice_mod)%>%ggplot(aes(year, pred))+geom_line()+ggtitle("Linear trend+")
Rice%>%add_residuals(Rice_mod)%>%ggplot(aes(year, resid))+geom_hline(yintercept = 0, color="white", size=3)+geom_jitter(color="red")+ggtitle("Remaining pattern")
# Warum sehen die residuals gleich aus, wie die normale Preisverteilung??
summary(Rice_mod) # Zusammenfassung lm

plot(Rice_mod)
#abline(Rice_mod, lwd=3, col="red")

mean(Rice_mod$residuals) # Mittelwert lm und residuals (muss nahe 0 liegen)
#hist(residuals(Rice_mod)) # Histogram Residuals Rice data


# Model quality
broom::glance(Rice_mod)


# ***Ende Beispiel Rice***



# *** Lineare Regression auf alle Produkte und Laender***

# Beispiel gapminder anhand Gruppierung **
# Gruppierung nach Produkt und Land (Variablen werden in data.data gespeichert) - *Nesting*
by_product_country<-Dataset_Arima%>%group_by(product, country)%>%nest()
#by_product_country$data[[520]]
# Modelfitting function  und Zuordnung zu jedem data frame
product_model<-function(df){lm(usd~year, data=df)}
models<-map(by_product_country$data, product_model)
# Erstellen neue Variable in df by_product_country
by_product_country<-by_product_country%>%mutate(model=map(data, product_model))
#by_product_country%>%filter(product=="Bread") # filtern nach Produkt
#by_product_country%>%arrange(country, product) # anordnen Land - Produkt
# ** If your list of data frames and list of models were separate objects, you have to remember that whenever you re-order or subset one vector, you need to re-order or subset all the others in order to keep them in sync. If you forget, your code will continue to work, but it will give the wrong answer!
# residuals hinzufügen
by_product_country<-by_product_country%>%mutate(resids=map2(data, model, add_residuals))
# *unnesting - df in regulären df umwandeln
resids<-unnest(by_product_country, resids)
# Plot residuals total
resids%>%ggplot(aes(year, resid))+geom_jitter(aes(group=product, color="red"), alpha=1/3)+geom_smooth(se=FALSE)+ggtitle("Residuals all products")
# Plot residuals per country and products
resids%>%ggplot(aes(year, resid, group=product))+geom_jitter(alpha=1/3, color="red")+facet_wrap(~country)+theme_bw(base_size = 8)+ggtitle("Residuals per country over all products")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
resids%>%ggplot(aes(year, resid, group=product))+geom_jitter(alpha=1/3, color="red")+facet_wrap(~product)+theme_bw(base_size = 8)+ggtitle("Residuals per product over all countries")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot linear trend per country and product
#Dataset_Arima%>%add_predictions(product_model)%>%ggplot(aes(year, pred, group=product))+geom_line()+ggtitle("Linear trend")
#linear_trend<-unnest(by_product_country, model)
resids%>%ggplot(aes(year, model, group=product))+geom_jitter(alpha=1/3, color="red")+facet_wrap(~country)+theme_bw(base_size = 8)+ggtitle("Residuals per country over all products")+theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Model quality
glance<-by_product_country%>%mutate(glance=map(model, broom::glance))%>%unnest(glance, .drop=TRUE)
# Looking for models that do not fit well - sortieren mit arrange nach r.squared; schlechteste Werte oben (hier: Oil, Ground nuts, Fish, Wheat, Flour usw. in den jeweiligen Ländern)
glance%>%arrange(r.squared)
# Plot r.squared alle Produkte; je höher der Wert desto besser, je naeher an 0 desto schlechter das Model
glance%>%ggplot(aes(product, r.squared))+geom_jitter(width=0.5, color="darkblue")+theme_bw(base_size=9)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Model quality per product")
glance%>%ggplot(aes(country, r.squared))+geom_jitter(width=0.5, color="darkblue")+theme_bw(base_size=9)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Model quality per country")
# Plot bad fit (R^2<0.25 R^2); good fit (R^2>0.75)
bad_fit<-filter(glance, r.squared<0.25)
Dataset_Arima%>%semi_join(bad_fit, by="product")%>%ggplot(aes(year, usd, color=product))+geom_jitter()+ggtitle("Bad Fit on product - r.squared<0.25")
good_fit<-filter(glance, r.squared>0.75)
Dataset_Arima%>%semi_join(good_fit, by="product")%>%ggplot(aes(year, usd, color=product))+geom_jitter()+ggtitle("Good Fit on product - r.squared>0.75")