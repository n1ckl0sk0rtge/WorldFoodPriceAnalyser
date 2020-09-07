library(devtools)
library(shiny)
library(leaflet)
library(maps)
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)
library(png)
library(broom)
library(ggfortify)

source("./../functions/dataPreprocessFunction.R")

source("./../functions/averageFoodPriceDevelopment.R")
source("./../functions/pricePerCountry.R")
source("./../functions/frequencyOfProductsPerSalesChannel.R")

source("./../functions/NeuronalNetwork_Nicklas.R")
source("./../functions/LinearRegression_Tanja.R")

worldgeodata <- readRDS("./../data/worldgeodata.RDS")
foodData <- readRDS("./../data/wfp_data.RDS")






server <- function(input, output, session) {

  foundational.map <- shiny::reactive({
    
    foodDataForProduct <- unique(filter(foodData, product == input$selectProducts)$country)
    
    countriesWithThoseProducts <- worldgeodata[which(worldgeodata@data$name %in% foodDataForProduct), ]
    
    leaflet(data = countriesWithThoseProducts) %>%
      setView(lng = 34.933625, lat = 29.903028, zoom = 2) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(minZoom = 2, maxZoom = 3)) %>%
      addPolygons( data = countriesWithThoseProducts,
                   layerId =  countriesWithThoseProducts@data$name,
                   fillColor = "green", 
                   stroke = FALSE,
                   group = "selectedCountries",
                   
                   label =  countriesWithThoseProducts@data$name,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto")
      )
  })
  
  output$myMap <- renderLeaflet({
    selectedCountries$ids <- NULL
    foundational.map()
  })
  
  selectedCountries <- shiny::reactiveValues( ids = vector() )
  
  shiny::observeEvent( input$myMap_shape_click, {
    click <- input$myMap_shape_click
    
    slectedCountry <- worldgeodata[which(worldgeodata@data$name == click$id), ]
    
    if(click$id %in% selectedCountries$ids){
      selectedCountries$ids <- selectedCountries$ids[selectedCountries$ids != click$id ]
      
      leaflet::leafletProxy( mapId = "myMap" ) %>%
        addPolygons( data = slectedCountry,
                     layerId = slectedCountry@data$name,
                     fillColor = "green",
                     stroke = FALSE,
                     group = "selectedCountries",
                     
                     label = slectedCountry@data$name,
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto")
        )
      
    } else {
      selectedCountries$ids <- c(selectedCountries$ids, click$id)  
      
      leaflet::leafletProxy( mapId = "myMap" ) %>%
        addPolygons( data = slectedCountry,
                     layerId = slectedCountry@data$name,
                     fillColor = "red",
                     stroke = FALSE,
                     group = "selectedCountries",
                     
                     label = slectedCountry@data$name,
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto")
        )
    }
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observe({
    foodDataForProduct <- unique(filter(foodData, product == input$selectProductsForForcast)$country)

    updateSelectInput(session, "selectCountryForForcast",
                      choices = foodDataForProduct
    )
  })

  output$averageFoodPriceDevelopmentPlot <- renderPlot({
    selectedData <- preprocessData(foodData, input$selectProducts, input$sliderYears, selectedCountries$ids)
    averageFoodPriceDevelopment(selectedData, NULL)
  })
  
  output$pricePerCountryPlot <- renderPlot({
    selectedData <- preprocessData(foodData, input$selectProducts, input$sliderYears, selectedCountries$ids)
    pricePerCountry(selectedData)
  })
  
  output$frequencyOfProductsPerSalesChannelPlot <- renderPlot({
    selectedData <- preprocessData(foodData, input$selectProducts, input$sliderYears, selectedCountries$ids)
    frequencyOfProductsPerSalesChannel(selectedData)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  output$forcastPlot <- renderPlot({
    selectedData <- preprocessData(foodData, input$selectProductsForForcast, list(2006, 2017), input$selectCountryForForcast)
    
    switch(input$selectForecastModelForForcast,
           "1" = forcast <- forcastingWithLinearRegression(selectedData),
           "2" = forcast <- forcastingWithNN(selectedData),
           forcast <- forcastingWithNN(selectedData)
    )
    
    averageFoodPriceDevelopment(selectedData, forcast)
  })
  
  output$regressionsGeradeLM <- renderPlot({
    selectedData <- preprocessData(foodData, input$selectProductsForForcast, list(2006, 2017), input$selectCountryForForcast)
    
    data <- select(selectedData, product, country, usd, year)
    model <- lm(usd~year, data=data)
      
    plot <- ggplot(data = data, aes(x = year, y = usd)) +
      geom_point() +
      stat_smooth(method = "lm", col = "red") +
      theme(panel.background = element_rect(fill = "white"),axis.line.x=element_line(), axis.line.y=element_line())
    
    print(plot)
  })
  
  output$residualsLM <- renderPlot({
    selectedData <- preprocessData(foodData, input$selectProductsForForcast, list(2006, 2017), input$selectCountryForForcast)
    
    data <- select(selectedData, product, country, usd, year)
    model <- lm(usd~year, data=data)
    
    plot <- ggplot(data=data, aes(model$residuals)) + 
      geom_histogram(binwidth = 1, color = "black", fill = "purple4") + 
      theme(panel.background = element_rect(fill = "white"),axis.line.x=element_line(),axis.line.y=element_line())
    
    print(plot)
  })
  
  output$summaryOfModel <- renderPrint({
    selectedData <- preprocessData(foodData, input$selectProductsForForcast, list(2006, 2017), input$selectCountryForForcast)
    
    switch(input$selectForecastModelForForcast,
           "1" = model <- lm(usd~year, data=selectedData),
           "2" = model <- readRDS("./../data/neuralnetworkForPricePredictionFinal.RDS"),
           model <- lm(usd~year, data=selectedData)
    )
    
    summary(model)
  })
  
  output$qualityOfModel <- renderPrint({
    selectedData <- preprocessData(foodData, input$selectProductsForForcast, list(2006, 2017), input$selectCountryForForcast)
    
    switch(input$selectForecastModelForForcast,
           "1" = model <- lm(usd~year, data=selectedData),
           "2" = model <- readRDS("./../data/neuralnetworkForPricePredictionFinal.RDS"),
           model <- lm(usd~year, data=selectedData)
    )
    
    if(input$selectForecastModelForForcast != "2"){
      print(broom::glance(model))
    } else {
      print("No values")
    }
    
  })


  
}