library(shiny)
library(leaflet)
library(maps)
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)

source("./../functions/foodPriceDevelopmentLineGraph.R")

worldgeodata <- readRDS("./../data/worldgeodata.RDS")
foodData <- readRDS("./../data/wfp_data.RDS")

server <- function(input, output, session) {

  foundational.map <- shiny::reactive({
    
    foodDataForProduct <- unique(filter(foodData, product == input$selectProducts)$country)
    
    countriesWithThoseProducts <- worldgeodata[which(worldgeodata@data$name %in% foodDataForProduct), ]
    
    leaflet(data = countriesWithThoseProducts) %>%
      setView(lng = 34.933625, lat = 29.903028, zoom = 2.2) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons( data = countriesWithThoseProducts,
                   layerId =  countriesWithThoseProducts@data$name,
                   fillColor = "green", 
                   stroke = FALSE,
                   group = "slectedCountries",
                   
                   label =  countriesWithThoseProducts@data$name,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto")
      )
  })
  
  output$myMap <- renderLeaflet({
    foundational.map()
  })
  
  slectedCountries <- shiny::reactiveValues( ids = vector() )
  
  shiny::observeEvent( input$myMap_shape_click, {
    
    click <- input$myMap_shape_click
    
    slectedCountry <- worldgeodata[which(worldgeodata@data$name == click$id), ]
    
    if(click$id %in% slectedCountries$ids){
      slectedCountries$ids <- slectedCountries$ids[slectedCountries$ids != click$id ]
      
      leaflet::leafletProxy( mapId = "myMap" ) %>%
        addPolygons( data = slectedCountry,
                     layerId = slectedCountry@data$name,
                     fillColor = "green",
                     stroke = FALSE,
                     group = "slectedCountries",
                     
                     label = slectedCountry@data$name,
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto")
        )
      
    } else {
      slectedCountries$ids <- c(slectedCountries$ids, click$id)  
      
      leaflet::leafletProxy( mapId = "myMap" ) %>%
        addPolygons( data = slectedCountry,
                     layerId = slectedCountry@data$name,
                     fillColor = "red",
                     stroke = FALSE,
                     group = "slectedCountries",
                     
                     label = slectedCountry@data$name,
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto")
        )
    }
    
  }) 
  
  output$mainPlot <- renderPlot({
    foodPriceDevelopmentLineGraph(foodData, input, slectedCountries$ids)
  })
  

  
}