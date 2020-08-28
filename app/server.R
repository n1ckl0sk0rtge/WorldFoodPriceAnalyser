library(shiny)
library(leaflet)
library(maps)
library(sp)
library(rgdal)

worldgeodata <- readRDS("./../data/worldgeodata.RDS")

server <- function(input, output, session) {
  # create foundational map
  foundational.map <- shiny::reactive({
    leaflet(data = worldgeodata) %>%
      setView(lng = 34.933625, lat = 29.903028, zoom = 2.2) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(fillColor = "green", stroke = FALSE) %>%
      addPolygons( data = worldgeodata
                   , fillOpacity = 0
                   , opacity = 0.0
                   , layerId = worldgeodata$name
                   , group = "click.list"
      )
  })
  
  
  output$myMap <- renderLeaflet({
    foundational.map()
  })
  
  
  # store the list of clicked polygons in a vector
  click.list <- shiny::reactiveValues( ids = vector() )
  
  
  
  shiny::observeEvent( input$myMap_shape_click, {
    # store the click(s) over time
    click <- input$myMap_shape_click
    click.list$ids <- c( click.list$ids, click$id ) 
    
    lines.of.interest <- worldgeodata[ which( worldgeodata$name %in% click.list$ids) , ]

    if( is.null( click$id ) ){
      req( click$id )
    } else if( !click$id %in% lines.of.interest@data$id){
      leaflet::leafletProxy( mapId = "myMap" ) %>%
      addPolylines( data = lines.of.interest,
                    layerId = lines.of.interest@data$id,
                    stroke = TRUE,
                    color = "black",
                    weight = 1
      )
    }
  }) # end of shiny::observeEvent({})
  
}