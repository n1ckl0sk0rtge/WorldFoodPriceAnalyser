# source: https://stackoverflow.com/questions/48432061/turn-states-on-a-map-into-clickable-objects-in-shiny

# install necessary packages
# install.packages( pkgs = c( "devtools", "shiny", "shinydashboard" ) )

# load necessary packages
library( leaflet )    
library( shiny )
library( shinydashboard )
library(maps)
library(sp)
library(rgdal)

# data <- read.csv("./../wfp_market_food_prices_inUSD.csv")

# import City of Chicago current community area boundaries
worldgeodata <- rgdal::readOGR("./../data/worldWithoutFranchGuinea.geojson")

worldMap = map("world", fill = TRUE, plot = FALSE)


server <- function(input, output, session) {
  # create foundational map
  foundational.map <- shiny::reactive({
    leaflet(data = worldgeodata) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(fillColor = "yellow", stroke = FALSE) %>%
       # setView( lng = -87.567215
       #         , lat = 41.822582
       #         , zoom = 11 ) %>%
      addPolygons( data = worldgeodata
                   , fillOpacity = 0
                   , opacity = 0.0
                   , layerId = worldgeodata$name
                   , group = "click.list"
      )
  })
  
  output$myMap <- renderLeaflet({
    
    foundational.map()
    
  }) # end of leaflet::renderLeaflet({})
  
  # store the list of clicked polygons in a vector
  click.list <- shiny::reactiveValues( ids = vector() )
  # observe where the user clicks on the leaflet map
  # during the Shiny app session
  # Courtesy of two articles:
  # https://stackoverflow.com/questions/45953741/select-and-deselect-polylines-in-shiny-leaflet
  # https://rstudio.github.io/leaflet/shiny.html
  shiny::observeEvent( input$myMap_shape_click, {
    # store the click(s) over time
    click <- input$myMap_shape_click
    # store the polygon ids which are being clicked
    click.list$ids <- c( click.list$ids, click$id ) 
    # filter the spatial data frame
    # by only including polygons
    # which are stored in the click.list$ids object
    lines.of.interest <- worldgeodata[ which( worldgeodata$name %in% click.list$ids) , ]
    # if statement
    if( is.null( click$id ) ){
      # check for required values, if true, then the issue
      # is "silent". See more at: ?req
      req( click$id )
    } else if( !click$id %in% lines.of.interest@data$id){
      # call the leaflet proxy
      leaflet::leafletProxy( mapId = "myMap" ) %>%
      # and add the polygon lines
      # using the data stored from the lines.of.interest object
      addPolylines( data = lines.of.interest,
                    layerId = lines.of.interest@data$id,
                    stroke = TRUE,
                    color = "black",
                    weight = 1
      )
      
    }
  }) # end of shiny::observeEvent({})
  # Create the logic for the "Clear the map" action button
  # which will clear the map of all user-created highlights
  # and display a clean version of the leaflet map
  shiny::observeEvent( input$clearHighlight, {
    # recreate $myMap
    output$myMap <- leaflet::renderLeaflet({
      # first
      # set the reactive value of click.list$ids to NULL
      click.list$ids <- NULL
      # second
      # recall the foundational.map() object
      foundational.map()
    }) # end of re-rendering $myMap
  }) # end of clearHighlight action button logic
}