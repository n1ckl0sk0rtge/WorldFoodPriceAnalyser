library( leaflet )    
library( shiny )
library( shinydashboard )

# Define UI for application that draws a histogram
ui <- fluidPage(
  # place the contents inside a box
  shinydashboard::box(
    width = 12
    , title = "Click on the map!"
    # separate the box by a column
    , column(
      width = 2
      , shiny::actionButton( inputId = "clearHighlight"
                             , icon = icon( name = "eraser")
                             , label = "Clear the Map"
                             , style = "color: #fff; background-color: #D75453; border-color: #C73232"
      )
    )
    # separate the box by a column
    , column(
      width = 10
      , leaflet::leafletOutput( outputId = "myMap"
                                , height = 850
      )
    )
  ) # end of the box
)