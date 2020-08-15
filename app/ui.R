library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
  
)