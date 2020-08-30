library(shiny)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(leaflet.extras)

# Read data from rds-file
data <- readRDS("./../data/wfp_data.RDS")
# list countries without duplicates
countrysWithoutDuplicates <- unique(data$country)

productsWithoutDuplicates <- unique(data$product)

yearsWithoutDuplicates <- strtoi(substr(sort(unique(data$year)), 1, 4))

# define user interface
ui <- fluidPage(
  # Application title
  titlePanel("World Food Prices in Developing Countries"),
  h5(em("created by: Tanja Bühr, Nicklas Körtge, Robert Bilger")),
  
  # define site layout  
  sidebarLayout(
    
    sidebarPanel(
      # define selector for countries
      #selectInput("select", label = h4("Select Country"), 
      #            choices = c(list("All countrys" = 1), countrysWithoutDuplicates),
      #           selected = 1),
      #
      #hr(),

      selectInput("selectProducts", label = h4("Select product"), 
                  choices = productsWithoutDuplicates, 
                  selected = 1),
      
      hr(),
      
      # make a group of checkboxes for years
      sliderInput("sliderYears", label = h4("Select years"),
                  min = head(yearsWithoutDuplicates, n=1), 
                  max = tail(yearsWithoutDuplicates, n=1), 
                  step = 1,
                  value = c(head(yearsWithoutDuplicates, n=1), tail(yearsWithoutDuplicates, n=1))),
      
      hr(),
      
      plotOutput("mainPlot")
      
    ),
    
    
    
    mainPanel(
      shinydashboard::box(
        width = 800,
        leaflet::leafletOutput( outputId = "myMap", height = 850 )
      )
    )
    
  )
  
)