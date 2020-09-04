library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet.extras)

# Read data from rds-file
data <- readRDS("./../data/wfp_data.RDS")
# list countries without duplicates
countrysWithoutDuplicates <- unique(data$country)

productsWithoutDuplicates <- unique(data$product)

yearsWithoutDuplicates <- strtoi(substr(sort(unique(data$year)), 1, 4))

# define user interface
ui <- navbarPage(title = h6("Leckerschmecker"),
  
  tabPanel(h6("DataViewer"),
  
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
      
      shinydashboard::box(
        title = h4("Select countries"),
        width = 600,
        leaflet::leafletOutput( outputId = "myMap", height = 600 )
      )
      
    ), 
    
    mainPanel(
      h4("Durchschnittliche Preisentwicklung über die Länder für das augewählte Produkt"),
      plotOutput("averageFoodPriceDevelopmentPlot"),
      
      h4("Preis pro Land für das ausgewählte Produkt"),
      plotOutput("pricePerCountryPlot"),
      
      h4("Häufigkeit der Produkte pro Saleschannel"),
      plotOutput("frequencyOfProductsPerSalesChannelPlot")
      
    )
    
  )
  
  ),
  
  tabPanel(h6("DataPresentator"),
    titlePanel("World Food Prices in Developing Countries"),
    h5(em("created by: Tanja Bühr, Nicklas Körtge, Robert Bilger")),
           
    mainPanel(
      img(src = "/Users/nkoertge/Desktop/count_country_product.png", height = 700, width = 1000), align="left",
             
    ),
  
  ),
  
  
  tabPanel(h6("DataForcaster"),
    titlePanel("World Food Prices in Developing Countries"),
    h5(em("created by: Tanja Bühr, Nicklas Körtge, Robert Bilger")),
           
    sidebarLayout(
      sidebarPanel(
        # define selector for countries
        #selectInput("select", label = h4("Select Country"), 
        #            choices = c(list("All countrys" = 1), countrysWithoutDuplicates),
        #           selected = 1),
        #
        #hr(),
        
        selectInput("selectProductsForForcast", label = h4("Select product"), 
                    choices = productsWithoutDuplicates, 
                    selected = 1),
        hr(),
        
        # make a group of checkboxes for years
        selectInput("selectCountryForForcast", label = h4("Select country"), 
                    choices = countrysWithoutDuplicates, 
                    selected = 1),
        hr(),
        
        
        selectInput("selectForecastModelForForcast", label = h4("Select Forecast Model"), 
                    choices = list("Linieare Regression" = 1, "Neuronales Netz" = 2, "[Robert]" = 3), 
                    selected = 1),
        hr()
        
      ), 
      
      mainPanel(
        h4("Vorhersage der Preisentwicklung für das asugewählte Produkt im Land"),
        plotOutput("forcastPlot"),
        
      )
      
    )      
           
  )
  
)