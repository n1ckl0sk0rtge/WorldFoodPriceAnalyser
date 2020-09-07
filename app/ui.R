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
  
  tabPanel(h6("View Dataset"),
  
  # Application title
  titlePanel("World Food Prices in Developing Countries"),
  p(em("created by: Tanja Bühr, Nicklas Körtge, Robert Bilger")),
  hr(),
  
  # define site layout  
  sidebarLayout(
    sidebarPanel(

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
      hr(),
      
      h4("Preis pro Land für das ausgewählte Produkt"),
      plotOutput("pricePerCountryPlot"),
      hr(),
      
      h4("Häufigkeit der Produkte pro Saleschannel"),
      plotOutput("frequencyOfProductsPerSalesChannelPlot"),
      hr()
      
    )
    
  )
  
  ),
  
  tabPanel(h6("Dataset Presentation"),
    titlePanel("World Food Prices in Developing Countries"),
    p(em("created by: Tanja Bühr, Nicklas Körtge, Robert Bilger")),
    hr(),
           
    mainPanel(
      h4("Present our data"),
      p("The data set collects information on the development of food prices in developing countries. It contains around 700,000 data points with detailed facts about the country, price, time, city, market and sales channel. The following graphics show the most important information about this data set."),
      
      img(src="count_country_product.png", align = "left", width='1000px'),
      p("This chart shows the counts of data per product and country. Rice, Oil and Flour have the highest counts."),
      hr(),
      
      img(src="plot_price_all_year.png", align = "left", width='1000px'),
      p("This plot shows the price range from 2006 to 2017. Price range until 2011 is below 200 USD. From 2012 on prices are spread until 800 USD. This also depends on the counts of data during the whole time frame (more data available from 2012 on)."),
      hr(),
      
      img(src="plot_price_sales_channel_product.png", align = "left", width='1000px'),
      p("Price spread per sales channel. Retail prices (from 0 to 100 USD) are significantly higher than wholesale prices (below 50 USD).")
    )
  
  ),
  
  
  tabPanel(h6("Forcasting"),
    titlePanel("World Food Prices in Developing Countries"),
    p(em("created by: Tanja Bühr, Nicklas Körtge, Robert Bilger")),
    hr(),
          
    sidebarLayout(
      sidebarPanel(
        
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
                    choices = list("Linieare Regression" = 1, "Neuronales Netz" = 2, "RandomForest" = 3), 
                    selected = 1),
        hr()
        
      ), 
      
      mainPanel(
        h4("Vorhersage der Preisentwicklung für das asugewählte Produkt im Land"),
        plotOutput("forcastPlot"),
        hr(),
        
        h4("Model Information"),
        tabsetPanel(type = "tabs",
                    tabPanel("Regressionsgerade", plotOutput("regressionsGeradeLM")),
                    tabPanel("Residuals", plotOutput("residualsLM")),
                    tabPanel("Linear Regression Fitness",img(src="r_squared_all_products.png", width='1000px')),
                    tabPanel("Neuronales Netz", img(src="nn.png", width='1000px'))
        ),
        hr(),
        
        h4("Summary"),
        p("Overview"),
        verbatimTextOutput("summaryOfModel"),
        p("Quality"),
        verbatimTextOutput("qualityOfModel"),
        hr()
        
      )
      
    )      
           
  )
  
)