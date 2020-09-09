library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(dplyr)
library(leaflet.extras)

list.of.packages <- c("shiny", "shinydashboard", "shinythemes", "leaflet", "leaflet.extras", "dplyr", "maps", "sp",
                      "rgdal", "dplyr", "ggplot2", "broom", "ggfortify", "forecast", "ggplot2", "reshape2", "neuralnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Read data from rds-file
data <- readRDS("./../data/wfp_data.RDS")
# list countries without duplicates
countrysWithoutDuplicates <- unique(data$country)

productsWithoutDuplicates <- unique(data$product)

yearsWithoutDuplicates <- strtoi(substr(sort(unique(data$year)), 1, 4))

# define user interface
ui <- fluidPage(theme = shinytheme("flatly"), 
  
                
  tags$style(HTML("
    .tabbable > .nav > li > a {color: #FFFFFF}
  ")),
  navbarPage(title = h6("Leckerschmecker"),
             
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
      tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #2C3E50;
                                                  border-top: 1px solid #2C3E50 ;
                                                  border-bottom: 1px solid #2C3E50 ;}

                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #2C3E50 }'
        ))
      ),
      sliderInput("sliderYears", label = h4("Select years"),
                  min = head(yearsWithoutDuplicates, n=1), 
                  max = tail(yearsWithoutDuplicates, n=1), 
                  step = 1,
                  value = c(head(yearsWithoutDuplicates, n=1), tail(yearsWithoutDuplicates, n=1))),
      hr(),
      
      shinydashboard::box(
        title = h4("Select countries by click"),
        p("The selectable countries are changing for each product. Only those will present were data is available"),
        width = 600,
        leaflet::leafletOutput( outputId = "myMap", height = 600 )
      )
      
    ), 
    
    mainPanel(
      h4("Average price development for the selected product"),
      plotOutput("averageFoodPriceDevelopmentPlot"),
      hr(),
      
      h4("Price in each country for the selected product"),
      plotOutput("pricePerCountryPlot"),
      hr(),
      
      h4("Frequency for the selected product in each sales cahnnel"),
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
      
      img(src="count_country_product.png", align = "left", width='100%'),
      p("This chart shows the counts of data per product and country. Rice, Oil and Flour have the highest counts."),
      hr(),
      
      img(src="plot_price_all_year.png", align = "left", width='100%'),
      p("This plot shows the price range from 2006 to 2017. Price range until 2011 is below 200 USD. From 2012 on prices are spread until 800 USD. This also depends on the counts of data during the whole time frame (more data available from 2012 on)."),
      hr(),
      
      img(src="plot_price_sales_channel_product.png", align = "left", width='100%'),
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
                    choices = list("Linieare Regression" = 1, "Neuronales Netz" = 2, "Arima" = 3), 
                    selected = 1),
        hr()
        
      ), 
      
      mainPanel(
        h4("Forcast for price development for the selected Product in the corresponding country"),
        plotOutput("forcastPlot"),
        hr(),
        
        h4("Model Information"),
        
        tags$style(HTML("
          .tabbable > .nav > li > a {color: #2C3E50}
        ")),
        
        tabsetPanel(type = "tabs",
                    tabPanel("[LR] regression line", plotOutput("regressionsGeradeLM")),
                    tabPanel("[LR] residuals", plotOutput("residualsLM")),
                    tabPanel("[LR] fit", img(src="r_squared_all_products.png", width='100%'), hr(), img(src="r_squared_all_countries.png", width='100%')),
                    tabPanel("[NN] neuronal network", img(src="nn.png", width='100%')),
                    tabPanel("[AR] Arima", plotOutput("resultAR"))
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
  
))