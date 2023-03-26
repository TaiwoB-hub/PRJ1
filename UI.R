# Load required libraries
require(leaflet)


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
            titlePanel("Bike-sharing demand prediction app"),
  # Create a side-bar layout
  sidebarLayout(
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # leaflet output with id = 'city_bike_map', height = 1000
      leafletOutput(outputId = "city_bike_map", height = 1000)),
    # Create a side bar to show detailed plots for a city
    sidebarPanel(selectInput(inputId="city_dropdown", label = "select a city:",
                             # select drop down list to select city
                             choices = c('All','Seoul','New York','Paris','Suzhou', 'London')),
                 plotOutput("temp_line", height="400px", width="100%"),
                 plotOutput("bike_line", click = "plot_click" ,height="400px", width="100%"),
                 verbatimTextOutput("bike_date_output"),
                 plotOutput("humidity_pred_chart", height = "400px", width = "100%")
    ))
))