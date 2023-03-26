# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")

test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  cities_max_bike <- city_weather_bike_df %>% 
    group_by(CITY_ASCII) %>% 
    summarise(BIKE_PREDICTION_LEVEL = case_when(
      max(BIKE_PREDICTION) < 50 ~ "small",
      max(BIKE_PREDICTION) >= 50 & max(BIKE_PREDICTION) < 100 ~ "medium",
      max(BIKE_PREDICTION) >= 100 ~ "large"
    ),
    DETAILED_LABEL = max(DETAILED_LABEL),
    LATITUDE = mean(LAT),
    LONGITUDE = mean(LNG),
    LABEL = paste0(CITY_ASCII, max(BIKE_PREDICTION), max(DETAILED_LABEL)))
  # prediction for the city
  # Observe drop-down event
  # Then render output plots with an id defined in ui.R
  # Render the leaflet output when user selects a city from the dropdown
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
      # Render a specific city map
      selected_city <- input$city_dropdown
      selected_city_data <- cities_max_bike[cities_max_bike$CITY_ASCII == selected_city,]
      
      output$city_bike_map <- renderLeaflet({
        leaflet(selected_city_data) %>%
          addTiles() %>%
          addMarkers(popup = selected_city_data$DETAILED_LABEL)
      })
      #Add a temperature trend plot using renderPlot(...) 
      output$temp_line<- renderPlot(ggplot(city_weather_bike_df, aes(x = FORECASTDATETIME, y = TEMPERATURE, group = 1)) +
          geom_line(color = "blue") +
          geom_point(size = 0.1) +
          geom_text(aes(label = TEMPERATURE), vjust = -1)
      )
      #add a bike-sharing demand prediction trend plot
      output$bike_line<- renderPlot(ggplot(city_weather_bike_df, aes(x= FORECASTDATETIME, y= BIKE_PREDICTION))+
                                      geom_line(color="blue")+
                                      geom_point(size=0.1)+
                                      geom_text(aes(label= BIKE_PREDICTION)))
      #Add a renderText() function to create a formatted text output showing the clicked x and y value.
      #bike_date <- as.character(city_weather_bike_df$FORECASTDATETIME[which.min(abs(city_weather_bike_df$FORECASTDATETIME - input$plot_click))])
      output$bike_date_output <- renderText(paste("Selected Date: ", bike_date))
      #Add a trend plot
      output$humidity_pred_chart<- renderPlot(ggplot(city_weather_bike_df, aes(x= HUMIDITY, y= BIKE_PREDICTION))+
                                      geom_point(color="blue")+
                                      geom_smooth(method = lm,formula=y ~ poly(x, 4)))
      
    } else {
      # Render the overview map
      output$city_bike_map <- renderLeaflet({
        leaflet(cities_max_bike) %>%
          addTiles() %>%
          addCircleMarkers(radius = ~case_when(
            BIKE_PREDICTION_LEVEL == "small" ~ 6,
            BIKE_PREDICTION_LEVEL == "medium" ~ 10,
            BIKE_PREDICTION_LEVEL == "large" ~ 12
          ),
          color = ~color_levels(BIKE_PREDICTION_LEVEL),
          stroke = FALSE,
          fillOpacity = 0.8,
          popup = ~LABEL)
      })
    }
  })
})
