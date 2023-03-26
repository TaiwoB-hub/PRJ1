library(rvest)
library(httr)
url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
# Get the root HTML node by calling the `read_html()` method with URL
my_title<- list(title="Bike sharing systems.html")
http_response<- GET("https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems", my_title = title)
print(http_response)
root_node <- read_html(http_response)
Table_node <- html_node(root_node, "table")
Table_data_frame<- html_table(Table_node, fill = TRUE)
View(Table_data_frame)
summary(Table_data_frame)
#To export or save in a csv format
df <- data.frame(Table_data_frame)
write.csv(df,"raw_bike_sharing_systems.csv", row.names = TRUE)
#Open API to download weather dataset
get_weather_forecaset_by_cities <- function(city_names){
  weather_data_frame <- data.frame()
  for (city_name in city_names){
    # Get forecast data for a given city list
    # Forecast API URL
    forecast_url <- 'https://api.openweathermap.org/data/2.5/forecast?'
    # Create query parameters
    forecast_query <- list(q = city_name, appid = "3cd789ba709b66a49e14590500ae02df", units="metric")
    result <- GET(forecast_url, query=forecast_query)
    # Note that the 5-day forecast JSON result is a list of lists. You can print the reponse to check the results
    json_result <- content(result, as="parsed")
    results<- json_result$list
    #loop the json result 
    for (result in results) {
      city <- c(city, city_name)
      weather <- c(weather,result$weather[[1]]$main)
      visibility <- c(visibility,result$main$visibility)
      temp <- c(temp,result$main$temp)
      temp_min <- c(temp_min,result$main$temp_min)
      temp_max <- c(temp_min,result$main$temp_max)
      pressure <- c(pressure,result$main$pressure)
      humidity <- c(humidity,result$main$humidity)
      wind_speed <- c(wind_speed,result$wind$speed)
      wind_deg <- c(wind_deg,result$wind$deg)
      forecast_datetime <- c(forecast_datetime,result$dt_txt)
      season <- c(season, "spring")
    }
    # Combine all vectors
    weather_data_frame <- data.frame(city=city,
                                     weather=weather,
                                     visibility=visibility,
                                     temp=temp,
                                     temp_min=temp_min,
                                     temp_max=temp_max,
                                     pressure=pressure,
                                     humidity=humidity,
                                     wind_speed=wind_speed,
                                     wind_deg=wind_deg,
                                     forecast_datetime=forecast_datetime,
                                     season=season)
  }
  # Return a data frame
  return(weather_data_frame)
}
cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- get_weather_forecaset_by_cities(cities)
write.csv(cities_weather_df, "cities_weather_forecast.csv", row.names=FALSE)
