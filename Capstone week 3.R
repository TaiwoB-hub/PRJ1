setwd('C:/Users/USER/R(wd)')
library(tidyverse)
library(RODBC)
library(DBI)
conn_path<- paste('BOLUDSN')
conn<- odbcConnect(conn_path)
sql.info<- sqlTypeInfo(conn)
names(sql.info)
conn.info<-odbcGetInfo(conn)
names(conn.info)
# to drop the two tables should in case they exist before creation
myboludsn2 <- "<BOLUWATIFE>"
tables <- c("WORLD_CITIES", "BIKE_SHARING_SYSTEMS", "CITIES_WEATHER_FORECAST", "SEOUL_BIKE_SHARING")
for (table in tables){  
  # Drop School table if it already exists
  out <- sqlTables(conn, tableType = "TABLE", schema = myboludsn2, tableName =table)
  if (nrow(out)>0) {
    err <- sqlDrop (conn, paste(myboludsn2,".",table,sep=""), errors=FALSE)  
    if (err==-1){
      cat("An error has occurred.\n")
      err.msg <- odbcGetErrMsg(conn)
      for (error in err.msg) {
        cat(error,"\n")
      }
    } else {
      cat ("Table: ",  myboludsn2,".",tables," was dropped\n")
    }
  } else {
    cat ("Table: ",  myboludsn2,".",tables," does not exist\n")
  }
}

# Create query for world_cities

world_cities<-sqlQuery(conn, "CREATE TABLE WORLD_CITIES(
                                                      CITY VARCHAR(50),
	                                                    CITY_ASCII VARCHAR(50),
	                                                    LAT DECIMAL(20,2),
	                                                    LNG DECIMAL(20,2),
	                                                    COUNTRY	VARCHAR(50),
	                                                    ISO2 VARCHAR(5),
	                                                    ISO3 VARCHAR(5),
	                                                    ADMIN_NAME VARCHAR(100),	
	                                                    CAPITAL	VARCHAR(50),
	                                                    POPULATION BIGINT,
	                                                    ID BIGINT NOT NULL)", errors = FALSE)


#To check if table was sucessfully created
if(world_cities==-1){
  cat("An error has occured.\n")
  msg <- odbcGetErrMsg(conn)
  print(msg)
}else{
  cat("Table was created successfully.\n")
}


# Create query for bike_sharing

Bike_sharing_systems<-sqlQuery(conn,"CREATE TABLE BIKE_SHARING_SYSTEMS (
  COUNTRY	VARCHAR(20),
  CITY VARCHAR(87),
  SYSTEM VARCHAR(40),
  BICYCLES VARCHAR(5))", errors=FALSE)

#To check if table was sucessfully created
if(Bike_sharing_systems==-1){
  cat("An error has occured.\n")
  msg <- odbcGetErrMsg(conn)
  print(msg)
}else{
  cat("Table was created successfully.\n")
}

# Create query for cities_weather_forecast

Cities_weather_forecast<-sqlQuery(conn, "CREATE TABLE CITIES_WEATHER_FORECAST(
  CITY VARCHAR(16),
  WEATHER VARCHAR(6),
  VISIBILITY SMALLINT,
  TEMP DECIMAL(6,2),
  TEMP_MIN DECIMAL(6,2),
  TEMP_MAX DECIMAL(6,2),
  PRESSURE SMALLINT,
  HUMIDITY SMALLINT,
  WIND_SPEED DECIMAL(6,2),
  WIND_DEG SMALLINT,
  SEASON VARCHAR(6),
  FORECAST_DATETIME TIMESTAMP
  PRIMARY KEY (CITY))", errors=FALSE
)

#To check if table was sucessfully created
if(Cities_weather_forecast==-1){
  cat("An error has occured.\n")
  msg <- odbcGetErrMsg(conn)
  print(msg)
}else{
  cat("Table was created successfully.\n")
}

# Create query for seoul_bike_sharing

Seoul_bike_sharing<-sqlQuery(conn, "CREATE TABLE SEOUL_BIKE_SHARING (
  DATE VARCHAR(30),
  RENTED_BIKE_COUNT SMALLINT,
  HOUR SMALLINT,
  TEMPERATURE DECIMAL(4,1),
  HUMIDITY SMALLINT,
  WIND_SPEED DECIMAL(3,1),
  VISIBILITY SMALLINT,
  DEW_POINT_TEMPERATURE DECIMAL(4,1),
  SOLAR_RADIATION DECIMAL(5,2),
  RAINFALL DECIMAL(3,1),
  SNOWFALL DECIMAL(3,1),
  SEASONS	VARCHAR(10),
  HOLIDAY	VARCHAR(20),
  FUNCTIONING_DAY VARCHAR(5))", errors= FALSE)

#To check if table was sucessfully created
if(Seoul_bike_sharing==-1){
  cat("An error has occured.\n")
  msg <- odbcGetErrMsg(conn)
  print(msg)
}else{
  cat("Table was created successfully.\n")
}
#To load the datasets into the created table
#world_cities
#i had to remove the first column before appending because the header refuses to be removed despite the command
world_citiesdf<- read.csv("world_cities.csv", header = FALSE)
world_citiesdf1<- world_citiesdf[-1,] 
world_citiesdf<-world_citiesdf1
sqlSave(conn, world_citiesdf,"world_cities",append = TRUE,fast =FALSE , colnames =FALSE , rownames = FALSE, verbose =TRUE)
#Bike_sharing_systems
Bike_sharing_systemsdf<-read.csv("bike_sharing_systems.csv", header = FALSE)
head(Bike_sharing_systemsdf)
#remove first row
Bike_sharing_systemsdf1<- Bike_sharing_systemsdf[-1,]
Bike_sharing_systemsdf<- Bike_sharing_systemsdf1
sqlSave(conn, Bike_sharing_systemsdf,"bike_sharing_systems",append = TRUE,fast =FALSE , colnames =FALSE , rownames = FALSE, verbose =FALSE)
#Cities_weather_forecast
Cities_weather_forecastdf<- read.csv("cities_weather_forecast.csv", header = TRUE)
head(Cities_weather_forecastdf)
#after several hours of refusing to append, i ended up changing header to TRUE and adding safer to FALSE which gave me a breakthrough
sqlSave(conn, Cities_weather_forecastdf,"cities_weather_forecast",append = TRUE,fast =FALSE , colnames =FALSE , rownames = FALSE, verbose =FALSE,safer = FALSE)
#Seoul_bike_sharing
Seoul_bike_sharingdf<-read.csv("seoul_bike_sharing.csv", header = FALSE)
Seoul_bike_sharingdf1<-Seoul_bike_sharingdf[-1,]
sqlSave(conn, Seoul_bike_sharingdf1,"seoul_bike_sharing",append = TRUE,fast =FALSE , colnames =FALSE , rownames = FALSE, verbose =FALSE)
#To fetch data in the database server
world_cities<-sqlFetch(conn, "world_cities")
head(world_cities)
bike_sharing_system<-sqlFetch(conn, "bike_sharing_systems")
head(bike_sharing_system)
Cities_weather_forecast<-sqlFetch(conn, "cities_weather_forecast")
head(Cities_weather_forecast)
seoul_bike_sharing<-sqlFetch(conn, "seoul_bike_sharing")
head(seoul_bike_sharing)
#TASK B
#1 Determine how many records are in the seoul_bike_sharing dataset.
sqlQuery(conn, "SELECT COUNT(*) FROM SEOUL_BIKE_SHARING")
#2 Determine how many hours had non-zero rented bike count.
sqlQuery(conn, "SELECT COUNT(HOUR) FROM SEOUL_BIKE_SHARING
         WHERE RENTED_BIKE_COUNT > 0")
#3 Query the the weather forecast for Seoul over the next 3 hours
sqlQuery(conn, "SELECT TOP 1* FROM CITIES_WEATHER_FORECAST
         WHERE CITY = 'Seoul' ")
#4Find which seasons are included in the seoul bike sharing dataset.
sqlQuery(conn, "SELECT DISTINCT SEASONS FROM SEOUL_BIKE_SHARING")
#5 Find the first and last dates in the Seoul Bike Sharing dataset.
sqlQuery(conn, "SELECT MAX(DATE) AS FIRST_DATE, MIN(DATE) AS LAST_DATE
         FROM SEOUL_BIKE_SHARING")
#6 Determine which date and hour had the most bike rentals
sqlQuery(conn, "SELECT DATE, HOUR 
         FROM SEOUL_BIKE_SHARING
         WHERE RENTED_BIKE_COUNT = (SELECT MAX(RENTED_BIKE_COUNT) 
         FROM SEOUL_BIKE_SHARING)")
#7Hourly popularity and temperature by season
# Determine the average hourly temperature and the average number of bike rentals per hour over each season. List the top ten results by average bike count.
sqlQuery(conn, "SELECT TOP 10  HOUR, SEASONS ,AVG(TEMPERATURE) AS AVG_HOURLY_TEMPERATURE, AVG(RENTED_BIKE_COUNT) AS AVERAGE_BIKE_COUNT
            FROM SEOUL_BIKE_SHARING
            GROUP BY SEASONS, HOUR
            ORDER BY AVERAGE_BIKE_COUNT")
#8Rental Seasonality
#Find the average hourly bike count during each season.
#Also include the minimum, maximum, and standard deviation of the hourly bike count for each season.
sqlQuery(conn, "SELECT DISTINCT SEASONS AS SEASONS,HOUR,
                                                AVG(RENTED_BIKE_COUNT) AS AVG_HOURLY_BIKE,
                                                MIN(RENTED_BIKE_COUNT) AS MIN_HOURLY_BIKE,
                                                MAX(RENTED_BIKE_COUNT) AS MAX_HOURLY_BIKE,
                                                STDEV(RENTED_BIKE_COUNT) AS SD_HOURLY_BIKE
                                                FROM SEOUL_BIKE_SHARING
                                                GROUP BY SEASONS, HOUR
                                                ORDER BY SEASONS, HOUR")
#9Weather Seasonality
#Consider the weather over each season. On average, what were the TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, DEW_POINT_TEMPERATURE, SOLAR_RADIATION, RAINFALL, and SNOWFALL per season?
#Include the average bike count as well , and rank the results by average bike count so you can see if it is correlated with the weather at all.

sqlQuery(conn, "SELECT DISTINCT SEASONS, AVG(TEMPERATURE) AS TEMPERATURE, 
                                          AVG(HUMIDITY) AS HUMIDITY,
                                          AVG(WIND_SPEED) AS WIND_SPEED,
                                          AVG(VISIBILITY) AS VISIBILITY,
                                          AVG(DEW_POINT_TEMPERATURE) AS DEW_POINT_TEMPERATURE,
                                          AVG(SOLAR_RADIATION) AS SOLAR_RADIATION,
                                          AVG(RAINFALL) AS RAINFALL,
                                          AVG(SNOWFALL) AS SNOWFALL,
                                          AVG(RENTED_BIKE_COUNT) AS AVERAGE_BIKE_COUNT
                                                                                           FROM SEOUL_BIKE_SHARING
                                                                                           GROUP BY SEASONS
                                                                                           ORDER BY SEASONS, AVERAGE_BIKE_COUNT")
#10 Total Bike Count and City Info for Seoul
# Use an implicit join across the WORLD_CITIES and the BIKE_SHARING_SYSTEMS tables to determine the total number of bikes avaialble in Seoul, plus the following city information about Seoul: CITY, COUNTRY, LAT, LON, POPULATION, in a single view.
#Notice that in this case, the CITY column will work for the WORLD_CITIES table, but in general you would have to use the CITY_ASCII column
sqlQuery(conn,"SELECT WC.CITY_ASCII, WC.LAT, WC.LNG,WC.COUNTRY, WC.POPULATION,BS.BICYCLES FROM WORLD_CITIES WC,BIKE_SHARING_SYSTEMS BS
         WHERE WC.CITY_ASCII=BS.CITY AND WC.CITY_ASCII='SEOUL'")
#11Find all city names and coordinates with comparable bike scale to Seoul's bike sharing system
#Find all cities with total bike counts between 15000 and 20000. Return the city and country names, plus the coordinates (LAT, LNG), population, and number of bicycles for each city.
sqlQuery(conn,"SELECT DISTINCT WC.CITY_ASCII AS CITY_NAME, WC.LAT, WC.LNG,WC.COUNTRY, WC.POPULATION,BS.BICYCLES FROM WORLD_CITIES WC,BIKE_SHARING_SYSTEMS BS
         WHERE WC.CITY_ASCII=BS.CITY AND BS.BICYCLES BETWEEN 1500 AND 2000")
#TASK C 
#Exploratory Data Analysis with tidyverse and ggplot2
library(tidyverse)
#1 read the dataset and check the datatyoes
seoul_bike_sharing<- read_csv("seoul_bike_sharing.csv", col_names = TRUE)
sapply(seoul_bike_sharing, typeof)
head(seoul_bike_sharing)
#2 convert the date column from chr to date
seoul_bike_sharingdf<-transform(seoul_bike_sharing, DATE = as.Date(DATE))
class(seoul_bike_sharing$DATE)
#or use the timechange library for any date format:dmy,ymd,mdy which is the better method
library(timechange)
library(lubridate)
seoul_bike_sharing$DATE<-dmy(seoul_bike_sharing$DATE)
#3 cast hour as categorical variable
seoul_bike_sharing$HOUR<- factor(seoul_bike_sharing$HOUR)
seoul_bike_sharing
#check the structure of the dataframe
str(seoul_bike_sharing)
#Finally ensure there is no missing value
sum(is.na(seoul_bike_sharing))
#4 Data summary
summary(seoul_bike_sharing)
#5 calculate how many holidays are there the dataset
Seoul_bike_sharingdf<-seoul_bike_sharing%>%
  filter(HOLIDAY == "Holiday")
count(Seoul_bike_sharingdf)
#6 Task 6 - Calculate the percentage of records that fall on a holiday.
whole_dataset<-nrow(seoul_bike_sharing)
holiday<-nrow(Seoul_bike_sharingdf)
percentage<-holiday/whole_dataset*100
print(percentage)
#7 Task 7 - Given there is exactly a full year of data, determine how many records we expect to have
Seoul_bike_sharingdf2<-seoul_bike_sharing%>%
  filter(DATE == as.POSIXct(DATE))%>%
  summarise(year(DATE)==1)
count(Seoul_bike_sharingdf2)
#8 Given the observations for the 'FUNCTIONING_DAY' how many records must there be?
Seoul_bike_sharingdf3<-seoul_bike_sharing%>%
  filter(FUNCTIONING_DAY == "Yes")
count(Seoul_bike_sharingdf3)
#9Load the dplyr package, group the data by SEASONS, and use the summarize() function to calculate the seasonal total rainfall and snowfall.
head(seoul_bike_sharing)
seasonal_rf<- seoul_bike_sharing%>%
  group_by(SEASONS)%>%
  summarize(rain_per_season=sum(RAINFALL))
  seasonal_rf
  
seasonal_snow<-seoul_bike_sharing%>%
  group_by(SEASONS)%>%
  summarize(snow_per_season=sum(SNOWFALL))
seasonal_snow
#Task 10 - Create a scatter plot of RENTED_BIKE_COUNT vs DATE.
#Tune the opacity using the alpha parameter such that the points don't obscure each other too much.
ggplot(seoul_bike_sharing,aes(x=DATE,y=RENTED_BIKE_COUNT))+
  geom_point(alpha = 0.5)+
  geom_smooth()
#Explanation: Rented bike count spiked to over 3000 counts during the summer and autumn due reduced rainfall and snow 
#but the numbers were greatly reduced to less than 1000 during winter due to heavy snow and bad weather.
#Task 11 Create the same plot of the RENTED_BIKE_COUNT time series, but now add HOURS as the color.
ggplot(seoul_bike_sharing,aes(x=DATE,y=RENTED_BIKE_COUNT, color = HOUR))+
  geom_point(alpha = 0.5)+
  geom_smooth()+
  labs(x = "Date", y = "Rented Bike Count", title = "Scatter Plot of Rented Bike Count vs Date")
#Explanation: Rented bike count peaked in the morning around (8.00 am) which indicates when people resume work
#And increases during the closing hours when people home from work (6.00pm which is represented by 18:00)
#Task 12 Create a histogram overlaid with a kernel density curve
ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = 30) + 
  geom_density(alpha = 0.5, fill = "blue") +
  labs(x = "Rented Bike Count", y = "Density", title = "Histogram of Rented Bike Count with Kernel Density Curve")
#Explanation We can see from the histogram that most of the time there are relatively few bikes rented. Indeed, the 'mode', or most frequent amount of bikes rented, is about 250.
#Judging by the 'bumps' at about 700, 900, and 1900, and 3200 bikes, it looks like there may be other modes hiding within subgroups of the data.
#Interestingly, judging from the tail of the distribution, on rare occasions there are many more bikes rented out than usual.
#13 Use a scatter plot to visualize the correlation between RENTED_BIKE_COUNT and TEMPERATURE by SEASONS
ggplot(seoul_bike_sharing,aes(x=TEMPERATURE,y=RENTED_BIKE_COUNT, color = HOUR, alpha=0.5))+
  geom_point()+
  facet_wrap(~SEASONS)
  geom_smooth()+
  labs(x = "Temperature", y = "Rented Bike Count", title = "Scatter Plot of Rented Bike Count vs Temperature by season")
#Explanation: Bike rented number increases with increased temperature as evident by seasons.
#Visually, we can see some strong correlations as approximately linear patterns.
  
#14 Create a display of four boxplots of RENTED_BIKE_COUNT vs. HOUR grouped by SEASONS.
  ggplot(seoul_bike_sharing, aes(x=HOUR,y=RENTED_BIKE_COUNT, color= SEASONS ))+
  geom_boxplot()+
    labs(title = "Bike Rentals by Hour and Season",
         x = "Hour",
         y = "Rented Bike Count",
         fill = "Season") +
    facet_wrap(~SEASONS)
#Explanation: Although the overall scale of bike rental counts changes with the seasons, key features remain very similar.
#For example, peak demand times are the same across all seasons, at 8 am and 6 pm
#15 Group the data by DATE, and use the summarize() function to calculate the daily total rainfall and snowfall.
  seoul_bike_sharing_daily<- seoul_bike_sharing%>%
    group_by(DATE)%>%
    summarize(daily_rf= sum(RAINFALL), daily_sf= sum(SNOWFALL))
seoul_bike_sharing_daily  
#plot if you want
ggplot(seoul_bike_sharing_daily, aes(x=DATE, y=daily_sf))+
  geom_line()
ggplot(seoul_bike_sharing_daily, aes(x=DATE, y=daily_rf))+
  geom_line()
#Determine how many days had snowfall.
seoul_bike_sharing_daily%>%
  count(daily_rf>0)
#!00 days has rainfall