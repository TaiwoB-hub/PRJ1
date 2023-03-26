setwd('C:/Users/USER/R(wd)')
library(tidyverse)
#Write a `for` loop to iterate over the above datasets and convert their column names 
dataset_list <- c('raw_bike_sharing_systems.csv', 
                  'raw_seoul_bike_sharing.csv',
                  'raw_cities_weather_forecast.csv',
                  'raw_worldcities.csv')
for(dataset_name in dataset_list){
  #read dataset
  dataset<- read_csv(dataset_name)

  #standardize its column
  
  #convert all column
  names(dataset)<- toupper(names(dataset))

  # Replace any white space separators by underscores, using the str_replace_all function
  names(dataset)<- str_replace_all(names(dataset), " ", "_")

  # Save the dataset 
  write.csv(dataset, dataset_name, row.names=FALSE)
  
}

  


#OR i used the below method cause the first dataset removes the coulumn header after running the function

raw_bike_sharing_systems<- read_csv("raw_bike_sharing_systems.csv", col_names = TRUE)
raw_seoul_bike_sharing<- read_csv("raw_seoul_bike_sharing.csv", col_names = TRUE)
raw_cities_weather_forecast<-read_csv("raw_cities_weather_forecast.csv", col_names = TRUE)
raw_worldcities<-read_csv("raw_worldcities.csv", col_names = TRUE)
# convert coulumns to upper and replace white spaces with _
#1
names(raw_bike_sharing_systems)<-toupper(names(raw_bike_sharing_systems))
names(raw_bike_sharing_systems)<- str_replace_all(names(raw_bike_sharing_systems)," ", "_")
head(raw_bike_sharing_systems)
write.csv(raw_bike_sharing_systems, "cleaned_raw_bike_sharing_systems.csv", row.names=FALSE)
#2
names(raw_seoul_bike_sharing)<- toupper(names(raw_seoul_bike_sharing))
names(raw_seoul_bike_sharing)<- str_replace_all(names(raw_seoul_bike_sharing), " ", "_")
head(raw_seoul_bike_sharing)
write.csv(raw_seoul_bike_sharing, "cleaned_raw_seoul_bike_sharing.csv", row.names=FALSE)
#3
names(raw_cities_weather_forecast)<- toupper(names(raw_cities_weather_forecast))
raw_seoul_bike_sharing<- str_replace_all(names(raw_cities_weather_forecast), " ", "_")
head(raw_cities_weather_forecast)
write.csv(raw_cities_weather_forecast, "cleaned_raw_cities_weather_forecast.csv", row.names=FALSE)
#4
names(raw_worldcities)<- toupper(names(raw_worldcities))
names(raw_worldcities)<- str_replace_all(names(raw_worldcities), " ", "_")
head(raw_worldcities)
write.csv(raw_worldcities, "cleaned_raw_worldcities.csv", row.names=FALSE)

#Process the web-scraped bike sharing system dataset
bike_sharing_df <- read_csv("cleaned_raw_bike_sharing_systems.csv")
head(bike_sharing_df)
sub_bike_sharing_df <- bike_sharing_df %>% select(COUNTRY,
                                                          CITY,
                                                          SYSTEM,
                                                          BICYCLES)
#lets see the types of selected column
sapply(sub_bike_sharing_df, typeof)
#Let's try to find any elements in the Bicycles column containing non-numeric characters.
find_character <- function(strings) grepl("[^0-9]", strings)
sub_bike_sharing_df %>% 
  select(BICYCLES) %>% 
  filter(find_character(BICYCLES)) %>%
  slice(0:10)
#Next, let's take a look at the other columns, namely COUNTRY, CITY, and SYSTEM, to see if they contain any undesired reference links
# Define a 'reference link' character class, 
# `[A-z0-9]` means at least one character 
# `\\[` and `\\]` means the character is wrapped by [], such as for [12] or [abc]
ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)
# Check whether the COUNTRY column has any reference links
sub_bike_sharing_df %>% 
  select(COUNTRY) %>% 
  filter(find_reference_pattern(COUNTRY)) %>%
  slice(0:10)
#looks like the COUNTRY column is clean. Lets check the CITY column.

# Check whether the CITY column has any reference links
sub_bike_sharing_df %>% 
    select(CITY) %>% 
    filter(find_reference_pattern(CITY)) %>%
    slice(0:10)
#CITY column has some reference links to be removed. Next, lets check the SYSTEM column.

# Check whether the System column has any reference links
sub_bike_sharing_df %>% 
  select(SYSTEM) %>% 
  filter(find_reference_pattern(SYSTEM)) %>%
  slice(0:10)
#So the SYSTEM column also has some reference links.
#Remove undesired reference link using regular expression

remove_ref <- function(column) {
  ref_pattern <- "\\[[\\w]+\\]"
  result <- str_replace_all(column, ref_pattern, " ")
  trimws(result)
  return(result)
}
result<-sub_bike_sharing_df %>% mutate(CITY=remove_ref(CITY), SYSTEM=remove_ref(SYSTEM), BICYCLES=remove_ref(BICYCLES))
result
#check weather all the references are removed
result %>% 
  select(CITY, SYSTEM, BICYCLES) %>% 
  filter(find_reference_pattern(CITY) | find_reference_pattern(SYSTEM) | find_reference_pattern(BICYCLES))
#Extract the numeric value using regular expressions
# Extract the first number
extract_num <- function(columns){
  # Define a digital pattern
  digitals_pattern <- "[^0-9]|"
  # Find the first match using str_extract
  columns<-str_extract(columns,digitals_pattern)
  # Convert the result to numeric using the as.numeric() function
  return(as.numeric(columns))
}
# Use the mutate() function on the BICYCLES column
results<-result%>%mutate(BICYCLES=extract_num(BICYCLES))
view(results)
summary(result$BICYCLES)
#save as a new dataset
write.csv(results, "bike_sharing_systems.csv")
#PART 2 of the week 2(Data wrangling of raw_seoul_bike_sharing)
seoul_bike_sharing_df<-read.csv("cleaned_raw_seoul_bike_sharing.csv",)
view(seoul_bike_sharing_df)
summary(seoul_bike_sharing_df)
dim(seoul_bike_sharing_df)
#to check for missing value
seoul_bike_sharing_df%>%
  map(~sum(is.na(.)))
#Drop rows with missing values in the RENTED_BIKE_COUNT column
seoul_bike_sharing_df_no_na_v<-seoul_bike_sharing_df%>%
  drop_na(RENTED_BIKE_COUNT)
seoul_bike_sharing_df_no_na_v%>%
  map(~sum(is.na(.)))
#Let's first take a look at the missing values in the TEMPERATURE column.
seoul_bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))
#Impute missing values for the TEMPERATURE column using its mean value
#get the mean using summary
summary(seoul_bike_sharing_df)
temp_mean<- c(12.87)
temp_mean
seoul_bike_sharing_df%>%
  filter(SEASONs=="summer")%>%
  summarise(temp_mean = mean(TEMPERATURE, na.rm = TRUE))
#To replace the missing value with the mean
replace_seoul_temp<- seoul_bike_sharing_df_no_na_v%>%
  replace_na(list(TEMPERATURE=temp_mean))
replace_seoul_temp
write.csv(replace_seoul_temp,'seoul_bike_sharing.csv')
#Create indicator (dummy) variables for categorical variables
#NB: regression models cannot process categorical variable so needs to be converted into indicator variables
#Convert HOUR column from numeric into character first:
seoul_bike_sharing<-read.csv('seoul_bike_sharing.csv')
glimpse(seoul_bike_sharing)
seoul_bike_df2<-transform(seoul_bike_sharing, HOUR = as.character(HOUR))
glimpse(seoul_bike_df2)
# Convert SEASONS, HOLIDAY, FUNCTIONING_DAY, and HOUR columns into indicator columns.
#1 SEASONS column
seoul_bike_df3<-seoul_bike_df2%>%
  mutate(dummy=1)%>%
  spread(key = SEASONS, value = dummy, fill = 0)
#2 HOLIDAY column
seoul_bike_df4<-seoul_bike_df3%>%
  mutate(dummy=1)%>%
  spread(key = HOLIDAY, value = dummy, fill = 0)
#3 since funcioning day has only a single value("yes"). then, it should remain that way
#4 HOUR
seoul_bike_df5<-seoul_bike_df4%>%
  mutate(dummy=1)%>%
  spread(key = HOUR, value = dummy, fill = 0)
view(seoul_bike_df5)
summary(seoul_bike_df5)
#save the dataset
write_csv(seoul_bike_df5, "seoul_bike_sharing_converted.csv")
library(caret)
#Last task: Normilize data
#apply min-max norminalization to the below coulumns 
# `RENTED_BIKE_COUNT`, `TEMPERATURE`, `HUMIDITY`, `WIND_SPEED`, `VISIBILITY`, `DEW_POINT_TEMPERATURE`, `SOLAR_RADIATION`, `RAINFALL`, `SNOWFALL`
seoul_bike_sharing_converted<- read.csv("seoul_bike_sharing_converted.csv")
head(seoul_bike_sharing_converted)
#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to 3 to 10 columns in dataset
seoul_bike_sharing_converted_norm <- as.data.frame(lapply(seoul_bike_sharing_converted[3:10], min_max_norm))
head(seoul_bike_sharing_converted_norm)
#To returm/add other columns to the dataset
seoul_bike_sharing_converted_norm$DATE<- seoul_bike_sharing_converted$DATE
seoul_bike_sharing_converted_norm$SNOWFALL<-seoul_bike_sharing_converted$SNOWFALL
seoul_bike_sharing_converted_norm$FUNCTIONING_DAY <- seoul_bike_sharing_converted$FUNCTIONING_DAY
seoul_bike_sharing_converted_norm$Autumn<-seoul_bike_sharing_converted$Autumn
seoul_bike_sharing_converted_norm$Spring<-seoul_bike_sharing_converted$Spring
seoul_bike_sharing_converted_norm$Summer<-seoul_bike_sharing_converted$Summer
seoul_bike_sharing_converted_norm$Winter<-seoul_bike_sharing_converted$Winter
seoul_bike_sharing_converted_norm$Holiday<-seoul_bike_sharing_converted$Holiday
seoul_bike_sharing_converted_norm$No.Holiday<-seoul_bike_sharing_converted$No.Holiday
seoul_bike_sharing_converted_norm$X0<-seoul_bike_sharing_converted$X0
seoul_bike_sharing_converted_norm$X1<-seoul_bike_sharing_converted$X1
seoul_bike_sharing_converted_norm$X2<-seoul_bike_sharing_converted$X2
seoul_bike_sharing_converted_norm$X3<-seoul_bike_sharing_converted$X3
seoul_bike_sharing_converted_norm$X4<-seoul_bike_sharing_converted$X4
seoul_bike_sharing_converted_norm$X5<-seoul_bike_sharing_converted$X5
seoul_bike_sharing_converted_norm$X6<-seoul_bike_sharing_converted$X6
seoul_bike_sharing_converted_norm$X7<-seoul_bike_sharing_converted$X7
seoul_bike_sharing_converted_norm$X8<-seoul_bike_sharing_converted$X8
seoul_bike_sharing_converted_norm$X9<-seoul_bike_sharing_converted$X9
seoul_bike_sharing_converted_norm$X10<-seoul_bike_sharing_converted$X10
seoul_bike_sharing_converted_norm$X11-seoul_bike_sharing_converted$X11
seoul_bike_sharing_converted_norm$X12<-seoul_bike_sharing_converted$X12
seoul_bike_sharing_converted_norm$X13<-seoul_bike_sharing_converted$X13
seoul_bike_sharing_converted_norm$X14<-seoul_bike_sharing_converted$X14
seoul_bike_sharing_converted_norm$X15<-seoul_bike_sharing_converted$X15
seoul_bike_sharing_converted_norm$X16<-seoul_bike_sharing_converted$X16
seoul_bike_sharing_converted_norm$X17<-seoul_bike_sharing_converted$X17
seoul_bike_sharing_converted_norm$X18<-seoul_bike_sharing_converted$X18
seoul_bike_sharing_converted_norm$X19<-seoul_bike_sharing_converted$X19
seoul_bike_sharing_converted_norm$X20<-seoul_bike_sharing_converted$X20
seoul_bike_sharing_converted_norm$X21<-seoul_bike_sharing_converted$X21
seoul_bike_sharing_converted_norm$X22<-seoul_bike_sharing_converted$X22
seoul_bike_sharing_converted_norm$X23<-seoul_bike_sharing_converted$X23
head(seoul_bike_sharing_converted_norm)
# Save the dataset as `seoul_bike_sharing_converted_normalized.csv`
write_csv(seoul_bike_sharing_converted_norm, "seoul_bike_sharing_converted_normalized.csv")
