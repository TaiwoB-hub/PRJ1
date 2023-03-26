setwd('C:/Users/USER/R(wd)')
library(rlang)
library(tidymodels)
library(tidyverse)
library(stringr)
seoul_bk_sharing_cn<-read_csv('seoul_bike_sharing_converted_normalized.csv')
view(seoul_bk_sharing_cn)
#Dataset from the url
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
bike_sharing_df
#We won't be using the DATE column, because 'as is', it basically acts like an data entry index.
#Removing DATE column and FUNCTIONAL_DAY
bike_sharing_df<-bike_sharing_df%>%
  select(-DATE,-FUNCTIONING_DAY)
#Split training and testing data
set.seed(1234)
bike_sharing_split<-initial_split(bike_sharing_df, prop = 0.75)
train_data<-training(bike_sharing_split)
test_data <-testing(bike_sharing_split)
train_data
test_data
# Build a linear regression model using weather variables only
lm_spec<-linear_reg()%>%
  set_engine(engine = 'lm')
lm_spec
lm_model_weather<- lm_spec%>%
  fit(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + RAINFALL + SNOWFALL ,data = train_data)
print(lm_model_weather$fit)
train_result<- lm_model_weather%>%
  predict(new_data = train_data)%>%
  mutate(truth = train_data$RENTED_BIKE_COUNT)
print(train_result)
test_result<- lm_model_weather%>%
  predict(new_data = test_data)%>%
  mutate(truth= test_data$RENTED_BIKE_COUNT)
print(test_result)
#Build a linear regression model using all variables including the DATE and FUNCTIONING_DAY
#we have to drop the functioning day due to its having a single value and R wont fit a multiple reg model with that
bike_sharing_df2<-read.csv('seoul_bike_sharing_converted_normalized2.csv')
bike_sharing_df2<-bike_sharing_df2%>%
  select(-DATE,-FUNCTIONING_DAY)
bike_sharing_df2
set.seed(1234)
bike_sharing_split2<-initial_split(bike_sharing_df2, prop = 0.75)
train_data2<-training(bike_sharing_split2)
test_data2 <-testing(bike_sharing_split2)
train_data2
test_data2
lm_model_all2<- lm_spec%>%
  fit(RENTED_BIKE_COUNT ~ . , data = train_data2)
print(lm_model_all$fit)
train_result2<-lm_model_all2%>%
  predict(new_data = train_data2)%>%
  mutate(truth = train_data2$RENTED_BIKE_COUNT)
print(train_result2)
test_result2<- lm_model_weather%>%
  predict(new_data = test_data2)%>%
  mutate(truth= test_data2$RENTED_BIKE_COUNT)
print(test_result2)
#TASK: Model evaluation and identification of important variables
#Rsq and R.M.S.E
rmse_all<-rmse(test_result2, truth = truth, estimate = .pred)
rsq_all<-rsq(test_result2, truth = truth, estimate = .pred)
rmse_all
rsq_all
rmse_weather<-rmse(test_result, truth = truth, estimate = .pred)
rsq_weather<-rsq(test_result, truth = truth, estimate = .pred)
rmse_weather
rsq_weather
#view coefficient
lm_model_all2$fit$coefficients
#sort into descending order
sorted_coeffs <- sort(lm_model_all2$fit$coefficients, decreasing = TRUE)
view(sorted_coeffs)
#Visualize the list using ggplot and geom_bar

coeffs_df <- data.frame(
  coefficient = names(sorted_coeffs),
  value = sorted_coeffs
)

# Create the plot using ggplot and geom_bar
ggplot(coeffs_df, aes(x = coefficient, y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  xlab("Coefficient") +
  ylab("Value") +
  ggtitle("Sorted Coefficients from lm Model")

# 4B
library("tidymodels")
library("tidyverse")
library("stringr")

dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)
#Remove DATE and FUNCTIONING COLUMNS
bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)
#Define a linear regression model specification.
lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")
#Split the data into training and testing datasets.
set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)
#Add polynomial terms
#Linear regression models are the most suitable models to capture the linear correlations among variables. 
#However, in real world data, many relationships may be non-linear.
#For example, the correlation between RENTED_BIKE_COUNT and TEMPERATURE does not look like linear:
ggplot(data = train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() 
# Plot the higher order polynomial fits
ggplot(data=train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")
# Fit a linear model with higher order polynomial on some important variables 
lm_poly <- lm(RENTED_BIKE_COUNT ~ poly(TEMPERATURE,6) + poly(HUMIDITY, 4) + poly(WIND_SPEED,2) + poly(VISIBILITY,2) + poly(DEW_POINT_TEMPERATURE, 2)+
  poly(SOLAR_RADIATION,6) + poly(RAINFALL,4) + poly(SNOWFALL,2) , data = train_data)
summary(lm_poly$fitted.values)
#Make predictions on test dataset using the lm_poly models
lm_poly_pred<-predict(lm_poly, newdata = test_data)
head(lm_poly_pred)
#Another minor improvement we could do here is to convert all negative prediction results to zero, because we can not have negative rented bike counts
lm_poly_pred[lm_poly_pred<0] <- 0
#Lets now calculate R-squared and RMSE for the test results generated by `lm_ploy` model
lm_poly_pred<- as.data.frame(lm_poly_pred)
head(lm_poly_pred)
lm_poly_pred_rsq <- rsq(lm_poly_pred, truth = truth, estimate = .pred)
lm_poly_pred_rsme <-rmse(lm_poly_pred, truth = truth, estimate = .pred)
#Add interaction terms
model <- lm(RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY, data = train_data)
summary(model)
# R-squared and RMSE for the new model to see if performance has improved

