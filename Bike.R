# Load in Libraries
library(tidyverse)
library(vroom)
library(DataExplorer)
library(patchwork)
library(tidymodels)
library(dply)

# Load in Data
data <- vroom("STAT348/train.csv")
data1 <- data
for (i in 1:ncol(data1)) {if (data1$weather[i] == 4) {
  data1$weather[i] = 3
}
}

data2 <- data1 |> mutate(day_of_week = strftime(as.Date(datetime), format = "%A"))


data3 <- data |> 
  mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  mutate(hour_of_day = substr(as.character(datetime), nchar(datetime)-4, nchar(datetime)-3)) |> 
  mutate(season = as.character(season)) |> 
  mutate(day_of_week = strftime(as.Date(datetime), format = "%A"))








testdata <- vroom("STAT348/tesas.Date()testdata <- vroom("STAT348/tesas.Date()testdata <- vroom("STAT348/test.csv")

testdata2 <- testdata |> mutate(day_of_week = strftime(as.Date(datetime), format = "%A"))





my_recipe <- recipe(count~ . , data=data) %>% # Set model formula and dataset
  step_mutate(weather = as.character(ifelse(weather == 4, 3, weather))) %>%
  step_mutate(hour_of_day = substr(as.character(datetime), nchar(as.character(datetime))-4, nchar(as.character(datetime))-3)) %>% 
  step_mutate(season = as.character(season)) |> 
  step_mutate(day_of_week = strftime(as.Date(datetime), format = "%A")) |> 
  step_select(day_of_week, season, holiday, hour_of_day, weather, workingday, temp, humidity, windspeed, count) 
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet13
bake(prepped_recipe, new_data=testdata)












# Look at data
plot_correlation(data1)


# My four graphs
graph1 <- ggplot(data1) +
  geom_bar(aes(x = weather))
graph1

graph2 <- ggplot(data1, aes(temp, atemp)) +
  geom_point() +
  geom_smooth()
graph2  

graph3 <- ggplot(data1, aes(humidity, count)) +
  geom_point() +
  geom_smooth()
graph3

graph4 <- ggplot(data1, aes(atemp, count)) +
  geom_point() +
  geom_smooth()
graph4


# Put graphs together
(graph1 + graph2) / (graph3 + graph4)


#Linear Regression

## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() |>  #Type of model
  set_engine("lm") |>  # Engine = What R function to use
  set_mode("regression") |>  # Regression just means quantitative response
  fit(formula=count~atemp+humidity+weather+holiday, data=data1)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
                            new_data=testdata) # Use fit to predict
bike_predictions ## Look at the output


## Make submission 
kaggle_submission <- bike_predictions %>%
bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")



# Poisson Regression
## Set up model
my_pois_model <- poisson_reg() %>% #Type of model
  set_engine("glm") %>% # GLM = generalized linear model
  set_mode("regression") %>%
fit(formula=count~atemp+humidity+weather+holiday+day_of_week, data=data2)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_pois_model,
                            new_data=testdata2) # Use fit to predict
bike_predictions ## Look at the output
