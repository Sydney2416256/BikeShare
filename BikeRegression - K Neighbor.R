# Load in Libraries
library(tidyverse)
library(vroom)
library(DataExplorer)
library(patchwork)
library(tidymodels)
library(dplyr)
library(poissonreg)
library(glmnet)
library(rpart)
library(ranger)
library(kknn)



# Load in Data
data <- vroom("STAT348/train.csv") |> 
  select(-casual, -registered) |> 
  mutate(datetime=as.POSIXct(datetime, format="%m/%d/%Y %H:%M")) |>
  mutate(count = log(count))
testdata <- vroom("STAT348/test.csv") |> 
  mutate(datetime=as.POSIXct(datetime, format="%m/%d/%Y %H:%M")) 






# Recipe
my_recipe <- recipe(count ~ ., data=data) |>
  step_mutate(season=as.factor(season)) |>
  step_mutate(holiday=as.factor(holiday)) |>
  step_mutate(workingday=as.factor(workingday)) |>
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |>
  step_mutate(weather=as.factor(weather)) |>
  step_date(datetime, features="dow") |>
  step_time(datetime, features=c("hour")) |>
  step_date(datetime, features="year") |>
  step_mutate(datetime_hour=as.factor(datetime_hour)) |>
  step_interact(terms = ~ datetime_hour:workingday) |> 
  step_interact(terms = ~ holiday:temp) |> 
  step_rm(atemp, datetime) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())






k_mod <- nearest_neighbor(
  mode = "regression",
  engine = "kknn",
  neighbors = 11,
  weight_func = "gaussian",
  dist_power = NULL
)

k_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(k_mod)|> 
  fit(data=data)




bike_pred <- predict(k_wf, new_data = testdata)
bike_predictions <- exp(bike_pred)



kaggle_submission <- bike_predictions %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./K_Neighbors1.csv", delim=",")

