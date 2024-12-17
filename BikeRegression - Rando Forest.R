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








rf_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 500) %>%
  set_engine("ranger") %>% 
  set_mode("regression")


grid_of_tuning_params <- grid_regular(
  mtry(range = c(1, 10)), 
  min_n(range = c(1, 10)),  
  levels = 5
)
                                      
grid_of_tuning_params

rf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_mod)

## Split data for CV
folds <- vfold_cv(data, v = 10, repeats=1)

## Run CV
CV_results <- rf_wf %>%
  tune_grid(
    resamples = folds,
    grid = grid_of_tuning_params, 
    metrics = metric_set(rmse)
  )


## Get Best tuning parameters
bestTune <- CV_results %>%
  select_best(metric="rmse")

final_wf <-
  rf_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=data)

## Predict
bike_pred <- predict(final_wf, new_data=testdata)
bike_predictions <- exp(bike_pred)



kaggle_submission <- bike_predictions %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./Forests2_Preds.csv", delim=",")

