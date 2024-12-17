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
library(dbarts)



# Load in Data
data1 <- vroom("STAT348/train.csv") |> 
  select(-count, -registered) |> 
  mutate(datetime=as.POSIXct(datetime, format="%m/%d/%Y %H:%M")) |>
  mutate(casual = log(casual+0.01))
testdata1 <- vroom("STAT348/test.csv") |> 
  mutate(datetime=as.POSIXct(datetime, format="%m/%d/%Y %H:%M")) 




# Recipe
my_recipe <- recipe(casual ~ ., data=data1) |>
  step_mutate(season=as.factor(season)) |>
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |>
  step_mutate(weather=as.factor(weather)) |>
  step_date(datetime, features="dow") |>
  step_time(datetime, features=c("hour")) |>
  step_date(datetime, features="year") |> 
  step_mutate(datetime_hour=as.factor(datetime_hour)) |>
  step_interact(terms = ~ datetime_hour:workingday) |> 
  step_rm(atemp, datetime) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())






bart_mod <- parsnip::bart(
  mode = "regression",
  engine = "dbarts",
  trees = 100,
  prior_terminal_node_coef = .95,
  prior_terminal_node_expo = NULL,
  prior_outcome_range = NULL
)


wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(bart_mod)|> 
  fit(data=data1)




bike_pred1 <- predict(wf, new_data = testdata1)
bike_predictions1 <- exp(bike_pred1)-.01











# Load in Data
data2 <- vroom("STAT348/train.csv") |> 
  select(-casual, -count) |> 
  mutate(datetime=as.POSIXct(datetime, format="%m/%d/%Y %H:%M")) |>
  mutate(registered = log(registered+.01))
testdata2 <- vroom("STAT348/test.csv") |> 
  mutate(datetime=as.POSIXct(datetime, format="%m/%d/%Y %H:%M")) 




# Recipe
my_recipe <- recipe(registered ~ ., data=data2) |>
  step_mutate(season=as.factor(season)) |>
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |>
  step_mutate(weather=as.factor(weather)) |>
  step_date(datetime, features="dow") |>
  step_time(datetime, features=c("hour")) |>
  step_date(datetime, features="year") |> 
  step_mutate(datetime_hour=as.factor(datetime_hour)) |>
  step_interact(terms = ~ datetime_hour:workingday) |> 
  step_rm(atemp, datetime) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())






bart_mod <- parsnip::bart(
  mode = "regression",
  engine = "dbarts",
  trees = 100,
  prior_terminal_node_coef = .85,
  prior_terminal_node_expo = NULL,
  prior_outcome_range = NULL
)


wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(bart_mod)|> 
  fit(data=data2)




bike_pred2 <- predict(wf, new_data = testdata2)
bike_predictions2 <- exp(bike_pred2)-.01 








bike_predictions <- cbind(bike_predictions1, bike_predictions2) 
names(bike_predictions)[1]<-paste("casual")
names(bike_predictions)[2]<-paste("registered")
bike_predictions <- bike_predictions |> 
  mutate(pred = casual + registered) 













kaggle_submission <- bike_predictions %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, pred) %>% #Just keep datetime and prediction variables
  rename(count=pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./Bart6.csv", delim=",")


