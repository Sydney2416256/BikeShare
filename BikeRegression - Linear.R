#
Load in Libraries
library(tidyverse)
library(vroom)
library(DataExplorer)
library(patchwork)
library(tidymodels)
library(dplyr)
library(poissonreg)


# Load in Data
data <- vroom("STAT348/train.csv") |> select(-casual, -registered) |> mutate(count = log(count))
testdata <- vroom("STAT348/test.csv")



# Recipe
my_recipe <- recipe(count ~ ., data=data) %>% 
  step_mutate(season=factor(season, levels= c(1,2,3,4))) %>% 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  step_mutate(weather=factor(weather, levels= c(1,2,3))) %>% 
  # step_date(datetime, features="dow") %>% 
  # step_time(datetime, features=c("hour", "minute")) %>% 
  step_rm(atemp) 


prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataSet13
testing <- bake(prepped_recipe, new_data=testdata)



lin_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")



bike_workflow <- workflow() |> 
  add_recipe(my_recipe) |> 
  add_model(lin_model) |> 
  fit(data=data)




bike_pred <- predict(bike_workflow, new_data = testing)
bike_predictions <- exp(bike_pred)



kaggle_submission <- bike_predictions %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./Poisson_Mutate_Preds.csv", delim=",")

