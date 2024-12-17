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
library(stacks)



# Load in Data
data <- vroom("train.csv") |> 
  select(-casual, -registered) |> 
  mutate(datetime=as.POSIXct(datetime, format="%m/%d/%Y %H:%M")) |>
  mutate(count = log(count))
testdata <- vroom("test.csv") |> 
  mutate(datetime=as.POSIXct(datetime, format="%m/%d/%Y %H:%M")) 



round_category <- function(value, categories) {
  differences <- abs(categories - value)
  nearest_index <- which.min(differences)
  return(categories[nearest_index])
}



windspeed_values <- unique(data$windspeed)
temp_values <- unique(data$temp)
humidity_values <- unique(data$humidity)
atemp_values <- unique(data$atemp)



testdata$windspeed <- sapply(testdata$windspeed, round_category, categories = windspeed_values)
testdata$temp <- sapply(testdata$temp, round_category, categories = temp_values)
testdata$humidity <- sapply(testdata$humidity, round_category, categories = humidity_values)
testdata$atemp <- sapply(testdata$atemp, round_category, categories = atemp_values)







# Recipe
my_recipe <- recipe(count ~ ., data=data) |>
  step_mutate(season=as.factor(season)) |>
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |>
  step_mutate(weather=as.factor(weather)) |>
  step_date(datetime, features="dow") |>
  step_time(datetime, features=c("hour")) |>
  step_mutate(datetime_hour=as.factor(datetime_hour)) |>
  step_rm(atemp, datetime) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())







## Split data for CV
folds <- vfold_cv(data, v = 10, repeats=1)

## Create a control grid
untunedModel <- control_stack_grid() #If tuning over a grid
tunedModel <- control_stack_resamples() #If not tuning a model







## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(preg_model)

## Grid of values to tune over
preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 7) ## L^2 total tuning possibilities

## Run the CV
preg_models <- preg_wf %>%
tune_grid(resamples=folds,
          grid=preg_tuning_grid,
          metrics=metric_set(rmse, mae, rsq),
          control = untunedModel) # including the control grid in the tuning ensures you can
# call on it later in the stacked model






## Create other resampling objects with different ML algorithms to include in a stacked model, for ex
lin_reg <-
  linear_reg() %>%
  set_engine("lm")
lin_reg_wf <-
  workflow() %>%
  add_model(lin_reg) %>%
  add_recipe(my_recipe)
lin_reg_model <-
  fit_resamples(
              lin_reg_wf,
              resamples = folds,
              metrics=metric_set(rmse, mae, rsq),
              control = tunedModel
)








## Random Forest Model
my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=500 ) %>%
  set_engine("ranger") %>% 
  set_mode("regression")
wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

grid_of_tuning_params <- grid_regular(mtry(range= c(1,10)),
                                      min_n(),
                                      levels = 5) 

grid_of_tuning_params

## Split data for CV
folds <- vfold_cv(data, v = 10, repeats=1)

## Run CV
CV_results <- wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params, 
            metrics=metric_set(rmse))


## Get Best tuning parameters
bestTune <- CV_results %>%
  select_best(metric="rmse")

Rando_Forest_wf <-
  wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=data)
Rando_Forest_models <- Rando_Forest_wf %>%
  tune_grid(resamples=folds,
            grid=preg_tuning_grid,
            metrics=metric_set(rmse, mae, rsq),
            control = untunedModel) 









## K Neighbor
k_neighbor <- nearest_neighbor(
  mode = "regression",
  engine = "kknn",
  neighbors = 11,
  weight_func = "gaussian",
  dist_power = NULL
)
k_neighbor_wf <-
  workflow() %>%
  add_model(k_neighbor) %>%
  add_recipe(my_recipe)
k_neighbor_model <-
  fit_resamples(
    k_neighbor_wf,
    resamples = folds,
    metrics=metric_set(rmse, mae, rsq),
    control = tunedModel
  )








## Bart
bart <- parsnip::bart(
  mode = "regression",
  engine = "dbarts",
  trees = 100,
  prior_terminal_node_coef = .9,
  prior_terminal_node_expo = NULL,
  prior_outcome_range = NULL
)
bart_wf <-
  workflow() %>%
  add_model(bart) %>%
  add_recipe(my_recipe)
bart_model <-
  fit_resamples(
    bart_wf,
    resamples = folds,
    metrics=metric_set(rmse, mae, rsq),
    control = tunedModel
  )






## Specify with models to include
my_stack <- stacks() %>%
#add_candidates(preg_models) %>%
add_candidates(k_neighbor_model) %>%
add_candidates(Rando_Forest_models) 








## Fit the stacked model
stack_mod <- my_stack %>%
blend_predictions() %>% # LASSO penalized regression meta-learner
  fit_members() ## Fit the members to the dataset






## If you want to build your own metalearner you'll have to do so manually
## using
stackData <- as_tibble(my_stack)

## Use the stacked data to get a prediction
stack_mod %>% predict(new_data=testdata)
















kaggle_submission <- bike_predictions %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./Forests2_Preds.csv", delim=",")

