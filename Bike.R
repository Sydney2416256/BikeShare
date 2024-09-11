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
testdata <- vroom("STAT348/test.csv")

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

