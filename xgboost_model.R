rm(list = ls())
#xgboost script
library(tidyverse)
library(xgboost)
library(caret)

dt <- data.table::fread('final_file2.txt') |> 
  select(date = V1,sales = V2)
# checking if the timestamp is complete

extracted_numbers <- str_extract_all(ifelse(dt$sales == "\t",0,dt$sales),
                                     "\\d+\\.?\\d*")

dt <- dt |>
  mutate(sales = as.numeric(unlist(extracted_numbers)),
         date = as.POSIXct(date,tryFormats = "%d/%m/%Y %H:%M"))

summary(dt)

# Check for missing saless
sapply(dt,function(x)sum(is.na(x)))

timestamps <- seq(as.POSIXct("2023-07-22 00:00:00"),
                  as.POSIXct("2023-09-20 23:55:00"), by = "5 min")


test_timestamp <- data.frame(date=timestamps)

if(
nrow(test_timestamp |> 
  left_join(dt,'date') |> 
  filter(is.na(sales))) == 0
)cat('you are good to go')else cat('there are missing lines')


# Extract hour, day, month, and year for seasonality analysis
dt <- dt |> 
  mutate(hour = hour(date),
         day = day(date),
         month = month(date),
         year = year(date))


###########################################3
# data prep and modeling


train.xgb <- dt[1:(nrow(dt)-288*3),]
test.xgb <- dt[(nrow(dt)-288*3):(nrow(dt)),]

train_data <- train.xgb |> 
  dplyr::select(hour, day, month, year)

target <- train.xgb$sales

xgb_grid <- expand.grid(
  nrounds = 100,
  max_depth = 3, # maximum depth of a tree
  colsample_bytree = 1, # subsample ratio of columns when constructing each tree
  eta = 0.05, # learning rate
  gamma = 0, # minimum loss reduction
  min_child_weight = 1,  # minimum sum of instance weight (hessian) needed in a child
  subsample = 1 # subsample ratio of the training instances
)

xgb_model <- caret::train(
  train_data, target,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  metric = 'MAE'
)

# Predict the training and test data
train_predictions <- predict(xgb_model, newdata=train_data)
test_predictions <- predict(xgb_model, newdata=test.xgb |> 
                              dplyr::select(hour, day, month, year))

# Compute Metrics
# Root Mean Squared Error (RMSE)
rmse_train <- sqrt(mean((train_predictions - target)^2))
rmse_test <- sqrt(mean((test_predictions - test.xgb$sales)^2))

# Mean Absolute Error (MAE)
mae_train <- mean(abs(train_predictions - target))
mae_test <- mean(abs(test_predictions - test.xgb$sales))

cat("Training RMSE:", rmse_train, "\n")
cat("Test RMSE:", rmse_test, "\n")
cat("Training MAE:", mae_train, "\n")
cat("Test MAE:", mae_test, "\n")

# Combine the data
plot_data <- rbind(
  data.frame(Time=1:length(train_predictions), Sales=c(train.xgb$sales), Type='Actual', Dataset='Train'),
  data.frame(Time=1:length(train_predictions), Sales=train_predictions, Type='Predicted', Dataset='Train'),
  data.frame(Time=(length(train_predictions)+1):(length(train_predictions)+length(test_predictions)), Sales=c(test.xgb$sales), Type='Actual', Dataset='Test'),
  data.frame(Time=(length(train_predictions)+1):(length(train_predictions)+length(test_predictions)), Sales=test_predictions, Type='Predicted', Dataset='Test')
)

# Plot
ggplot(plot_data, aes(x=Time, y=Sales, colour=Type)) +
  geom_line() +
  facet_wrap(~Dataset, scales="free_x") +
  labs(title="Actual vs Predicted Sales", x="Time", y="Sales") +
  theme_minimal()
