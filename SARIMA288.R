#https://stats.stackexchange.com/questions/579846/how-to-forecast-a-time-series-which-is-generated-by-accumulating-data-of-every-f

library(tidyverse)
library(forecast)
library(TTR)

dt <- data.table::fread('initial_file.txt') |> 
  select(date = V1,sales = V2)

extracted_numbers <- str_extract_all(ifelse(dt$sales == "\t",0,dt$sales),
                                     "\\d+\\.?\\d*")

dt <- dt |>
  mutate(sales = as.numeric(unlist(extracted_numbers)),
         date = as.POSIXct(date,tryFormats = "%d/%m/%Y %H:%M"))

train <- dt |> 
  filter(date < as.Date('2023/09/18'))

test <-  dt |> 
  filter(date >= as.Date('2023/09/18'))


# Set timestamp as the time index
sales_ts <- ts(train$sales, frequency = 288, start = c(2023, 7, 22, 0, 0))

# Fit SARIMA(0,0,0)(0,1,0)[288] model
sarima_model <- Arima(sales_ts, order = c(0,0,0), 
                      seasonal = list(order = c(0,1,0), period = 288))

# Extract residuals from the SARIMA model
residuals <- residuals(sarima_model)

# Plot residuals
plot(residuals, main = "Residuals of SARIMA Model")

# Check mean and variance of residuals
cat("Mean of Residuals:", mean(residuals), "\n")
cat("Variance of Residuals:", var(residuals), "\n")

# Ljung-Box test for autocorrelation in residuals
ljung_box_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
cat("Ljung-Box Test p-value:", ljung_box_test$p.value, "\n")


# Forecast sales for the next n steps (e.g., 288 5-minute intervals)
days <- 3
n_steps <- 288*days
sales_forecast <- forecast(sarima_model, h = n_steps)

# Extract the forecasted values
forecast_values <- as.numeric(sales_forecast$mean)

# Actual values for the forecasted period 
actual_values <- test$sales

plot_data <- data.frame(Actual = actual_values,Predicted = forecast_values)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual Values", y = "Predicted Values", 
       title = "Actual vs Predicted Values") +
  theme_minimal()


plot_data <- data.frame(Actual = actual_values, Predicted = forecast_values, 
                        Lower_CI = sales_forecast$lower[,1],
                        Upper_CI = sales_forecast$upper[,1])

# Plot using ggplot2 with lines and confidence interval
ggplot(plot_data, aes(x = seq_along(Actual))) +
  geom_line(aes(y = Actual), color = "blue", size = 1, linetype = "solid", alpha = 0.7) +
  geom_line(aes(y = Predicted), color = "green", size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "gray", alpha = 0.5) +
  labs(x = "Time", y = "Values",
       title = "Actual vs Predicted Values with 80% Confidence Interval") +
  theme_minimal()

# Mean Absolute Percentage Error (MAPE) calculation
mape <- mean(abs((actual_values - forecast_values) / actual_values),na.rm = T) * 100

# Other metrics (e.g., Mean Absolute Error (MAE), Root Mean Squared Error (RMSE))
mae <- mean(abs(actual_values - forecast_values))
rmse <- sqrt(mean((actual_values - forecast_values)^2))
