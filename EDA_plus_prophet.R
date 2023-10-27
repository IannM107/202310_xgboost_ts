library(tidyverse)
library(prophet)

dt <- data.table::fread('initial_file.txt') |> 
  select(date = V1,sales = V2)

table(dt$sales)

extracted_numbers <- str_extract_all(ifelse(dt$sales == "\t",0,dt$sales),
                                     "\\d+\\.?\\d*")

dt <- dt |>
  mutate(sales = as.numeric(unlist(extracted_numbers)),
         date = as.POSIXct(date,tryFormats = "%d/%m/%Y %H:%M"))

summary(dt)

# Check for missing saless
sapply(dt,function(x)sum(is.na(x)))

# any repeated date?
nrow(dt) == length(unique(dt$date))


# Time series plot
dt |>
  filter(date > as.Date('2023/07/25'),date <= as.Date('2023/07/29')) |> 
ggplot( aes(x = date, y = sales)) +
  geom_line() +
  labs(x = "date", y = "sales") +
  ggtitle("Time Series Plot")



# Extract hour, day, month, and year for seasonality analysis
dt$hour <- hour(dt$date)
dt$day <- day(dt$date)
dt$month <- month(dt$date)
dt$year <- year(dt$date)

# Hourly average plot
hourly_avg <- dt |>
  group_by(hour) |>
  summarise(avg_sales = mean(sales))

ggplot(hourly_avg, aes(x = hour, y = avg_sales)) +
  geom_bar(stat = "identity") +
  labs(x = "Hour", y = "Average sales") +
  ggtitle("Hourly Average Plot")

# Monthly trend plot
monthly_trend <- dt |>
  group_by(month, year) |> 
  summarise(avg_sales = mean(sales))

ggplot(monthly_trend, aes(x = interaction(month, year), y = avg_sales)) +
  geom_point() +
  labs(x = "Month-Year", y = "Average sales") +
  ggtitle("Monthly Trend Plot")


# Boxplot for outlier detection
ggplot(dt, aes(y = sales)) +
  geom_boxplot() +
  ggtitle("Boxplot for Outlier Detection")



#######################################
# prophet

dt <- dt |> select(ds=date,y=sales)

m <- prophet(dt, changepoint.prior.scale=0.01)
future <- make_future_dataframe(m, periods = 300, freq = 60 * 60)
fcst <- predict(m, future)
#plot(m, fcst)


prophet_plot_components(m, fcst)


dt2 <- dt |>
  mutate(ds = as.POSIXct(ds, tz="GMT")) |>
  filter(as.numeric(format(ds, "%H")) > 3)
m <- prophet(dt2)

prophet_plot_components(m, fcst)

future <- make_future_dataframe(m, periods = 300, freq = 60 * 60)
future2 <- future |>  
  filter(as.numeric(format(ds, "%H")) > 6)
fcst <- predict(m, future2)
plot(m, fcst)
