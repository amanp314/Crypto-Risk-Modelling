library(httr)
library(jsonlite)
library(ggplot2)
library(zoo)
library(dplyr)
library(tidyr)

#run Model Comp before to get prediction comparison

# Fetch historical Fear & Greed Index data
url <- "https://api.alternative.me/fng/?limit=0"
response <- GET(url)
raw_content <- content(response, as = "text", encoding = "UTF-8")
fng_data <- fromJSON(raw_content)$data

# Convert to data frame
fng_df <- data.frame(
  value = as.numeric(fng_data$value),
  classification = fng_data$value_classification,
  date = as.Date(as.POSIXct(as.numeric(fng_data$timestamp), origin = "1970-01-01"))
)

# Load Bitcoin data
btc <- read.csv("BTC_Clean.csv")
btc$Date <- as.Date(btc$Date)
btc <- btc[order(btc$Date), ]
btc$return <- c(NA, diff(log(btc$Close)))
btc_returns <- na.omit(btc[, c("Date", "return")])

# Merge datasets on Date
merged_df <- merge(fng_df, btc_returns, by.x = "date", by.y = "Date", all = FALSE)

# Ensure Date columns have same class
comparison_df$Date <- as.Date(comparison_df$Date)
merged_df$date <- as.Date(merged_df$date)

# Merge predictions by date
merged_df <- merge(
  merged_df,
  comparison_df[, c("Date", "ARIMA", "GARCH", "RF", "XGB", "LSTM")],
  by.x = "date",
  by.y = "Date",
  all.x = TRUE
)

# Calculate 21-day rolling volatility using the 'return' column
merged_df$Volatility <- zoo::rollapply(merged_df$return, width = 21, FUN = sd, align = "right", fill = NA)


# Assign volatility regimes
quantiles <- quantile(merged_df$Volatility, probs = c(1/3, 2/3), na.rm = TRUE)
merged_df$Vol_Condition <- with(merged_df,
                                ifelse(Volatility < quantiles[1], "Low",
                                       ifelse(Volatility > quantiles[2], "High", "Mid"))
)

# Assign sentiment regimes (from Fear & Greed index)
merged_df$Sentiment_Condition <- with(merged_df,
                                      ifelse(value <= 30, "Extreme Fear",
                                             ifelse(value >= 70, "Extreme Greed", "Neutral"))
)

# RMSE for each model and condition (Volatility)
long_vol <- merged_df %>%
  filter(!is.na(Vol_Condition)) %>%
  pivot_longer(cols = c(ARIMA, GARCH, RF, XGB, LSTM),
               names_to = "Model", values_to = "Prediction") %>%
  group_by(Vol_Condition, Model) %>%
  summarise(RMSE = sqrt(mean((Prediction - return)^2, na.rm = TRUE)), .groups = "drop")

# RMSE for each model and condition (Sentiment)
long_sent <- merged_df %>%
  filter(!is.na(Sentiment_Condition)) %>%
  pivot_longer(cols = c(ARIMA, GARCH, RF, XGB, LSTM),
               names_to = "Model", values_to = "Prediction") %>%
  group_by(Sentiment_Condition, Model) %>%
  summarise(RMSE = sqrt(mean((Prediction - return)^2, na.rm = TRUE)), .groups = "drop")

# Best model for each regime (Table 3 output)
best_vol <- long_vol %>% group_by(Vol_Condition) %>% slice_min(RMSE)
best_sent <- long_sent %>% group_by(Sentiment_Condition) %>% slice_min(RMSE)

# Print tables
cat("Table 3: Best Models by Volatility Regime\n")
print(best_vol)
cat("\nTable 3: Best Models by Sentiment Regime\n")
print(best_sent)

