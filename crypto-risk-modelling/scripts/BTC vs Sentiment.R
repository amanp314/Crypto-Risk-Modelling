library(httr)
library(jsonlite)
library(ggplot2)

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

ggplot(merged_df, aes(x = date, y = value)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Crypto Fear & Greed Index Over Time",
    x = "Date",
    y = "Index Value"
  ) +
  theme_minimal()

# Normalize data for comparison
merged_df$scaled_return <- scale(merged_df$return)
merged_df$scaled_fng <- scale(merged_df$value)

# Plot
ggplot(merged_df, aes(x = date)) +
  geom_line(aes(y = scaled_return, color = "Bitcoin Return")) +
  geom_line(aes(y = scaled_fng, color = "Fear & Greed Index")) +
  labs(
    title = "Bitcoin Returns vs. Fear & Greed Index",
    x = "Date",
    y = "Scaled Values",
    color = "Legend"
  ) +
  theme_minimal()

