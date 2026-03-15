# Load required packages
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(ggplot2)

# Define a function to clean each dataset
clean_data <- function(df, date_format = "%m/%d/%Y") {
  df <- df %>%
    # Select only the needed columns (Date and Price)
    select(Date, Price) %>%
    # Remove commas from Price and convert to numeric
    mutate(
      Price = as.numeric(gsub(",", "", Price)),
      Date = as.Date(Date, format = date_format)
    ) %>%
    arrange(Date)  # Ensure the data is in ascending order
  return(df)
}

# Read in your CSV files 
btc <- read.csv("Bitcoin 2020-24.csv", stringsAsFactors = FALSE)
eth <- read.csv("Ethereum 2020-24.csv", stringsAsFactors = FALSE)
sp500 <- read.csv("S&P 500 2020-24.csv", stringsAsFactors = FALSE)
dow <- read.csv("Dow Jones 2020-24.csv", stringsAsFactors = FALSE)

# Clean each dataset
btc_clean   <- clean_data(btc, date_format = "%m/%d/%Y")
eth_clean   <- clean_data(eth, date_format = "%m/%d/%Y")
sp500_clean <- clean_data(sp500, date_format = "%d/%m/%Y")
dow_clean   <- clean_data(dow, date_format = "%m/%d/%Y")

# Function to compute daily log returns
compute_returns <- function(df) {
  df %>%
    mutate(Return = c(NA, diff(log(Price)))) %>%
    filter(!is.na(Return))
}

# Compute log returns for each asset
btc_returns   <- compute_returns(btc_clean)
eth_returns   <- compute_returns(eth_clean)
sp500_returns <- compute_returns(sp500_clean)
dow_returns   <- compute_returns(dow_clean)

# Function to compute rolling 30-day annualised volatility
compute_volatility <- function(returns_series, window = 30) {
  # Calculate rolling standard deviation and annualise it (using 252 trading days)
  roll_sd <- rollapply(returns_series, width = window, FUN = sd, fill = NA, align = "right")
  volatility <- roll_sd * sqrt(252)
  return(volatility)
}

# Apply the rolling volatility function to each asset
btc_returns$Volatility   <- compute_volatility(btc_returns$Return, window = 30)
eth_returns$Volatility   <- compute_volatility(eth_returns$Return, window = 30)
sp500_returns$Volatility <- compute_volatility(sp500_returns$Return, window = 30)
dow_returns$Volatility   <- compute_volatility(dow_returns$Return, window = 30)

# Prepare data for plotting by selecting Date and Volatility, and adding an Asset identifier
btc_vol   <- btc_returns %>% select(Date, Volatility) %>% mutate(Asset = "Bitcoin")
eth_vol   <- eth_returns %>% select(Date, Volatility) %>% mutate(Asset = "Ethereum")
sp500_vol <- sp500_returns %>% select(Date, Volatility) %>% mutate(Asset = "S&P 500")
dow_vol   <- dow_returns %>% select(Date, Volatility) %>% mutate(Asset = "Dow Jones")

# Combine all data into one data frame
vol_data <- bind_rows(btc_vol, eth_vol, sp500_vol, dow_vol)

# Plot the 30-Day Rolling Annualised Volatility using ggplot2
ggplot(vol_data, aes(x = Date, y = Volatility, colour = Asset)) +
  geom_line(size = 1) +
  labs(title = "30-Day Rolling Annualised Volatility (2020-2024)",
       x = "Date",
       y = "Annualised Volatility",
       colour = "Asset") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
