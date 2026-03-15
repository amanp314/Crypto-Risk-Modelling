# Load required packages
library(readr)
library(dplyr)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(rugarch)

# Load cleaned CSVs
btc <- read_csv("BTC_Clean.csv")
eth <- read_csv("ETH_Clean.csv")
djia <- read_csv("DJIA_Clean.csv")
spx <- read_csv("SPX_Clean.csv")

# Convert to Date and sort
btc <- btc %>% mutate(Date = as.Date(Date)) %>% arrange(Date)
eth <- eth %>% mutate(Date = as.Date(Date)) %>% arrange(Date)
djia <- djia %>% mutate(Date = as.Date(Date)) %>% arrange(Date)
spx <- spx %>% mutate(Date = as.Date(Date)) %>% arrange(Date)

# VAR and Exceedance

# Calculate daily returns
btc$return <- dailyReturn(xts(btc$Close, order.by = btc$Date))
eth$return <- dailyReturn(xts(eth$Close, order.by = eth$Date))
djia$return <- dailyReturn(xts(djia$Close, order.by = djia$Date))
spx$return <- dailyReturn(xts(spx$Close, order.by = spx$Date))

# Bitcoin
btc_returns <- na.omit(btc$return)
btc_var_95 <- quantile(btc_returns, probs = 0.05)
btc_var_99 <- quantile(btc_returns, probs = 0.01)
btc_exc_95 <- mean(btc_returns < btc_var_95) * 100
btc_exc_99 <- mean(btc_returns < btc_var_99) * 100

# Ethereum
eth_returns <- na.omit(eth$return)
eth_var_95 <- quantile(eth_returns, probs = 0.05)
eth_var_99 <- quantile(eth_returns, probs = 0.01)
eth_exc_95 <- mean(eth_returns < eth_var_95) * 100
eth_exc_99 <- mean(eth_returns < eth_var_99) * 100

# Dow Jones
djia_returns <- na.omit(djia$return)
djia_var_95 <- quantile(djia_returns, probs = 0.05)
djia_var_99 <- quantile(djia_returns, probs = 0.01)
djia_exc_95 <- mean(djia_returns < djia_var_95) * 100
djia_exc_99 <- mean(djia_returns < djia_var_99) * 100

# S&P 500
spx_returns <- na.omit(spx$return)
spx_var_95 <- quantile(spx_returns, probs = 0.05)
spx_var_99 <- quantile(spx_returns, probs = 0.01)
spx_exc_95 <- mean(spx_returns < spx_var_95) * 100
spx_exc_99 <- mean(spx_returns < spx_var_99) * 100

# Combine into a single table
var_table <- data.frame(
  Asset = c("Bitcoin", "Ethereum", "Dow Jones", "S&P 500"),
  VaR_95 = round(c(btc_var_95, eth_var_95, djia_var_95, spx_var_95), 5),
  Exceed_95 = round(c(btc_exc_95, eth_exc_95, djia_exc_95, spx_exc_95), 2),
  VaR_99 = round(c(btc_var_99, eth_var_99, djia_var_99, spx_var_99), 5),
  Exceed_99 = round(c(btc_exc_99, eth_exc_99, djia_exc_99, spx_exc_99), 2)
)

print(var_table)

# Figure 1 — Time Series Plot of BTC Returns vs VaR (95% and 99%)

# Prepare plot data
# Create a new dataframe with regular types
btc_plot <- data.frame(
  Date = index(btc$return),
  return = as.numeric(btc$return)
)

btc_var_95 <- quantile(btc_plot$return, probs = 0.05)
btc_var_99 <- quantile(btc_plot$return, probs = 0.01)


label_x_pos <- max(btc_plot$Date) - 10
# Plot
ggplot(btc_plot, aes(x = Date)) +
  geom_line(aes(y = return), colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = btc_var_95, linetype = "dashed", colour = "blue", linewidth = 1) +
  geom_hline(yintercept = btc_var_99, linetype = "dashed", colour = "red", linewidth = 1) +
  annotate("text", x = label_x_pos, y = btc_var_95 - 0.015, 
           label = "95% VaR", hjust = 1, colour = "blue", size = 3.5, fontface = "bold") +
  annotate("text", x = label_x_pos, y = btc_var_99 - 0.015, 
           label = "99% VaR", hjust = 1, colour = "red", size = 3.5, fontface = "bold") +
  labs(title = "Bitcoin Daily Returns vs Historical VaR",
       x = "Date",
       y = "Daily Log Return") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13)
  )

#Figure 2 - Ethereum – GARCH-Predicted Volatility Overlay on Returns

# Remove NA and fit GARCH model
eth_returns <- na.omit(eth$return)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = eth_returns)

# Extract conditional volatility
volatility <- sigma(fit)

# Match it with dates
eth_plot <- data.frame(
  Date = index(eth_returns),
  return = as.numeric(eth_returns),
  volatility = as.numeric(volatility)
)

ggplot(eth_plot, aes(x = Date)) +
  geom_line(aes(y = return), colour = "black", linewidth = 0.3) +
  geom_line(aes(y = volatility), colour = "purple", linewidth = 0.7) +
  labs(title = "Ethereum Daily Returns and GARCH(1,1) Volatility",
       y = "Daily Log Return and Conditional Volatility",
       x = "Date") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13)
  )

