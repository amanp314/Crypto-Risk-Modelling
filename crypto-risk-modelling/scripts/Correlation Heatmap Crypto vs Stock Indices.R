# Load libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)

# Function to read and clean data
prep_data <- function(filename, asset_name) {
  read_csv(filename, show_col_types = FALSE) %>%
    select(Date, Price) %>%
    rename(!!asset_name := Price) %>%
    mutate(Date = mdy(Date))  # parses mm/dd/yyyy
}

# Read files (no need for file paths since you're in the correct folder)
btc <- prep_data("Bitcoin 2020-24.csv", "Bitcoin")
eth <- prep_data("Ethereum 2020-24.csv", "Ethereum")
sp500 <- prep_data("S&P 500 2020-24.csv", "S&P500")
dow <- prep_data("Dow Jones 2020-24.csv", "DowJones")

# Merge datasets by Date
all_data <- reduce(list(btc, eth, sp500, dow), full_join, by = "Date") %>%
  arrange(Date) %>%
  drop_na()

# Compute log returns
log_returns <- all_data %>%
  mutate(across(-Date, ~ log(. / lag(.)))) %>%
  drop_na()

# Correlation matrix
cor_matrix <- cor(select(log_returns, -Date), use = "complete.obs")

# Reshape for heatmap
melted_cor <- melt(cor_matrix)

# Plot heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal(base_size = 14) +
  labs(title = "Correlation Heatmap: Crypto vs Stock Indices (2020–2024)",
       x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


