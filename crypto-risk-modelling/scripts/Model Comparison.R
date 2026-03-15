library(forecast)
library(quantmod)
library(rugarch)
library(randomForest)
library(caret)
library(xgboost)
library(reshape2)
library(ggplot2)

# Step 1: Load Cleaned Bitcoin Data
btc <- read.csv("BTC_Clean.csv")
btc$Date <- as.Date(btc$Date)

# Step 2: Sort by date and calculate log returns
btc <- btc[order(btc$Date), ]
btc$return <- c(NA, diff(log(btc$Close)))

# Step 3: Remove NA from first return
btc_returns <- na.omit(btc$return)


#ARIMA

# Step 1: Fit ARIMA model using auto.arima
arima_model <- auto.arima(btc_returns)

# Step 2: Generate in-sample predictions
arima_pred <- fitted(arima_model)

# Step 3: Compute MSE and RMSE
actual <- btc_returns
predicted <- as.numeric(arima_pred)

# Align lengths (in case of mismatch)
n <- min(length(actual), length(predicted))
actual <- actual[1:n]
predicted <- predicted[1:n]

mse <- mean((actual - predicted)^2)
rmse <- sqrt(mse)

# Step 4: Output results
cat("ARIMA MSE:", round(mse, 5), "\n")
cat("ARIMA RMSE:", round(rmse, 5), "\n")

# Load and prepare data
btc <- read.csv("BTC_Clean.csv")
btc$Date <- as.Date(btc$Date)
btc <- btc[order(btc$Date), ]
btc$return <- c(NA, diff(log(btc$Close)))
btc_returns <- na.omit(btc$return)

# Train/test split (80/20)
split_idx <- floor(0.8 * length(btc_returns))
train <- btc_returns[1:split_idx]
test <- btc_returns[(split_idx + 1):length(btc_returns)]

# Compute MSE and RMSE
mse <- mean((test - predicted)^2)
rmse <- sqrt(mse)

cat("ARIMA Test MSE:", round(mse, 5), "\n")
cat("ARIMA Test RMSE:", round(rmse, 5), "\n")

#GARCH

# Prepare train/test split for GARCH 
train <- btc_returns[1:split_idx]
test <- btc_returns[(split_idx + 1):length(btc_returns)]

# GARCH(1,1) specification
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)

# Fit on training data
fit <- ugarchfit(spec, data = train)

# Forecast for test period
fc <- ugarchforecast(fit, n.ahead = length(test))
predicted <- as.numeric(fc@forecast$seriesFor)

# Compute MSE and RMSE
mse <- mean((test - predicted)^2)
rmse <- sqrt(mse)

cat("GARCH Test MSE:", round(mse, 5), "\n")
cat("GARCH Test RMSE:", round(rmse, 5), "\n")

#LAG

# Create lag features (supervised learning)
btc_ml <- data.frame(
  return = btc_returns,
  lag1 = dplyr::lag(btc_returns, 1),
  lag2 = dplyr::lag(btc_returns, 2),
  lag3 = dplyr::lag(btc_returns, 3)
)
btc_ml <- na.omit(btc_ml)

# Train/test split
split_idx <- floor(0.8 * nrow(btc_ml))
train <- btc_ml[1:split_idx, ]
test <- btc_ml[(split_idx + 1):nrow(btc_ml), ]

# Rolling ARIMA
train_series <- train$return
test_series <- test$return

arima_preds <- numeric(length(test_series))

for (i in 1:length(test_series)) {
  ts_train <- ts(as.numeric(train_series))
  model <- auto.arima(ts_train)
  fc <- forecast(model, h = 1)
  arima_preds[i] <- as.numeric(fc$mean)
  train_series <- c(train_series, test_series[i])
}

# ARIMA rolling forecast errors (out-of-sample, for Table 2)
mse_arima <- mean((test_series - arima_preds)^2)
rmse_arima <- sqrt(mse_arima)
cat("ARIMA Rolling Test MSE:", round(mse_arima, 5), "\n")
cat("ARIMA Rolling Test RMSE:", round(rmse_arima, 5), "\n")

# Random Forest model
set.seed(42)
rf_model <- randomForest(return ~ ., data = train)

# Predict
predicted <- predict(rf_model, newdata = test)

# Evaluate
actual <- test$return
mse <- mean((actual - predicted)^2)
rmse <- sqrt(mse)

cat("Random Forest Test MSE:", round(mse, 5), "\n")
cat("Random Forest Test RMSE:", round(rmse, 5), "\n")

#XGBOOST

# Prepare data
train_matrix <- xgb.DMatrix(data = as.matrix(train[, -1]), label = train$return)
test_matrix  <- xgb.DMatrix(data = as.matrix(test[, -1]), label = test$return)

# Train XGBoost model
set.seed(42)
xgb_model <- xgboost(data = train_matrix, nrounds = 100, objective = "reg:squarederror", verbose = 0)

# Predict
predicted <- predict(xgb_model, newdata = test_matrix)

# Evaluate
actual <- test$return
mse <- mean((actual - predicted)^2)
rmse <- sqrt(mse)

cat("XGBoost Test MSE:", round(mse, 5), "\n")
cat("XGBoost Test RMSE:", round(rmse, 5), "\n")


#LSTM

lstm_preds <- read.csv("lstm_predictions.csv")
mse <- mean((lstm_preds$Actual - lstm_preds$LSTM_Pred)^2)
rmse <- sqrt(mse)
cat("LSTM Test MSE:", round(mse, 5), "\n")
cat("LSTM Test RMSE:", round(rmse, 5), "\n")

# Create dataframe for plotting: actual + predictions
comparison_df <- data.frame(
  Date = btc$Date[(length(btc$Date) - length(test$return) + 1):length(btc$Date)],
  Actual = test$return,
  ARIMA = arima_preds,
  GARCH = as.numeric(ugarchforecast(fit, n.ahead = length(test$return))@forecast$seriesFor),
  RF = predict(rf_model, newdata = test),
  XGB = pmax(pmin(predict(xgb_model, newdata = test_matrix), 0.1), -0.1),
  LSTM = lstm_preds$LSTM_Pred
)

# Reshape for plotting
long_df <- melt(comparison_df, id.vars = "Date", variable.name = "Model", value.name = "Return")

long_df$Type <- with(long_df, ifelse(Model == "Actual", "Actual",
                                     ifelse(Model %in% c("ARIMA", "GARCH"), "Traditional",
                                            ifelse(Model %in% c("RF", "XGB"), "Machine Learning", "Deep Learning"))))

# Plot all models vs actual
ggplot(long_df, aes(x = Date, y = Return, colour = Model, linetype = Model)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Predicted vs Actual Bitcoin Returns by Model Group",
       y = "Return",
       x = "Date") +
  facet_wrap(~ Type, scales = "free_y", ncol = 1) +
  theme_minimal(base_size = 13) +
  scale_colour_manual(values = c(
    "Actual" = "black",
    "ARIMA" = "blue",
    "GARCH" = "purple",
    "RF" = "forestgreen",
    "XGB" = "orange",
    "LSTM" = "red"
  )) +
  scale_linetype_manual(values = c(
    "Actual" = "solid",
    "ARIMA" = "dotted",
    "GARCH" = "twodash",
    "RF" = "dotdash",
    "XGB" = "longdash",
    "LSTM" = "dashed"
  )) +
  theme(
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )

mse_table <- data.frame(
  Model = c("ARIMA", "GARCH", "Random Forest", "XGBoost", "LSTM"),
  MSE = c(0.00076, 0.00076, 0.00089, 0.00183, 0.00065),
  RMSE = c(0.2749, 0.2755, 0.2977, 0.0428, 0.02551)
)

cat("\nTable: MSE and RMSE for All Models\n")
print(mse_table)



