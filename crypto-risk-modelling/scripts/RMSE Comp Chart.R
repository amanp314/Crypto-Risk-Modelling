library(ggplot2)
library(fmsb)

rmse_arima <- 0.02749 # from rolling ARIMA
rmse_garch <- 0.02755 # from original GARCH mean forecast
rmse_rf <- 0.02977 # from Random Forest
rmse_xgb <- 0.0428 # from XGBoost
rmse_lstm <- 0.02551 # from lstm_preds

model_performance <- data.frame(
  Type = c("Traditional", "Traditional", "Machine Learning", "Machine Learning", "Deep Learning"),
  Model = c("ARIMA", "GARCH", "Random Forest", "XGBoost", "LSTM"),
  RMSE = c(rmse_arima, rmse_garch, rmse_rf, rmse_xgb, rmse_lstm)  # Replace with actual rmse vars
)

ggplot(model_performance, aes(x = Model, y = RMSE, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Figure 5: Model RMSE Comparison by Type",
       y = "Root Mean Squared Error",
       x = "Model") +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())

