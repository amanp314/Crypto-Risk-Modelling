import pandas as pd
import numpy as np
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense
from sklearn.preprocessing import StandardScaler

# Load your test data (btc_test_data.csv must be in the same folder)
data = pd.read_csv('btc_test_data.csv')

# Separate features and target
X = data.drop(columns=['return']).values
y = data['return'].values

# Scale features
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Reshape for LSTM [samples, timesteps, features]
X_scaled = X_scaled.reshape((X_scaled.shape[0], 1, X_scaled.shape[1]))

# Build LSTM model
model = Sequential()
model.add(LSTM(50, activation='relu', input_shape=(X_scaled.shape[1], X_scaled.shape[2])))
model.add(Dense(1))
model.compile(optimizer='adam', loss='mse')

# Train model
model.fit(X_scaled, y, epochs=50, batch_size=32, verbose=1)

# Predict
y_pred = model.predict(X_scaled).flatten()

# Save predictions to CSV
output = pd.DataFrame({'LSTM_Pred': y_pred, 'Actual': y})
output.to_csv('lstm_predictions.csv', index=False)
print("Done! Results saved to lstm_predictions.csv")
