import numpy as np
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt

# Data provided
bmis = np.array([30.30, 33.88, 31.46, 30.21, 27.61, 32.39, 27.91, 37.67, 39.22, 26.74])
blood_pressures = np.array([125.45, 130.81, 127.19, 125.32, 121.41, 128.58, 121.86, 136.51, 138.83, 120.12])

# Reshaping the data to fit the model
X = bmis.reshape(-1, 1)
y = blood_pressures

# Create a linear regression model
model = LinearRegression()

# Train the model using the training sets
model.fit(X, y)

# Making predictions using the model
y_pred = model.predict(X)

# Plotting the results
plt.scatter(X, y, color='blue', label='Actual Data')
plt.plot(X, y_pred, color='red', label='Fitted Line')
plt.title('BMI vs Blood Pressure')
plt.xlabel('BMI')
plt.ylabel('Blood Pressure')
plt.legend()
plt.show()
