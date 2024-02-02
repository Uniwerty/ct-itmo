import pandas as pd
import numpy as np
from statsmodels.api import tsa
from sklearn.metrics import mean_squared_error
from scipy.optimize import minimize


class MyThetaModel:
    season_period = 12
    gd_start = np.array([0.0, 0.0])
    gd_lr = 10e-8
    gd_epoch = 1000
    grad_dx = 10e-6

    def __init__(self, data):
        self.decompose = tsa.seasonal_decompose(data, period=self.season_period, model='additive')
        self.data = data - self.decompose.seasonal
        self.data_size = data.shape[0]
        self.a = 0
        self.b = 0
        self.alpha = 0.5
        self.theta0 = None
        self.theta2 = None

    def fit(self):
        self.a, self.b = self.fit_linear_regression_params()
        self.theta0 = self.compose_theta0()

        self.theta2 = self.compose_theta2()
        self.alpha = self.fit_exponential_smoothing_params()
        self.theta2 = self.exponential_smoothing(self.alpha)
        return self

    def forecast(self, period):
        forecast_index = pd.date_range(start=self.data.index[-1], periods=period + 1, freq='MS')
        forecast_range = range(self.data_size - 1, self.data_size + period)
        theta0_forecast = pd.Series([self.a * i + self.b for i in forecast_range], index=forecast_index)
        theta2_forecast = pd.Series([self.theta2[-1] for _ in forecast_range], index=forecast_index)
        current_season = (self.data_size - 1) % self.season_period
        seasonal_range = range(current_season, current_season + period + 1)
        seasonal_forecast = pd.Series([self.decompose.seasonal[i] for i in seasonal_range], index=forecast_index)
        return (theta0_forecast + theta2_forecast + seasonal_forecast) * 0.5

    def fit_linear_regression_params(self):
        point = self.gd_start
        for i in range(self.gd_epoch):
            point -= np.dot(self.gd_lr, self.gradient(point))
        return point[0], point[1]

    def compose_theta0(self):
        return pd.Series([self.a * i + self.b for i in range(self.data_size)], index=self.data.index)

    def compose_theta2(self):
        return self.data * 2 - self.theta0

    def fit_exponential_smoothing_params(self):
        result = minimize(
            lambda alpha: mean_squared_error(self.theta2, self.exponential_smoothing(alpha)),
            np.array([self.alpha]),
            bounds=[(0, 1)]
        )
        return result.x[0]

    def exponential_smoothing(self, alpha):
        t = [0] * self.data_size
        t[0] = self.theta2.iloc[0]
        for i in range(1, self.data_size):
            t[i] = alpha * self.theta2.iloc[i] + (1 - alpha) * t[i - 1]
        return pd.Series(t, index=self.data.index)

    def gradient(self, point):
        grad = np.zeros(2)
        a = point[0]
        b = point[1]
        dist0 = self.distances(a, b)
        grad[0] = (dist0 - self.distances(a - self.grad_dx, b)) / self.grad_dx
        grad[1] = (dist0 - self.distances(a, b - self.grad_dx)) / self.grad_dx
        return grad

    def distances(self, a, b):
        res = 0
        for i in range(len(self.data)):
            res += (a * i + b - self.data[i]) ** 2
        return res / len(self.data)
