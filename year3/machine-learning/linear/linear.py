import numpy as np
from numpy import dot, transpose, eye
from numpy.linalg import inv
from sklearn.metrics import f1_score

from kernels import get_kernel, get_kernel_gradient
from losses import get_loss, get_loss_gradient


class MatrixRegressor:
    factors = None

    def fit(self, F, y, tau):
        self.factors = dot(
            dot(
                inv(
                    dot(transpose(F), F) + tau * eye(F.shape[1])
                ),
                transpose(F)
            ),
            y
        )

    def predict(self, X):
        return dot(X, self.factors)


class MatrixClassifier:
    regressor = MatrixRegressor()

    def fit(self, F, y, tau):
        self.regressor.fit(F, y, tau)

    def predict(self, X):
        return list(
            map(
                lambda x: 1 if x >= 0 else -1,
                self.regressor.predict(X)
            )
        )


class GDClassifier:
    loss = None
    loss_gradient = None
    l1 = 0
    l2 = 0
    lr = 1e-4
    epochs = 1
    h = 1e-6
    w = None
    train_history = None
    test_history = None

    def __init__(self, lr, epochs, loss='linear', l1=0, l2=0):
        self.loss = get_loss(loss)
        self.loss_gradient = get_loss_gradient(loss)
        self.l1 = l1
        self.l2 = l2
        self.lr = lr
        self.epochs = epochs

    def fit(self, X, y):
        self.w = np.zeros(X.shape[1])
        for epoch in range(self.epochs):
            gradient = self.loss_gradient(X, y, self.w) + self.l1 * np.sign(self.w) + 2 * self.l2 * self.w
            self.w -= self.lr * gradient

    def predict(self, X):
        return np.sign(dot(X, self.w))

    def debug_fit(self, X_train, y_train, X_test, y_test):
        self.w = np.zeros(X_train.shape[1])
        self.train_history = np.zeros(self.epochs)
        self.test_history = np.zeros(self.epochs)
        for epoch in range(self.epochs):
            gradient = self.loss_gradient(X_train, y_train, self.w) + self.l1 * np.sign(self.w) + 2 * self.l2 * self.w
            self.w -= self.lr * gradient
            self.train_history[epoch] = self.loss(X_train, y_train, self.w)
            self.test_history[epoch] = f1_score(y_test, self.predict(X_test))


class SVMClassifier:
    c = 1
    lr = 1e-4
    epochs = 1
    kernel = None
    kernel_gradient = None
    w = None
    train_history = None
    test_history = None

    def __init__(self, c, lr, epochs, kernel='linear', r=0, n=2, sigma=1, alpha=1):
        self.c = c
        self.kernel = get_kernel(kernel, r, n, sigma, alpha)
        self.kernel_gradient = get_kernel_gradient(kernel, r, n, sigma, alpha)
        self.lr = lr
        self.epochs = epochs

    def fit(self, X, y):
        self.w = np.zeros(X.shape[1])
        for epoch in range(self.epochs):
            gradient = self.kernel_gradient(X, y, self.w) + 1 / self.c * self.w
            self.w -= self.lr * gradient

    def predict(self, X):
        return np.array([1 if self.kernel(X.iloc[i], self.w) >= 0 else -1 for i in range(X.shape[0])])

    def debug_fit(self, X_train, y_train, X_test, y_test):
        self.w = np.zeros(X_train.shape[1])
        self.train_history = np.zeros(self.epochs)
        self.test_history = np.zeros(self.epochs)
        for epoch in range(self.epochs):
            gradient = self.kernel_gradient(X_train, y_train, self.w) + 1 / self.c * self.w
            self.w -= self.lr * gradient
            self.train_history[epoch] = self.loss(X_train, y_train)
            self.test_history[epoch] = f1_score(y_test, self.predict(X_test))

    def loss(self, X, y):
        value = 0
        n = X.shape[0]
        for i in range(n):
            value += max(0, 1 - dot(X.iloc[i], self.w) * y.iloc[i])
        return value / n
