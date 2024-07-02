import numpy as np
from numpy import dot

from losses import get_loss, get_loss_gradient


class GDClassifier:
    loss = None
    loss_gradient = None
    l1 = 0
    l2 = 0
    lr = 1e-4
    epochs = 1
    w = None

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
        return np.array([1 if dot(X.iloc[i], self.w) >= 0 else -1 for i in range(X.shape[0])])
