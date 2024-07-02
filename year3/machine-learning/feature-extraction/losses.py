import numpy as np


def get_loss(loss_name):
    match loss_name:
        case 'linear':
            return linear_loss
        case 'logarithmic':
            return logarithmic_loss
        case 'square':
            return square_loss
        case 'sigmoid':
            return sigmoid_loss
        case _:
            raise Exception('Unknown loss specified: '
                            'linear, logarithmic, square or sigmoid are available.')


def get_loss_gradient(loss_name):
    match loss_name:
        case 'linear':
            return linear_loss_gradient
        case 'logarithmic':
            return logarithmic_loss_gradient
        case 'square':
            return square_loss_gradient
        case 'sigmoid':
            return sigmoid_loss_gradient
        case _:
            raise Exception('Unknown loss specified: '
                            'linear, logarithmic, square or sigmoid are available.')


def linear_loss(X, y, w):
    value = 0
    n = X.shape[0]
    for i in range(n):
        value += max(0, -np.dot(X.iloc[i], w) * y.iloc[i])
    return value / n


def logarithmic_loss(X, y, w):
    value = 0
    n = X.shape[0]
    for i in range(n):
        value += np.log2(1 + np.exp(-np.dot(X.iloc[i], w) * y.iloc[i]))
    return value / n


def square_loss(X, y, w):
    value = 0
    n = X.shape[0]
    for i in range(n):
        m = np.dot(X.iloc[i], w) * y.iloc[i]
        value += (1 - m) * (1 - m)
    return value / n


def sigmoid_loss(X, y, w):
    value = 0
    n = X.shape[0]
    for i in range(n):
        value += 2 / (1 + np.exp(np.dot(X.iloc[i], w) * y.iloc[i]))
    return value / n


def linear_loss_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    n = X.shape[0]
    for i in range(n):
        if np.dot(X.iloc[i], w) * y.iloc[i] <= 0:
            value -= y.iloc[i] * X.iloc[i]
    return value


def logarithmic_loss_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    n = X.shape[0]
    for i in range(n):
        value += X.iloc[i] * y.iloc[i] / (1 + np.exp(y.iloc[i] * np.dot(X.iloc[i], w)))
    return -value


def square_loss_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    n = X.shape[0]
    for i in range(n):
        value -= 2 * y.iloc[i] * (1 - y.iloc[i] * np.dot(X.iloc[i], w)) * X.iloc[i]
    return value


def sigmoid_loss_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    n = X.shape[0]
    for i in range(n):
        sigmoid = 1 / (1 + np.exp(y.iloc[i] * np.dot(X.iloc[i], w)))
        value -= 2 * y.iloc[i] * sigmoid * (1 - sigmoid) * X.iloc[i]
    return value

