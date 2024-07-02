import numpy as np


def get_kernel(kernel_name, r=0, n=2, sigma=1, alpha=1):
    match kernel_name:
        case 'linear':
            return linear_kernel
        case 'polynomial':
            return lambda u, v: polynomial_kernel(r, n, u, v)
        case 'gaussian':
            return lambda u, v: gaussian_kernel(sigma, u, v)
        case 'laplacian':
            return lambda u, v: laplacian_kernel(alpha, u, v)
        case _:
            raise Exception('Unknown kernel specified: linear, polynomial, gaussian or laplacian are available.')


def get_kernel_gradient(kernel_name, r=0, n=2, sigma=1, alpha=1):
    match kernel_name:
        case 'linear':
            return linear_kernel_gradient
        case 'polynomial':
            return lambda X, y, w: polynomial_kernel_gradient(r, n, X, y, w)
        case 'gaussian':
            return lambda X, y, w: gaussian_kernel_gradient(sigma, X, y, w)
        case 'laplacian':
            return lambda X, y, w: laplacian_kernel_gradient(alpha, X, y, w)
        case _:
            raise Exception('Unknown kernel specified: linear, polynomial, gaussian or laplacian are available.')


def linear_kernel(u, v):
    return np.dot(u, v)


def polynomial_kernel(r, n, u, v):
    return (np.dot(u, v) + r) ** n


def gaussian_kernel(sigma, u, v):
    return np.exp(-(np.dot(u - v, u - v)) / (2 * sigma ** 2))


def laplacian_kernel(alpha, u, v):
    return np.exp(-alpha * np.linalg.norm(u - v, ord=2))


def linear_kernel_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    for i in range(X.shape[0]):
        if y.iloc[i] * linear_kernel(X.iloc[i], w) < 1:
            value -= y.iloc[i] * X.iloc[i]
    return value


def polynomial_kernel_gradient(r, n, X, y, w):
    value = np.zeros(X.shape[1])
    for i in range(X.shape[0]):
        if y.iloc[i] * polynomial_kernel(r, n, X.iloc[i], w) < 1:
            value -= y.iloc[i] * n * (np.dot(X.iloc[i], w) + r) ** (n - 1) * X.iloc[i]
    return value


def gaussian_kernel_gradient(sigma, X, y, w):
    value = np.zeros(X.shape[1])
    sigma2 = sigma * sigma
    for i in range(X.shape[0]):
        if y.iloc[i] * gaussian_kernel(sigma, X.iloc[i], w) < 1:
            diff = X.iloc[i] - w
            value -= y.iloc[i] / sigma2 * np.exp(-np.dot(diff, diff) / (2 * sigma2)) * diff
    return value


def laplacian_kernel_gradient(alpha, X, y, w):
    value = np.zeros(X.shape[1])
    for i in range(X.shape[0]):
        if y.iloc[i] * laplacian_kernel(alpha, X.iloc[i], w) < 1:
            diff = X.iloc[i] - w
            norm = np.linalg.norm(diff, ord=2)
            value -= y.iloc[i] * alpha / norm * np.exp(-alpha * norm) * diff
    return value
