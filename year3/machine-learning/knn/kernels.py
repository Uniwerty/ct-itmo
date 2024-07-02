import numpy as np


def get_kernel(kernel_name, a=1, b=1):
    match kernel_name:
        case 'uniform':
            return lambda u: 0.5 * (1 if u < 1 else 0)
        case 'gaussian':
            return lambda u: 1 / np.sqrt(2 * np.pi) * np.exp(- u * u / 2)
        case 'triangular':
            return lambda u: (1 - np.abs(u)) * (1 if u < 1 else 0)
        case 'epanechnikov':
            return lambda u: 0.75 * (1 - u ** 2) * (1 if u < 1 else 0)
        case 'biquadratic':
            return lambda u: 0.9375 * (1 - u ** 2) ** 2 * (1 if u < 1 else 0)
        case 'common':
            return lambda u: (1 - np.abs(u) ** a) ** b * (1 if u < 1 else 0)
        case _:
            raise Exception('Unknown kernel specified: '
                            'uniform, gaussian, triangular, epanechnikov, biquadratic or common are available.')
