import numpy as np
from sklearn.metrics import DistanceMetric


def get_metric(metric_name, p=2):
    if metric_name == 'minkowski':
        return DistanceMetric.get_metric('minkowski', p=p)
    elif metric_name == 'cosine':
        return DistanceMetric.get_metric(
            'pyfunc',
            func=lambda x, y: 1 - np.inner(x, y) / (np.linalg.norm(x) * np.linalg.norm(y))
        )
    elif metric_name in ['manhattan', 'euclidean', 'chebyshev', 'minkowski']:
        return DistanceMetric.get_metric(metric_name)
    else:
        raise Exception('Unknown metric specified: '
                        'manhattan, euclidean, chebyshev, minkowski or cosine are available.')