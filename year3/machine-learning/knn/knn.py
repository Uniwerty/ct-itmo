import numpy as np
from sklearn.neighbors import BallTree
from kernels import get_kernel
from metrics import get_metric


class KnnClassifier:
    neighbors_number = 1
    window_type = None
    h = 1
    kernel_function = None
    metric = None
    tree = None
    values = None
    weights = None
    features_number = 0

    def __init__(self,
                 neighbors_number,
                 window_type='unfixed',
                 h=1,
                 kernel='uniform',
                 a=1,
                 b=1,
                 metric='minkowski',
                 p=2):
        self.neighbors_number = neighbors_number
        if window_type not in ['fixed', 'unfixed']:
            raise Exception('Unknown window type specified: fixed or unfixed are available.')
        self.window_type = window_type
        self.h = h
        self.metric = get_metric(metric, p)
        self.kernel_function = get_kernel(kernel, a, b)

    def fit(self, X, y, w=None):
        self.tree = BallTree(X, metric=self.metric)
        self.values = y
        self.weights = w
        self.features_number = len(y.unique())

    def predict(self, X):
        preds = [0.0] * X.shape[0]
        for i in range(X.shape[0]):
            if self.window_type == 'fixed':
                ind, dist = self.tree.query_radius([X.iloc[i].values], self.h, return_distance=True)
                ind, dist = ind[0], dist[0]
                h = self.h
            else:
                dist, ind = self.tree.query([X.iloc[i].values], self.neighbors_number + 1, return_distance=True)
                dist, ind = dist[0], ind[0]
                h = dist[-1]
            feature_factors = [0.0] * self.features_number
            for j in range(len(ind)):
                weight = 1 if self.weights is None else self.weights[ind[j]]
                factor = self.kernel_function(dist[j] / h) * weight
                feature_factors[self.values.iloc[ind[j]]] += factor
            preds[i] = np.argmax(feature_factors)
        return preds
