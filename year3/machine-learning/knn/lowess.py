import numpy as np
from sklearn.neighbors import BallTree
from kernels import get_kernel
from metrics import get_metric


class Lowess:
    neighbors_number = 1
    knn_kernel = None
    lowess_kernel = None
    metric = None

    def __init__(self,
                 neighbors_number,
                 knn_kernel='uniform',
                 lowess_kernel='uniform',
                 knn_a=1,
                 knn_b=1,
                 lowess_a=1,
                 lowess_b=1,
                 metric='minkowski',
                 p=2):
        self.neighbors_number = neighbors_number
        self.metric = get_metric(metric, p)
        self.knn_kernel = get_kernel(knn_kernel, knn_a, knn_b)
        self.lowess_kernel = get_kernel(lowess_kernel, lowess_a, lowess_b)

    def get_weights(self, X, y):
        tree = BallTree(X, metric=self.metric)
        features_number = len(y.unique())
        lowess_weights = [0.0] * X.shape[0]
        for i in range(X.shape[0]):
            dist, ind = tree.query([X.iloc[i].values], self.neighbors_number + 1, return_distance=True)
            dist, ind = dist[0][1:], ind[0][1:]
            h = dist[-1]
            feature_factors = [0.0] * features_number
            for j in range(len(ind)):
                factor = self.knn_kernel(dist[j] / h)
                feature_factors[y.iloc[ind[j]]] += factor
            feature_factors /= np.linalg.norm(feature_factors)
            lowess_weights[i] = self.lowess_kernel(1 - feature_factors[y.iloc[i]])
        return lowess_weights
