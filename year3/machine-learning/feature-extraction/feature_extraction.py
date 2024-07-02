import pandas as pd
import numpy as np
from linear import GDClassifier
from sklearn.metrics import f1_score
from sklearn.linear_model import SGDClassifier


class EmbeddedFeatureSelector:
    def __init__(self, lr, epochs, loss='linear', l1=0, l2=0):
        self.classifier = GDClassifier(lr, epochs, loss, l1, l2)

    def select_features(self, X, y, n_features):
        self.classifier.fit(X, y)
        feature_importance = self.classifier.w.apply(abs).sort_values(ascending=False)
        return feature_importance[:n_features].index.values


class WrapperFeatureSelector:
    def __init__(self):
        self.classifier = SGDClassifier()

    def select_features(self, X, y, n_features):
        features = set(X.columns.values)
        selected_features = []
        for i in range(n_features):
            max_quality = 0
            best_feature = None
            for feature in features:
                X_subset = X[selected_features + [feature]]
                self.classifier.fit(X_subset, y)
                quality = f1_score(y, self.classifier.predict(X_subset))
                if quality > max_quality:
                    max_quality = quality
                    best_feature = feature
            selected_features.append(best_feature)
            features.remove(best_feature)
        return selected_features


class FilterFeatureSelector:
    def select_features(self, X, y, n_features):
        features = X.columns
        conditional_variance = pd.Series(np.zeros(features.size), index=features)
        size = X.shape[0]
        pos_size = y.loc[y == 1].size
        neg_size = size - pos_size
        for feature in features:
            pos_sum = 0
            pos_sqr_sum = 0
            neg_sum = 0
            neg_sqr_sum = 0
            for i in range(size):
                value = X[feature].iloc[i]
                if y.iloc[i] == 1:
                    pos_sum += value
                    pos_sqr_sum += value * value
                else:
                    neg_sum += value
                    neg_sqr_sum += value * value
            pos_variance = pos_sqr_sum / pos_size - (pos_sum / pos_size) ** 2
            neg_variance = neg_sqr_sum / neg_size - (neg_sum / neg_size) ** 2
            conditional_variance[feature] = (pos_variance * pos_size + neg_variance * neg_size) / size
        return conditional_variance.sort_values()[:n_features].index.values
