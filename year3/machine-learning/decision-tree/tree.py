import numpy as np

from gains import get_gain


class DTreeClassifier:
    gain = None
    max_depth = None
    max_leaf_size = None
    tree = None

    def __init__(self, criterion, max_depth, max_leaf_size):
        self.gain = get_gain(criterion)
        self.max_depth = max_depth
        self.max_leaf_size = max_leaf_size

    def fit(self, X, y, weights=None):
        size = X.shape[0]
        positives = 0
        for i in range(size):
            if y.iloc[i] == 1:
                positives += 1
        w = weights if weights is not None else np.full(size, 1 / size)
        self.tree = self.build_tree(X, y, w, positives, size - positives, 1)

    def predict(self, X):
        return np.array([self.tree.get_label(X.iloc[i]) for i in range(X.shape[0])])

    def build_tree(self, samples, target, weights, positives, negatives, depth):
        size = samples.shape[0]
        df = samples.copy()
        target_feature = target.name
        weight_feature = '_Weight'
        if depth == self.max_depth or size <= self.max_leaf_size:
            leaf = DecisionTree()
            label = 0
            for i in range(size):
                label += weights[i] * target.iloc[i]
            leaf.label = 1 if label >= 0 else -1
            return leaf
        max_gain = 0
        split_feature = None
        split_value = 0
        left_positives = 0
        left_negatives = 0
        new_samples = None
        new_target = None
        new_weights = None
        for feature_name in samples:
            df.insert(0, target_feature, target)
            df.insert(0, weight_feature, weights)
            sorted_samples = df.sort_values(feature_name)
            df = df.drop(columns=[target_feature, weight_feature])
            feature = sorted_samples[feature_name]
            sorted_target = sorted_samples[target_feature]
            sorted_weights = sorted_samples[weight_feature]
            sorted_samples = sorted_samples.drop(columns=[target_feature, weight_feature])
            cur_positives = 1 if sorted_target.iloc[0] == 1 else 0
            cur_negatives = 1 - cur_positives
            for i in range(1, size - 1):
                if sorted_target.iloc[i] == 1:
                    cur_positives += 1
                else:
                    cur_negatives += 1
                cur_gain = self.gain(positives, negatives, cur_positives, cur_negatives)
                if cur_gain >= max_gain:
                    max_gain = cur_gain
                    split_feature = feature_name
                    split_value = feature.iloc[i]
                    left_positives = cur_positives
                    left_negatives = cur_negatives
                    new_samples = sorted_samples
                    new_target = sorted_target
                    new_weights = sorted_weights
        tree = DecisionTree()
        tree.split_rule = lambda v: True if v[split_feature] <= split_value else False
        left_samples = left_positives + left_negatives
        tree.left = self.build_tree(
            new_samples.iloc[:left_samples],
            new_target.iloc[:left_samples],
            new_weights.iloc[:left_samples],
            left_positives,
            left_negatives,
            depth + 1
        )
        tree.right = self.build_tree(
            new_samples.iloc[left_samples:],
            new_target.iloc[left_samples:],
            new_weights.iloc[left_samples:],
            positives - left_positives,
            negatives - left_negatives,
            depth + 1
        )
        return tree


class RForestClassifier:
    n_estimators = None
    criterion = None
    max_depth = None
    max_leaf_size = None
    forest = None

    def __init__(self, n_estimators, criterion, max_depth, max_leaf_size):
        self.n_estimators = n_estimators
        self.criterion = criterion
        self.max_depth = max_depth
        self.max_leaf_size = max_leaf_size

    def fit(self, X, y):
        self.forest = []
        df = X.copy()
        target_feature = y.name
        n_samples = df.shape[0] // self.n_estimators
        n_features = int(np.sqrt(df.shape[1]))
        for i in range(self.n_estimators):
            tree = DTreeClassifier(self.criterion, self.max_depth, self.max_leaf_size)
            df.insert(0, target_feature, y)
            sample = df.sample(n=n_samples, replace=True, axis=0)
            target = sample[target_feature]
            sample = sample.drop(columns=[target_feature]).sample(n=n_features, axis=1)
            df = df.drop(columns=[target_feature])
            tree.fit(sample, target)
            self.forest.append(tree)

    def predict(self, X):
        result = np.zeros(X.shape[0])
        for tree in self.forest:
            result += tree.predict(X)
        for i in range(X.shape[0]):
            result[i] = 1 if result[i] >= 0 else -1
        return result


class BoostingClassifier:
    n_estimators = None
    criterion = None
    max_depth = None
    max_leaf_size = None
    estimators = None
    estimators_weights = None

    def __init__(self, n_estimators, criterion, max_depth, max_leaf_size):
        self.n_estimators = n_estimators
        self.criterion = criterion
        self.max_depth = max_depth
        self.max_leaf_size = max_leaf_size

    def fit(self, X, y):
        self.estimators = []
        self.estimators_weights = []
        weights = np.full(X.shape[0], 1 / X.shape[0])
        for i in range(self.n_estimators):
            tree = DTreeClassifier(self.criterion, self.max_depth, self.max_leaf_size)
            tree.fit(X, y, weights)
            prediction = tree.predict(X)
            errors = (prediction != y)
            rate = np.average(errors, weights=weights)
            tree_weight = 0.5 * np.log((1 - rate) / (rate + 1e-9))
            weights *= tree_weight * errors
            weights /= np.sum(weights)
            self.estimators.append(tree)
            self.estimators_weights.append(tree_weight)

    def predict(self, X):
        result = np.zeros(X.shape[0])
        for i in range(self.n_estimators):
            result += self.estimators_weights[i] * self.estimators[i].predict(X)
        for i in range(X.shape[0]):
            result[i] = 1 if result[i] >= 0 else -1
        return result.astype(int)


class DecisionTree:
    split_rule = None
    left = None
    right = None
    label = None

    def get_label(self, v):
        if self.split_rule is None:
            return self.label
        if self.split_rule(v):
            return self.left.get_label(v)
        else:
            return self.right.get_label(v)
