import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim


class MultiLayerPerceptron(nn.Module):

    def __init__(self, n_layers, input_size, xavier=False, batch_norm=False):
        super(MultiLayerPerceptron, self).__init__()
        self.layers = nn.Sequential()
        for i in range(n_layers):
            layer = nn.Linear(input_size, input_size)
            if xavier:
                torch.nn.init.xavier_uniform_(layer.weight)
            self.layers.append(layer)
            if batch_norm:
                self.layers.append(nn.BatchNorm1d(input_size))
            self.layers.append(nn.ReLU())
        layer = nn.Linear(input_size, 1)
        self.layers.append(layer)
        if xavier:
            torch.nn.init.xavier_uniform_(layer.weight)
        self.layers.append(nn.Sigmoid())

    def forward(self, x):
        return self.layers(x).squeeze()


class MLPClassifier:
    model = None
    train_history = None
    test_history = None

    def __init__(self, n_layers, input_size, xavier=False, batch_norm=False):
        self.model = MultiLayerPerceptron(n_layers, input_size, xavier, batch_norm)

    def fit(self, X, y, epochs, lr):
        criterion = nn.BCELoss()
        optimizer = optim.SGD(params=self.model.layers.parameters(), lr=lr)
        for epoch in range(epochs):
            prediction = self.model.forward(X)
            loss = criterion(prediction, y)
            loss.backward()
            optimizer.step()
            optimizer.zero_grad()

    def predict(self, X):
        return np.where(self.model.forward(X).detach().numpy() < 0.5, 0, 1)


class ResidualBlock(nn.Module):

    def __init__(self, input_size, xavier=False):
        super(ResidualBlock, self).__init__()
        self.layer1 = nn.Linear(input_size, input_size)
        self.layer2 = nn.Linear(input_size, input_size)
        if xavier:
            torch.nn.init.xavier_uniform_(self.layer1.weight)
            torch.nn.init.xavier_uniform_(self.layer2.weight)

    def forward(self, x):
        out = self.layer1(x)
        out = torch.relu(out)
        return self.layer2(out) + x


class ResidualNetwork(nn.Module):
    def __init__(self, n_layers, input_size, xavier=False, batch_norm=False, blocks=None):
        super(ResidualNetwork, self).__init__()
        self.layers = nn.Sequential()
        if blocks is not None:
            for block in blocks:
                self.layers.append(block)
                self.layers.append(nn.ReLU())
            self.layers.append(nn.Linear(input_size, 1))
            self.layers.append(nn.Sigmoid())
            return
        self.block_list = []
        for i in range(n_layers):
            block = ResidualBlock(input_size, xavier)
            self.layers.append(block)
            self.block_list.append(block)
            if batch_norm:
                self.layers.append(nn.BatchNorm1d(input_size))
            self.layers.append(nn.ReLU())
        layer = nn.Linear(input_size, 1)
        self.layers.append(layer)
        if xavier:
            torch.nn.init.xavier_uniform_(layer.weight)
        self.layers.append(nn.Sigmoid())

    def forward(self, x):
        return self.layers(x).squeeze()

    def cut(self, X):
        size = len(self.block_list)
        means = [0] * size
        blocks = {}
        for i in range(size):
            m = np.average(self.forward(X).detach().numpy())
            means[i] = m
            blocks[m] = self.block_list[i]
        means.sort()
        good_blocks = []
        for i in range(int(0.2 * size), size):
            good_blocks.append(blocks[means[i]])
        return good_blocks


class ResNetClassifier:
    model = None
    train_history = None
    test_history = None

    def __init__(self, n_layers, input_size, xavier=False, batch_norm=False, blocks=None):
        self.model = ResidualNetwork(n_layers, input_size, xavier, batch_norm, blocks)

    def fit(self, X, y, epochs, lr, X_test=None, y_test=None):
        self.train_history = []
        self.test_history = []
        criterion = nn.BCELoss()
        optimizer = optim.SGD(params=self.model.layers.parameters(), lr=lr)
        for epoch in range(epochs):
            prediction = self.model.forward(X)
            loss = criterion(prediction, y)
            self.train_history.append(loss.item())
            loss.backward()
            optimizer.step()
            optimizer.zero_grad()
            if X_test is not None:
                with torch.no_grad():
                    output = self.model.forward(X_test)
                    loss = criterion(y_test, output)
                    self.test_history.append(loss.item())

    def predict(self, X):
        return np.where(self.model.forward(X).detach().numpy() < 0.5, 0, 1)


