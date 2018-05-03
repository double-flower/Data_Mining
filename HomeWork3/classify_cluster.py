import pandas as pd
from sklearn.feature_extraction import DictVectorizer
from sklearn.ensemble import AdaBoostClassifier
from sklearn import model_selection, svm, metrics
from sklearn.cluster import KMeans, SpectralClustering
import matplotlib.pyplot as plt
import itertools

def preprocess():
    # read the dataset
    x_train = pd.read_csv("dataset/train.csv")
    x_test = pd.read_csv("dataset/test.csv")

    # view the information of dataset
    x_train.info()
    x_test.info()

    # process the missing values
    # (1) abandon some features:
    #     PassengerID, Name, Ticket are useless
    #     Cabin has too many missing values
    features = ['Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked']
    x_train, y_train = x_train[features], x_train['Survived']
    x_test = x_test[features]

    # (2) filling missing values of the features with mean
    x_train['Age'].fillna(x_train['Age'].mean(), inplace=True)      # "inplace=True" indicates direct modifying to original object
    x_train['Embarked'].fillna(x_train['Embarked'].mode()[0], inplace=True)
    x_test['Age'].fillna(x_test['Age'].mean(), inplace=True)
    x_test['Fare'].fillna(x_test['Fare'].mean(), inplace=True)

    # check the filling result of missing values
    x_train.info()
    x_test.info()

    return x_train, y_train, x_test

def svmClassifier():
    x_train, y_train, x_test = preprocess()

    # convert features to vectors
    dv = DictVectorizer(sparse=False)
    x_train = dv.fit_transform(x_train.to_dict(orient='record'))
    x_test = dv.fit_transform(x_test.to_dict(orient='record'))

    # set the parameters by cross-validation
    parameters = {'kernel':('linear', 'rbf'), 'C':[1,10]}

    # create and train the model
    model = model_selection.GridSearchCV(svm.SVC(), parameters, refit=True, cv=4, scoring='roc_auc')
    model.fit(x_train, y_train)

    # summarize the fit result of the model
    print("Grid scores on validation set:")
    means = model.cv_results_['mean_test_score']
    stds = model.cv_results_['std_test_score']
    for mean, std, params in zip(means, stds, model.cv_results_['params']):
        print("%0.3f (+/-%0.3f) for %r" % (mean, std, params))

    print("Best parameters:")
    print(model.best_params_)
    #print(model)

    # write the predicted result to file
    y_predict = model.predict(x_test)
    result = {'PassengerId': pd.read_csv('dataset/test.csv')['PassengerId'], 'Survived': y_predict}
    result = pd.DataFrame(result)
    result.to_csv('submission_SVM.csv', index=False)

def AdaBoostClassify():
    x_train, y_train, x_test = preprocess()

    # convert features to vectors
    dv = DictVectorizer(sparse=False)
    x_train = dv.fit_transform(x_train.to_dict(orient='record'))
    x_test = dv.fit_transform(x_test.to_dict(orient='record'))

    parameters = {'n_estimators':[50, 100, 150, 200], 'learning_rate':[1, 0.1, 0.01]}
    # create and train the model
    # model = AdaBoostClassifier(n_estimators=200, learning_rate=0.1)
    #model = AdaBoostClassifier(n_estimators=400, learning_rate=0.1)
    model = model_selection.GridSearchCV(AdaBoostClassifier(), parameters, refit=True, cv=4, scoring='roc_auc')
    model.fit(x_train, y_train)

    # summarize the fit result of the model
    print("Grid scores on validation set:")
    means = model.cv_results_['mean_test_score']
    stds = model.cv_results_['std_test_score']
    for mean, std, params in zip(means, stds, model.cv_results_['params']):
        print("%0.3f (+/-%0.3f) for %r" % (mean, std, params))

    print("Best parameters:")
    print(model.best_params_)

    # write the predicted result to file
    y_predict = model.predict(x_test)
    result = {'PassengerId': pd.read_csv('dataset/test.csv')['PassengerId'], 'Survived': y_predict}
    result = pd.DataFrame(result)
    result.to_csv('submission_AdaBoost.csv', index=False)

def KmeansClustering():
    x_train, y_train, x_test = preprocess()
    features = ['Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked']
    errorResult = {}
    numFeatures = 2

    for comb in itertools.combinations(range(len(features)), numFeatures):
        temp = []
        for j in comb:
            temp.append(features[j])
        feature = pd.DataFrame(x_train, columns=temp)

        # convert features to vectors
        dv = DictVectorizer(sparse=True)
        feature = dv.fit_transform(feature.to_dict(orient='record'))

        # create and train the model
        model = KMeans(n_clusters=2)
        model.fit(feature)

        result = model.labels_
        errorResult[tuple(temp)] = metrics.adjusted_rand_score(y_train, result)

    errorResult = sorted(errorResult.items(), key=lambda x:x[1])

    keyList = []
    valueList = []
    for key, value in errorResult:
        keyList.append(key)
        valueList.append(value)
        print("%-25s: %.3f" % (key, value))

    plt.bar(range(1, len(errorResult)+1), valueList)
    # for key in keyList:
    #     print(key)
    plt.title('adjusted rand index')
    #plt.show()
    plt.savefig('Kmeans.png')

def spectralClustering():
    x_train, y_train, x_test = preprocess()
    features = ['Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked']
    errorResult = {}
    numFeatures = 2

    for comb in itertools.combinations(range(len(features)), numFeatures):
        temp = []
        for j in comb:
            temp.append(features[j])

        feature = pd.DataFrame(x_train, columns=temp)

        # convert features to vectors
        dv = DictVectorizer(sparse=False)
        feature = dv.fit_transform(feature.to_dict(orient='record'))

        # Clustering
        result = SpectralClustering(affinity='nearest_neighbors', n_clusters=2, n_neighbors=8).fit_predict(feature)
        errorResult[tuple(temp)] = metrics.adjusted_rand_score(y_train, result)

    errorResult = sorted(errorResult.items(), key=lambda x: x[1])

    keyList = []
    valueList = []
    for key, value in errorResult:
        keyList.append(key)
        valueList.append(value)
        print("%-25s: %.3f" % (key, value))

    plt.bar(range(1, len(errorResult) + 1), valueList)
    #for key in keyList:
    #    print(key)
    plt.title('adjusted rand index')
    plt.savefig('SpectralClustering.png')

if __name__ == '__main__':
    # svmClassifier()
    # AdaBoostClassify()
    # KmeansClustering()
    spectralClustering()