#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sun Nov 25 12:54:45 2018

@author: hduser
"""
import pandas as pd

file_dir = '/home/hduser/Dropbox/Analytics/nfl_draft_2019/data/'

file_name = 'imputed_data.csv'

# read data
csv = pd.read_csv(file_dir + file_name)

# clean raw data
csv['team'].fillna('FA', inplace=True)

csv.drop(['Unnamed: 0','num_missing_vals'], axis=1, inplace=True)

csv.dropna(thresh = csv.shape[1]-5, inplace=True)

# initial data exploration and cleanup
#df.dropna(thresh=5)

# replace missing combine stats with the mean
csv["forty"].fillna(csv.groupby("pos")["forty"].transform("mean"), inplace=True)
csv["vertical"].fillna(csv.groupby("pos")["vertical"].transform("mean"), inplace=True)
csv["broad"].fillna(csv.groupby("pos")["broad"].transform("mean"), inplace=True)
csv["bench"].fillna(csv.groupby("pos")["bench"].transform("mean"), inplace=True)
csv["threecone"].fillna(csv.groupby("pos")["threecone"].transform("mean"), inplace=True)
csv["shuttle"].fillna(csv.groupby("pos")["shuttle"].transform("mean"), inplace=True)

# one hot encoding for college
# one hot encode colleges with > 50 records of data.  Rest is encoded as Other    
threshold = 50
college_counts = csv['college'].value_counts()
repl = college_counts[college_counts <= threshold].index
ohe_college = pd.get_dummies(csv['college'].replace(repl, 'other'), prefix = 'ohe_college')

nfl = pd.concat([csv, ohe_college], axis = 1)
nfl = pd.get_dummies(nfl, columns=["pos"])

# K let's neaten up the columns
nfl.columns = nfl.columns.str.replace(' ', '_').str.lower()
nfl.columns = nfl.columns.str.replace('.', '_').str.lower()
nfl.columns = nfl.columns.str.replace('&', '').str.lower()
nfl.columns = nfl.columns.str.replace('(', '').str.lower()
nfl.columns = nfl.columns.str.replace(')', '').str.lower()
nfl.columns = nfl.columns.str.replace('__', '_').str.lower()
nfl.columns = nfl.columns.str.replace('__', '_').str.lower()

all_cols = pd.Series(nfl.columns)

ml_y_bool = 'first_rount_pick'
ml_y_pick = 'pick'

# Get the list of columns that can be used for ML.  
ml_X_cols = []
for col in all_cols:
    if col not in ['first_round_pick','url','pick','team','player','college']:
        ml_X_cols.append(col)

# Toast the last few nulls 
nfl= nfl.dropna()

# Split the data 70/30 into train test 
from sklearn.model_selection import train_test_split
nfl_train, nfl_test = train_test_split(nfl, test_size=0.3, random_state=73)
#nfl_train = nfl.query('year <> 2018')
#nfl_test = nfl.query('year == 2018')
#train_test_split(nfl, test_size=0.3, random_state=73)

from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error,  r2_score

from math import sqrt

lrm = LinearRegression().fit(X=nfl_train[ml_X_cols], y=nfl_train[ml_y_pick])
nfl_test['linreg_y_pred'] = lrm.predict(nfl_test[ml_X_cols])
lrm_score = lrm.score(X=nfl_train[ml_X_cols], y=nfl_train[ml_y_pick])
lrm_rms = sqrt(mean_squared_error(nfl_test[ml_y_pick], nfl_test['linreg_y_pred']))
lrm_r2s = r2_score(nfl_test[ml_y_pick], nfl_test['linreg_y_pred'])
print(lrm_score)
print(lrm_rms)
print(lrm_r2s)


from sklearn.tree import DecisionTreeRegressor
dtrm = DecisionTreeRegressor(max_depth=10).fit(X=nfl_train[ml_X_cols], y=nfl_train[ml_y_pick])
nfl_test['dtreg_y_pred'] = dtrm.predict(nfl_test[ml_X_cols])
dtrm_rms = sqrt(mean_squared_error(nfl_test[ml_y_pick], nfl_test['dtreg_y_pred']))
dtrm_r2s = r2_score(nfl_test[ml_y_pick], nfl_test['dtreg_y_pred'])
print(dtrm_score)
print(dtrm_rms)
print(dtrm_r2s)



from sklearn import linear_model

lasso = linear_model.Lasso(alpha=0.1).fit(X=nfl_train[ml_X_cols], y=nfl_train[ml_y_pick])
nfl_test['lasso_y_pred'] = lasso.predict(nfl_test[ml_X_cols])
lasso_score = lasso.score(X=nfl_train[ml_X_cols], y=nfl_train[ml_y_pick])
lasso_rms = sqrt(mean_squared_error(nfl_test[ml_y_pick], nfl_test['lasso_y_pred']))
lasso_r2s = r2_score(nfl_test[ml_y_pick], nfl_test['lasso_y_pred'])
print(lasso_score)
print(lasso_rms)
print(lasso_r2s)
###################### TRY WITH SCALING  !  First time doing this

from sklearn.preprocessing import StandardScaler
scaler = StandardScaler()
scaler.fit(nfl[ml_X_cols])
lrm = LinearRegression().fit(X=scaler.transform(nfl_train[ml_X_cols]), y=nfl_train[ml_y_pick])
nfl_test['linreg_ss_y_pred'] = lrm.predict(scaler.transform(nfl_test[ml_X_cols]))

# K that's good - that worked.... values are the same.
# Good to know for future tests here (decision tree and kmeans etc)




from sklearn.linear_model import LogisticRegression
lr = LogisticRegression(penalty = 'l1', C = .1,random_state = 1)
nfl_model = lr.fit(X=nfl_train[ml_X_cols],y=nfl_train.first_round_pick)
# Get predictions for the test set
nfl_test["logreg_pred"] = nfl_model.predict(X=nfl_test[ml_X_cols])

from sklearn.metrics import accuracy_score
print("Accuracy Score - {}".format(accuracy_score(nfl_test["first_round_pick"], nfl_test["logreg_pred"])))
print("Score - {}".format(lr.score(X=nfl_test[ml_X_cols],y=nfl_test.first_round_pick)))
nfl_sumr = nfl_test.groupby(["logreg_pred","first_round_pick"]).size()

from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
print(classification_report(y_true=nfl_test['first_round_pick'], y_pred=nfl_test['logreg_pred']))
print(confusion_matrix(y_true=nfl_test['first_round_pick'], y_pred=nfl_test['logreg_pred']))



from sklearn.grid_search import GridSearchCV

param_grid = {
    'penalty': ['l1', 'l2'],
    'C': [0.001,0.01,0.1,1,10,100,1000]
    }

# Doing this with 2 folds because I can't remember why
lr_grid = GridSearchCV(estimator=lr, param_grid=param_grid, cv=2)  
lr_grid.fit(X=nfl_train[ml_X_cols],y=nfl_train.first_round_pick)

# Here is all the scores in a dictionary sort of thing
print("Logistic Reg - grid scores {} ".format(lr_grid.grid_scores_))


# 4. See which is the best value for the Hyperparameter, using the accuracy score
print("Logistic Regression best param = {}".format( lr_grid.best_params_))
print("Logistic Regression best score = {}".format( lr_grid.best_score_))
print("Logistic Regression best estimator = {}".format( lr_grid.best_estimator_))

from sklearn import tree
dtc = tree.DecisionTreeClassifier(max_depth=20)
dtm = dtc.fit(X=nfl_train[ml_X_cols], y=nfl_train['first_round_pick'])
nfl_test['dt_pred'] = dtm.predict(nfl_test[ml_X_cols])

from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
print(classification_report(y_true=nfl_test['first_round_pick'], y_pred=nfl_test['dt_pred']))
print(confusion_matrix(y_true=nfl_test['first_round_pick'], y_pred=nfl_test['dt_pred']))


dtc2 = tree.DecisionTreeClassifier(max_depth=20)
dtm2 = dtc2.fit(X=scaler.transform(nfl_train[ml_X_cols]), y=nfl_train['first_round_pick'])
nfl_test['dt_ss_y_pred'] = dtm2.predict(scaler.transform(nfl_test[ml_X_cols]))

clf = tree.DecisionTreeClassifier()
parameters = {'max_features': ['log2', 'sqrt','auto'], 
              'criterion': ['entropy', 'gini'],
              'max_depth': [2, 3, 5, 10, 15, 20], 
              'min_samples_split': [2, 3, 5],
              'min_samples_leaf': [1,5,8]
             }

from sklearn.model_selection import GridSearchCV
grid_obj = GridSearchCV(clf, parameters, cv=2)
grid_obj = grid_obj.fit(X=nfl_train[ml_X_cols],y=nfl_train.first_round_pick)

print("Decision Tree - best performing {} ".format(clf))
print("Decision Tree - best performing {} ".format(clf.score))

print("Decision Tree - grid info {} ".format(grid_obj.grid_scores_))

clf = grid_obj.best_estimator_

clf.fit(X=nfl_train[ml_X_cols],y=nfl_train.first_round_pick)

nfl_test['dt_best_pred'] = clf.predict(X=nfl_test[ml_X_cols])

from sklearn.naive_bayes import GaussianNB
nbc = GaussianNB()
nbm = nbc.fit(X=nfl_train[ml_X_cols],y=nfl_train.first_round_pick)
nfl_test['nb_pred'] = nbm.predict(nfl_test[ml_X_cols])
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
print(classification_report(y_true=nfl_test['first_round_pick'], y_pred=nfl_test['nb_pred']))
print(confusion_matrix(y_true=nfl_test['first_round_pick'], y_pred=nfl_test['nb_pred']))

from sklearn.cluster import KMeans

est = KMeans(5)  # 4 clusters
est.fit(scaler.transform(nfl[ml_X_cols]))
nfl['kmeans_5'] = est.predict(scaler.transform(nfl[ml_X_cols]))

from sklearn.cluster import KMeans

est = KMeans(4)  # 4 clusters
est.fit(scaler.transform(nfl[ml_X_cols]))
nfl['kmeans_4'] = est.predict(scaler.transform(nfl[ml_X_cols]))

from sklearn.cluster import MeanShift
est = MeanShift(bandwidth=10)  # 4 clusters
est.fit(scaler.transform(nfl[ml_X_cols]))
nfl['meanshift'] = est.predict(scaler.transform(nfl[ml_X_cols]))

from sklearn.cluster import DBSCAN
dbmodel = DBSCAN(eps=5, min_samples=5).fit(scaler.transform(nfl[ml_X_cols]))
nfl['dbscan'] = dbmodel.labels_
