import pandas as pd
from pandas import json_normalize
from pandas import DataFrame as df
import numpy as np
import json
import matplotlib.pyplot as plt
# %matplotlib inline
import sklearn
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor, AdaBoostRegressor, GradientBoostingRegressor, BaggingRegressor
from sklearn.neighbors import KNeighborsRegressor
from sklearn.neural_network import MLPRegressor
from sklearn import preprocessing
import imblearn
from imblearn.over_sampling import RandomOverSampler
import joblib
from joblib import dump, load

input_path = "E:/GSD/2022 Summer/RA/location-analysis/google street view images/"
csv = input_path + "all_image_features.csv"
all =  pd.read_csv(csv)
print(all)
print(len(all))

def adjusted_r2(r2,k,n):
  adj_r2_score =  1 - ((1-r2)*(n-1)/(n-k-1))
  return adj_r2_score

all_se = all[['built_score','paved_score','sky_score','terrain_score','vegetation_score','pole_score','median_income_E','gini_index_E','pct_rental_E','pct_sf_homes_E','annual_loans_per_sf_home','annual_loans_per_mf_home', 'income_ratio_sf','pct_sf_purchase','pct_sf_refi','pct_sf_rehab','pct_sf_cashout','mean_rate']]
print(len(all_se[all_se.isnull().T.any()]),"rows contain NA.")
select = all_se.dropna()
print(len(select))
  
y_ros = np.array(select)[:,8]
X_ros = np.array(select) [:,0:6]
x_train, x_test, y_train, y_test = train_test_split(X_ros, y_ros ,test_size = 0.15, random_state=5)

# sc = StandardScaler()
# X_train = sc.fit_transform(x_train)
# X_test = sc.transform(x_test)

clf_rf = RandomForestRegressor(n_estimators = 20)
rf = clf_rf.fit (x_train, y_train)
y_pred_rf = rf.predict(x_test)
r2_rf = r2_score(y_test, y_pred_rf)
adj_r2_rf = adjusted_r2(r2_rf,X_ros.shape[1]-1,len(x_test))
print('Random Forest R2 =',r2_rf)
print('Random Forest Adj R2 =,', adj_r2_rf)
print('Random Forest Mean Squared Error =', mean_squared_error(y_test, y_pred_rf))

clf_knn = KNeighborsRegressor()
knn = clf_knn.fit (x_train, y_train)
y_pred_knn = knn.predict(x_test)
r2_knn = r2_score(y_test, y_pred_knn)
adj_r2_knn = adjusted_r2(r2_knn,X_ros.shape[1]-1,len(x_test))
print('KNN R2 =',r2_knn)
print('KNN Adj R2 =,', adj_r2_knn)
print('KNN Mean Squared Error =', mean_squared_error(y_test, y_pred_knn))

clf_svm = SVR()
svm = clf_svm.fit(x_train, y_train)
y_pred_svm = svm.predict(x_test)
r2_svm = r2_score(y_test, y_pred_svm)
adj_r2_svm = adjusted_r2(r2_svm,X_ros.shape[1]-1,len(x_test))
print('SVR R2 =',r2_svm)
print('SVR Adj R2 =,', adj_r2_svm)
print('SVR Mean Squared Error =', mean_squared_error(y_test, y_pred_svm))

clf_linr = LinearRegression()
linr = clf_linr.fit (x_train, y_train)
y_pred_linr = linr.predict(x_test)
r2_linr = r2_score(y_test, y_pred_linr)
adj_r2_linr = adjusted_r2(r2_linr,X_ros.shape[1]-1,len(x_test))
print('Linear Regression R2 =',r2_linr)
print('Linear Regression Adj R2 =,', adj_r2_linr)
print('Linear Regression Mean Squared Error =', mean_squared_error(y_test, y_pred_linr))

clf_rid = Ridge()
rid = clf_rid.fit (x_train, y_train)
y_pred_rid = rid.predict(x_test)
r2_rid = r2_score(y_test, y_pred_rid)
adj_r2_rid = adjusted_r2(r2_rid,X_ros.shape[1]-1,len(x_test))
print('Ridge R2 =',r2_rid)
print('Ridge Adj R2 =,', adj_r2_rid)
print('Ridge Mean Squared Error =', mean_squared_error(y_test, y_pred_rid))

clf_las = Lasso()
las = clf_las.fit (x_train, y_train)
y_pred_las = las.predict(x_test)
r2_las = r2_score(y_test, y_pred_las)
adj_r2_las = adjusted_r2(r2_las,X_ros.shape[1]-1,len(x_test))
print('Lasso R2 =',r2_las)
print('Lasso Adj R2 =,', adj_r2_las)
print('Lasso Mean Squared Error =', mean_squared_error(y_test, y_pred_las))

clf_mlp = MLPRegressor()
mlp = clf_mlp.fit (x_train, y_train)
y_pred_mlp = mlp.predict(x_test)
r2_mlp = r2_score(y_test, y_pred_mlp)
adj_r2_mlp = adjusted_r2(r2_mlp,X_ros.shape[1]-1,len(x_test))
print('MLP Regressor R2 =',r2_mlp)
print('MLP Regressor Adj R2 =,', adj_r2_mlp)
print('MLP Regressor Mean Squared Error =', mean_squared_error(y_test, y_pred_mlp))

###
clf_ada = AdaBoostRegressor()
ada = clf_ada.fit (x_train, y_train)
y_pred_ada = ada.predict(x_test)
r2_ada = r2_score(y_test, y_pred_ada)
adj_r2_ada = adjusted_r2(r2_ada,X_ros.shape[1]-1,len(x_test))
print('Ada Boost Regressor R2 =',r2_ada)
print('Ada Boost Regressor Adj R2 =,', adj_r2_ada)
print('Ada Boost Regressor Mean Squared Error =', mean_squared_error(y_test, y_pred_ada))

clf_gra = GradientBoostingRegressor()
gra = clf_gra.fit (x_train, y_train)
y_pred_gra = gra.predict(x_test)
r2_gra = r2_score(y_test, y_pred_gra)
adj_r2_gra = adjusted_r2(r2_gra,X_ros.shape[1]-1,len(x_test))
print('Gradient Boosting Regressor R2 =',r2_gra)
print('Gradient Boosting Regressor Adj R2 =,', adj_r2_gra)
print('Gradient Boosting Regressor Mean Squared Error =', mean_squared_error(y_test, y_pred_gra))

clf_bag = BaggingRegressor()
bag = clf_bag.fit (x_train, y_train)
y_pred_bag = bag.predict(x_test)
r2_bag = r2_score(y_test, y_pred_bag)
adj_r2_bag = adjusted_r2(r2_bag,X_ros.shape[1]-1,len(x_test))
print('Bagging Regressor R2 =',r2_bag)
print('Bagging Regressor Adj R2 =,', adj_r2_bag)
print('Bagging Regressor Mean Squared Error =', mean_squared_error(y_test, y_pred_bag))
