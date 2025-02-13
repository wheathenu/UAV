import numpy as np
import pandas as pd
from sklearn.linear_model import Ridge
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import r2_score, mean_squared_error


tkw_data = pd.read_csv('', index_col='sample_name')
y = tkw_data['real_TKW']

drone_features = pd.read_csv('', index_col='sample_name')
X = drone_features


X = X.values  # (n_samples, n_features)
y = y.values  # (n_samples,)


scaler_X = StandardScaler()
X_scaled = scaler_X.fit_transform(X)


ridge = Ridge()
param_grid = {'alpha': np.logspace(-3, 3, 13)}
grid = GridSearchCV(ridge, param_grid, cv=5, scoring='neg_mean_squared_error')
grid.fit(X_scaled, y)


best_alpha = grid.best_params_['alpha']
print("Optimal alpha:", best_alpha)


best_ridge = grid.best_estimator_  
y_pred = best_ridge.predict(X_scaled)


predicted_tkw_df = pd.DataFrame({'Predicted_TKW': y_pred}, index=tkw_data.index)


predicted_tkw_df.to_csv('predicted_tkw_all_data.csv')


print(predicted_tkw_df)