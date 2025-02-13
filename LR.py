import numpy as np
import pandas as pd
from sklearn.linear_model import Lasso
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import r2_score, mean_squared_error


tkw_data = pd.read_csv('', index_col='sample_name')
y = tkw_data['real_TKW']

drone_features = pd.read_csv('', index_col='sample_name')
X = drone_features


scaler_X = StandardScaler()
X_scaled = scaler_X.fit_transform(X)


X_train, X_test, y_train, y_test = train_test_split(X_scaled, y, test_size=0.2, random_state=42)


lasso = Lasso()

param_grid = {'alpha': np.logspace(-4, 4, 20)} 
grid = GridSearchCV(lasso, param_grid, cv=5, scoring='neg_mean_squared_error')
grid.fit(X_train, y_train)


best_alpha = grid.best_params_['alpha']
best_lasso = grid.best_estimator_  
test_score = mean_squared_error(y_test, best_lasso.predict(X_test))
print("Optimal alpha:", best_alpha)
print("Test RMSE:", np.sqrt(test_score))
print("Test R²:", r2_score(y_test, best_lasso.predict(X_test)))


y_pred_all = best_lasso.predict(X_scaled)


predicted_tkw_df = pd.DataFrame({'Predicted_TKW': y_pred_all}, index=X.index)
predicted_tkw_df.to_csv('predicted_tkw_all_samples_lasso.csv')


print(predicted_tkw_df)