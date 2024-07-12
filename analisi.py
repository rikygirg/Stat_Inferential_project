import pandas as pd
import statsmodels.api as sm
import numpy as np

# Load the dataset
stars = pd.read_csv("new_stars.csv")

# Filter the data to include only 'Main Sequence' and 'Brown Dwarf'
filtered_stars = stars[(stars['Type'] == "Main Sequence") | (stars['Type'] == "Brown Dwarf")]

# Log transform the columns
stars['log_L'] = np.log(stars['L'])
stars['log_R'] = np.log(stars['R'])
stars['log_T'] = np.log(stars['Temperature'])

# Define the dependent and independent variables
X = stars[['log_R', 'log_T']]
y = stars['log_L']

# Add a constant to the model (intercept)
X = sm.add_constant(X)

# Fit the linear model
model = sm.OLS(y, X).fit()

# Print the summary of the model
print(model.summary())
