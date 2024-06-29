# Credit Scoring Analysis

This repository contains R code for analyzing credit scoring data sourced from [Kaggle](https://www.kaggle.com/kashnitsky/mlcourse?select=credit_scoring_sample.csv).
The analysis includes data cleaning, exploratory data analysis, logistic regression modeling, and decision tree modeling for predicting credit defaults.


## Scripts Overview

### exploration_des_donnees script: Data Cleaning and exploratory analysis

#### Overview

The first script focuses on initial data cleaning, renaming variables and exploratory data analysis.

#### Key Steps

1. **Data Cleaning and Renaming:**
   - Renamed variables for clarity and consistency.
   - Removed observations with missing values or outliers.

2. **Exploratory Data Analysis:**
   - Visualized the distribution of `Client_Qualite` using histograms.
   - Summary Statistics.
   - Grouped `age` into categories and visualized using boxplots.



### Modélisations script: Further Data Exploration and Decision Tree Modeling

#### Overview

The second script extends the analysis with additional exploratory data analysis, handling of extreme values, and building a decision tree model.

#### Key Steps

1. **Further Data Cleaning:**
   - Renamed variables for clarity and consistency.
   - Removed observations with missing values resulting in 36,420 clean observations.

2. **Handling Extreme Values:**
   - Removed extreme values for specific variables (`retard_30_59_jours`, `retard_60_89_jours`).

3. **Logistic Regression Modeling:**
   - Split data into training(70%) and test(30%) sets.
   - Built a logistic regression model (glm) predicting Client_Qualite (credit default) using explanatory variables.
   - Evaluated model performance using: Confusion Matrix, ROC Curve (Area Under the Curve = 0.8027), Sensitivity, Specificity, and Misclassification Rate.
   - stepwise AIC selection to validate the model,
   - odds ratios (exp(coef(m))) for interpretability,
   - Variance Inflation Factors (VIF) in order to assess multicollinearity and Wald tests for statistical significance.
    
3. **Decision Tree Modeling:**
   - Built a decision tree model (rpart) for predicting credit default `Client_Qualite`.
   - Evaluated model accuracy using test data (81.47%) and tuned hyperparameters.

## Conclusion

This repository provides comprehensive R scripts for analyzing credit scoring data, covering initial data cleaning, exploratory analysis, logistic regression, and decision tree modeling. 
These scripts aim to predict credit defaults based on various financial indicators, offering insights into model performance and interpretability.
