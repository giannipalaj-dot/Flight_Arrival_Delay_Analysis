Project Overview

This project explores factors associated with commercial flight arrival
delays and builds a multiple linear regression model to estimate arrival
delay time in minutes.

The analysis uses flight-level operational variables, including baggage
loading time, number of flights, late-arriving aircraft, airport
distance, weather, security, cleaning, fueling, and support crew
availability. The goal is to examine relationships among these
variables, compare regression specifications, and evaluate predictive
performance on data that was held out from model training.

Objectives
- Inspect the dataset and identify missing values.
- Summarize the distribution of arrival delays.
- Examine correlations between arrival delays and numeric predictors.
- Split the observations into training and test sets.
- Fit and compare full and reduced multiple linear regression models.
- Evaluate predictions using RMSE, MAE, and R-squared.
- Review regression diagnostics and potential modeling issues.

Tools and Packages
The project is written in R and uses these packages:
- psych --- descriptive statistics
- moments --- Jarque-Bera normality test
- caTools --- training/test partition
- stargazer --- formatted regression output
- car --- regression diagnostics, including Durbin-Watson,
  non-constant variance, and VIF tests

Methodology
1. Data review: Inspect variable types, sample rows, column names,
   and missing values.
2. Exploratory analysis: Summarize arrival delays, test the target
   variable's distribution, and create histograms and boxplots.
3. Correlation analysis: Calculate correlations among numeric
   variables and visualize selected relationships with arrival delay.
4. Data partitioning: Use a reproducible 70% training / 30% test
   split
5. Modeling: Fit a full multiple linear regression model and a
   reduced model using a selected set of predictors.
6. Evaluation: Compare training and test performance using RMSE,
   MAE, and R-squared. Plot actual versus predicted delays and the test
   residual distribution.
7. Diagnostics: Review standard regression diagnostic plots, the
   Durbin-Watson test, the non-constant variance test, and variance
   inflation factors.

Evaluation Metrics
- RMSE (Root Mean Squared Error): Measures the typical size of
  prediction errors while penalizing larger errors more heavily. It is
  reported in minutes.
- MAE (Mean Absolute Error): Measures the average absolute
  difference between actual and predicted arrival delays. It is
  reported in minutes.
- R-squared: Measures the proportion of variation in arrival
  delays explained by the model. The test-set R-squared is calculated
  on observations not used to fit the model.
