# Methods

## Data Cleaning

We used the BP and cholesterol variables from the package `cardioStatsUSA`. For additional variables we pull the NHANES all continuous data from 2001-2020 cycles including following criteria: Dietary, Questionnaire, Examination and Laboratory. We excluded the first two cycles 1999-2001 due to to changed of survey design and difference in questions/measures. Subjects are restricted to the participants included in `library(cardioStatsUSA)`. We kept variables that spanned for at least five cycles. We extratced variables based on the the data avaiability and relevance to the outcome interested. Extra interested variables were recoded such as birth cohort, etc. Detailed list of extracted and recoded variables can be find [insert link].

## Data PreProcessing

### Missing Imputations
We dropped variables with more than 50% of missingness and performed multiple imputation with CART(Classification and Regressio Trees) regressor to obtain the complete dataset.

### Standardization
Continuous variables 

### Survey Weight

## Feature Selection/Important Variable Extraction

For all algorithms below we used a three stage approach:

* fit three models on the outcome: a) before 2013 b) after 2013 and c) overall

* compare the features selected by these three models and the corresponding direction/significance

* compare the direction of the features. if they have the same direction before and after, then check the trend of this feature very likely there is a change of the feature itself over the year. if they have different direction, then it means the relationship between the outcome and feature changed after 2013

### Logistic Regression/Linear Regression with LASSO

### Random Forest

### XGBoost

### TabNet

## Compare the extratced Variables
within each of the algorithms and between

### Final List of variables
three rank lists for each of the methods

## Model buidling



### Stepwise backward elimination