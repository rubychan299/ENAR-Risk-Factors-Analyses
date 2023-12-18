# Data Analyses Work Flow

## Data Cleaning
Done with the data extraction

next step: merge the data into one data frame

## Data Preprocessing
Missing data - Imputation
Standardization
data Imbalance
survey weight

 - create variables/outcome if necessary

## Feature selection
1) fit three linear lasso regressions on the outcome: a) before 2013 b) after 2013 and c) overall 

2) compare the features selected by these three models and the corresponding direction/significance

3) compare the direction of the features. if they have the same direction before and after, then check the trend of this feature very likely there is a change of the feature itself over the year. if they have different direction, then it means the relationship between the outcome and feature changed after 2013

### Logistic Regression/Linear Regression with LASSO

### Stepwise backward elimination

### Naive Bayes

### KNN

### Random Forest

### TabNet

## Compare the extratced Variables
within each of the algorithms and between

### Final List of variables
three rank lists for each of the methods

## Model buidling




