# ENAR-Risk-Factors-Analyses
ENAR DataFest 2024

Team Members: Taehyo Kim, Yuyu(Ruby) Chen, Cindy Patippe 

# Proposal for Analysis of Blood Pressure Control Trends Among US Adults

## I. Introduction
A. Problem Statement
Controlling blood pressure (BP) is crucial for reducing cardiovascular disease risk. However, the prevalence of BP control among US adults with hypertension has been declining since 2013. This study aims to investigate potential causes or correlates of this trend using publicly available data, thereby contributing to the development of effective interventions.

## II. Data Description
We will utilize data from the US National Health and Nutrition Examination Survey (NHANES) from 1999 to 2020. This dataset includes information on demographics, blood pressure levels, hypertension status, antihypertensive medication usage, and co-morbidities for 59,799 noninstitutionalized US adults.
## III. Approaches
A. Risk Factor Analysis
1. Trend Analysis of Feature Importance:
  - Analyze the significance and impact of different factors (demographics, medication usage, comorbidities, etc.) on BP control across different NHANES cycles/years.
  - Divide the dataset into two parts (before and after 2013) for exploratory data analysis
  - Use birth cohorts to assess blood pressure control among different generations (make a variable for this)
  - Visualizations on data (show something interesting)

2. Feature Selection at each cycle:
  - Select features based on their relevance and trend analysis.
  - Pay attention to the weights for the survey design (svy_strata, svy_weight_mec) use svydesign in R
  - Employ statistical methods to ensure robustness in feature selection.
  - Use machine learning models to determine feature importance and temporal trends. We will use logistic regression + spline (with ridge/lasso penalties), KNN, Naive bayes, random forest/gradient boosted model, TabNet + feature importance using SHAP or other variable importance methods (Captum · Model Interpretability for PyTorch))
  - Compare the extracted features by different methods for the pooled regression models

3. Divide and Conquer using Spline:
  - Conduct linear spline regression (pooled before and after 2013?) with selected covariates on the aggregated dataset.
  - Include ‘year’ as an additional covariate to understand temporal changes.
  - Aim to predict BP control outcomes and identify key contributing factors.

B. Comparative Analysis with Prior Studies
• Cadmium and Blood Pressure Interaction:
    Reference: “Cadmium affects blood pressure and negatively interacts with obesity: Findings from NHANES 1999–2014.”
    Analyze the interaction between cadmium exposure, obesity, and BP trends.
• Phthalates Exposure and Hyperuricemia:
    Reference: “Relationship between phthalates exposures and hyperuricemia in U.S. general population, a multi-cycle study of NHANES 2007–2016.”
    Examine if similar methodology and findings can be applied to BP control.
• Gender-Specific Trends:
    Reference: “Blood Pressure and Cardiovascular Disease Mortality Among US Adults: A Sex-Stratified Analysis, 1999–2019.”
    Assess gender differences in BP control and related cardiovascular outcomes.
Need to add more 

V. Expected Outcomes
• Identification of key factors contributing to the decline in BP control.
• Insights into the interaction of various risk factors and their temporal changes.
• Recommendations for targeted interventions to improve BP control among hypertensive patients.

VI. Conclusion
This study aims to provide valuable insights into the worsening trend of BP control among US adults with hypertension. By leveraging comprehensive data analysis and comparing findings with existing research, we hope to contribute to the development of effective strategies to combat hypertension and associated cardiovascular risks.





