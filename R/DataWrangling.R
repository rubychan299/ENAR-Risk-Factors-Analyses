
# loading data####

library(cardioStatsUSA)
data(nhanes_data)
# for blood pressure and hypertension sub-population
summary(nhanes_data$svy_strata[nhanes_data$svy_subpop_htn == 1])
table(nhanes_data$svy_subpop_htn, nhanes_data$svy_subpop_chol)

# for lipids and cholesterol sub-population
nhanes_data[svy_subpop_chol == 1]