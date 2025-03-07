rm(list = ls())
load('/Users/taehyo/Library/CloudStorage/Dropbox/NYU/Research/Research/Code/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/dat_hyp_final_2013_2020.RData')

library(mice)
library(tidyr)
library(tidyverse)
library(kableExtra)
library(survey)
library(dplyr)
library(jtools)

# Define datasets
data_list_years <- list(
  "2013" = dat_hyp_2013_final,
  "2015" = dat_hyp_2015_final,
  "2017" = dat_hyp_2017_final
)

# Store results
pooled_results_all_years <- list()

for (year in names(data_list_years)) {
  data_list <- data_list_years[[year]]
  models <- list()
  
  for (i in seq_along(data_list)) {
    data <- data_list[[i]]
    # Extract all variable names, excluding survey design variables
    predictors <- setdiff(names(data), c("SEQN", "htn_aware","htn_resistant_jnc7","cc_smoke.1", "bp_med_use", "bp_med_n_pills", "cc_acr_gteq30", "bp_control_jnc7", "svy_weight_mec", "svy_strata", "svy_psu"))
    
    # Construct formula dynamically
    formula <- as.formula(paste("bp_control_jnc7 ~", paste(predictors, collapse = " + ")))
    
    # Define survey design
    svydesign_obj <- svydesign(
      id = ~svy_psu, 
      strata = ~svy_strata,
      weights = ~svy_weight_mec,  
      data = data,
      nest = TRUE
    )
    
    # Fit model
    models[[i]] <- svyglm(formula, design = svydesign_obj, family = quasibinomial(link = "logit"))
    
    # models[[i]] <- glm(formula, data = data, family = "quasibinomial")
  }
  
  # Pool results
  mira_models <- as.mira(models)
  pooled_results <- pool(mira_models)
  
  # Summarize results
  pooled_summary <- summary(pooled_results)
  
  # Format into dataframe
  pooled_results_df <- data.frame(
    term = pooled_summary$term,
    estimate = pooled_summary$estimate,
    std.error = pooled_summary$std.error,
    statistic = pooled_summary$statistic,
    p.value = pooled_summary$p.value,
    conf.low = pooled_summary$estimate - 1.96 * pooled_summary$std.error,
    conf.high = pooled_summary$estimate + 1.96 * pooled_summary$std.error,
    significant = pooled_summary$p.value < 0.05  # TRUE/FALSE for significance
  )
  
  # Store results for the year
  pooled_results_all_years[[year]] <- pooled_results_df
}

# Print final results
for (year in names(pooled_results_all_years)) {
  file_name <- paste0("/Users/taehyo/Library/CloudStorage/Dropbox/NYU/Research/Research/Code/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/", year, "glm.csv")
  write.csv(pooled_results_all_years[[year]], file = file_name, row.names = FALSE)
  
  cat("\nSaved results for", year, "to", file_name, "\n")
}

# Check vif 
X <- model.matrix(models[[1]])[, -1]  # Removes intercept column
# Extract survey weights
w <- dat_hyp_2017_final[[1]]$svy_weight_mec  
# Compute VIF
print(svyvif(models[[1]], X, w))










