rm(list = ls())
cat("\014")

# Load the required data
# load("/Users/cdp4029/Downloads/dat_train_test_bootstrapped_2013_2020 (1).RData")

# Install required packages if not already installed
# install.packages("survey")
# install.packages("glmnet")
# install.packages("dplyr")
# install.packages("Matrix")

# Load necessary libraries
library(survey)
library(glmnet)
library(dplyr)
library(Matrix)
library(svyVarSel)  
library(pROC)
# Recode the outcome variable ("No" = 0, "Yes" = 1)


# Function to process each dataset
process_dataset <- function(dataset, outcome_var, predictor_vars, alpha = 0.5, k = 5, R = 1) {
  # Recode the outcome variable ("No" = 0, "Yes" = 1)
  dataset[[outcome_var]] <- ifelse(dataset[[outcome_var]] == "No", 0, 1)
  
  # Define the survey design
  survey_design <- svydesign(
    id = ~svy_psu,
    strata = ~svy_strata,
    weights = ~svy_weight_mec,
    data = dataset,
    nest = TRUE
  )
  
  # Fit the Elastic Net model using welnet
  elastic_net_model <- welnet(
    data = dataset,
    col.y = outcome_var,
    col.x = predictor_vars,
    family = "binomial",
    alpha = alpha,
    cluster = "svy_psu",
    strata = "svy_strata",
    weights = "svy_weight_mec",
    method = "dCV",
    k = k,
    R = R
  )
  
  # Extract feature importance
  feature_importance <- elastic_net_model$model$min$beta
  feature_importance <- as.data.frame(as.matrix(feature_importance))
  feature_importance$Feature <- rownames(feature_importance)
  colnames(feature_importance) <- c("Importance", "Feature")
  
  return(list(model = elastic_net_model, feature_importance = feature_importance))
}


# Function to apply process_dataset to nested lists
process_nested_list <- function(nested_list, outcome_var, predictor_vars) {
  lapply(nested_list, function(inner_list) {
    lapply(inner_list, function(dataset) {
      process_dataset(dataset, outcome_var, predictor_vars)
    })
  })
}

# Process datasets for each year

# Define the outcome variable and predictor variables
outcome_var <- "bp_control_jnc7"
columns_to_exclude <- c('bp_control_jnc7', 'SEQN', 
                        "svy_strata", "svy_weight_mec", "svy_psu")
all_vars <- colnames(dat_hyp_2013_samp[[1]][[1]])
predictor_vars <- setdiff(all_vars, columns_to_exclude)
elastic_net_models_2013 <- process_nested_list(dat_hyp_2013_samp, outcome_var, predictor_vars)

all_vars <- colnames(dat_hyp_2015_samp[[1]][[1]])
predictor_vars <- setdiff(all_vars, columns_to_exclude)
elastic_net_models_2015 <- process_nested_list(dat_hyp_2015_samp, outcome_var, predictor_vars)

all_vars <- colnames(dat_hyp_2017_samp[[1]][[1]])
predictor_vars <- setdiff(all_vars, columns_to_exclude)
elastic_net_models_2017 <- process_nested_list(dat_hyp_2017_samp, outcome_var, predictor_vars)

# Function to extract and combine feature importance
aggregate_feature_importance <- function(elastic_net_models) {
  # Flatten the nested list
  flattened_models <- unlist(elastic_net_models, recursive = FALSE)
  
  # Extract feature importance from each model
  feature_importance_list <- lapply(flattened_models, function(model_info) {
    as.data.frame(t(model_info$feature_importance))[-2,]
  })
  
  # Combine feature importance into a single data frame
  combined_importance <- bind_rows(feature_importance_list)
  
  # Calculate mean importance for each feature
  mean_importance <- as.data.frame(t(combined_importance %>% 
    mutate(across(where(is.character), as.numeric, na.rm = TRUE)) %>% 
    summarise(across(where(is.numeric), mean, na.rm = TRUE))))
  
  mean_importance$Feature <- rownames(mean_importance)
           
  
  # Identify top 25 features based on mean importance
  top_features <- mean_importance$Feature[(order(abs(mean_importance$V1), decreasing = TRUE))[1:25]]
  
  return(top_features)
}

# Aggregate feature importance for each year
top_features_2013 <- aggregate_feature_importance(elastic_net_models_2013)
top_features_2015 <- aggregate_feature_importance(elastic_net_models_2015)
top_features_2017 <- aggregate_feature_importance(elastic_net_models_2017)

# Function to evaluate model performance
evaluate_model_performance <- function(train_data, test_data, outcome_var, predictor_vars, alpha = 0.5, k = 5, R = 1) {
  # Recode the outcome variable
  train_data[[outcome_var]] <- ifelse(train_data[[outcome_var]] == "No", 0, 1)
  test_data[[outcome_var]] <- ifelse(test_data[[outcome_var]] == "No", 0, 1)
  
  # Define the survey design for training data
  train_design <- svydesign(
    id = ~svy_psu,
    strata = ~svy_strata,
    weights = ~svy_weight_mec,
    data = train_data,
    nest = TRUE
  )
  
  # Fit the Elastic Net model using welnet
  elastic_net_model <- welnet(
    data = train_data,
    col.y = outcome_var,
    col.x = predictor_vars,
    family = "binomial",
    alpha = alpha,
    cluster = "svy_psu",
    strata = "svy_strata",
    weights = "svy_weight_mec",
    method = "dCV",
    k = k,
    R = R
  )
  
  # Predict on test data
  x_test <- as.matrix(test_data[, predictor_vars])
  coef <- c(elastic_net_model$model$min$a0, elastic_net_model$model$min$beta[,'s0']) 
  coef <- matrix(coef, ncol = 1)
  predictions <- cbind(1,x_test) %*% coef
  
  predictions <- as.numeric(predictions > 0)
  
  # Evaluate performance: Calculate AUC
  y_test <- test_data[[outcome_var]]
  roc_curve <- roc(y_test, predictions)
  auc_value <- auc(roc_curve)
  
  return(auc_value)
}

# Evaluate performance for each dataset
all_vars <- colnames(dat_hyp_2013_samp[[1]][[1]])
predictor_vars <- setdiff(all_vars, columns_to_exclude)
auc_2013 <- lapply(seq_along(dat_hyp_2013_std_train), function(i){
  evaluate_model_performance(dat_hyp_2013_std_train[[i]], dat_hyp_2013_std_test[[i]], outcome_var, predictor_vars)}) 

auc_2013 <- do.call(rbind,auc_2013) 
auc_2013_mean <- c(mean(auc_2013), sd(auc_2013))

all_vars <- colnames(dat_hyp_2015_samp[[1]][[1]])
predictor_vars <- setdiff(all_vars, columns_to_exclude)
auc_2015 <- lapply(seq_along(dat_hyp_2015_std_train), function(i){
  evaluate_model_performance(dat_hyp_2015_std_train[[i]], dat_hyp_2015_std_test[[i]], outcome_var, predictor_vars)}) 

auc_2015 <- do.call(rbind,auc_2015) 
auc_2015_mean <- c(mean(auc_2015), sd(auc_2015))

all_vars <- colnames(dat_hyp_2017_samp[[1]][[1]])
predictor_vars <- setdiff(all_vars, columns_to_exclude)
auc_2017 <- lapply(seq_along(dat_hyp_2017_std_train), function(i){
  evaluate_model_performance(dat_hyp_2017_std_train[[i]], dat_hyp_2017_std_test[[i]], outcome_var, predictor_vars)}) 

auc_2017 <- do.call(rbind,auc_2017) 
auc_2017_mean <- c(mean(auc_2017), sd(auc_2017))
# Print AUC values
cat("AUC for 2013 data:", auc_2013_mean, "\n")
cat("AUC for 2015 data:", auc_2015_mean, "\n")
cat("AUC for 2017 data:", auc_2017_mean, "\n")

save(auc_2013, auc_2015, auc_2017,auc_2013_mean, auc_2015_mean, auc_2017_mean, file = "data/cleaned/2013_to_2023_cleaned/auc_values_ES.RData")
save(top_features_2013, top_features_2015, top_features_2017, file = "data/cleaned/2013_to_2023_cleaned/top_features_ES.RData")
