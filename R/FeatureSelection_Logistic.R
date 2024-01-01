# Load replicated data
load("dat_train_test_bootstrapped.RData")

# Access the list of replicated data
replicated_data_list <- dat_hyp_final_pre_samp

# Outcome variables
outcome_vars <- c("bp_control_jnc7", "bp_control_accaha", "bp_control_140_90", "bp_control_130_80")

# Columns to exclude
columns_to_exclude <- c("SEQN", "svy_psu", "svy_weight_mec", "svy_strata", "svy_year", "bp_control_jnc7", "bp_control_accaha", "bp_control_140_90", "bp_control_130_80")

# Function to exclude specified columns
exclude_columns <- function(dataset) {
  predictors <- dataset[, !colnames(dataset) %in% columns_to_exclude]
  return(predictors)
}

# Apply the function to each dataset in the list
replicated_data_list <- lapply(replicated_data_list, exclude_columns)

# Create an empty list to store results
all_results_pre <- list()

# Define a function to run the model
run_model <- function(outcome_var, train_data, weights) {
  outcome <- train_data[[outcome_var]]
  predictors <- exclude_columns(train_data)  
  lasso_model <- cv.glmnet(as.matrix(predictors), outcome, family = "binomial", alpha = 1, weights = weights)
  best_lambda <- lasso_model$lambda.min
  lasso_coef <- coef(lasso_model, s = best_lambda)
  selected_features <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
  return(list(selected_features = selected_features, lasso_coef = lasso_coef))
}
# Loop through outcomes and replications
for (outcome_var in outcome_vars) {
  replication_results_pre <- list()
  for (replication in 1:10) {
    # Extract training and testing sets for the current replication
    train_data <- dat_hyp_final_pre_train  
    test_data <- dat_hyp_final_pre_test  
    
    # Run the model for the current outcome and replication
    result <- run_model(outcome_var, train_data, train_data$svy_weight_mec)
    
    # Store the result
    replication_results_pre[[replication]] <- result
  }
  
  # Store results for the current outcome
  all_results_pre[[outcome_var]] <- replication_results_pre
}

# Save everything (modify the path accordingly)
save(all_results_pre, file = "/Users/cdp4029/Downloads/all_results_replicated_pre.RData")

# Compute mean and standard deviation of the feature importances
mean_importances_pre <- sapply(all_results_pre, function(outcome_result) {
  sapply(outcome_result, function(replication_result_pre) {
    mean(replication_result_pre$lasso_coef[, 1])
  })
})

sd_importances_pre <- sapply(all_results_pre, function(outcome_result) {
  sapply(outcome_result, function(replication_result_pre) {
    sd(replication_result_pre$lasso_coef[, 1])
  })
})

# Save mean and standard deviation into a CSV file
importances_stats_pre <- data.frame(mean_importance = mean_importances_pre, sd_importance = sd_importances_pre)
write.csv(importances_stats, file = "/Users/cdp4029/Downloads/importances_stats_replicated_pre.csv")

# Create ranking plots
install.packages("ggplot")
library(ggplot2)
for (outcome_var in outcome_vars) {
  # Extract feature names and coefficients
  coef_list_pre <- lapply(all_results_pre[[outcome_var]], function(replication_result_pre) replication_result_pre$lasso_coef[, 1])
  coef_df_pre <- data.frame(outcome = outcome_var, replication = rep(1:10, each = length(coef_list_pre)), do.call(rbind, coef_list_pre))
  
  # Plot
  ggplot(coef_df_pre, aes(x = reorder(rownames(coef_df_pre), -V1), y = V1)) +
    geom_bar(stat = "identity", fill = "blue", color = "black") +
    coord_flip() +
    labs(title = paste("LASSO Coefficients for Outcome(Pre):", outcome_var), x = "Coefficient", y = "Value") +
    theme_minimal()  # This line is part of the same line as labs()
}

#Post

# Access the list of replicated data
replicated_data_list_post <- dat_hyp_final_post_samp

# Outcome variables
outcome_vars <- c("bp_control_jnc7", "bp_control_accaha", "bp_control_140_90", "bp_control_130_80")

# Columns to exclude
columns_to_exclude <- c("SEQN", "svy_psu", "svy_weight_mec", "svy_strata", "svy_year", "bp_control_jnc7", "bp_control_accaha", "bp_control_140_90", "bp_control_130_80")

# Function to exclude specified columns
exclude_columns <- function(dataset) {
  predictors <- dataset[, !colnames(dataset) %in% columns_to_exclude]
  return(predictors)
}

# Apply the function to each dataset in the list
replicated_data_list_post <- lapply(replicated_data_list_post, exclude_columns)

# Create an empty list to store results
all_results_post <- list()

# Define a function to run the model
run_model <- function(outcome_var, train_data, weights) {
  outcome <- train_data[[outcome_var]]
  predictors <- exclude_columns(train_data)  
  lasso_model <- cv.glmnet(as.matrix(predictors), outcome, family = "binomial", alpha = 1, weights = weights)
  best_lambda <- lasso_model$lambda.min
  lasso_coef <- coef(lasso_model, s = best_lambda)
  selected_features <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
  return(list(selected_features = selected_features, lasso_coef = lasso_coef))
}
# Loop through outcomes and replications
for (outcome_var in outcome_vars) {
  replication_results_post <- list()
  for (replication in 1:10) {
    # Extract training and testing sets for the current replication
    train_data <- dat_hyp_final_post_train  
    test_data <- dat_hyp_final_post_test 
    
    # Run the model for the current outcome and replication
    result <- run_model(outcome_var, train_data, train_data$svy_weight_mec)
    
    # Store the result
    replication_results_post[[replication]] <- result
  }
  
  # Store results for the current outcome
  all_results_post[[outcome_var]] <- replication_results_post
}

# Save everything (modify the path accordingly)
save(all_results_post, file = "/Users/cdp4029/Downloads/all_results_replicated_post.RData")


# Compute mean and standard deviation of the feature importances
mean_importances_post <- sapply(all_results_post, function(outcome_result) {
  sapply(outcome_result, function(replication_result_post) {
    mean(replication_result_post$lasso_coef[, 1])
  })
})

sd_importances_post <- sapply(all_results_post, function(outcome_result) {
  sapply(outcome_result, function(replication_result_post) {
    sd(replication_result_post$lasso_coef[, 1])
  })
})

# Save mean and standard deviation into a CSV file
importances_stats_post <- data.frame(mean_importance = mean_importances_post, sd_importance = sd_importances_post)
write.csv(importances_stats_post, file = "/Users/cdp4029/Downloads/importances_stats_replicated_post.csv")

# Create ranking plots for "Post"
for (outcome_var in outcome_vars) {
  # Extract feature names and coefficients
  coef_list_post <- lapply(all_results_post[[outcome_var]], function(replication_result_post) replication_result_post$lasso_coef[, 1])
  coef_df_post <- data.frame(outcome = outcome_var, replication = rep(1:10, each = length(coef_list_post)), do.call(rbind, coef_list_post))
  
  # Plot
  ggplot(coef_df_post, aes(x = reorder(rownames(coef_df_post), -V1), y = V1)) +
    geom_bar(stat = "identity", fill = "blue", color = "black") +
    coord_flip() +
    labs(title = paste("LASSO Coefficients for Outcome(Post):", outcome_var), x = "Coefficient", y = "Value") +
    theme_minimal()  
}


#Overall

# Access the list of replicated data
replicated_data_list_overall <- dat_hyp_final_samp

# Outcome variables
outcome_vars <- c("bp_control_jnc7", "bp_control_accaha", "bp_control_140_90", "bp_control_130_80")

# Columns to exclude
columns_to_exclude <- c("SEQN", "svy_psu", "svy_weight_mec", "svy_strata", "svy_year", "bp_control_jnc7", "bp_control_accaha", "bp_control_140_90", "bp_control_130_80")

# Function to exclude specified columns
exclude_columns <- function(dataset) {
  predictors <- dataset[, !colnames(dataset) %in% columns_to_exclude]
  return(predictors)
}

# Apply the function to each dataset in the list
replicated_data_list_overall <- lapply(replicated_data_list_overall, exclude_columns)

# Create an empty list to store results
all_results_overall <- list()

# Define a function to run the model
run_model <- function(outcome_var, train_data, weights) {
  outcome <- train_data[[outcome_var]]
  predictors <- exclude_columns(train_data)  
  lasso_model <- cv.glmnet(as.matrix(predictors), outcome, family = "binomial", alpha = 1, weights = weights)
  best_lambda <- lasso_model$lambda.min
  lasso_coef <- coef(lasso_model, s = best_lambda)
  selected_features <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
  return(list(selected_features = selected_features, lasso_coef = lasso_coef))
}
# Loop through outcomes and replications
for (outcome_var in outcome_vars) {
  replication_results_overall <- list()
  for (replication in 1:10) {
    # Extract training and testing sets for the current replication
    train_data <- dat_hyp_final_train  
    test_data <- dat_hyp_final_test  
    
    # Run the model for the current outcome and replication
    result <- run_model(outcome_var, train_data, train_data$svy_weight_mec)
    
    # Store the result
    replication_results_overall[[replication]] <- result
  }
  
  # Store results for the current outcome
  all_results_overall[[outcome_var]] <- replication_results_overall
}

# Save everything (modify the path accordingly)
save(all_results_overall, file = "/Users/cdp4029/Downloads/all_results_replicated_overall.RData")


# Compute mean and standard deviation of the feature importances
mean_importances_overall <- sapply(all_results_overall, function(outcome_result) {
  sapply(outcome_result, function(replication_result_overall) {
    mean(replication_result_overall$lasso_coef[, 1])
  })
})

sd_importances_overall <- sapply(all_results_overall, function(outcome_result) {
  sapply(outcome_result, function(replication_result_overall) {
    sd(replication_result_overall$lasso_coef[, 1])
  })
})

# Save mean and standard deviation into a CSV file
importances_stats_overall <- data.frame(mean_importance = mean_importances_overall, sd_importance = sd_importances_overall)
write.csv(importances_stats_overall, file = "/Users/cdp4029/Downloads/importances_stats_replicated_overall.csv")

# Create ranking plots for "Overall"
for (outcome_var in outcome_vars) {
  # Extract feature names and coefficients
  coef_list_overall <- lapply(all_results_overall[[outcome_var]], function(replication_result_overall) replication_result_overall$lasso_coef[, 1])
  coef_df_overall <- data.frame(outcome = outcome_var, replication = rep(1:10, each = length(coef_list_overall)), do.call(rbind, coef_list_overall))
  
  # Plot
  ggplot(coef_df_overall, aes(x = reorder(rownames(coef_df_overall), -V1), y = V1)) +
    geom_bar(stat = "identity", fill = "blue", color = "black") +
    coord_flip() +
    labs(title = paste("LASSO Coefficients for Outcome(Overall):", outcome_var), x = "Coefficient", y = "Value") +
    theme_minimal()  
}







# Pre
for (outcome_var in outcome_vars) {
  # Extract feature names, coefficients, means, and standard deviations
  coef_list_pre <- lapply(all_results_pre[[outcome_var]], function(replication_result_pre) {
    coef_df_pre <- data.frame(coef = replication_result_pre$lasso_coef[, 1])
    coef_df_pre$mean <- mean(coef_df_pre$coef)
    coef_df_pre$sd <- sd(coef_df_pre$coef)
    coef_df_pre
  })
  
  # Combine results for each outcome
  coef_df_pre <- do.call(rbind, coef_list_pre)
  
  # Save to CSV
  write.csv(coef_df_pre, file = paste("/Users/cdp4029/Downloads/lasso_coefficients_pre_", outcome_var, ".csv", sep = ""))
}

# Post
for (outcome_var in outcome_vars) {
  # Extract feature names, coefficients, means, and standard deviations
  coef_list_post <- lapply(all_results_post[[outcome_var]], function(replication_result_post) {
    coef_df_post <- data.frame(coef = replication_result_post$lasso_coef[, 1])
    coef_df_post$mean <- mean(coef_df_post$coef)
    coef_df_post$sd <- sd(coef_df_post$coef)
    coef_df_post
  })
  
  # Combine results for each outcome
  coef_df_post <- do.call(rbind, coef_list_post)
  
  # Save to CSV
  write.csv(coef_df_post, file = paste("/Users/cdp4029/Downloads/lasso_coefficients_post_", outcome_var, ".csv", sep = ""))
}

# Overall
for (outcome_var in outcome_vars) {
  # Extract feature names, coefficients, means, and standard deviations
  coef_list_overall <- lapply(all_results_overall[[outcome_var]], function(replication_result_overall) {
    coef_df_overall <- data.frame(coef = replication_result_overall$lasso_coef[, 1])
    coef_df_overall$mean <- mean(coef_df_overall$coef)
    coef_df_overall$sd <- sd(coef_df_overall$coef)
    coef_df_overall
  })
  
  # Combine results for each outcome
  coef_df_overall <- do.call(rbind, coef_list_overall)
  
  # Save to CSV
  write.csv(coef_df_overall, file = paste("/Users/cdp4029/Downloads/lasso_coefficients_overall_", outcome_var, ".csv", sep = ""))
}









# Create a function to extract final features, means, and standard deviations
extract_features_stats <- function(results_list) {
  features_stats_list <- lapply(results_list, function(outcome_result) {
    lapply(outcome_result, function(replication_result) {
      selected_features <- replication_result$selected_features
      mean_values <- colMeans(replication_result$lasso_coef)
      sd_values <- apply(replication_result$lasso_coef, 2, sd)
      result_df <- data.frame(
        Feature = selected_features,
        Mean = mean_values,
        SD = sd_values
      )
      return(result_df)
    })
  })
  return(features_stats_list)
}

# Apply the function to each set of results
features_stats_pre <- extract_features_stats(all_results_pre)
features_stats_post <- extract_features_stats(all_results_post)
features_stats_overall <- extract_features_stats(all_results_overall)

# Combine all replications for Pre
combined_features_stats_pre <- lapply(seq_along(outcome_vars), function(i) {
  outcome_var <- outcome_vars[i]
  combined_df <- do.call(rbind, features_stats_pre[[i]])
  return(list(Outcome = outcome_var, FeaturesStats = combined_df))
})

# Combine all replications for Post
combined_features_stats_post <- lapply(seq_along(outcome_vars), function(i) {
  outcome_var <- outcome_vars[i]
  combined_df <- do.call(rbind, features_stats_post[[i]])
  return(list(Outcome = outcome_var, FeaturesStats = combined_df))
})

# Combine all replications for Overall
combined_features_stats_overall <- lapply(seq_along(outcome_vars), function(i) {
  outcome_var <- outcome_vars[i]
  combined_df <- do.call(rbind, features_stats_overall[[i]])
  return(list(Outcome = outcome_var, FeaturesStats = combined_df))
})

# Save CSV files for Pre
for (i in seq_along(combined_features_stats_pre)) {
  outcome_var <- combined_features_stats_pre[[i]]$Outcome
  combined_df <- combined_features_stats_pre[[i]]$FeaturesStats
  file_path <- sprintf("/Users/cdp4029/Downloads/features_stats_pre_%s_combined.csv", outcome_var)
  write.csv(combined_df, file = file_path, row.names = FALSE)
}

# Save CSV files for Post
for (i in seq_along(combined_features_stats_post)) {
  outcome_var <- combined_features_stats_post[[i]]$Outcome
  combined_df <- combined_features_stats_post[[i]]$FeaturesStats
  file_path <- sprintf("/Users/cdp4029/Downloads/features_stats_post_%s_combined.csv", outcome_var)
  write.csv(combined_df, file = file_path, row.names = FALSE)
}

# Save CSV files for Overall
for (i in seq_along(combined_features_stats_overall)) {
  outcome_var <- combined_features_stats_overall[[i]]$Outcome
  combined_df <- combined_features_stats_overall[[i]]$FeaturesStats
  file_path <- sprintf("/Users/cdp4029/Downloads/features_stats_overall_%s_combined.csv", outcome_var)
  write.csv(combined_df, file = file_path, row.names = FALSE)
}







# Combine all replications for each outcome
combined_final_features <- lapply(seq_along(outcome_vars), function(i) {
  outcome_var <- outcome_vars[i]
  
  # Extract features for pre, post, and overall
  features_pre <- unique(unlist(lapply(all_results_pre[[outcome_var]], function(replication_result_pre) {
    replication_result_pre$selected_features
  })))
  
  features_post <- unique(unlist(lapply(all_results_post[[outcome_var]], function(replication_result_post) {
    replication_result_post$selected_features
  })))
  
  features_overall <- unique(unlist(lapply(all_results_overall[[outcome_var]], function(replication_result_overall) {
    replication_result_overall$selected_features
  })))
  
  # Combine features for pre, post, and overall
  all_combined_features <- unique(c(features_pre, features_post, features_overall))
  
  # Function to extract coefficients and handle missing features
  extract_coefficients <- function(results, features) {
    sapply(results, function(replication_result) {
      coef_vector <- rep(NA, length(features))
      matching_features <- replication_result$selected_features %in% features
      coef_vector[matching_features] <- replication_result$lasso_coef[matching_features, 1]
      return(coef_vector)
    })
  }
  
  # Extract coefficients for pre, post, and overall
  coef_pre <- extract_coefficients(all_results_pre[[outcome_var]], all_combined_features)
  coef_post <- extract_coefficients(all_results_post[[outcome_var]], all_combined_features)
  coef_overall <- extract_coefficients(all_results_overall[[outcome_var]], all_combined_features)
  
  # Combine coefficients and create a long-format data frame
  combined_df <- data.frame(
    Outcome = rep(outcome_var, length(all_combined_features) * 10),
    Feature = rep(all_combined_features, each = 10),
    Coefficient_Pre = as.vector(coef_pre),
    Coefficient_Post = as.vector(coef_post),
    Coefficient_Overall = as.vector(coef_overall)
  )
  
  return(combined_df)
})

# Save to CSV files
for (i in seq_along(outcome_vars)) {
  outcome_var <- outcome_vars[i]
  combined_df <- combined_final_features[[i]]
  csv_file <- paste("/Users/cdp4029/Downloads/final_features_", tolower(outcome_var), ".csv", sep = "")
  write.csv(combined_df, file = csv_file, row.names = FALSE)
}
