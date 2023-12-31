rm(list = ls())
library(tidyverse)
library(rpms)
library(xgboost)
library(rBayesianOptimization)
library(caret)
library(purrr)

# load the data

load("data/cleaned/dat_train_test_bootstrapped.RData")

# outcomes: bp_control_jnc7, bp_control_accaha, bp_control_140_90, bp_control_130_80

# Gradient Boosting####

## functions####

`%notin%` <- Negate(`%in%`)

vars <- names(dat_hyp_final)[names(dat_hyp_final) %notin% c("SEQN", "svy_psu", "svy_weight_mec", "svy_strata","svy_year", "bp_control_jnc7","bp_control_accaha", "bp_control_140_90", "bp_control_130_80")]
outcome <- c("bp_control_jnc7", "bp_control_accaha", "bp_control_140_90", "bp_control_130_80")

xgboost_rank <- function(outcome, train_data, weight, test_data){
  xgb_df <- as.matrix(train_data[names(train_data) %in% vars])
  xgb_label <- as.numeric(factor(train_data[,outcome], levels = c("No", "Yes"))) - 1
  xgb_matrix <- xgb.DMatrix(data = xgb_df, label = xgb_label)
  xgb <- xgboost(data = xgb_matrix, nrounds = 100, eta = 0.1, weight = weight)
  
  xgb_importance <- xgb.importance(feature_names = vars, model = xgb)
  rankimp <- arrange(xgb_importance,desc(Frequency))
  
  output <- list(xgb = xgb,rankimp = rankimp)
  return(output)
}

xgboost_eval <- function(outcome, train_data, weight, test_data){
  
  # Fit the xgboost model
  model <- xgboost_rank(outcome,train_data, weight)$xgb
  
  # Compute Confusion Matrix
  test_df <- as.matrix(test_data[names(test_data) %in% vars])
  test_label <- as.numeric(factor(test_data[,outcome], levels = c("No", "Yes"))) - 1
  test_matrix <- xgb.DMatrix(data = test_df, label = test_label)
  predictions <- predict(model, newdata = test_matrix)
  predicted_labels <- as.factor(ifelse(predictions > 0.5, 1, 0))
  
  confusionMatrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))
  precision <- confusionMatrix$byClass["Pos Pred Value"]
  recall <- confusionMatrix$byClass["Sensitivity"]
  f1_scores <- 2 * (precision * recall) / (precision + recall)
  
  # Computing weighted F1 score
  class_sizes <- table(test_label)
  weights <- class_sizes / sum(class_sizes)
  weighted_f1 <- sum(f1_scores * weights, na.rm = TRUE)
  
  output <- list(model = model,weighted_f1 = weighted_f1)
  return(output)
}

xgboost_rank_samp <- function(outcome, data_bootstrapped, weight){
  
  model <- vector("list", length = length(data_bootstrapped))
  feature_list <- vector("list", length = length(data_bootstrapped))
  mean_rankimp <- NULL
  
  # Fit the xgboost model
  for(i in 1:length(data_bootstrapped)){
    train_data <- data_bootstrapped[[i]]
    fit <- xgboost_rank(outcome,train_data, weight)
    model[[i]] <- fit$xgb
    feature_list[[i]] <- fit$rankimp
  }
  
  features <- purrr::map(feature_list, ~ .x %>% dplyr::select(Feature) %>% unique())
  features_table <- dplyr::bind_rows(features, .id = "DataFrame") %>%
    dplyr::count(Feature) %>%
    dplyr::filter(n >= 2)
  
  feature_list <- purrr::map(feature_list, ~ .x %>%
                                       dplyr::filter(Feature %in% features_table$Feature))
  
  mean_rankimp <- feature_list %>% 
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::group_by(Feature) %>% 
    summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), 
                                        sd = ~sd(.x, na.rm = TRUE)))) %>% 
    arrange(desc(Frequency_mean))
  
  output <- list(model = model,mean_rankimp = mean_rankimp)
  return(output)
}

## Outcome 1: bp_control_jnc7####

### Pre 2013####
pre2013_bp_control_jnc7 <- xgboost_eval("bp_control_jnc7",dat_hyp_final_pre_train, "svy_weight_mec",dat_hyp_final_pre_test)

pre2013_bp_control_jnc7_samp <- xgboost_rank_samp("bp_control_jnc7",dat_hyp_final_pre_samp, "svy_weight_mec")


### Post 2013####

post2013_bp_control_jnc7 <- xgboost_eval("bp_control_jnc7",dat_hyp_final_post_train, "svy_weight_mec",dat_hyp_final_post_test)

post2013_bp_control_jnc7_samp <- xgboost_rank_samp("bp_control_jnc7",dat_hyp_final_post_samp, "svy_weight_mec")

### Overall####

bp_control_jnc7 <- xgboost_eval("bp_control_jnc7",dat_hyp_final_train, "svy_weight_mec",dat_hyp_final_test)

bp_control_jnc7_samp <- xgboost_rank_samp("bp_control_jnc7",dat_hyp_final_samp, "svy_weight_mec")


## Outcome 2: bp_control_accaha####

### Pre 2013####
pre2013_bp_control_accaha <- xgboost_eval("bp_control_accaha",dat_hyp_final_pre_train, "svy_weight_mec",dat_hyp_final_pre_test)

pre2013_bp_control_accaha_samp <- xgboost_rank_samp("bp_control_accaha",dat_hyp_final_pre_samp, "svy_weight_mec")

### Post 2013####

post2013_bp_control_accaha <- xgboost_eval("bp_control_accaha",dat_hyp_final_post_train, "svy_weight_mec",dat_hyp_final_post_test)

post2013_bp_control_accaha_samp <- xgboost_rank_samp("bp_control_accaha",dat_hyp_final_post_samp, "svy_weight_mec")

### Overall####

bp_control_accaha <- xgboost_eval("bp_control_accaha",dat_hyp_final_train, "svy_weight_mec",dat_hyp_final_test)

bp_control_accaha_samp <- xgboost_rank_samp("bp_control_accaha",dat_hyp_final_samp, "svy_weight_mec")


## Outcome 3: bp_control_140_90####

### Pre 2013####
pre2013_bp_control_140_90 <- xgboost_eval("bp_control_140_90",dat_hyp_final_pre_train, "svy_weight_mec",dat_hyp_final_pre_test)

pre2013_bp_control_140_90_samp <- xgboost_rank_samp("bp_control_140_90",dat_hyp_final_pre_samp, "svy_weight_mec")

### Post 2013####

post2013_bp_control_140_90 <- xgboost_eval("bp_control_140_90",dat_hyp_final_post_train, "svy_weight_mec",dat_hyp_final_post_test)

post2013_bp_control_140_90_samp <- xgboost_rank_samp("bp_control_140_90",dat_hyp_final_post_samp, "svy_weight_mec")

### Overall####

bp_control_140_90 <- xgboost_eval("bp_control_140_90",dat_hyp_final_train, "svy_weight_mec",dat_hyp_final_test)

bp_control_140_90_samp <- xgboost_rank_samp("bp_control_140_90",dat_hyp_final_samp, "svy_weight_mec")


## Outcome 4: bp_control_130_80####

### Pre 2013####
pre2013_bp_control_130_80 <- xgboost_eval("bp_control_130_80",dat_hyp_final_pre_train, "svy_weight_mec",dat_hyp_final_pre_test)

pre2013_bp_control_130_80_samp <- xgboost_rank_samp("bp_control_130_80",dat_hyp_final_pre_samp, "svy_weight_mec")

### Post 2013####

post2013_bp_control_130_80 <- xgboost_eval("bp_control_130_80",dat_hyp_final_post_train, "svy_weight_mec",dat_hyp_final_post_test)

post2013_bp_control_130_80_samp <- xgboost_rank_samp("bp_control_130_80",dat_hyp_final_post_samp, "svy_weight_mec")

### Overall####

bp_control_130_80 <- xgboost_eval("bp_control_130_80",dat_hyp_final_train, "svy_weight_mec",dat_hyp_final_test)

bp_control_130_80_samp <- xgboost_rank_samp("bp_control_130_80",dat_hyp_final_samp, "svy_weight_mec")


## Output Rank List####

outcome1.list <- bind_rows(list(pre2013 = pre2013_bp_control_jnc7_samp$mean_rankimp, post2013 = post2013_bp_control_jnc7_samp$mean_rankimp, overall = bp_control_jnc7_samp$mean_rankimp), .id = "Year")
outcome2.list <- bind_rows(list(pre2013 = pre2013_bp_control_accaha_samp$mean_rankimp, post2013 = post2013_bp_control_accaha_samp$mean_rankimp, overall = bp_control_accaha_samp$mean_rankimp), .id = "Year")
outcome3.list <- bind_rows(list(pre2013 = pre2013_bp_control_140_90_samp$mean_rankimp, post2013 = post2013_bp_control_140_90_samp$mean_rankimp, overall = bp_control_140_90_samp$mean_rankimp), .id = "Year")
outcome4.list <- bind_rows(list(pre2013 = pre2013_bp_control_130_80_samp$mean_rankimp, post2013 = post2013_bp_control_130_80_samp$mean_rankimp, overall = bp_control_130_80_samp$mean_rankimp), .id = "Year")

outcome1.finallist <- outcome1.list %>%
  group_by(Feature) %>% mutate(Count = n()) %>% filter(Count >= 2)
outcome2.finallist <- outcome2.list %>%
  group_by(Feature) %>% mutate(Count = n()) %>% filter(Count >= 2)
outcome3.finallist <- outcome3.list %>%
  group_by(Feature) %>% mutate(Count = n()) %>% filter(Count >= 2)
outcome4.finallist <- outcome4.list %>%
  group_by(Feature) %>% mutate(Count = n()) %>% filter(Count >= 2)


write.csv(outcome1.list, file = "tables/xgboost/bp_control_jnc7_features.csv")
write.csv(outcome2.list, file = "tables/xgboost/bp_control_accaha_features.csv")
write.csv(outcome3.list, file = "tables/xgboost/bp_control_140_90_features.csv")
write.csv(outcome4.list, file = "tables/xgboost/bp_control_130_80_features.csv")

write.csv(outcome1.finallist, file = "tables/xgboost/bp_control_jnc7_features_final.csv")
write.csv(outcome2.finallist, file = "tables/xgboost/bp_control_accaha_features_final.csv")
write.csv(outcome3.finallist, file = "tables/xgboost/bp_control_140_90_features_final.csv")
write.csv(outcome4.finallist, file = "tables/xgboost/bp_control_130_80_features_final.csv")

save(pre2013_bp_control_jnc7_samp, post2013_bp_control_jnc7_samp,bp_control_jnc7_samp,
     pre2013_bp_control_accaha_samp, post2013_bp_control_accaha_samp, bp_control_accaha_samp,
     pre2013_bp_control_140_90_samp, post2013_bp_control_140_90_samp, bp_control_140_90_samp,
     pre2013_bp_control_130_80_samp, post2013_bp_control_130_80_samp, bp_control_130_80_samp,
     file = "tables/xgboost/xgboost_ranklist.RData")
