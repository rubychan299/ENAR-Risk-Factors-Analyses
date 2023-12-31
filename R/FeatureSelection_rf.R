rm(list = ls())
library(tidyverse)
library(randomForest)
library(caret)
library(purrr)

# load the data

load("data/cleaned/dat_train_test_bootstrapped.RData")

# outcomes: bp_control_jnc7, bp_control_accaha, bp_control_140_90, bp_control_130_80

# Random Forest####

## functions####

`%notin%` <- Negate(`%in%`)

vars <- names(dat_hyp_final)[names(dat_hyp_final) %notin% c("SEQN", "svy_psu", "svy_weight_mec", "svy_strata","svy_year", "bp_control_jnc7","bp_control_accaha", "bp_control_140_90", "bp_control_130_80")]

rf_rank <- function(outcome, train_data, weight){
  
  # Fit the rf model
  formula_str <- paste0(outcome,"~", paste(vars, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  rf_model <- randomForest(formula_obj, data = train_data, importance = TRUE, weights = train_data[,weight])
  importance <- data.frame(importance(rf_model))
  rankimp <- arrange(importance,desc(MeanDecreaseGini))
  
  output <- list(rf_model = rf_model, rankimp = rankimp)
  return(output)
}

rf_eval <- function(outcome, train_data, weight, test_data){
  
  # Fit the rf model
  model <- rf_rank(outcome,train_data, weight)$rf_model

  # Compute Confusion Matrix
  predictions <- predict(model, newdata = test_data)
  confusionMatrix <- confusionMatrix(predictions, test_data[,outcome])
  
  precision <- confusionMatrix$byClass["Pos Pred Value"]
  recall <- confusionMatrix$byClass["Sensitivity"]
  f1_scores <- 2 * (precision * recall) / (precision + recall)
  
  # Computing weighted F1 score
  class_sizes <- table(test_data[,outcome])
  weights <- class_sizes / sum(class_sizes)
  weighted_f1 <- sum(f1_scores * weights, na.rm = TRUE)
  
  output <- list(model = model,weighted_f1 = weighted_f1)
  return(output)
}

rf_rank_samp <- function(outcome, data_bootstrapped, weight){
  
  model <- vector("list", length = length(data_bootstrapped))
  feature_list <- vector("list", length = length(data_bootstrapped))
  mean_rankimp <- NULL
  
  # Fit the rf model
  for(i in 1:length(data_bootstrapped)){
    train_data <- data_bootstrapped[[i]]
    fit <- rf_rank(outcome,train_data, weight)
    model[[i]] <- fit$rf_model
    feature_list[[i]] <- fit$rankimp
    feature_list[[i]][['Feature']] <- rownames(feature_list[[i]])
  }
  
  mean_rankimp <- feature_list %>% 
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::group_by(Feature) %>% 
    summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE), 
                                        sd = ~sd(.x, na.rm = TRUE)))) %>% 
    arrange(desc(MeanDecreaseAccuracy_mean))
    
  output <- list(model = model,mean_rankimp = mean_rankimp)
  return(output)
}

## Outcome 1: bp_control_jnc7####

### Pre 2013####
pre2013_bp_control_jnc7 <- rf_eval("bp_control_jnc7",dat_hyp_final_pre_train, "svy_weight_mec",dat_hyp_final_pre_test)

pre2013_bp_control_jnc7_samp <- rf_rank_samp("bp_control_jnc7",dat_hyp_final_pre_samp, "svy_weight_mec")

### Post 2013####

post2013_bp_control_jnc7 <- rf_eval("bp_control_jnc7",dat_hyp_final_post_train, "svy_weight_mec",dat_hyp_final_post_test)

post2013_bp_control_jnc7_samp <- rf_rank_samp("bp_control_jnc7",dat_hyp_final_post_samp, "svy_weight_mec")

### Overall####

bp_control_jnc7 <- rf_eval("bp_control_jnc7",dat_hyp_final_train, "svy_weight_mec",dat_hyp_final_test)

bp_control_jnc7_samp <- rf_rank_samp("bp_control_jnc7",dat_hyp_final_samp, "svy_weight_mec")


## Outcome 2: bp_control_accaha####

### Pre 2013####
pre2013_bp_control_accaha <- rf_eval("bp_control_accaha",dat_hyp_final_pre_train, "svy_weight_mec",dat_hyp_final_pre_test)

pre2013_bp_control_accaha_samp <- rf_rank_samp("bp_control_accaha",dat_hyp_final_pre_samp, "svy_weight_mec")

### Post 2013####

post2013_bp_control_accaha <- rf_eval("bp_control_accaha",dat_hyp_final_post_train, "svy_weight_mec",dat_hyp_final_post_test)

post2013_bp_control_accaha_samp <- rf_rank_samp("bp_control_accaha",dat_hyp_final_post_samp, "svy_weight_mec")

### Overall####

bp_control_accaha <- rf_eval("bp_control_accaha",dat_hyp_final_train, "svy_weight_mec",dat_hyp_final_test)

bp_control_accaha_samp <- rf_rank_samp("bp_control_accaha",dat_hyp_final_samp, "svy_weight_mec")


## Outcome 3: bp_control_140_90####

### Pre 2013####
pre2013_bp_control_140_90 <- rf_eval("bp_control_140_90",dat_hyp_final_pre_train, "svy_weight_mec",dat_hyp_final_pre_test)

pre2013_bp_control_140_90_samp <- rf_rank_samp("bp_control_140_90",dat_hyp_final_pre_samp, "svy_weight_mec")

### Post 2013####

post2013_bp_control_140_90 <- rf_eval("bp_control_140_90",dat_hyp_final_post_train, "svy_weight_mec",dat_hyp_final_post_test)

post2013_bp_control_140_90_samp <- rf_rank_samp("bp_control_140_90",dat_hyp_final_post_samp, "svy_weight_mec")

### Overall####

bp_control_140_90 <- rf_eval("bp_control_140_90",dat_hyp_final_train, "svy_weight_mec",dat_hyp_final_test)

bp_control_140_90_samp <- rf_rank_samp("bp_control_140_90",dat_hyp_final_samp, "svy_weight_mec")


## Outcome 4: bp_control_130_80####

### Pre 2013####
pre2013_bp_control_130_80 <- rf_eval("bp_control_130_80",dat_hyp_final_pre_train, "svy_weight_mec",dat_hyp_final_pre_test)

pre2013_bp_control_130_80_samp <- rf_rank_samp("bp_control_130_80",dat_hyp_final_pre_samp, "svy_weight_mec")

### Post 2013####

post2013_bp_control_130_80 <- rf_eval("bp_control_130_80",dat_hyp_final_post_train, "svy_weight_mec",dat_hyp_final_post_test)

post2013_bp_control_130_80_samp <- rf_rank_samp("bp_control_130_80",dat_hyp_final_post_samp, "svy_weight_mec")

### Overall####

bp_control_130_80 <- rf_eval("bp_control_130_80",dat_hyp_final_train, "svy_weight_mec",dat_hyp_final_test)

bp_control_130_80_samp <- rf_rank_samp("bp_control_130_80",dat_hyp_final_samp, "svy_weight_mec")


## Output Rank List####

outcome1.list <- bind_rows(list(pre2013 = pre2013_bp_control_jnc7_samp$mean_rankimp, post2013 = post2013_bp_control_jnc7_samp$mean_rankimp, overall = bp_control_jnc7_samp$mean_rankimp), .id = "Year")
outcome2.list <- bind_rows(list(pre2013 = pre2013_bp_control_accaha_samp$mean_rankimp, post2013 = post2013_bp_control_accaha_samp$mean_rankimp, overall = bp_control_accaha_samp$mean_rankimp), .id = "Year")
outcome3.list <- bind_rows(list(pre2013 = pre2013_bp_control_140_90_samp$mean_rankimp, post2013 = post2013_bp_control_140_90_samp$mean_rankimp, overall = bp_control_140_90_samp$mean_rankimp), .id = "Year")
outcome4.list <- bind_rows(list(pre2013 = pre2013_bp_control_130_80_samp$mean_rankimp, post2013 = post2013_bp_control_130_80_samp$mean_rankimp, overall = bp_control_130_80_samp$mean_rankimp), .id = "Year")

outcome1.finallist <- outcome1.list %>% group_by(Year) %>% top_n(50) %>% arrange(desc(MeanDecreaseAccuracy_mean)) %>% group_by(Feature) %>% mutate(Count = n()) %>% filter(Count >= 2)
outcome2.finallist <- outcome2.list %>% group_by(Year) %>% top_n(50) %>% arrange(desc(MeanDecreaseAccuracy_mean)) %>% group_by(Feature) %>% mutate(Count = n()) %>% filter(Count >= 2)
outcome3.finallist <- outcome2.list %>% group_by(Year) %>% top_n(50) %>% arrange(desc(MeanDecreaseAccuracy_mean)) %>% group_by(Feature) %>% mutate(Count = n()) %>% filter(Count >= 2)
outcome4.finallist <- outcome2.list %>% group_by(Year) %>% top_n(50) %>% arrange(desc(MeanDecreaseAccuracy_mean)) %>% group_by(Feature) %>% mutate(Count = n()) %>% filter(Count >= 2)


write.csv(outcome1.list, file = "tables/rf/bp_control_jnc7_features.csv")
write.csv(outcome2.list, file = "tables/rf/bp_control_accaha_features.csv")
write.csv(outcome3.list, file = "tables/rf/bp_control_140_90_features.csv")
write.csv(outcome4.list, file = "tables/rf/bp_control_130_80_features.csv")

write.csv(outcome1.finallist, file = "tables/rf/bp_control_jnc7_features_final.csv")
write.csv(outcome2.finallist, file = "tables/rf/bp_control_accaha_features_final.csv")
write.csv(outcome3.finallist, file = "tables/rf/bp_control_140_90_features_final.csv")
write.csv(outcome4.finallist, file = "tables/rf/bp_control_130_80_features_final.csv")

save(pre2013_bp_control_jnc7_samp, post2013_bp_control_jnc7_samp,bp_control_jnc7_samp,
     pre2013_bp_control_accaha_samp, post2013_bp_control_accaha_samp, bp_control_accaha_samp,
     pre2013_bp_control_140_90_samp, post2013_bp_control_140_90_samp, bp_control_140_90_samp,
     pre2013_bp_control_130_80_samp, post2013_bp_control_130_80_samp, bp_control_130_80_samp,
     file = "tables/rf/rf_ranklist.RData")
