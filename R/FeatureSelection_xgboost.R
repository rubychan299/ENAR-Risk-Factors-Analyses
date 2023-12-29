rm(list = ls())
library(tidyverse)
library(rpms)
library(xgboost)
library(rBayesianOptimization)
library(caret)

# Data Manipulation####

# load the data

load("data/cleaned/dat_hyp_final_svy.RData")

# Rename the variables by removing spaces
names(dat_hyp_final) <- gsub(" ", "", names(dat_hyp_final))
# Replace spaces, periods, and plus signs with underscores
names(dat_hyp_final) <- gsub(" ", "_", names(dat_hyp_final))
names(dat_hyp_final) <- gsub("\\.", "_", names(dat_hyp_final))
names(dat_hyp_final) <- gsub("\\+", "plus", names(dat_hyp_final))
names(dat_hyp_final) <- gsub("\\-", "plus", names(dat_hyp_final))
names(dat_hyp_final) <- gsub(">", "", names(dat_hyp_final))
names(dat_hyp_final) <- gsub("<", "", names(dat_hyp_final))

# Drop probelmatic vars
dat_hyp_final <- dat_hyp_final %>% 
  select(-contains("DSD010_x"), -contains("DRDINT_x"), -contains("DRDINT_y"), -LBDHDDSI,
         -URXUMS, -URXCRS, -contains("BPQ050A"), -contains("cc_cvd_stroke"), -contains("svy_subpop_chol"))

# subset the data into pre 2013 and post 2013

dat_hyp_final_pre <- dat_hyp_final %>% 
  filter(svy_year == "2003-2004" | svy_year == "2005-2006" | svy_year == "2007-2008" | svy_year == "2009-2010" | svy_year == "2011-2012")

dat_hyp_final_post <- dat_hyp_final %>% 
  filter(svy_year == "2013-2014" | svy_year == "2015-2016" | svy_year == "2017-2020")

dat_hyp_final <- dat_hyp_final %>% 
  filter(svy_year != "1999-2000" | svy_year == "2001-2002")

# split into 70% training and 30% testing
set.seed(2024)
dat_hyp_final_pre_ind <- sample(1:nrow(dat_hyp_final_pre), 0.7*nrow(dat_hyp_final_pre))
dat_hyp_final_pre_train <- dat_hyp_final_pre[dat_hyp_final_pre_ind, ]
dat_hyp_final_pre_test <- dat_hyp_final_pre[-dat_hyp_final_pre_ind, ]

dat_hyp_final_post_ind <- sample(1:nrow(dat_hyp_final_post), 0.7*nrow(dat_hyp_final_post))
dat_hyp_final_post_train <- dat_hyp_final_post[dat_hyp_final_post_ind, ]
dat_hyp_final_post_test <- dat_hyp_final_post[-dat_hyp_final_post_ind, ]

dat_hyp_final_ind <- sample(1:nrow(dat_hyp_final), 0.7*nrow(dat_hyp_final))
dat_hyp_final_train <- dat_hyp_final[dat_hyp_final_ind, ]
dat_hyp_final_test <- dat_hyp_final[-dat_hyp_final_ind, ]

# outcomes: bp_control_jnc7, bp_control_accaha, bp_control_140_90, bp_control_130_80

# Gradient Boosting####

## Outcome 1: bp_control_jnc7####
`%notin%` <- Negate(`%in%`)

vars <- names(dat_hyp_final)[names(dat_hyp_final) %notin% c("SEQN", "svy_psu", "svy_weight_mec", "svy_strata","svy_year", "bp_control_jnc7","bp_control_accaha", "bp_control_140_90", "bp_control_130_80")]

### Pre 2013####
xgb1_df <- as.matrix(dat_hyp_final_pre_train[names(dat_hyp_final_pre_train) %in% vars])
xgb1_label <- dat_hyp_final_pre_train$bp_control_jnc7
xgb1_matrix <- xgb.DMatrix(data = xgb1_df, label = xgb1_label)
xgb1 <- xgboost(data = xgb1_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Hyperparameter Tuning
# tune_grid <- expand.grid(nrounds = c(100, 200),
#                          eta = c(0.01, 0.05, 0.1),
#                          max_depth = c(3, 6, 9),
#                          gamma = c(0, 0.1, 0.2),
#                          min_child_weight = c(1, 6),
#                          subsample = c(0.5, 1),
#                          colsample_bytree = c(0.5, 1))
# control <- trainControl(method = "cv", number = 5)
# tuned_model <- caret::train(xgb1_df, factor(xgb1_label), method = "xgbTree",
#                      trControl = control, tuneGrid = tune_grid, weights = weights)

# Extract feature importance
# xgb1 <- xgboost(data = xgb1_matrix, nrounds = 100, eta = 0.1, weight = weights)

xgb1_importance <- xgb.importance(feature_names = vars, model = xgb1)
print(xgb1_importance)

# Plot feature importance
xgb1.plot <- xgb.ggplot.importance(xgb1_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_jnc7_features_pre.png",xgb1.plot)

### Post 2013####
xgb2_df <- as.matrix(dat_hyp_final_post_train[names(dat_hyp_final_post_train) %in% vars])
xgb2_label <- dat_hyp_final_post_train$bp_control_jnc7
xgb2_matrix <- xgb.DMatrix(data = xgb2_df, label = xgb2_label)
xgb2 <- xgboost(data = xgb2_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb2_importance <- xgb.importance(feature_names = vars, model = xgb1)
print(xgb2_importance)

# Plot feature importance
xgb2.plot <- xgb.ggplot.importance(xgb2_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_jnc7_features_post.png",xgb2.plot)

### Overall####
xgb3_df <- as.matrix(dat_hyp_final_train[names(dat_hyp_final_train) %in% vars])
xgb3_label <- dat_hyp_final_train$bp_control_jnc7
xgb3_matrix <- xgb.DMatrix(data = xgb3_df, label = xgb3_label)
xgb3 <- xgboost(data = xgb3_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb3_importance <- xgb.importance(feature_names = vars, model = xgb3)
print(xgb3_importance)

# Plot feature importance
xgb3.plot <- xgb.ggplot.importance(xgb3_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_jnc7_features_overall.png",xgb3.plot)


outcome1.imp <- bind_rows(list(xgb1 = xgb1_importance, xgb2 = xgb2_importance, xgb3 = xgb3_importance), .id = "source")


## Outcome 2: bp_control_accaha####

### Pre 2013####
xgb4_df <- as.matrix(dat_hyp_final_pre_train[names(dat_hyp_final_pre_train) %in% vars])
xgb4_label <- dat_hyp_final_pre_train$bp_control_accaha
xgb4_matrix <- xgb.DMatrix(data = xgb4_df, label = xgb4_label)
xgb4 <- xgboost(data = xgb4_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb4_importance <- xgb.importance(feature_names = vars, model = xgb4)
print(xgb4_importance)

# Plot feature importance
xgb4.plot <- xgb.ggplot.importance(xgb4_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_accaha_features_pre.png",xgb4.plot)


### Post 2013####
xgb5_df <- as.matrix(dat_hyp_final_post_train[names(dat_hyp_final_post_train) %in% vars])
xgb5_label <- dat_hyp_final_post_train$bp_control_accaha
xgb5_matrix <- xgb.DMatrix(data = xgb5_df, label = xgb5_label)
xgb5 <- xgboost(data = xgb5_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb5_importance <- xgb.importance(feature_names = vars, model = xgb5)
print(xgb5_importance)

# Plot feature importance
xgb5.plot <- xgb.ggplot.importance(xgb5_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_accaha_features_post.png",xgb5.plot)


### Overall####
xgb6_df <- as.matrix(dat_hyp_final_train[names(dat_hyp_final_train) %in% vars])
xgb6_label <- dat_hyp_final_train$bp_control_accaha
xgb6_matrix <- xgb.DMatrix(data = xgb6_df, label = xgb6_label)
xgb6 <- xgboost(data = xgb6_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb6_importance <- xgb.importance(feature_names = vars, model = xgb6)
print(xgb6_importance)

# Plot feature importance
xgb6.plot <- xgb.ggplot.importance(xgb6_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_accaha_features_overall.png",xgb6.plot)

outcome2.imp <- bind_rows(list(xgb4 = xgb4_importance, xgb5 = xgb5_importance, xgb6 = xgb6_importance), .id = "source")


## Outcome 3: bp_control_140_90####

### Pre 2013####
xgb7_df <- as.matrix(dat_hyp_final_pre_train[names(dat_hyp_final_pre_train) %in% vars])
xgb7_label <- dat_hyp_final_pre_train$bp_control_140_90
xgb7_matrix <- xgb.DMatrix(data = xgb7_df, label = xgb7_label)
xgb7 <- xgboost(data = xgb7_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb7_importance <- xgb.importance(feature_names = vars, model = xgb7)
print(xgb7_importance)

# Plot feature importance
xgb7.plot <- xgb.ggplot.importance(xgb7_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_140_90_features_pre.png",xgb7.plot)


### Post 2013####
xgb8_df <- as.matrix(dat_hyp_final_post_train[names(dat_hyp_final_post_train) %in% vars])
xgb8_label <- dat_hyp_final_post_train$bp_control_140_90
xgb8_matrix <- xgb.DMatrix(data = xgb7_df, label = xgb7_label)
xgb8 <- xgboost(data = xgb8_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb8_importance <- xgb.importance(feature_names = vars, model = xgb8)
print(xgb8_importance)

# Plot feature importance
xgb8.plot <- xgb.ggplot.importance(xgb8_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_140_90_features_post.png",xgb8.plot)

### Overall####
xgb9_df <- as.matrix(dat_hyp_final_train[names(dat_hyp_final_train) %in% vars])
xgb9_label <- dat_hyp_final_train$bp_control_140_90
xgb9_matrix <- xgb.DMatrix(data = xgb9_df, label = xgb9_label)
xgb9 <- xgboost(data = xgb9_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb9_importance <- xgb.importance(feature_names = vars, model = xgb9)
print(xgb9_importance)

# Plot feature importance
xgb9.plot <- xgb.ggplot.importance(xgb9_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_140_90_features_overall.png",xgb9.plot)

outcome3.imp <- bind_rows(list(xgb7 = xgb7_importance, xgb8 = xgb8_importance, xgb9 = xgb9_importance), .id = "source")

## Outcome 4: bp_control_130_80####

### Pre 2013####
xgb10_df <- as.matrix(dat_hyp_final_pre_train[names(dat_hyp_final_pre_train) %in% vars])
xgb10_label <- dat_hyp_final_pre_train$bp_control_130_80
xgb10_matrix <- xgb.DMatrix(data = xgb1_df, label = xgb1_label)
xgb10 <- xgboost(data = xgb10_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb10_importance <- xgb.importance(feature_names = vars, model = xgb10)
print(xgb10_importance)

# Plot feature importance
xgb10.plot <- xgb.ggplot.importance(xgb10_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_130_80_features_pre.png",xgb10.plot)

### Post 2013####
xgb11_df <- as.matrix(dat_hyp_final_post_train[names(dat_hyp_final_post_train) %in% vars])
xgb11_label <- dat_hyp_final_post_train$bp_control_130_80
xgb11_matrix <- xgb.DMatrix(data = xgb11_df, label = xgb11_label)
xgb11 <- xgboost(data = xgb11_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb11_importance <- xgb.importance(feature_names = vars, model = xgb11)
print(xgb11_importance)

# Plot feature importance
xgb11.plot <- xgb.ggplot.importance(xgb11_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_130_80_features_post.png",xgb11.plot)

### Overall####
xgb12_df <- as.matrix(dat_hyp_final_train[names(dat_hyp_final_train) %in% vars])
xgb12_label <- dat_hyp_final_train$bp_control_jnc7
xgb12_matrix <- xgb.DMatrix(data = xgb12_df, label = xgb3_label)
xgb12 <- xgboost(data = xgb12_matrix, nrounds = 100, eta = 0.1, weight = weights)

# Extract feature importance
xgb12_importance <- xgb.importance(feature_names = vars, model = xgb12)
print(xgb12_importance)

# Plot feature importance
xgb12.plot <- xgb.ggplot.importance(xgb12_importance, measure = "Frequency", rel_to_first = TRUE, xlab = "Relative importance")
ggsave("plots/xgboost/bp_control_130_80_features_overall.png",xgb12.plot)

outcome4.imp <- bind_rows(list(xgb10 = xgb10_importance, xgb11 = xgb11_importance, xgb12 = xgb12_importance), .id = "source")

write.csv(outcome1.imp, file = "tables/xgboost/bp_control_jnc7_features.csv")
write.csv(outcome2.imp, file = "tables/xgboost/bp_control_accaha_features.csv")
write.csv(outcome3.imp, file = "tables/xgboost/bp_control_140_90_features.csv")
write.csv(outcome4.imp, file = "tables/xgboost/bp_control_130_80_features.csv")



