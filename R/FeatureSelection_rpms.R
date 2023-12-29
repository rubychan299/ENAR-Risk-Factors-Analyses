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

# change the outcomes to numeric
dat_hyp_final <- dat_hyp_final %>% 
  mutate(bp_control_jnc7 = as.numeric(bp_control_jnc7),
         bp_control_accaha = as.numeric(bp_control_accaha),
         bp_control_140_90 = as.numeric(bp_control_140_90),
         bp_control_130_80 = as.numeric(bp_control_130_80))

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
dat_hyp_final_train <- dat_hyp_final_pre[dat_hyp_final_ind, ]
dat_hyp_final_test <- dat_hyp_final_pre[-dat_hyp_final_ind, ]

# outcomes: bp_control_jnc7, bp_control_accaha, bp_control_140_90, bp_control_130_80

# Gradient Boosting####

## Outcome 1: bp_control_jnc7####
`%notin%` <- Negate(`%in%`)

# rpms (doesn't really work) 
# Create the formula string
vars <- names(dat_hyp_final)[names(dat_hyp_final) %notin% c("SEQN", "svy_psu", "svy_weight_mec", "svy_strata","svy_year", "bp_control_jnc7","bp_control_accaha", "bp_control_140_90", "bp_control_130_80")]
# formula_str <- paste("bp_control_jnc7 ~", paste(vars1, collapse = " + "))
# 
# # Convert to formula
# formula_obj <- as.formula(formula_str)

# Use in rpms_boost
# xgb1 <- rpms_boost(formula_obj, data = dat_hyp_final, strata = ~svy_strata,
#                    weights = ~svy_weight_mec, pval=.01)

