rm(list = ls())
# load('/Users/taehyo/Library/CloudStorage/Dropbox/NYU/Research/Research/Code/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/dat_hyp_final_2013_2020.RData')

library(mice)
library(tidyr)
library(tidyverse)
library(kableExtra)
library(survey)
library(dplyr)
library(jtools)

# Define datasets
dat_hyp_2013_glm <- lapply(dat_hyp_2013_final, function(x){
  x <- x %>% 
    select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
           demo_age_years, demo_gender, race,
           HIQ011, OHQ845, PFQ049, PFQ054, 
           cc_diabetes, cc_ckd, cc_cvd_chd, cc_cvd_stroke, cc_cvd_hf, cc_cvd_any, 
           LBXSBU, LBDMONO, LBXRBCSI, LBXTC, 
           phq9_category, cc_bmi, FSDAD, WHQ030)
})

dat_hyp_2015_glm <- lapply(dat_hyp_2015_final, function(x){
  x <- x %>% 
    select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
           demo_age_years, demo_gender,race,
           HIQ011, cc_diabetes, cc_ckd, cc_cvd_chd,cc_cvd_hf, cc_cvd_any,
           weight_change, LBXTC, phq9_category, cc_bmi, FSDAD, cc_smoke)
})


dat_hyp_2017_glm <- lapply(dat_hyp_2017_final, function(x){
  x <- x %>% 
    select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
           demo_age_years, demo_gender, race,
           HIQ011, cc_diabetes, cc_ckd, URXUMS, LBXSBU, LBXSTR, LBDMONO,
           LBXBPB, LBXTC, phq9_category, cc_bmi, WHQ030)
})

dat_hyp_2021_glm <- lapply(dat_hyp_2021_final, function(x){
  x <- x %>% 
    select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
           demo_age_years, demo_gender, demo_race, OHQ845,
           cc_cvd_any, cc_diabetes, LBXRBCSI, LBXBPB, LBXTC,LBDMONO,
           weight_change, cc_smoke, cc_bmi)
})
           
           

data_list_years <- list(
  "2013" = dat_hyp_2013_glm,
  "2015" = dat_hyp_2015_glm,
  "2017" = dat_hyp_2017_glm,
  "2021" = dat_hyp_2021_glm
)

name_map <- c(
  "bp_control_jnc7" = "BP control",
  "cc_diabetes" = "Diabetes",
  "cc_cvd_any" = "Cardiovascular \nDisease",
  "cc_cvd_stroke" = "Stroke",
  "cc_cvd_chd" = "Coronary Heart \nDisease",
  "cc_cvd_hf" = "Heart Failure",
  "cc_bmi" = "Body Mass Index",
  "cc_ckd" = "Chronic Kidney \nDisease",
  "HIQ011" = "Covered by \nHealth Insurance",
  "OHQ845" = "Oral Health",
  "LBXRBCSI" = "Red Blood Cell \nIndex",
  "LBDMONO" = "Monocyte Count",
  "LBXSCH" = "Serum \nCholesterol",
  "LBXSTR" = "Strontium",
  "LBXSBU" = "Serum Urea \nNitrogen",
  "URXUMS" = "Urinary \nAlbumin",
  "URXUCR" = "Urinary Creatinine",
  "LBXSUA" = "Serum Uric Acid",
  "LBXTC" = "Total Cholesterol",
  "LBXTHG" = "Blood Mercury",
  "LBXBPB" = "Blood Lead",
  "PFQ054" = "Physical Function: \nWalking",
  "PFQ049" = "Physical Function: \nWorking",
  "phq9_category" = "Depression",
  "WHQ030" = "Weight Status",
  "FSDAD" = "Food Security",
  "weight_change" = "Weight Change",
  "BMXBMI" = "Body Mass Index",
  "KIQ022" = "Chronic Kidney \nDisease"
)

# Store results
pooled_results_all_years <- list()
pooled_results_all_years_svy <- list()

for (year in names(data_list_years)) {
  data_list <- data_list_years[[year]]
  models <- list()
  models_svy <- list()
  
  for (i in seq_along(data_list)) {
    data <- data_list[[i]]
    # Extract all variable names, excluding survey design variables
    predictors <- setdiff(names(data), c("SEQN", "bp_control_jnc7", "svy_weight_mec", "svy_strata", "svy_psu"))
    
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
    models_svy[[i]] <- svyglm(formula, design = svydesign_obj, family = quasibinomial(link = "logit"),
    control = list(maxit = 500))
    
    models[[i]] <- glm(formula, data = data, family = quasibinomial(link = "logit"),
                       control = list(maxit = 500))
    
  }
  
  # Pool results
  mira_models <- as.mira(models)
  pooled_results <- pool(mira_models)
  mira_models_svy <- as.mira(models_svy)
  pooled_results_svy <- pool(mira_models)
  
  # Summarize results
  pooled_summary <- summary(pooled_results)
  pooled_summary_svy <- summary(pooled_results_svy)
  
  # Format into dataframe
  pooled_results_df <- data.frame(
    term = pooled_summary$term,
    estimate = pooled_summary$estimate,
    std.error = pooled_summary$std.error,
    or = exp(pooled_summary$estimate),
    statistic = pooled_summary$statistic,
    p.value = pooled_summary$p.value,
    conf.low = pooled_summary$estimate - 1.96 * pooled_summary$std.error,
    conf.high = pooled_summary$estimate + 1.96 * pooled_summary$std.error,
    significant = pooled_summary$p.value < 0.05  # TRUE/FALSE for significance
  )
  
  pooled_results_df_svy <- data.frame(
    term = pooled_summary_svy$term,
    estimate = pooled_summary_svy$estimate,
    std.error = pooled_summary_svy$std.error,
    or = exp(pooled_summary_svy$estimate),
    statistic = pooled_summary_svy$statistic,
    p.value = pooled_summary_svy$p.value,
    conf.low = pooled_summary_svy$estimate - 1.96 * pooled_summary_svy$std.error,
    conf.high = pooled_summary_svy$estimate + 1.96 * pooled_summary_svy$std.error,
    significant = pooled_summary_svy$p.value < 0.05  # TRUE/FALSE for significance
  )
  
  # Store results for the year
  pooled_results_all_years[[year]] <- pooled_results_df
  pooled_results_all_years_svy[[year]] <- pooled_results_df_svy
}

# Print final results
for (year in names(pooled_results_all_years)) {
  file_name <- paste0("tables/", year, "glm.csv")
  write.csv(pooled_results_all_years[[year]], file = file_name, row.names = FALSE)
  file_name_svy <- paste0("tables/", year, "svyglm.csv")
  write.csv(pooled_results_all_years_svy[[year]], file = file_name_svy, row.names = FALSE)
  
  cat("\nSaved results for", year, "to", file_name, "\n")
}

# # Check vif 
# X <- model.matrix(models[[1]])[, -1]  # Removes intercept column
# # Extract survey weights
# w <- dat_hyp_2017_final[[1]]$svy_weight_mec  
# # Compute VIF
# print(svyvif(models[[1]], X, w))




# 
# 
> print(direct_causes_2013)
$G
HIQ011              OHQ845              PFQ049              PFQ054 
TRUE               FALSE                TRUE               FALSE 
cc_diabetes              cc_ckd          cc_cvd_chd       cc_cvd_stroke 
FALSE                TRUE               FALSE               FALSE 
cc_cvd_hf          cc_cvd_any        LBXSBU_resid        LBXSCH_resid 
FALSE               FALSE               FALSE                TRUE 
LBDMONO_resid      LBXRBCSI_resid phq9_category_resid        cc_bmi_resid 
FALSE               FALSE               FALSE               FALSE 
FSDAD_resid        WHQ030_resid 
FALSE               FALSE 

$zMin
[1] 3.8148788 1.6410033 3.4745797 0.5102130 1.4034016 7.2168300 1.7787406 1.7643376
[9] 1.7569715 1.7692688 0.8840009 5.3219704 0.9091544 1.8412733 0.1487073 1.9362067
[17] 1.0854345 1.6099408
# > print(direct_causes_2015)
$G
HIQ011         cc_diabetes              cc_ckd          cc_cvd_chd 
TRUE               FALSE                TRUE                TRUE 
cc_cvd_hf          cc_cvd_any weight_change_resid         LBXTC_resid 
FALSE               FALSE               FALSE                TRUE 
phq9_category_resid        cc_bmi_resid         FSDAD_resid      cc_smoke_resid 
FALSE                TRUE               FALSE               FALSE 

$zMin
[1] 3.3274910 1.5384707 7.6874310 3.0674844 0.3234180 0.3385707 1.9437050 5.2552383
[9] 0.2296232 2.2629432 1.4143949 0.3306090
# 
$G
HIQ011         cc_diabetes              cc_ckd        URXUMS_resid 
TRUE               FALSE                TRUE                TRUE 
LBXSBU_resid        LBXSCH_resid        LBXSTR_resid       LBDMONO_resid 
TRUE               FALSE               FALSE               FALSE 
LBXBPB_resid         LBXTC_resid phq9_category_resid        cc_bmi_resid 
TRUE                TRUE               FALSE                TRUE 
WHQ030_resid 
FALSE 

$zMin
[1] 3.8072174 1.7006872 3.4328386 4.5180746 3.5157364 1.0307871 0.7402599 1.5049523
[9] 2.8359868 5.2621229 1.9518518 2.8679429 0.9655353
# > print(direct_causes_2021)
# $G
# HIQ011              KIQ022              OHQ845          cc_cvd_any 
# FALSE                TRUE                TRUE               FALSE 
# cc_diabetes        BMXBMI_resid      LBXRBCSI_resid        LBXBPB_resid 
# FALSE               FALSE                TRUE                TRUE 
# LBXTHG_resid         LBXTC_resid       LBDMONO_resid weight_change_resid 
# FALSE               FALSE               FALSE               FALSE 
# 
# $zMin
# [1] 1.6666436 2.0237530 2.3635559 1.0938308 0.7821955 1.9538986 2.2908685 3.6601798 1.9355163
# [10] 1.9019292 1.7891868 0.8027584