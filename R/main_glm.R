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
           demo_age_years, demo_gender, demo_race, bp_med_use, HIQ011,
           cc_cvd_any, cc_diabetes, LBXBPB, LBXTC,
           phq9_category, cc_bmi)
})
           
           

data_list_years <- list(
  "2013" = dat_hyp_2013_glm,
  "2015" = dat_hyp_2015_glm,
  "2017" = dat_hyp_2017_glm,
  "2021" = dat_hyp_2021_glm
)

name_map <- c(
  "bp_control_jnc7" = "BP control",
  "bp_med_use" = "Antihypertensive \nMedication Use",
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

# Stratfied by Meds####

dat_hyp_2013_glm <- lapply(dat_hyp_2013_final, function(x){
  x <- x %>% 
    select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
           demo_age_years, demo_gender, race, 
           HIQ011, OHQ845, PFQ049, PFQ054, 
           cc_diabetes, cc_ckd, cc_cvd_chd, cc_cvd_stroke, cc_cvd_hf, cc_cvd_any, 
           LBXSBU, LBDMONO, LBXRBCSI, LBXTC, 
           phq9_category, cc_bmi, FSDAD, WHQ030,bp_med_use)
})

dat_hyp_2015_glm <- lapply(dat_hyp_2015_final, function(x){
  x <- x %>% 
    select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
           demo_age_years, demo_gender,race,
           HIQ011, cc_diabetes, cc_ckd, cc_cvd_chd,cc_cvd_hf, cc_cvd_any,
           weight_change, LBXTC, phq9_category, cc_bmi, FSDAD, cc_smoke,bp_med_use)
})


dat_hyp_2017_glm <- lapply(dat_hyp_2017_final, function(x){
  x <- x %>% 
    select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
           demo_age_years, demo_gender, race,
           HIQ011, cc_diabetes, cc_ckd, URXUMS, LBXSBU, LBXSTR, LBDMONO,
           LBXBPB, LBXTC, phq9_category, cc_bmi, WHQ030, bp_med_use)
})

dat_hyp_2021_glm <- lapply(dat_hyp_2021_final, function(x){
  x <- x %>% 
    select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
           demo_age_years, demo_gender, demo_race, bp_med_use, HIQ011,
           cc_cvd_any, cc_diabetes, LBXBPB, LBXTC,
           phq9_category, cc_bmi, bp_med_use)
})


data_list_years <- list(
  "2013" = dat_hyp_2013_glm,
  "2015" = dat_hyp_2015_glm,
  "2017" = dat_hyp_2017_glm,
  "2021" = dat_hyp_2021_glm
)

# Store results
pooled_results_all_years <- list()
pooled_results_all_years_svy <- list()

for (year in names(data_list_years)) {
  data_list <- data_list_years[[year]]
  data_list_yes <- lapply(data_list, function(x){
    x <- x %>% 
      filter(bp_med_use == "Yes") %>% 
      select(-bp_med_use)
  })
  data_list_no <- lapply(data_list, function(x){
    x <- x %>% 
      filter(bp_med_use == "No") %>% 
      select(-bp_med_use)
  })
  models_yes <- models_no <- list()
  models_svy_yes <- models_svy_no <- list()
  
  for (i in seq_along(data_list)) {
    data_yes <- data_list_yes[[i]]
    data_no <- data_list_no[[i]]
    # Extract all variable names, excluding survey design variables
    predictors <- setdiff(names(data_yes), c("SEQN", "bp_control_jnc7", "svy_weight_mec", "svy_strata", "svy_psu"))
    
    # Construct formula dynamically
    formula <- as.formula(paste("bp_control_jnc7 ~", paste(predictors, collapse = " + ")))
    
    # Define survey design
    svydesign_obj_yes <- svydesign(
      id = ~svy_psu,
      strata = ~svy_strata,
      weights = ~svy_weight_mec,
      data = data_yes,
      nest = TRUE
    )
    
    svydesign_obj_no <- svydesign(
      id = ~svy_psu,
      strata = ~svy_strata,
      weights = ~svy_weight_mec,
      data = data_no,
      nest = TRUE
    )
    
    # Fit model
    models_svy_yes[[i]] <- svyglm(formula, design = svydesign_obj_yes, family = quasibinomial(link = "logit"),
                              control = list(maxit = 500))
    models_svy_no[[i]] <- svyglm(formula, design = svydesign_obj_no, family = quasibinomial(link = "logit"),
                              control = list(maxit = 500))
    
    models_yes[[i]] <- glm(formula, data = data_yes, family = quasibinomial(link = "logit"),
                       control = list(maxit = 500))
    models_no[[i]] <- glm(formula, data = data_no, family = quasibinomial(link = "logit"),
                           control = list(maxit = 500))
    
  }
  
  # Pool results
  mira_models_yes <- as.mira(models_yes)
  pooled_results_yes <- pool(mira_models_yes)
  mira_models_no <- as.mira(models_no)
  pooled_results_no <- pool(mira_models_no)
  mira_models_svy_yes <- as.mira(models_svy_yes)
  pooled_results_svy_yes <- pool(mira_models_yes)
  mira_models_svy_no <- as.mira(models_svy_no)
  pooled_results_svy_no <- pool(mira_models_no)
  
  # Summarize results
  pooled_summary_yes <- summary(pooled_results_yes)
  pooled_summary_no <- summary(pooled_results_no)
  pooled_summary_svy_yes <- summary(pooled_results_svy_yes)
  pooled_summary_svy_no <- summary(pooled_results_svy_no)
  
  # Format into dataframe
  pooled_results_df_yes <- data.frame(
    term = pooled_summary_yes$term,
    estimate = pooled_summary_yes$estimate,
    std.error = pooled_summary_yes$std.error,
    or = exp(pooled_summary_yes$estimate),
    statistic = pooled_summary_yes$statistic,
    p.value = pooled_summary_yes$p.value,
    conf.low = pooled_summary_yes$estimate - 1.96 * pooled_summary_yes$std.error,
    conf.high = pooled_summary_yes$estimate + 1.96 * pooled_summary_yes$std.error,
    significant = pooled_summary_yes$p.value < 0.05  # TRUE/FALSE for significance
  )
  
  pooled_results_df_no <- data.frame(
    term = pooled_summary_no$term,
    estimate = pooled_summary_no$estimate,
    std.error = pooled_summary_no$std.error,
    or = exp(pooled_summary_no$estimate),
    statistic = pooled_summary_no$statistic,
    p.value = pooled_summary_no$p.value,
    conf.low = pooled_summary_no$estimate - 1.96 * pooled_summary_no$std.error,
    conf.high = pooled_summary_no$estimate + 1.96 * pooled_summary_no$std.error,
    significant = pooled_summary_no$p.value < 0.05  # TRUE/FALSE for significance
  )
  
  
  pooled_results_df_svy_yes <- data.frame(
    term = pooled_summary_svy_yes$term,
    estimate = pooled_summary_svy_yes$estimate,
    std.error = pooled_summary_svy_yes$std.error,
    or = exp(pooled_summary_svy_yes$estimate),
    statistic = pooled_summary_svy_yes$statistic,
    p.value = pooled_summary_svy_yes$p.value,
    conf.low = pooled_summary_svy_yes$estimate - 1.96 * pooled_summary_svy_yes$std.error,
    conf.high = pooled_summary_svy_yes$estimate + 1.96 * pooled_summary_svy_yes$std.error,
    significant = pooled_summary_svy_yes$p.value < 0.05  # TRUE/FALSE for significance
  )
  
  pooled_results_df_svy_no <- data.frame(
    term = pooled_summary_svy_no$term,
    estimate = pooled_summary_svy_no$estimate,
    std.error = pooled_summary_svy_no$std.error,
    or = exp(pooled_summary_svy_no$estimate),
    statistic = pooled_summary_svy_no$statistic,
    p.value = pooled_summary_svy_no$p.value,
    conf.low = pooled_summary_svy_no$estimate - 1.96 * pooled_summary_svy_no$std.error,
    conf.high = pooled_summary_svy_no$estimate + 1.96 * pooled_summary_svy_no$std.error,
    significant = pooled_summary_svy_no$p.value < 0.05  # TRUE/FALSE for significance
  )
  
  pooled_results_df <- bind_cols(pooled_results_df_yes, pooled_results_df_no)
  pooled_results_df_svy <- bind_cols(pooled_results_df_svy_yes, pooled_results_df_svy_no)
  
  # Store results for the year
  pooled_results_all_years[[year]] <- pooled_results_df
  pooled_results_all_years_svy[[year]] <- pooled_results_df_svy
}

# Print final results
for (year in names(pooled_results_all_years)) {
  file_name <- paste0("tables/", year, "glm_med.csv")
  write.csv(pooled_results_all_years[[year]], file = file_name, row.names = FALSE)
  file_name_svy <- paste0("tables/", year, "svyglm_med.csv")
  write.csv(pooled_results_all_years_svy[[year]], file = file_name_svy, row.names = FALSE)
  
  cat("\nSaved results for", year, "to", file_name, "\n")
}


# Parsimonious model####

# 2013
# kick out: cc_cvd_stroke, cc_cvd_chd, LBXSBU, LBDMONO

# 2015
# kick out: phq9_category, weight_change, cc_cvd_hf

# 2017
# kick out: cc_diabetes

# 2021
# keep everything
  
# Define datasets
dat_hyp_2013_glm_sub <- lapply(dat_hyp_2013_glm, function(x){
  x <- x %>% 
    select(-cc_cvd_stroke, -cc_cvd_chd, -LBXSBU, -LBDMONO)
})

dat_hyp_2015_glm_sub <- lapply(dat_hyp_2015_glm, function(x){
  x <- x %>% 
    select(-phq9_category, -weight_change, -cc_cvd_hf)
})

dat_hyp_2017_glm_sub <- lapply(dat_hyp_2017_glm, function(x){
  x <- x %>% 
    select(-cc_diabetes)
})

# dat_hyp_2017_glm_sub <- lapply(dat_hyp_2021_final, function(x){
#   x <- x %>% 
#     select(SEQN, bp_control_jnc7, svy_weight_mec, svy_strata, svy_psu, 
#            demo_age_years, demo_gender, demo_race, OHQ845,
#            cc_cvd_any, cc_diabetes, LBXRBCSI, LBXBPB, LBXTC,LBDMONO,
#            weight_change, cc_smoke, cc_bmi)
# })

data_list_years <- list(
  "2013" = dat_hyp_2013_glm_sub,
  "2015" = dat_hyp_2015_glm_sub,
  "2017" = dat_hyp_2017_glm_sub
  # "2021" = dat_hyp_2021_glm_sub
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
  file_name <- paste0("tables/", year, "glm_sub.csv")
  write.csv(pooled_results_all_years[[year]], file = file_name, row.names = FALSE)
  file_name_svy <- paste0("tables/", year, "svyglm_sub.csv")
  write.csv(pooled_results_all_years_svy[[year]], file = file_name_svy, row.names = FALSE)
  
  cat("\nSaved results for", year, "to", file_name, "\n")
}
