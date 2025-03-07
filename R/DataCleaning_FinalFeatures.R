library(readxl)
library(readr)

# Use only JNC7 features
features_en_jnc <- read_csv("data/cleaned/2013_to_2023_cleaned/top_features_long_format_jnc7.csv")

features_en_jnc <- features_en_jnc %>%
  # Remove everything after "." in Feature
  mutate(Feature = str_remove(Feature, "\\..*")) %>%
  
  # Remove Value column
  select(-Value) %>%
  
  # Create row index within each year to align side-by-side
  group_by(Year) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  
  # Transform to wide format by Year
  pivot_wider(names_from = Year, values_from = Feature) %>%
  
  # Remove the row_id column
  select(-row_id, -Metric)


features_xgb_jnc_2013 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/final_features_for_gb2013.csv")
features_xgb_jnc_2015 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/final_features_for_gb2015.csv")
features_xgb_jnc_2017 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/final_features_for_gb2017.csv")

fearures_xgb_jnc <- bind_cols(features_xgb_jnc_2013, features_xgb_jnc_2015, features_xgb_jnc_2017) %>% 
  dplyr::select(-contains("Count")) %>% 
  mutate(across(everything(), ~ str_remove(.x, "_Yes$")))

colnames(fearures_xgb_jnc) <- c("2013", "2015", "2017")


features_2013 <- union(unique(features_en_jnc$`2013`), unique(fearures_xgb_jnc$`2013`))
features_2015 <- union(unique(features_en_jnc$`2015`), unique(fearures_xgb_jnc$`2015`))
features_2017 <- union(unique(features_en_jnc$`2017`), unique(fearures_xgb_jnc$`2017`))

features_2013 <- c(
  "htn_resistant_jnc7", "bp_med_use", "bp_med_n_class", "bp_med_vasod",
  "htn_aware", "cc_diabetes", "cc_acr_gteq30", "bp_med_ccb_ndh",
  "bp_med_diur_Ksparing", "bp_med_ccb_dh", "demo_race",
  "cc_ckd", "bp_med_ace", "bp_med_aldo", "bp_med_diur_thz",
  "bp_med_diur_loop",  "demo_age_years", "cc_cvd_any",
  "cc_cvd_chd", "cc_hba1c", "cc_acr", "demo_race",
  "LBXHGB", "LBXTC", "URXUMS", "INDFMMPI", "BMXBMI", "cc_egfr",
  "LBDMONO", "LBXRBCSI", "LBXBCD", "LBXBPB", "bp_med_n_pills",
  "LBDHDD", "bp_med_angioten", "LBXCOT", "SEQN", "svy_weight_mec", 
  "svy_psu", "svy_strata", "bp_control_jnc7")

dat_hyp_2013_final <- lapply(dat_hyp_mice_2013, function(x) x[, features_2013])

features_2015 <- c(
  "htn_resistant_jnc7", "bp_med_n_class", "bp_med_use", "bp_med_alpha",
  "htn_aware", "cc_ckd", "bp_med_ace",
  "cc_diabetes", "cc_cvd_hf", "bp_med_n_pills", "cc_acr_gteq30",
  "demo_race", "bp_med_ccb_ndh", "bp_med_aldo", "bp_med_diur_thz",
  "bp_med_diur_loop", "LBXTC", "demo_gender", "cc_acr",
  "demo_age_years", "cc_hba1c", "LBXBSE",
  "INDFMMPI", "cc_egfr", "URXUMS", "weight_change",
  "BMXBMI", "LBXCOT", "LBXBPB", "LBDHDD",
  "LBDEONO", "LBDMONO", "LBXRBCSI",
  "LBXHGB", "URXUCR.x.x.x","SEQN", "svy_weight_mec", 
  "svy_psu", "svy_strata", "bp_control_jnc7"
)

dat_hyp_2015_final <- lapply(dat_hyp_mice_2015, function(x) x[, features_2015])

features_2017 <- c(
  "htn_resistant_jnc7", "bp_med_use", "bp_med_n_class", "htn_aware",
  "bp_med_aldo", "bp_med_diur_loop", "cc_acr_gteq30", "bp_med_n_pills",
  "bp_med_ace", "bp_med_central", "cc_diabetes", "bp_med_diur_Ksparing",
  "bp_med_diur_thz", "demo_race", "bp_med_beta",
  "bp_med_vasod", "LBXTHG", "cc_smoke", "cc_acr",
  "cc_hba1c", "demo_age_years", "INDFMMPI", "cc_ckd",
  "WHD020", "LBXHGB", "LBDHDD", "LBXWBCSI", "phq9",
  "cc_egfr", "LBDMONO", "LBXBPB", "LBXBSE",
  "URXUCR.x.x.x", "WHD050", "cc_smoke", "SEQN", "svy_weight_mec", 
  "svy_psu", "svy_strata", "bp_control_jnc7"
)

dat_hyp_2017_final <- lapply(dat_hyp_mice_2017, function(x) x[, features_2017])

save(dat_hyp_2013_final, dat_hyp_2015_final, dat_hyp_2017_final, file = "data/cleaned/2013_to_2023_cleaned/dat_hyp_final_2013_2020.RData")


