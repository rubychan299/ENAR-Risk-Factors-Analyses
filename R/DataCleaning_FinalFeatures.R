library(readxl)
library(readr)

# Use only JNC7 features
features_en_jnc_2013 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/top_features_2013.csv")
features_en_jnc_2015 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/top_features_2015.csv")
features_en_jnc_2017 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/top_features_2017.csv")

features_en_jnc <- bind_cols(features_en_jnc_2013, features_en_jnc_2015, features_en_jnc_2017) %>% 
  mutate(across(everything(), ~ str_remove(.x, ".Yes$")))
colnames(features_en_jnc) <- c("2013", "2015", "2017")

features_xgb_jnc_2013 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/final_features_for_gb_2013.csv")
features_xgb_jnc_2015 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/final_features_for_gb_2015.csv")
features_xgb_jnc_2017 <- read_csv("data/cleaned/2013_to_2023_cleaned/features_xgboost/jnc7/final_features_for_gb_2017.csv")

fearures_xgb_jnc <- bind_cols(features_xgb_jnc_2013, features_xgb_jnc_2015, features_xgb_jnc_2017) %>% 
  dplyr::select(-contains("Count")) %>% 
  mutate(across(everything(), ~ str_remove(.x, "_Yes$")))

colnames(fearures_xgb_jnc) <- c("2013", "2015", "2017")


# features_2013 <- union(unique(features_en_jnc$`2013`), unique(fearures_xgb_jnc$`2013`))
# features_2015 <- union(unique(features_en_jnc$`2015`), unique(fearures_xgb_jnc$`2015`))
# features_2017 <- union(unique(features_en_jnc$`2017`), unique(fearures_xgb_jnc$`2017`))

features_2013 <- intersect(unique(features_en_jnc$`2013`), unique(fearures_xgb_jnc$`2013`))
features_2015 <- intersect(unique(features_en_jnc$`2015`), unique(fearures_xgb_jnc$`2015`))
features_2017 <- intersect(unique(features_en_jnc$`2017`), unique(fearures_xgb_jnc$`2017`))


# features_2013 <- c(
#   "bp_med_ace", "cc_ckd", "bp_med_diur_thz", "bp_med_angioten",
#   "cc_diabetes", "bp_med_beta", "bp_med_ccb", 
#   "cc_cvd_ascvd", "bp_med_diur_loop", "cc_acr", "cc_cvd_any",
#   "demo_age_years", "bp_med_central", "LBDHDD", "cc_smoke",
#   "demo_gender", "LBDMONO", "LBXRBCSI",
#   "demo_race", "LBXBSE", "cc_egfr",
#   "bp_med_renin_inhibitors", "cc_hba1c", "INDFMMPI", "URXUMS",
#   "BMXBMI", "LBXHGB", "URXUCR.x.x.x",
#   "LBXCOT", "LBXBPB", "LBDEONO", "LBXBCD", "SEQN", "svy_weight_mec", 
#   "svy_psu", "svy_strata", "bp_control_jnc7")

features_2013 <- c("bp_med_ace", "cc_ckd", "bp_med_diur_thz", "bp_med_angioten",
                   "cc_diabetes", "bp_med_beta", "bp_med_ccb", "cc_acr",
                   "demo_age_years", "LBDHDD", "LBDMONO", "LBXRBCSI",
                   "LBXBSE", "cc_egfr", "SEQN", "svy_weight_mec", 
                   "svy_psu", "svy_strata", "bp_control_jnc7")

dat_hyp_2013_final <- lapply(dat_hyp_mice_2013, function(x) x[, features_2013])

# features_2015 <- c(
#   "bp_med_ace", "cc_ckd", "bp_med_angioten", "bp_med_diur_thz",
#   "bp_med_beta", "cc_diabetes", "cc_cvd_any", "demo_gender",
#   "bp_med_diur_loop", "cc_cvd_mi", "demo_race",
#   "bp_med_central", "cc_smoke", "URXUMS",
#   "weight_change", "cc_acr", "LBDEONO",
#   "LBXBSE", "demo_age_years", "WHD050", "LBXCOT",
#   "cc_hba1c", "INDFMMPI",
#   "bp_med_ccb", "LBXBCD", "BMXBMI", "cc_egfr",
#   "LBXHGB", "LBXBPB", "LBDHDD", "LBXTHG",
#   "URXUCR.x.x.x", "LBDMONO","SEQN", "svy_weight_mec", 
#   "svy_psu", "svy_strata", "bp_control_jnc7"
# )
# 
# dat_hyp_2015_final <- lapply(dat_hyp_mice_2015, function(x) x[, features_2015])
# 
# features_2017 <- c(
#   "bp_med_ace", "bp_med_diur_thz", "bp_med_angioten", "cc_ckd",
#   "cc_diabetes", "bp_med_beta", "bp_med_central", "bp_med_diur_loop",
#   "demo_race", "bp_med_ccb", "LBXTHG",
#   "LBXBPB", "URXUMS", "cc_egfr", "URXUCR.x.x.x",
#   "cc_acr", "cc_cvd_mi", "demo_age_years",
#   "cc_cvd_ascvd", "LBDHDD", "BMXBMI", "LBXBSE",
#   "LBXHGB", "cc_hba1c", "INDFMMPI",
#   "weight_change", "LBXBCD", "LBXRBCSI", "phq9",
#   "demo_gender", "cc_cvd_any", "SEQN", "svy_weight_mec", 
#   "svy_psu", "svy_strata", "bp_control_jnc7"
# )

features_2015 <- c(
  "bp_med_ace", "bp_med_angioten", "bp_med_diur_thz", "bp_med_beta",
  "cc_diabetes", "URXUMS", "weight_change", "cc_acr",
  "LBDEONO", "LBXBSE", "demo_age_years", "LBXCOT","SEQN", "svy_weight_mec", 
  "svy_psu", "svy_strata", "bp_control_jnc7"
)

dat_hyp_2015_final <- lapply(dat_hyp_mice_2015, function(x) x[, features_2015])

features_2017 <- c(
  "bp_med_ace", "bp_med_diur_thz", "bp_med_angioten", "cc_ckd",
  "cc_diabetes", "bp_med_beta", "bp_med_ccb", "LBXBPB",
  "URXUMS", "cc_acr", "demo_age_years", "cc_cvd_ascvd",
  "LBDHDD", "BMXBMI", "LBXHGB", "SEQN", "svy_weight_mec", 
  "svy_psu", "svy_strata", "bp_control_jnc7"
)

dat_hyp_2017_final <- lapply(dat_hyp_mice_2017, function(x) x[, features_2017])

save(dat_hyp_2013_final, dat_hyp_2015_final, dat_hyp_2017_final, file = "data/cleaned/2013_to_2023_cleaned/dat_hyp_final_2013_2020.RData")


