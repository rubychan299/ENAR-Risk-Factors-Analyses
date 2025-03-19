library(readxl)
library(readr)

load("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/cleaned/dat_hyp_mice.RData")
load("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/top_features_ES.RData")

# Use only JNC7 features
features_en_jnc <- bind_cols(top_features_2013, top_features_2015, top_features_2017) %>% 
  mutate(across(everything(), ~ str_remove(.x, ".Yes$")))
colnames(features_en_jnc) <- c("2013", "2015", "2017")


# features_2013 <- union(unique(features_en_jnc$`2013`), unique(fearures_xgb_jnc$`2013`))
# features_2015 <- union(unique(features_en_jnc$`2015`), unique(fearures_xgb_jnc$`2015`))
# features_2017 <- union(unique(features_en_jnc$`2017`), unique(fearures_xgb_jnc$`2017`))

features_en_jnc$`2013`
features_en_jnc$`2015`
features_en_jnc$`2017`

features_2013 <- c(
  "cc_ckd", "cc_cvd_any",
  "HIQ011", "cc_cvd_hf", "LBXTC",
  "PFQ049", "cc_diabetes", "cc_bmi",
  "PFQ054",  "phq9_category",
  "WHQ030", "OHQ845",
  "FSDAD", "race", 
  "LBXSBU", "WHQ030","cc_cvd_chd",
  "cc_cvd_stroke", "demo_age_years", 
  "LBXSBU","LBXRBCSI","LBDMONO",
  "demo_gender",
  "SEQN", "svy_weight_mec", 
  "svy_psu", "svy_strata", "bp_control_jnc7")
  
  
dat_hyp_2013_final <- lapply(dat_hyp_mice_2013, function(x) x[, colnames(x) %in% features_2013])


features_2015 <- c(
  "cc_ckd", "MCQ080",
  "cc_cvd_chd", "SLQ050", "cc_cvd_hf",
  "HIQ011", "PFQ051", "LBXTC",
  "LBXTC", "demo_gender","demo_age_years", 
  "FSDHH", "race", "cc_bmi","cc_smoke", "race",
  "cc_cvd_any", "race", "FSDAD",
  "FSDAD", "phq9_category", "cc_diabetes","weight_change",
  "DBQ700","SEQN", "svy_weight_mec", 
  "svy_psu", "svy_strata", "bp_control_jnc7")

dat_hyp_2015_final <- lapply(dat_hyp_mice_2015, function(x) x[, colnames(x) %in% features_2015])


features_2017 <- c(
  "cc_ckd", "HIQ011", "SLQ050",
  "cc_bmi", "LBXSBU", "race", "cc_diabetes",
  "demo_gender","demo_age_years", 
  "MCQ080", "LBXSTR", "LBXBPB", "PAQ605",
  "WHQ030", "LBXSUA", 
  "PAQ620", "LBXTC", "URXUMS",
  "LBXTHG", "KIQ022", "phq9_category", "URXUCR",
  "LBDMONO", "SEQN", "svy_weight_mec", 
  "svy_psu", "svy_strata", "bp_control_jnc7"
)

dat_hyp_2017_final <- lapply(dat_hyp_mice_2017, function(x) x[, colnames(x) %in% features_2017])

save(dat_hyp_2013_final, dat_hyp_2015_final, dat_hyp_2017_final, file = "data/cleaned/2013_to_2023_cleaned/dat_hyp_final_2013_2020.RData")


