library(survey)
library(gtsummary)
library(tidyverse)
library(haven)
library(remotes)
library(summarytools)
library(miceRanger)
library(doParallel)
library(caret)
library(mice)

# Check Original Data####

## Loading Data####
library(cardioStatsUSA)
data(nhanes_data)

# for blood pressure and hypertension sub-population
summary(nhanes_data$svy_strata[nhanes_data$svy_subpop_htn == 1])
table(nhanes_data$svy_subpop_htn, nhanes_data$svy_subpop_chol)

# for lipids and cholesterol sub-population
nhanes_data[svy_subpop_chol == 1]

## Survey Design####
# nhanes_svy <- svydesign(
#   data = nhanes_data[nhanes_data$svy_subpop_htn == 1,], 
#   ids = ~svy_psu,
#   strata = ~svy_strata,
#   weights = ~svy_weight_mec, 
#   nest = T
# )
# 
# summary(nhanes_svy)

## Table 1####

# table1 <- tbl_svysummary(nhanes_svy)
# table1 <- tbl_svysummary(nhanes_svy,
#                          by = "svy_year")
# 
# table1 %>%
#   as_gt() %>%
#   gt::gtsave(filename = "tables/table1_byyrs.html")

# Merge with External Data####

## Load External Data####
# Note: PAHS_H(lab), PAXRAW_C, PAXRAW_D, SPXRAW_E, SPXRAW_F, SPXRAW_G(exam) not avaliable

dat.sources <-  list.files("data/cleaned", 
                           pattern="*_df.RData$", full.names=TRUE, 
                           ignore.case=TRUE)
sapply(dat.sources, load, .GlobalEnv)

# Merge with cardioStatsUSA
nhanes_data <- rename(nhanes_data, SEQN = svy_id)

table(nhanes_data$svy_year)

nhanes_data <- nhanes_data %>% 
  mutate(Year = case_when(svy_year == "1999-2000" ~ "1999",
                          svy_year == "2001-2002" ~ "2001",
                          svy_year == "2003-2004" ~ "2003",
                          svy_year == "2005-2006" ~ "2005",
                          svy_year == "2007-2008" ~ "2007",
                          svy_year == "2009-2010" ~ "2009",
                          svy_year == "2011-2012" ~ "2011",
                          svy_year == "2013-2014" ~ "2013",
                          svy_year == "2015-2016" ~ "2015",
                          svy_year == "2017-2020" ~ "P",))

dat_full <- nhanes_data %>% 
  full_join(di_df, by = c('SEQN','Year')) %>% 
  full_join(ex_df, by = c('SEQN','Year')) %>% 
  full_join(lab_df, by = c('SEQN','Year')) %>% 
  full_join(ques_df, by = c('SEQN','Year'))

# Drop 1999-2000, 2001-2002 and 2017-2018

dat_full <- dat_full %>% 
  filter(Year != "1999" | Year != "2001" | Year!= "2017") %>% 
  select(-Year)

# Keep only people with hypertension

dat_full_hyp <- dat_full[dat_full$svy_subpop_htn == 1,]
# summary <- dfSummary(dat_full_hyp)
# view(summary)

# Check duplicate
d <- duplicated(t(dat_full_hyp))
d <- d[d == T]

# Drop variables and subset based on discussion
dat_hyp <- dat_full_hyp[dat_full_hyp$htn_accaha == "Yes",]

chols <- c("chol_ldl_5cat", "chol_nonhdl_5cat", "chol_hdl")

dat_hyp <- dat_hyp %>% 
  select(-contains("uncontrolled"), -contains("escesh"), -BMIRECUM,
         -BMXHEAD, -BMIHEAD, -BPXCHR, -LBDBMNLC, -DBQ010, -DBD030, -DBD050, -LBDLDL, -LBXTC,
         -chol_total, -chol_total_gteq_200,-chol_total_gteq_240, chol_hdl, -chol_hdl_low, 
         -chol_trig, -chol_trig_gteq_150, -chol_ldl, -chol_ldl_lt_70, -chol_ldl_gteq_70,
         -chol_ldl_gteq_190, -chol_nonhdl,-chol_nonhdl_lt_100, -chol_nonhdl_gteq_100, -chol_nonhdl_gteq_220)

# outcomes: bp_control_jnc7, bp_control_accaha, bp_control_140_90, bp_control_130_80

## Missing Imputation####

## remove vars with more than 50% missing
dat_hyp_cleaned <- dat_hyp %>% select(where(~mean(is.na(.)) < 0.5))
dat_hyp_cleaned <- as.data.frame(dat_hyp_cleaned)

# write.csv(dat_hyp_cleaned, "data/cleaned/dat_hyp_cleaned.csv", row.names=FALSE)

## perform mice with RF(fast?)
# set.seed(2024)
# cl <- makeCluster(4)
# registerDoParallel(cl)
# miceObj <- miceRanger(dat_hyp_cleaned, m=6, parallel = TRUE, verbose = FALSE)
# stopCluster(cl)
# registerDoSEQ()

# dat_hyp_cleaned <- completeData(miceObj)

## switch to mice package
miceObj <- mice(dat_hyp_cleaned, method = "cart")
dat_hyp_mice <- complete(miceObj)


# save(dat_hyp_cleaned, miceObj,dat_hyp_mice, file = "data/cleaned/dat_hyp_cleaned_mice.RData")
load("data/cleaned/dat_hyp_cleaned_mice.RData")

summary_mice <- dfSummary(dat_hyp_mice)
view(summary_mice)
## Data Transformation####

# continuous_vars <- sapply(dat_hyp_cleaned, is.numeric)
# categorical_vars <- !continuous_vars  # Assuming non-numeric are categorical

`%notin%` <- Negate(`%in%`)

continuous_vars <- dat_hyp_mice %>% select(demo_age_years, bp_sys_mean, bp_dia_mean, DSDCOUNT.x, DSDANCNT, DR1EXMER, DR1DAY, DS1DSCNT, DS1ANCNT, 
                                           BMXWT, BMXHT, BMXBMI, BMXLEG, BMXARML, BMXARMC, BMXWAIST, BPXPLS,
                                           BPXML1,BPXSY1, BPXDI1, BPXSY2, BPXDI2, BPXSY3, BPXDI3, LBXWBCSI,
                                           LBXLYPCT, LBXMOPCT, LBXNEPCT, LBXEOPCT, LBXBAPCT, LBDLYMNO, 
                                           LBDMONO, LBDNENO, LBDEONO, LBDBANO, LBXRBCSI, LBXHGB, LBXHCT,
                                           LBXMCVSI, LBXMCHSI, LBXMC, LBXRDW, LBXPLTSI, LBXMPSI, LBDTCSI,
                                           LBDHDD, LBDHDDSI, LBXGH, URXUMA, URXUMS, URXUCR, URXCRS,ALQ130,
                                           HOD050, WHD010, WHD020, WHD050, WHD110, WHD120, WHD130, WHD140)
categorical_vars <- as.data.frame(lapply(dat_hyp_mice[names(dat_hyp_mice) %notin% names(continuous_vars)], as.factor))

preproc_values <- preProcess(continuous_vars, method = c("center", "scale"))
df_scaled <- predict(preproc_values, continuous_vars)

svy_var <- dat_hyp_mice %>% select(SEQN,svy_weight_mec, svy_psu, svy_strata, svy_year)

categorical_vars <- categorical_vars %>% select(-svy_subpop_htn, -htn_accaha, -htn_jnc7,
                                                -SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -svy_year)

df_dummy <- dummyVars("~ .", data = categorical_vars)
df_encoded <- predict(df_dummy, categorical_vars)

dat_hyp_final <- cbind(svy_var, df_scaled, df_encoded)


## Survey Design####
dat_hyp_svy <- svydesign(
  data = dat_hyp_final, 
  ids = ~svy_psu,
  strata = ~svy_strata,
  weights = ~svy_weight_mec, 
  nest = T
)

weights <- weights(dat_hyp_svy)

save(dat_hyp_cleaned, dat_hyp_mice, dat_hyp_final,dat_hyp_svy,weights, file = "data/cleaned/dat_hyp_final_svy.RData")
