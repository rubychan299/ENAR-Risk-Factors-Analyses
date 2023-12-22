library(survey)
library(gtsummary)
library(tidyverse)
library(haven)
library(remotes)
library(summarytools)
library(miceRanger)
library(doParallel)
library(caret)

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

### discuss if need to remove:
# bp_control_140_90 bp_uncontrolled_140_90               htn_jnc7 
# TRUE                   TRUE                   TRUE 
# htn_escesh                DRABF.y               DRDINT.y 
# TRUE                   TRUE                   TRUE 
# DRDINT.y.y               BMIRECUM                BMXHEAD 
# TRUE                   TRUE                   TRUE 
# BMIHEAD                 BPXCHR               LBDFERSI 
# TRUE                   TRUE                   TRUE 
# LBDBMNLC                 DBQ010                 DBD030 
# TRUE                   TRUE                   TRUE 
# DBD050 
# TRUE

## Missing Imputation####

## remove vars with more than 50% missing
dat_hyp_cleaned <- dat_full_hyp %>% select(where(~mean(is.na(.)) < 0.5))
dat_hyp_cleaned <- as.data.frame(dat_hyp_cleaned)

## perform mice with RF(fast?)
set.seed(2024)
cl <- makeCluster(4)
registerDoParallel(cl)
miceObj <- miceRanger(dat_hyp_cleaned, m=6, parallel = TRUE, verbose = FALSE)
stopCluster(cl)
registerDoSEQ()

dat_hyp_cleaned <- completeData(miceObj)

## Data Transformation####
continuous_vars <- sapply(dat_hyp_cleaned, is.numeric)
categorical_vars <- !continuous_vars  # Assuming non-numeric are categorical

preproc_values <- preProcess(dat_hyp_cleaned[, continuous_vars], method = c("center", "scale"))
df_scaled <- predict(preproc_values, dat_hyp_cleaned[, continuous_vars])

df_factor <- as.data.frame(lapply(dat_hyp_cleaned[, categorical_vars], as.factor))
df_dummy <- dummyVars("~ .", data = df_factor)
df_encoded <- predict(df_dummy, df_factor)

dat_hyp_final <- cbind(df_scaled, df_encoded)


## Survey Design####
dat_hyp_svy <- svydesign(
  data = dat_hyp_final, 
  ids = ~svy_psu,
  strata = ~svy_strata,
  weights = ~svy_weight_mec, 
  nest = T
)

weights <- weights(dat_hyp_svy)

## Table 1####
table1 <- tbl_svysummary(dat_hyp_svy,
                         by = "svy_year")

table1 %>%
  as_gt() %>%
  gt::gtsave(filename = "tables/table1_byyrs_full.html")
