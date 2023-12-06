library(survey)
library(gtsummary)
library(tidyverse)
library(haven)

# Loading Data####
library(cardioStatsUSA)
data(nhanes_data)

# for blood pressure and hypertension sub-population
summary(nhanes_data$svy_strata[nhanes_data$svy_subpop_htn == 1])
table(nhanes_data$svy_subpop_htn, nhanes_data$svy_subpop_chol)

# for lipids and cholesterol sub-population
nhanes_data[svy_subpop_chol == 1]

# Survey Design####
nhanes_svy <- svydesign(
  data = nhanes_data[nhanes_data$svy_subpop_htn == 1,], 
  ids = ~svy_psu,
  strata = ~svy_strata,
  weights = ~svy_weight_mec, 
  nest = T
)

summary(nhanes_svy)

# Table 1####

table1 <- tbl_svysummary(nhanes_svy)
table1 <- tbl_svysummary(nhanes_svy,
                         by = "svy_year")

table1 %>%
  as_gt() %>%
  gt::gtsave(filename = "tables/table1_byyrs.html")

# Load External Data####
# Note: PAHS_H(lab), PAXRAW_C, PAXRAW_D, SPXRAW_E, SPXRAW_F, SPXRAW_G(exam) not avaliable
id <- nhanes_data$svy_id

dat.sources <-  list.files("data/cleaned", 
                               pattern="*.RData$", full.names=TRUE, 
                               ignore.case=TRUE)
sapply(dat.sources, load, .GlobalEnv)
