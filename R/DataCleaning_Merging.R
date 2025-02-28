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
# Support functions####

combine_datasets <- function(survey_list, ids_vector, id_column_name) {
  library(dplyr)
  
  final_datasets <- list()
  column_presence_count <- list()
  
  # Helper function to coerce character columns with numeric content to numeric
  coerce_numeric <- function(df) {
    num_coerced_df <- df
    for (col_name in names(df)) {
      if (is.character(df[[col_name]])) {
        suppressed_warnings <- suppressWarnings(as.numeric(df[[col_name]]))
        if (!any(is.na(suppressed_warnings))) {
          num_coerced_df[[col_name]] <- suppressed_warnings
        }
      }
    }
    return(num_coerced_df)
  }
  
  # Loop through each year
  for(year in names(survey_list)) {
    # Loop through each survey dataframe in the year
    for(survey_name in names(survey_list[[year]])) {
      
      # Skip if the dataset is NULL or doesn't have the ID column
      if(is.null(survey_list[[year]][[survey_name]]) || !(id_column_name %in% names(survey_list[[year]][[survey_name]]))) {
        next
      }
      
      # Subset and coerce numeric columns
      survey_subset <- survey_list[[year]][[survey_name]] %>%
        # filter(!!sym(id_column_name) %in% ids_vector) %>%
        coerce_numeric()
      
      # Skip this dataset if it becomes empty after subsetting
      if(nrow(survey_subset) == 0) {
        next
      }
      
      # Standardize the survey name
      standardized_name <- if (startsWith(survey_name, "P_")) {
        substring(survey_name, 3)
      } else {
        gsub(pattern = "_[A-Z]$", replacement = "", x = survey_name)
      }
      
      # Create an identifier for the year based on the survey_name
      survey_with_year <- survey_subset %>%
        mutate(Year = year)
      
      # Bind rows without checking for common columns first
      if(!standardized_name %in% names(final_datasets)) {
        final_datasets[[standardized_name]] <- survey_with_year
        column_presence_count[[standardized_name]] <- setNames(numeric(ncol(survey_with_year)), names(survey_with_year))
      } else {
        final_datasets[[standardized_name]] <- bind_rows(final_datasets[[standardized_name]], survey_with_year)
        
        # Count the presence of each column
        current_cols <- names(survey_with_year)
        column_presence_count[[standardized_name]][current_cols] <- column_presence_count[[standardized_name]][current_cols] + 1
      }
    }
  }
  
  # Filter out columns that don't have at least five years of data
  # for(dataset_name in names(final_datasets)) {
  #   cols_to_keep <- names(which(column_presence_count[[dataset_name]] >= 5))
  #   final_datasets[[dataset_name]] <- final_datasets[[dataset_name]][, cols_to_keep, drop = FALSE]
  # }
  
  # Remove NULL datasets from the final list
  final_datasets <- final_datasets[!sapply(final_datasets, is.null)]
  
  return(final_datasets)
}

remove_variable <- function(survey_list, var_name) {
  lapply(survey_list, function(year_list) {
    lapply(year_list, function(df) {
      if(!is.null(df) && var_name %in% names(df)) {
        dplyr::select(df, -!!sym(var_name))
      } else {
        df
      }
    })
  })
}


# Use nhanesA package####
library(nhanesA)
library(cardioStatsUSA)

years <- c(2013, 2015, 2017, 'P')

data(nhanes_data)
id <- nhanes_data$svy_id


source("R/funcs.R")

# Load the Data####

load("data/cleaned/Examinations.RData")
load("data/cleaned/Dietary.RData")
load("data/cleaned/Laboratory.RData")
load("data/cleaned/Questionaires.RData")
load("data/cleaned/Demographics.RData")

# Combine the data (2013 - 2020)####

ditables <- ditables[names(ditables) %in% years]
di_dat <- combine_datasets(ditables, id, 'SEQN')

labtables <- labtables[names(labtables) %in% years]
lab_dat <- combine_datasets(labtables, id, 'SEQN')

extables <- extables[names(extables) %in% years]
ex_dat <- combine_datasets(extables, id, 'SEQN')

qtables <- qtables[names(qtables) %in% years]
q_dat <- combine_datasets(qtables, id, 'SEQN')

demotables <- demotables[names(demotables) %in% years]
demo_dat <- combine_datasets(demotables, id, 'SEQN')

# Questionaire####
quesnames <- c("ACQ","BPQ", "CDQ", "DEQ", "DIQ","DBQ","RXQ", "ECQ", "HIQ", "AUQ", "HUQ", "IMQ",
               "VTQ","PUQ", "DPQ", "DUQ", "HSD","HSQ","ALQ", "IND", "INQ", "HOD", 
               "HOQ", "FSD", "CBD", "WHQ", "SMQ", "KIQ", "OCQ", "OHQ","PAQ", 
               "PFQ", "MCQ")

ques_dat_new <- q_dat[names(q_dat) %in% quesnames]

quesvars <- readxl::read_excel("data/FinalVars.xlsx", 
                               sheet = "QuesVars", col_names = FALSE)$`...1`
quesvars <- sapply(quesvars, function(x) strsplit(x, " - ")[[1]][1])

ques_dat_cleaned <- select_variable(ques_dat_new, quesvars)

ques_df <- combine_surveys(ques_dat_cleaned)

ques_df <- ques_df %>% 
  select(where(~mean(is.na(.)) < 0.5))

## Merge in 2021-2023####
# List all .xpt files with full path
qdata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/questionnaire", full.names = TRUE)

# Read each file and assign it to the global environment
lapply(qdata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

# Get all objects in the global environment
all_objects <- ls(envir = .GlobalEnv)

qdatasets <- grep("_L$", all_objects, value = TRUE)

datasets <- mget(qdatasets, envir = .GlobalEnv)

datasets <- select_variable(datasets, quesvars)

datasets <- lapply(datasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  # Add Year
  return(df)
})

# Perform full joins across all datasets
full_joined_data <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets)

# full_joined_data <- full_joined_data[colnames(full_joined_data) %in% colnames(ques_df)]

# Bind ques_df with the fully joined dataset
ques_df_joined <- full_join(ques_df, full_joined_data)

demodata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/demographics", full.names = TRUE)

# Read each file and assign it to the global environment
lapply(demodata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

# Get all objects in the global environment
all_objects <- ls(envir = .GlobalEnv)

# Filter objects that end with "_L"
demodata <-  grep("_L$", all_objects, value = TRUE)[!grep("_L$", all_objects, value = TRUE) %in% qdatasets]

demodatasets <- mget(demodata, envir = .GlobalEnv)

demodatasets <- lapply(demodatasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  # Add Year
  return(df)
})

# Perform full joins across all datasets
full_joined_data <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), demodatasets)


# Bind ques_df with the fully joined dataset
ques_df_joined <- full_join(ques_df_joined, full_joined_data)

# duplicate_seqn <- ques_df_joined %>%
#   group_by(SEQN, Year) %>%
#   filter(n() > 1)  # Keep only duplicates

## Recoding####

ques_df_joined <- ques_df_joined %>% 
  mutate(IMQ011 = case_when(
    IMQ011 %in% c(1,2) ~ "Yes",
    IMQ011 == 3 ~ "No",
    TRUE ~ NA_character_),
    IMQ020 = case_when(
      IMQ020 %in% c(1,2) ~ "Yes",
      IMQ020 == 3 ~ "No",
      TRUE ~ NA_character_),

    MCQ010 = case_when(
      MCQ010 == 1 ~ "Yes",
      MCQ010 == 2 ~ "No",
      TRUE ~ NA_character_),
    MCQ080 = case_when(
      MCQ080 == 1 ~ "Yes",
      MCQ080 == 2 ~ "No",
      TRUE ~ NA_character_),
    PAQ605 = case_when(
      PAQ605 == 1 ~ "Yes",
      PAQ605 == 2 ~ "No",
      TRUE ~ NA_character_),
    PAQ620 = case_when(
      PAQ620 == 1 ~ "Yes",
      PAQ620 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    across(starts_with("OCQ6"), ~ ifelse(
      .x == 99, NA, .x)),
    
    SMQ020 = case_when(
      SMQ020 == 1 ~ "Yes",
      SMQ020 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    across(starts_with("SMQ"), ~ ifelse(
      .x %in% c(77, 99, 777, 999),NA, .x)),
    
    across(starts_with("WHD"), ~ ifelse(
      .x %in% c(7777, 9999),NA, .x)),
    
    weight_change = as.numeric(WHD020) - as.numeric(WHD050),
    
    WHQ030 = case_when(
      WHQ030 == 1 ~ "Overweight",
      WHQ030 == 2 ~ "Underweight",
      WHQ030 == 3 ~ "Normal Weight",
      TRUE ~ NA_character_),
    
    WHQ070 = case_when(
      WHQ070 == 1 ~ "Yes",
      WHQ070 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    DIQ010 = case_when(
      DIQ010 == 1 ~ "Yes",
      DIQ010 %in% c(2,3) ~ "No",
      TRUE ~ NA_character_),
    
    DBQ700 = case_when(
      DBQ700 %in% c(1:3) ~ "Good",
      DBQ700 %in% c(4,5) ~ "Average/Bad",
      TRUE ~ NA_character_),
    
    across(c(contains("VTQ"), contains("PUQ")), ~ case_when(
      .x == 1 ~ "Yes",
      .x == 2 ~ "No",
      TRUE ~ NA_character_)),
    
    phq9 = rowSums(select(., starts_with("DPQ")), na.rm = TRUE),
    
    phq9_category = case_when(
      phq9 < 5 ~ "Minimal",
      phq9 %in% 5:9 ~ "Mild",
      phq9 %in% 10:14 ~ "Moderate",
      phq9 %in% 15:19 ~ "Moderately Severe",
      phq9 >= 20 ~ "Severe",
      TRUE ~ NA_character_),

    across(starts_with("FSD"), ~ case_when(
      .x  == 1 ~ "full food security: 0",
      .x  == 2 ~ "marginal food security: 1-2",
      .x  == 3 ~ "low food security: 3-5",
      .x  == 4 ~ "very low food security: 6-10",
      TRUE ~ NA_character_)),
      
    across(starts_with("BPQ"), ~ case_when(
      .x  == 1 ~ "Yes",
      .x  == 2 ~ "No",
      TRUE ~ NA_character_)),
    
    HIQ011 = case_when(
      HIQ011 == 1 ~ "Yes",
      HIQ011 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    ACD011A = case_when(
      ACD011A == 1 ~ "English",
      TRUE ~ "Non-English"),
    
    OHQ845 = case_when(
      OHQ845 %in% c(1:3) ~ "Good and Above",
      OHQ845 %in% c(4:5) ~ "Fair/Poor",
      TRUE ~ NA_character_),
  )

# Dietrary####

dinames <- c("DSQTOT","DS1TOT")

di_dat_new <- di_dat[names(di_dat) %in% dinames]

divars <- readxl::read_excel("data/FinalVars.xlsx", 
                             sheet = "DietaryVars", col_names = FALSE)$`...1`
divars <- sapply(divars, function(x) strsplit(x, " - ")[[1]][1])

di_dat_cleaned <- select_variable(di_dat_new, divars)

di_df <- combine_surveys(di_dat_cleaned)


di_df <- di_df %>% 
  select(where(~mean(is.na(.)) < 0.5))

# all columns have more then 50% missing values, so we will not use dietary data for analysis

## Merge in 2021-2023####
# List all .xpt files with full path
didata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/dietary", full.names = TRUE)

# Read each file and assign it to the global environment
lapply(didata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

# Get all objects in the global environment
all_objects <- ls(envir = .GlobalEnv)

# Filter objects that end with "_L"
di_datasets <- grep("_L$", all_objects, value = TRUE)[!grep("_L$", all_objects, value = TRUE) %in% c(qdatasets, demodata)]

datasets <- mget(di_datasets, envir = .GlobalEnv)

datasets <- datasets[names(datasets) %in% paste0(dinames, "_L")]

# No Dietary data avaliable for 2021-2023

# Laboratory####

labnames <- c("ALB_CR", "L40", "L05", "CBC", "CRP", "L06TFR", "GLU", "GHB", "UIO", "L06HM", 
              "OPD", "HPVSWR", "VOCMWB", "PAH", "PHTHTE", "PERNT", "UAS", "PBCD", "HEPA", 
              "L02HBS", "HEPC", "COT", "HDL", "TRIGLY", "TCHOL")

lab_dat_new <- lab_dat[names(lab_dat) %in% labnames]

labvars <- readxl::read_excel("data/FinalVars.xlsx", 
                              sheet = "LabVars", col_names = FALSE)$`...1`
labvars <- sapply(labvars, function(x) strsplit(x, " - ")[[1]][1])

lab_dat_cleaned <- select_variable(lab_dat_new, labvars)

lab_df <- combine_surveys(lab_dat_cleaned)

lab_df <- lab_df %>% 
  select(where(~mean(is.na(.)) < 0.5))

## Merge in 2021-2023####
# List all .xpt files with full path
labdata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/laboratory", full.names = TRUE)

# Read each file and assign it to the global environment
lapply(labdata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

# Get all objects in the global environment
all_objects <- ls(envir = .GlobalEnv)

# Filter objects that end with "_L"
lab_datasets <- grep("_L$", all_objects, value = TRUE)[!grep("_L$", all_objects, value = TRUE) %in% c(qdatasets, di_datasets)]

datasets <- mget(lab_datasets, envir = .GlobalEnv)

datasets <- lapply(datasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  # Add Year
  return(df)
})

# Perform full joins across all datasets
full_joined_data <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets)

full_joined_data <- full_joined_data[colnames(full_joined_data) %in% colnames(lab_df)]


# Bind lab_df with the fully joined dataset
lab_df_joined <- bind_rows(lab_df, full_joined_data)

# duplicate_seqn <- lab_df %>%
#   group_by(SEQN, Year) %>%
#   filter(n() > 1)  # Keep only duplicates

# Examination####

exnames <- c("AUX", "BPX", "BMX", 'DXX', 'BPXO', 'OHXDENT')

ex_dat_new <- ex_dat[names(ex_dat) %in% exnames]

exvars <- readxl::read_excel("data/FinalVars.xlsx", 
                             sheet = "ExamVars", col_names = FALSE)$`...1`

exvars <- sapply(exvars, function(x) strsplit(x, " - ")[[1]][1])

ex_dat_cleaned <- select_variable(ex_dat_new, exvars)

ex_df <- combine_surveys(ex_dat_cleaned)
# 
# ex_df <- ex_df %>% 
#   select(where(~mean(is.na(.)) < 0.5))

## Merge in 2021-2023####
# List all .xpt files with full path
examdata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/exam", full.names = TRUE)

# Read each file and assign it to the global environment
lapply(examdata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

# Get all objects in the global environment
all_objects <- ls(envir = .GlobalEnv)

# Filter objects that end with "_L"
exam_datasets <- grep("_L$", all_objects, value = TRUE)[!grep("_L$", all_objects, value = TRUE) %in% c(qdatasets, di_datasets, lab_datasets)]

datasets <- mget(exam_datasets, envir = .GlobalEnv)

datasets <- lapply(datasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  # Add Year
  return(df)
})

# Perform full joins across all datasets
full_joined_data <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets)

full_joined_data <- full_joined_data[colnames(full_joined_data) %in% colnames(ex_df)]


# Bind ex_df with the fully joined dataset
ex_df_joined <- bind_rows(ex_df, full_joined_data)


## Recoding####
ex_df_joined <- ex_df_joined %>% 
  mutate(BPXPULS = ifelse(BPXPULS == 1, "Regular", "Irregular"))

# Define BP variables for general years and 2021
bp_vars_general <- c("BPXSY1", "BPXDI1", "BPXSY2", "BPXDI2", "BPXSY3", "BPXDI3", "BPXSY4", "BPXDI4")
bp_vars_2021 <- c("BPXOSY1", "BPXODI1", "BPXOSY2", "BPXODI2", "BPXOSY3", "BPXODI3")
# Compute mean and standard deviation separately for 2021 and other years
bp_stats_general <- ex_df_joined %>%
  filter(Year != 2021) %>%
  summarise(across(all_of(bp_vars_general), list(mean = ~ mean(. , na.rm = TRUE),
                                                 sd = ~ sd(. , na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_sep = "_") %>%
  mutate(Year = "Other Years")

bp_stats_2021 <- ex_df_joined %>%
  filter(Year == 2021) %>%
  summarise(across(all_of(bp_vars_2021), list(mean = ~ mean(. , na.rm = TRUE),
                                              sd = ~ sd(. , na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_sep = "_") %>%
  mutate(Year = "2021")
# Combine both statistics
bp_stats_long <- bind_rows(bp_stats_general, bp_stats_2021)
# Display the results
print(bp_stats_long)


# Since not much fluctuation between BP1-BP3, we will take average of 1,2,3 to create new numerical variables called BPXSY and BPXDI
# Dual-Energy X-ray 
# select DXDTRBMD, DXXLABMD based on low correlation with other bmi variables and DXD 
# 2021 and P is measured by BPXOSY, BPXODI
exam_bp <- ex_df_joined %>%
  mutate(
    BPXSY = ifelse(Year %in% c('2021', 'P'), 
                   rowMeans(select(., BPXOSY1, BPXOSY2, BPXOSY3), na.rm = TRUE) + 1.5, 
                   rowMeans(select(., BPXSY1, BPXSY2, BPXSY3), na.rm = TRUE)),
    BPXDI = ifelse(Year %in% c('2021', 'P'), 
                   rowMeans(select(., BPXODI1, BPXODI2, BPXODI3), na.rm = TRUE) - 1.3, 
                   rowMeans(select(., BPXDI1, BPXDI2, BPXDI3), na.rm = TRUE))
  ) %>%
  select(SEQN, Year, BPXSY, BPXDI, BPXPULS, BMXBMI, DXDTRBMD, DXXLABMD)  # Keep only relevant columns

# removing BMXWAIST since high correlation with BMXBMI
print(cor.test(ex_df_joined$BMXBMI, ex_df_joined$BMXWAIST))

missingness <- exam_bp %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Percentage")
print(missingness)

# Encode into hearing loss vs noraml based on literature's threshold for normal hearing 
exam_audio <- ex_df_joined %>%
  rowwise() %>%  
  mutate(hearing_loss = ifelse(
    any(c(AUXU1K1R, AUXU500R, AUXU1K2R, AUXU2KR, AUXU3KR, 
          AUXU4KR, AUXU6KR, AUXU8KR, AUXU1K1L, AUXU500L, 
          AUXU1K2L, AUXU2KL, AUXU3KL, AUXU4KL, AUXU6KL, AUXU8KL) > 21, 
        na.rm = TRUE), 
    "No",   # Normal
    "Yes"   # Hearing Loss
  )) %>%
  select(SEQN, Year, hearing_loss)
table(exam_audio$hearing_loss)


ex_df_final <- exam_bp %>%
  left_join(exam_audio, by = c("SEQN", "Year"))

# Merge data
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

nhanes_data <- filter(nhanes_data, Year %in% c(2013, 2015, 2017, 'P'))
table(nhanes_data$Year)

dat_full <- ques_df_joined %>% 
  full_join(lab_df_joined, by = c('SEQN','Year')) %>% 
  full_join(ex_df_final, by = c('SEQN','Year')) 

dat_full$Year <- ifelse(dat_full$Year == 2017, "P", as.character(dat_full$Year))
table(dat_full$Year)

dat_full_2013_2020 <- dat_full %>% 
  filter(Year != 2021) %>%
  inner_join(nhanes_data, by = c('SEQN','Year'))

dat_full_2021 <- dat_full %>%
  filter(Year == 2021) 
  
# dat_full <- dat_full %>% 
#   left_join(nhanes_data, by = c('SEQN','Year'))

dat_full_hyp <- dat_full_2013_2020[dat_full_2013_2020$svy_subpop_htn == 1,]

# Check duplicate
d <- duplicated(t(dat_full_hyp))
d <- d[d == T]

# Drop variables and subset based on discussion
dat_hyp <- dat_full_hyp[dat_full_hyp$htn_accaha == "Yes",]

chols <- c("chol_ldl_5cat", "chol_nonhdl_5cat", "chol_hdl")

dat_hyp <- dat_hyp %>% 
  select(-contains("uncontrolled"), -contains("escesh"),
         -chol_total, -chol_total_gteq_200,-chol_total_gteq_240, chol_hdl, -chol_hdl_low, 
         -chol_trig, -chol_trig_gteq_150, -chol_ldl, -chol_ldl_lt_70, -chol_ldl_gteq_70,
         -chol_ldl_gteq_190, -chol_nonhdl,-chol_nonhdl_lt_100, -chol_nonhdl_gteq_100, -chol_nonhdl_gteq_220)


## Missing Imputation####

table(dat_hyp$Year, dat_hyp$cc_cvd_hf, useNA = "always")

## remove vars with more than 50% missing
dat_hyp_cleaned <- dat_hyp %>% select(where(~mean(is.na(.)) < 0.5))
dat_hyp_cleaned <- as.data.frame(dat_hyp_cleaned)

dat_hyp %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Percentage") %>% 
  filter(Missing_Percentage > 0)

# Perform imputation by cycle
miceObj_2013 <- mice::mice(dat_hyp_cleaned[dat_hyp_cleaned$Year == "2013",], method = "cart")
dat_hyp_mice_2013 <- mice::complete(miceObj_2013, "all")

miceObj_2015 <- mice::mice(dat_hyp_cleaned[dat_hyp_cleaned$Year == "2015",], method = "cart")
dat_hyp_mice_2015 <- mice::complete(miceObj_2015, "all")

miceObj_2017 <- mice::mice(dat_hyp_cleaned[dat_hyp_cleaned$Year == "P",], method = "cart")
dat_hyp_mice_2017 <- mice::complete(miceObj_2017, "all")

# Save the Imputed data
save(dat_full_2013_2020, dat_full_2021, dat_hyp_cleaned, miceObj_2013, miceObj_2015, miceObj_2017, dat_hyp_mice_2013, dat_hyp_mice_2015, dat_hyp_mice_2017,
     file = "data/cleaned/dat_hyp_mice.RData")


## Standardize the data####

categorical_vars <- dat_hyp_cleaned %>% select(where(is.character), where(is.factor)) %>% colnames()
# hearing_loss problematic - only 1 level,  all yes in year 2013 and 2021
# BPXPULS problematic - all missing in year 2017
dat_hyp_mice_2013_cat <- lapply(dat_hyp_mice_2013, function(x){x <- x %>% 
  select(-Year, -svy_subpop_htn, -htn_accaha, -htn_jnc7,
         -SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -svy_year,
         -bp_control_jnc7, -bp_control_accaha, -bp_control_140_90, -bp_control_130_80, - hearing_loss, -BPXPULS) %>% 
  mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars]
  x <- predict(dummyVars("~ .", data = x), x)})

dat_hyp_mice_2015_cat <- lapply(dat_hyp_mice_2015, function(x){x <- x %>% 
  select(-Year, -svy_subpop_htn, -htn_accaha, -htn_jnc7,
         -SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -svy_year,
         -bp_control_jnc7, -bp_control_accaha, -bp_control_140_90, -bp_control_130_80, - hearing_loss, -BPXPULS) %>%
  mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars]
  x <- predict(dummyVars("~ .", data = x), x)})

dat_hyp_mice_2017_cat <- lapply(dat_hyp_mice_2017, function(x){x <- x %>%
  select(-Year, -svy_subpop_htn, -htn_accaha, -htn_jnc7,
         -SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -svy_year,
         -bp_control_jnc7, -bp_control_accaha, -bp_control_140_90, -bp_control_130_80, - hearing_loss, -BPXPULS) %>%
  mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars]})

dat_hyp_mice_2013_continuous <- lapply(dat_hyp_mice_2013, function(x){x <- x %>% 
  select(-Year, -svy_subpop_htn, -htn_accaha, -htn_jnc7,
         -SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -svy_year,
         -bp_control_jnc7, -bp_control_accaha, -bp_control_140_90, -bp_control_130_80, - hearing_loss, -BPXPULS)
  x <- x[!colnames(x) %in% categorical_vars]
  x <- predict(preProcess(x, method = c("center", "scale")), x)})

dat_hyp_mice_2015_continuous <- lapply(dat_hyp_mice_2015, function(x){x <- x %>%
  select(-Year, -svy_subpop_htn, -htn_accaha, -htn_jnc7,
         -SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -svy_year,
         -bp_control_jnc7, -bp_control_accaha, -bp_control_140_90, -bp_control_130_80, - hearing_loss, -BPXPULS)
  x <- x[!colnames(x) %in% categorical_vars]
  x <- predict(preProcess(x, method = c("center", "scale")), x)})

dat_hyp_mice_2017_continuous <- lapply(dat_hyp_mice_2017, function(x){x <- x %>%
  select(-Year, -svy_subpop_htn, -htn_accaha, -htn_jnc7,
         -SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -svy_year,
         -bp_control_jnc7, -bp_control_accaha, -bp_control_140_90, -bp_control_130_80, - hearing_loss, -BPXPULS)
  x <- x[!colnames(x) %in% categorical_vars]
  x <- predict(preProcess(x, method = c("center", "scale")), x)})

svy_vars_2013 <- lapply(dat_hyp_mice_2013, function(x){x <- x %>% 
  select(Year, svy_subpop_htn, htn_accaha, htn_jnc7,
         SEQN,svy_weight_mec, svy_psu, svy_strata, svy_year,
         bp_control_jnc7, bp_control_accaha, bp_control_140_90, bp_control_130_80)})

svy_vars_2015 <- lapply(dat_hyp_mice_2015, function(x){x <- x %>%
  select(Year, svy_subpop_htn, htn_accaha, htn_jnc7,
         SEQN,svy_weight_mec, svy_psu, svy_strata, svy_year,
         bp_control_jnc7, bp_control_accaha, bp_control_140_90, bp_control_130_80)})

svy_vars_2017 <- lapply(dat_hyp_mice_2017, function(x){x <- x %>%
  select(Year, svy_subpop_htn, htn_accaha, htn_jnc7,
         SEQN,svy_weight_mec, svy_psu, svy_strata, svy_year,
         bp_control_jnc7, bp_control_accaha, bp_control_140_90, bp_control_130_80)})

dat_hyp_2013_std <- Map(cbind, svy_vars_2013, dat_hyp_mice_2013_cat, dat_hyp_mice_2013_continuous)
dat_hyp_2015_std <- Map(cbind, svy_vars_2015, dat_hyp_mice_2015_cat, dat_hyp_mice_2015_continuous)
dat_hyp_2017_std <- Map(cbind, svy_vars_2017, dat_hyp_mice_2017_cat, dat_hyp_mice_2017_continuous)


dat_hyp_2013_std <- lapply(dat_hyp_2013_std, function(x){x <- x %>%
  select(colnames(x)[!grepl("\\.No$", colnames(x))])})

dat_hyp_2015_std <- lapply(dat_hyp_2015_std, function(x){x <- x %>%
  select(colnames(x)[!grepl("\\.No$", colnames(x))])})

dat_hyp_2017_std <- lapply(dat_hyp_2017_std, function(x){x <- x %>%
  select(colnames(x)[!grepl("\\.No$", colnames(x))])})


save(dat_hyp_2013_std, dat_hyp_2015_std, dat_hyp_2017_std,file = "data/cleaned/dat_hyp_std_2013_2020.RData")

## Bootsrapping ####
# split into 80% training and 20% testing

set.seed(2024)
dat_hyp_2013_std_ind <- sample(1:nrow(dat_hyp_2013_std[[1]]), 0.8*nrow(dat_hyp_2013_std[[1]]))
dat_hyp_2013_std_train <- lapply(dat_hyp_2013_std, function(x){x[dat_hyp_2013_std_ind, ]})
dat_hyp_2013_std_test <- lapply(dat_hyp_2013_std, function(x){x[-dat_hyp_2013_std_ind, ]})

dat_hyp_2015_std_ind <- sample(1:nrow(dat_hyp_2015_std[[1]]), 0.8*nrow(dat_hyp_2015_std[[1]]))
dat_hyp_2015_std_train <- lapply(dat_hyp_2015_std, function(x){x[dat_hyp_2015_std_ind, ]})
dat_hyp_2015_std_test <- lapply(dat_hyp_2015_std, function(x){x[-dat_hyp_2015_std_ind, ]})

dat_hyp_2017_std_ind <- sample(1:nrow(dat_hyp_2017_std[[1]]), 0.8*nrow(dat_hyp_2017_std[[1]]))
dat_hyp_2017_std_train <- lapply(dat_hyp_2017_std, function(x){x[dat_hyp_2017_std_ind, ]})
dat_hyp_2017_std_test <- lapply(dat_hyp_2017_std, function(x){x[-dat_hyp_2017_std_ind, ]})


# Perform resampling to create multiple training sets

num_replicates <- 50  
dat_hyp_2013_samp <- dat_hyp_2015_samp <- dat_hyp_2017_samp <- vector("list", length = num_replicates)

for (i in 1:num_replicates) {
  dat_hyp_2013_samp[[i]] <- lapply(dat_hyp_2013_std_train, function(x){x[sample(1:nrow(dat_hyp_2013_std_train[[1]]), replace = TRUE), ]})
  
  dat_hyp_2015_samp[[i]] <- lapply(dat_hyp_2015_std_train, function(x){x[sample(1:nrow(dat_hyp_2015_std_train[[1]]), replace = TRUE), ]})
  
  dat_hyp_2017_samp[[i]] <- lapply(dat_hyp_2017_std_train, function(x){x[sample(1:nrow(dat_hyp_2017_std_train[[1]]), replace = TRUE), ]})
  
}

save(dat_hyp_2013_std_train, dat_hyp_2013_std_test, dat_hyp_2015_std_train, 
     dat_hyp_2015_std_test, dat_hyp_2017_std_train,
     dat_hyp_2017_std_test, dat_hyp_2013_samp,
     dat_hyp_2015_samp, dat_hyp_2017_samp,
     file = "data/cleaned/dat_train_test_bootstrapped_2013_2020.RData")

