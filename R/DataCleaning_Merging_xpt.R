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

source("R/funcs.R")

# Questionaire####
quesvars <- readxl::read_excel("data/FinalVars.xlsx", 
                               sheet = "QuesVars", col_names = FALSE)$`...1`
quesvars <- sapply(quesvars, function(x) strsplit(x, " - ")[[1]][1])

qdata_2013 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2013_2014/questionnaire", full.names = TRUE)
qdata_2015 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2015_2016/questionnaire", full.names = TRUE)
qdata_2017 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2017_2018/questionnaire", full.names = TRUE)
qdata_2020 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2017_2020/questionnaire", full.names = TRUE)
qdata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2021_2023/questionnaire", full.names = TRUE)

lapply(qdata_2013, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(qdata_2015, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(qdata_2017, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

lapply(qdata_2020, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(qdata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

all_objects <- ls(envir = .GlobalEnv)

qdatasets_2013 <- grep("_H$", all_objects, value = TRUE)
datasets_2013 <- mget(qdatasets_2013, envir = .GlobalEnv)
datasets_2013 <- select_variable(datasets_2013, quesvars)
datasets_2013 <- lapply(datasets_2013, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2013')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})
datasets_2013 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2013)

qdatasets_2015 <- grep("_I$", all_objects, value = TRUE)
datasets_2015 <- mget(qdatasets_2015, envir = .GlobalEnv)
datasets_2015 <- select_variable(datasets_2015, quesvars)
datasets_2015 <- lapply(datasets_2015, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2015')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})
datasets_2015 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2015)

qdatasets_2017 <- grep("_J$", all_objects, value = TRUE)
datasets_2017 <- mget(qdatasets_2017, envir = .GlobalEnv)
datasets_2017 <- select_variable(datasets_2017, quesvars)
datasets_2017 <- lapply(datasets_2017, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2017')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})
datasets_2017 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2017)

qdatasets_2020 <- grep("^P_", all_objects, value = TRUE)
datasets_2020 <- mget(qdatasets_2020, envir = .GlobalEnv)
datasets_2020 <- select_variable(datasets_2020, quesvars)
datasets_2020 <- lapply(datasets_2020, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2020')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})
datasets_2020 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2020)

qdatasets_2021 <- grep("_L", all_objects, value = TRUE)
datasets_2021 <- mget(qdatasets_2021, envir = .GlobalEnv)
datasets_2021 <- select_variable(datasets_2021, quesvars)
datasets_2021 <- lapply(datasets_2021, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  
  return(df)
})
datasets_2021 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2021)

ques_df_joined <- full_join(datasets_2013, datasets_2015) %>% 
  full_join(datasets_2017) %>% 
  full_join(datasets_2020) %>% 
  full_join(datasets_2021)

ques_df_joined %>%
  group_by(SEQN, Year) %>%
  filter(n() > 1)  # Keep only duplicates

ques_df_joined <- ques_df_joined %>%
  distinct(SEQN, Year, .keep_all = TRUE)

ques_df_joined <- ques_df_joined %>% 
  mutate(
    ALQ101 = case_when(
      ALQ101 == 1 ~ "Yes",
      ALQ101 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    across(starts_with("ALQ"), ~ ifelse(
      .x %in% c( 777, 999),NA, .x)),
    
    across(starts_with("ALQ"), ~ ifelse(
      .x %in% c( 777, 999),NA, .x)),
    
    across(starts_with("PAQ7"), ~ ifelse(
      .x %in% c(8, 77, 99),NA, .x)),
    
    OCQ180  = ifelse(OCQ180 %in% c(77777, 99999),NA, OCQ180),
    
    CDQ001 = case_when(
      CDQ001 == 1 ~ "Yes",
      CDQ001 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    DEQ034A = case_when(
      DEQ034A %in% c(1:3,6) ~ "Most of the time",
      DEQ034A %in% c(4,5) ~ "Rarely or Never",
      TRUE ~ NA_character_),
    
    DEQ034D = case_when(
      DEQ034D %in% c(1:3) ~ "Most of the time",
      DEQ034D %in% c(4,5) ~ "Rarely or Never",
      TRUE ~ NA_character_),
    
    HSD010 = case_when(
      HSD010 %in% c(1:3) ~ "Good",
      DEQ034D %in% c(4,5) ~ "Not Good",
      TRUE ~ NA_character_),
    
    KIQ022 = case_when(
      KIQ022 == 1 ~ "Yes",
      KIQ022 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    across(starts_with("PFQ"), ~ case_when(
      .x  == 1 ~ "Yes",
      .x  == 2 ~ "No",
      TRUE ~ NA_character_)),
    
    SLQ050 = case_when(
      SLQ050 == 1 ~ "Yes",
      SLQ050 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    SLQ060 = case_when(
      SLQ060 == 1 ~ "Yes",
      SLQ060 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    IMQ011 = case_when(
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
      TRUE ~ NA_character_)
  )

# Drop vars with too much missing cycles
ques_df_joined <- ques_df_joined %>% 
  select(-starts_with("DPQ"), -WHD020, -WHD050, -OCQ610, -OCQ640, -PAQ706,-SLD010H, -SLQ060, -VTQ233A)
  
# Demographics####
demodata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/demographics", full.names = TRUE)

# Read each file and assign it to the global environment
lapply(demodata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

# Get all objects in the global environment
all_objects <- ls(envir = .GlobalEnv)

# Filter objects that end with "_L"
demodata <-  grep("_L$", all_objects, value = TRUE)[!grep("_L$", all_objects, value = TRUE) %in% qdatasets_2021]

demodatasets <- mget(demodata, envir = .GlobalEnv)

demodatasets <- lapply(demodatasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  # Add Year
  return(df)
})

# Perform full joins across all datasets
demo_df <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), demodatasets)

# Dietrary####

divars <- readxl::read_excel("data/FinalVars.xlsx", 
                               sheet = "DietaryVars", col_names = FALSE)$`...1`
divars <- sapply(divars, function(x) strsplit(x, " - ")[[1]][1])
# all columns have more then 50% missing values, so we will not use dietary data for analysis

# Laboratory####
labvars <- readxl::read_excel("data/FinalVars.xlsx", 
                               sheet = "LabVars", col_names = FALSE)$`...1`
labvars <- sapply(labvars, function(x) strsplit(x, " - ")[[1]][1])

labdata_2013 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2013_2014/laboratory", full.names = TRUE)
labdata_2015 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2015_2016/laboratory", full.names = TRUE)
labdata_2017 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2017_2018/laboratory", full.names = TRUE)
labdata_2020 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2017_2020/laboratory", full.names = TRUE)
labdata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2021_2023/laboratory", full.names = TRUE)

lapply(labdata_2013, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(labdata_2015, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(labdata_2017, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(labdata_2020, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(labdata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

all_objects <- ls(envir = .GlobalEnv)

labdatasets_2013 <- grep("_H$", all_objects, value = TRUE)[!grep("_H$", all_objects, value = TRUE) %in% qdatasets_2013]
labdatasets_2015 <- grep("_I$", all_objects, value = TRUE)[!grep("_I$", all_objects, value = TRUE) %in% qdatasets_2015]
labdatasets_2017 <- grep("_J$", all_objects, value = TRUE)[!grep("_J$", all_objects, value = TRUE) %in% qdatasets_2017]
labdatasets_2020 <- grep("^P_", all_objects, value = TRUE)[!grep("^P_", all_objects, value = TRUE) %in% qdatasets_2020]
labdatasets_2021 <- grep("_L$", all_objects, value = TRUE)[!grep("_L$", all_objects, value = TRUE) %in% qdatasets_2021]

datasets_2013 <- mget(labdatasets_2013, envir = .GlobalEnv)
datasets_2015 <- mget(labdatasets_2015, envir = .GlobalEnv)
datasets_2017 <- mget(labdatasets_2017, envir = .GlobalEnv)
datasets_2020 <- mget(labdatasets_2020, envir = .GlobalEnv)
datasets_2021 <- mget(labdatasets_2021, envir = .GlobalEnv)

datasets_2013 <- select_variable(datasets_2013, labvars)
datasets_2015 <- select_variable(datasets_2015, labvars)
datasets_2017 <- select_variable(datasets_2017, labvars)
datasets_2020 <- select_variable(datasets_2020, labvars)
datasets_2021 <- select_variable(datasets_2021, labvars)

datasets_2013 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2013)
datasets_2015 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2015)
datasets_2017 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2017)
datasets_2020 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2020)
datasets_2021 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2021)

datasets_2013 <- lapply(datasets_2013, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2013')  %>%
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})

datasets_2015 <- lapply(datasets_2015, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2015')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})

datasets_2017 <- lapply(datasets_2017, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2017')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})

datasets_2020 <- lapply(datasets_2020, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2020')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})

datasets_2021 <- lapply(datasets_2021, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  %>% 
    select(where(~mean(is.na(.)) < 0.5))
  return(df)
})

datasets_2013 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2013)
datasets_2015 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2015)
datasets_2017 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2017)
datasets_2020 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2020)
datasets_2021 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2021)

# d <- duplicated(t(datasets_2013))
# d <- d[d == T]
# names(d)

datasets_2013 <- datasets_2013 %>% 
  select(-URXUCR.y.y.y,-URXUCR.y.y.y.y,-URXUCR.y.y.y.y.y, -URXUCR.x.x.x.x.x.x,
  -URXUCR.y.y.y.y.y.y, -URXUCR.x.x.x.x.x.x.x, -URXUCR.y.y.y.y.y.y.y, -URXUCR.x.x.x.x.x.x.x.x,
  -URXUCR.y.y.y.y.y.y.y.y, -URXUCR.x.x.x.x.x.x.x.x.x, -URXUCR.y.y.y.y.y.y.y.y.y)

lab_df_joined <- full_join(datasets_2013, datasets_2015) %>% 
  full_join(datasets_2017) %>% 
  full_join(datasets_2020) %>% 
  full_join(datasets_2021)

lab_df_joined %>%
  group_by(SEQN, Year) %>%
  filter(n() > 1)  # Keep only duplicates

summary(lab_df_joined)

lab_df_joined <- lab_df_joined %>% 
  select(-URXUCR.x, -URXUCL, -URXUCR.y, -URXUCR.x.x,  -URXUCR.y.y, -URXUCR.x.x.x, -URXUCR.x.x.x.x, -URXUCR.x.x.x.x.x,
         -URXUAS.x, -URXUAS.y, -URXUAS)

# Examination####

exvars <- readxl::read_excel("data/FinalVars.xlsx", 
                               sheet = "ExamVars", col_names = FALSE)$`...1`
exvars <- sapply(exvars, function(x) strsplit(x, " - ")[[1]][1])

exdata_2013 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2013_2014/examination", full.names = TRUE)
exdata_2015 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2015_2016/examination", full.names = TRUE)
exdata_2017 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2017_2018/examination", full.names = TRUE)
exdata_2020 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2017_2020/examination", full.names = TRUE)
exdata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/nhanes_all_cycles/2021_2023/examination", full.names = TRUE)

lapply(exdata_2013, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(exdata_2015, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(exdata_2017, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(exdata_2020, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})
lapply(exdata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

all_objects <- ls(envir = .GlobalEnv)

exdatasets_2013 <- grep("_H$", all_objects, value = TRUE)[!grep("_H$", all_objects, value = TRUE) %in% c(qdatasets_2013, labdatasets_2013)]
exdatasets_2015 <- grep("_I$", all_objects, value = TRUE)[!grep("_I$", all_objects, value = TRUE) %in% c(qdatasets_2015, labdatasets_2015)]
exdatasets_2017 <- grep("_J$", all_objects, value = TRUE)[!grep("_J$", all_objects, value = TRUE) %in% c(qdatasets_2017, labdatasets_2017)]
exdatasets_2020 <- grep("^P_", all_objects, value = TRUE)[!grep("^P_", all_objects, value = TRUE) %in% c(qdatasets_2020, labdatasets_2020)]
exdatasets_2021 <- grep("_L$", all_objects, value = TRUE)[!grep("_L$", all_objects, value = TRUE) %in% c(qdatasets_2021, labdatasets_2021)]

datasets_2013 <- mget(exdatasets_2013, envir = .GlobalEnv)
datasets_2015 <- mget(exdatasets_2015, envir = .GlobalEnv)
datasets_2017 <- mget(exdatasets_2017, envir = .GlobalEnv)
datasets_2020 <- mget(exdatasets_2020, envir = .GlobalEnv)
datasets_2021 <- mget(exdatasets_2021, envir = .GlobalEnv)

datasets_2013 <- select_variable(datasets_2013, exvars)
datasets_2015 <- select_variable(datasets_2015, exvars)
datasets_2017 <- select_variable(datasets_2017, exvars)
datasets_2020 <- select_variable(datasets_2020, exvars)
datasets_2021 <- select_variable(datasets_2021, exvars)

datasets_2013 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2013)
datasets_2015 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2015)
datasets_2017 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2017)
datasets_2020 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2020)
datasets_2021 <- Filter(function(x) nrow(x) > 0 & ncol(x) > 0, datasets_2021)

datasets_2013 <- lapply(datasets_2013, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2013')
  return(df)
})

datasets_2015 <- lapply(datasets_2015, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2015')
  return(df)
})

datasets_2017 <- lapply(datasets_2017, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2017')  
  return(df)
})

datasets_2020 <- lapply(datasets_2020, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2020')  
  return(df)
})

datasets_2021 <- lapply(datasets_2021, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  
  return(df)
})

datasets_2013 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2013)
datasets_2015 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2015)
datasets_2017 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2017)
datasets_2020 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2020)
datasets_2021 <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets_2021)

datasets_2013 %>%
  group_by(SEQN, Year) %>%
  filter(n() > 1)  # Keep only duplicates

datasets_2013 <- datasets_2013 %>%
  distinct(SEQN, Year, .keep_all = TRUE)
datasets_2015 <- datasets_2015 %>%
  distinct(SEQN, Year, .keep_all = TRUE)
datasets_2017 <- datasets_2017 %>%
  distinct(SEQN, Year, .keep_all = TRUE)
datasets_2020 <- datasets_2020 %>%
  distinct(SEQN, Year, .keep_all = TRUE)
datasets_2021 <- datasets_2021 %>%
  distinct(SEQN, Year, .keep_all = TRUE)


ex_df_joined <- full_join(datasets_2013, datasets_2015) %>% 
  full_join(datasets_2017) %>% 
  full_join(datasets_2020) %>% 
  full_join(datasets_2021)

ex_df_joined %>%
  group_by(SEQN, Year) %>%
  filter(n() > 1)  # Keep only duplicates

summary(ex_df_joined)

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

summary(ex_df_final)

# Merge with cardioStatsUSA####
library(cardioStatsUSA)
data(nhanes_data)
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

dat_full$Year <- ifelse(dat_full$Year %in% c(2017, 2020), "P", as.character(dat_full$Year))
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
dat_hyp <- dat_full_hyp[dat_full_hyp$htn_jnc7 == "Yes",]

dat_hyp <- dat_hyp %>% 
  select(-contains("uncontrolled"), -contains("escesh"), -contains("bp_med"), -contains("chol_med"),
         -chol_total, -chol_total_gteq_200,-chol_total_gteq_240, chol_hdl, -chol_hdl_low, 
         -chol_trig, -chol_trig_gteq_150, -chol_ldl, -chol_ldl_lt_70, -chol_ldl_gteq_70,
         -chol_ldl_gteq_190, -chol_nonhdl,-chol_nonhdl_lt_100, -chol_nonhdl_gteq_100, -chol_nonhdl_gteq_220,
         -cc_cvd_ascvd, -cc_acr, -cc_egfr, -cc_egfr_lt60, -cc_acr_gteq30, -htn_aware, -htn_accaha, -htn_jnc7,
         -bp_control_accaha, -htn_resistant_accaha, -htn_resistant_accaha_thz, -htn_resistant_accaha_thz, -WHD010,
         -demo_pregnant, -bp_dia_mean, -bp_sys_mean, -bp_cat_meds_excluded, -bp_cat_meds_included, -bp_control_140_90,
         -chol_hdl, -chol_ldl_5cat, -chol_ldl_lt_100, -chol_ldl_gteq_100, -chol_ldl_persistent, -chol_nonhdl_5cat,
         -chol_measured_never, -chol_measured_last, -bp_control_130_80, -BPXSY, -BPQ020, -BPXDI, -svy_subpop_chol, -htn_resistant_jnc7_thz,
         -svy_subpop_htn, -phq9, -htn_resistant_jnc7, -svy_year) %>% 
  mutate(race = case_when(demo_race_black == "Yes" ~ "Non-Hispanic Black",
                          demo_race == "Non-Hispanic White" ~ "Non-Hispanic White",
                          demo_race == "Non-Hispanic Asian" ~ "Non-Hispanic Asian",
                          demo_race == "Hispanic" ~ "Hispanic",
                          demo_race == "Other" ~ "Other")) %>% 
  select(-demo_race, -demo_race_black, -demo_age_cat)

## Missing Imputation####

table(dat_hyp$Year, dat_hyp$cc_cvd_hf, useNA = "always")

## remove vars with more than 50% missing
dat_hyp_cleaned_2013 <- dat_hyp %>%
  filter(Year == "2013") %>% select(where(~mean(is.na(.)) < 0.5))
dat_hyp_cleaned_2015 <- dat_hyp %>%
  filter(Year == "2015") %>% select(where(~mean(is.na(.)) < 0.5))
dat_hyp_cleaned_2017 <- dat_hyp %>%
  filter(Year == "P") %>% select(where(~mean(is.na(.)) < 0.5))

dat_hyp_cleaned_2013 %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Percentage") %>% 
  filter(Missing_Percentage > 0)

# Perform imputation by cycle
miceObj_2013 <- mice::mice(dat_hyp_cleaned_2013, method = "cart")
dat_hyp_mice_2013 <- mice::complete(miceObj_2013, "all")

miceObj_2015 <- mice::mice(dat_hyp_cleaned_2015, method = "cart")
dat_hyp_mice_2015 <- mice::complete(miceObj_2015, "all")

miceObj_2017 <- mice::mice(dat_hyp_cleaned_2017, method = "cart")
dat_hyp_mice_2017 <- mice::complete(miceObj_2017, "all")

# Check missingness
View(dat_hyp_mice_2013[[1]] %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Percentage") %>% 
  filter(Missing_Percentage > 0))

dat_hyp_mice_2013 <- lapply(dat_hyp_mice_2013, function(df){
  df <- df %>% select(-HSD010, -IMQ011, -IMQ020, -WHQ070, -PUQ100, -PUQ110, -CDQ001, -ALQ101, -hearing_loss) %>% 
    na.omit()
})

dat_hyp_mice_2015 <- lapply(dat_hyp_mice_2015, function(df){
  df <- df %>% select(-HSD010, -IMQ011, -IMQ020, -WHQ070, -PUQ100, -PUQ110, -CDQ001, -ALQ101, -hearing_loss) %>% 
    na.omit()
})

dat_hyp_mice_2017 <- lapply(dat_hyp_mice_2017, function(df){
  df <- df %>% select(-IMQ011, -IMQ020, -WHQ070, -PUQ100, -PUQ110, -CDQ001, -FSDHH, -FSDAD, -hearing_loss) %>% 
    na.omit()
})
# Save the Imputed data
save(dat_full_2013_2020, dat_full_2021, dat_hyp_cleaned_2013,dat_hyp_cleaned_2015, dat_hyp_cleaned_2017, miceObj_2013, miceObj_2015, miceObj_2017, dat_hyp_mice_2013, dat_hyp_mice_2015, dat_hyp_mice_2017,
     file = "data/cleaned/dat_hyp_mice.RData")

## Standardize the data####

categorical_vars_2013 <- dat_hyp_mice_2013[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()
categorical_vars_2015 <- dat_hyp_mice_2015[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()
categorical_vars_2017 <- dat_hyp_mice_2017[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

dat_hyp_mice_2013_cat <- lapply(dat_hyp_mice_2013, function(x){x <- x %>% 
  select(-Year, -SEQN,-svy_weight_mec, -svy_psu, -svy_strata,
         -bp_control_jnc7) %>% 
  mutate(across(where(is.character), as.factor))
x <- x[colnames(x) %in% categorical_vars_2013]
x <- predict(dummyVars("~ .", data = x), x)})

dat_hyp_mice_2015_cat <- lapply(dat_hyp_mice_2015, function(x){x <- x %>% 
  select(-Year, -SEQN,-svy_weight_mec, -svy_psu, -svy_strata,
         -bp_control_jnc7) %>% 
  mutate(across(where(is.character), as.factor))
x <- x[colnames(x) %in% categorical_vars_2015]
x <- predict(dummyVars("~ .", data = x), x)})

dat_hyp_mice_2017_cat <- lapply(dat_hyp_mice_2017, function(x){x <- x %>%
  select(-Year, -SEQN,-svy_weight_mec, -svy_psu, -svy_strata,
         -bp_control_jnc7) %>% 
  mutate(across(where(is.character), as.factor))
x <- x[colnames(x) %in% categorical_vars_2017]
x <- predict(dummyVars("~ .", data = x), x)})

dat_hyp_mice_2013_continuous <- lapply(dat_hyp_mice_2013, function(x){x <- x %>% 
  select(-Year, -SEQN,-svy_weight_mec, -svy_psu, -svy_strata,
         -bp_control_jnc7) 
x <- x[!colnames(x) %in% categorical_vars_2013]
x <- predict(preProcess(x, method = c("center", "scale")), x)})

dat_hyp_mice_2015_continuous <- lapply(dat_hyp_mice_2015, function(x){x <- x %>%
  select(-Year, -SEQN,-svy_weight_mec, -svy_psu, -svy_strata,
         -bp_control_jnc7) 
x <- x[!colnames(x) %in% categorical_vars_2015]
x <- predict(preProcess(x, method = c("center", "scale")), x)})

dat_hyp_mice_2017_continuous <- lapply(dat_hyp_mice_2017, function(x){x <- x %>%
  select(-Year, -SEQN,-svy_weight_mec, -svy_psu, -svy_strata,
         -bp_control_jnc7)
x <- x[!colnames(x) %in% categorical_vars_2017]
x <- predict(preProcess(x, method = c("center", "scale")), x)})

svy_vars_2013 <- lapply(dat_hyp_mice_2013, function(x){x <- x %>% 
  select(Year, SEQN,svy_weight_mec, svy_psu, svy_strata,
         bp_control_jnc7)})

svy_vars_2015 <- lapply(dat_hyp_mice_2015, function(x){x <- x %>%
  select(Year, SEQN,svy_weight_mec, svy_psu, svy_strata,
         bp_control_jnc7)})

svy_vars_2017 <- lapply(dat_hyp_mice_2017, function(x){x <- x %>%
  select(Year, SEQN,svy_weight_mec, svy_psu, svy_strata,
         bp_control_jnc7)})

dat_hyp_2013_std <- Map(cbind, svy_vars_2013, dat_hyp_mice_2013_cat, dat_hyp_mice_2013_continuous)
dat_hyp_2015_std <- Map(cbind, svy_vars_2015, dat_hyp_mice_2015_cat, dat_hyp_mice_2015_continuous)
dat_hyp_2017_std <- Map(cbind, svy_vars_2017, dat_hyp_mice_2017_cat, dat_hyp_mice_2017_continuous)


dat_hyp_2013_std <- lapply(dat_hyp_2013_std, function(x){x <- x %>%
  select(colnames(x)[!grepl("\\.No$", colnames(x))], -Year) %>%
  na.omit()})

dat_hyp_2015_std <- lapply(dat_hyp_2015_std, function(x){x <- x %>%
  select(colnames(x)[!grepl("\\.No$", colnames(x))], -Year) %>%
  na.omit()})

dat_hyp_2017_std <- lapply(dat_hyp_2017_std, function(x){x <- x %>%
  select(colnames(x)[!grepl("\\.No$", colnames(x))], -Year) %>%
  na.omit()})


# Check missingness

View(dat_hyp_2013_std[[1]] %>%
       summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
       pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Percentage"))

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

# clean 2021####

rm(list = ls())

load('data/cleaned/2013_to_2023_cleaned/auc_values_ES.RData')
load('data/cleaned/2013_to_2023_cleaned/top_features_ES.RData')
load('data/cleaned/2013_to_2023_cleaned/dat_hyp_final_2013_2020.RData')

clean_names <- function(features) {
  sub("\\..*", "", features)
}

# Apply the cleaning function to each feature list
cleaned_2013 <- clean_names(top_features_2013)
cleaned_2015 <- clean_names(top_features_2015)
cleaned_2017 <- clean_names(top_features_2017)

# Find the intersection of the cleaned variable names
common_features <- intersect(intersect(cleaned_2013, cleaned_2015), cleaned_2017)
union_features <- union(union(cleaned_2013, cleaned_2015), cleaned_2017)

data_dir <- "data/2021_to_2023_data"
xpt_files <- list.files(data_dir, pattern = "\\.xpt$", recursive = TRUE, full.names = TRUE)
matching_files <- c()
for (file in xpt_files) {
  # Read the .xpt file
  df <- tryCatch(read_xpt(file), error = function(e) NULL)
  
  # Skip files that could not be read
  if (is.null(df)) next
  
  # Check if any variable in union_features exists in the dataset
  matched_vars <- intersect(names(df), union_features)
  
  if (length(matched_vars) > 0) {
    matching_files[[file]] <- matched_vars  # Store the matched variables
  }
}
print(matching_files) # the ones i don't need to recode 

files_needed <- c("DEMO_L.xpt", "BMX_L.xpt", "BPXO_L.xpt", "INQ_L.xpt", "HDL_L.xpt", "WHQ_L.xpt", "MCQ_L.xpt", "SMQ_L.xpt", "DIQ_L.xpt", "CBC_L.xpt", "PBCD_L.xpt", "TCHOL_L.xpt", "HIQ_L.xpt", "KIQ_U_L.xpt", "OHQ_L.xpt") 
xpt_files <- list.files(path = data_dir, pattern = "\\.xpt$", recursive = TRUE, full.names = TRUE)
xpt_files <- xpt_files[basename(xpt_files) %in% files_needed]

datasets <- lapply(xpt_files, function(file) {
  df <- read_xpt(file) %>% as.data.frame()
  return(df)
})

data <- Reduce(function(x, y) inner_join(x, y, by = "SEQN"), datasets)

data <- data %>%
  mutate(
    svy_psu = SDMVPSU,
    svy_strata = SDMVSTRA,
    svy_weight_mec = WTMEC2YR,
    
    demo_race = case_when(
      RIDRETH3 == 3 ~ "Non-Hispanic White",
      RIDRETH3 == 4 ~ "Non-Hispanic Black",
      RIDRETH3 == 6 ~ "Non-Hispanic Asian",
      RIDRETH3 %in% c(1, 2) ~ "Hispanic",
      RIDRETH3 == 7 ~ "Other",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Hispanic", "Other")),
    
    demo_gender = factor(RIAGENDR, labels = c("Men", "Women")),
    demo_age_years = as.numeric(RIDAGEYR),
    
    across(starts_with("WHD"), ~ ifelse(
      .x %in% c(7777, 9999),NA, .x)),
    
    weight_change = as.numeric(WHD020) - as.numeric(WHD050),
    
    BPXSY = rowMeans(select(., BPXOSY1, BPXOSY2, BPXOSY3), na.rm = TRUE) + 1.5, 
    BPXDI = rowMeans(select(., BPXODI1, BPXODI2, BPXODI3), na.rm = TRUE) - 1.3, 
    
    bp_control_jnc7 = factor(ifelse(BPXSY < 140 & BPXDI < 90, "Yes", "No"), levels = c("No", "Yes")),
    
    cc_cvd_any = case_when(
      if_any(c(MCQ160E, MCQ160F, MCQ160C, MCQ160B), ~ . == 1) ~ "Yes",
      if_all(c(MCQ160E, MCQ160F, MCQ160C, MCQ160B), ~ . == 2) ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("No", "Yes")),
    
    cc_diabetes = case_when(
      DIQ070 == 1 | DIQ010 == 1 ~ "Yes",
      DIQ070 == 2 | DIQ010 %in% c(2,3) ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("No", "Yes")),
    
    cc_smoke = case_when(
      SMQ040 %in% c(1, 2) ~ "Current",  # If SMQ040 is 1 or 2 → "Current"
      SMQ040 %in% c(2, 3) & SMQ020 == 1 ~ "Former",  # If SMQ040 is 2 or 3 AND SMQ020 == 1 → "Former"
      SMQ040 == 3 & SMQ020 == 2 ~ "Never",  # If SMQ040 is 3 AND SMQ020 == 2 → "Never"
      SMQ020 == 2 ~ "Never", # SMQ040 has a lot missing, so 
      TRUE ~ NA_character_  # Assign NA if none of the conditions match
    ) %>% factor(levels = c("Never", "Former", "Current"))
  ) %>%
  select(
    SEQN, BMXBMI, LBXRBCSI, LBXBPB, LBXTHG, LBXTC, HIQ011, KIQ022, OHQ845, LBDMONO,
    svy_psu, svy_strata, svy_weight_mec, demo_race, demo_gender, demo_age_years, 
    weight_change, bp_control_jnc7, cc_cvd_any, cc_diabetes, cc_smoke
  )

# Check missingness
missing_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  arrange(desc(Missing_Count))

print(missing_summary)

save(data, file = "data/cleaned/2013_to_2023_cleaned/dat_unimputed_2021_2023.RData")

load("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/dat_unimputed_2021_2023.RData")

data_2021 <- data

data_2021 <- data_2021 %>% 
  mutate(HIQ011 = case_when(
    HIQ011 == 1 ~ "Yes",
    HIQ011 == 2 ~ "No",
    TRUE ~ NA_character_),
    KIQ022 = case_when(
      KIQ022 == 1 ~ "Yes",
      KIQ022 == 2 ~ "No",
      TRUE ~ NA_character_),
    OHQ845 = case_when(
      OHQ845 %in% c(1:3) ~ "Good and Above",
      OHQ845 %in% c(4:5) ~ "Fair/Poor",
      TRUE ~ NA_character_),
    cc_bmi = case_when(
      BMXBMI < 25 ~ "<25",
      BMXBMI >= 25 & BMXBMI < 30 ~ "25 to <30",
      BMXBMI >= 30 & BMXBMI < 35 ~ "30 to <35",
      BMXBMI >= 35 ~ "35+"
    ) %>% factor(levels = c("<25", "25 to <30", "30 to <35", "35+")))

miceObj_2021 <- mice::mice(data_2021, method = "cart")
dat_hyp_mice_2021 <- mice::complete(miceObj_2021, "all")

View(dat_hyp_mice_2021[[1]] %>%
       summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
       pivot_longer(cols = everything(), names_to = "Column", values_to = "Missing_Percentage") %>% 
       filter(Missing_Percentage > 0))

dat_hyp_mice_2021 <- lapply(dat_hyp_mice_2021, function(df){
  df <- df  %>% 
    na.omit()
})



categorical_vars_2021 <- dat_hyp_mice_2021[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

dat_hyp_2021_cat <- lapply(dat_hyp_mice_2021, function(x){
  x <- x %>% 
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata) %>% 
    mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars_2021]
})

dat_hyp_2021_continuous <- lapply(dat_hyp_mice_2021, function(x){
  x <- x %>% 
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata)  
  x <- x[!colnames(x) %in% categorical_vars_2021]
  x <- x %>% mutate(across(where(is.double), as.numeric))})

svy_vars_2021 <- lapply(dat_hyp_mice_2021, function(x){x <- x %>% 
  select(SEQN,svy_weight_mec, svy_psu, svy_strata)})

dat_hyp_2021_fci <- Map(cbind, dat_hyp_2021_cat, dat_hyp_2021_continuous)

dat_hyp_2021_final <- Map(cbind, svy_vars_2021, dat_hyp_2021_fci)

save(dat_hyp_2021_fci,dat_hyp_2021_final, file = "data/cleaned/2013_to_2023_cleaned/dat_hyp_2021_fci.RData")
