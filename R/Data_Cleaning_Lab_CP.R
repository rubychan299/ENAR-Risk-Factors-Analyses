rm(list = ls())
source("funcs.R")
# Load the Data
load("Laboratory.RData")
library(haven)
library(dplyr)

labnames <- c("ALB_CR", "L40", "L05", "CBC", "CRP", "L06TFR", "GLU", "GHB", "UIO", "L06HM", 
              "OPD", "HPVSWR", "VOCMWB", "PAH", "PHTHTE", "PERNT", "UAS", "PBCD", "HEPA", 
              "L02HBS", "HEPC", "COT", "HDL", "TRIGLY", "TCHOL")

lab_dat_new <- lab_dat[names(lab_dat) %in% labnames]

labvars <- readxl::read_excel("LaboratoryVars.xlsx",col_names = F)$`...1`
labvars <- sapply(labvars, function(x) strsplit(x, " - ")[[1]][1])

lab_dat_cleaned <- select_variable(lab_dat_new, labvars)

lab_df <- combine_surveys(lab_dat_cleaned)

keep <- c(labvars, "Year", "SEQN")


# Merge in 2021-2023####
# List all .xpt files with full path

# Read XPT files and add Year = 2021
# List all dataset names
datasets <- list(
  FERTIN_L = read_xpt("LAB2021-2023/FERTIN_L.xpt"),
  GHB_L    = read_xpt("LAB2021-2023/GHB_L.xpt"),
  GLU_L    = read_xpt("LAB2021-2023/GLU_L.xpt"),
  HDL_L    = read_xpt("LAB2021-2023/HDL_L.xpt"),
  HEPA_L   = read_xpt("LAB2021-2023/HEPA_L.xpt"),
  PBCD_L   = read_xpt("LAB2021-2023/PBCD_L.xpt"),
  TCHOL_L  = read_xpt("LAB2021-2023/TCHOL_L.xpt")
)

# Add Year = 2021 to all datasets in the list
datasets <- lapply(datasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = 2021)  # Add Year
  return(df)
})

# Perform full joins across all datasets
full_joined_data <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets)
full_joined_data <- full_joined_data[colnames(full_joined_data) %in% colnames(lab_df)]

# Ensure Year is numeric in all datasets before merging
lab_df <- lab_df %>%
  mutate(Year = as.numeric(gsub("[^0-9]", "", Year)))  # Remove non-numeric characters
full_joined_data <- full_joined_data %>% mutate(Year = as.numeric(Year))

lab_joined <- bind_rows(lab_df, full_joined_data)


duplicate_seqn <- lab_joined %>%
  group_by(SEQN, Year) %>%
  filter(n() > 1)  # Keep only duplicates
# Get all objects in the global environment
all_objects <- ls(envir = .GlobalEnv)

lab_dat_cleaned$L06HM$URXUCR <- NULL # Creatinine already covered in ALB_CR, removing duplicate 


save(lab_df, file = "Lab_df_Cindy.RData")

library(remotes)
library(summarytools)
library(dplyr)
summary <- dfSummary(lab_df)
view(summary)