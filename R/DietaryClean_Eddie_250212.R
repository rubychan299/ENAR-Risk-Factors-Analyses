
source("R/funcs.R")
library(haven)
library(dplyr)

# Load 1999-2020 exam data (nhanesA format)
load("data/cleaned/Dietary.RData")

dia_names <- c("DR1TOT", "DSQIDS")

dia_dat_new <- di_dat[names(di_dat) %in% dia_names]

diavars <- readxl::read_excel("data/DietaryVars.xlsx",col_names = F, sheet = "Sheet2")$`...1`
diavars <- sapply(diavars, function(x) strsplit(x, " - ")[[1]][1])

dia_dat_cleaned <- select_variable(dia_dat_new, diavars)

dia_df <- combine_surveys(dia_dat_cleaned)

# need to fix what variables to keep

keep <- c(diavars, "Year", "SEQN")

# read in 2021-2023
DR1TOT_L <- read_xpt("data/2021_to_2023_data/dietary/DR1TOT_L.xpt")

# Add Year = 2021 to all smaller datasets
datasets <- list(DR1TOT_L)

datasets <- lapply(datasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  # Add Year
  return(df)
})

# Perform full joins across all datasets
full_joined_data <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets)

# Bind ex_df with the fully joined dataset
ex_df_joined <- bind_rows(dia_df, full_joined_data)

temp <- ex_df_joined[colnames(ex_df_joined) %in% keep]

temp <- temp %>% filter(Year %in% c(2011, 2013, 2015, "P", 2021))

save(ques_df, file = "data/cleaned/dias_Eddie_df.RData")