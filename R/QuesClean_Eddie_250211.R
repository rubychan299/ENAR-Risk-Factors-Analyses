
source("R/funcs.R")
library(haven)
library(dplyr)

# Load 1999-2020 exam data (nhanesA format)
load("data/cleaned/Questionaires.RData")

quesnames <- c("ACQ","BPQ", "CDQ", "DEQ", "DIQ","DBQ","RXQ", "ECQ", "HIQ", "AUQ", "HUQ", "IMQ")

ques_dat_new <- q_dat[names(q_dat) %in% quesnames]

quesvars <- readxl::read_excel("data/QuestionaireVars.xlsx",col_names = F, sheet = "Sheet3")$`...1`
quesvars <- sapply(quesvars, function(x) strsplit(x, " - ")[[1]][1])

ques_dat_cleaned <- select_variable(ques_dat_new, quesvars)

ques_df <- combine_surveys(ques_dat_cleaned)

keep <- c("SEQN", "Year", "ACD040", "BPQ100D", "DIQ010", "IMQ020")

# read in 2021-2023
ACQ_L <- read_xpt("data/2021_to_2023_data/questionnaire/ACQ_L.xpt")
BPQ_L <- read_xpt("data/2021_to_2023_data/questionnaire/BPQ_L.xpt")
DIQ_L <- read_xpt("data/2021_to_2023_data/questionnaire/DIQ_L.xpt")
IMQ_L <- read_xpt("data/2021_to_2023_data/questionnaire/IMQ_L.xpt")

# Add Year = 2021 to all smaller datasets
datasets <- list(ACQ_L, BPQ_L, DIQ_L, IMQ_L)

datasets <- lapply(datasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  # Add Year
  return(df)
})

# Perform full joins across all datasets
full_joined_data <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets)

# Bind ex_df with the fully joined dataset
ex_df_joined <- bind_rows(ques_df, full_joined_data)

temp <- ex_df_joined[colnames(ex_df_joined) %in% keep]

temp <- temp %>% filter(Year %in% c(2011, 2013, 2015, "P", 2021))

save(ques_df, file = "data/cleaned/ques_Eddie_df.RData")
