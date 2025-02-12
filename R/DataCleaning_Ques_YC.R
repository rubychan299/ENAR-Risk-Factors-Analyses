
source("R/funcs.R")

# Load the Data

load("data/cleaned/Questionaires.RData")

quesnames <- c("VTQ","PUQ", "DPQ", "DUQ", "HSD","HSQ","ALQ", "IND", "INQ", "HOD", 
               "HOQ", "FSD", "CBD", "WHQ", "SMQ", "KIQ", "OCQ", "OHQ","PAQ", 
               "PFQ", "MCQ")

ques_dat_new <- q_dat[names(q_dat) %in% quesnames]

quesvars <- readxl::read_excel("data/VariableScreening_Questionaire_YC.xlsx", 
                                  sheet = "Final Var List", col_names = FALSE)$`...1`
quesvars <- sapply(quesvars, function(x) strsplit(x, " - ")[[1]][1])

ques_dat_cleaned <- select_variable(ques_dat_new, quesvars)

ques_df <- combine_surveys(ques_dat_cleaned)

# Merge in 2021-2023####
# List all .xpt files with full path
qdata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/questionnaire", full.names = TRUE)

# Read each file and assign it to the global environment
lapply(qdata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

demodata_2021 <- list.files("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/2021_to_2023_data/demographics", full.names = TRUE)

# Read each file and assign it to the global environment
lapply(demodata_2021, function(file) {
  assign(tools::file_path_sans_ext(basename(file)), haven::read_xpt(file), envir = .GlobalEnv)
})

# Get all objects in the global environment
all_objects <- ls(envir = .GlobalEnv)

# Filter objects that end with "_L"
datasets <- grep("_L$", all_objects, value = TRUE)

datasets <- mget(datasets, envir = .GlobalEnv)

datasets <- lapply(datasets, function(df) {
  df <- as.data.frame(df)  # Ensure it's a data frame
  df <- df %>% mutate(Year = '2021')  # Add Year
  return(df)
})

# Perform full joins across all datasets
full_joined_data <- Reduce(function(x, y) full_join(x, y, by = c("SEQN", "Year")), datasets)

full_joined_data <- full_joined_data[colnames(full_joined_data) %in% colnames(ques_df)]


# Bind ques_df with the fully joined dataset
ques_df_joined <- bind_rows(ques_df, full_joined_data)

duplicate_seqn <- ques_df_joined %>%
  group_by(SEQN, Year) %>%
  filter(n() > 1)  # Keep only duplicates

save(ques_df_joined, file = "data/cleaned/ques_df_YC.RData")
