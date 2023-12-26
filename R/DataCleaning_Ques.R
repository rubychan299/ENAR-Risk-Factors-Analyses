
source("R/funcs.R")

# Load the Data

load("data/cleaned/Questionaires.RData")

quesnames <- c("ACQ","BPQ", "CDQ", "DEQ", "DIQ","DBQ","RXQ", "ECQ", "HIQ", "AUQ", 
               "HUQ", "IMQ", "KIQ", "MCQ", "WHQ", "HOQ", "INQ", "ALQ", "HSQ","DPQ", 
               "SXQ", "PUQMEC", "RHQ","OCQ", "OHQ", "OSQ", "PFQ", "SLQ", "SMQ", "SMQRTU")

ques_dat_new <- q_dat[names(q_dat) %in% quesnames]

quesvars <- readxl::read_excel("data/QuestionaireVars.xlsx",col_names = F, sheet = "Sheet2")$`...1`
quesvars <- sapply(quesvars, function(x) strsplit(x, " - ")[[1]][1])

ques_dat_cleaned <- select_variable(ques_dat_new, quesvars)

ques_df <- combine_surveys(ques_dat_cleaned)

save(ques_df, file = "data/cleaned/ques_df.RData")
