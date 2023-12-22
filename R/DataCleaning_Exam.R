
source("R/funcs.R")

# Load the Data

load("data/cleaned/Examinations.RData")

exnames <- c("AUX", "BPX", "BMX")

ex_dat_new <- ex_dat[names(ex_dat) %in% exnames]

exvars <- readxl::read_excel("data/ExamVars.xlsx",col_names = F)$`...1`
exvars <- sapply(exvars, function(x) strsplit(x, " - ")[[1]][1])

ex_dat_cleaned <- select_variable(ex_dat_new, exvars)

ex_df <- combine_surveys(ex_dat_cleaned)

save(ex_df, file = "data/cleaned/Exam_df.RData")
