
source("R/funcs.R")

# Load the Data

load("data/cleaned/Dietary.RData")

dinames <- c("DR1TOT","DR2TOT", "DSQ1", "DSQ2","DSQTOT","DS1TOT", "DS2TOT")

di_dat_new <- di_dat[names(di_dat) %in% dinames]


divars <- readxl::read_excel("data/DietaryVars.xlsx",col_names = F)$`...1`
divars <- sapply(divars, function(x) strsplit(x, " - ")[[1]][1])

di_dat_cleaned <- select_variable(di_dat_new, divars)

di_df <- combine_surveys(di_dat_cleaned)

