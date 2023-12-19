
source("R/funcs.R")

# Load the Data

load("data/cleaned/Laboratory.RData")

labnames <- c("ALB_CR", "L40", "L05", "CBC", "CRP", "L06TFR", "GLU", "GHB", "UIO", "L06HM", 
              "OPD", "HPVSWR", "VOCMWB", "PAH", "PHTHTE", "PERNT", "UAS", "PBCD", "HEPA", 
              "L02HBS", "HEPC", "COT", "HDL", "TRIGLY", "TCHOL")

lab_dat_new <- lab_dat[names(lab_dat) %in% labnames]

labvars <- readxl::read_excel("data/LaboratoryVars.xlsx",col_names = F)$`...1`
labvars <- sapply(labvars, function(x) strsplit(x, " - ")[[1]][1])

lab_dat_cleaned <- select_variable(lab_dat_new, labvars)

lab_df <- combine_surveys(lab_dat_cleaned)

save(lab_df, file = "data/cleaned/Lab_df.RData")
