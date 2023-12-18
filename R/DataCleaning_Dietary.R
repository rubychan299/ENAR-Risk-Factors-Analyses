# Support function
select_variable <- function(survey_list, var_name) {
  lapply(survey_list, function(year_list) {
    lapply(year_list, function(df) {
      if(var_name %in% names(df)) {
        dplyr::select(df, sym(var_name))
      } else {
        df
      }
    })
  })
}

load("data/cleaned/Dietary.RData")

dinames <- c("DR1IFF", "DR2IFF","DR1TOT","DR2TOT", "DS1IDS", "DS2IDS", "DSQ1", "DSQ2","DSQTOT","DS1TOT", "DS2TOT")

di_dat_new <- di_dat[names(di_dat) %in% dinames]


table(di_dat_new[["DR1IFF"]]$DRDINT)
table(di_dat_new[["DR2IFF"]]$DRDINT)
# Keep if di_dat_new[["DR2IFF"]]$DRDINT == 2

divars <- readxl::read_excel("data/DietaryVars.xlsx",col_names = F)$`...1`
divars <- sapply(divars, function(x) strsplit(x, " - ")[[1]][1])

di_dat_cleaned <- select_variable(di_dat_new, divars)
