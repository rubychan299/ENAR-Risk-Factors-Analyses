load("data/cleaned/Dietary.RData")

dinames <- c("DR1IFF", "DR2IFF","DR1TOT","DR2TOT", "DS1IDS", "DS2IDS", "DSQ1", "DSQ2","DSQTOT","DS1TOT", "DS2TOT")

di_dat_new <- di_dat[names(di_dat) %in% dinames]


