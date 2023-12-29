rm(list = ls())
library(tidyverse)

# Data PreProcessing####

# load the data

load("data/cleaned/dat_hyp_final_svy.RData")

# Rename the variables by removing spaces
names(dat_hyp_final) <- gsub(" ", "", names(dat_hyp_final))
# Replace spaces, periods, and plus signs with underscores
names(dat_hyp_final) <- gsub(" ", "_", names(dat_hyp_final))
names(dat_hyp_final) <- gsub("\\.", "_", names(dat_hyp_final))
names(dat_hyp_final) <- gsub("\\+", "plus", names(dat_hyp_final))
names(dat_hyp_final) <- gsub("\\-", "plus", names(dat_hyp_final))
names(dat_hyp_final) <- gsub(">", "", names(dat_hyp_final))
names(dat_hyp_final) <- gsub("<", "", names(dat_hyp_final))

# Drop probelmatic vars
dat_hyp_final <- dat_hyp_final %>% 
  select(-contains("DSD010_x"), -contains("DRDINT_x"), -contains("DRDINT_y"), -LBDHDDSI,
         -URXUMS, -URXCRS, -contains("BPQ050A"), -contains("cc_cvd_stroke"), -contains("svy_subpop_chol"))

# subset the data into pre 2013 and post 2013

dat_hyp_final_pre <- dat_hyp_final %>% 
  filter(svy_year == "2003-2004" | svy_year == "2005-2006" | svy_year == "2007-2008" | svy_year == "2009-2010" | svy_year == "2011-2012")

dat_hyp_final_post <- dat_hyp_final %>% 
  filter(svy_year == "2013-2014" | svy_year == "2015-2016" | svy_year == "2017-2020")

dat_hyp_final <- dat_hyp_final %>% 
  filter(svy_year != "1999-2000" | svy_year == "2001-2002")

# split into 80% training and 20% testing
set.seed(2024)
dat_hyp_final_pre_ind <- sample(1:nrow(dat_hyp_final_pre), 0.8*nrow(dat_hyp_final_pre))
dat_hyp_final_pre_train <- dat_hyp_final_pre[dat_hyp_final_pre_ind, ]
dat_hyp_final_pre_test <- dat_hyp_final_pre[-dat_hyp_final_pre_ind, ]

write.csv(dat_hyp_final_pre_train, file = paste0("data/cleaned/dat_hyp_final_pre_train.csv"), row.names = FALSE)
write.csv(dat_hyp_final_pre_test, file = paste0("data/cleaned/dat_hyp_final_pre_test.csv"), row.names = FALSE)

dat_hyp_final_post_ind <- sample(1:nrow(dat_hyp_final_post), 0.8*nrow(dat_hyp_final_post))
dat_hyp_final_post_train <- dat_hyp_final_post[dat_hyp_final_post_ind, ]
dat_hyp_final_post_test <- dat_hyp_final_post[-dat_hyp_final_post_ind, ]

write.csv(dat_hyp_final_post_train, file = paste0("data/cleaned/dat_hyp_final_post_train.csv"), row.names = FALSE)
write.csv(dat_hyp_final_post_test, file = paste0("data/cleaned/dat_hyp_final_post_test.csv"), row.names = FALSE)

dat_hyp_final_ind <- sample(1:nrow(dat_hyp_final), 0.8*nrow(dat_hyp_final))
dat_hyp_final_train <- dat_hyp_final[dat_hyp_final_ind, ]
dat_hyp_final_test <- dat_hyp_final[-dat_hyp_final_ind, ]

write.csv(dat_hyp_final_train, file = paste0("data/cleaned/dat_hyp_final_train.csv"), row.names = FALSE)
write.csv(dat_hyp_final_test, file = paste0("data/cleaned/dat_hyp_final_test.csv"), row.names = FALSE)


# Perform resampling to create multiple training sets

num_replicates <- 10  
dat_hyp_final_pre_samp <- dat_hyp_final_post_samp <- dat_hyp_final_samp <- vector("list", length = num_replicates)

for (i in 1:num_replicates) {
  dat_hyp_final_pre_samp[[i]] <- dat_hyp_final_pre_train[sample(1:nrow(dat_hyp_final_pre_train), replace = TRUE), ]
  filename <- paste0("data/cleaned/bootstrapped/dat_hyp_final_pre_samp_", i, ".csv")
  write.csv(dat_hyp_final_pre_samp[[i]], file = filename, row.names = FALSE)
  
  dat_hyp_final_post_samp[[i]] <- dat_hyp_final_post_train[sample(1:nrow(dat_hyp_final_post_train), replace = TRUE), ]
  filename <- paste0("data/cleaned/bootstrapped/dat_hyp_final_post_samp_", i, ".csv")
  write.csv(dat_hyp_final_post_samp[[i]], file = filename, row.names = FALSE)
  
  dat_hyp_final_samp[[i]] <- dat_hyp_final_train[sample(1:nrow(dat_hyp_final_train), replace = TRUE), ]
  filename <- paste0("data/cleaned/bootstrapped/dat_hyp_final_samp_samp_", i, ".csv")
  write.csv(dat_hyp_final_samp[[i]], file = filename, row.names = FALSE)
  
}

save(dat_hyp_final_pre, dat_hyp_final_post, dat_hyp_final, 
     dat_hyp_final_pre_train, dat_hyp_final_pre_test,
     dat_hyp_final_post_train, dat_hyp_final_post_test,
     dat_hyp_final_train, dat_hyp_final_test,
     dat_hyp_final_pre_samp, dat_hyp_final_post_samp, dat_hyp_final_samp,
     weights,
     file = "data/cleaned/dat_train_test_bootstrapped.RData")
