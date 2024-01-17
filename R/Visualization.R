library(tidyverse)
library(ggplot2)
library(survey)
library(gtsummary)
library(MASS)
library(car)
library(glmnet)
library(tidymodels)
library(hdi)

# load the data####

load("data/cleaned/dat_train_test_bootstrapped.RData")
load("data/cleaned/dat_hyp_final_svy.RData")

# the extracted features####

lasso_1 <- unique(read_csv("tables/Logistic/final_selected_features_bp_control_jnc7.csv")$Feature)
lasso_2 <- unique(read_csv("tables/Logistic/final_selected_features_bp_control_accaha.csv")$Feature)
lasso_3 <- unique(read_csv("tables/Logistic/final_selected_features_bp_control_140_90.csv")$Feature)
lasso_4 <- unique(read_csv("tables/Logistic/final_selected_features_bp_control_130_80.csv")$Feature)

rf_1 <- unique(read_csv("tables/rf/bp_control_jnc7_features_final_rf.csv")$Feature)
rf_2 <- unique(read_csv("tables/rf/bp_control_accaha_features_final_rf.csv")$Feature)
rf_3 <- unique(read_csv("tables/rf/bp_control_140_90_features_final_rf.csv")$Feature)
rf_4 <- unique(read_csv("tables/rf/bp_control_130_80_features_final_rf.csv")$Feature)

xgboost_1 <- unique(read_csv("tables/xgboost/bp_control_jnc7_features_final.csv")$Feature)
xgboost_2 <- unique(read_csv("tables/xgboost/bp_control_accaha_features_final.csv")$Feature)
xgboost_3 <- unique(read_csv("tables/xgboost/bp_control_140_90_features_final.csv")$Feature)
xgboost_4 <- unique(read_csv("tables/xgboost/bp_control_130_80_features_final.csv")$Feature)

tabnet_1 <- unique(read_csv("tables/tabnet/final_jnc7.csv")$Feature)
tabnet_2 <- unique(read_csv("tables/tabnet/final_accaha.csv")$Feature)
tabnet_3 <- unique(read_csv("tables/tabnet/final_140_90.csv")$Feature)
tabnet_4 <- unique(read_csv("tables/tabnet/final_130_80.csv")$Feature)

jnc7 <- c(xgboost_1, tabnet_1, rf_1, lasso_1)
jnc7 <- names(table(jnc7)[table(jnc7) >= 2])
accaha <- c(xgboost_2, tabnet_2, rf_2, lasso_2)
accaha <- names(table(accaha)[table(accaha) >= 2])

jnc7 <- jnc7[!jnc7 %in% c("BPXSY2","BPXSY3")]
accaha <- accaha[!accaha %in% c("BPXSY1", "BPXSY2", "BPXSY3", "BMXARMC","BMXARML", "BMXLEG",
                                "bp_med_n_pills_None", "bp_med_n_pills_One","bp_med_pills_gteq_2_No",                                
                                "bp_med_recommended_accaha_No","bp_med_recommended_accaha_Yes",                         
                                "bp_med_recommended_jnc7_No", "bp_med_recommended_jnc7_Yes",
                                "BPAEN2_1", "BPQ020_2","BPXDI1", "BPXDI2", "BPXDI3","BPXML1",                                               
                                "cc_cvd_any_No","DPQ020_0","DPQ070_0", "DPQ080_1","DR1DAY",                                               
                                "DR1EXMER","HOD050", "HSQ590_9", "htn_aware_No",
                                "htn_resistant_accaha_No","htn_resistant_accaha_Yes","LBDNENO","OSQ060_1", "URXUCR", "WHD120",
                                "demo_age_cat_18to44","demo_age_cat_65to74","LBXHA_2"  )]

# jnc7 <- c("demo_race", "demo_gender","demo_age_years",
#           "bp_cat_meds_excluded","bp_cat_meds_included", "bp_dia_mean",
#           "bp_med_recommended_jnc7", "bp_sys_mean", "cc_ckd", "cc_diabetes", 
#           "htn_resistant_jnc7")
# 
# accaha <- c("demo_race", "demo_gender","demo_age_years", "BMXBMI",
#             "bp_cat_meds_excluded","bp_cat_meds_included", "bp_dia_mean",
#             "bp_med_ace", "bp_med_aldo", "bp_med_angioten", "bp_med_diur_thz", "bp_med_n_class",
#             "bp_sys_mean", "BPQ040A", "BPQ090D", "BPXPLS","DSDCOUNT.x",
#             "LBDHDD", "LBDMONO", "LBXEOPCT", "LBXLYPCT", "LBXPLTSI",
#             "MCQ080", "MCQ160A")


# Pre 2013####
jnc7_y <- dat_hyp_final_pre$bp_control_jnc7

jnc7_x <- as.matrix(dat_hyp_final_pre[jnc7])

pre2013.cv <- cv.glmnet(jnc7_x, jnc7_y, family = "binomial", alpha = 0.5, weights = dat_hyp_final_pre$svy_weight_mec)
pre2013.cv$lambda.min

pre2013 <- glmnet(jnc7_x, jnc7_y, family = "binomial", alpha = 0.5, weights = dat_hyp_final_pre$svy_weight_mec, lambda = pre2003.cv$lambda.min)
coef(pre2013)

if (is.factor(jnc7_y)) {
  jnc7_y <- as.numeric(as.factor(jnc7_y)) - 1
}

pre2013.fit <- lasso.proj(jnc7_x, jnc7_y, family = "binomial", parallel = T)
pre2013.fit$betahat
pval.pre2013.fit <- pre2013.fit$pval
ci.pre2013.fit <- confint(pre2013.fit)
# Post 2013####
jnc7_y <- dat_hyp_final_post$bp_control_jnc7

jnc7_x <- as.matrix(dat_hyp_final_post[jnc7])

post2013.cv <- cv.glmnet(jnc7_x, jnc7_y, family = "binomial", alpha = 0.5, weights = dat_hyp_final_post$svy_weight_mec)
post2013.cv$lambda.min

post2013 <- glmnet(jnc7_x, jnc7_y, family = "binomial", alpha = 0.5, weights = dat_hyp_final_post$svy_weight_mec, lambda = post2013.cv$lambda.min)
coef(post2013)

if (is.factor(jnc7_y)) {
  jnc7_y <- as.numeric(as.factor(jnc7_y)) - 1
}
post2013.fit <- lasso.proj(jnc7_x, jnc7_y, family = "binomial")


pval.post2013.fit <- post2013.fit$pval
ci.post2013.fit <- confint(post2013.fit)


plot.jnc7.pre <- data.frame(coeff = coef(pre2013)[-1,1],
                       features = names(coef(pre2013)[-1,]),
                       ci.lower = ci.pre2013.fit[,1],
                       ci.upper = ci.pre2013.fit[,2],
                       p.val = pval.pre2013.fit)
plot.jnc7.pre <- plot.jnc7.pre %>% 
  mutate(sig = case_when(p.val < 0.05 ~ "Significant",
                         p.val >= 0.05 ~ "Not-significnat"))

plot.jnc7.pre$years <- "Pre-2013"

plot.jnc7.post <- data.frame(coeff = coef(post2013)[-1,1],
                             features = names(coef(post2013)[-1,]),
                             ci.lower = ci.post2013.fit[,1],
                             ci.upper = ci.post2013.fit[,2],
                             p.val = pval.post2013.fit)

plot.jnc7.post <- plot.jnc7.post %>% 
  mutate(sig = case_when(p.val < 0.05 ~ "Significant",
                         p.val >= 0.05 ~ "Not-significnat"))

plot.jnc7.post$years <- "Post-2013"

plot.jnc7 <- rbind(plot.jnc7.pre, plot.jnc7.post)

ggplot(data = plot.jnc7) +
  geom_point(mapping = aes(x = features, y = coeff, group = sig, color = sig)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  facet_wrap(~years)

  
# ACC/AHA####

# Pre 2013####
accaha_y <- dat_hyp_final_pre$bp_control_accaha

accaha_x <- as.matrix(dat_hyp_final_pre[accaha])

accahapre2013.cv <- cv.glmnet(accaha_x, accaha_y, family = "binomial", alpha = 0.5, weights = dat_hyp_final_pre$svy_weight_mec)
accahapre2013.cv$lambda.min

accahapre2013 <- glmnet(accaha_x, accaha_y, family = "binomial", alpha = 0.5, weights = dat_hyp_final_pre$svy_weight_mec, lambda = accahapre2013.cv$lambda.min)
coef(accahapre2013)

if (is.factor(accaha_y)) {
  accaha_y <- as.numeric(as.factor(accaha_y)) - 1
}

accahapre2013.fit <- lasso.proj(accaha_x, accaha_y, family = "binomial")
pval.accahapre2013.fit <- accahapre2013.fit$pval
ci.accahapre2013.fit <- confint(accahapre2013.fit)

# Post 2013####
accaha_y <- dat_hyp_final_post$bp_control_accaha

accaha_x <- as.matrix(dat_hyp_final_post[accaha])

accahapost2013.cv <- cv.glmnet(accaha_x, accaha_y, family = "binomial", alpha = 0.5, weights = dat_hyp_final_post$svy_weight_mec)
accahapost2013.cv$lambda.min

accahapost2013 <- glmnet(accaha_x, accaha_y, family = "binomial", alpha = 0.5, weights = dat_hyp_final_post$svy_weight_mec, lambda = accahapost2013.cv$lambda.min)
coef(accahapost2013)

if (is.factor(accaha_y)) {
  accaha_y <- as.numeric(as.factor(accaha_y)) - 1
}

accahapost2013.fit <- lasso.proj(accaha_x, accaha_y, family = "binomial")
pval.accahapost2013.fit <- accahapost2013.fit$pval
ci.accahapost2013.fit<- confint(accahapost2013.fit)

plot.accaha.pre <- data.frame(coeff = coef(accahapre2013)[-1,1],
                            features = names(coef(accahapre2013)[-1,]),
                            ci.lower = ci.accahapre2013.fit[,1],
                            ci.upper = ci.accahapre2013.fit[,2],
                            p.val = pval.accahapre2013.fit)
plot.accaha.pre <- plot.accaha.pre %>% 
  mutate(sig = case_when(p.val < 0.05 ~ "Significant",
                         p.val >= 0.05 ~ "Not-significnat"))

plot.accaha.pre$years <- "Pre-2013"

plot.accaha.post <- data.frame(coeff = coef(accahapost2013)[-1,1],
                             features = names(coef(accahapost2013)[-1,]),
                             ci.lower = ci.accahapost2013.fit[,1],
                             ci.upper = ci.accahapost2013.fit[,2],
                             p.val = pval.accahapost2013.fit)

plot.accaha.post <- plot.accaha.post %>% 
  mutate(sig = case_when(p.val < 0.05 ~ "Significant",
                         p.val >= 0.05 ~ "Not-significnat"))

plot.accaha.post$years <- "Post-2013"

plot.accaha <- rbind(plot.accaha.pre, plot.accaha.post)
View(cbind(features = plot.accaha.pre$features, coeff.pre = plot.accaha.pre$coeff, coeff.post = plot.accaha.post$coeff))

p.accaha <- ggplot(data = plot.accaha) +
  geom_point(mapping = aes(x = features, y = coeff, group = sig, color = sig)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  facet_wrap(~years) +
  ggtitle("Lasso Coefficients for BP Control with ACC/AHA guideline")
ggsave("plots/final_plot_lasso_accaha.png", p.accaha, width = 16, height =  8, units = "in")
  
# try 0.1 threshold
plot.accaha.pre <- data.frame(coeff = coef(accahapre2013)[-1,1],
                              features = names(coef(accahapre2013)[-1,]),
                              ci.lower = ci.accahapre2013.fit[,1],
                              ci.upper = ci.accahapre2013.fit[,2],
                              p.val = pval.accahapre2013.fit)
plot.accaha.pre <- plot.accaha.pre %>% 
  mutate(sig = case_when(p.val < 0.1 ~ "Significant",
                         p.val >= 0.1 ~ "Not-significnat"))

plot.accaha.pre$years <- "Pre-2013"

plot.accaha.post <- data.frame(coeff = coef(accahapost2013)[-1,1],
                               features = names(coef(accahapost2013)[-1,]),
                               ci.lower = ci.accahapost2013.fit[,1],
                               ci.upper = ci.accahapost2013.fit[,2],
                               p.val = pval.accahapost2013.fit)

plot.accaha.post <- plot.accaha.post %>% 
  mutate(sig = case_when(p.val < 0.1 ~ "Significant",
                         p.val >= 0.1 ~ "Not-significnat"))

plot.accaha.post$years <- "Post-2013"

plot.accaha <- rbind(plot.accaha.pre, plot.accaha.post)
View(cbind(features = plot.accaha.pre$features, coeff.pre = plot.accaha.pre$coeff, coeff.post = plot.accaha.post$coeff))

p.accaha.01 <- ggplot(data = plot.accaha) +
  geom_point(mapping = aes(x = features, y = coeff, group = sig, color = sig)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  facet_wrap(~years) +
  ggtitle("Lasso Coefficients for BP Control with ACC/AHA guideline")
ggsave("plots/final_plot_lasso_accaha_v2.png", p.accaha.01, width = 16, height =  8, units = "in")


