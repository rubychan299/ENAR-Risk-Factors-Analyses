rm(list = ls())
library(splines)
library(tidyverse)
library(ggplot2)
library(survey)
library(gtsummary)
library(MASS)
library(car)
library(glmnet)
library(tidymodels)

# load the data####

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
t140_90 <- c(xgboost_3, tabnet_3, rf_3, lasso_3)
t140_90 <- names(table(t140_90)[table(t140_90) >= 2])
t130_80 <- c(xgboost_4, tabnet_4, rf_4, lasso_4)
t130_80 <- names(table(t130_80)[table(t130_80) >= 2])


# Bivariate table####
dat_hyp_cleaned <- dat_hyp_cleaned %>% 
  select(-contains("DSD010_x"), -contains("DRDINT_x"), -contains("DRDINT_y"), -LBDHDDSI,
         -URXUMS, -URXCRS, -contains("BPQ050A"), -contains("cc_cvd_stroke"), -contains("svy_subpop_chol"))

dat_hyp_cleaned <- dat_hyp_cleaned %>% 
  filter(svy_year != "1999-2000" | svy_year != "2001-2002")

dat_hyp_cleaned <- dat_hyp_cleaned %>% 
  mutate(svy_year = case_when(svy_year == "2003-2004" ~ 2003,
                              svy_year == "2005-2006" ~ 2005,
                              svy_year == "2007-2008" ~ 2007,
                              svy_year == "2009-2010" ~ 2009,
                              svy_year == "2011-2012" ~ 2011,
                              svy_year == "2013-2014" ~ 2013,
                              svy_year == "2015-2016" ~ 2015,
                              svy_year == "2017-2020" ~ 2017))

dat_hyp_cleaned <- dat_hyp_cleaned %>% 
  mutate(Year = case_when(svy_year < 2013 ~ "Pre 2013",
                          svy_year >= 2013 ~ "Post 2013"))

## Survey Design####
dat_hyp_cleaned_svy <- svydesign(
  data = dat_hyp_cleaned, 
  ids = ~svy_psu,
  strata = ~svy_strata,
  weights = ~svy_weight_mec, 
  nest = T
)

table1 <- tbl_svysummary(dat_hyp_cleaned_svy,
                         missing = "no", 
                         statistic = list(all_continuous() ~ "{mean}({sd})", all_categorical() ~ "{n} ({p}%)"))
table1 %>%
  as_gt() %>%
  gt::gtsave(filename = "tables/univariate.html")

table2 <- tbl_svysummary(dat_hyp_cleaned_svy,
                         missing = "no", 
                         by = "bp_control_jnc7",
                         statistic = list(all_continuous() ~ "{mean}({sd})", all_categorical() ~ "{n} ({p}%)")) %>% 
  add_p()

table2 %>%
  as_gt() %>%
  gt::gtsave(filename = "tables/bivariate_bp_control_jnc7.html")

table3 <- tbl_svysummary(dat_hyp_cleaned_svy,
                         missing = "no", 
                         by = "bp_control_accaha",
                         statistic = list(all_continuous() ~ "{mean}({sd})", all_categorical() ~ "{n} ({p}%)")) %>% 
  add_p()


table3 %>%
  as_gt() %>%
  gt::gtsave(filename = "tables/bivariate_bp_control_accaha.html")

table4 <- tbl_svysummary(dat_hyp_cleaned_svy,
                         missing = "no", 
                         by = "Year",
                         statistic = list(all_continuous() ~ "{mean}({sd}, {mean.std.error})", all_categorical() ~ "{n} ({p}%, {p.std.error})")) %>% 
  add_p()

table4 %>%
  as_gt() %>%
  gt::gtsave(filename = "tables/bivariate_Year.html")


# Logistic Spline Regression with Complex Survey design####

dat_hyp_mice <- dat_hyp_mice %>% 
  dplyr::select(-contains("DSD010_x"), -contains("DRDINT_x"), -contains("DRDINT_y"), -LBDHDDSI,
         -URXUMS, -URXCRS, -contains("BPQ050A"), -contains("cc_cvd_stroke"), -contains("svy_subpop_chol"))

# dat_hyp_mice <- dat_hyp_mice %>% 
#   filter(svy_year != "1999-2000" | svy_year != "2001-2002")

dat_hyp_mice <- dat_hyp_mice %>%
  filter(svy_year == "2003-2004" | svy_year == "2005-2006" | svy_year == "2007-2008" | svy_year == "2009-2010" | svy_year == "2011-2012" | svy_year == "2013-2014" | svy_year == "2015-2016" | svy_year == "2017-2020")

dat_hyp_mice <- dat_hyp_mice %>% 
  mutate(svy_year = case_when(svy_year == "2003-2004" ~ 2003,
                              svy_year == "2005-2006" ~ 2005,
                              svy_year == "2007-2008" ~ 2007,
                              svy_year == "2009-2010" ~ 2009,
                              svy_year == "2011-2012" ~ 2011,
                              svy_year == "2013-2014" ~ 2013,
                              svy_year == "2015-2016" ~ 2015,
                              svy_year == "2017-2020" ~ 2017))

dat_hyp_mice <- dat_hyp_mice %>% 
  mutate(BPQ040A = as.factor(BPQ040A),
         BPQ090D = as.factor(BPQ090D),
         MCQ080 = as.factor(MCQ080),
         MCQ160A = as.factor(MCQ160A))

## Survey Design####
dat_hyp_svy <- svydesign(
  data = dat_hyp_mice, 
  ids = ~svy_psu,
  strata = ~svy_strata,
  weights = ~svy_weight_mec, 
  nest = T
)

## JNC7####

jnc7 <- c("demo_race", "demo_race_black", "demo_gender","demo_age_years",
          "bp_cat_meds_excluded","bp_cat_meds_included", "bp_dia_mean",
         "bp_med_n_pills", "bp_med_recommended_accaha",
         "bp_med_recommended_jnc7", "bp_med_use", "bp_sys_mean",
         "BPXSY2", "BPXSY3", "cc_ckd", "cc_diabetes", 
         "htn_resistant_jnc7")

formula_str <- paste0("bp_control_jnc7 ~ ",paste(jnc7, collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
formula_obj <- as.formula(formula_str)

### Check for multicolinearity####

y <- matrix(dat_hyp_mice$bp_control_jnc7)
x <- as.matrix(dat_hyp_mice[jnc7])

ridge_jnc7 <- cv.glmnet(x, y, weights = dat_hyp_mice$svy_weight_mec, alpha = 0, family="binomial")
lambda <- ridge_jnc7$lambda.min
ridge_jnc7 <- glmnet(x, y, weights = dat_hyp_mice$svy_weight_mec, alpha = 0, family="binomial", lambda = lambda)
coef(ridge_jnc7)

library(ggcorrplot)
model.matrix(~0+., data=dat_hyp_mice[,jnc7]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

#### Update the variable list based on the correlation####

jnc7 <- c("demo_race", "demo_gender","demo_age_years",
          "bp_cat_meds_excluded","bp_cat_meds_included", "bp_dia_mean",
          "bp_med_n_pills", "bp_med_recommended_jnc7", "bp_sys_mean", "cc_ckd", "cc_diabetes", 
          "htn_resistant_jnc7")

formula_str <- paste0("bp_control_jnc7 ~ ",paste(jnc7, collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
formula_obj <- as.formula(formula_str)


# model_jnc7 <- glm(formula_obj, data = dat_hyp_mice, family = "binomial")
model_jnc7 <- svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial", control = glm.control(maxit = 50))
summary(model_jnc7)
AIC(model_jnc7)
vif(model_jnc7)

### by p-val####
# spline_liner <- "bs(svy_year, degree =1, knots = c(2013))"
# final_model_jnc7 <- stepwise_elimination(data = dat_hyp_mice, design = dat_hyp_svy, response_var = "bp_control_jnc7", predictors = jnc7, spline_terms = spline_liner, p_threshold = 0.1)

model_summary <- summary(model_jnc7)
p_values <- coef(model_summary)[, "Pr(>|t|)"]
variable_to_remove <- names(p_values[p_values > 0.1])
predictors <- setdiff(jnc7, variable_to_remove)

formula_str <- paste0("bp_control_jnc7 ~ ",paste(predictors, collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
formula_obj <- as.formula(formula_str)

final_model_jnc7_pval <- svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial", control = glm.control(maxit = 50))
summary(final_model_jnc7_pval)
AIC(final_model_jnc7_pval)

### by AIC####

final_model_jnc7_aic <- stepAIC(model_jnc7, direction = "both")

### Start with univariate regression####

for(i in seq_along(jnc7)){
  
  formula_str <- paste0("bp_control_jnc7 ~ ",paste(jnc7[i], collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
  formula_obj <- as.formula(formula_str)
  
  jnc7_uni<- svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial", control = glm.control(maxit = 50))
  print(summary(jnc7_uni))
  
}

jnc7 <- c("demo_race", "demo_gender","demo_age_years",
          "bp_cat_meds_excluded","bp_cat_meds_included", "bp_dia_mean",
          "bp_med_recommended_jnc7", "bp_sys_mean", "cc_ckd", "cc_diabetes", 
          "htn_resistant_jnc7")


formula_str <- paste0("bp_control_jnc7 ~ ",paste(jnc7, collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
formula_obj <- as.formula(formula_str)

model_jnc7 <- svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial", control = glm.control(maxit = 50))
summary(model_jnc7)

model_jnc7_tab <- tbl_regression(model_jnc7)

## ACC/AHA####

accaha <- c("demo_race", "demo_race_black", "demo_gender","demo_age_years", 
            "BMXARMC", "BMXARML", "BMXBMI", "BMXLEG",
            "bp_cat_meds_excluded","bp_cat_meds_included", "bp_dia_mean",
            "bp_med_ace", "bp_med_aldo", "bp_med_angioten", "bp_med_diur_thz", "bp_med_n_class",
            "bp_med_n_pills", "bp_med_pills_gteq_2", "bp_med_recommended_accaha", "bp_med_recommended_jnc7",
            "bp_med_use", "bp_sys_mean", "BPAEN2", "BPQ020", "BPQ040A", "BPQ090D",
            "BPXDI1", "BPXDI2", "BPXDI3", "BPXML1", "BPXPLS", "BPXSY1", "BPXSY2", "BPXSY3", 
            "cc_cvd_any", "DPQ020", "DPQ070", "DPQ080", "DR1DAY", "DR1EXMER",
            "DSDCOUNT.x", "HOD050", "HSQ590", "htn_aware", "htn_resistant_accaha",
            "LBDHDD", "LBDMONO", "LBDNENO", "LBXEOPCT", "LBXHA", "LBXLYPCT", "LBXPLTSI",
            "MCQ080", "MCQ160A", "OSQ060", "URXUCR", "WHD120")

### Check for multicolinearity####

model.matrix(~0+., data=dat_hyp_mice[,accaha]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

#### Update the variable list based on the correlation####

accaha <- c("demo_race", "demo_gender","demo_age_years", "BMXBMI", 
            "bp_cat_meds_excluded","bp_cat_meds_included", "bp_dia_mean",
            "bp_med_ace", "bp_med_aldo", "bp_med_angioten", "bp_med_diur_thz", "bp_med_n_class",
            "bp_sys_mean", "BPAEN2", "BPQ040A", "BPQ090D", "BPXPLS", "DPQ070", "DPQ080",
            "DSDCOUNT.x", "HOD050", "HSQ590",
            "LBDHDD", "LBDMONO", "LBDNENO", "LBXEOPCT", "LBXHA", "LBXLYPCT", "LBXPLTSI",
            "MCQ080", "MCQ160A", "OSQ060", "URXUCR", "WHD120")

model.matrix(~0+., data=dat_hyp_mice[,accaha]) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)


formula_str <- paste0("bp_control_accaha ~ ",paste(accaha, collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
formula_obj <- as.formula(formula_str)

model_accaha <- svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial",control = glm.control(maxit = 50))
summary(model_accaha)
vif(model_accaha)
AIC(model_accaha)

### by AIC####

final_model_accaha_aic <- stepAIC(svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial",control = glm.control(maxit = 50)), direction = "both")

### by p-val####
model_summary <- summary(model_accaha)
p_values <- coef(model_summary)[, "Pr(>|t|)"]
variable_to_remove <- names(p_values[p_values > 0.5])
predictors <- setdiff(accaha, variable_to_remove)

formula_str <- paste0("bp_control_accaha ~ ",paste(predictors, collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
formula_obj <- as.formula(formula_str)

final_model_accaha_pval <- svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial", control = glm.control(maxit = 50))
summary(final_model_accaha_pval)


### Start with univariate regression####

for(i in seq_along(accaha)){
  
  formula_str <- paste0("bp_control_accaha ~ ",paste(accaha[i], collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
  formula_obj <- as.formula(formula_str)
  
  accaha_uni<- svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial", control = glm.control(maxit = 50))
  print(summary(accaha_uni))
  
}


accaha <- c("demo_race", "demo_gender","demo_age_years", "BMXBMI", 
            "bp_cat_meds_excluded","bp_cat_meds_included", "bp_dia_mean",
            "bp_med_ace", "bp_med_aldo", "bp_med_angioten", "bp_med_diur_thz", "bp_med_n_class",
            "bp_sys_mean", "BPQ040A", "BPQ090D", "BPXPLS","DSDCOUNT.x",
            "LBDHDD", "LBDMONO", "LBXEOPCT", "LBXLYPCT", "LBXPLTSI",
            "MCQ080", "MCQ160A")

formula_str <- paste0("bp_control_accaha ~ ",paste(accaha, collapse = " + "), "+ bs(svy_year, degree =1, knots = c(2013))")
formula_obj <- as.formula(formula_str)

model_accaha <- svyglm(formula_obj, design = dat_hyp_svy, family = "quasibinomial",control = glm.control(maxit = 50))
summary(model_accaha)
vif(model_accaha)
AIC(model_accaha)

model_accaha_tab <- tbl_regression(model_accaha)

model_output <-
  tbl_merge(
    tbls = list(model_jnc7_tab, model_accaha_tab),
    tab_spanner = c("**JNC7**", "**ACC/AHA**")
  )

model_output %>%
  as_gt() %>%
  gt::gtsave(filename = "tables/final_models.html")

table(dat_hyp_mice$bp_control_accaha, dat_hyp_mice$svy_year)

# Visualization
year.grid <-seq(range(dat_hyp_mice$svy_year)[1], range(dat_hyp_mice$svy_year)[2], length.out = 26147)
pred.spline <- predict(model_accaha, se=T)
plot.data <- data.frame(x = year.grid, y = pred.spline)

ggplot() +
  geom_point(data = plot.data, mapping = aes(x = x, y = y), color = 'grey') +
  geom_smooth(method="lm",
              formula=  y ~ splines::bs(x, knots = c(2013), degree = 1))

ggplot() +
  geom_line(aes(x, y.link), data = plot.data, color = "red") +  # Spline curve
  theme_minimal() +
  labs(title = "Linear Spline", x = "Race", y = "BP Control ACC/AHA") +
  ggtitle("BP Control ACC/AHA")
