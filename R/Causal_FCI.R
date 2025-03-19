load("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/dat_hyp_final_2013_2020.RData")

library(pcalg)
library(tidyverse)
library(micd)
library(igraph)
library(Rgraphviz)
library(caret)
# library(polycor)
library(tpc)

# Clean data and obtain controlled residuals####

categorical_vars_2013 <- dat_hyp_2013_final[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

# Convert categorical variables to factors
dat_hyp_2013_final_cat <- lapply(dat_hyp_2013_final, function(x){
  x <- x %>%
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -demo_gender,
           -race, -phq9_category, -cc_bmi, -FSDAD, -WHQ030) %>%
  mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars_2013]
  x <- predict(dummyVars("~ .", data = x), x)
  x <- x[,colnames(x)[!grepl("\\.No$", colnames(x))]]
  x <- x[,colnames(x)[!grepl("\\.Fair/Poor$", colnames(x))]]
  })

dat_hyp_2013_final_continuous <- lapply(dat_hyp_2013_final, function(x){
  x <- x %>%
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -demo_gender,
           -race) %>% 
    mutate(across(where(is.character), as.factor)) %>% 
    mutate(phq9_category = as.numeric(phq9_category),
           cc_bmi = as.numeric(cc_bmi), 
           FSDAD = as.numeric(FSDAD), 
           WHQ030 = as.numeric(WHQ030))
  x <- cbind(x[!colnames(x) %in% categorical_vars_2013], x %>% select(phq9_category, cc_bmi, FSDAD, WHQ030))
  x <- x %>% mutate(across(where(is.double), as.numeric))})


dat_hyp_2013_final_control <- lapply(dat_hyp_2013_final, function(x){
  x <- x %>%
    select(demo_age_years, demo_gender, race)})

dat_hyp_2013_fci <- Map(cbind, dat_hyp_2013_final_control, dat_hyp_2013_final_cat, dat_hyp_2013_final_continuous)

# Identify continuous variables
continuous_vars_2013 <- setdiff(names(dat_hyp_2013_fci[[1]]), c("demo_gender", "demo_age_years", "race"))

# Loop through continuous variables and extract residuals

resid_2013 <- lapply(dat_hyp_2013_fci, function(df) {
  # Create an empty dataframe to store residuals
  residuals_df <- df[, c("demo_gender", "demo_age_years", "race")]  # Keep categorical variables
  residuals_df <- as.data.frame(residuals_df)
  
  # Loop through each continuous variable
  for (var in continuous_vars_2013) {
    model <- lm(as.formula(paste0("`", var, "` ~ demo_gender + demo_age_years + race")), data = df)
    residuals_df[[paste0(var, "_resid")]] <- resid(model)
  }
  
  residuals_df <- residuals_df %>% 
    select(-demo_gender, -demo_age_years, -race) # Remove the original variables 
  
  colnames(residuals_df) <- sub("\\..*", "", colnames(residuals_df))
  
  residuals_df <- residuals_df %>% mutate(across(where(is.double), as.numeric))
  
  return(residuals_df)  # Ensure the result is returned
})

categorical_vars_2015 <- dat_hyp_2015_final[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

# Convert categorical variables to factors
dat_hyp_2015_final_cat <- lapply(dat_hyp_2015_final, function(x){
  x <- x %>%
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -demo_gender,
           -race, -phq9_category, -cc_bmi, -FSDHH, -cc_smoke,
           -FSDAD) %>%
    mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars_2013]
  x <- predict(dummyVars("~ .", data = x), x)
  x <- x[,colnames(x)[!grepl("\\.No$", colnames(x))]]
  x <- x[,colnames(x)[!grepl("\\.Average/Bad$", colnames(x))]]
})

dat_hyp_2015_final_continuous <- lapply(dat_hyp_2015_final, function(x){
  x <- x %>%
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -demo_gender,
           -race) %>% 
    mutate(across(where(is.character), as.factor)) %>% 
    mutate(phq9_category = as.numeric(phq9_category),
           cc_bmi = as.numeric(cc_bmi), 
           FSDAD = as.numeric(FSDAD), 
           cc_smoke = as.numeric(cc_smoke))
  x <- cbind(x[!colnames(x) %in% categorical_vars_2015], x %>% select(phq9_category, cc_bmi, FSDAD, cc_smoke))
  x <- x %>% mutate(across(where(is.double), as.numeric))})


dat_hyp_2015_final_control <- lapply(dat_hyp_2015_final, function(x){
  x <- x %>%
    select(demo_age_years, demo_gender, race)})

dat_hyp_2015_fci <- Map(cbind, dat_hyp_2015_final_control, dat_hyp_2015_final_cat, dat_hyp_2015_final_continuous)

# Identify continuous variables
continuous_vars_2015 <- setdiff(names(dat_hyp_2015_fci[[1]]), c("demo_gender", "demo_age_years", "race"))

# Loop through continuous variables and extract residuals

resid_2015 <- lapply(dat_hyp_2015_fci, function(df) {
  # Create an empty dataframe to store residuals
  residuals_df <- df[, c("demo_gender", "demo_age_years", "race")]  # Keep categorical variables
  residuals_df <- as.data.frame(residuals_df)
  
  # Loop through each continuous variable
  for (var in continuous_vars_2015) {
    model <- lm(as.formula(paste0("`", var, "` ~ demo_gender + demo_age_years + race")), data = df)
    residuals_df[[paste0(var, "_resid")]] <- resid(model)
  }
  
  residuals_df <- residuals_df %>% 
    select(-demo_gender, -demo_age_years, -race) # Remove the original variables 
  
  colnames(residuals_df) <- sub("\\..*", "", colnames(residuals_df))
  
  residuals_df <- residuals_df %>% mutate(across(where(is.double), as.numeric))
  
  return(residuals_df)  # Ensure the result is returned
})

categorical_vars_2017 <- dat_hyp_2017_final[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

# Convert categorical variables to factors
dat_hyp_2017_final_cat <- lapply(dat_hyp_2017_final, function(x){
  x <- x %>%
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -demo_gender,
           -race, -phq9_category, -cc_bmi, -WHQ030) %>%
    mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars_2013]
  x <- predict(dummyVars("~ .", data = x), x)
  x <- x[,colnames(x)[!grepl("\\.No$", colnames(x))]]
})

dat_hyp_2017_final_continuous <- lapply(dat_hyp_2017_final, function(x){
  x <- x %>%
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata, -demo_gender,
           -race,-LBXTHG, -URXUCR, -LBXSUA) %>% 
    mutate(across(where(is.character), as.factor)) %>% 
    mutate(phq9_category = as.numeric(phq9_category),
           cc_bmi = as.numeric(cc_bmi), 
           WHQ030 = as.numeric(WHQ030),
           URXUMS = log(URXUMS),
           LBXSTR = log(LBXSTR)
           )
  x <- cbind(x[!colnames(x) %in% categorical_vars_2017], x %>% select(phq9_category, cc_bmi, WHQ030))
  x <- x %>% mutate(across(where(is.double), as.numeric))})


dat_hyp_2017_final_control <- lapply(dat_hyp_2017_final, function(x){
  x <- x %>%
    select(demo_age_years, demo_gender, race)})

dat_hyp_2017_fci <- Map(cbind, dat_hyp_2017_final_control, dat_hyp_2017_final_cat, dat_hyp_2017_final_continuous)

# Identify continuous variables
continuous_vars_2017 <- setdiff(names(dat_hyp_2017_fci[[1]]), c("demo_gender", "demo_age_years", "race"))

# Loop through continuous variables and extract residuals

resid_2017 <- lapply(dat_hyp_2017_fci, function(df) {
  # Create an empty dataframe to store residuals
  residuals_df <- df[, c("demo_gender", "demo_age_years", "race")]  # Keep categorical variables
  residuals_df <- as.data.frame(residuals_df)
  
  # Loop through each continuous variable
  for (var in continuous_vars_2017) {
    model <- lm(as.formula(paste0("`", var, "` ~ demo_gender + demo_age_years + race")), data = df)
    residuals_df[[paste0(var, "_resid")]] <- resid(model)
  }
  
  residuals_df <- residuals_df %>% 
    select(-demo_gender, -demo_age_years, -race) # Remove the original variables 
  
  colnames(residuals_df) <- sub("\\..*", "", colnames(residuals_df))
  
  residuals_df <- residuals_df %>% mutate(across(where(is.double), as.numeric))
  
  return(residuals_df)  # Ensure the result is returned
})

load("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/dat_hyp_2021_fci.RData")

categorical_vars_2021 <- dat_hyp_2021_fci[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

# Convert categorical variables to factors
dat_hyp_2021_final_cat <- lapply(dat_hyp_2021_fci, function(x){
  x <- x %>%
    select(-demo_gender,-demo_race, -cc_smoke, -cc_bmi) %>%
    mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars_2021]
  x <- predict(dummyVars("~ .", data = x), x)
  x <- x[,colnames(x)[!grepl("\\.No$", colnames(x))]]
  x <- x[,colnames(x)[!grepl("\\.Fair/Poor$", colnames(x))]]
})

dat_hyp_2021_final_continuous <- lapply(dat_hyp_2021_fci, function(x){
  x <- x %>%
    select(-demo_gender,-demo_race, -LBXTHG, -BMXBMI) %>% 
    mutate(across(where(is.character), as.factor)) %>%
    mutate(cc_smoke = as.numeric(cc_smoke),
           cc_bmi = as.numeric(cc_bmi))
  x <- cbind(x[!colnames(x) %in% categorical_vars_2021], x %>% select(cc_smoke, cc_bmi))
  x <- x %>% mutate(across(where(is.double), as.numeric))})


dat_hyp_2021_final_control <- lapply(dat_hyp_2021_fci, function(x){
  x <- x %>%
    select(demo_age_years, demo_gender, demo_race)})

dat_hyp_2021_fci <- Map(cbind, dat_hyp_2021_final_control, dat_hyp_2021_final_cat, dat_hyp_2021_final_continuous)

# Identify continuous variables
continuous_vars_2021 <- setdiff(names(dat_hyp_2021_fci[[1]]), c("demo_gender", "demo_age_years", "demo_race"))

# Loop through continuous variables and extract residuals

resid_2021 <- lapply(dat_hyp_2021_fci, function(df) {
  # Create an empty dataframe to store residuals
  residuals_df <- df[, c("demo_gender", "demo_age_years", "demo_race")]  # Keep categorical variables
  residuals_df <- as.data.frame(residuals_df)
  
  # Loop through each continuous variable
  for (var in continuous_vars_2021) {
    model <- lm(as.formula(paste0("`", var, "` ~ demo_gender + demo_age_years + demo_race")), data = df)
    residuals_df[[paste0(var, "_resid")]] <- resid(model)
  }
  
  residuals_df <- residuals_df %>% 
    select(-demo_gender, -demo_age_years, -demo_race) # Remove the original variables 
  
  colnames(residuals_df) <- sub("\\..*", "", colnames(residuals_df))
  
  residuals_df <- residuals_df %>% mutate(across(where(is.double), as.numeric))
  
  return(residuals_df)  # Ensure the result is returned
})

# Run fci algorithm####

load("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/dat_hyp_fci_2013_2017.RData")

## Define tiers####
Tiers_2013 <- c(3, 3, 3, 3, 4, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 3, 2, 1, 2)
Context_2013 <- c("HIQ011", "OHQ845", "PFQ049", "PFQ054", "bp_control_jnc7", "cc_diabetes", "cc_ckd", "cc_cvd_chd", 
                  "cc_cvd_stroke", "cc_cvd_hf", "cc_cvd_any", "LBXSBU_resid", "LBDMONO_resid", "LBXRBCSI_resid",
                  "LBXTC_resid", "phq9_category_resid","cc_bmi_resid","FSDAD_resid", "WHQ030_resid")

Tiers_2015 <- c(3, 4, 2, 2, 2, 2, 2, 3, 1, 3, 2, 1, 1)
Context_2015 <- c("HIQ011", "bp_control_jnc7", "cc_diabetes", "cc_ckd", "cc_cvd_chd", "cc_cvd_hf", "cc_cvd_any", 
                  "weight_change_resid", "LBXTC_resid","phq9_category_resid","cc_bmi_resid",      
                  "FSDAD_resid","cc_smoke_resid")

Tiers_2017 <- c(3, 4, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2)
Context_2017 <- c("HIQ011", "bp_control_jnc7", "cc_diabetes", "cc_ckd", "URXUMS_resid", "LBXSBU_resid", 
                  "LBXSTR_resid", "LBDMONO_resid", "LBXBPB_resid", 
                  "LBXTC_resid", "phq9_category_resid","cc_bmi_resid","WHQ030_resid")

Tiers_2021 <- c(3, 3, 3, 4, 2, 1, 1, 1, 1, 1, 3, 1, 2)
Context_2021 <- c("HIQ011", "KIQ022", "OHQ845", "bp_control_jnc7", "cc_cvd_any", "cc_diabetes", 
                  "LBXRBCSI_resid", "LBXBPB_resid", "LBXTC_resid", 
                  "LBDMONO_resid", "weight_change_resid", "cc_smoke_resid", "cc_bmi_resid")

# tier 1: lab variables
# tier 2: diseases 
# tier 3: life style factors + byproduct of diseases/lab variables 
# tier 4: bp control

## 2013-2014####

fci.fit.2013 <- fciPlus(suffStat = resid_2013, indepTest = mixMItest, alpha = 0.05,
              labels = colnames(resid_2013[[1]]))

pc.fit.2013 <- tpc(suffStat = resid_2013, indepTest = mixMItest, alpha = 0.05,
              labels = colnames(resid_2013[[1]]), tiers = Tiers_2013)

# Extract adjacency matrix
adj_matrix_2013 <- as(fci.fit.2013@amat, "matrix")  # Convert to standard matrix format

# Create graph from adjacency matrix
graph_fci_2013 <- graph_from_adjacency_matrix(adj_matrix_2013, mode = "directed", diag = FALSE)

# Plot the graph
# plot(graph_fci_2013, 
#      vertex.label = V(graph_fci_2013)$name, 
#      vertex.size = 20, 
#      edge.arrow.size = 0.5, 
#      main = "FCI Causal Graph")

# plot(graph_fci_2013)

# Highlight the outcome variable
V(graph_fci_2013)$color <- ifelse(V(graph_fci_2013)$name == "bp_control_jnc7", "red", "lightblue")

# Plot the causal graph
plot(graph_fci_2013, 
     vertex.label = V(graph_fci_2013)$name, 
     vertex.size = 10, 
     edge.arrow.size = 0.05, 
     main = paste("Causal Graph 2013-2014 - Outcome:", "bp_control_jnc7"))

# Create a node attribute list to change the color of the outcome variable
attrs <- list(
  node = list(fillcolor = "lightblue", fontsize = 14),  # Default style
  edge = list(color = "black")
)

# Modify the outcome variable color
nodeAttrs <- list(fillcolor = setNames(rep("red", 1), "bp_control_jnc7"))  # Outcome node in red

# Plot the graph with the highlighted outcome variable
# plot(pc.fit.2013@graph, attxrs = attrs, nodeAttrs = nodeAttrs)

# Convert PC adjacency matrix to igraph format
graph_pc_2013 <- igraph::graph_from_graphnel(pc.fit.2013@graph)

# Modify node colors: Red for outcome, blue for other nodes
V(graph_pc_2013)$color <- ifelse(V(graph_pc_2013)$name == "bp_control_jnc7", "red", "lightblue")

# Plot with customized labels and colors
plot(graph_pc_2013, 
     vertex.label = V(graph_pc_2013)$name, 
     vertex.size = 15, 
     edge.arrow.size = 0.15, 
     main = "PC Algorithm 2013-2014 - Causal Graph",
     vertex.color = V(graph_pc_2013)$color)


x <- as.matrix(resid_2013[[1]][,colnames(resid_2013[[1]]) != "bp_control_jnc7"])
y <- resid_2013[[1]][,colnames(resid_2013[[1]]) %in% "bp_control_jnc7"]

direct_causes_2013 <- pcSelect(y = y, dm = x, alpha = 0.05)
print(direct_causes_2013)
with(direct_causes_2013, zMin[G])

save(fci.fit.2013, pc.fit.2013, direct_causes_2013, file = "data/cleaned/2013_to_2023_cleaned/fci_pc_2013.RData")

## 2015-2016####
fci.fit.2015 <- fciPlus(suffStat = resid_2015, indepTest = mixMItest, alpha = 0.05,
                    labels = colnames(resid_2015[[1]]))
pc.fit.2015 <- tpc(suffStat = resid_2015, indepTest = mixMItest, alpha = 0.05,
                  labels = colnames(resid_2015[[1]]), tiers = Tiers_2015)

# Plot the graph with the highlighted outcome variable
plot(pc.fit.2015@graph, attrs = attrs, nodeAttrs = nodeAttrs)

# Convert PC adjacency matrix to igraph format
graph_pc_2015 <- igraph::graph_from_graphnel(pc.fit.2015@graph)

# Modify node colors: Red for outcome, blue for other nodes
V(graph_pc_2015)$color <- ifelse(V(graph_pc_2015)$name == "bp_control_jnc7", "red", "lightblue")

# Plot with customized labels and colors
plot(graph_pc_2015, 
     vertex.label = V(graph_pc_2015)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = "PC Algorithm 2015-2016 - Causal Graph",
     vertex.color = V(graph_pc_2015)$color)
# Extract adjacency matrix
adj_matrix_2015 <- as(fci.fit.2015@amat, "matrix")  # Convert to standard matrix format

# Create graph from adjacency matrix
graph_fci_2015 <- graph_from_adjacency_matrix(adj_matrix_2015, mode = "directed", diag = FALSE)

# Plot the graph
plot(graph_fci_2015, 
     vertex.label = V(graph_fci_2015)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.05, 
     main = "FCI Causal Graph")

plot(fci.fit.2015)

# Highlight the outcome variable
V(graph_fci_2015)$color <- ifelse(V(graph_fci_2015)$name == "bp_control_jnc7", "red", "lightblue")

# Plot the causal graph
plot(graph_fci_2015, 
     vertex.label = V(graph_fci_2015)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = paste("Causal Graph 2015-2016 - Outcome:", "bp_control_jnc7"))

x <- as.matrix(resid_2015[[1]][,colnames(resid_2015[[1]]) != "bp_control_jnc7"])
y <- resid_2015[[1]][,colnames(resid_2015[[1]]) %in% "bp_control_jnc7"]

direct_causes_2015 <- pcSelect(y = y, dm = x, alpha = 0.05)
print(direct_causes_2015)
with(direct_causes_2015, zMin[G])

save(fci.fit.2015, pc.fit.2015, direct_causes_2015, file = "data/cleaned/2013_to_2023_cleaned/fci_pc_2015.RData")

## 2017-2020####
fci.fit.2017 <- fciPlus(suffStat = resid_2017, indepTest = mixMItest, alpha = 0.05,
                    labels = colnames(resid_2017[[1]]))

pc.fit.2017 <- tpc(suffStat = resid_2017, indepTest = mixMItest, alpha = 0.05,
                   labels = colnames(resid_2017[[1]]), tiers = Tiers_2017)

# Plot the graph with the highlighted outcome variable
plot(pc.fit.2017@graph, attrs = attrs, nodeAttrs = nodeAttrs)

# Convert PC adjacency matrix to igraph format
graph_pc_2017 <- igraph::graph_from_graphnel(pc.fit.2017@graph)

# Modify node colors: Red for outcome, blue for other nodes
V(graph_pc_2017)$color <- ifelse(V(graph_pc_2017)$name == "bp_control_jnc7", "red", "lightblue")

# Plot with customized labels and colors
plot(graph_pc_2017, 
     vertex.label = V(graph_pc_2017)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = "PC Algorithm 2017-2020 - Causal Graph",
     vertex.color = V(graph_pc_2017)$color)
# Extract adjacency matrix
adj_matrix_2017 <- as(fci.fit.2017@amat, "matrix")  # Convert to standard matrix format

# Create graph from adjacency matrix
graph_fci_2017 <- graph_from_adjacency_matrix(adj_matrix_2017, mode = "directed", diag = FALSE)

# Plot the graph
plot(graph_fci_2017, 
     vertex.label = V(graph_fci_2017)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = "FCI Causal Graph")

plot(fci.fit.2017)

# Highlight the outcome variable
V(graph_fci_2017)$color <- ifelse(V(graph_fci_2017)$name == "bp_control_jnc7", "red", "lightblue")

# Plot the causal graph
plot(graph_fci_2017, 
     vertex.label = V(graph_fci_2017)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = paste("Causal Graph 2017-2020 - Outcome:", "bp_control_jnc7"))

x <- as.matrix(resid_2017[[1]][,colnames(resid_2017[[1]]) != "bp_control_jnc7"])
y <- resid_2017[[1]][,colnames(resid_2017[[1]]) %in% "bp_control_jnc7"]

direct_causes_2017 <- pcSelect(y = y, dm = x, alpha = 0.05)
print(direct_causes_2017)
with(direct_causes_2017, zMin[G])

save(fci.fit.2017, pc.fit.2017, direct_causes_2017, file = "data/cleaned/2013_to_2023_cleaned/fci_pc_2017.RData")

## 2021-2023####

fci.fit.2021 <- fciPlus(suffStat = resid_2021, indepTest = mixMItest, alpha = 0.05,
                        labels = colnames(resid_2021[[1]]))

pc.fit.2021 <- tpc(suffStat = resid_2021, indepTest = mixMItest, alpha = 0.05,
                   labels = colnames(resid_2021[[1]]), tiers = Tiers_2021)

# Plot the graph with the highlighted outcome variable
plot(pc.fit.2021@graph, attrs = attrs, nodeAttrs = nodeAttrs)

# Convert PC adjacency matrix to igraph format
graph_pc_2021 <- igraph::graph_from_graphnel(pc.fit.2021@graph)

# Modify node colors: Red for outcome, blue for other nodes
V(graph_pc_2021)$color <- ifelse(V(graph_pc_2021)$name == "bp_control_jnc7", "red", "lightblue")

# Plot with customized labels and colors
plot(graph_pc_2021, 
     vertex.label = V(graph_pc_2021)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = "PC Algorithm 2021-2023 - Causal Graph",
     vertex.color = V(graph_pc_2021)$color)
# Extract adjacency matrix
adj_matrix_2021 <- as(fci.fit.2021@amat, "matrix")  # Convert to standard matrix format

# Create graph from adjacency matrix
graph_fci_2021 <- graph_from_adjacency_matrix(adj_matrix_2021, mode = "directed", diag = FALSE)

# Plot the graph
plot(graph_fci_2021, 
     vertex.label = V(graph_fci_2021)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = "FCI Causal Graph")

plot(fci.fit.2021)

# Highlight the outcome variable
V(graph_fci_2021)$color <- ifelse(V(graph_fci_2021)$name == "bp_control_jnc7", "red", "lightblue")

# Plot the causal graph
plot(graph_fci_2021, 
     vertex.label = V(graph_fci_2021)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = paste("Causal Graph 2021-2023 - Outcome:", "bp_control_jnc7"))

x <- as.matrix(resid_2021[[1]][,colnames(resid_2021[[1]]) != "bp_control_jnc7"])
y <- resid_2021[[1]][,colnames(resid_2021[[1]]) %in% "bp_control_jnc7"]

direct_causes_2021 <- pcSelect(y = y, dm = x, alpha = 0.05)
print(direct_causes_2021)
with(direct_causes_2021, zMin[G])

save(fci.fit.2021, pc.fit.2021, direct_causes_2021, file = "data/cleaned/2013_to_2023_cleaned/fci_pc_2021.RData")

## requires packages: pcalg, igraph, RGBL

# IDA Calculation####

source("~/Documents/GitHub/lv-ida/lvida.R")

### sometimes the algorithms in pcalg can return a cyclic graph (for some alpha setting)
### LV-IDA (and IDA) won't work in such cases so this is a script to alert you
source("~/Documents/GitHub/lv-ida/iscyclic.R")
###

if(is.cyclic(as(pc.fit.2013@graph, "matrix"))){
  cat("#### FOUND CYCLIC GRAPH #### \n LV-IDA won't work here! \n try again! \n")
}

## 2013-2014####
# Compute covariance matrix
cov_matrix_2013 <- cov(resid_2013[[1]])

# Check determinant (should not be near zero)
det(cov_matrix_2013)

isValidGraph(as(pc.fit.2013@graph, "matrix"), type = "cpdag")
pc_cpdag_2013 <- dag2cpdag(pc.fit.2013@graph)

# Run jointIda with the regularized covariance matrix
ida.total.2013 <- jointIda(x.pos = c(1:4, 6:19), y.pos = 5, cov_matrix_2013, pc_cpdag_2013, technique = "RRC", type = "cpdag")
ida.total.2013 <- apply(ida.total.2013, 1, mean)
names(ida.total.2013) <- colnames(cov_matrix_2013)[-5]

ida.est.2013 <- vector("list", ncol(cov_matrix_2013))

for(i in 1:ncol(cov_matrix_2013)){
  ida.est.2013[[i]] <- try(idaFast(
    x.pos = i,  # All variables as potential causes
    y.pos = 1:ncol(cov_matrix_2013),  # All variables as potential effects
    cov_matrix_2013,       # Covariance matrix
    pc_cpdag_2013      # CPDAG from TPC output
  ))
}

names(ida.est.2013) <- colnames(cov_matrix_2013)

ida.est.2013 <- lapply(ida.est.2013, function(x) {
  x <- try(apply(x, 1, mean))
  if (inherits(x, "try-error")) {
    return(NA)
  } else {
    return(x)
  }
})

ida.est.2013 <- do.call(cbind, ida.est.2013)
rownames(ida.est.2013) <- colnames(cov_matrix_2013)

## 2015-2016####
# Compute covariance matrix
cov_matrix_2015 <- cov(resid_2015[[1]])

# Check determinant (should not be near zero)
det(cov_matrix_2015)

# lv.ida.est.2015 <- lv.ida(x.pos = 1,y.pos = 5,cov_matrix, fci.fit.2015@amat, method="global")
# lv.ida.est.2015 # this is the multiset of causal effects of x = 2 on y = pvar

isValidGraph(as(pc.fit.2015@graph, "matrix"), type = "cpdag")
pc_cpdag_2015 <- dag2cpdag(pc.fit.2015@graph)

# Run jointIda with the regularized covariance matrix
ida.total.2015 <- jointIda(x.pos = c(1, 3:13), y.pos = 2, cov_matrix_2015, pc_cpdag_2015, technique = "RRC", type = "cpdag")
ida.total.2015 <- apply(ida.total.2015, 1, mean)
names(ida.total.2015) <- colnames(cov_matrix_2015)[-2]

ida.est.2015 <- vector("list", ncol(cov_matrix_2015))

for(i in 1:ncol(cov_matrix_2015)){
  ida.est.2015[[i]] <- try(idaFast(
    x.pos = i,  # All variables as potential causes
    y.pos = 1:ncol(cov_matrix_2015),  # All variables as potential effects
    cov_matrix_2015,       # Covariance matrix
    pc_cpdag_2015      # CPDAG from TPC output
  ))
}

names(ida.est.2015) <- colnames(cov_matrix_2015)

ida.est.2015 <- lapply(ida.est.2015, function(x) {
  x <- try(apply(x, 1, mean))
  if (inherits(x, "try-error")) {
    return(NA)
  } else {
    return(x)
  }
})

ida.est.2015 <- do.call(cbind, ida.est.2015)
rownames(ida.est.2015) <- colnames(cov_matrix_2015)

## 2017-2020####
# Compute covariance matrix
cov_matrix_2017 <- cov(resid_2017[[1]])

# Check determinant (should not be near zero)
det(cov_matrix_2017)

isValidGraph(as(pc.fit.2017@graph, "matrix"), type = "cpdag")
pc_cpdag_2017 <- dag2cpdag(pc.fit.2017@graph)

# Run jointIda with the regularized covariance matrix
ida.total.2017 <- jointIda(x.pos = c(1, 3:13), y.pos = 2, cov_matrix_2017, pc_cpdag_2017, technique = "RRC", type = "cpdag")
ida.total.2017 <- apply(ida.total.2017, 1, mean)

names(ida.total.2017) <- colnames(cov_matrix_2017)[-2]

ida.est.2017 <- vector("list", ncol(cov_matrix_2017))

for(i in 1:ncol(cov_matrix_2017)){
  ida.est.2017[[i]] <- try(idaFast(
    x.pos = i,  # All variables as potential causes
    y.pos = 1:ncol(cov_matrix_2017),  # All variables as potential effects
    cov_matrix_2017,       # Covariance matrix
    pc_cpdag_2017      # CPDAG from TPC output
  ))
}

names(ida.est.2017) <- colnames(cov_matrix_2017)

ida.est.2017 <- lapply(ida.est.2017, function(x) {
  x <- try(apply(x, 1, mean))
  if (inherits(x, "try-error")) {
    return(NA)
  } else {
    return(x)
  }
})

ida.est.2017 <- do.call(cbind, ida.est.2017)
rownames(ida.est.2017) <- colnames(cov_matrix_2017)

## 2021-2023####
# Compute covariance matrix
summary(resid_2021[[1]])
cov_matrix_2021 <- cov(resid_2021[[1]])

# Check determinant (should not be near zero)
det(cov_matrix_2021)

isValidGraph(as(pc.fit.2021@graph, "matrix"), type = "cpdag")
pc_cpdag_2021 <- dag2cpdag(pc.fit.2021@graph)

# Run jointIda with the regularized covariance matrix
ida.total.2021 <- ida(x.pos = 1, y.pos = 4, cov_matrix_2021, pc_cpdag_2021,method = "global", type = "cpdag")

ida.total.2021 <- jointIda(x.pos = c(1:3, 5:13), y.pos = 4, cov_matrix_2021, pc_cpdag_2021, technique = "RRC", type = "cpdag")
ida.total.2021 <- apply(ida.total.2021, 1, mean)

names(ida.total.2021) <- colnames(cov_matrix_2021)[-4]

ida.est.2021 <- vector("list", ncol(cov_matrix_2021))

for(i in 1:ncol(cov_matrix_2021)){
  ida.est.2021[[i]] <- try(idaFast(
    x.pos = i,  # All variables as potential causes
    y.pos = 1:ncol(cov_matrix_2021),  # All variables as potential effects
    cov_matrix_2021,       # Covariance matrix
    pc_cpdag_2021      # CPDAG from TPC output
  ))
}

names(ida.est.2021) <- colnames(cov_matrix_2021)

ida.est.2021 <- lapply(ida.est.2021, function(x) {
  x <- try(apply(x, 1, mean))
  if (inherits(x, "try-error")) {
    return(NA)
  } else {
    return(x)
  }
})

ida.est.2021 <- do.call(cbind, ida.est.2021)
rownames(ida.est.2021) <- colnames(cov_matrix_2021)

save(ida.total.2013, ida.est.2013, ida.total.2015, ida.est.2015, ida.total.2017, ida.est.2017, 
     ida.total.2021,
     ida.est.2021, file = "data/cleaned/2013_to_2023_cleaned/ida_2013_2021.RData")
