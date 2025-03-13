load("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/dat_hyp_final_2013_2020.RData")

library(pcalg)
library(tidyverse)
library(micd)
library(igraph)
library(Rgraphviz)
library(caret)

categorical_vars_2013 <- dat_hyp_2013_final[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

dat_hyp_2013_final_cat <- lapply(dat_hyp_2013_final, function(x){
  x <- x %>% 
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata) %>% 
  mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars_2013]
  x <- predict(dummyVars("~ .", data = x), x)
  x <- x[,colnames(x)[!grepl("\\.No$", colnames(x))]] 
  })

dat_hyp_2013_final_continuous <- lapply(dat_hyp_2013_final, function(x){
  x <- x %>% 
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata)  
  x <- x[!colnames(x) %in% categorical_vars_2013]})

dat_hyp_2013_fci <- Map(cbind, dat_hyp_2013_final_cat, dat_hyp_2013_final_continuous)

categorical_vars_2015 <- dat_hyp_2015_final[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

dat_hyp_2015_final_cat <- lapply(dat_hyp_2015_final, function(x){
  x <- x %>% 
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata) %>% 
    mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars_2015]
  x <- predict(dummyVars("~ .", data = x), x)
  x <- x[,colnames(x)[!grepl("\\.No$", colnames(x))]] 
})

dat_hyp_2015_final_continuous <- lapply(dat_hyp_2015_final, function(x){
  x <- x %>% 
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata)  
  x <- x[!colnames(x) %in% categorical_vars_2015]})

dat_hyp_2015_fci <- Map(cbind, dat_hyp_2015_final_cat, dat_hyp_2015_final_continuous)


categorical_vars_2017 <- dat_hyp_2017_final[[1]] %>% select(where(is.character), where(is.factor)) %>% colnames()

dat_hyp_2017_final_cat <- lapply(dat_hyp_2017_final, function(x){
  x <- x %>% 
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata) %>% 
    mutate(across(where(is.character), as.factor))
  x <- x[colnames(x) %in% categorical_vars_2017]
  x <- predict(dummyVars("~ .", data = x), x)
  x <- x[,colnames(x)[!grepl("\\.No$", colnames(x))]] 
})

dat_hyp_2017_final_continuous <- lapply(dat_hyp_2017_final, function(x){
  x <- x %>% 
    select(-SEQN,-svy_weight_mec, -svy_psu, -svy_strata)  
  x <- x[!colnames(x) %in% categorical_vars_2017]})

dat_hyp_2017_fci <- Map(cbind, dat_hyp_2017_final_cat, dat_hyp_2017_final_continuous)

save(dat_hyp_2013_fci, dat_hyp_2015_fci, dat_hyp_2017_fci, file = "data/cleaned/2013_to_2023_cleaned/dat_hyp_fci_2013_2017.RData")

# Run fci algorithm

load("~/Documents/GitHub/ENAR-Risk-Factors-Analyses/data/cleaned/2013_to_2023_cleaned/dat_hyp_fci_2013_2017.RData")

# forbidden_edges <- matrix(T, nrow = length(colnames(dat_hyp_2013_fci[[1]])), ncol = length(colnames(dat_hyp_2013_fci[[1]])), 
#                           dimnames = list(colnames(dat_hyp_2013_fci[[1]]), colnames(dat_hyp_2013_fci[[1]])))
# 
# # Prevent direct edges from Tier 1 → Tier 3 (forcing mediation)
# for (x in tier_1) {
#   for (y in tier_3) {
#     forbidden_edges[x, y] <- F
#   }
# }

fci.fit.2013 <- fciPlus(suffStat = dat_hyp_2013_fci, indepTest = mixMItest, alpha = 0.05,
              labels = colnames(dat_hyp_2013_fci[[1]]))
pc.fit.2013 <- pc(suffStat = dat_hyp_2013_fci, indepTest = mixMItest, alpha = 0.05,
              labels = colnames(dat_hyp_2013_fci[[1]]))
# Extract adjacency matrix
adj_matrix_2013 <- as(fci.fit.2013@amat, "matrix")  # Convert to standard matrix format

# Create graph from adjacency matrix
graph_fci_2013 <- graph_from_adjacency_matrix(adj_matrix_2013, mode = "directed", diag = FALSE)

# Plot the graph
plot(graph_fci_2013, 
     vertex.label = V(graph_fci_2013)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.5, 
     main = "FCI Causal Graph")

plot(graph_fci_2013)

# Create a node attribute list to change the color of the outcome variable
attrs <- list(
  node = list(fillcolor = "lightblue", fontsize = 14),  # Default style
  edge = list(color = "black")
)

# Modify the outcome variable color
nodeAttrs <- list(fillcolor = setNames(rep("red", 1), "bp_control_jnc7.Yes"))  # Outcome node in red

# Plot the graph with the highlighted outcome variable
plot(pc.fit.2013@graph, attrs = attrs, nodeAttrs = nodeAttrs)

# Convert PC adjacency matrix to igraph format
graph_pc_2013 <- igraph::graph_from_graphnel(pc.fit.2013@graph)

# Modify node colors: Red for outcome, blue for other nodes
V(graph_pc_2013)$color <- ifelse(V(graph_pc_2013)$name == "bp_control_jnc7.Yes", "red", "lightblue")

# Plot with customized labels and colors
plot(graph_pc_2013, 
     vertex.label = V(graph_pc_2013)$name, 
     vertex.size = 15, 
     edge.arrow.size = 0.15, 
     main = "PC Algorithm 2013-2014 - Causal Graph",
     vertex.color = V(graph_pc_2013)$color)



# Highlight the outcome variable
V(graph_fci_2013)$color <- ifelse(V(graph_fci_2013)$name == "bp_control_jnc7.Yes", "red", "lightblue")

# Plot the causal graph
plot(graph_fci_2013, 
     vertex.label = V(graph_fci_2013)$name, 
     vertex.size = 10, 
     edge.arrow.size = 0.05, 
     main = paste("Causal Graph 2013-2014 - Outcome:", "bp_control_jnc7"))

x <- as.matrix(dat_hyp_2013_fci[[1]][,colnames(dat_hyp_2013_fci[[1]]) != "bp_control_jnc7.Yes"])
y <- dat_hyp_2013_fci[[1]][,colnames(dat_hyp_2013_fci[[1]]) %in% "bp_control_jnc7.Yes"]

direct_causes_2013 <- pcSelect(y = y, dm = x, alpha = 0.05)
print(direct_causes_2013)
with(direct_causes_2013, zMin[G])



fci.fit.2015 <- fciPlus(suffStat = dat_hyp_2015_fci, indepTest = mixMItest, alpha = 0.01,
                    labels = colnames(dat_hyp_2015_fci[[1]]))
pc.fit.2015 <- pc(suffStat = dat_hyp_2015_fci, indepTest = mixMItest, alpha = 0.05,
                  labels = colnames(dat_hyp_2015_fci[[1]]))

# Plot the graph with the highlighted outcome variable
plot(pc.fit.2015@graph, attrs = attrs, nodeAttrs = nodeAttrs)

# Convert PC adjacency matrix to igraph format
graph_pc_2015 <- igraph::graph_from_graphnel(pc.fit.2015@graph)

# Modify node colors: Red for outcome, blue for other nodes
V(graph_pc_2015)$color <- ifelse(V(graph_pc_2015)$name == "bp_control_jnc7.Yes", "red", "lightblue")

# Plot with customized labels and colors
plot(graph_pc_2015, 
     vertex.label = V(graph_pc_2013)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = "PC Algorithm - Causal Graph",
     vertex.color = V(graph_pc_2013)$color)
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
V(graph_fci_2015)$color <- ifelse(V(graph_fci_2015)$name == "bp_control_jnc7.Yes", "red", "lightblue")

# Plot the causal graph
plot(graph_fci_2015, 
     vertex.label = V(graph_fci_2015)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = paste("Causal Graph 2015-2016 - Outcome:", "bp_control_jnc7"))

x <- as.matrix(dat_hyp_2015_fci[[1]][,colnames(dat_hyp_2015_fci[[1]]) != "bp_control_jnc7.Yes"])
y <- dat_hyp_2015_fci[[1]][,colnames(dat_hyp_2015_fci[[1]]) %in% "bp_control_jnc7.Yes"]

direct_causes_2015 <- pcSelect(y = y, dm = x, alpha = 0.05)
print(direct_causes_2015)
with(direct_causes_2015, zMin[G])

save(fci.fit.2015, adj_matrix_2015, graph_fci_2015, direct_causes_2015, file = "data/cleaned/2013_to_2023_cleaned/fci_2015.RData")

fci.fit.2017 <- fciPlus(suffStat = dat_hyp_2017_fci, indepTest = mixMItest, alpha = 0.01,
                    labels = colnames(dat_hyp_2017_fci[[1]]))
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
V(graph_fci_2017)$color <- ifelse(V(graph_fci_2017)$name == "bp_control_jnc7.Yes", "red", "lightblue")

# Plot the causal graph
plot(graph_fci_2017, 
     vertex.label = V(graph_fci_2017)$name, 
     vertex.size = 20, 
     edge.arrow.size = 0.15, 
     main = paste("Causal Graph 2017-2020 - Outcome:", "bp_control_jnc7"))

x <- as.matrix(dat_hyp_2017_fci[[1]][,colnames(dat_hyp_2017_fci[[1]]) != "bp_control_jnc7.Yes"])
y <- dat_hyp_2017_fci[[1]][,colnames(dat_hyp_2017_fci[[1]]) %in% "bp_control_jnc7.Yes"]

direct_causes_2017 <- pcSelect(y = y, dm = x, alpha = 0.05)
print(direct_causes_2017)
with(direct_causes_2017, zMin[G])

save(fci.fit.2017, adj_matrix_2017, graph_fci_2017, direct_causes_2017, file = "data/cleaned/2013_to_2023_cleaned/fci_2017.RData")

## requires packages: pcalg, igraph, RGBL

source("~/Documents/GitHub/lv-ida/lvida.R")

### sometimes the algorithms in pcalg can return a cyclic graph (for some alpha setting)
### LV-IDA (and IDA) won't work in such cases so this is a script to alert you
source("~/Documents/GitHub/lv-ida/iscyclic.R")
###

if(is.cyclic(fci.fit.2013@amat)){
  cat("#### FOUND CYCLIC GRAPH #### \n LV-IDA won't work here! \n try again! \n")
}

adj_matrix_2013_lvida <- fci.fit.2013@amat
colnames(adj_matrix_2013_lvida) <- 1:ncol(adj_matrix_2013_lvida)
rownames(adj_matrix_2013_lvida) <- 1:nrow(adj_matrix_2013_lvida)

lv.ida.est.2013 <- lv.ida(x.pos = 27,y.pos = 28,cov(dat_hyp_2013_fci[[1]]),fci.fit.2013,method="global")
lv.ida.est.2013 # this is the multiset of causal effects of x = 2 on y = pvar

isValidGraph(as(pc.fit.2013@graph, "matrix"), type = "cpdag")
pc_cpdag <- dag2cpdag(pc.fit.2013@graph)
# Regularize the covariance matrix
# Compute covariance matrix
cov_matrix <- cov(dat_hyp_2013_fci[[1]])
# Check determinant (should not be near zero)
det(cov_matrix)
lambda <- 1e-5  # Small regularization term
cov_matrix_reg <- cov_matrix + lambda * diag(ncol(cov_matrix))

# Run jointIda with the regularized covariance matrix
ida.est.2013 <- ida(x.pos = 1, y.pos = 16, cov_matrix_reg, pc_cpdag, method = "local", type = "cpdag")

ida.est.2013 <- jointIda(x.pos = 38, y.pos = 39, cov_matrix_reg, pc_cpdag, technique = "RRC", type = "cpdag")

ida.est.2013 <- idaFast(x.pos = 15,y.pos.set = c(1:14),cov(dat_hyp_2013_fci[[1]]),pc_cpdag)
