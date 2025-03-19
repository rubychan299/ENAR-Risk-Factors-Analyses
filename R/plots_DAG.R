################### Visualization 
library(ggdag)
library(ggplot2)
library(dplyr)
library(dagitty)

# 2013####
# Extract adjacency matrix from TPC output
adj_matrix_2013 <- as(pc.fit.2013@graph, "matrix")

# Extract directed edges (X → Y where adj_matrix == 1)
edge_list_2013_1 <- which(adj_matrix_2013 == 1, arr.ind = TRUE)

# Extract bidirectional/uncertain edges (X ↔ Y where adj_matrix == 2)
edge_list_2013_2 <- which(adj_matrix_2013 == 2, arr.ind = TRUE)

# Convert indices to variable names
edges_2013_1 <- data.frame(
  from = colnames(adj_matrix_2013)[edge_list_2013_1[,1]],
  to   = colnames(adj_matrix_2013)[edge_list_2013_1[,2]],
  type = "directed"
)

edges_2013_2 <- data.frame(
  from = colnames(adj_matrix_2013)[edge_list_2013_2[,1]],
  to   = colnames(adj_matrix_2013)[edge_list_2013_2[,2]],
  type = "bidirectional"
)

# Combine both edge lists
edges_2013 <- rbind(edges_2013_1, edges_2013_2)

# Print extracted edges
print(edges_2013)

# Convert directed edges to dagify() formula format
dag_directed_2013 <- paste(edges_2013$to[edges_2013$type == "directed"], "~", edges_2013$from[edges_2013$type == "directed"], collapse = ", ")

# Convert bidirectional edges (X ↔ Y) by adding both directions
dag_bidirectional_2013 <- paste(edges_2013$from[edges_2013$type == "bidirectional"], "~~", edges_2013$to[edges_2013$type == "bidirectional"], ",",
                           edges_2013$to[edges_2013$type == "bidirectional"], "~~", edges_2013$from[edges_2013$type == "bidirectional"], collapse = ", ")

# Combine both formulas if there are bidirectional edges
dag_formula_2013 <- ifelse(nchar(dag_bidirectional_2013) > 0,
                      paste(dag_directed_2013, ",", dag_bidirectional_2013),
                      dag_directed)

# Create dagify() structure
dag_model_2013 <- eval(parse(text = paste0("dagify(", dag_formula_2013, ")")))

# Print DAG model
print(dag_model_2013)

# Plot the DAG
# ggdag(dag_model_2013, text = TRUE) + theme_dag()


name_map <- c(
  "bp_control_jnc7" = "BP control",
  "cc_diabetes" = "Diabetes",
  "cc_cvd_any" = "Cardiovascular \nDisease",
  "cc_cvd_stroke" = "Stroke",
  "cc_cvd_chd" = "Coronary Heart \nDisease",
  "cc_cvd_hf" = "Heart Failure",
  "cc_bmi_resid" = "Body Mass Index",
  "cc_ckd" = "Chronic Kidney \nDisease",
  "cc_smoke_resid" = "Smoking Status",
  "HIQ011" = "Covered by \nHealth Insurance",
  "OHQ845" = "Oral Health",
  "LBXRBCSI_resid" = "Red Blood Cell \nCount",
  "LBDMONO_resid" = "Monocyte Count",
  "LBXSTR_resid" = "Triglycerides",
  "LBXSBU_resid" = "Blood Urea \nNitrogen",
  "URXUMS_resid" = "Urinary \nAlbumin",
  "URXUCR_resid" = "Urinary Creatinine",
  "LBXSUA_resid" = "Uric Acid",
  "LBXTC_resid" = "Total Cholesterol",
  "LBXTHG_resid" = "Blood Mercury",
  "LBXBPB_resid" = "Blood Lead",
  "PFQ054" = "Difficulty \nWalking",
  "PFQ049" = "Difficulty \nWorking",
  "phq9_category_resid" = "Depression",
  "WHQ030_resid" = "Weight Perception",
  "FSDAD_resid" = "Adult Food Security",
  "weight_change_resid" = "Weight Change",
  "KIQ022" = "Chronic Kidney \nDisease"
)

# Convert DAG to tidy format and apply renaming
dag_tidy_2013 <- tidy_dagitty(dag_model_2013) %>%
  mutate(
    name = recode(name, !!!name_map),  # Rename nodes
    to = recode(to, !!!name_map)  # Rename edges
  )

# Ensure colors are assigned correctly to renamed nodes
node_colors <- tibble(
  name = dag_tidy_2013$data$name,  
  color = ifelse(dag_tidy_2013$data$name == "BP control", "lightgoldenrod1", "lightblue")  
)

ggdag_plot_2013 <- dag_tidy_2013 %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_void() +
  geom_dag_point(aes(color = name), size = 30, stroke = 0.2) +  # Bigger nodes, thinner border
  geom_dag_edges(
    edge_color = "darkgray",  # Black edges
    edge_width = 0.5,  # Make edges thinner
    start_cap = ggraph::circle(11, "mm"),  # Moves edge start outward
    end_cap = ggraph::circle(11, "mm"), 
    arrow_directed = grid::arrow(length = grid::unit(5, "pt"), ends = "last", type = "closed"),
    arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"),ends = "last", type = "closed")
  ) +  
  scale_color_manual(values = setNames(node_colors$color, node_colors$name)) +  
  geom_dag_text(color = "black", size = 3, fontface = "bold") +  # Larger text with "repel"
  ggtitle("PC Diagram 2013-2014") +
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black"),  # White plot area with black border
    plot.background = element_rect(fill = "white", color = NA)  # White full background
  ) 

# Save plot
ggsave("plots/PC_graph_2013.png", ggdag_plot_2013, width = 13, height = 13.72, dpi = 300)

# 2015####

# Extract adjacency matrix from TPC output
adj_matrix_2015 <- as(pc.fit.2015@graph, "matrix")

# Extract directed edges (X → Y where adj_matrix == 1)
edge_list_2015_1 <- which(adj_matrix_2015 == 1, arr.ind = TRUE)

# Extract bidirectional/uncertain edges (X ↔ Y where adj_matrix == 2)
edge_list_2015_2 <- which(adj_matrix_2015 == 2, arr.ind = TRUE)

# Convert indices to variable names
edges_2015_1 <- data.frame(
  from = colnames(adj_matrix_2015)[edge_list_2015_1[,1]],
  to   = colnames(adj_matrix_2015)[edge_list_2015_1[,2]],
  type = "directed"
)

edges_2015_2 <- data.frame(
  from = colnames(adj_matrix_2015)[edge_list_2015_2[,1]],
  to   = colnames(adj_matrix_2015)[edge_list_2015_2[,2]],
  type = "bidirectional"
)

# Combine both edge lists
edges_2015 <- rbind(edges_2015_1, edges_2015_2)

# Print extracted edges
print(edges_2015)

# Convert directed edges to dagify() formula format
dag_directed_2015 <- paste(edges_2015$to[edges_2015$type == "directed"], "~", edges_2015$from[edges_2015$type == "directed"], collapse = ", ")

# Convert bidirectional edges (X ↔ Y) by adding both directions
dag_bidirectional_2015 <- paste(edges_2015$from[edges_2015$type == "bidirectional"], "~~", edges_2015$to[edges_2015$type == "bidirectional"], ",",
                                edges_2015$to[edges_2015$type == "bidirectional"], "~~", edges_2015$from[edges_2015$type == "bidirectional"], collapse = ", ")

# Combine both formulas if there are bidirectional edges
dag_formula_2015 <- ifelse(nchar(dag_bidirectional_2015) > 0,
                           paste(dag_directed_2015, ",", dag_bidirectional_2015),
                           dag_directed)

# Create dagify() structure
dag_model_2015 <- eval(parse(text = paste0("dagify(", dag_formula_2015, ")")))

# Print DAG model
print(dag_model_2015)

# Plot the DAG
# ggdag(dag_model_2013, text = TRUE) + theme_dag()

# Convert DAG to tidy format and apply renaming
dag_tidy_2015 <- tidy_dagitty(dag_model_2015) %>%
  mutate(
    name = recode(name, !!!name_map),  # Rename nodes
    to = recode(to, !!!name_map)  # Rename edges
  )

# Ensure colors are assigned correctly to renamed nodes
node_colors <- tibble(
  name = dag_tidy_2015$data$name,  
  color = ifelse(dag_tidy_2015$data$name == "BP control", "lightgoldenrod1", "lightblue")  
)

ggdag_plot_2015 <- dag_tidy_2015 %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_void() +
  geom_dag_point(aes(color = name), size = 30, stroke = 0.2) +  # Bigger nodes, thinner border
  geom_dag_edges(
    edge_color = "darkgray",  # Black edges
    edge_width = 0.5,  # Make edges thinner
    start_cap = ggraph::circle(11, "mm"),  # Moves edge start outward
    end_cap = ggraph::circle(11, "mm"), 
    arrow_directed = grid::arrow(length = grid::unit(5, "pt"), ends = "last", type = "closed"),
    arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"),ends = "last", type = "closed")
  ) +  
  scale_color_manual(values = setNames(node_colors$color, node_colors$name)) +  
  geom_dag_text(color = "black", size = 3, fontface = "bold") +  # Larger text with "repel"
  ggtitle("PC Diagram 2015-2017") +
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black"),  # White plot area with black border
    plot.background = element_rect(fill = "white", color = NA)  # White full background
  ) 

# Save plot
ggsave("plots/PC_graph_2015.png", ggdag_plot_2015, width = 13, height = 13.72, dpi = 300)

# 2017####

# Extract adjacency matrix from TPC output
adj_matrix_2017 <- as(pc.fit.2017@graph, "matrix")

# Extract directed edges (X → Y where adj_matrix == 1)
edge_list_2017_1 <- which(adj_matrix_2017 == 1, arr.ind = TRUE)

# Extract bidirectional/uncertain edges (X ↔ Y where adj_matrix == 2)
edge_list_2017_2 <- which(adj_matrix_2017 == 2, arr.ind = TRUE)

# Convert indices to variable names
edges_2017_1 <- data.frame(
  from = colnames(adj_matrix_2017)[edge_list_2017_1[,1]],
  to   = colnames(adj_matrix_2017)[edge_list_2017_1[,2]],
  type = "directed"
)

edges_2017_2 <- data.frame(
  from = colnames(adj_matrix_2017)[edge_list_2017_2[,1]],
  to   = colnames(adj_matrix_2017)[edge_list_2017_2[,2]],
  type = "bidirectional"
)

# Combine both edge lists
edges_2017 <- rbind(edges_2017_1, edges_2017_2)

# Print extracted edges
print(edges_2017)

# Convert directed edges to dagify() formula format
dag_directed_2017 <- paste(edges_2017$to[edges_2017$type == "directed"], "~", edges_2017$from[edges_2017$type == "directed"], collapse = ", ")

# Convert bidirectional edges (X ↔ Y) by adding both directions
dag_bidirectional_2017 <- paste(edges_2017$from[edges_2017$type == "bidirectional"], "~~", edges_2017$to[edges_2017$type == "bidirectional"], ",",
                                edges_2017$to[edges_2017$type == "bidirectional"], "~~", edges_2017$from[edges_2017$type == "bidirectional"], collapse = ", ")

# Combine both formulas if there are bidirectional edges
dag_formula_2017 <- ifelse(nchar(dag_bidirectional_2017) > 0,
                           paste(dag_directed_2017, ",", dag_bidirectional_2017),
                           dag_directed)

# Create dagify() structure
dag_model_2017 <- eval(parse(text = paste0("dagify(", dag_formula_2017, ")")))

# Print DAG model
print(dag_model_2017)

# Plot the DAG
# ggdag(dag_model_2013, text = TRUE) + theme_dag()

# Convert DAG to tidy format and apply renaming
dag_tidy_2017 <- tidy_dagitty(dag_model_2017) %>%
  mutate(
    name = recode(name, !!!name_map),  # Rename nodes
    to = recode(to, !!!name_map)  # Rename edges
  )

# Ensure colors are assigned correctly to renamed nodes
node_colors <- tibble(
  name = dag_tidy_2017$data$name,  
  color = ifelse(dag_tidy_2017$data$name == "BP control", "lightgoldenrod1", "lightblue")  
)

ggdag_plot_2017 <- dag_tidy_2017 %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_void() +
  geom_dag_point(aes(color = name), size = 30, stroke = 0.2) +  # Bigger nodes, thinner border
  geom_dag_edges(
    edge_color = "darkgray",  # Black edges
    edge_width = 0.5,  # Make edges thinner
    start_cap = ggraph::circle(11, "mm"),  # Moves edge start outward
    end_cap = ggraph::circle(11, "mm"), 
    arrow_directed = grid::arrow(length = grid::unit(5, "pt"), ends = "last", type = "closed"),
    arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"),ends = "last", type = "closed")
  ) +  
  scale_color_manual(values = setNames(node_colors$color, node_colors$name)) +  
  geom_dag_text(color = "black", size = 3, fontface = "bold") +  # Larger text with "repel"
  ggtitle("PC Diagram 2017-2020") +
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black"),  # White plot area with black border
    plot.background = element_rect(fill = "white", color = NA)  # White full background
  ) 

# Save plot
ggsave("plots/PC_graph_2017.png", ggdag_plot_2017, width = 13, height = 13.72, dpi = 300)

# 2021####

# Extract adjacency matrix from TPC output
adj_matrix_2021 <- as(pc.fit.2021@graph, "matrix")

# Extract directed edges (X → Y where adj_matrix == 1)
edge_list_2021_1 <- which(adj_matrix_2021 == 1, arr.ind = TRUE)

# Extract bidirectional/uncertain edges (X ↔ Y where adj_matrix == 2)
edge_list_2021_2 <- which(adj_matrix_2021 == 2, arr.ind = TRUE)

# Convert indices to variable names
edges_2021_1 <- data.frame(
  from = colnames(adj_matrix_2021)[edge_list_2021_1[,1]],
  to   = colnames(adj_matrix_2021)[edge_list_2021_1[,2]],
  type = "directed"
)

edges_2021_2 <- data.frame(
  from = colnames(adj_matrix_2021)[edge_list_2021_2[,1]],
  to   = colnames(adj_matrix_2021)[edge_list_2021_2[,2]],
  type = "bidirectional"
)

# Combine both edge lists
edges_2021 <- rbind(edges_2021_1, edges_2021_2)

# Print extracted edges
print(edges_2021)

# Convert directed edges to dagify() formula format
dag_directed_2021 <- paste(edges_2021$to[edges_2021$type == "directed"], "~", edges_2021$from[edges_2021$type == "directed"], collapse = ", ")

# Convert bidirectional edges (X ↔ Y) by adding both directions
dag_bidirectional_2021 <- paste(edges_2021$from[edges_2021$type == "bidirectional"], "~~", edges_2021$to[edges_2021$type == "bidirectional"], ",",
                                edges_2021$to[edges_2021$type == "bidirectional"], "~~", edges_2021$from[edges_2021$type == "bidirectional"], collapse = ", ")

# Combine both formulas if there are bidirectional edges
dag_formula_2021 <- ifelse(nchar(dag_bidirectional_2021) > 0,
                           paste(dag_directed_2021, ",", dag_bidirectional_2021),
                           dag_directed)

# Create dagify() structure
dag_model_2021 <- eval(parse(text = paste0("dagify(", dag_formula_2021, ")")))

# Print DAG model
print(dag_model_2021)

# Plot the DAG
# ggdag(dag_model_2013, text = TRUE) + theme_dag()

# Convert DAG to tidy format and apply renaming
dag_tidy_2021 <- tidy_dagitty(dag_model_2021) %>%
  mutate(
    name = recode(name, !!!name_map),  # Rename nodes
    to = recode(to, !!!name_map)  # Rename edges
  )

# Ensure colors are assigned correctly to renamed nodes
node_colors <- tibble(
  name = dag_tidy_2021$data$name,  
  color = ifelse(dag_tidy_2021$data$name == "BP control", "lightgoldenrod1", "lightblue")  
)

ggdag_plot_2021 <- dag_tidy_2021 %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_void() +
  geom_dag_point(aes(color = name), size = 30, stroke = 0.2) +  # Bigger nodes, thinner border
  geom_dag_edges(
    edge_color = "darkgray",  # Black edges
    edge_width = 0.5,  # Make edges thinner
    start_cap = ggraph::circle(11, "mm"),  # Moves edge start outward
    end_cap = ggraph::circle(11, "mm"), 
    arrow_directed = grid::arrow(length = grid::unit(5, "pt"), ends = "last", type = "closed"),
    arrow_bidirected = grid::arrow(length = grid::unit(5, "pt"),ends = "last", type = "closed")
  ) +  
  scale_color_manual(values = setNames(node_colors$color, node_colors$name)) +  
  geom_dag_text(color = "black", size = 3, fontface = "bold") +  # Larger text with "repel"
  ggtitle("PC Diagram 2021-2023") +
  theme(
    legend.position = "none",  
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black"),  # White plot area with black border
    plot.background = element_rect(fill = "white", color = NA)  # White full background
  ) 

# Save plot
ggsave("plots/PC_graph_2021.png", ggdag_plot_2021, width = 13, height = 13.72, dpi = 300)

save(ggdag_plot_2013, ggdag_plot_2015, ggdag_plot_2017, ggdag_plot_2021, file = "plots/PC_graphs.RData")
