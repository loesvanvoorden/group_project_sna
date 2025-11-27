# Electoral Cycle Network Analysis: Dutch Parliament 2023-2024

# Create output directories
if(!dir.exists("results/statistics")) dir.create("results/statistics", recursive = TRUE)
if(!dir.exists("results/visualizations")) dir.create("results/visualizations", recursive = TRUE)
if(!dir.exists("results/models")) dir.create("results/models", recursive = TRUE)

# 1. LOAD DATA ----------------------------------------------------------------

# Load ideology data
ideology_data <- read.csv("data/political_axes_data.csv", stringsAsFactors = FALSE)
names(ideology_data) <- c("left_right", "conservative_progressive", "party")
ideology_data <- ideology_data[!is.na(ideology_data$party) & ideology_data$party != "", ]

# Load pre-generated edge lists from Python script
edgelist_pre <- read.csv("results/edge_lists/edges_pre_election.csv", stringsAsFactors = FALSE)
edgelist_post <- read.csv("results/edge_lists/edges_post_formation.csv", stringsAsFactors = FALSE)
edgelist_cosponsor_pre <- read.csv("results/edge_lists/cosponsor_pre_election.csv", stringsAsFactors = FALSE)
edgelist_cosponsor_post <- read.csv("results/edge_lists/cosponsor_post_formation.csv", stringsAsFactors = FALSE)
coalition_edgelist <- read.csv("results/edge_lists/coalition_edges.csv", stringsAsFactors = FALSE)

print(sprintf("  Ideology data: %d parties", nrow(ideology_data)))
print(sprintf("  Pre-election edges: %d", nrow(edgelist_pre)))
print(sprintf("  Post-formation edges: %d", nrow(edgelist_post)))
print("")

# 2. CREATE NETWORKS ----------------------------------------------------------

# Create nodelist from ideology data
nodelist <- data.frame(
  name = sort(ideology_data$party),
  left_right = ideology_data$left_right[match(sort(ideology_data$party), ideology_data$party)],
  conservative_progressive = ideology_data$conservative_progressive[match(sort(ideology_data$party), ideology_data$party)],
  stringsAsFactors = FALSE
)

# Create igraph networks directly
g_pre <- igraph::graph_from_data_frame(d = edgelist_pre, vertices = nodelist, directed = FALSE)
g_post <- igraph::graph_from_data_frame(d = edgelist_post, vertices = nodelist, directed = FALSE)

print(sprintf("  Created networks for %d parties", nrow(nodelist)))

# Create fully connected adjacency matrices (required for QAP and GERGM)
adj_matrix_pre <- igraph::as_adjacency_matrix(g_pre, attr = "weight", sparse = FALSE)
adj_matrix_post <- igraph::as_adjacency_matrix(g_post, attr = "weight", sparse = FALSE)

# Set 0 values to 1e-6 to make fully connected (but keep diagonal as 0)
adj_matrix_pre[adj_matrix_pre == 0] <- 1e-6
adj_matrix_post[adj_matrix_post == 0] <- 1e-6
diag(adj_matrix_pre) <- 0
diag(adj_matrix_post) <- 0

# Create fully connected networks
g_pre_connected <- igraph::graph_from_adjacency_matrix(adj_matrix_pre, 
                                                       mode = "undirected", 
                                                       weighted = TRUE)
g_post_connected <- igraph::graph_from_adjacency_matrix(adj_matrix_post, 
                                                        mode = "undirected", 
                                                        weighted = TRUE)

# Vertex names are already set from nodelist

print(sprintf("  Pre-election: %d nodes, %d edges (fully connected)", 
              nrow(nodelist), snafun::count_edges(g_pre_connected)))
print(sprintf("  Post-formation: %d nodes, %d edges (fully connected)", 
              nrow(nodelist), snafun::count_edges(g_post_connected)))
print("")

# 3. VISUALIZATIONS -----------------------------------------------------------

# Ideology correlation plot
pearson_test <- stats::cor.test(ideology_data$left_right, ideology_data$conservative_progressive, method = "pearson")
print(sprintf("  Ideology correlation: r = %.3f (p = %.4f)", pearson_test$estimate, pearson_test$p.value))

pdf("results/visualizations/ideology_correlation.pdf", width = 8, height = 6)
plot(ideology_data$left_right, ideology_data$conservative_progressive,
     xlab = "Left-Right", ylab = "Conservative-Progressive",
     main = sprintf("Ideology Dimensions (r = %.3f)", pearson_test$estimate),
     pch = 19, col = "steelblue")
abline(lm(conservative_progressive ~ left_right, data = ideology_data), col = "red", lwd = 2)
text(ideology_data$left_right, ideology_data$conservative_progressive, 
     labels = ideology_data$party, pos = 3, cex = 0.7)
dev.off()

# Network visualization
g_pre_viz <- igraph::graph_from_data_frame(d = edgelist_pre, vertices = nodelist, directed = FALSE)
g_post_viz <- igraph::graph_from_data_frame(d = edgelist_post, vertices = nodelist, directed = FALSE)

# Add party type for coloring
party_type <- ifelse(
  nodelist$name %in% c("BIJ1", "PvdD", "GroenLinks", "PvdA", "GroenLinks-PvdA", "DENK", "SP", "ChristenUnie", "50PLUS"), "Left",
  ifelse(nodelist$name %in% c("Volt", "D66", "NSC", "BBB"), "Center", "Right")
)
igraph::V(g_pre_viz)$party_type <- party_type
igraph::V(g_post_viz)$party_type <- party_type

# Ideology position for layout
ideology_pos <- ifelse(
  nodelist$name %in% c("BIJ1", "PvdD", "GroenLinks", "PvdA", "GroenLinks-PvdA", "DENK", "SP"), 1,
  ifelse(nodelist$name %in% c("ChristenUnie", "50PLUS", "Volt", "D66", "NSC", "Omtzigt"), 2,
  ifelse(nodelist$name %in% c("BBB", "PVV", "CDA"), 3,
  ifelse(nodelist$name %in% c("VVD", "SGP"), 4, 5)))
)

# Create layout
set.seed(42)
layout_coords <- matrix(0, nrow = nrow(nodelist), ncol = 2)
for(i in seq_len(nrow(nodelist))) {
  layout_coords[i, 1] <- ideology_pos[i] + stats::runif(1, -0.3, 0.3)
  layout_coords[i, 2] <- stats::runif(1, -1, 1)
}

# Colors and visualization
party_colors <- c("Left" = "#E74C3C", "Center" = "#F39C12", "Right" = "#3498DB")
igraph::V(g_pre_viz)$color <- party_colors[igraph::V(g_pre_viz)$party_type]
igraph::V(g_post_viz)$color <- party_colors[igraph::V(g_post_viz)$party_type]
igraph::V(g_pre_viz)$size <- pmax(8, sqrt(igraph::degree(g_pre_viz)) * 4)
igraph::V(g_post_viz)$size <- pmax(8, sqrt(igraph::degree(g_post_viz)) * 4)

if(snafun::count_edges(g_pre_viz) > 0) {
  igraph::E(g_pre_viz)$width <- pmax(0.5, (igraph::E(g_pre_viz)$weight / max(igraph::E(g_pre_viz)$weight)) * 3)
  threshold_pre <- mean(igraph::E(g_pre_viz)$weight) * 1.3
  igraph::E(g_pre_viz)$color <- ifelse(igraph::E(g_pre_viz)$weight >= threshold_pre,
                                       grDevices::rgb(0.3, 0.3, 0.3, 0.8),
                                       grDevices::rgb(0.5, 0.5, 0.5, 0.15))
}

if(snafun::count_edges(g_post_viz) > 0) {
  igraph::E(g_post_viz)$width <- pmax(0.5, (igraph::E(g_post_viz)$weight / max(igraph::E(g_post_viz)$weight)) * 3)
  threshold_post <- mean(igraph::E(g_post_viz)$weight) * 1.3
  igraph::E(g_post_viz)$color <- ifelse(igraph::E(g_post_viz)$weight >= threshold_post,
                                        grDevices::rgb(0.3, 0.3, 0.3, 0.8),
                                        grDevices::rgb(0.5, 0.5, 0.5, 0.15))
}

# Plot networks
pdf("results/visualizations/network_comparison.pdf", width = 14, height = 7)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 2))
plot(g_pre_viz, layout = layout_coords, vertex.label.cex = 0.7, vertex.label.color = "black",
     vertex.frame.color = "white", main = "PRE-ELECTION\n(Nov 22, 2022 - Nov 21, 2023)")
plot(g_post_viz, layout = layout_coords, vertex.label.cex = 0.7, vertex.label.color = "black",
     vertex.frame.color = "white", main = "POST-FORMATION\n(Jul 5, 2024 - Jul 4, 2025)")
dev.off()

print("  Ideology correlation plot saved to results/visualizations/ideology_correlation.pdf")
print("  Network comparison plot saved to results/visualizations/network_comparison.pdf")
print("")

# 4. NETWORK BINARIZATION FOR ERGM -------------------------------------------

# Binarize networks using 70th percentile threshold (captures top 30% cooperation)
weights_pre <- igraph::E(g_pre_connected)$weight[igraph::E(g_pre_connected)$weight > 1e-6]
weights_post <- igraph::E(g_post_connected)$weight[igraph::E(g_post_connected)$weight > 1e-6]

threshold_pre <- quantile(weights_pre, 0.70)
threshold_post <- quantile(weights_post, 0.70)

# Create binary networks
adj_binary_pre <- igraph::as_adjacency_matrix(g_pre_connected, sparse = FALSE)
adj_binary_post <- igraph::as_adjacency_matrix(g_post_connected, sparse = FALSE)
adj_binary_pre[adj_binary_pre < threshold_pre] <- 0
adj_binary_post[adj_binary_post < threshold_post] <- 0
adj_binary_pre[adj_binary_pre >= threshold_pre] <- 1
adj_binary_post[adj_binary_post >= threshold_post] <- 1
diag(adj_binary_pre) <- 0
diag(adj_binary_post) <- 0

# Create binary igraph objects
g_study2_pre_binary <- igraph::graph_from_adjacency_matrix(adj_binary_pre, mode = "undirected")
g_study2_post_binary <- igraph::graph_from_adjacency_matrix(adj_binary_post, mode = "undirected")

# Add ideology attributes
igraph::V(g_study2_pre_binary)$left_right <- nodelist$left_right
igraph::V(g_study2_post_binary)$left_right <- nodelist$left_right

print(sprintf("  Binary networks: Pre(%d edges), Post(%d edges)", 
              snafun::count_edges(g_study2_pre_binary), snafun::count_edges(g_study2_post_binary)))
print("")

# 5. QAP ANALYSIS -------------------------------------------------------------

# QAP correlation test
set.seed(12345)
observed_corr <- sna::gcor(adj_matrix_pre, adj_matrix_post)

suppressWarnings({
  qap_result <- sna::qaptest(
    list(adj_matrix_pre, adj_matrix_post),
    FUN = sna::gcor,
    reps = 1000,
    g1 = 1, g2 = 2
  )
})

# Use built-in summary method from sna package
qap_summary <- summary(qap_result)
print("  QAP Test Summary:")
print(qap_summary)

# Calculate p-value manually since sna summary structure may vary
observed_corr <- qap_result$testval
p_value_one_tailed <- mean(qap_result$dist >= qap_result$testval)
p_value_two_tailed <- 2 * min(p_value_one_tailed, 1 - p_value_one_tailed)

print(sprintf("  Observed correlation: %.4f", observed_corr))
print(sprintf("  P-value (two-tailed): %.4f", p_value_two_tailed))

if(p_value_two_tailed < 0.05) {
  interpretation <- "Networks are significantly similar"
  print("  Interpretation: Networks are significantly similar (p < 0.05)")
} else {
  interpretation <- "Networks differ significantly"
  print("  Interpretation: Networks differ significantly (p >= 0.05)")
}

# Save QAP plot
pdf("results/visualizations/qap_results.pdf", width = 10, height = 6)
plot(qap_result)
dev.off()

save(qap_result, qap_summary, observed_corr, p_value_two_tailed, interpretation, file = "results/statistics/qap_results.RData")

# 6. ERGM ANALYSIS ------------------------------------------------------------

# Create edge covariate matrices
cosponsor_matrix_pre <- matrix(0, nrow = nrow(nodelist), ncol = nrow(nodelist))
cosponsor_matrix_post <- matrix(0, nrow = nrow(nodelist), ncol = nrow(nodelist))
coalition_matrix_pre <- matrix(0, nrow = nrow(nodelist), ncol = nrow(nodelist))
coalition_matrix_post <- matrix(0, nrow = nrow(nodelist), ncol = nrow(nodelist))

rownames(cosponsor_matrix_pre) <- colnames(cosponsor_matrix_pre) <- nodelist$name
rownames(cosponsor_matrix_post) <- colnames(cosponsor_matrix_post) <- nodelist$name
rownames(coalition_matrix_pre) <- colnames(coalition_matrix_pre) <- nodelist$name
rownames(coalition_matrix_post) <- colnames(coalition_matrix_post) <- nodelist$name

# Fill covariate matrices
for(i in seq_len(nrow(edgelist_cosponsor_pre))) {
  from <- edgelist_cosponsor_pre$from[i]
  to <- edgelist_cosponsor_pre$to[i]
  weight <- edgelist_cosponsor_pre$weight[i]
  cosponsor_matrix_pre[from, to] <- weight
  cosponsor_matrix_pre[to, from] <- weight
}

for(i in seq_len(nrow(edgelist_cosponsor_post))) {
  from <- edgelist_cosponsor_post$from[i]
  to <- edgelist_cosponsor_post$to[i]
  weight <- edgelist_cosponsor_post$weight[i]
  cosponsor_matrix_post[from, to] <- weight
  cosponsor_matrix_post[to, from] <- weight
}

for(i in seq_len(nrow(coalition_edgelist))) {
  from <- coalition_edgelist$from[i]
  to <- coalition_edgelist$to[i]
  weight <- coalition_edgelist$weight[i]
  coalition_matrix_pre[from, to] <- weight
  coalition_matrix_pre[to, from] <- weight
  coalition_matrix_post[from, to] <- weight
  coalition_matrix_post[to, from] <- weight
}

# Convert to network objects for ERGM (network library required for vertex attributes)

net_pre_binary <- network::as.network(adj_binary_pre, directed = FALSE)
net_post_binary <- network::as.network(adj_binary_post, directed = FALSE)
network::set.vertex.attribute(net_pre_binary, "left_right", nodelist$left_right)
network::set.vertex.attribute(net_post_binary, "left_right", nodelist$left_right)

# ERGM model specification (tests all 5 hypotheses)
formula_pre <- net_pre_binary ~ edges + absdiff("left_right") + gwesp(0.1, fixed = TRUE) + 
               kstar(3) + edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre)
formula_post <- net_post_binary ~ edges + absdiff("left_right") + gwesp(0.1, fixed = TRUE) + 
                kstar(3) + edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post)

# Estimate ERGMs
set.seed(1234)
ergm_pre <- ergm::ergm(formula_pre, control = ergm::control.ergm(
  MCMC.burnin = 20000, MCMC.samplesize = 25000, seed = 1234, 
  MCMC.interval = 1500, MCMC.prop = ~sparse + .triadic))

set.seed(1234)
ergm_post <- ergm::ergm(formula_post, control = ergm::control.ergm(
  MCMC.burnin = 20000, MCMC.samplesize = 25000, seed = 1234, 
  MCMC.interval = 1500, MCMC.prop = ~sparse + .triadic))

# Results
summary(ergm_pre)

summary(ergm_post)

# Save results
saveRDS(ergm_pre, "results/models/ergm_pre_election.rds")
saveRDS(ergm_post, "results/models/ergm_post_formation.rds")