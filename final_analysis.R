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

# 4. ADD ATTRIBUTES FOR GERGM ------------------------------------------------

# Ideology attributes are already in the nodelist, just copy to networks
g_study2_pre <- g_pre_connected
g_study2_post <- g_post_connected

# The left_right attribute is already available from nodelist creation

# Add edge attributes (co-sponsorship and coalition)
add_edge_attribute <- function(g_voting, edgelist_attribute, attr_name) {
  edges_voting <- igraph::as_data_frame(g_voting, what = "edges")
  
  # Initialize all attribute values to 0
  edges_voting[[attr_name]] <- 0
  
  # If there are attributes to add, match them
  if(nrow(edgelist_attribute) > 0) {
    # Create canonical pairs for both
    voting_pairs <- paste(pmin(edges_voting$from, edges_voting$to),
                         pmax(edges_voting$from, edges_voting$to), sep = "_")
    attr_pairs <- paste(pmin(edgelist_attribute$from, edgelist_attribute$to),
                       pmax(edgelist_attribute$from, edgelist_attribute$to), sep = "_")
    
    # Match and assign
    matches <- match(voting_pairs, attr_pairs)
    edges_voting[[attr_name]][!is.na(matches)] <- edgelist_attribute$weight[matches[!is.na(matches)]]
  }
  
  # Recreate graph and copy vertex attributes
  g_with_attr <- igraph::graph_from_data_frame(edges_voting, directed = FALSE, vertices = igraph::V(g_voting)$name)
  for(attr in igraph::list.vertex.attributes(g_voting)) {
    if(attr != "name") igraph::vertex_attr(g_with_attr, attr) <- igraph::vertex_attr(g_voting, attr)
  }
  
  return(g_with_attr)
}

# Add co-sponsorship and coalition edge attributes
g_study2_pre <- add_edge_attribute(g_study2_pre, edgelist_cosponsor_pre, "cosponsor_count")
g_study2_pre <- add_edge_attribute(g_study2_pre, coalition_edgelist, "coalition_count")

g_study2_post <- add_edge_attribute(g_study2_post, edgelist_cosponsor_post, "cosponsor_count")
g_study2_post <- add_edge_attribute(g_study2_post, coalition_edgelist, "coalition_count")

print("  Ideology attributes added to vertices")
print("  Co-sponsorship and coalition attributes added to edges")
print("  Networks ready for GERGM analysis")
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

save(qap_result, qap_summary, observed_corr, p_value_two_tailed, interpretation, file = "results/statistics/qap_results.RData")
print("  QAP results saved to results/statistics/qap_results.RData")
print("")

# 6. GERGM ANALYSIS -----------------------------------------------------------

