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

# Load pre-filtered edge lists from Python script (no R filtering needed!)

# Study 1 edge lists (identical node structure for QAP)
study1_edgelist_pre <- read.csv("results/edge_lists/study1_edges_pre_election.csv", stringsAsFactors = FALSE)
study1_edgelist_post <- read.csv("results/edge_lists/study1_edges_post_formation.csv", stringsAsFactors = FALSE)
study1_cosponsor_pre <- read.csv("results/edge_lists/study1_cosponsor_pre_election.csv", stringsAsFactors = FALSE)
study1_cosponsor_post <- read.csv("results/edge_lists/study1_cosponsor_post_formation.csv", stringsAsFactors = FALSE)
study1_coalition <- read.csv("results/edge_lists/study1_coalition_edges.csv", stringsAsFactors = FALSE)

# Study 2 edge lists (optimized structure for ERGM/visualization)
study2_edgelist_pre <- read.csv("results/edge_lists/study2_edges_pre_election.csv", stringsAsFactors = FALSE)
study2_edgelist_post <- read.csv("results/edge_lists/study2_edges_post_formation.csv", stringsAsFactors = FALSE)
study2_cosponsor_pre <- read.csv("results/edge_lists/study2_cosponsor_pre_election.csv", stringsAsFactors = FALSE)
study2_cosponsor_post <- read.csv("results/edge_lists/study2_cosponsor_post_formation.csv", stringsAsFactors = FALSE)
study2_coalition_pre <- read.csv("results/edge_lists/study2_coalition_pre_election.csv", stringsAsFactors = FALSE)
study2_coalition_post <- read.csv("results/edge_lists/study2_coalition_post_formation.csv", stringsAsFactors = FALSE)


# 2. CREATE NETWORKS ----------------------------------------------------------

# Create nodelists from edge lists (already filtered in Python)
study1_parties <- sort(unique(c(study1_edgelist_pre$from, study1_edgelist_pre$to, 
                               study1_edgelist_post$from, study1_edgelist_post$to)))
study2_parties_pre <- sort(unique(c(study2_edgelist_pre$from, study2_edgelist_pre$to)))
study2_parties_post <- sort(unique(c(study2_edgelist_post$from, study2_edgelist_post$to)))

# Create nodelists with ideology attributes
create_nodelist <- function(party_names) {
  data.frame(
    name = party_names,
    left_right = ideology_data$left_right[match(party_names, ideology_data$party)],
    conservative_progressive = ideology_data$conservative_progressive[match(party_names, ideology_data$party)],
    stringsAsFactors = FALSE
  )
}

nodelist_study1 <- create_nodelist(study1_parties)
nodelist_pre_study2 <- create_nodelist(study2_parties_pre)
nodelist_post_study2 <- create_nodelist(study2_parties_post)


# Create networks directly from pre-filtered edge lists
g_pre_study1 <- igraph::graph_from_data_frame(d = study1_edgelist_pre, vertices = nodelist_study1, directed = FALSE)
g_post_study1 <- igraph::graph_from_data_frame(d = study1_edgelist_post, vertices = nodelist_study1, directed = FALSE)

g_pre_study2 <- igraph::graph_from_data_frame(d = study2_edgelist_pre, vertices = nodelist_pre_study2, directed = FALSE)
g_post_study2 <- igraph::graph_from_data_frame(d = study2_edgelist_post, vertices = nodelist_post_study2, directed = FALSE)

# Remove self-loops from all networks
g_pre_study1 <- igraph::simplify(g_pre_study1, remove.multiple = FALSE, remove.loops = TRUE)
g_post_study1 <- igraph::simplify(g_post_study1, remove.multiple = FALSE, remove.loops = TRUE)
g_pre_study2 <- igraph::simplify(g_pre_study2, remove.multiple = FALSE, remove.loops = TRUE)
g_post_study2 <- igraph::simplify(g_post_study2, remove.multiple = FALSE, remove.loops = TRUE)

# Create fully connected adjacency matrices for Study 1 (QAP requires identical node structure)
adj_matrix_pre <- igraph::as_adjacency_matrix(g_pre_study1, attr = "weight", sparse = FALSE)
adj_matrix_post <- igraph::as_adjacency_matrix(g_post_study1, attr = "weight", sparse = FALSE)

# Set 0 values to 1e-6 to make fully connected (but keep diagonal as 0)
adj_matrix_pre[adj_matrix_pre == 0] <- 1e-6
adj_matrix_post[adj_matrix_post == 0] <- 1e-6
diag(adj_matrix_pre) <- 0
diag(adj_matrix_post) <- 0

# Create fully connected networks for Study 1 (QAP)
g_pre_connected <- igraph::graph_from_adjacency_matrix(adj_matrix_pre, 
                                                       mode = "undirected", 
                                                       weighted = TRUE)
g_post_connected <- igraph::graph_from_adjacency_matrix(adj_matrix_post, 
                                                        mode = "undirected", 
                                                        weighted = TRUE)


# 3. AGREEMENT RATE DISTRIBUTION ANALYSIS (for threshold determination) ------

# Extract edge weights from Study 2 networks (should now be agreement rates 0-1)
original_weights_pre <- snafun::extract_edge_attribute(g_pre_study2, "weight")
original_weights_post <- snafun::extract_edge_attribute(g_post_study2, "weight")

# Convert to agreement rates as percentages (multiply fractions by 100)
agreement_rates_pre <- original_weights_pre * 100
agreement_rates_post <- original_weights_post * 100

# Calculate basic statistics
stats_pre <- list(
  mean = mean(agreement_rates_pre),
  median = median(agreement_rates_pre),
  q1 = quantile(agreement_rates_pre, 0.25),
  q3 = quantile(agreement_rates_pre, 0.75)
)

stats_post <- list(
  mean = mean(agreement_rates_post),
  median = median(agreement_rates_post),
  q1 = quantile(agreement_rates_post, 0.25),
  q3 = quantile(agreement_rates_post, 0.75)
)

# Display statistics for threshold decision
print("Agreement rate statistics:")
print(sprintf("PRE-ELECTION:  Mean=%.1f%%, Median=%.1f%%, Q1=%.1f%%, Q3=%.1f%%", 
              stats_pre$mean, stats_pre$median, stats_pre$q1, stats_pre$q3))
print(sprintf("POST-FORMATION: Mean=%.1f%%, Median=%.1f%%, Q1=%.1f%%, Q3=%.1f%%", 
              stats_post$mean, stats_post$median, stats_post$q1, stats_post$q3))

# Create simple agreement rate distribution plots
pdf("results/visualizations/agreement_distributions_simple.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))

hist(agreement_rates_pre, breaks = 10, main = "Pre-Election Agreement Rates", 
     xlab = "Agreement Rate (%)", ylab = "Frequency", col = "skyblue", border = "black")
abline(v = stats_pre$median, col = "red", lwd = 4)

hist(agreement_rates_post, breaks = 10, main = "Post-Formation Agreement Rates",
     xlab = "Agreement Rate (%)", ylab = "Frequency", col = "lightcoral", border = "black")
abline(v = stats_post$median, col = "red", lwd = 4)

dev.off()

# 4. VISUALIZATIONS -----------------------------------------------------------

# Ideology correlation plot
pearson_test <- stats::cor.test(ideology_data$left_right, ideology_data$conservative_progressive, method = "pearson")

pdf("results/visualizations/ideology_correlation.pdf", width = 8, height = 6)
plot(ideology_data$left_right, ideology_data$conservative_progressive,
     xlab = "Left-Right", ylab = "Conservative-Progressive",
     main = sprintf("Ideology Dimensions (r = %.3f)", pearson_test$estimate),
     pch = 19, col = "steelblue")
abline(lm(conservative_progressive ~ left_right, data = ideology_data), col = "red", lwd = 2)
text(ideology_data$left_right, ideology_data$conservative_progressive, 
     labels = ideology_data$party, pos = 3, cex = 0.7)
dev.off()

# Network visualization (use Study 2 networks for better visual clarity)
g_pre_viz <- g_pre_study2
g_post_viz <- g_post_study2

# Add party type for coloring based on left_right values from political_axes_data.csv
party_type_pre <- ifelse(
  nodelist_pre_study2$left_right <= -0.3, "Left",    # Left: left_right <= -0.3
  ifelse(nodelist_pre_study2$left_right >= 0.2, "Right", "Center")  # Right: left_right >= 0.2, Center: in between
)
party_type_post <- ifelse(
  nodelist_post_study2$left_right <= -0.3, "Left",    # Left: left_right <= -0.3
  ifelse(nodelist_post_study2$left_right >= 0.2, "Right", "Center")  # Right: left_right >= 0.2, Center: in between
)

igraph::V(g_pre_viz)$party_type <- party_type_pre
igraph::V(g_post_viz)$party_type <- party_type_post

# Use default igraph layout for network positioning
set.seed(42)

# Colors and visualization
party_colors <- c("Left" = "#E74C3C", "Center" = "#F39C12", "Right" = "#3498DB")
igraph::V(g_pre_viz)$color <- party_colors[igraph::V(g_pre_viz)$party_type]
igraph::V(g_post_viz)$color <- party_colors[igraph::V(g_post_viz)$party_type]
igraph::V(g_pre_viz)$size <- pmax(8, sqrt(igraph::degree(g_pre_viz)) * 4)
igraph::V(g_post_viz)$size <- pmax(8, sqrt(igraph::degree(g_post_viz)) * 4)

# Style pre-election network edges
edge_weights_pre <- snafun::extract_edge_attribute(g_pre_viz, "weight")
igraph::E(g_pre_viz)$width <- pmax(0.5, (edge_weights_pre / max(edge_weights_pre)) * 3)
threshold_pre <- stats_pre$q3 / 100  # Convert Q3 from percentage back to fraction
igraph::E(g_pre_viz)$color <- ifelse(edge_weights_pre >= threshold_pre,
                                     grDevices::rgb(0.3, 0.3, 0.3, 0.8),
                                     grDevices::rgb(0.5, 0.5, 0.5, 0.15))

# Style post-formation network edges
edge_weights_post <- snafun::extract_edge_attribute(g_post_viz, "weight")
igraph::E(g_post_viz)$width <- pmax(0.5, (edge_weights_post / max(edge_weights_post)) * 3)
threshold_post <- stats_post$q3 / 100  # Convert Q3 from percentage back to fraction
igraph::E(g_post_viz)$color <- ifelse(edge_weights_post >= threshold_post,
                                      grDevices::rgb(0.3, 0.3, 0.3, 0.8),
                                      grDevices::rgb(0.5, 0.5, 0.5, 0.15))

# Plot networks (Study 2 networks for visualization)
pdf("results/visualizations/network_comparison.pdf", width = 14, height = 7)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 2))
plot(g_pre_viz, vertex.label.cex = 0.7, vertex.label.color = "black",
     vertex.frame.color = "white", main = "PRE-ELECTION (Study 2 Networks)\n(Nov 22, 2022 - Nov 21, 2023)")
plot(g_post_viz, vertex.label.cex = 0.7, vertex.label.color = "black",
     vertex.frame.color = "white", main = "POST-FORMATION (Study 2 Networks)\n(Jul 5, 2024 - Jul 4, 2025)")
dev.off()


# 5. ADD ATTRIBUTES FOR STUDY 2 NETWORKS -------------------------------------

# Study 2: Create binarized networks with Q3 and Mean thresholds for ERGMs
threshold_pre_q3 <- stats_pre$q3 / 100
threshold_pre_mean <- stats_pre$mean / 100
threshold_post_q3 <- stats_post$q3 / 100
threshold_post_mean <- stats_post$mean / 100


# Create 4 binarized networks for ERGMs (reusing edge weights from earlier)
# Pre-election Q3 threshold
g_pre_q3 <- igraph::subgraph_from_edges(g_pre_study2, 
                                        igraph::E(g_pre_study2)[original_weights_pre >= threshold_pre_q3], 
                                        delete.vertices = FALSE)
igraph::E(g_pre_q3)$weight <- 1  # Binarize

# Pre-election Mean threshold  
g_pre_mean <- igraph::subgraph_from_edges(g_pre_study2,
                                          igraph::E(g_pre_study2)[original_weights_pre >= threshold_pre_mean],
                                          delete.vertices = FALSE)
igraph::E(g_pre_mean)$weight <- 1  # Binarize

# Post-formation Q3 threshold
g_post_q3 <- igraph::subgraph_from_edges(g_post_study2,
                                         igraph::E(g_post_study2)[original_weights_post >= threshold_post_q3],
                                         delete.vertices = FALSE)
igraph::E(g_post_q3)$weight <- 1  # Binarize

# Post-formation Mean threshold
g_post_mean <- igraph::subgraph_from_edges(g_post_study2,
                                           igraph::E(g_post_study2)[original_weights_post >= threshold_post_mean],
                                           delete.vertices = FALSE)
igraph::E(g_post_mean)$weight <- 1  # Binarize

# Add edge attributes (co-sponsorship and coalition)
# Create covariate matrices directly (cleaner approach like final_ergm.R)
create_covariate_matrix <- function(edgelist, nodelist) {
  # Initialize empty matrix
  n <- length(nodelist)
  matrix <- matrix(0, nrow = n, ncol = n)
  rownames(matrix) <- nodelist
  colnames(matrix) <- nodelist
  
  # Fill matrix from edge list
  if(nrow(edgelist) > 0) {
    for(i in seq_len(nrow(edgelist))) {
      from_idx <- which(nodelist == edgelist$from[i])
      to_idx <- which(nodelist == edgelist$to[i])
      if(length(from_idx) > 0 && length(to_idx) > 0) {
        matrix[from_idx, to_idx] <- edgelist$weight[i]
        matrix[to_idx, from_idx] <- edgelist$weight[i]  # Symmetric for undirected
      }
    }
  }
  
  return(matrix)
}

# Create covariate matrices for all networks
cosponsor_matrix_pre <- create_covariate_matrix(study2_cosponsor_pre, nodelist_pre_study2$name)
coalition_matrix_pre <- create_covariate_matrix(study2_coalition_pre, nodelist_pre_study2$name)
cosponsor_matrix_post <- create_covariate_matrix(study2_cosponsor_post, nodelist_post_study2$name)
coalition_matrix_post <- create_covariate_matrix(study2_coalition_post, nodelist_post_study2$name)

# 6. QAP ANALYSIS -------------------------------------------------------------

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
qap_summary

# Create QAP plot showing distribution and observed value
pdf("results/visualizations/qap_results.pdf", width = 8, height = 6)
plot(qap_result)
dev.off()

save(qap_result, qap_summary, file = "results/statistics/qap_results.RData")

# 7. ERGM ANALYSIS (after threshold selection) ----------------------------

# Convert binarized networks to network objects for ERGM
net_pre_q3 <- network::as.network(igraph::as_adjacency_matrix(g_pre_q3, sparse = FALSE), directed = FALSE)
net_pre_mean <- network::as.network(igraph::as_adjacency_matrix(g_pre_mean, sparse = FALSE), directed = FALSE)
net_post_q3 <- network::as.network(igraph::as_adjacency_matrix(g_post_q3, sparse = FALSE), directed = FALSE)
net_post_mean <- network::as.network(igraph::as_adjacency_matrix(g_post_mean, sparse = FALSE), directed = FALSE)

# Add vertex attributes
network::set.vertex.attribute(net_pre_q3, "left_right", nodelist_pre_study2$left_right)
network::set.vertex.attribute(net_pre_mean, "left_right", nodelist_pre_study2$left_right)
network::set.vertex.attribute(net_post_q3, "left_right", nodelist_post_study2$left_right)
network::set.vertex.attribute(net_post_mean, "left_right", nodelist_post_study2$left_right)

# ERGM control parameters
ergm_control <- ergm::control.ergm(
  MCMC.burnin = 7000,
  MCMC.samplesize = 20000,
  MCMC.interval = 1500,
  seed = 1234,
  MCMLE.maxit = 40,
  parallel = 5,
  parallel.type = "PSOCK",
  MCMC.prop = ~sparse + .triadic
)

print("Running 4 Final ERGMs:")
set.seed(1234)

# Final Model 1: Pre-election Q3 threshold
FinalModel_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ 
    edges + absdiff("left_right") +
    kstar(3) + edgecov(cosponsor_matrix_pre) + 
    gwesp(0.5, fixed = TRUE),
  control = ergm_control
)

# Final Model 2: Pre-election Mean threshold
FinalModel_pre_mean <- ergm::ergm(
  net_pre_mean ~ 
    edges + absdiff("left_right") +
    kstar(3) + edgecov(cosponsor_matrix_pre) + 
    gwesp(0.5, fixed = TRUE),
  control = ergm_control
)

# Final Model 3: Post-formation Q3 threshold  
FinalModel_post_q3 <- ergm::ergm(
  net_post_q3 ~ 
    edges + absdiff("left_right") +
    kstar(3) + edgecov(cosponsor_matrix_post) + 
    gwesp(0.5, fixed = TRUE),
  control = ergm_control
)

# Final Model 4: Post-formation Mean threshold
FinalModel_post_mean <- ergm::ergm(
  net_post_mean ~ 
    edges + absdiff("left_right") +
    kstar(3) + edgecov(cosponsor_matrix_post) + 
    gwesp(0.5, fixed = TRUE),
  control = ergm_control
)

# Display all 4 models in comparison table
texreg::screenreg(list(FinalModel_pre_q3, FinalModel_pre_mean, FinalModel_post_q3, FinalModel_post_mean),
                  custom.model.names = c("Pre Q3", "Pre Mean", "Post Q3", "Post Mean"))

# Save all models
saveRDS(FinalModel_pre_q3, file = "results/models/final_ergm_pre_q3.rds")
saveRDS(FinalModel_pre_mean, file = "results/models/final_ergm_pre_mean.rds")  
saveRDS(FinalModel_post_q3, file = "results/models/final_ergm_post_q3.rds")
saveRDS(FinalModel_post_mean, file = "results/models/final_ergm_post_mean.rds")

# Save ERGM diagnostics (simplified)
save_ergm_diagnostics <- function(ergm_model, output_prefix) {
  output_dir <- "results/ergm_diagnostics"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # MCMC diagnostics
  pdf(file.path(output_dir, paste0(output_prefix, "mcmc.pdf")))
  ergm::mcmc.diagnostics(ergm_model)
  dev.off()
  
  # Goodness of fit
  pdf(file.path(output_dir, paste0(output_prefix, "gof.pdf")))
  snafun::stat_plot_gof(ergm::gof(ergm_model))
  dev.off()
  
  # Model summary
  writeLines(capture.output(summary(ergm_model)), 
             file.path(output_dir, paste0(output_prefix, "summary.txt")))
}

# Save diagnostics for all 4 Final ERGMs
save_ergm_diagnostics(FinalModel_pre_q3, "final_pre_q3_")
save_ergm_diagnostics(FinalModel_pre_mean, "final_pre_mean_")
save_ergm_diagnostics(FinalModel_post_q3, "final_post_q3_")
save_ergm_diagnostics(FinalModel_post_mean, "final_post_mean_")