# Electoral Cycle Network Analysis: Dutch Parliament 2023-2024

# Create output directories
if(!dir.exists("results/statistics")) dir.create("results/statistics", recursive = TRUE)
if(!dir.exists("results/visualizations")) dir.create("results/visualizations", recursive = TRUE)
if(!dir.exists("results/models")) dir.create("results/models", recursive = TRUE)
if(!dir.exists("results/tests")) dir.create("results/tests", recursive = TRUE)

# 1. LOAD DATA ----------------------------------------------------------------

# Load ideology data
ideology_data <- read.csv("data/political_axes_data.csv", stringsAsFactors = FALSE)
names(ideology_data) <- c("left_right", "conservative_progressive", "party")
ideology_data <- ideology_data[!is.na(ideology_data$party) & ideology_data$party != "", ]

# Load pre-filtered edge lists from Python script

# Study 1 edge lists (identical node structure for QAP - voting agreement only)
study1_edgelist_pre <- read.csv("results/edge_lists/study1_edges_pre_election.csv", stringsAsFactors = FALSE)
study1_edgelist_post <- read.csv("results/edge_lists/study1_edges_post_formation.csv", stringsAsFactors = FALSE)

# Study 2 edge lists (optimized structure for ERGM/visualization)
study2_edgelist_pre <- read.csv("results/edge_lists/study2_edges_pre_election.csv", stringsAsFactors = FALSE)
study2_edgelist_post <- read.csv("results/edge_lists/study2_edges_post_formation.csv", stringsAsFactors = FALSE)
study2_cosponsor_pre <- read.csv("results/edge_lists/study2_cosponsor_pre_election.csv", stringsAsFactors = FALSE)
study2_cosponsor_post <- read.csv("results/edge_lists/study2_cosponsor_post_formation.csv", stringsAsFactors = FALSE)
study2_coalition_pre <- read.csv("results/edge_lists/study2_coalition_pre_election.csv", stringsAsFactors = FALSE)
study2_coalition_post <- read.csv("results/edge_lists/study2_coalition_post_formation.csv", stringsAsFactors = FALSE)


# 2. NORMALIZE WEIGHTS BY TOTAL MOTIONS  -------------------------------------

# Load voting data to count total motions
voting_data_pre <- read.csv("data/voting_data_2023_preelection.csv", stringsAsFactors = FALSE)
voting_data_post <- read.csv("data/voting_data_clean.csv", stringsAsFactors = FALSE)

# Filter to date ranges
voting_data_pre$date <- lubridate::ymd_hms(voting_data_pre$GewijzigdOp)
voting_data_post$date <- lubridate::ymd_hms(voting_data_post$GewijzigdOp)

election_date <- lubridate::ymd("2023-11-22")
formation_date <- lubridate::ymd("2024-07-05")

motion_data_pre <- voting_data_pre[
  voting_data_pre$date >= (election_date - lubridate::years(1)) & 
  voting_data_pre$date <= (election_date - lubridate::days(1)), ]

motion_data_post <- voting_data_post[
  voting_data_post$date >= formation_date, ]

# Count total motions
total_motions_pre <- length(unique(motion_data_pre$Besluit_Id))
total_motions_post <- length(unique(motion_data_post$Besluit_Id))

# Normalize edge weights (raw counts → fractions of total motions)
study1_edgelist_pre$weight <- study1_edgelist_pre$weight / total_motions_pre
study1_edgelist_post$weight <- study1_edgelist_post$weight / total_motions_post
study2_edgelist_pre$weight <- study2_edgelist_pre$weight / total_motions_pre
study2_edgelist_post$weight <- study2_edgelist_post$weight / total_motions_post

# 3. CREATE NETWORKS ----------------------------------------------------------

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

# Create adjacency matrices for Study 1 (QAP)
adj_matrix_pre <- igraph::as_adjacency_matrix(g_pre_study1, attr = "weight", sparse = FALSE)
adj_matrix_post <- igraph::as_adjacency_matrix(g_post_study1, attr = "weight", sparse = FALSE)

# 4. AGREEMENT RATE DISTRIBUTION ANALYSIS (for threshold determination) ------

# Extract edge weights from Study 2 networks
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

# 6. ERGM ANALYSIS (after threshold selection) ----------------------------

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

# Convergence Test Models (decay 0.7)
Model1_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ 
    edges + absdiff("left_right") + 
    edgecov(coalition_matrix_pre) + edgecov(cosponsor_matrix_pre) +
    kstar(3) + gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

Model2_pre_mean <- ergm::ergm(
  net_pre_mean ~ 
    edges + absdiff("left_right") + 
    edgecov(coalition_matrix_pre) + edgecov(cosponsor_matrix_pre) +
    kstar(3) + gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

Model3_post_q3 <- ergm::ergm(
  net_post_q3 ~ 
    edges + absdiff("left_right") + 
    edgecov(coalition_matrix_post) + edgecov(cosponsor_matrix_post) +
    kstar(3) + gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

Model4_post_mean <- ergm::ergm(
  net_post_mean ~ 
    edges + absdiff("left_right") + 
    edgecov(coalition_matrix_post) + edgecov(cosponsor_matrix_post) +
    kstar(3) + gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

# Model comparison: AIC/BIC for threshold selection
model_comparison <- data.frame(
  Model = c("Pre Q3 (0.7)", "Pre Mean (0.7)", "Post Q3 (0.7)", "Post Mean (0.7)"),
  AIC = c(AIC(Model1_pre_q3), AIC(Model2_pre_mean), 
          AIC(Model3_post_q3), AIC(Model4_post_mean)),
  BIC = c(BIC(Model1_pre_q3), BIC(Model2_pre_mean), 
          BIC(Model3_post_q3), BIC(Model4_post_mean))
)

# Save convergence test models
saveRDS(Model1_pre_q3, file = "results/tests/convergence_pre_q3.rds")
saveRDS(Model2_pre_mean, file = "results/tests/convergence_pre_mean.rds")
saveRDS(Model3_post_q3, file = "results/tests/convergence_post_q3.rds")
saveRDS(Model4_post_mean, file = "results/tests/convergence_post_mean.rds")

# Save ERGM diagnostics
save_ergm_diagnostics <- function(ergm_model, output_prefix) {
  output_dir <- "results/ergm_diagnostics/convergence_models"
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

# Save diagnostics for convergence test models
save_ergm_diagnostics(Model1_pre_q3, "convergence_pre_q3_")
save_ergm_diagnostics(Model2_pre_mean, "convergence_pre_mean_")
save_ergm_diagnostics(Model3_post_q3, "convergence_post_q3_")
save_ergm_diagnostics(Model4_post_mean, "convergence_post_mean_")

# 7. STEPWISE MODEL BUILDING FOR PRE-ELECTION Q3 NETWORK ---------------------

# Table 1: Testing exogenous terms
Exog1_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right"),
  control = ergm_control
)

Exog2_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + edgecov(cosponsor_matrix_pre),
  control = ergm_control
)

Exog3_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + edgecov(coalition_matrix_pre),
  control = ergm_control
)

Exog4_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre),
  control = ergm_control
)

# Table 2: Adding endogenous terms one by one
Endog1_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre),
  control = ergm_control
)

Endog2_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre) +
    kstar(3),
  control = ergm_control
)

Endog3_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre) +
    gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

Endog4_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre) +
    kstar(3) + gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

# Table 3: Testing different GWESP decay parameters
Decay1_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre) +
    kstar(3) + gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

Decay2_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre) +
    kstar(3) + gwesp(0.75, fixed = TRUE),
  control = ergm_control
)

Decay3_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre) +
    kstar(3) + gwesp(0.8, fixed = TRUE),
  control = ergm_control
)

Decay4_pre_q3 <- ergm::ergm(
  net_pre_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_pre) + edgecov(coalition_matrix_pre) +
    kstar(3) + gwesp(0.85, fixed = TRUE),
  control = ergm_control
)

# Save stepwise models for pre-election
saveRDS(Exog1_pre_q3, file = "results/tests/stepwise_exog1_pre_q3.rds")
saveRDS(Exog2_pre_q3, file = "results/tests/stepwise_exog2_pre_q3.rds")
saveRDS(Exog3_pre_q3, file = "results/tests/stepwise_exog3_pre_q3.rds")
saveRDS(Exog4_pre_q3, file = "results/tests/stepwise_exog4_pre_q3.rds")

saveRDS(Endog1_pre_q3, file = "results/tests/stepwise_endog1_pre_q3.rds")
saveRDS(Endog2_pre_q3, file = "results/tests/stepwise_endog2_pre_q3.rds")
saveRDS(Endog3_pre_q3, file = "results/tests/stepwise_endog3_pre_q3.rds")
saveRDS(Endog4_pre_q3, file = "results/tests/stepwise_endog4_pre_q3.rds")

saveRDS(Decay1_pre_q3, file = "results/tests/stepwise_decay1_pre_q3.rds")
saveRDS(Decay2_pre_q3, file = "results/tests/stepwise_decay2_pre_q3.rds")
saveRDS(Decay3_pre_q3, file = "results/tests/stepwise_decay3_pre_q3.rds")
saveRDS(Decay4_pre_q3, file = "results/tests/stepwise_decay4_pre_q3.rds")

# 8. STEPWISE MODEL BUILDING FOR POST-FORMATION Q3 NETWORK ------------------

# Table 1: Testing exogenous terms
Exog1_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right"),
  control = ergm_control
)

Exog2_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + edgecov(cosponsor_matrix_post),
  control = ergm_control
)

Exog3_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + edgecov(coalition_matrix_post),
  control = ergm_control
)

Exog4_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post),
  control = ergm_control
)

# Table 2: Adding endogenous terms one by one
Endog1_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post),
  control = ergm_control
)

Endog2_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post) +
    kstar(3),
  control = ergm_control
)

Endog3_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post) +
    gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

Endog4_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post) +
    kstar(3) + gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

# Table 3: Testing different GWESP decay parameters
Decay1_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post) +
    kstar(3) + gwesp(0.7, fixed = TRUE),
  control = ergm_control
)

Decay2_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post) +
    kstar(3) + gwesp(0.75, fixed = TRUE),
  control = ergm_control
)

Decay3_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post) +
    kstar(3) + gwesp(0.8, fixed = TRUE),
  control = ergm_control
)

Decay4_post_q3 <- ergm::ergm(
  net_post_q3 ~ edges + absdiff("left_right") + 
    edgecov(cosponsor_matrix_post) + edgecov(coalition_matrix_post) +
    kstar(3) + gwesp(0.85, fixed = TRUE),
  control = ergm_control
)

# Save stepwise models for post-formation
saveRDS(Exog1_post_q3, file = "results/tests/stepwise_exog1_post_q3.rds")
saveRDS(Exog2_post_q3, file = "results/tests/stepwise_exog2_post_q3.rds")
saveRDS(Exog3_post_q3, file = "results/tests/stepwise_exog3_post_q3.rds")
saveRDS(Exog4_post_q3, file = "results/tests/stepwise_exog4_post_q3.rds")

saveRDS(Endog1_post_q3, file = "results/tests/stepwise_endog1_post_q3.rds")
saveRDS(Endog2_post_q3, file = "results/tests/stepwise_endog2_post_q3.rds")
saveRDS(Endog3_post_q3, file = "results/tests/stepwise_endog3_post_q3.rds")
saveRDS(Endog4_post_q3, file = "results/tests/stepwise_endog4_post_q3.rds")

saveRDS(Decay1_post_q3, file = "results/tests/stepwise_decay1_post_q3.rds")
saveRDS(Decay2_post_q3, file = "results/tests/stepwise_decay2_post_q3.rds")
saveRDS(Decay3_post_q3, file = "results/tests/stepwise_decay3_post_q3.rds")
saveRDS(Decay4_post_q3, file = "results/tests/stepwise_decay4_post_q3.rds")