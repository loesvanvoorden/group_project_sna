# ============================================================================
# THREE-PERIOD NETWORK ANALYSIS: Pre-Election vs Post-Formation Comparison
# Focus: Far from Election, Close to Election, and Post Cabinet Formation
# ============================================================================

library(dplyr)
library(lubridate)
library(igraph)
library(ggplot2)

# ============================================================================
# RESEARCH DESIGN
# ============================================================================

cat("===============================================================================\n")
cat("THREE-PERIOD NETWORK ANALYSIS: Pre-Election & Post-Formation Comparison\n")
cat("Focus: Network Evolution Across Electoral Cycle & Cabinet Formation\n")
cat("===============================================================================\n\n")

cat("RESEARCH QUESTION:\n")
cat("How do co-voting patterns between parties change across the electoral cycle?\n\n")

cat("TEMPORAL DESIGN (EQUAL PERIOD STRUCTURE):\n")
cat("• FAR FROM ELECTION: Q1+Q2 2023 (January-June) - 6 months\n")
cat("• CLOSE TO ELECTION: Q3+Q4 2023 (July-November 22) - ~5 months\n")
cat("• POST FORMATION: Q3+Q4 2024 (July-December) - 6 months\n")
cat("• Election Date: November 22, 2023\n")
cat("• Cabinet Formation: July 2, 2024\n\n")

cat("NETWORK STRUCTURE:\n")
cat("• ALL edges included (complete cooperation network)\n")
cat("• Edges weighted by number of agreements between parties\n")
cat("• Minimum 5 shared votes required for edge creation\n\n")

cat("VISUALIZATION:\n")
cat("• RED nodes = Left-wing parties (SP, PvdD, GroenLinks, PvdA, etc.)\n")
cat("• ORANGE nodes = Center parties (D66, Volt)\n")  
cat("• BLUE nodes = Right-wing parties (VVD, CDA, PVV, FVD, etc.)\n")
cat("• Node size = degree centrality (more connections = larger)\n")
cat("• Edge thickness = cooperation strength (more agreements = thicker)\n")
cat("• EDGE HIGHLIGHTING: Edges 30% above mean weight shown prominently\n")
cat("  (Weaker edges very faint to clearly show strongest cooperation patterns)\n\n")

# ============================================================================
# LOAD AND PREPARE DATA
# ============================================================================

cat("Loading data...\n")

# Load pre-election data (2023) - for far and close periods
voting_data_2023 <- read.csv("voting_data_2023_preelection.csv", stringsAsFactors = FALSE)
voting_data_2023$date <- ymd_hms(voting_data_2023$GewijzigdOp)
voting_data_2023$quarter <- quarter(voting_data_2023$date)

# Load post-election data (2024) - for post period only
voting_data_2024 <- read.csv("voting_data_clean.csv", stringsAsFactors = FALSE)
voting_data_2024$date <- ymd_hms(voting_data_2024$GewijzigdOp)
voting_data_2024$quarter <- quarter(voting_data_2024$date)

# Note: We'll use voting_data_2023 for 2023 periods and voting_data_2024 for 2024 periods
# to avoid any potential data overlap issues
voting_data <- voting_data_2023

cat(sprintf("Total records: %d\n", nrow(voting_data)))
cat(sprintf("Date range: %s to %s\n", min(voting_data$date), max(voting_data$date)))
cat(sprintf("Unique parties: %d\n", length(unique(voting_data$ActorFractie))))
cat(sprintf("Unique motions: %d\n\n", length(unique(voting_data$Besluit_Id))))

# ============================================================================
# CREATE TEMPORAL PERIODS
# ============================================================================

cat("Creating temporal periods...\n")

# Period 1: FAR FROM ELECTION (Q1+Q2 2023) - use same approach as q1q2_vs_q3q4
data_far <- voting_data_2023 %>%
  filter(quarter %in% c(1, 2))

cat("FAR FROM ELECTION (Q1+Q2 2023):\n")
cat(sprintf("  Votes: %d\n", nrow(data_far)))
cat(sprintf("  Motions: %d\n", length(unique(data_far$Besluit_Id))))
cat(sprintf("  Parties: %d\n", length(unique(data_far$ActorFractie))))
cat(sprintf("  Date range: %s to %s\n\n", min(data_far$date), max(data_far$date)))

# Period 2: CLOSE TO ELECTION (Q3+Q4 2023) - use same approach as q1q2_vs_q3q4
data_close <- voting_data_2023 %>%
  filter(quarter %in% c(3, 4))

cat("CLOSE TO ELECTION (Q3+Q4 2023):\n")
cat(sprintf("  Votes: %d\n", nrow(data_close)))
cat(sprintf("  Motions: %d\n", length(unique(data_close$Besluit_Id))))
cat(sprintf("  Parties: %d\n", length(unique(data_close$ActorFractie))))
cat(sprintf("  Date range: %s to %s\n\n", min(data_close$date), max(data_close$date)))

# Period 3: POST FORMATION (Q3+Q4 2024) - use 2024 data
data_post <- voting_data_2024 %>%
  filter(year(date) == 2024, quarter %in% c(3, 4))

cat("POST FORMATION (Q3+Q4 2024):\n")
cat(sprintf("  Votes: %d\n", nrow(data_post)))
cat(sprintf("  Motions: %d\n", length(unique(data_post$Besluit_Id))))
cat(sprintf("  Parties: %d\n", length(unique(data_post$ActorFractie))))
cat(sprintf("  Date range: %s to %s\n\n", min(data_post$date), max(data_post$date)))

# ============================================================================
# NETWORK CREATION FUNCTIONS
# ============================================================================

calculate_party_agreements <- function(data) {
  
  # Create party-motion voting matrix
  party_votes <- data %>%
    select(ActorFractie, Besluit_Id, Soort) %>%
    distinct()
  
  cat(sprintf("  Clean voting records: %d\n", nrow(party_votes)))
  
  # Self-join to get all party pairs per motion
  agreements <- party_votes %>%
    inner_join(party_votes, by = "Besluit_Id", suffix = c("_1", "_2")) %>%
    filter(ActorFractie_1 < ActorFractie_2)  # Avoid duplicates
  
  # Calculate agreement statistics
  party_agreement_summary <- agreements %>%
    group_by(ActorFractie_1, ActorFractie_2) %>%
    summarise(
      total_votes = n(),
      agreements = sum(Soort_1 == Soort_2),
      disagreements = sum(Soort_1 != Soort_2),
      agreement_rate = agreements / total_votes,
      .groups = 'drop'
    ) %>%
    filter(total_votes >= 5)  # Minimum 5 shared votes
  
  cat(sprintf("  Party pairs: %d\n", nrow(party_agreement_summary)))
  cat(sprintf("  Mean agreements: %.1f\n", mean(party_agreement_summary$agreements)))
  cat(sprintf("  Mean agreement rate: %.3f\n\n", mean(party_agreement_summary$agreement_rate)))
  
  return(party_agreement_summary)
}

create_party_network <- function(agreements, all_parties) {
  
  # Include ALL edges (no threshold filtering)
  edges <- agreements %>%
    select(from = ActorFractie_1, to = ActorFractie_2, 
           weight = agreements, agreement_rate, total_votes)
  
  cat(sprintf("    Total party pairs: %d\n", nrow(agreements)))
  cat(sprintf("    Mean agreements: %.1f\n", mean(agreements$agreements)))
  cat(sprintf("    Including ALL %d edges in network\n", nrow(edges)))
  
  # Create network
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = all_parties)
  
  # Add party attributes for visualization
  V(g)$degree <- degree(g)
  V(g)$strength <- strength(g)
  V(g)$betweenness <- betweenness(g, weights = NA)
  
  # Party categories for coloring based on ideology
  V(g)$party_type <- case_when(
    V(g)$name %in% c("SP", "PvdD", "BIJ1", "GroenLinks", "PvdA", "DENK") ~ "Left",
    V(g)$name %in% c("D66", "Volt") ~ "Center", 
    V(g)$name %in% c("VVD", "CDA", "ChristenUnie", "BBB", "PVV", "FVD", "SGP", "JA21") ~ "Right",
    TRUE ~ "Center"
  )
  
  # Ideology for layout (approximate left-right positions)
  V(g)$ideology <- case_when(
    V(g)$name %in% c("SP", "PvdD", "BIJ1") ~ 1,           # Left
    V(g)$name %in% c("GroenLinks", "PvdA", "DENK") ~ 2,   # Center-left
    V(g)$name %in% c("D66", "Volt") ~ 3,                  # Center
    V(g)$name %in% c("VVD", "CDA", "ChristenUnie", "BBB") ~ 4,  # Center-right
    V(g)$name %in% c("PVV", "FVD", "SGP", "JA21") ~ 5,    # Right
    TRUE ~ 3  # Default center
  )
  
  cat(sprintf("    Final network: %d nodes, %d edges, density = %.3f\n\n", 
              vcount(g), ecount(g), edge_density(g)))
  
  return(g)
}

# ============================================================================
# CREATE NETWORKS FOR ALL THREE PERIODS
# ============================================================================

cat("\nCREATING PARTY COOPERATION NETWORKS\n")
cat("====================================\n")

cat("Processing FAR FROM ELECTION period...\n")
agreements_far <- calculate_party_agreements(data_far)

cat("Processing CLOSE TO ELECTION period...\n")
agreements_close <- calculate_party_agreements(data_close)

cat("Processing POST FORMATION period...\n")
agreements_post <- calculate_party_agreements(data_post)

# Get unique parties for each period (to match q1q2_vs_q3q4 analysis)
parties_far <- unique(data_far$ActorFractie)
parties_far <- parties_far[!is.na(parties_far)]

parties_close <- unique(data_close$ActorFractie)
parties_close <- parties_close[!is.na(parties_close)]

parties_post <- unique(data_post$ActorFractie)
parties_post <- parties_post[!is.na(parties_post)]

# For visualization consistency, use union of all parties but note each period's active parties
all_parties_for_layout <- unique(c(parties_far, parties_close, parties_post))

cat(sprintf("Parties in FAR period: %d\n", length(parties_far)))
cat(sprintf("Parties in CLOSE period: %d\n", length(parties_close)))
cat(sprintf("Parties in POST period: %d\n", length(parties_post)))
cat(sprintf("Total unique parties across all periods: %d\n\n", length(all_parties_for_layout)))

# Create networks using only parties active in each specific period
cat("Creating FAR FROM ELECTION network:\n")
g_far <- create_party_network(agreements_far, parties_far)

cat("\nCreating CLOSE TO ELECTION network:\n")
g_close <- create_party_network(agreements_close, parties_close)

cat("\nCreating POST FORMATION network:\n")
g_post <- create_party_network(agreements_post, parties_post)

# ============================================================================
# NETWORK COMPARISON STATISTICS
# ============================================================================

cat("\nNETWORK COMPARISON STATISTICS\n")
cat("=============================\n")

comparison_df <- data.frame(
  Metric = c("Nodes", "Edges", "Density", "Mean Degree", "Transitivity", 
             "Avg Path Length", "Modularity", "Components"),
  Far = c(
    vcount(g_far),
    ecount(g_far),
    edge_density(g_far),
    mean(degree(g_far)),
    transitivity(g_far),
    ifelse(is_connected(g_far), mean_distance(g_far), NA),
    ifelse(ecount(g_far) > 0, modularity(cluster_louvain(g_far)), NA),
    count_components(g_far)
  ),
  Close = c(
    vcount(g_close),
    ecount(g_close),
    edge_density(g_close),
    mean(degree(g_close)),
    transitivity(g_close),
    ifelse(is_connected(g_close), mean_distance(g_close), NA),
    ifelse(ecount(g_close) > 0, modularity(cluster_louvain(g_close)), NA),
    count_components(g_close)
  ),
  Post = c(
    vcount(g_post),
    ecount(g_post),
    edge_density(g_post),
    mean(degree(g_post)),
    transitivity(g_post),
    ifelse(is_connected(g_post), mean_distance(g_post), NA),
    ifelse(ecount(g_post) > 0, modularity(cluster_louvain(g_post)), NA),
    count_components(g_post)
  )
)

comparison_df <- comparison_df %>%
  mutate(
    Change_Far_to_Close = Close - Far,
    Change_Close_to_Post = Post - Close,
    PctChange_Far_to_Close = ifelse(Far != 0, (Close - Far) / Far * 100, NA),
    PctChange_Close_to_Post = ifelse(Close != 0, (Post - Close) / Close * 100, NA)
  )

print(comparison_df)

# ============================================================================
# NETWORK VISUALIZATIONS
# ============================================================================

cat("\nCREATING NETWORK VISUALIZATIONS\n")
cat("(Highlighting edges 30% above mean weight for clarity)\n")

# Color schemes based on political ideology
party_colors <- c("Left" = "#E74C3C",      # Red for left-wing parties
                  "Center" = "#F39C12",    # Orange for centrist parties  
                  "Right" = "#3498DB")     # Blue for right-wing parties

ideology_colors <- colorRampPalette(c("#E74C3C", "#F39C12", "#F1C40F", "#2ECC71", "#3498DB"))(5)

# ============================================================================
# 1. SIDE-BY-SIDE NETWORK COMPARISON (3 NETWORKS)
# ============================================================================

pdf("network_comparison_three_periods.pdf", width = 24, height = 8)
par(mfrow = c(1, 3), mar = c(2, 2, 4, 2))

# Common layout for comparison
set.seed(42)
# Use ideology-based layout for meaningful positioning
layout_coords <- matrix(0, nrow = length(all_parties_for_layout), ncol = 2)
for(i in 1:length(all_parties_for_layout)) {
  party <- all_parties_for_layout[i]
  if(party %in% V(g_far)$name) {
    ideology_pos <- V(g_far)$ideology[V(g_far)$name == party][1]
  } else if(party %in% V(g_close)$name) {
    ideology_pos <- V(g_close)$ideology[V(g_close)$name == party][1]
  } else if(party %in% V(g_post)$name) {
    ideology_pos <- V(g_post)$ideology[V(g_post)$name == party][1]
  } else {
    ideology_pos <- 3
  }
  layout_coords[i, 1] <- ideology_pos + runif(1, -0.3, 0.3)
  layout_coords[i, 2] <- runif(1, -1, 1)
}

# FAR FROM ELECTION network
V(g_far)$color <- party_colors[V(g_far)$party_type]
V(g_far)$size <- pmax(8, sqrt(V(g_far)$degree) * 4)

# Highlight strongest edges: 30% above mean weight
mean_weight_far <- mean(E(g_far)$weight)
weight_threshold_far <- mean_weight_far * 1.30
edges_to_show_far <- which(E(g_far)$weight >= weight_threshold_far)

# Set edge properties - stronger edges are more visible
E(g_far)$width <- pmax(0.5, (E(g_far)$weight / max(E(g_far)$weight)) * 3)
E(g_far)$color <- ifelse(E(g_far)$weight >= weight_threshold_far, 
                         rgb(0.5, 0.5, 0.5, 0.85),  # Prominent for strong edges
                         rgb(0.5, 0.5, 0.5, 0.08))  # Very faint for weaker edges

cat(sprintf("  FAR: Highlighting %d/%d edges (30%% above mean of %.1f = threshold: %.1f)\n", 
            length(edges_to_show_far), ecount(g_far), mean_weight_far, weight_threshold_far))

plot(g_far,
     layout = layout_coords[match(V(g_far)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "FAR FROM ELECTION\n(Q1+Q2 2023: January-June)")

# CLOSE TO ELECTION network
V(g_close)$color <- party_colors[V(g_close)$party_type]
V(g_close)$size <- pmax(8, sqrt(V(g_close)$degree) * 4)

# Highlight strongest edges: 30% above mean weight
mean_weight_close <- mean(E(g_close)$weight)
weight_threshold_close <- mean_weight_close * 1.30
edges_to_show_close <- which(E(g_close)$weight >= weight_threshold_close)

# Set edge properties - stronger edges are more visible
E(g_close)$width <- pmax(0.5, (E(g_close)$weight / max(E(g_close)$weight)) * 3)
E(g_close)$color <- ifelse(E(g_close)$weight >= weight_threshold_close, 
                           rgb(0.5, 0.5, 0.5, 0.85),  # Prominent for strong edges
                           rgb(0.5, 0.5, 0.5, 0.08))  # Very faint for weaker edges

cat(sprintf("  CLOSE: Highlighting %d/%d edges (30%% above mean of %.1f = threshold: %.1f)\n", 
            length(edges_to_show_close), ecount(g_close), mean_weight_close, weight_threshold_close))

plot(g_close,
     layout = layout_coords[match(V(g_close)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "CLOSE TO ELECTION\n(Q3+Q4 2023: July-November)")

# POST ELECTION network
V(g_post)$color <- party_colors[V(g_post)$party_type]
V(g_post)$size <- pmax(8, sqrt(V(g_post)$degree) * 4)

# Highlight strongest edges: 30% above mean weight
mean_weight_post <- mean(E(g_post)$weight)
weight_threshold_post <- mean_weight_post * 1.30
edges_to_show_post <- which(E(g_post)$weight >= weight_threshold_post)

# Set edge properties - stronger edges are more visible
E(g_post)$width <- pmax(0.5, (E(g_post)$weight / max(E(g_post)$weight)) * 3)
E(g_post)$color <- ifelse(E(g_post)$weight >= weight_threshold_post, 
                          rgb(0.5, 0.5, 0.5, 0.85),  # Prominent for strong edges
                          rgb(0.5, 0.5, 0.5, 0.08))  # Very faint for weaker edges

cat(sprintf("  POST: Highlighting %d/%d edges (30%% above mean of %.1f = threshold: %.1f)\n", 
            length(edges_to_show_post), ecount(g_post), mean_weight_post, weight_threshold_post))

plot(g_post,
     layout = layout_coords[match(V(g_post)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "POST FORMATION\n(Q3+Q4 2024: July-December)")

dev.off()

# ============================================================================
# 2. DETAILED ANALYSIS
# ============================================================================

pdf("detailed_three_period_analysis.pdf", width = 24, height = 16)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))

# Degree distributions
hist(degree(g_far), breaks = 20, col = "#E74C3C", border = "white",
     main = "Degree Distribution - Far", xlab = "Degree", ylab = "Frequency")
hist(degree(g_close), breaks = 20, col = "#F39C12", border = "white",
     main = "Degree Distribution - Close", xlab = "Degree", ylab = "Frequency")
hist(degree(g_post), breaks = 20, col = "#3498DB", border = "white",
     main = "Degree Distribution - Post", xlab = "Degree", ylab = "Frequency")

# Community detection
communities_far <- cluster_louvain(g_far)
communities_close <- cluster_louvain(g_close)
communities_post <- cluster_louvain(g_post)

plot(communities_far, g_far, 
     layout = layout_coords[match(V(g_far)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.6, main = "Communities - Far from Election")
plot(communities_close, g_close,
     layout = layout_coords[match(V(g_close)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.6, main = "Communities - Close to Election")
plot(communities_post, g_post,
     layout = layout_coords[match(V(g_post)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.6, main = "Communities - Post Formation")

# Party type comparison
party_type_far <- table(V(g_far)$party_type)
party_type_close <- table(V(g_close)$party_type)
party_type_post <- table(V(g_post)$party_type)

barplot(party_type_far, col = party_colors[names(party_type_far)],
        main = "Party Ideology Distribution - Far",
        ylab = "Number of Parties", las = 2)

barplot(party_type_close, col = party_colors[names(party_type_close)],
        main = "Party Ideology Distribution - Close", 
        ylab = "Number of Parties", las = 2)

barplot(party_type_post, col = party_colors[names(party_type_post)],
        main = "Party Ideology Distribution - Post", 
        ylab = "Number of Parties", las = 2)

dev.off()

# ============================================================================
# 3. CHANGE ANALYSIS VISUALIZATION
# ============================================================================

pdf("network_changes_three_periods.pdf", width = 20, height = 12)
par(mfrow = c(2, 3), mar = c(8, 4, 3, 2))

# Network metrics comparison
metrics_data <- comparison_df %>%
  filter(Metric %in% c("Edges", "Density", "Mean Degree", "Components")) %>%
  select(Metric, Far, Close, Post)

metrics_matrix <- as.matrix(metrics_data[, -1])
rownames(metrics_matrix) <- metrics_data$Metric

barplot(t(metrics_matrix), beside = TRUE, col = c("#E74C3C", "#F39C12", "#3498DB"),
        main = "Network Metrics Comparison",
        ylab = "Value", las = 2,
        legend.text = c("Far (Q1+Q2 2023)", "Close (Q3+Q4 2023)", "Post (Q1+Q2 2024)"),
        args.legend = list(x = "topright"))

# Percent change visualization - Far to Close
valid_changes_fc <- comparison_df[is.finite(comparison_df$PctChange_Far_to_Close), ]
if(nrow(valid_changes_fc) > 0) {
  barplot(valid_changes_fc$PctChange_Far_to_Close,
          names.arg = valid_changes_fc$Metric,
          col = ifelse(valid_changes_fc$PctChange_Far_to_Close > 0, "#2ECC71", "#E74C3C"),
          main = "Percent Change (Far → Close)",
          ylab = "Percent Change (%)", las = 2)
  abline(h = 0, lty = 2)
} else {
  plot.new()
  text(0.5, 0.5, "No valid percent changes to display", cex = 1.5)
}

# Percent change visualization - Close to Post
valid_changes_cp <- comparison_df[is.finite(comparison_df$PctChange_Close_to_Post), ]
if(nrow(valid_changes_cp) > 0) {
  barplot(valid_changes_cp$PctChange_Close_to_Post,
          names.arg = valid_changes_cp$Metric,
          col = ifelse(valid_changes_cp$PctChange_Close_to_Post > 0, "#2ECC71", "#E74C3C"),
          main = "Percent Change (Close → Post)",
          ylab = "Percent Change (%)", las = 2)
  abline(h = 0, lty = 2)
} else {
  plot.new()
  text(0.5, 0.5, "No valid percent changes to display", cex = 1.5)
}

# Party activity comparison
party_activity_far <- data_far %>% count(ActorFractie, sort = TRUE) %>% head(10)
party_activity_close <- data_close %>% count(ActorFractie, sort = TRUE) %>% head(10)
party_activity_post <- data_post %>% count(ActorFractie, sort = TRUE) %>% head(10)

barplot(party_activity_far$n, names.arg = party_activity_far$ActorFractie,
        col = "#E74C3C", main = "Top 10 Most Active Parties - Far",
        ylab = "Number of Votes", las = 2, cex.names = 0.7)

barplot(party_activity_close$n, names.arg = party_activity_close$ActorFractie,
        col = "#F39C12", main = "Top 10 Most Active Parties - Close",
        ylab = "Number of Votes", las = 2, cex.names = 0.7)

barplot(party_activity_post$n, names.arg = party_activity_post$ActorFractie,
        col = "#3498DB", main = "Top 10 Most Active Parties - Post",
        ylab = "Number of Votes", las = 2, cex.names = 0.7)

dev.off()

# ============================================================================
# EXPORT NETWORK DATA
# ============================================================================

cat("\nExporting network data...\n")

# Export edge lists
write.csv(igraph::as_data_frame(g_far, "edges"), 
          "edges_far_from_election.csv", row.names = FALSE)
write.csv(igraph::as_data_frame(g_close, "edges"), 
          "edges_close_to_election.csv", row.names = FALSE)
write.csv(igraph::as_data_frame(g_post, "edges"), 
          "edges_post_formation.csv", row.names = FALSE)

# Export comparison statistics
write.csv(comparison_df, "three_period_comparison.csv", row.names = FALSE)

cat("\n===============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("===============================================================================\n")
cat("Generated files:\n")
cat("  1. network_comparison_three_periods.pdf - Side-by-side comparison\n")
cat("  2. detailed_three_period_analysis.pdf - Detailed network analysis\n")
cat("  3. network_changes_three_periods.pdf - Change metrics visualization\n")
cat("  4. edges_far_from_election.csv - Edge list for far period\n")
cat("  5. edges_close_to_election.csv - Edge list for close period\n")
cat("  6. edges_post_formation.csv - Edge list for post formation period\n")
cat("  7. three_period_comparison.csv - Statistical comparison\n")
cat("===============================================================================\n")

