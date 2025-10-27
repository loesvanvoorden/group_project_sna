# ============================================================================
# THREE MONTH PRE-ELECTION vs POST-FORMATION NETWORK ANALYSIS
# Focus: 3 Months Before Election vs 3 Months After Cabinet Formation
# ============================================================================

library(lubridate)
library(igraph)
library(ggplot2)

# ============================================================================
# RESEARCH DESIGN
# ============================================================================

cat("===============================================================================\n")
cat("THREE MONTH PRE-ELECTION vs POST-FORMATION NETWORK ANALYSIS\n")
cat("Focus: 3 Months Before Election vs 3 Months After Cabinet Formation\n")
cat("===============================================================================\n\n")

cat("RESEARCH QUESTION:\n")
cat("How do co-voting patterns between parties change from three months before election to three months after cabinet formation?\n\n")

cat("TEMPORAL DESIGN:\n")
cat("• PRE-ELECTION: August 22, 2023 - November 21, 2023 (3 months before election)\n")
cat("• POST-FORMATION: July 5, 2024 - October 4, 2024 (3 months after formation)\n")
cat("• Election Date: November 22, 2023\n")
cat("• Cabinet Formation: July 5, 2024\n\n")

cat("NETWORK STRUCTURE:\n")
cat("• ALL edges included (complete cooperation network)\n")
cat("• Edges weighted by number of agreements between parties\n")
cat("• Minimum 5 shared votes required for edge creation\n\n")

cat("VISUALIZATION:\n")
cat("• RED nodes = Left-wing parties (BIJ1, PvdD, GroenLinks-PvdA, SP, DENK, ChristenUnie, 50PLUS)\n")
cat("• ORANGE nodes = Center parties (Volt, D66, NSC, BBB)\n")  
cat("• BLUE nodes = Right-wing parties (CDA, VVD, SGP, PVV, JA21, FVD, BVNL)\n")
cat("• Node size = degree centrality (more connections = larger)\n")
cat("• Edge thickness = cooperation strength (more agreements = thicker)\n")
cat("• EDGE HIGHLIGHTING: Edges 30% above mean weight shown prominently\n")
cat("  (Weaker edges very faint to clearly show strongest cooperation patterns)\n\n")

# ============================================================================
# LOAD AND PREPARE DATA
# ============================================================================

cat("Loading data...\n")

# Load pre-election data (2023)
voting_data_2023 <- read.csv("data/voting_data_2023_preelection.csv", stringsAsFactors = FALSE)
voting_data_2023$date <- ymd_hms(voting_data_2023$GewijzigdOp)

# Load post-election data (2024)
voting_data_2024 <- read.csv("data/voting_data_clean.csv", stringsAsFactors = FALSE)
voting_data_2024$date <- ymd_hms(voting_data_2024$GewijzigdOp)

cat(sprintf("Total 2023 records: %d\n", nrow(voting_data_2023)))
cat(sprintf("Total 2024 records: %d\n\n", nrow(voting_data_2024)))

# ============================================================================
# CREATE TEMPORAL PERIODS
# ============================================================================

cat("Creating temporal periods...\n")

# Key dates
election_date <- ymd("2023-11-22")
formation_date <- ymd("2024-07-05")

# Period 1: PRE-ELECTION (3 months before election)
# August 22, 2023 - November 21, 2023
pre_start <- election_date - months(3)
pre_end <- election_date - days(1)

# Filter data using base R
data_pre <- voting_data_2023[voting_data_2023$date >= pre_start & voting_data_2023$date <= pre_end, ]

cat("PRE-ELECTION (Aug 22, 2023 - Nov 21, 2023):\n")
cat(sprintf("  Votes: %d\n", nrow(data_pre)))
cat(sprintf("  Motions: %d\n", length(unique(data_pre$Besluit_Id))))
cat(sprintf("  Parties: %d\n", length(unique(data_pre$ActorFractie))))
cat(sprintf("  Date range: %s to %s\n\n", min(data_pre$date), max(data_pre$date)))

# Period 2: POST-FORMATION (3 months after cabinet formation)
# July 5, 2024 - October 4, 2024
post_start <- formation_date
post_end <- formation_date + months(3)

# Filter data using base R
data_post <- voting_data_2024[voting_data_2024$date >= post_start & voting_data_2024$date <= post_end, ]

cat("POST-FORMATION (Jul 5, 2024 - Oct 4, 2024):\n")
cat(sprintf("  Votes: %d\n", nrow(data_post)))
cat(sprintf("  Motions: %d\n", length(unique(data_post$Besluit_Id))))
cat(sprintf("  Parties: %d\n", length(unique(data_post$ActorFractie))))
cat(sprintf("  Date range: %s to %s\n\n", min(data_post$date), max(data_post$date)))

# ============================================================================
# NETWORK CREATION FUNCTIONS
# ============================================================================

calculate_party_agreements <- function(data) {
  
  # Create party-motion voting matrix using base R
  party_votes <- data[, c("ActorFractie", "Besluit_Id", "Soort")]
  party_votes <- party_votes[!duplicated(party_votes), ]
  
  cat(sprintf("  Clean voting records: %d\n", nrow(party_votes)))
  
  # Create all party pairs per motion using base R
  agreements_list <- list()
  
  # Get unique motions
  unique_motions <- unique(party_votes$Besluit_Id)
  
  for(motion in unique_motions) {
    motion_data <- party_votes[party_votes$Besluit_Id == motion, ]
    parties_in_motion <- motion_data$ActorFractie
    votes_in_motion <- motion_data$Soort
    
    # Create all pairs of parties in this motion
    if(length(parties_in_motion) >= 2) {
      for(i in 1:(length(parties_in_motion)-1)) {
        for(j in (i+1):length(parties_in_motion)) {
          party1 <- parties_in_motion[i]
          party2 <- parties_in_motion[j]
          vote1 <- votes_in_motion[i]
          vote2 <- votes_in_motion[j]
          
          # Create pair identifier (alphabetical order)
          if(party1 < party2) {
            pair_id <- paste(party1, party2, sep = "_")
            agreement <- ifelse(vote1 == vote2, 1, 0)
          } else {
            pair_id <- paste(party2, party1, sep = "_")
            agreement <- ifelse(vote1 == vote2, 1, 0)
          }
          
          if(pair_id %in% names(agreements_list)) {
            agreements_list[[pair_id]]$total_votes <- agreements_list[[pair_id]]$total_votes + 1
            agreements_list[[pair_id]]$agreements <- agreements_list[[pair_id]]$agreements + agreement
          } else {
            agreements_list[[pair_id]] <- list(
              party1 = ifelse(party1 < party2, party1, party2),
              party2 = ifelse(party1 < party2, party2, party1),
              total_votes = 1,
              agreements = agreement
            )
          }
        }
      }
    }
  }
  
  # Convert to data frame
  if(length(agreements_list) > 0) {
    agreements_df <- data.frame(
      ActorFractie_1 = sapply(agreements_list, function(x) x$party1),
      ActorFractie_2 = sapply(agreements_list, function(x) x$party2),
      total_votes = sapply(agreements_list, function(x) x$total_votes),
      agreements = sapply(agreements_list, function(x) x$agreements),
      stringsAsFactors = FALSE
    )
    
    # Calculate additional metrics
    agreements_df$disagreements <- agreements_df$total_votes - agreements_df$agreements
    agreements_df$agreement_rate <- agreements_df$agreements / agreements_df$total_votes
    
    # Filter minimum 5 shared votes
    agreements_df <- agreements_df[agreements_df$total_votes >= 5, ]
  } else {
    agreements_df <- data.frame(
      ActorFractie_1 = character(0),
      ActorFractie_2 = character(0),
      total_votes = numeric(0),
      agreements = numeric(0),
      disagreements = numeric(0),
      agreement_rate = numeric(0)
    )
  }
  
  cat(sprintf("  Party pairs: %d\n", nrow(agreements_df)))
  if(nrow(agreements_df) > 0) {
    cat(sprintf("  Mean agreements: %.1f\n", mean(agreements_df$agreements)))
    cat(sprintf("  Mean agreement rate: %.3f\n\n", mean(agreements_df$agreement_rate)))
  } else {
    cat("  No party pairs found\n\n")
  }
  
  return(agreements_df)
}

create_party_network <- function(agreements, all_parties) {
  
  if(nrow(agreements) == 0) {
    # Create empty network
    g <- make_empty_graph(n = length(all_parties), directed = FALSE)
    V(g)$name <- all_parties
    return(g)
  }
  
  # Include ALL edges (no threshold filtering)
  edges <- data.frame(
    from = agreements$ActorFractie_1,
    to = agreements$ActorFractie_2,
    weight = agreements$agreements,
    agreement_rate = agreements$agreement_rate,
    total_votes = agreements$total_votes
  )
  
  cat(sprintf("    Total party pairs: %d\n", nrow(agreements)))
  cat(sprintf("    Mean agreements: %.1f\n", mean(agreements$agreements)))
  cat(sprintf("    Including ALL %d edges in network\n", nrow(edges)))
  
  # Create network
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = all_parties)
  
  # Add party attributes for visualization
  V(g)$degree <- degree(g)
  V(g)$strength <- strength(g)
  V(g)$betweenness <- betweenness(g, weights = NA)
  
  # Party categories for coloring based on Kieskompas ideology data
  party_names <- V(g)$name
  # Left: negative values (< -0.2)
  # Center: close to 0 (-0.2 to 0.2)
  # Right: positive values (> 0.2)
  V(g)$party_type <- ifelse(
    party_names %in% c("BIJ1", "PvdD", "GroenLinks", "PvdA", "GroenLinks-PvdA", "DENK", "SP", "ChristenUnie", "50PLUS"), "Left",
    ifelse(party_names %in% c("Volt", "D66", "NSC", "BBB"), "Center", "Right")
  )
  
  # Ideology for layout (left-right positions based on Kieskompas)
  # Far left: BIJ1, PvdD, GroenLinks-PvdA, DENK, SP
  # Center-left: ChristenUnie, 50PLUS, Volt, D66, NSC
  # Center-right: BBB, PVV, CDA
  # Right: VVD, SGP
  # Far right: JA21, FVD, BVNL
  V(g)$ideology <- ifelse(
    party_names %in% c("BIJ1", "PvdD", "GroenLinks", "PvdA", "GroenLinks-PvdA", "DENK", "SP"), 1,
    ifelse(party_names %in% c("ChristenUnie", "50PLUS", "Volt", "D66", "NSC"), 2,
    ifelse(party_names %in% c("BBB", "PVV", "CDA"), 3,
    ifelse(party_names %in% c("VVD", "SGP"), 4, 5)))
  )
  
  cat(sprintf("    Final network: %d nodes, %d edges, density = %.3f\n\n", 
              vcount(g), ecount(g), edge_density(g)))
  
  return(g)
}

# ============================================================================
# CREATE NETWORKS FOR BOTH PERIODS
# ============================================================================

cat("\nCREATING PARTY COOPERATION NETWORKS\n")
cat("====================================\n")

cat("Processing PRE-ELECTION period...\n")
agreements_pre <- calculate_party_agreements(data_pre)

cat("Processing POST-FORMATION period...\n")
agreements_post <- calculate_party_agreements(data_post)

# Get unique parties for each period
parties_pre <- unique(data_pre$ActorFractie)
parties_pre <- parties_pre[!is.na(parties_pre)]

parties_post <- unique(data_post$ActorFractie)
parties_post <- parties_post[!is.na(parties_post)]

# For visualization consistency, use union of all parties
all_parties_for_layout <- unique(c(parties_pre, parties_post))

cat(sprintf("Parties in PRE period: %d\n", length(parties_pre)))
cat(sprintf("Parties in POST period: %d\n", length(parties_post)))
cat(sprintf("Total unique parties across both periods: %d\n\n", length(all_parties_for_layout)))

# Create networks using only parties active in each specific period
cat("Creating PRE-ELECTION network:\n")
g_pre <- create_party_network(agreements_pre, parties_pre)

cat("Creating POST-FORMATION network:\n")
g_post <- create_party_network(agreements_post, parties_post)

# ============================================================================
# NETWORK COMPARISON STATISTICS
# ============================================================================

cat("\nNETWORK COMPARISON STATISTICS\n")
cat("=============================\n")

comparison_df <- data.frame(
  Metric = c("Nodes", "Edges", "Density", "Mean Degree", "Transitivity", 
             "Avg Path Length", "Modularity", "Components"),
  Pre = c(
    vcount(g_pre),
    ecount(g_pre),
    edge_density(g_pre),
    mean(degree(g_pre)),
    transitivity(g_pre),
    ifelse(is_connected(g_pre), mean_distance(g_pre), NA),
    ifelse(ecount(g_pre) > 0, modularity(cluster_louvain(g_pre)), NA),
    count_components(g_pre)
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

# Calculate changes using base R
comparison_df$Change_Pre_to_Post <- comparison_df$Post - comparison_df$Pre
comparison_df$PctChange_Pre_to_Post <- ifelse(comparison_df$Pre != 0, 
                                            (comparison_df$Post - comparison_df$Pre) / comparison_df$Pre * 100, 
                                            NA)

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

# ============================================================================
# 1. SIDE-BY-SIDE NETWORK COMPARISON (2 NETWORKS)
# ============================================================================

pdf("results/visualizations/network_comparison_three_month_pre_vs_post_formation.pdf", width = 16, height = 8)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 2))

# Common layout for comparison
set.seed(42)
# Use ideology-based layout for meaningful positioning
layout_coords <- matrix(0, nrow = length(all_parties_for_layout), ncol = 2)
for(i in 1:length(all_parties_for_layout)) {
  party <- all_parties_for_layout[i]
  if(party %in% V(g_pre)$name) {
    ideology_pos <- V(g_pre)$ideology[V(g_pre)$name == party][1]
  } else if(party %in% V(g_post)$name) {
    ideology_pos <- V(g_post)$ideology[V(g_post)$name == party][1]
  } else {
    ideology_pos <- 3
  }
  layout_coords[i, 1] <- ideology_pos + runif(1, -0.3, 0.3)
  layout_coords[i, 2] <- runif(1, -1, 1)
}

# PRE-ELECTION network
V(g_pre)$color <- party_colors[V(g_pre)$party_type]
V(g_pre)$size <- pmax(8, sqrt(V(g_pre)$degree) * 4)

# Highlight strongest edges: 30% above mean weight
if(ecount(g_pre) > 0) {
  mean_weight_pre <- mean(E(g_pre)$weight)
  weight_threshold_pre <- mean_weight_pre * 1.30
  edges_to_show_pre <- which(E(g_pre)$weight >= weight_threshold_pre)
  
  # Set edge properties - stronger edges are more visible
  E(g_pre)$width <- pmax(0.5, (E(g_pre)$weight / max(E(g_pre)$weight)) * 3)
  E(g_pre)$color <- ifelse(E(g_pre)$weight >= weight_threshold_pre, 
                           rgb(0.5, 0.5, 0.5, 0.85),  # Prominent for strong edges
                           rgb(0.5, 0.5, 0.5, 0.08))  # Very faint for weaker edges
  
  cat(sprintf("  PRE: Highlighting %d/%d edges (30%% above mean of %.1f = threshold: %.1f)\n", 
              length(edges_to_show_pre), ecount(g_pre), mean_weight_pre, weight_threshold_pre))
} else {
  cat("  PRE: No edges to highlight\n")
}

plot(g_pre,
     layout = layout_coords[match(V(g_pre)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "PRE-ELECTION (3 MONTHS)\n(Aug 22, 2023 - Nov 21, 2023)")

# POST-FORMATION network
V(g_post)$color <- party_colors[V(g_post)$party_type]
V(g_post)$size <- pmax(8, sqrt(V(g_post)$degree) * 4)

# Highlight strongest edges: 30% above mean weight
if(ecount(g_post) > 0) {
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
} else {
  cat("  POST: No edges to highlight\n")
}

plot(g_post,
     layout = layout_coords[match(V(g_post)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "POST-FORMATION (3 MONTHS)\n(Jul 5, 2024 - Oct 4, 2024)")

dev.off()

# ============================================================================
# 2. DETAILED ANALYSIS
# ============================================================================

pdf("results/visualizations/detailed_three_month_pre_vs_post_formation_analysis.pdf", width = 16, height = 12)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))

# Degree distributions
hist(degree(g_pre), breaks = 20, col = "#E74C3C", border = "white",
     main = "Degree Distribution - Pre-Election (3 Months)", xlab = "Degree", ylab = "Frequency")
hist(degree(g_post), breaks = 20, col = "#3498DB", border = "white",
     main = "Degree Distribution - Post-Formation (3 Months)", xlab = "Degree", ylab = "Frequency")

# Community detection
if(ecount(g_pre) > 0) {
  communities_pre <- cluster_louvain(g_pre)
  plot(communities_pre, g_pre, 
       layout = layout_coords[match(V(g_pre)$name, all_parties_for_layout), ],
       vertex.label.cex = 0.6, main = "Communities - Pre-Election")
} else {
  plot.new()
  text(0.5, 0.5, "No communities\n(empty network)", cex = 1.5)
}

if(ecount(g_post) > 0) {
  communities_post <- cluster_louvain(g_post)
  plot(communities_post, g_post,
       layout = layout_coords[match(V(g_post)$name, all_parties_for_layout), ],
       vertex.label.cex = 0.6, main = "Communities - Post-Formation")
} else {
  plot.new()
  text(0.5, 0.5, "No communities\n(empty network)", cex = 1.5)
}

# Party type comparison
party_type_pre <- table(V(g_pre)$party_type)
party_type_post <- table(V(g_post)$party_type)

barplot(party_type_pre, col = party_colors[names(party_type_pre)],
        main = "Party Ideology Distribution - Pre-Election",
        ylab = "Number of Parties", las = 2)

barplot(party_type_post, col = party_colors[names(party_type_post)],
        main = "Party Ideology Distribution - Post-Formation", 
        ylab = "Number of Parties", las = 2)

dev.off()

# ============================================================================
# 3. CHANGE ANALYSIS VISUALIZATION
# ============================================================================

pdf("results/visualizations/network_changes_three_month_pre_vs_post_formation.pdf", width = 16, height = 10)
par(mfrow = c(2, 2), mar = c(8, 4, 3, 2))

# Network metrics comparison
metrics_data <- comparison_df[comparison_df$Metric %in% c("Edges", "Density", "Mean Degree", "Components"), ]
metrics_data <- metrics_data[, c("Metric", "Pre", "Post")]

metrics_matrix <- as.matrix(metrics_data[, -1])
rownames(metrics_matrix) <- metrics_data$Metric

barplot(t(metrics_matrix), beside = TRUE, col = c("#E74C3C", "#3498DB"),
        main = "Network Metrics Comparison (3 Month Periods)",
        ylab = "Value", las = 2,
        legend.text = c("Pre-Election", "Post-Formation"),
        args.legend = list(x = "topright"))

# Percent change visualization
valid_changes <- comparison_df[is.finite(comparison_df$PctChange_Pre_to_Post), ]
if(nrow(valid_changes) > 0) {
  barplot(valid_changes$PctChange_Pre_to_Post,
          names.arg = valid_changes$Metric,
          col = ifelse(valid_changes$PctChange_Pre_to_Post > 0, "#2ECC71", "#E74C3C"),
          main = "Percent Change (Pre → Post-Formation)",
          ylab = "Percent Change (%)", las = 2)
  abline(h = 0, lty = 2)
} else {
  plot.new()
  text(0.5, 0.5, "No valid percent changes to display", cex = 1.5)
}

# Party activity comparison
party_activity_pre <- table(data_pre$ActorFractie)
party_activity_pre <- sort(party_activity_pre, decreasing = TRUE)[1:min(10, length(party_activity_pre))]

party_activity_post <- table(data_post$ActorFractie)
party_activity_post <- sort(party_activity_post, decreasing = TRUE)[1:min(10, length(party_activity_post))]

barplot(party_activity_pre, col = "#E74C3C", main = "Top 10 Most Active Parties - Pre-Election",
        ylab = "Number of Votes", las = 2, cex.names = 0.7)

barplot(party_activity_post, col = "#3498DB", main = "Top 10 Most Active Parties - Post-Formation",
        ylab = "Number of Votes", las = 2, cex.names = 0.7)

dev.off()

# ============================================================================
# EXPORT NETWORK DATA
# ============================================================================

cat("\nExporting network data...\n")

# Export edge lists
write.csv(igraph::as_data_frame(g_pre, "edges"), 
          "results/edge_lists/edges_three_month_pre_election.csv", row.names = FALSE)
write.csv(igraph::as_data_frame(g_post, "edges"), 
          "results/edge_lists/edges_three_month_post_formation.csv", row.names = FALSE)

# Export comparison statistics
write.csv(comparison_df, "results/statistics/three_month_pre_vs_post_formation_comparison.csv", row.names = FALSE)

cat("\n===============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("===============================================================================\n")
cat("Generated files:\n")
cat("  1. network_comparison_three_month_pre_vs_post_formation.pdf - Side-by-side comparison\n")
cat("  2. detailed_three_month_pre_vs_post_formation_analysis.pdf - Detailed network analysis\n")
cat("  3. network_changes_three_month_pre_vs_post_formation.pdf - Change metrics visualization\n")
cat("  4. edges_three_month_pre_election.csv - Edge list for pre-election period\n")
cat("  5. edges_three_month_post_formation.csv - Edge list for post-formation period\n")
cat("  6. three_month_pre_vs_post_formation_comparison.csv - Statistical comparison\n")
cat("===============================================================================\n")

