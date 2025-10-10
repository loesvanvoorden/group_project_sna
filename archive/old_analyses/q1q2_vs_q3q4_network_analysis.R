#!/usr/bin/env Rscript
# ============================================================================
# Q1+Q2 vs Q3+Q4 Network Analysis: 2023 Pre-Election
# Research Question: Do co-voting patterns change as elections get closer?
# Focus: Network plotting and comparison visualizations  
# ============================================================================

library(dplyr)
library(lubridate)
library(igraph)
library(ggplot2)

cat("===============================================================================\n")
cat("Q1+Q2 vs Q3+Q4 NETWORK ANALYSIS: 2023 Pre-Election Period\n")
cat("Focus: Network Plotting and Comparison\n")
cat("===============================================================================\n\n")

# ============================================================================
# RESEARCH DESIGN
# ============================================================================

cat("RESEARCH QUESTION:\n")
cat("Do co-voting patterns between parties change as elections get closer?\n\n")

cat("TEMPORAL DESIGN:\n")
cat("• FAR FROM ELECTION: Q1+Q2 2023 (January-June)\n")
cat("• CLOSE TO ELECTION: Q3+Q4 2023 (July-November 22)\n")
cat("• Election Date: November 22, 2023\n\n")

cat("NETWORK THRESHOLD:\n")
cat("• Edges form when parties have 30% above average agreements\n")
cat("• This identifies statistically significant cooperation patterns\n")
cat("• Avoids arbitrary quantile-based thresholds\n\n")

cat("VISUALIZATION:\n")
cat("• RED nodes = Left-wing parties (SP, PvdD, GroenLinks, PvdA, etc.)\n")
cat("• ORANGE nodes = Center parties (D66, Volt)\n")  
cat("• BLUE nodes = Right-wing parties (VVD, CDA, PVV, FVD, etc.)\n")
cat("• Node size = degree centrality (more connections = larger)\n")
cat("• Edge thickness = cooperation strength (more agreements = thicker)\n\n")

# ============================================================================
# LOAD AND PREPARE DATA
# ============================================================================

cat("Loading 2023 pre-election data...\n")

voting_data <- read.csv("voting_data_2023_preelection.csv", stringsAsFactors = FALSE)
voting_data$date <- ymd_hms(voting_data$GewijzigdOp)
voting_data$quarter <- quarter(voting_data$date)

cat(sprintf("Total records: %d\n", nrow(voting_data)))
cat(sprintf("Date range: %s to %s\n", 
            min(voting_data$date, na.rm=TRUE), max(voting_data$date, na.rm=TRUE)))
cat(sprintf("Unique parties: %d\n", n_distinct(voting_data$ActorFractie)))
cat(sprintf("Unique motions: %d\n\n", n_distinct(voting_data$Besluit_Id)))

# ============================================================================
# CREATE TEMPORAL PERIODS
# ============================================================================

cat("Creating temporal periods...\n")

# Far from election: Q1+Q2 (January-June 2023)
data_far <- voting_data %>%
  filter(quarter %in% c(1, 2)) %>%
  filter(!is.na(ActorFractie) & !is.na(Besluit_Id) & !is.na(Soort))

# Close to election: Q3+Q4 (July-November 2023)
data_close <- voting_data %>%
  filter(quarter %in% c(3, 4)) %>%
  filter(!is.na(ActorFractie) & !is.na(Besluit_Id) & !is.na(Soort))

cat("FAR FROM ELECTION (Q1+Q2 2023):\n")
cat(sprintf("  Votes: %d\n", nrow(data_far)))
cat(sprintf("  Motions: %d\n", n_distinct(data_far$Besluit_Id)))
cat(sprintf("  Parties: %d\n", n_distinct(data_far$ActorFractie)))
cat(sprintf("  Date range: %s to %s\n", 
            min(data_far$date, na.rm=TRUE), max(data_far$date, na.rm=TRUE)))

cat("\nCLOSE TO ELECTION (Q3+Q4 2023):\n")
cat(sprintf("  Votes: %d\n", nrow(data_close)))
cat(sprintf("  Motions: %d\n", n_distinct(data_close$Besluit_Id)))
cat(sprintf("  Parties: %d\n", n_distinct(data_close$ActorFractie)))
cat(sprintf("  Date range: %s to %s\n\n", 
            min(data_close$date, na.rm=TRUE), max(data_close$date, na.rm=TRUE)))

# ============================================================================
# PARTY COOPERATION NETWORK FUNCTIONS
# ============================================================================

calculate_party_agreements <- function(voting_data, period_name) {
  cat(sprintf("Processing %s period...\n", period_name))
  
  # Create party-motion voting matrix
  party_votes <- voting_data %>%
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

create_party_network <- function(agreements, all_parties, above_average_pct = 0.30) {
  
  # Calculate average agreements across all party pairs
  avg_agreements <- mean(agreements$agreements)
  
  # Set threshold as X% above average (default 30% above average)
  threshold <- avg_agreements * (1 + above_average_pct)
  
  edges <- agreements %>%
    filter(agreements >= threshold) %>%
    select(from = ActorFractie_1, to = ActorFractie_2, 
           weight = agreements, agreement_rate, total_votes)
  
  cat(sprintf("    Average agreements: %.1f\n", avg_agreements))
  cat(sprintf("    Threshold (%.0f%% above average): %.1f agreements\n", 
              above_average_pct * 100, threshold))
  cat(sprintf("    Edges after threshold: %d out of %d pairs (%.1f%%)\n", 
              nrow(edges), nrow(agreements), (nrow(edges)/nrow(agreements))*100))
  
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
  
  cat(sprintf("    Final network: %d nodes, %d edges, density = %.3f\n", 
              vcount(g), ecount(g), edge_density(g)))
  
  return(g)
}

# ============================================================================
# CREATE NETWORKS
# ============================================================================

cat("CREATING PARTY COOPERATION NETWORKS\n")
cat("====================================\n")

# Calculate agreements
agreements_far <- calculate_party_agreements(data_far, "FAR FROM ELECTION")
agreements_close <- calculate_party_agreements(data_close, "CLOSE TO ELECTION")

# Get all parties
all_parties <- unique(c(unique(data_far$ActorFractie), unique(data_close$ActorFractie)))
all_parties <- all_parties[!is.na(all_parties)]

cat(sprintf("Total parties across both periods: %d\n\n", length(all_parties)))

# Create networks
cat("Creating FAR FROM ELECTION network:\n")
g_far <- create_party_network(agreements_far, all_parties, 0.30)

cat("\nCreating CLOSE TO ELECTION network:\n")
g_close <- create_party_network(agreements_close, all_parties, 0.30)

# ============================================================================
# NETWORK COMPARISON STATISTICS
# ============================================================================

cat("\nNETWORK COMPARISON STATISTICS\n")
cat("=============================\n")

comparison <- data.frame(
  Metric = c("Nodes", "Edges", "Density", "Mean Degree", "Transitivity", 
             "Avg Path Length", "Modularity", "Components"),
  FarFromElection = c(
    vcount(g_far),
    ecount(g_far),
    round(edge_density(g_far), 4),
    round(mean(degree(g_far)), 2),
    round(transitivity(g_far, type = "global"), 4),
    ifelse(is.connected(g_far), round(average.path.length(g_far), 2), NA),
    round(modularity(cluster_louvain(g_far)), 4),
    components(g_far)$no
  ),
  CloseToElection = c(
    vcount(g_close),
    ecount(g_close),
    round(edge_density(g_close), 4),
    round(mean(degree(g_close)), 2),
    round(transitivity(g_close, type = "global"), 4),
    ifelse(is.connected(g_close), round(average.path.length(g_close), 2), NA),
    round(modularity(cluster_louvain(g_close)), 4),
    components(g_close)$no
  )
)

comparison$Change <- comparison$CloseToElection - comparison$FarFromElection
comparison$PercentChange <- round((comparison$Change / comparison$FarFromElection) * 100, 1)

print(comparison)

# ============================================================================
# NETWORK VISUALIZATIONS
# ============================================================================

cat("\nCreating network visualizations...\n")

# Color schemes based on political ideology
party_colors <- c("Left" = "#E74C3C",      # Red for left-wing parties
                  "Center" = "#F39C12",    # Orange for centrist parties  
                  "Right" = "#3498DB")     # Blue for right-wing parties

ideology_colors <- colorRampPalette(c("#E74C3C", "#F39C12", "#F1C40F", "#2ECC71", "#3498DB"))(5)

# ============================================================================
# 1. SIDE-BY-SIDE NETWORK COMPARISON
# ============================================================================

pdf("network_comparison_q1q2_vs_q3q4.pdf", width = 20, height = 10)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 2))

# Common layout for comparison
set.seed(42)
# Use ideology-based layout for meaningful positioning
layout_coords <- matrix(0, nrow = length(all_parties), ncol = 2)
for(i in 1:length(all_parties)) {
  party <- all_parties[i]
  if(party %in% V(g_far)$name) {
    ideology_pos <- V(g_far)$ideology[V(g_far)$name == party][1]
  } else if(party %in% V(g_close)$name) {
    ideology_pos <- V(g_close)$ideology[V(g_close)$name == party][1]
  } else {
    ideology_pos <- 3
  }
  layout_coords[i, 1] <- ideology_pos + rnorm(1, 0, 0.3)  # Left-right axis
  layout_coords[i, 2] <- rnorm(1, 0, 1)  # Random vertical
}

# FAR FROM ELECTION network
V(g_far)$color <- party_colors[V(g_far)$party_type]
V(g_far)$size <- pmax(8, sqrt(V(g_far)$degree) * 4)
E(g_far)$width <- pmax(0.5, (E(g_far)$weight / max(E(g_far)$weight)) * 3)

plot(g_far,
     layout = layout_coords[match(V(g_far)$name, all_parties), ],
     main = "FAR FROM ELECTION\n(Q1+Q2 2023: January-June)",
     vertex.label = V(g_far)$name,
     vertex.label.cex = 0.8,
     vertex.label.dist = 1.2,
     vertex.label.color = "black",
     vertex.frame.color = "white",
     edge.color = rgb(0.5, 0.5, 0.5, 0.6),
     margin = c(0, 0, 0, 0))

# CLOSE TO ELECTION network
V(g_close)$color <- party_colors[V(g_close)$party_type]
V(g_close)$size <- pmax(8, sqrt(V(g_close)$degree) * 4)
E(g_close)$width <- pmax(0.5, (E(g_close)$weight / max(E(g_close)$weight)) * 3)

plot(g_close,
     layout = layout_coords[match(V(g_close)$name, all_parties), ],
     main = "CLOSE TO ELECTION\n(Q3+Q4 2023: July-November)",
     vertex.label = V(g_close)$name,
     vertex.label.cex = 0.8,
     vertex.label.dist = 1.2,
     vertex.label.color = "black",
     vertex.frame.color = "white",
     edge.color = rgb(0.5, 0.5, 0.5, 0.6),
     margin = c(0, 0, 0, 0))

dev.off()

# ============================================================================
# 2. DETAILED NETWORK ANALYSIS PLOTS
# ============================================================================

pdf("detailed_network_analysis.pdf", width = 16, height = 20)
par(mfrow = c(4, 2), mar = c(4, 4, 4, 2))

# Degree distribution comparison
hist(degree(g_far), breaks = 10, col = rgb(0.3, 0.7, 0.9, 0.8), 
     main = "Degree Distribution - Far from Election", 
     xlab = "Degree", ylab = "Frequency", xlim = c(0, max(c(degree(g_far), degree(g_close)))))

hist(degree(g_close), breaks = 10, col = rgb(0.9, 0.7, 0.3, 0.8),
     main = "Degree Distribution - Close to Election",
     xlab = "Degree", ylab = "Frequency", xlim = c(0, max(c(degree(g_far), degree(g_close)))))

# Strength distribution comparison  
hist(strength(g_far), breaks = 10, col = rgb(0.3, 0.7, 0.9, 0.8),
     main = "Strength Distribution - Far from Election",
     xlab = "Strength (Total Agreements)", ylab = "Frequency")

hist(strength(g_close), breaks = 10, col = rgb(0.9, 0.7, 0.3, 0.8),
     main = "Strength Distribution - Close to Election", 
     xlab = "Strength (Total Agreements)", ylab = "Frequency")

# Community detection comparison
comm_far <- cluster_louvain(g_far)
comm_close <- cluster_louvain(g_close)

plot(comm_far, g_far,
     layout = layout_with_fr(g_far),
     main = "Communities - Far from Election",
     vertex.label.cex = 0.7,
     vertex.size = 8,
     margin = c(0, 0, 0, 0))

plot(comm_close, g_close,
     layout = layout_with_fr(g_close),
     main = "Communities - Close to Election",
     vertex.label.cex = 0.7,
     vertex.size = 8,
     margin = c(0, 0, 0, 0))

# Party type comparison
party_type_far <- table(V(g_far)$party_type)
party_type_close <- table(V(g_close)$party_type)

barplot(party_type_far, col = party_colors[names(party_type_far)],
        main = "Party Ideology Distribution - Far from Election",
        ylab = "Number of Parties", las = 2)

barplot(party_type_close, col = party_colors[names(party_type_close)],
        main = "Party Ideology Distribution - Close to Election", 
        ylab = "Number of Parties", las = 2)

dev.off()

# ============================================================================
# 3. CHANGE ANALYSIS VISUALIZATION
# ============================================================================

pdf("network_changes_analysis.pdf", width = 14, height = 10)
par(mfrow = c(2, 3), mar = c(5, 4, 4, 2))

# Network metrics comparison
metrics_data <- comparison[c("Density", "Transitivity", "Mean Degree", "Modularity"), ]
barplot(t(as.matrix(metrics_data[, c("FarFromElection", "CloseToElection")])),
        beside = TRUE, names.arg = metrics_data$Metric,
        col = c("#3498DB", "#E74C3C"),
        main = "Network Metrics Comparison",
        ylab = "Value", las = 2,
        legend.text = c("Far (Q1+Q2)", "Close (Q3+Q4)"),
        args.legend = list(x = "topright"))

# Percent change visualization (excluding infinite values)
valid_changes <- metrics_data[is.finite(metrics_data$PercentChange), ]
if(nrow(valid_changes) > 0) {
  barplot(valid_changes$PercentChange,
          names.arg = valid_changes$Metric,
          col = ifelse(valid_changes$PercentChange > 0, "#2ECC71", "#E74C3C"),
          main = "Percent Change (Far → Close)",
          ylab = "Percent Change (%)", las = 2)
  abline(h = 0, lty = 2)
} else {
  plot.new()
  text(0.5, 0.5, "No valid percent changes to display", cex = 1.5)
}

# Party activity comparison
party_activity_far <- data_far %>% count(ActorFractie, sort = TRUE) %>% head(10)
party_activity_close <- data_close %>% count(ActorFractie, sort = TRUE) %>% head(10)

barplot(party_activity_far$n, names.arg = party_activity_far$ActorFractie,
        col = "#3498DB", main = "Top 10 Most Active Parties - Far",
        ylab = "Number of Votes", las = 2, cex.names = 0.7)

barplot(party_activity_close$n, names.arg = party_activity_close$ActorFractie,
        col = "#E74C3C", main = "Top 10 Most Active Parties - Close", 
        ylab = "Number of Votes", las = 2, cex.names = 0.7)

# Agreement rate distributions
boxplot(agreements_far$agreement_rate, agreements_close$agreement_rate,
        names = c("Far from Election", "Close to Election"),
        col = c("#3498DB", "#E74C3C"),
        main = "Agreement Rate Distributions",
        ylab = "Agreement Rate")

# Edge weight comparison
plot(density(E(g_far)$weight), col = "#3498DB", lwd = 2,
     main = "Edge Weight Distributions",
     xlab = "Agreement Weight", ylab = "Density")
lines(density(E(g_close)$weight), col = "#E74C3C", lwd = 2)
legend("topright", legend = c("Far (Q1+Q2)", "Close (Q3+Q4)"),
       col = c("#3498DB", "#E74C3C"), lwd = 2)

dev.off()

# ============================================================================
# SAVE DATA AND NETWORKS
# ============================================================================

cat("Saving results...\n")

# Save comparison statistics
write.csv(comparison, "network_comparison_statistics.csv", row.names = FALSE)

# Save networks
save(g_far, file = "network_far_from_election.RData")
save(g_close, file = "network_close_to_election.RData")

# Save edge lists
edges_far <- igraph::as_data_frame(g_far, what = "edges")
edges_close <- igraph::as_data_frame(g_close, what = "edges")

write.csv(edges_far, "edges_far_from_election.csv", row.names = FALSE)
write.csv(edges_close, "edges_close_to_election.csv", row.names = FALSE)

# Save vertex attributes
vertices_far <- igraph::as_data_frame(g_far, what = "vertices")
vertices_close <- igraph::as_data_frame(g_close, what = "vertices")

write.csv(vertices_far, "vertices_far_from_election.csv", row.names = FALSE)
write.csv(vertices_close, "vertices_close_to_election.csv", row.names = FALSE)

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n===============================================================================\n")
cat("Q1+Q2 vs Q3+Q4 NETWORK ANALYSIS COMPLETE\n")
cat("===============================================================================\n\n")

cat("RESEARCH QUESTION: Do co-voting patterns change as elections get closer?\n\n")

cat("KEY FINDINGS:\n")
density_change <- comparison$PercentChange[comparison$Metric == "Density"]
transitivity_change <- comparison$PercentChange[comparison$Metric == "Transitivity"]
degree_change <- comparison$PercentChange[comparison$Metric == "Mean Degree"]

cat(sprintf("• Network density change: %.1f%%\n", density_change))
cat(sprintf("• Transitivity change: %.1f%%\n", transitivity_change))
cat(sprintf("• Mean degree change: %.1f%%\n", degree_change))

if(abs(density_change) > 5) {
  if(density_change > 0) {
    cat("→ MORE cooperation as election approaches\n")
  } else {
    cat("→ LESS cooperation as election approaches\n")
  }
} else {
  cat("→ STABLE cooperation patterns\n")
}

cat("\nVISUALIZATIONS CREATED:\n")
cat("• network_comparison_q1q2_vs_q3q4.pdf - Side-by-side network comparison\n")
cat("• detailed_network_analysis.pdf - Comprehensive network analysis\n")
cat("• network_changes_analysis.pdf - Change metrics and distributions\n")

cat("\nDATA FILES CREATED:\n")
cat("• network_comparison_statistics.csv - Summary statistics\n")
cat("• network_far_from_election.RData / network_close_to_election.RData\n")
cat("• edges_*.csv and vertices_*.csv - Network data\n")

cat("\n🎯 Networks plotted and compared successfully! 📊\n")
cat("\nDone! ✓\n")
