# ============================================================================
# THREE-PERIOD NETWORK ANALYSIS: Z-Score Normalized (for Pattern Comparison)
# Focus: Comparing RELATIVE cooperation patterns across electoral cycle
# ============================================================================

library(dplyr)
library(lubridate)
library(igraph)
library(ggplot2)

# ============================================================================
# RESEARCH DESIGN
# ============================================================================

cat("===============================================================================\n")
cat("THREE-PERIOD NETWORK ANALYSIS: Z-Score Normalized Networks\n")
cat("Focus: Comparing Cooperation PATTERNS (not absolute volumes)\n")
cat("===============================================================================\n\n")

cat("RESEARCH QUESTION:\n")
cat("Do co-voting PATTERNS between parties change across the electoral cycle?\n\n")

cat("TEMPORAL DESIGN:\n")
cat("• FAR FROM ELECTION: Q1+Q2 2023 (January-June) - 6 months\n")
cat("• CLOSE TO ELECTION: Q3+Q4 2023 (July-November 22) - ~5 months\n")
cat("• POST FORMATION: Q3+Q4 2024 (July-December) - 6 months\n")
cat("• Election Date: November 22, 2023\n")
cat("• Cabinet Formation: July 2, 2024\n\n")

cat("NORMALIZATION APPROACH:\n")
cat("• Z-scores: (weight - mean) / sd within each period\n")
cat("• Makes periods directly comparable\n")
cat("• Shows RELATIVE cooperation strength (not absolute volume)\n")
cat("• Identifies structurally important ties in each period\n\n")

# ============================================================================
# LOAD AND PREPARE DATA
# ============================================================================

cat("Loading data...\n")

# Load pre-election data (2023)
voting_data_2023 <- read.csv("voting_data_2023_preelection.csv", stringsAsFactors = FALSE)
voting_data_2023$date <- ymd_hms(voting_data_2023$GewijzigdOp)
voting_data_2023$quarter <- quarter(voting_data_2023$date)

# Load post-election data (2024)
voting_data_2024 <- read.csv("voting_data_clean.csv", stringsAsFactors = FALSE)
voting_data_2024$date <- ymd_hms(voting_data_2024$GewijzigdOp)
voting_data_2024$quarter <- quarter(voting_data_2024$date)

cat(sprintf("Total 2023 records: %d\n", nrow(voting_data_2023)))
cat(sprintf("Total 2024 records: %d\n\n", nrow(voting_data_2024)))

# ============================================================================
# CREATE TEMPORAL PERIODS
# ============================================================================

cat("Creating temporal periods...\n")

# Period 1: FAR FROM ELECTION
data_far <- voting_data_2023 %>% filter(quarter %in% c(1, 2))
cat(sprintf("FAR: %d votes, %d motions, %d parties\n", 
            nrow(data_far), 
            length(unique(data_far$Besluit_Id)),
            length(unique(data_far$ActorFractie[!is.na(data_far$ActorFractie)]))))

# Period 2: CLOSE TO ELECTION
data_close <- voting_data_2023 %>% filter(quarter %in% c(3, 4))
cat(sprintf("CLOSE: %d votes, %d motions, %d parties\n", 
            nrow(data_close), 
            length(unique(data_close$Besluit_Id)),
            length(unique(data_close$ActorFractie[!is.na(data_close$ActorFractie)]))))

# Period 3: POST FORMATION
data_post <- voting_data_2024 %>% filter(year(date) == 2024, quarter %in% c(3, 4))
cat(sprintf("POST: %d votes, %d motions, %d parties\n\n", 
            nrow(data_post), 
            length(unique(data_post$Besluit_Id)),
            length(unique(data_post$ActorFractie[!is.na(data_post$ActorFractie)]))))

# ============================================================================
# NETWORK CREATION FUNCTIONS WITH Z-SCORE NORMALIZATION
# ============================================================================

calculate_party_agreements <- function(data) {
  party_votes <- data %>%
    select(ActorFractie, Besluit_Id, Soort) %>%
    distinct()
  
  agreements <- party_votes %>%
    inner_join(party_votes, by = "Besluit_Id", suffix = c("_1", "_2")) %>%
    filter(ActorFractie_1 < ActorFractie_2)
  
  party_agreement_summary <- agreements %>%
    group_by(ActorFractie_1, ActorFractie_2) %>%
    summarise(
      total_votes = n(),
      agreements = sum(Soort_1 == Soort_2),
      disagreements = sum(Soort_1 != Soort_2),
      agreement_rate = agreements / total_votes,
      .groups = 'drop'
    ) %>%
    filter(total_votes >= 5)
  
  # Add Z-SCORE NORMALIZATION
  mean_weight <- mean(party_agreement_summary$agreements)
  sd_weight <- sd(party_agreement_summary$agreements)
  
  party_agreement_summary <- party_agreement_summary %>%
    mutate(
      z_score = (agreements - mean_weight) / sd_weight,
      raw_weight = agreements
    )
  
  cat(sprintf("  Party pairs: %d\n", nrow(party_agreement_summary)))
  cat(sprintf("  Raw weight - Mean: %.1f, SD: %.1f\n", mean_weight, sd_weight))
  cat(sprintf("  Z-score range: %.2f to %.2f\n", 
              min(party_agreement_summary$z_score), 
              max(party_agreement_summary$z_score)))
  cat(sprintf("  Strong ties (z > 1): %d (%.1f%%)\n\n", 
              sum(party_agreement_summary$z_score > 1),
              100 * mean(party_agreement_summary$z_score > 1)))
  
  return(party_agreement_summary)
}

create_normalized_network <- function(agreements, all_parties, period_name) {
  # Use Z-SCORE as weight
  edges <- agreements %>%
    select(from = ActorFractie_1, to = ActorFractie_2, 
           z_score, raw_weight = agreements, agreement_rate, total_votes)
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = all_parties)
  
  # Set z-score as the weight
  E(g)$weight <- edges$z_score
  E(g)$raw_weight <- edges$raw_weight
  E(g)$agreement_rate <- edges$agreement_rate
  
  # Add network attributes
  V(g)$degree <- degree(g)
  V(g)$strength <- strength(g)
  V(g)$betweenness <- betweenness(g, weights = NA)
  
  # Party categories for coloring (matching original)
  V(g)$party_type <- case_when(
    V(g)$name %in% c("SP", "PvdD", "BIJ1", "GroenLinks", "PvdA", "DENK", "GroenLinks-PvdA") ~ "Left",
    V(g)$name %in% c("D66", "Volt") ~ "Center", 
    V(g)$name %in% c("VVD", "CDA", "ChristenUnie", "BBB", "PVV", "FVD", "SGP", "JA21", "NSC") ~ "Right",
    TRUE ~ "Center"
  )
  
  # Ideology for layout (matching original)
  V(g)$ideology <- case_when(
    V(g)$name %in% c("SP", "PvdD", "BIJ1") ~ 1,           # Left
    V(g)$name %in% c("GroenLinks", "PvdA", "DENK", "GroenLinks-PvdA") ~ 2,   # Center-left
    V(g)$name %in% c("D66", "Volt", "Omtzigt") ~ 3,       # Center
    V(g)$name %in% c("VVD", "CDA", "ChristenUnie", "BBB", "NSC") ~ 4,  # Center-right
    V(g)$name %in% c("PVV", "FVD", "SGP", "JA21") ~ 5,    # Right
    TRUE ~ 3  # Default center
  )
  
  cat(sprintf("%s Network Created:\n", period_name))
  cat(sprintf("  Nodes: %d, Edges: %d\n", vcount(g), ecount(g)))
  cat(sprintf("  Z-score weight range: %.2f to %.2f\n", 
              min(E(g)$weight), max(E(g)$weight)))
  cat(sprintf("  Density: %.3f\n\n", edge_density(g)))
  
  return(g)
}

# ============================================================================
# CALCULATE AGREEMENTS AND CREATE NETWORKS
# ============================================================================

cat("CALCULATING PARTY AGREEMENTS (with z-scores):\n")
cat("===============================================\n\n")

cat("FAR FROM ELECTION:\n")
agreements_far <- calculate_party_agreements(data_far)

cat("CLOSE TO ELECTION:\n")
agreements_close <- calculate_party_agreements(data_close)

cat("POST FORMATION:\n")
agreements_post <- calculate_party_agreements(data_post)

# Get parties for each period
parties_far <- unique(data_far$ActorFractie)
parties_far <- parties_far[!is.na(parties_far)]
parties_close <- unique(data_close$ActorFractie)
parties_close <- parties_close[!is.na(parties_close)]
parties_post <- unique(data_post$ActorFractie)
parties_post <- parties_post[!is.na(parties_post)]

# Create networks
g_far <- create_normalized_network(agreements_far, parties_far, "FAR")
g_close <- create_normalized_network(agreements_close, parties_close, "CLOSE")
g_post <- create_normalized_network(agreements_post, parties_post, "POST")

# Party attributes are now set within create_normalized_network() function

# ============================================================================
# VISUALIZATION SETUP (MATCHING ORIGINAL EXACTLY)
# ============================================================================

party_colors <- c("Left" = "#E74C3C", "Center" = "#F39C12", "Right" = "#3498DB")

# For visualization consistency, use union of all parties
all_parties_for_layout <- unique(c(parties_far, parties_close, parties_post))

# Create ideology-based layout (EXACTLY matching original)
set.seed(42)  # Same seed as original for exact reproducibility
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

# ============================================================================
# VISUALIZATION 1: NORMALIZED NETWORK COMPARISON
# ============================================================================

cat("Creating normalized network visualizations...\n")

pdf("network_comparison_normalized.pdf", width = 24, height = 8)  # Same size as original
par(mfrow = c(1, 3), mar = c(2, 2, 4, 2))

# Color and size nodes (matching original)
V(g_far)$color <- party_colors[V(g_far)$party_type]
V(g_far)$size <- pmax(8, sqrt(V(g_far)$degree) * 4)

V(g_close)$color <- party_colors[V(g_close)$party_type]
V(g_close)$size <- pmax(8, sqrt(V(g_close)$degree) * 4)

V(g_post)$color <- party_colors[V(g_post)$party_type]
V(g_post)$size <- pmax(8, sqrt(V(g_post)$degree) * 4)

# FAR network
# Highlight edges with z > 1.0 (above average)
E(g_far)$width <- pmax(0.3, (E(g_far)$weight + 3) / 6 * 3)  # Scale z-scores for visibility
E(g_far)$color <- ifelse(E(g_far)$weight > 1.0, 
                         rgb(0.3, 0.3, 0.3, 0.8),   # Strong: z > 1
                         rgb(0.5, 0.5, 0.5, 0.15))  # Weak: z <= 1

strong_far <- sum(E(g_far)$weight > 1.0)
cat(sprintf("  FAR: Highlighting %d/%d edges (z > 1.0)\n", strong_far, ecount(g_far)))

plot(g_far,
     layout = layout_coords[match(V(g_far)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "FAR FROM ELECTION\n(Q1+Q2 2023: Z-Score Normalized)")

# CLOSE network
E(g_close)$width <- pmax(0.3, (E(g_close)$weight + 3) / 6 * 3)
E(g_close)$color <- ifelse(E(g_close)$weight > 1.0,
                           rgb(0.3, 0.3, 0.3, 0.8),
                           rgb(0.5, 0.5, 0.5, 0.15))

strong_close <- sum(E(g_close)$weight > 1.0)
cat(sprintf("  CLOSE: Highlighting %d/%d edges (z > 1.0)\n", strong_close, ecount(g_close)))

plot(g_close,
     layout = layout_coords[match(V(g_close)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "CLOSE TO ELECTION\n(Q3+Q4 2023: Z-Score Normalized)")

# POST network
E(g_post)$width <- pmax(0.3, (E(g_post)$weight + 3) / 6 * 3)
E(g_post)$color <- ifelse(E(g_post)$weight > 1.0,
                          rgb(0.3, 0.3, 0.3, 0.8),
                          rgb(0.5, 0.5, 0.5, 0.15))

strong_post <- sum(E(g_post)$weight > 1.0)
cat(sprintf("  POST: Highlighting %d/%d edges (z > 1.0)\n\n", strong_post, ecount(g_post)))

plot(g_post,
     layout = layout_coords[match(V(g_post)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "POST FORMATION\n(Q3+Q4 2024: Z-Score Normalized)")

dev.off()

# ============================================================================
# VISUALIZATION 2: COMPARISON OF RAW vs NORMALIZED
# ============================================================================

cat("Creating raw vs normalized comparison...\n")

pdf("raw_vs_normalized_comparison.pdf", width = 16, height = 10)
par(mfrow = c(2, 3), mar = c(2, 2, 4, 2))

# Row 1: Raw weights
cat("  Plotting raw weight networks...\n")
plot_network_raw <- function(g, period_name, layout_coords, all_parties) {
  # Use raw weights for visualization
  E(g)$display_width <- pmax(0.5, (E(g)$raw_weight / max(E(g)$raw_weight)) * 3)
  mean_raw <- mean(E(g)$raw_weight)
  threshold <- mean_raw * 1.3
  E(g)$display_color <- ifelse(E(g)$raw_weight >= threshold,
                               rgb(0.3, 0.3, 0.3, 0.8),
                               rgb(0.5, 0.5, 0.5, 0.15))
  
  plot(g,
       layout = layout_coords[match(V(g)$name, all_parties), ],
       vertex.label.cex = 0.6,
       vertex.label.color = "black",
       vertex.frame.color = "white",
       edge.width = E(g)$display_width,
       edge.color = E(g)$display_color,
       main = sprintf("%s\n(Raw Weights)", period_name))
}

plot_network_raw(g_far, "FAR", layout_coords, all_parties_for_layout)
plot_network_raw(g_close, "CLOSE", layout_coords, all_parties_for_layout)
plot_network_raw(g_post, "POST", layout_coords, all_parties_for_layout)

# Row 2: Z-scores
cat("  Plotting z-score normalized networks...\n")
plot_network_zscore <- function(g, period_name, layout_coords, all_parties) {
  E(g)$display_width <- pmax(0.3, (E(g)$weight + 3) / 6 * 3)
  E(g)$display_color <- ifelse(E(g)$weight > 1.0,
                               rgb(0.3, 0.3, 0.3, 0.8),
                               rgb(0.5, 0.5, 0.5, 0.15))
  
  plot(g,
       layout = layout_coords[match(V(g)$name, all_parties), ],
       vertex.label.cex = 0.6,
       vertex.label.color = "black",
       vertex.frame.color = "white",
       edge.width = E(g)$display_width,
       edge.color = E(g)$display_color,
       main = sprintf("%s\n(Z-Score Normalized)", period_name))
}

plot_network_zscore(g_far, "FAR", layout_coords, all_parties_for_layout)
plot_network_zscore(g_close, "CLOSE", layout_coords, all_parties_for_layout)
plot_network_zscore(g_post, "POST", layout_coords, all_parties_for_layout)

dev.off()

# ============================================================================
# STATISTICAL COMPARISON
# ============================================================================

cat("Comparing network structures...\n\n")

# Export normalized edge lists
write.csv(igraph::as_data_frame(g_far, "edges"), 
          "edges_normalized_far.csv", row.names = FALSE)
write.csv(igraph::as_data_frame(g_close, "edges"), 
          "edges_normalized_close.csv", row.names = FALSE)
write.csv(igraph::as_data_frame(g_post, "edges"), 
          "edges_normalized_post.csv", row.names = FALSE)

# Compare strong edge patterns
comparison <- data.frame(
  Period = c("Far", "Close", "Post"),
  Total_Edges = c(ecount(g_far), ecount(g_close), ecount(g_post)),
  Strong_Edges_Z1 = c(
    sum(E(g_far)$weight > 1.0),
    sum(E(g_close)$weight > 1.0),
    sum(E(g_post)$weight > 1.0)
  ),
  Very_Strong_Z2 = c(
    sum(E(g_far)$weight > 2.0),
    sum(E(g_close)$weight > 2.0),
    sum(E(g_post)$weight > 2.0)
  ),
  Pct_Strong = c(
    100 * mean(E(g_far)$weight > 1.0),
    100 * mean(E(g_close)$weight > 1.0),
    100 * mean(E(g_post)$weight > 1.0)
  ),
  Mean_Z = c(
    mean(E(g_far)$weight),
    mean(E(g_close)$weight),
    mean(E(g_post)$weight)
  ),
  SD_Z = c(
    sd(E(g_far)$weight),
    sd(E(g_close)$weight),
    sd(E(g_post)$weight)
  )
)

write.csv(comparison, "normalized_network_comparison.csv", row.names = FALSE)

cat("NORMALIZED NETWORK COMPARISON:\n")
cat("==============================\n")
print(comparison)

cat("\n===============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("===============================================================================\n")
cat("Generated files:\n")
cat("  1. network_comparison_normalized.pdf - Side-by-side z-score networks\n")
cat("  2. raw_vs_normalized_comparison.pdf - Raw vs normalized comparison\n")
cat("  3. normalized_network_comparison.csv - Statistical comparison\n")
cat("  4. edges_normalized_*.csv - Z-score edge lists for each period\n")
cat("\nInterpretation:\n")
cat("  • Z-scores show RELATIVE cooperation strength within each period\n")
cat("  • z > 1.0 = above-average cooperation (top ~16%)\n")
cat("  • z > 2.0 = very strong cooperation (top ~2%)\n")
cat("  • Compare % of strong edges across periods to see pattern changes\n")
cat("===============================================================================\n")

