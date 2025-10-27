# ============================================================================
# PRE-ELECTION vs POST-FORMATION NETWORK ANALYSIS: Z-Score Normalized
# Focus: Comparing RELATIVE cooperation patterns before election vs after formation
# ============================================================================

library(lubridate)
library(igraph)
library(ggplot2)

# ============================================================================
# RESEARCH DESIGN
# ============================================================================

cat("===============================================================================\n")
cat("PRE-ELECTION vs POST-FORMATION NETWORK ANALYSIS: Z-Score Normalized Networks\n")
cat("Focus: Comparing Cooperation PATTERNS (not absolute volumes)\n")
cat("===============================================================================\n\n")

cat("RESEARCH QUESTION:\n")
cat("Do co-voting PATTERNS between parties change from before election to after cabinet formation?\n\n")

cat("TEMPORAL DESIGN:\n")
cat("• PRE-ELECTION: November 22, 2022 - November 21, 2023 (1 year before election)\n")
cat("• POST-FORMATION: July 5, 2024 - July 4, 2025 (1 year after formation)\n")
cat("• Election Date: November 22, 2023\n")
cat("• Cabinet Formation: July 5, 2024\n\n")

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

# Period 1: PRE-ELECTION (1 year before election)
# November 22, 2022 - November 21, 2023
pre_start <- election_date - years(1)
pre_end <- election_date - days(1)

data_pre <- voting_data_2023[voting_data_2023$date >= pre_start & voting_data_2023$date <= pre_end, ]

cat(sprintf("PRE-ELECTION: %d votes, %d motions, %d parties\n", 
            nrow(data_pre), 
            length(unique(data_pre$Besluit_Id)),
            length(unique(data_pre$ActorFractie[!is.na(data_pre$ActorFractie)]))))

# Period 2: POST-FORMATION (1 year after cabinet formation)
# July 5, 2024 - July 4, 2025
post_start <- formation_date
post_end <- formation_date + years(1)

data_post <- voting_data_2024[voting_data_2024$date >= post_start & voting_data_2024$date <= post_end, ]

cat(sprintf("POST-FORMATION: %d votes, %d motions, %d parties\n\n", 
            nrow(data_post), 
            length(unique(data_post$Besluit_Id)),
            length(unique(data_post$ActorFractie[!is.na(data_post$ActorFractie)]))))

# ============================================================================
# NETWORK CREATION FUNCTIONS WITH Z-SCORE NORMALIZATION
# ============================================================================

calculate_party_agreements <- function(data) {
  # Create party-motion voting matrix using base R
  party_votes <- data[, c("ActorFractie", "Besluit_Id", "Soort")]
  party_votes <- party_votes[!duplicated(party_votes), ]
  
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
  
  # Add Z-SCORE NORMALIZATION
  if(nrow(agreements_df) > 0) {
    mean_weight <- mean(agreements_df$agreements)
    sd_weight <- sd(agreements_df$agreements)
    
    agreements_df$z_score <- (agreements_df$agreements - mean_weight) / sd_weight
    agreements_df$raw_weight <- agreements_df$agreements
    
    cat(sprintf("  Party pairs: %d\n", nrow(agreements_df)))
    cat(sprintf("  Raw weight - Mean: %.1f, SD: %.1f\n", mean_weight, sd_weight))
    cat(sprintf("  Z-score range: %.2f to %.2f\n", 
                min(agreements_df$z_score), 
                max(agreements_df$z_score)))
    cat(sprintf("  Strong ties (z > 1): %d (%.1f%%)\n\n", 
                sum(agreements_df$z_score > 1),
                100 * mean(agreements_df$z_score > 1)))
  } else {
    cat("  No party pairs found\n\n")
  }
  
  return(agreements_df)
}

create_normalized_network <- function(agreements, all_parties, period_name) {
  if(nrow(agreements) == 0) {
    # Create empty network
    g <- make_empty_graph(n = length(all_parties), directed = FALSE)
    V(g)$name <- all_parties
    return(g)
  }
  
  # Use Z-SCORE as weight
  edges <- data.frame(
    from = agreements$ActorFractie_1,
    to = agreements$ActorFractie_2,
    z_score = agreements$z_score,
    raw_weight = agreements$agreements,
    agreement_rate = agreements$agreement_rate,
    total_votes = agreements$total_votes
  )
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = all_parties)
  
  # Set z-score as the weight
  E(g)$weight <- edges$z_score
  E(g)$raw_weight <- edges$raw_weight
  E(g)$agreement_rate <- edges$agreement_rate
  
  # Add network attributes
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
    ifelse(party_names %in% c("ChristenUnie", "50PLUS", "Volt", "D66", "NSC", "Omtzigt"), 2,
    ifelse(party_names %in% c("BBB", "PVV", "CDA"), 3,
    ifelse(party_names %in% c("VVD", "SGP"), 4, 5)))
  )
  
  cat(sprintf("%s Network Created:\n", period_name))
  cat(sprintf("  Nodes: %d, Edges: %d\n", vcount(g), ecount(g)))
  if(ecount(g) > 0) {
    cat(sprintf("  Z-score weight range: %.2f to %.2f\n", 
                min(E(g)$weight), max(E(g)$weight)))
  }
  cat(sprintf("  Density: %.3f\n\n", edge_density(g)))
  
  return(g)
}

# ============================================================================
# CALCULATE AGREEMENTS AND CREATE NETWORKS
# ============================================================================

cat("CALCULATING PARTY AGREEMENTS (with z-scores):\n")
cat("===============================================\n\n")

cat("PRE-ELECTION:\n")
agreements_pre <- calculate_party_agreements(data_pre)

cat("POST-FORMATION:\n")
agreements_post <- calculate_party_agreements(data_post)

# Get parties for each period
parties_pre <- unique(data_pre$ActorFractie)
parties_pre <- parties_pre[!is.na(parties_pre)]
parties_post <- unique(data_post$ActorFractie)
parties_post <- parties_post[!is.na(parties_post)]

# Create networks
g_pre <- create_normalized_network(agreements_pre, parties_pre, "PRE")
g_post <- create_normalized_network(agreements_post, parties_post, "POST")

# ============================================================================
# VISUALIZATION SETUP (MATCHING ORIGINAL EXACTLY)
# ============================================================================

party_colors <- c("Left" = "#E74C3C", "Center" = "#F39C12", "Right" = "#3498DB")

# For visualization consistency, use union of all parties
all_parties_for_layout <- unique(c(parties_pre, parties_post))

# Create ideology-based layout (EXACTLY matching original)
set.seed(42)  # Same seed as original for exact reproducibility
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

# ============================================================================
# VISUALIZATION 1: NORMALIZED NETWORK COMPARISON
# ============================================================================

cat("Creating normalized network visualizations...\n")

pdf("results/visualizations/network_comparison_normalized_pre_vs_post_formation.pdf", width = 16, height = 8)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 2))

# Color and size nodes (matching original)
V(g_pre)$color <- party_colors[V(g_pre)$party_type]
V(g_pre)$size <- pmax(8, sqrt(V(g_pre)$degree) * 4)

V(g_post)$color <- party_colors[V(g_post)$party_type]
V(g_post)$size <- pmax(8, sqrt(V(g_post)$degree) * 4)

# PRE network
# Highlight edges with z > 1.0 (above average)
if(ecount(g_pre) > 0) {
  E(g_pre)$width <- pmax(0.3, (E(g_pre)$weight + 3) / 6 * 3)  # Scale z-scores for visibility
  E(g_pre)$color <- ifelse(E(g_pre)$weight > 1.0, 
                           rgb(0.3, 0.3, 0.3, 0.8),   # Strong: z > 1
                           rgb(0.5, 0.5, 0.5, 0.15))  # Weak: z <= 1
  
  strong_pre <- sum(E(g_pre)$weight > 1.0)
  cat(sprintf("  PRE: Highlighting %d/%d edges (z > 1.0)\n", strong_pre, ecount(g_pre)))
} else {
  cat("  PRE: No edges to highlight\n")
}

plot(g_pre,
     layout = layout_coords[match(V(g_pre)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "PRE-ELECTION\n(Nov 22, 2022 - Nov 21, 2023: Z-Score Normalized)")

# POST network
if(ecount(g_post) > 0) {
  E(g_post)$width <- pmax(0.3, (E(g_post)$weight + 3) / 6 * 3)
  E(g_post)$color <- ifelse(E(g_post)$weight > 1.0,
                            rgb(0.3, 0.3, 0.3, 0.8),
                            rgb(0.5, 0.5, 0.5, 0.15))
  
  strong_post <- sum(E(g_post)$weight > 1.0)
  cat(sprintf("  POST: Highlighting %d/%d edges (z > 1.0)\n\n", strong_post, ecount(g_post)))
} else {
  cat("  POST: No edges to highlight\n\n")
}

plot(g_post,
     layout = layout_coords[match(V(g_post)$name, all_parties_for_layout), ],
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.frame.color = "white",
     main = "POST-FORMATION\n(Jul 5, 2024 - Jul 4, 2025: Z-Score Normalized)")

dev.off()

# ============================================================================
# VISUALIZATION 2: COMPARISON OF RAW vs NORMALIZED
# ============================================================================

cat("Creating raw vs normalized comparison...\n")

pdf("results/visualizations/raw_vs_normalized_comparison_pre_vs_post_formation.pdf", width = 16, height = 8)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 2))

# Row 1: Raw weights
cat("  Plotting raw weight networks...\n")
plot_network_raw <- function(g, period_name, layout_coords, all_parties) {
  if(ecount(g) > 0) {
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
  } else {
    plot.new()
    text(0.5, 0.5, sprintf("%s\n(No edges)", period_name), cex = 1.5)
  }
}

plot_network_raw(g_pre, "PRE-ELECTION", layout_coords, all_parties_for_layout)
plot_network_raw(g_post, "POST-FORMATION", layout_coords, all_parties_for_layout)

dev.off()

# ============================================================================
# STATISTICAL COMPARISON
# ============================================================================

cat("Comparing network structures...\n\n")

# Export normalized edge lists
write.csv(igraph::as_data_frame(g_pre, "edges"), 
          "results/edge_lists/edges_normalized_pre_election.csv", row.names = FALSE)
write.csv(igraph::as_data_frame(g_post, "edges"), 
          "results/edge_lists/edges_normalized_post_formation.csv", row.names = FALSE)

# Compare strong edge patterns
comparison <- data.frame(
  Period = c("Pre", "Post"),
  Total_Edges = c(ecount(g_pre), ecount(g_post)),
  Strong_Edges_Z1 = c(
    ifelse(ecount(g_pre) > 0, sum(E(g_pre)$weight > 1.0), 0),
    ifelse(ecount(g_post) > 0, sum(E(g_post)$weight > 1.0), 0)
  ),
  Very_Strong_Z2 = c(
    ifelse(ecount(g_pre) > 0, sum(E(g_pre)$weight > 2.0), 0),
    ifelse(ecount(g_post) > 0, sum(E(g_post)$weight > 2.0), 0)
  ),
  Pct_Strong = c(
    ifelse(ecount(g_pre) > 0, 100 * mean(E(g_pre)$weight > 1.0), 0),
    ifelse(ecount(g_post) > 0, 100 * mean(E(g_post)$weight > 1.0), 0)
  ),
  Mean_Z = c(
    ifelse(ecount(g_pre) > 0, mean(E(g_pre)$weight), 0),
    ifelse(ecount(g_post) > 0, mean(E(g_post)$weight), 0)
  ),
  SD_Z = c(
    ifelse(ecount(g_pre) > 0, sd(E(g_pre)$weight), 0),
    ifelse(ecount(g_post) > 0, sd(E(g_post)$weight), 0)
  )
)

write.csv(comparison, "results/statistics/normalized_network_comparison_pre_vs_post_formation.csv", row.names = FALSE)

cat("NORMALIZED NETWORK COMPARISON:\n")
cat("==============================\n")
print(comparison)

cat("\n===============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("===============================================================================\n")
cat("Generated files:\n")
cat("  1. network_comparison_normalized_pre_vs_post_formation.pdf - Side-by-side z-score networks\n")
cat("  2. raw_vs_normalized_comparison_pre_vs_post_formation.pdf - Raw vs normalized comparison\n")
cat("  3. normalized_network_comparison_pre_vs_post_formation.csv - Statistical comparison\n")
cat("  4. edges_normalized_*.csv - Z-score edge lists for each period\n")
cat("\nInterpretation:\n")
cat("  • Z-scores show RELATIVE cooperation strength within each period\n")
cat("  • z > 1.0 = above-average cooperation (top ~16%)\n")
cat("  • z > 2.0 = very strong cooperation (top ~2%)\n")
cat("  • Compare % of strong edges across periods to see pattern changes\n")
cat("===============================================================================\n")