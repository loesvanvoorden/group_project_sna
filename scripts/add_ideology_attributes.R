# ==============================================================================
# ADD IDEOLOGY NODE ATTRIBUTES TO NETWORKS
# Helper script to load Kieskompas ideology data as network node attributes
# ==============================================================================

library(dplyr)
library(igraph)

# Load ideology data from Kieskompas 2023
ideology_data <- read.csv("data/political_axes_data.csv", stringsAsFactors = FALSE)

# Clean column names (remove trailing commas if any)
colnames(ideology_data) <- c("left_right", "conservative_progressive", "party")

# Display the data
cat("===============================================\n")
cat("IDEOLOGY DATA FROM KIESKOMPAS 2023\n")
cat("===============================================\n\n")

cat("Dimensions:\n")
cat("  • Left-Right: -1 (left) to +1 (right)\n")
cat("  • Conservative-Progressive: -1 (conservative) to +1 (progressive)\n\n")

print(ideology_data)

cat("\n===============================================\n")
cat("SUMMARY STATISTICS\n")
cat("===============================================\n\n")

cat(sprintf("Total parties: %d\n", nrow(ideology_data)))
cat(sprintf("Left-Right range: [%.3f, %.3f]\n", 
            min(ideology_data$left_right, na.rm = TRUE), 
            max(ideology_data$left_right, na.rm = TRUE)))
cat(sprintf("Conservative-Progressive range: [%.3f, %.3f]\n\n", 
            min(ideology_data$conservative_progressive, na.rm = TRUE), 
            max(ideology_data$conservative_progressive, na.rm = TRUE)))

# ==============================================================================
# FUNCTION: Add ideology attributes to a network
# ==============================================================================

add_ideology_to_network <- function(network, ideology_df = ideology_data) {
  #' Add ideology node attributes to an igraph network
  #' 
  #' @param network An igraph network object with party names as vertex names
  #' @param ideology_df Data frame with columns: left_right, conservative_progressive, party
  #' @return Network with added vertex attributes: ideology_lr, ideology_cp
  
  # Match parties in network to ideology data
  party_names <- V(network)$name
  
  # Initialize attributes with NA
  V(network)$ideology_lr <- NA
  V(network)$ideology_cp <- NA
  
  # Fill in ideology values for matched parties
  for (i in 1:length(party_names)) {
    party <- party_names[i]
    match_idx <- which(ideology_df$party == party)
    
    if (length(match_idx) > 0) {
      V(network)$ideology_lr[i] <- ideology_df$left_right[match_idx]
      V(network)$ideology_cp[i] <- ideology_df$conservative_progressive[match_idx]
    }
  }
  
  # Report matches
  matched <- sum(!is.na(V(network)$ideology_lr))
  cat(sprintf("Matched %d/%d parties from network to ideology data\n", 
              matched, length(party_names)))
  
  if (matched < length(party_names)) {
    unmatched <- party_names[is.na(V(network)$ideology_lr)]
    cat("Unmatched parties:", paste(unmatched, collapse = ", "), "\n")
  }
  
  return(network)
}

# ==============================================================================
# FUNCTION: Calculate ideological distance matrix
# ==============================================================================

calculate_ideology_distance <- function(network) {
  #' Calculate pairwise ideological distance between parties
  #' Uses Euclidean distance on 2D ideology space
  #' 
  #' @param network igraph network with ideology_lr and ideology_cp attributes
  #' @return Matrix of ideological distances (dyadic)
  
  n <- vcount(network)
  dist_matrix <- matrix(NA, n, n)
  rownames(dist_matrix) <- V(network)$name
  colnames(dist_matrix) <- V(network)$name
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && !is.na(V(network)$ideology_lr[i]) && !is.na(V(network)$ideology_lr[j])) {
        lr_diff <- V(network)$ideology_lr[i] - V(network)$ideology_lr[j]
        cp_diff <- V(network)$ideology_cp[i] - V(network)$ideology_cp[j]
        dist_matrix[i, j] <- sqrt(lr_diff^2 + cp_diff^2)
      }
    }
  }
  
  return(dist_matrix)
}

# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================

cat("\n===============================================\n")
cat("EXAMPLE: Load ideology into a network\n")
cat("===============================================\n\n")

# Example: Create a simple network with party names
example_parties <- c("VVD", "PVV", "CDA", "D66", "GroenLinks-PvdA", "SP", "FVD")
example_network <- make_full_graph(length(example_parties), directed = FALSE)
V(example_network)$name <- example_parties

cat("Before adding ideology:\n")
cat("Vertex attributes:", paste(names(vertex_attr(example_network)), collapse = ", "), "\n\n")

# Add ideology attributes
example_network <- add_ideology_to_network(example_network)

cat("\nAfter adding ideology:\n")
cat("Vertex attributes:", paste(names(vertex_attr(example_network)), collapse = ", "), "\n\n")

# Display ideology for each party
cat("Party ideologies:\n")
for (i in 1:vcount(example_network)) {
  cat(sprintf("  %s: L-R=%.3f, C-P=%.3f\n", 
              V(example_network)$name[i],
              V(example_network)$ideology_lr[i],
              V(example_network)$ideology_cp[i]))
}

# Calculate ideological distances
cat("\n===============================================\n")
cat("EXAMPLE: Calculate ideological distances\n")
cat("===============================================\n\n")

ideology_dist <- calculate_ideology_distance(example_network)

cat("Sample pairwise distances:\n")
cat(sprintf("  VVD ↔ PVV: %.3f\n", ideology_dist["VVD", "PVV"]))
cat(sprintf("  VVD ↔ GroenLinks-PvdA: %.3f\n", ideology_dist["VVD", "GroenLinks-PvdA"]))
cat(sprintf("  D66 ↔ GroenLinks-PvdA: %.3f\n", ideology_dist["D66", "GroenLinks-PvdA"]))

cat("\n===============================================\n")
cat("USAGE IN YOUR ANALYSIS\n")
cat("===============================================\n\n")

cat("For Study 1 (MRQAP), use this script as follows:\n\n")
cat("1. Source this script:\n")
cat("   source('scripts/add_ideology_attributes.R')\n\n")
cat("2. Add ideology to your network:\n")
cat("   g <- add_ideology_to_network(g, ideology_data)\n\n")
cat("3. Calculate distance matrix:\n")
cat("   ideology_dist_matrix <- calculate_ideology_distance(g)\n\n")
cat("4. Use in MRQAP as explanatory variable:\n")
cat("   # ideology_dist_matrix becomes your node attribute matrix\n\n")

cat("===============================================\n")
cat("DATA READY FOR STUDY 1 ANALYSIS\n")
cat("===============================================\n")

