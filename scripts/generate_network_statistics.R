# ============================================================================
# COMPREHENSIVE NETWORK STATISTICS: Raw vs Normalized
# ============================================================================

library(dplyr)
library(lubridate)
library(igraph)

cat("===============================================================================\n")
cat("COMPREHENSIVE NETWORK STATISTICS COMPARISON\n")
cat("Raw Weights vs Z-Score Normalized\n")
cat("===============================================================================\n\n")

# Load data
voting_data_2023 <- read.csv("voting_data_2023_preelection.csv", stringsAsFactors = FALSE)
voting_data_2023$date <- ymd_hms(voting_data_2023$GewijzigdOp)
voting_data_2023$quarter <- quarter(voting_data_2023$date)

voting_data_2024 <- read.csv("voting_data_clean.csv", stringsAsFactors = FALSE)
voting_data_2024$date <- ymd_hms(voting_data_2024$GewijzigdOp)
voting_data_2024$quarter <- quarter(voting_data_2024$date)

# Create periods
data_far <- voting_data_2023 %>% filter(quarter %in% c(1, 2))
data_close <- voting_data_2023 %>% filter(quarter %in% c(3, 4))
data_post <- voting_data_2024 %>% filter(year(date) == 2024, quarter %in% c(3, 4))

# Calculate agreements function
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
  
  # Add z-scores
  mean_weight <- mean(party_agreement_summary$agreements)
  sd_weight <- sd(party_agreement_summary$agreements)
  
  party_agreement_summary <- party_agreement_summary %>%
    mutate(z_score = (agreements - mean_weight) / sd_weight)
  
  return(party_agreement_summary)
}

# Calculate agreements
agreements_far <- calculate_party_agreements(data_far)
agreements_close <- calculate_party_agreements(data_close)
agreements_post <- calculate_party_agreements(data_post)

# Get parties
parties_far <- unique(data_far$ActorFractie)
parties_far <- parties_far[!is.na(parties_far)]
parties_close <- unique(data_close$ActorFractie)
parties_close <- parties_close[!is.na(parties_close)]
parties_post <- unique(data_post$ActorFractie)
parties_post <- parties_post[!is.na(parties_post)]

# Create networks
create_network <- function(agreements, parties) {
  edges <- agreements %>%
    select(from = ActorFractie_1, to = ActorFractie_2, 
           raw_weight = agreements, z_score, agreement_rate)
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = parties)
  E(g)$raw_weight <- edges$raw_weight
  E(g)$z_score <- edges$z_score
  E(g)$agreement_rate <- edges$agreement_rate
  
  return(g)
}

g_far <- create_network(agreements_far, parties_far)
g_close <- create_network(agreements_close, parties_close)
g_post <- create_network(agreements_post, parties_post)

# ============================================================================
# PRINT COMPREHENSIVE STATISTICS
# ============================================================================

print_network_stats <- function(g, period_name) {
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat(sprintf("%s NETWORK\n", period_name))
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  cat("BASIC NETWORK STRUCTURE:\n")
  cat(sprintf("  Nodes: %d parties\n", vcount(g)))
  cat(sprintf("  Edges: %d cooperation ties\n", ecount(g)))
  cat(sprintf("  Density: %.3f (%.1f%%)\n", edge_density(g), edge_density(g)*100))
  cat(sprintf("  Components: %d separate groups\n", components(g)$no))
  cat(sprintf("  Mean Degree: %.2f connections per party\n", mean(degree(g))))
  
  cat("\nRAW WEIGHT STATISTICS:\n")
  cat(sprintf("  Mean Edge Weight: %.1f agreements\n", mean(E(g)$raw_weight)))
  cat(sprintf("  Median Edge Weight: %.1f agreements\n", median(E(g)$raw_weight)))
  cat(sprintf("  SD Edge Weight: %.1f agreements\n", sd(E(g)$raw_weight)))
  cat(sprintf("  Min Edge Weight: %.0f agreements\n", min(E(g)$raw_weight)))
  cat(sprintf("  Max Edge Weight: %.0f agreements\n", max(E(g)$raw_weight)))
  
  cat("\nZ-SCORE NORMALIZED STATISTICS:\n")
  cat(sprintf("  Mean Z-Score: %.3f (always ~0 by definition)\n", mean(E(g)$z_score)))
  cat(sprintf("  SD Z-Score: %.3f (always ~1 by definition)\n", sd(E(g)$z_score)))
  cat(sprintf("  Min Z-Score: %.2f\n", min(E(g)$z_score)))
  cat(sprintf("  Max Z-Score: %.2f\n", max(E(g)$z_score)))
  
  cat("\nSTRONG COOPERATION TIES (Z-SCORE THRESHOLDS):\n")
  strong_1 <- sum(E(g)$z_score > 1.0)
  strong_2 <- sum(E(g)$z_score > 2.0)
  cat(sprintf("  Above average (z > 1.0): %d edges (%.1f%%)\n", 
              strong_1, 100*strong_1/ecount(g)))
  cat(sprintf("  Very strong (z > 2.0): %d edges (%.1f%%)\n", 
              strong_2, 100*strong_2/ecount(g)))
  
  cat("\nAGREEMENT RATE STATISTICS:\n")
  cat(sprintf("  Mean Agreement Rate: %.1f%%\n", mean(E(g)$agreement_rate)*100))
  cat(sprintf("  Median Agreement Rate: %.1f%%\n", median(E(g)$agreement_rate)*100))
  
  cat("\nNETWORK COHESION:\n")
  cat(sprintf("  Transitivity (Clustering): %.3f\n", transitivity(g)))
  if(components(g)$no == 1) {
    cat(sprintf("  Average Path Length: %.2f\n", mean_distance(g)))
    cat(sprintf("  Diameter: %.0f\n", diameter(g)))
  } else {
    cat(sprintf("  Average Path Length: NA (disconnected network)\n"))
    cat(sprintf("  Diameter: NA (disconnected network)\n"))
  }
  
  cat("\n")
}

# Print all statistics
print_network_stats(g_far, "FAR FROM ELECTION (Q1+Q2 2023)")
print_network_stats(g_close, "CLOSE TO ELECTION (Q3+Q4 2023)")
print_network_stats(g_post, "POST FORMATION (Q3+Q4 2024)")

# ============================================================================
# CREATE COMPARISON TABLE
# ============================================================================

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SIDE-BY-SIDE COMPARISON\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

comparison <- data.frame(
  Metric = c(
    "Nodes (parties)",
    "Edges (ties)",
    "Density",
    "Components",
    "Mean Degree",
    "",
    "Mean Raw Weight",
    "Median Raw Weight",
    "SD Raw Weight",
    "",
    "Strong Ties (z>1)",
    "% Strong Ties",
    "Very Strong (z>2)",
    "",
    "Mean Agreement Rate"
  ),
  Far = c(
    vcount(g_far),
    ecount(g_far),
    round(edge_density(g_far), 3),
    components(g_far)$no,
    round(mean(degree(g_far)), 2),
    "",
    round(mean(E(g_far)$raw_weight), 1),
    round(median(E(g_far)$raw_weight), 1),
    round(sd(E(g_far)$raw_weight), 1),
    "",
    sum(E(g_far)$z_score > 1.0),
    paste0(round(100*mean(E(g_far)$z_score > 1.0), 1), "%"),
    sum(E(g_far)$z_score > 2.0),
    "",
    paste0(round(mean(E(g_far)$agreement_rate)*100, 1), "%")
  ),
  Close = c(
    vcount(g_close),
    ecount(g_close),
    round(edge_density(g_close), 3),
    components(g_close)$no,
    round(mean(degree(g_close)), 2),
    "",
    round(mean(E(g_close)$raw_weight), 1),
    round(median(E(g_close)$raw_weight), 1),
    round(sd(E(g_close)$raw_weight), 1),
    "",
    sum(E(g_close)$z_score > 1.0),
    paste0(round(100*mean(E(g_close)$z_score > 1.0), 1), "%"),
    sum(E(g_close)$z_score > 2.0),
    "",
    paste0(round(mean(E(g_close)$agreement_rate)*100, 1), "%")
  ),
  Post = c(
    vcount(g_post),
    ecount(g_post),
    round(edge_density(g_post), 3),
    components(g_post)$no,
    round(mean(degree(g_post)), 2),
    "",
    round(mean(E(g_post)$raw_weight), 1),
    round(median(E(g_post)$raw_weight), 1),
    round(sd(E(g_post)$raw_weight), 1),
    "",
    sum(E(g_post)$z_score > 1.0),
    paste0(round(100*mean(E(g_post)$z_score > 1.0), 1), "%"),
    sum(E(g_post)$z_score > 2.0),
    "",
    paste0(round(mean(E(g_post)$agreement_rate)*100, 1), "%")
  )
)

print(comparison, row.names = FALSE)

# Export
write.csv(comparison, "comprehensive_network_statistics.csv", row.names = FALSE)

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("STATISTICS EXPORTED TO: comprehensive_network_statistics.csv\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

