# ============================================================================
# COMPONENT ANALYSIS: Identify which parties are in which components
# ============================================================================

library(dplyr)
library(lubridate)
library(igraph)

cat("===============================================================================\n")
cat("NETWORK COMPONENT ANALYSIS\n")
cat("Analyzing which parties belong to which components in each period\n")
cat("===============================================================================\n\n")

# ============================================================================
# LOAD DATA AND CREATE NETWORKS
# ============================================================================

# Load pre-election data (2023)
voting_data_2023 <- read.csv("voting_data_2023_preelection.csv", stringsAsFactors = FALSE)
voting_data_2023$date <- ymd_hms(voting_data_2023$GewijzigdOp)
voting_data_2023$quarter <- quarter(voting_data_2023$date)

# Load post-election data (2024)
voting_data_2024 <- read.csv("voting_data_clean.csv", stringsAsFactors = FALSE)
voting_data_2024$date <- ymd_hms(voting_data_2024$GewijzigdOp)
voting_data_2024$quarter <- quarter(voting_data_2024$date)

# Create periods
data_far <- voting_data_2023 %>% filter(quarter %in% c(1, 2))
data_close <- voting_data_2023 %>% filter(quarter %in% c(3, 4))
data_post <- voting_data_2024 %>% filter(year(date) == 2024, quarter %in% c(3, 4))

# Function to calculate agreements
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
  
  return(party_agreement_summary)
}

# Function to create network with highlighted edges
create_network_for_analysis <- function(agreements, all_parties) {
  edges <- agreements %>%
    select(from = ActorFractie_1, to = ActorFractie_2, 
           weight = agreements, agreement_rate, total_votes)
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = all_parties)
  return(g)
}

# Calculate agreements
cat("Calculating party agreements...\n")
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
g_far <- create_network_for_analysis(agreements_far, parties_far)
g_close <- create_network_for_analysis(agreements_close, parties_close)
g_post <- create_network_for_analysis(agreements_post, parties_post)

# ============================================================================
# ANALYZE COMPONENTS
# ============================================================================

analyze_components <- function(g, period_name) {
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat(sprintf("%s NETWORK COMPONENTS\n", period_name))
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  # Get component membership
  comp <- components(g)
  n_components <- comp$no
  
  cat(sprintf("Total components: %d\n", n_components))
  cat(sprintf("Total parties: %d\n", vcount(g)))
  cat(sprintf("Total edges: %d\n", ecount(g)))
  cat(sprintf("Density: %.3f\n\n", edge_density(g)))
  
  # Analyze each component
  for(i in 1:n_components) {
    members <- V(g)$name[comp$membership == i]
    size <- length(members)
    
    cat(sprintf("COMPONENT %d (Size: %d parties, %.1f%% of network)\n", 
                i, size, (size/vcount(g))*100))
    cat(paste(rep("-", 80), collapse = ""), "\n")
    
    # Sort members alphabetically
    members <- sort(members)
    
    # Print members
    cat("Parties:\n")
    for(party in members) {
      cat(sprintf("  • %s\n", party))
    }
    
    # Get subgraph for this component
    subg <- induced_subgraph(g, which(comp$membership == i))
    
    # Component statistics
    cat("\nComponent Statistics:\n")
    cat(sprintf("  Edges within component: %d\n", ecount(subg)))
    cat(sprintf("  Density within component: %.3f\n", edge_density(subg)))
    cat(sprintf("  Mean degree within component: %.2f\n", mean(degree(subg))))
    
    if(size > 2) {
      cat(sprintf("  Transitivity within component: %.3f\n", transitivity(subg)))
      if(is_connected(subg)) {
        cat(sprintf("  Diameter: %.0f\n", diameter(subg)))
        cat(sprintf("  Average path length: %.3f\n", mean_distance(subg)))
      }
    }
    
    cat("\n")
  }
  
  return(comp)
}

# Analyze each period
comp_far <- analyze_components(g_far, "FAR FROM ELECTION (Q1+Q2 2023)")
comp_close <- analyze_components(g_close, "CLOSE TO ELECTION (Q3+Q4 2023)")
comp_post <- analyze_components(g_post, "POST FORMATION (Q3+Q4 2024)")

# ============================================================================
# CROSS-PERIOD COMPONENT COMPARISON
# ============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("COMPONENT EVOLUTION ACROSS PERIODS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Summary:\n")
cat(sprintf("  FAR:   %d components (network fragmentation)\n", comp_far$no))
cat(sprintf("  CLOSE: %d components (%.0f%% reduction)\n", 
            comp_close$no, ((comp_far$no - comp_close$no)/comp_far$no)*100))
cat(sprintf("  POST:  %d components (same as CLOSE)\n\n", comp_post$no))

# Component sizes
cat("Largest Component Size:\n")
cat(sprintf("  FAR:   %d parties (%.1f%% of network)\n", 
            max(comp_far$csize), (max(comp_far$csize)/vcount(g_far))*100))
cat(sprintf("  CLOSE: %d parties (%.1f%% of network)\n", 
            max(comp_close$csize), (max(comp_close$csize)/vcount(g_close))*100))
cat(sprintf("  POST:  %d parties (%.1f%% of network)\n\n", 
            max(comp_post$csize), (max(comp_post$csize)/vcount(g_post))*100))

cat("Interpretation:\n")
cat("• Fewer components = more network integration\n")
cat("• Larger components = broader cooperation coalitions\n")
cat("• Small/isolated components = parties with limited strong cooperation\n")

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ANALYSIS COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

