# Tweede Kamer Voting Data - Bipartite Network Analysis
# This script scrapes voting data and creates a bipartite network of parties and motions
# Data is filtered to include only voting records from 2024 onwards (last year)

# Install and load required packages
required_packages <- c("httr", "jsonlite", "igraph", "dplyr", "ggplot2", "tidyr", "utils")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =============================================================================
# 1. DATA COLLECTION
# =============================================================================

cat("=============================================================================\n")
cat("Fetching voting data from Tweede Kamer Open Data API...\n")
cat("Time period: From 2024-01-01 onwards (last ~2 years)\n")
cat("=============================================================================\n")

# Base URL for the OData API
base_url <- "https://gegevensmagazijn.tweedekamer.nl/OData/v4/2.0"

# Function to fetch data from OData API with pagination
fetch_stemmingen <- function(limit = 10000, from_date = "2024-01-01") {
  stemmingen_url <- paste0(base_url, "/Stemming")
  
  all_stemmingen <- data.frame()
  skip <- 0
  top <- 250  # API limit is 250 records per request
  
  repeat {
    cat(sprintf("Fetching records %d to %d...\n", skip + 1, skip + top))
    
    # Build query with pagination and date filter
    # Filter for data from the specified date onwards
    date_filter <- sprintf("GewijzigdOp ge %s", from_date)
    query_url <- paste0(stemmingen_url, 
                       "?$filter=", URLencode(date_filter),
                       "&$skip=", skip, 
                       "&$top=", top)
    
    cat(sprintf("Query URL: %s\n", query_url))
    
    # Make API request
    response <- tryCatch({
      GET(query_url, 
          add_headers(Accept = "application/json"),
          timeout(60))
    }, error = function(e) {
      cat("Error fetching data:", e$message, "\n")
      return(NULL)
    })
    
    # Check if request was successful
    if (is.null(response)) {
      cat("Response is NULL\n")
      break
    }
    
    if (status_code(response) != 200) {
      cat(sprintf("Failed to fetch data. Status code: %d\n", status_code(response)))
      # Try to get error message
      error_content <- tryCatch({
        content(response, as = "text", encoding = "UTF-8")
      }, error = function(e) {
        "Could not parse error message"
      })
      cat(sprintf("Error details: %s\n", substr(error_content, 1, 500)))
      break
    }
    
    # Parse JSON response
    content_text <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(content_text, flatten = TRUE)
    
    # Extract the value array
    if (!"value" %in% names(data) || length(data$value) == 0) {
      cat("No more data available.\n")
      break
    }
    
    all_stemmingen <- rbind(all_stemmingen, data$value)
    cat(sprintf("Total records collected so far: %d\n", nrow(all_stemmingen)))
    
    # Check if we've reached the limit or end of data
    if (nrow(data$value) < top || nrow(all_stemmingen) >= limit) {
      break
    }
    
    skip <- skip + top
    
    # Small delay to be nice to the API
    Sys.sleep(0.5)
  }
  
  return(all_stemmingen)
}

# Fetch the voting data (from 2024 onwards - last year)
cat("Filtering for voting data from 2024-01-01 onwards...\n")
voting_data <- fetch_stemmingen(limit = 50000, from_date = "2024-01-01")

cat(sprintf("Successfully fetched %d voting records.\n", nrow(voting_data)))

# Check if we got any data
if (nrow(voting_data) == 0) {
  stop("No data was fetched from the API. Please check the error messages above and try again.")
}

# =============================================================================
# 2. DATA PREPROCESSING
# =============================================================================

cat("\nPreprocessing data...\n")

# Check what columns we actually have
cat("Available columns:\n")
print(names(voting_data))

# Remove votes that were mistakes (if Vergissing column exists)
if ("Vergissing" %in% names(voting_data)) {
  voting_data_clean <- voting_data %>%
    filter(is.na(Vergissing) | Vergissing == FALSE)
} else {
  voting_data_clean <- voting_data
}

# Check for required columns
required_cols <- c("Fractie_Id", "Besluit_Id")
missing_cols <- setdiff(required_cols, names(voting_data_clean))
if (length(missing_cols) > 0) {
  stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")))
}

# Remove missing values for required fields
voting_data_clean <- voting_data_clean %>%
  filter(!is.na(Fractie_Id), 
         !is.na(Besluit_Id))

# Create unique identifiers for parties and motions
# Check if ActorFractie exists, otherwise use Fractie_Id
if ("ActorFractie" %in% names(voting_data_clean)) {
  voting_data_clean <- voting_data_clean %>%
    filter(!is.na(ActorFractie)) %>%
    mutate(
      party = ActorFractie,
      motion = Besluit_Id
    )
} else {
  voting_data_clean <- voting_data_clean %>%
    mutate(
      party = Fractie_Id,
      motion = Besluit_Id
    )
}

# Get unique parties and motions
unique_parties <- unique(voting_data_clean$party)
unique_motions <- unique(voting_data_clean$motion)

cat(sprintf("Data contains:\n"))
cat(sprintf("  - %d unique parties\n", length(unique_parties)))
cat(sprintf("  - %d unique motions\n", length(unique_motions)))
cat(sprintf("  - %d voting relationships (edges)\n", nrow(voting_data_clean)))

# =============================================================================
# 3. CREATE BIPARTITE NETWORK
# =============================================================================

cat("\nCreating bipartite network...\n")

# Create edge list
edge_list <- voting_data_clean %>%
  select(party, motion) %>%
  distinct()  # Remove duplicate edges if any

# Create bipartite graph
# Type FALSE = parties (first column), Type TRUE = motions (second column)
g <- graph_from_data_frame(edge_list, directed = FALSE)

# Set vertex types for bipartite network
V(g)$type <- V(g)$name %in% unique_motions

# Verify it's bipartite
cat(sprintf("Is bipartite: %s\n", is_bipartite(g)))

# Get graph statistics
cat("\n=============================================================================\n")
cat("NETWORK STATISTICS\n")
cat("=============================================================================\n")
cat(sprintf("Total nodes: %d\n", vcount(g)))
cat(sprintf("  - Party nodes: %d\n", sum(!V(g)$type)))
cat(sprintf("  - Motion nodes: %d\n", sum(V(g)$type)))
cat(sprintf("Total edges: %d\n", ecount(g)))
cat(sprintf("Network density: %.4f\n", edge_density(g)))

# =============================================================================
# 4. DEGREE DISTRIBUTION ANALYSIS
# =============================================================================

cat("\n=============================================================================\n")
cat("DEGREE DISTRIBUTION ANALYSIS\n")
cat("=============================================================================\n")

# Calculate degrees
all_degrees <- degree(g)

# Separate degrees by node type
party_nodes <- V(g)[V(g)$type == FALSE]
motion_nodes <- V(g)[V(g)$type == TRUE]

party_degrees <- degree(g, v = party_nodes)
motion_degrees <- degree(g, v = motion_nodes)

# Summary statistics for parties
cat("\nParty Degree Statistics:\n")
cat(sprintf("  Mean: %.2f\n", mean(party_degrees)))
cat(sprintf("  Median: %.2f\n", median(party_degrees)))
cat(sprintf("  Min: %d\n", min(party_degrees)))
cat(sprintf("  Max: %d\n", max(party_degrees)))
cat(sprintf("  SD: %.2f\n", sd(party_degrees)))

# Summary statistics for motions
cat("\nMotion Degree Statistics:\n")
cat(sprintf("  Mean: %.2f\n", mean(motion_degrees)))
cat(sprintf("  Median: %.2f\n", median(motion_degrees)))
cat(sprintf("  Min: %d\n", min(motion_degrees)))
cat(sprintf("  Max: %d\n", max(motion_degrees)))
cat(sprintf("  SD: %.2f\n", sd(motion_degrees)))

# Top parties by degree
cat("\nTop 10 Most Active Parties (by number of votes):\n")
top_parties <- sort(party_degrees, decreasing = TRUE)[1:min(10, length(party_degrees))]
for (i in 1:length(top_parties)) {
  cat(sprintf("  %d. %s: %d votes\n", i, names(top_parties)[i], top_parties[i]))
}

# =============================================================================
# 5. VISUALIZATIONS
# =============================================================================

cat("\nCreating visualizations...\n")

# Prepare data for plotting
party_degree_df <- data.frame(
  node = names(party_degrees),
  degree = as.numeric(party_degrees),
  type = "Party"
)

motion_degree_df <- data.frame(
  node = names(motion_degrees),
  degree = as.numeric(motion_degrees),
  type = "Motion"
)

degree_df <- rbind(party_degree_df, motion_degree_df)

# Plot 1: Degree distribution histogram (separate for parties and motions)
p1 <- ggplot(degree_df, aes(x = degree, fill = type)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  facet_wrap(~type, scales = "free") +
  labs(
    title = "Degree Distribution: Parties vs Motions",
    subtitle = "Tweede Kamer Voting Network",
    x = "Degree",
    y = "Frequency",
    fill = "Node Type"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("degree_distribution_histogram.png", p1, width = 12, height = 6, dpi = 300)

# Plot 2: Log-log plot for degree distribution (checking power law)
degree_counts_party <- as.data.frame(table(party_degrees))
names(degree_counts_party) <- c("degree", "frequency")
degree_counts_party$degree <- as.numeric(as.character(degree_counts_party$degree))
degree_counts_party$type <- "Party"

degree_counts_motion <- as.data.frame(table(motion_degrees))
names(degree_counts_motion) <- c("degree", "frequency")
degree_counts_motion$degree <- as.numeric(as.character(degree_counts_motion$degree))
degree_counts_motion$type <- "Motion"

degree_counts <- rbind(degree_counts_party, degree_counts_motion)

p2 <- ggplot(degree_counts, aes(x = degree, y = frequency, color = type)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_line(alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~type, scales = "free") +
  labs(
    title = "Degree Distribution (Log-Log Scale)",
    subtitle = "Checking for power-law behavior",
    x = "Degree (log scale)",
    y = "Frequency (log scale)",
    color = "Node Type"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("degree_distribution_loglog.png", p2, width = 12, height = 6, dpi = 300)

# Plot 3: Cumulative degree distribution
p3 <- ggplot(degree_df, aes(x = degree, color = type)) +
  stat_ecdf(size = 1.2) +
  facet_wrap(~type, scales = "free_x") +
  labs(
    title = "Cumulative Degree Distribution",
    subtitle = "ECDF of node degrees",
    x = "Degree",
    y = "Cumulative Probability",
    color = "Node Type"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("degree_distribution_cumulative.png", p3, width = 12, height = 6, dpi = 300)

# Plot 4: Box plot comparison
p4 <- ggplot(degree_df, aes(x = type, y = degree, fill = type)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  labs(
    title = "Degree Distribution Comparison",
    subtitle = "Box plot on log scale",
    x = "Node Type",
    y = "Degree (log scale)",
    fill = "Node Type"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("degree_distribution_boxplot.png", p4, width = 8, height = 6, dpi = 300)

# =============================================================================
# 6. SAVE RESULTS
# =============================================================================

cat("\nSaving results...\n")

# Save the network object
save(g, file = "bipartite_network.RData")

# Save degree data
write.csv(degree_df, "degree_distribution.csv", row.names = FALSE)

# Save detailed voting data
write.csv(voting_data_clean, "voting_data_clean.csv", row.names = FALSE)

# Create summary report
summary_report <- list(
  network_stats = list(
    total_nodes = vcount(g),
    party_nodes = sum(!V(g)$type),
    motion_nodes = sum(V(g)$type),
    total_edges = ecount(g),
    density = edge_density(g)
  ),
  party_degree_stats = list(
    mean = mean(party_degrees),
    median = median(party_degrees),
    min = min(party_degrees),
    max = max(party_degrees),
    sd = sd(party_degrees)
  ),
  motion_degree_stats = list(
    mean = mean(motion_degrees),
    median = median(motion_degrees),
    min = min(motion_degrees),
    max = max(motion_degrees),
    sd = sd(motion_degrees)
  ),
  top_parties = as.list(head(sort(party_degrees, decreasing = TRUE), 10))
)

write(toJSON(summary_report, pretty = TRUE), "network_summary.json")

cat("\n=============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("=============================================================================\n")
cat("\nGenerated files:\n")
cat("  - bipartite_network.RData (network object)\n")
cat("  - degree_distribution.csv (degree data)\n")
cat("  - voting_data_clean.csv (cleaned voting data)\n")
cat("  - network_summary.json (summary statistics)\n")
cat("  - degree_distribution_histogram.png\n")
cat("  - degree_distribution_loglog.png\n")
cat("  - degree_distribution_cumulative.png\n")
cat("  - degree_distribution_boxplot.png\n")
cat("\n")

