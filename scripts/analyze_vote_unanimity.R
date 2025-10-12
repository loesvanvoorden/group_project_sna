# ============================================================================
# VOTE UNANIMITY ANALYSIS
# Analyze whether agreement patterns are driven by near-unanimous votes
# ============================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

cat("==============================================================\n")
cat("VOTE UNANIMITY ANALYSIS: Are most votes near-unanimous?\n")
cat("==============================================================\n\n")

cat("GOAL: Check if high agreement rates are driven by:\n")
cat("  (a) Genuine party cooperation, or\n")
cat("  (b) Many lopsided/unanimous votes\n\n")

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading data...\n")

# Load 2023 data (Far and Close periods)
voting_data_2023 <- read.csv("data/voting_data_2023_preelection.csv", stringsAsFactors = FALSE)
voting_data_2023$date <- ymd_hms(voting_data_2023$GewijzigdOp)
voting_data_2023$quarter <- quarter(voting_data_2023$date)

# Load 2024 data (Post period)
voting_data_2024 <- read.csv("data/voting_data_clean.csv", stringsAsFactors = FALSE)
voting_data_2024$date <- ymd_hms(voting_data_2024$GewijzigdOp)
voting_data_2024$quarter <- quarter(voting_data_2024$date)

# ============================================================================
# CREATE TEMPORAL PERIODS
# ============================================================================

cat("\nCreating temporal periods...\n")

# Period 1: FAR FROM ELECTION (Q1+Q2 2023)
data_far <- voting_data_2023 %>%
  filter(quarter %in% c(1, 2))

# Period 2: CLOSE TO ELECTION (Q3+Q4 2023)
data_close <- voting_data_2023 %>%
  filter(quarter %in% c(3, 4))

# Period 3: POST FORMATION (Q3+Q4 2024)
data_post <- voting_data_2024 %>%
  filter(year(date) == 2024, quarter %in% c(3, 4))

cat(sprintf("  FAR: %d votes, %d motions\n", 
            nrow(data_far), length(unique(data_far$Besluit_Id))))
cat(sprintf("  CLOSE: %d votes, %d motions\n", 
            nrow(data_close), length(unique(data_close$Besluit_Id))))
cat(sprintf("  POST: %d votes, %d motions\n\n", 
            nrow(data_post), length(unique(data_post$Besluit_Id))))

# ============================================================================
# CALCULATE VOTE SPLITS FOR EACH MOTION
# ============================================================================

cat("Calculating vote splits per motion...\n\n")

analyze_vote_split <- function(data, period_name) {
  
  # Get vote distribution per motion
  motion_splits <- data %>%
    filter(Soort %in% c("Voor", "Tegen")) %>%  # Only Yes/No votes
    group_by(Besluit_Id, Soort) %>%
    summarise(count = n(), .groups = 'drop_last') %>%
    mutate(total = sum(count)) %>%
    ungroup() %>%
    # Calculate proportion voting Yes
    pivot_wider(names_from = Soort, values_from = count, values_fill = 0) %>%
    mutate(
      total_votes = Voor + Tegen,
      pct_yes = Voor / total_votes,
      pct_no = Tegen / total_votes,
      # Unanimity measure: how close to 100% or 0%
      unanimity = pmax(pct_yes, pct_no),
      # Vote balance: 0.5 = perfectly split, 1.0 = unanimous
      balance = abs(pct_yes - 0.5) + 0.5,
      period = period_name
    )
  
  return(motion_splits)
}

splits_far <- analyze_vote_split(data_far, "Far from Election")
splits_close <- analyze_vote_split(data_close, "Close to Election")
splits_post <- analyze_vote_split(data_post, "Post Formation")

# Combine all periods
all_splits <- bind_rows(splits_far, splits_close, splits_post)

# ============================================================================
# STATISTICS ON VOTE UNANIMITY
# ============================================================================

cat("==============================================================\n")
cat("VOTE UNANIMITY STATISTICS\n")
cat("==============================================================\n\n")

calculate_unanimity_stats <- function(splits, period_name) {
  cat(sprintf("%-25s\n", toupper(period_name)))
  cat(sprintf("  Total motions: %d\n", nrow(splits)))
  cat(sprintf("  Mean unanimity: %.1f%%\n", mean(splits$unanimity) * 100))
  cat(sprintf("  Median unanimity: %.1f%%\n", median(splits$unanimity) * 100))
  
  # Count near-unanimous votes (different thresholds)
  unanimous_100 <- sum(splits$unanimity == 1.0)
  unanimous_95 <- sum(splits$unanimity >= 0.95)
  unanimous_90 <- sum(splits$unanimity >= 0.90)
  unanimous_80 <- sum(splits$unanimity >= 0.80)
  unanimous_70 <- sum(splits$unanimity >= 0.70)
  
  cat(sprintf("  100%% unanimous: %d (%.1f%%)\n", 
              unanimous_100, unanimous_100/nrow(splits)*100))
  cat(sprintf("  ≥95%% unanimous: %d (%.1f%%)\n", 
              unanimous_95, unanimous_95/nrow(splits)*100))
  cat(sprintf("  ≥90%% unanimous: %d (%.1f%%)\n", 
              unanimous_90, unanimous_90/nrow(splits)*100))
  cat(sprintf("  ≥80%% unanimous: %d (%.1f%%)\n", 
              unanimous_80, unanimous_80/nrow(splits)*100))
  cat(sprintf("  ≥70%% unanimous: %d (%.1f%%)\n", 
              unanimous_70, unanimous_70/nrow(splits)*100))
  
  # Balanced votes (40-60% split)
  balanced <- sum(splits$unanimity >= 0.4 & splits$unanimity <= 0.6)
  cat(sprintf("  Balanced (40-60%%): %d (%.1f%%)\n", 
              balanced, balanced/nrow(splits)*100))
  
  # Quartiles
  cat(sprintf("  Quartiles: Q1=%.1f%%, Q2=%.1f%%, Q3=%.1f%%\n\n", 
              quantile(splits$unanimity, 0.25)*100,
              quantile(splits$unanimity, 0.50)*100,
              quantile(splits$unanimity, 0.75)*100))
}

calculate_unanimity_stats(splits_far, "Far from Election")
calculate_unanimity_stats(splits_close, "Close to Election")
calculate_unanimity_stats(splits_post, "Post Formation")

# ============================================================================
# CREATE VISUALIZATIONS
# ============================================================================

cat("Creating visualizations...\n")

# Set period order for plots
all_splits$period <- factor(all_splits$period, 
                            levels = c("Far from Election", "Close to Election", "Post Formation"))

# ============================================================================
# 1. HISTOGRAM OF VOTE SPLITS
# ============================================================================

p1 <- ggplot(all_splits, aes(x = pct_yes, fill = period)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~period, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#3498DB")) +
  labs(
    title = "Distribution of Vote Splits: % Voting Yes",
    subtitle = "Are most votes near 0% or 100% (unanimous)?",
    x = "% Voting Yes",
    y = "Number of Motions",
    fill = "Period"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 11),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"))

ggsave("results/visualizations/vote_splits_histogram.pdf", p1, width = 10, height = 10)

# ============================================================================
# 2. DENSITY PLOT OF UNANIMITY
# ============================================================================

p2 <- ggplot(all_splits, aes(x = unanimity, fill = period)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0.9, linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "darkred", size = 0.8) +
  annotate("text", x = 0.9, y = Inf, label = "90% unanimous", 
           hjust = -0.1, vjust = 1.5, size = 3, color = "red") +
  annotate("text", x = 0.95, y = Inf, label = "95% unanimous", 
           hjust = -0.1, vjust = 3, size = 3, color = "darkred") +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#3498DB")) +
  labs(
    title = "Vote Unanimity Distribution Across Periods",
    subtitle = "Unanimity = max(% Yes, % No) — Higher = more lopsided votes",
    x = "Vote Unanimity (0.5 = balanced, 1.0 = unanimous)",
    y = "Density",
    fill = "Period"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"))

ggsave("results/visualizations/vote_unanimity_density.pdf", p2, width = 10, height = 6)

# ============================================================================
# 3. BOX PLOT OF UNANIMITY BY PERIOD
# ============================================================================

p3 <- ggplot(all_splits, aes(x = period, y = unanimity, fill = period)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red", size = 0.8) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "darkred", size = 0.8) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#3498DB")) +
  labs(
    title = "Vote Unanimity by Period",
    subtitle = "Do some periods have more lopsided votes than others?",
    x = "",
    y = "Vote Unanimity (max % voting same way)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("results/visualizations/vote_unanimity_boxplot.pdf", p3, width = 10, height = 6)

# ============================================================================
# 4. CUMULATIVE DISTRIBUTION
# ============================================================================

p4 <- ggplot(all_splits, aes(x = unanimity, color = period)) +
  stat_ecdf(size = 1.2) +
  geom_vline(xintercept = 0.9, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("#E74C3C", "#F39C12", "#3498DB")) +
  labs(
    title = "Cumulative Distribution of Vote Unanimity",
    subtitle = "What % of motions have unanimity below a given threshold?",
    x = "Vote Unanimity",
    y = "Cumulative Probability",
    color = "Period"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"))

ggsave("results/visualizations/vote_unanimity_cumulative.pdf", p4, width = 10, height = 6)

# ============================================================================
# 5. COMBINED SUMMARY PLOT
# ============================================================================

# Create summary statistics table
summary_stats <- all_splits %>%
  group_by(period) %>%
  summarise(
    Total_Motions = n(),
    Mean_Unanimity = mean(unanimity),
    Median_Unanimity = median(unanimity),
    Pct_95_Unanimous = mean(unanimity >= 0.95) * 100,
    Pct_90_Unanimous = mean(unanimity >= 0.90) * 100,
    Pct_Balanced = mean(unanimity >= 0.4 & unanimity <= 0.6) * 100
  )

# Bar plot of unanimity thresholds
summary_long <- summary_stats %>%
  select(period, Pct_95_Unanimous, Pct_90_Unanimous, Pct_Balanced) %>%
  pivot_longer(cols = -period, names_to = "Category", values_to = "Percentage") %>%
  mutate(Category = case_when(
    Category == "Pct_95_Unanimous" ~ "≥95% Unanimous",
    Category == "Pct_90_Unanimous" ~ "≥90% Unanimous",
    Category == "Pct_Balanced" ~ "Balanced (40-60%)"
  ))

summary_long$Category <- factor(summary_long$Category, 
                                levels = c("≥95% Unanimous", "≥90% Unanimous", "Balanced (40-60%)"))

p5 <- ggplot(summary_long, aes(x = period, y = Percentage, fill = Category)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#2ECC71")) +
  labs(
    title = "Vote Characteristics by Period",
    subtitle = "% of motions in each category",
    x = "",
    y = "% of Motions",
    fill = "Vote Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("results/visualizations/vote_unanimity_summary.pdf", p5, width = 10, height = 6)

# ============================================================================
# SAVE STATISTICS
# ============================================================================

write.csv(summary_stats, "results/statistics/vote_unanimity_statistics.csv", row.names = FALSE)

cat("\n==============================================================\n")
cat("INTERPRETATION GUIDE\n")
cat("==============================================================\n\n")

cat("HIGH unanimity (>90%) means:\n")
cat("  → Agreement rates might be inflated by lopsided votes\n")
cat("  → Parties agree because the vote itself is obvious\n")
cat("  → Less signal about genuine party cooperation\n\n")

cat("LOW unanimity (balanced votes) means:\n")
cat("  → Agreement rates reflect genuine party alignment\n")
cat("  → More contested/political issues\n")
cat("  → Better signal for cooperation patterns\n\n")

cat("OUTPUTS:\n")
cat("  • vote_splits_histogram.pdf - Distribution of % voting Yes\n")
cat("  • vote_unanimity_density.pdf - Density plots by period\n")
cat("  • vote_unanimity_boxplot.pdf - Box plots by period\n")
cat("  • vote_unanimity_cumulative.pdf - Cumulative distributions\n")
cat("  • vote_unanimity_summary.pdf - Bar chart summary\n")
cat("  • vote_unanimity_statistics.csv - Summary statistics\n\n")

cat("Analysis complete!\n")

