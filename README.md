# Electoral Cycle Network Analysis: Dutch Parliament 2023-2024

## Overview

This repository contains a comprehensive social network analysis of co-voting patterns between political parties in the Dutch House of Representatives during the 2023-2024 electoral cycle. The analysis compares cooperation networks across different temporal periods to understand how party behavior changes around elections and government formation.

---

## Repository Structure

```
├── README.md                      # This file
│ 
├── report/                        # Quarto report documents
│   ├── SNA4DSprojectTemplate2025.qmd   # Main report source
│   ├── r-references.bib           # Bibliography
│   ├── Picture1.png               # Kieskompas visualization
│   ├── Picture2.png               # Extracted coordinates
│   └── _output/                   # Rendered PDFs
├── data/                          # Raw voting data
│   ├── voting_data_2023_preelection.csv  # 2023 voting records
│   ├── voting_data_clean.csv      # 2024 voting records
│   └── political_axes_data.csv    # Party ideology (Kieskompas 2023)
├── scripts/                       # R analysis scripts
│   ├── fetch_voting_data.R        # Fetch data from Tweede Kamer API
│   │
│   ├── one_month_pre_election_vs_post_formation.R # 1 month comparison (raw)
│   ├── one_month_pre_election_vs_post_formation_normalized.R # (z-score)
│   │
│   ├── three_month_pre_election_vs_post_formation.R # 3 month comparison (raw)
│   ├── three_month_pre_election_vs_post_formation_normalized.R # (z-score)
│   │
│   ├── pre_election_vs_post_formation_analysis.R # 1 year comparison (raw)
│   ├── pre_election_vs_post_formation_analysis_normalized.R # (z-score)
│   │
│   ├── analyze_components.R       # Component analysis
│   ├── generate_network_statistics.R # Comprehensive statistics
│   ├── analyze_vote_unanimity.R   # Vote distribution validation
│   └── add_ideology_attributes.R  # Helper: Add Kieskompas data to networks
└── results/
    ├── visualizations/            # Network plots (PDFs)
    ├── statistics/                # Summary statistics (CSV)
    └── edge_lists/                # Network edge data (CSV)
```

---

## Analysis Approaches

### Temporal Comparisons

| Analysis | Time Periods | Use Case |
|----------|-------------|----------|
| **1-Month Snapshot** | 1 month before election vs 1 month after formation | Short-term transition effects |
| **3-Month Comparison** | 3 months before election vs 3 months after formation | Medium-term patterns (recommended) |
| **1-Year Comparison** | 1 year before election vs 1 year after formation | Long-term structural changes |

### Network Types

- **Raw Weight Networks**: Edges weighted by absolute number of agreements
  - Shows actual cooperation volume
  - Affected by voting activity levels

- **Z-Score Normalized Networks**: Edges weighted by standardized cooperation
  - Formula: `z = (weight - mean) / sd`
  - Compares relative cooperation patterns
  - Independent of vote volume differences

---

## Quick Start

### 1. Render the Report

```bash
cd report/
quarto render SNA4DSprojectTemplate2025.qmd
```

**Output:** `report/_output/SNA4DSprojectTemplate2025.pdf`

### 2. Fetch Data from API (Optional)

**Note:** The data is already included in the repository. Only run this if you need to update with the latest votes from the Tweede Kamer.

```bash
Rscript scripts/fetch_voting_data.R
```

This fetches voting data via the Open Data Portal API and saves:
- `data/voting_data_2023_preelection.csv` (2023 votes)
- `data/voting_data_clean.csv` (2024 votes)

### 3. Run Network Analysis

**Run All Analyses at Once:**
```bash
./run_all_analyses.sh
```

**Or run individual analyses:**

**One-Month Snapshot Analysis:**
```bash
# Raw weights
Rscript scripts/one_month_pre_election_vs_post_formation.R

# Z-score normalized
Rscript scripts/one_month_pre_election_vs_post_formation_normalized.R
```

**Three-Month Comparison (Recommended):**
```bash
# Raw weights
Rscript scripts/three_month_pre_election_vs_post_formation.R

# Z-score normalized (recommended for Study 2)
Rscript scripts/three_month_pre_election_vs_post_formation_normalized.R
```

**One-Year Comparison:**
```bash
# Raw weights
Rscript scripts/pre_election_vs_post_formation_analysis.R

# Z-score normalized
Rscript scripts/pre_election_vs_post_formation_analysis_normalized.R
```

**Additional Analyses:**
```bash
# Generate comprehensive statistics
Rscript scripts/generate_network_statistics.R

# Analyze vote unanimity (validation)
Rscript scripts/analyze_vote_unanimity.R

# Add ideology attributes (for Study 1)
Rscript scripts/add_ideology_attributes.R
```

---

## Key Files

### Documentation
- **`README.md`** - Project overview and quick start guide
- **`report/SNA4DSprojectTemplate2025.qmd`** ⭐ - Full methodology section (Dataset + Biases)

### Key Visualizations

| File | Content |
|------|---------|
| `network_comparison_one_month_pre_vs_post_formation.pdf` | 1-month snapshot (raw) |
| `network_comparison_one_month_pre_vs_post_formation_normalized.pdf` | 1-month snapshot (z-score) |
| `network_comparison_three_month_pre_vs_post_formation.pdf` | 3-month comparison (raw) ⭐ |
| `network_comparison_three_month_pre_vs_post_formation_normalized.pdf` | 3-month comparison (z-score) ⭐ |
| `network_comparison_pre_vs_post_formation.pdf` | 1-year comparison (raw) |
| `network_comparison_normalized_pre_vs_post_formation.pdf` | 1-year comparison (z-score) |
| `vote_unanimity_summary.pdf` | Vote balance validation |

### Key Statistics

| File | Content |
|------|---------|
| `one_month_pre_vs_post_formation_comparison.csv` | 1-month comparison stats |
| `three_month_pre_vs_post_formation_comparison.csv` | 3-month comparison stats ⭐ |
| `pre_vs_post_formation_comparison.csv` | 1-year comparison stats |
| `normalized_network_comparison_pre_vs_post_formation.csv` | Z-score comparison (1-year) |
| `vote_unanimity_statistics.csv` | Vote distribution analysis |

---

## Data Sources

### Parliamentary Voting Data
- **Source:** Tweede Kamer OData API
- **Coverage:** 2023-2024 electoral cycle
- **Records:** 82,365 votes across 5,213 motions from 23 parties
- **Files:** 
  - `voting_data_2023_preelection.csv` (69,544 records)
  - `voting_data_clean.csv` (49,472 records)

### Party Ideology Data
- **Source:** Kieskompas 2023
- **Dimensions:** Left-Right & Conservative-Progressive axes
- **Scale:** -1 to +1 on each dimension
- **Coverage:** 19 parties with coordinates
- **File:** `political_axes_data.csv`
- **Use:** Node attributes for Study 1 (MRQAP analysis)

---

## Temporal Periods

### Key Political Events
- **Election Date:** November 22, 2023
- **Cabinet Formation:** July 5, 2024 (Schoof I)

### One-Month Snapshot
- **Pre-Election:** October 22, 2023 - November 21, 2023 (1 month before election)
  - 588 motions, 12,447 votes, 21 parties
- **Post-Formation:** July 5, 2024 - August 4, 2024 (1 month after formation)
  - 27 motions, 269 votes, 15 parties

### Three-Month Comparison (Recommended)
- **Pre-Election:** August 22, 2023 - November 21, 2023 (3 months before election)
  - 1,102 motions, 23,850 votes, 22 parties
- **Post-Formation:** July 5, 2024 - October 4, 2024 (3 months after formation)
  - 243 motions, 1,599 votes, 15 parties

### One-Year Comparison
- **Pre-Election:** November 22, 2022 - November 21, 2023 (1 year before election)
  - 3,309 motions, 69,544 votes, 25 parties
- **Post-Formation:** July 5, 2024 - July 4, 2025 (1 year after formation)
  - 5,004 motions, 33,177 votes, 17 parties

---

## Network Construction

### Edge Definition
- **Nodes:** Political parties
- **Edges:** Co-voting agreements between party pairs
- **Edge Weight (Raw):** Number of motions where both parties voted the same way
- **Edge Weight (Z-Score):** Standardized cooperation within each period

### Filtering
- Minimum 5 shared votes required for edge creation
- Duplicate votes removed (one vote per party per motion)
- **Note:** Raw data contains 2,745 duplicate votes (same party voting multiple times on same motion) which are properly deduplicated

### Visualization (Based on Kieskompas 2023 Data)
- **RED nodes** = Left-wing parties (BIJ1, PvdD, GroenLinks-PvdA, SP, DENK, ChristenUnie, 50PLUS)
- **ORANGE nodes** = Center parties (Volt, D66, NSC, BBB)
- **BLUE nodes** = Right-wing parties (CDA, VVD, SGP, PVV, JA21, FVD, BVNL)
- **Node size** = Degree centrality (more connections = larger)
- **Edge thickness** = Cooperation strength
- **Edge highlighting:** 
  - Raw networks: 30% above mean weight
  - Z-score networks: z > 1.0

---

## Requirements

### R Packages
```r
install.packages(c("lubridate", "igraph", "ggplot2"))
```

**Note:** Scripts use only base R, `lubridate`, `igraph`, and `ggplot2`. No `dplyr` or `tidyr` dependencies.

### Report Rendering
```bash
# Install Quarto: https://quarto.org/docs/get-started/
```

---

## Key Findings

### Network Changes (3-Month Comparison - Recommended)
- **Nodes:** 22 → 15 parties (-31.8%)
- **Edges:** 211 → 105 (-50.2%)
- **Density:** 0.913 → 1.000 (+9.5%)
- **Mean Degree:** 19.2 → 14.0 (-27.0%)
- **Transitivity:** 1.000 → 1.000 (both fully transitive)
- **Components:** 2 → 1 (network became more integrated)
- **Agreement Rate:** 54.4% → 61.4% (higher cooperation post-formation)

### Network Changes (1-Month Snapshot)
- **Nodes:** 21 → 15 parties (-28.6%)
- **Edges:** 210 → 61 (-70.9%)
- **Density:** 1.000 → 0.581 (-41.9%)
- **Mean Degree:** 20.0 → 8.1 (-59.3%)
- **Components:** 1 → 2 (network became fragmented)
- **Note:** Limited post-formation data (only 27 motions)

### Network Changes (1-Year Comparison)
- **Nodes:** 25 → 17 parties (-32.0%)
- **Edges:** 223 → 112 (-49.8%)
- **Density:** 0.743 → 0.824 (+10.8%)
- **Mean Degree:** 17.8 → 13.2 (-26.1%)
- **Components:** 5 → 3 (network became more integrated)

### Vote Unanimity Validation
- **Mean Agreement Rate:** 55-56% across all periods
- **Near-Unanimous Votes:** ~20% of motions
- **Balanced Votes:** ~30% of motions
- **Conclusion:** Agreement rates reflect genuine cooperation, not unanimous voting

---

## Citation

```bibtex
@misc{dutch_parliament_network_2025,
  title={Electoral Cycle Network Analysis: Dutch Parliament 2023-2024},
  author={[Your Name]},
  year={2025},
  url={[Repository URL]}
}
```

---

## License

This project is for academic research purposes.

---

## Contact

For questions or issues, please open an issue in the repository.
