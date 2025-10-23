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
│   ├── three_period_network_analysis.R           # 3-period comparison (raw)
│   ├── three_period_network_analysis_normalized.R # 3-period (z-score)
│   │
│   ├── two_period_network_analysis.R           # 1 year before/after election (raw)
│   ├── two_period_network_analysis_normalized.R # 1 year before/after election (z-score)
│   │
│   ├── pre_election_vs_post_formation_analysis.R # 1 year: election vs formation (raw)
│   ├── pre_election_vs_post_formation_analysis_normalized.R # (z-score)
│   │
│   ├── one_month_pre_election_vs_post_formation.R # 1 month comparison (raw)
│   ├── one_month_pre_election_vs_post_formation_normalized.R # (z-score)
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
| **3-Period** | Q1+Q2 2023, Q3+Q4 2023, Q3+Q4 2024 | Full electoral cycle view |
| **2-Period (Election)** | 1 year before vs 1 year after election | Election impact |
| **2-Period (Formation)** | 1 year before election vs 1 year after formation | Government formation impact |
| **1-Month Snapshot** | 1 month before election vs 1 month after formation | Short-term changes |

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

**Three-Period Analysis (Quarters):**
```bash
# Raw weights
Rscript scripts/three_period_network_analysis.R

# Z-score normalized (recommended for RQ2)
Rscript scripts/three_period_network_analysis_normalized.R
```

**Two-Period Analysis (1 year before vs after election):**
```bash
# Raw weights
Rscript scripts/two_period_network_analysis.R

# Z-score normalized
Rscript scripts/two_period_network_analysis_normalized.R
```

**Pre-Election vs Post-Formation Analysis (1 year):**
```bash
# Raw weights
Rscript scripts/pre_election_vs_post_formation_analysis.R

# Z-score normalized (recommended for Study 2)
Rscript scripts/pre_election_vs_post_formation_analysis_normalized.R
```

**One-Month Snapshot Analysis:**
```bash
# Raw weights
Rscript scripts/one_month_pre_election_vs_post_formation.R

# Z-score normalized
Rscript scripts/one_month_pre_election_vs_post_formation_normalized.R
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
| `network_comparison_three_periods.pdf` | Raw weight networks (3 periods) |
| `network_comparison_normalized.pdf` ⭐ | Z-score networks (3 periods) |
| `network_comparison_pre_vs_post_formation.pdf` | 1-year comparison (election vs formation) |
| `network_comparison_one_month_pre_vs_post_formation.pdf` | 1-month snapshot comparison |
| `network_comparison_one_month_pre_vs_post_formation_normalized.pdf` | 1-month z-score comparison |
| `vote_unanimity_summary.pdf` | Vote balance validation |

### Key Statistics

| File | Content |
|------|---------|
| `comprehensive_network_statistics.csv` | All network metrics across periods |
| `vote_unanimity_statistics.csv` | Vote distribution analysis |
| `pre_vs_post_formation_comparison.csv` | Statistical comparison (1 year) |
| `one_month_pre_vs_post_formation_comparison.csv` | Statistical comparison (1 month) |

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

### Three-Period Analysis
| Period | Timeframe | Event |
|--------|-----------|-------|
| **Far from Election** | Q1+Q2 2023 (Jan 20 – Jun 29, 2023) | Normal operations (Rutte IV) |
| **Close to Election** | Q3+Q4 2023 (Jul 5 – Nov 13, 2023) | **Election: Nov 22, 2023** |
| **Post Formation** | Q3+Q4 2024 (Jul 5 – Dec 20, 2024) | **New cabinet: July 2, 2024** (Schoof I) |

### Two-Period Analysis (Election)
- **Pre-Election:** November 22, 2022 - November 21, 2023 (1 year before election)
- **Post-Election:** November 22, 2023 - November 21, 2024 (1 year after election)

### Two-Period Analysis (Formation)
- **Pre-Election:** November 22, 2022 - November 21, 2023 (1 year before election)
- **Post-Formation:** July 5, 2024 - July 4, 2025 (1 year after cabinet formation)

### One-Month Snapshot
- **Pre-Election:** October 22, 2023 - November 21, 2023 (1 month before election)
- **Post-Formation:** July 5, 2024 - August 4, 2024 (1 month after formation)

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

### Visualization
- **RED nodes** = Left-wing parties (SP, PvdD, GroenLinks, PvdA, etc.)
- **ORANGE nodes** = Center parties (D66, Volt)
- **BLUE nodes** = Right-wing parties (VVD, CDA, PVV, FVD, etc.)
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

### Network Changes (1-Year Comparison: Pre-Election vs Post-Formation)
- **Nodes:** 23 → 15 parties (-34.8%)
- **Edges:** 253 → 105 (-58.5%)
- **Density:** 1.000 → 1.000 (both fully connected)
- **Mean Degree:** 22.0 → 14.0 (-36.4%)
- **Transitivity:** 1.000 → 1.000 (both fully transitive)

### Network Changes (1-Month Snapshot)
- **Nodes:** 21 → 15 parties (-28.6%)
- **Edges:** 210 → 61 (-70.9%)
- **Density:** 1.000 → 0.581 (-41.9%)
- **Mean Degree:** 20.0 → 8.1 (-59.3%)
- **Components:** 1 → 2 (network became fragmented)

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
