# Electoral Cycle Network Analysis: Dutch Parliament 2023-2024

## Study Design

| Period | Timeframe | Event |
|--------|-----------|-------|
| **Far from Election** | Q1+Q2 2023 (Jan 20 – Jun 29, 2023) | Normal operations (Rutte IV) |
| **Close to Election** | Q3+Q4 2023 (Jul 5 – Nov 13, 2023) | **Election: Nov 22, 2023** |
| **Post Formation** | Q3+Q4 2024 (Jul 5 – Dec 20, 2024) | **New cabinet: July 2, 2024** (Schoof I) |

---

## Repository Structure

```
├── README.md                      # This file
├── 3 SNA 2.Rproj                  # R project file
├── report/                        # Quarto report documents
│   ├── SNA4DSprojectTemplate2025.qmd   # Main report source
│   ├── r-references.bib           # Bibliography
│   ├── Picture1.png               # Kieskompas visualization
│   ├── Picture2.png               # Extracted coordinates
│   └── _output/                   # Rendered PDFs
├── data/                          # Raw voting data
│   ├── voting_data_2023_preelection.csv
│   ├── voting_data_clean.csv      # 2024 data
│   └── political_axes_data.csv    # Party ideology (Kieskompas 2023)
├── scripts/                       # R analysis scripts
│   ├── three_period_network_analysis.R
│   ├── three_period_network_analysis_normalized.R
│   ├── analyze_components.R
│   ├── generate_network_statistics.R
│   ├── analyze_vote_unanimity.R
│   └── add_ideology_attributes.R  # Helper: Add Kieskompas data to networks
└── results/
    ├── visualizations/            # Network plots (PDFs)
    ├── statistics/                # Summary statistics (CSV)
    └── edge_lists/                # Network edge data (CSV)
```

---

## Quick Start

### Render the Report

```bash
cd report/
quarto render SNA4DSprojectTemplate2025.qmd
```

**Output:** `report/_output/SNA4DSprojectTemplate2025.pdf`

### Run Network Analysis

**Raw weights:**
```bash
Rscript scripts/three_period_network_analysis.R
```

**Z-score normalized (recommended):**
```bash
Rscript scripts/three_period_network_analysis_normalized.R
```

**Generate all statistics:**
```bash
Rscript scripts/generate_network_statistics.R
```

**Add ideology attributes (for Study 1):**
```bash
Rscript scripts/add_ideology_attributes.R
```

This loads the Kieskompas ideology data and demonstrates how to:
- Add left-right and conservative-progressive coordinates as node attributes
- Calculate ideological distance matrices for MRQAP analysis

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
| `vote_unanimity_summary.pdf` | Vote balance validation |

### Key Statistics
| File | Content |
|------|---------|
| `comprehensive_network_statistics.csv` | All network metrics |
| `vote_unanimity_statistics.csv` | Vote distribution analysis |

---

## Data Sources

- **Tweede Kamer OData API** - Parliamentary voting records (2023-2024)
  - 82,365 votes across 5,213 motions from 23 parties
  - Files: `voting_data_2023_preelection.csv`, `voting_data_clean.csv`
  
- **Kieskompas 2023** - Party ideological positions (for Study 1)
  - 2D ideology: Left-Right & Conservative-Progressive axes
  - Normalized scale: -1 to +1 on each dimension
  - File: `political_axes_data.csv` (19 parties with coordinates)

---

## Methodology Highlights

### Network Construction
- **Nodes:** Political parties (fracties)
- **Edges:** Co-voting ties (min 5 shared votes)
- **Weight:** Number of agreements between parties

### Z-Score Normalization

**Why?** Post-formation has 65% fewer votes → raw counts incomparable

**Solution:** Standardize within each period:
```
z = (weight - mean) / sd
```

**Result:** Compare cooperation patterns independent of voting volume

### Key Findings
- Agreement rates: 63.6% (Far) → 56.2% (Close) → 62.2% (Post)
- Network density: 0.75 → 0.91 → 1.00 (complete connectivity)
- Strong ties (z>1): 16.8% (Far), 15.7% (Close), **20.0%** (Post)

---

## Requirements

```r
install.packages(c("dplyr", "lubridate", "igraph", "ggplot2", "tidyr"))
```

For report rendering:
```bash
# Install Quarto: https://quarto.org/docs/get-started/
```
