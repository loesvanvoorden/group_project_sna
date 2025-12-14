# Dutch Parliament Voting Network Analysis

## Overview

This repository contains code and data for analyzing co-voting patterns between political parties in the Dutch House of Representatives during the 2023-2024 parliamentary cycle. The analysis uses social network methods to examine voting agreement networks across two temporal periods.

---

## Repository Structure

```
├── README.md                      # This file
├── final_analysis.R               # ⭐ Main analysis script (reproduces entire project)
├── ergm_testing.R                 # Comprehensive ERGM model testing script
├── report/                        # Quarto report documents
│   ├── SNA4DSprojectTemplate2025.qmd
│   ├── r-references.bib
│   ├── Picture1.png
│   ├── plotdigitizer.png
│   ├── building_pre.png
│   ├── building_post.png
│   └── apa-7th-edition.csl
├── data/                          # Raw data files
│   ├── voting_data_2023_preelection.csv
│   ├── voting_data_clean.csv
│   ├── political_axes_data.csv
│   ├── coauthoring_data_2023_preelection.json
│   ├── coauthoring_data_2024_postformation.json
│   └── nrtimes_coalition_together.csv
├── scripts/                       # Preprocessing and data collection scripts
│   ├── fetch_voting_data.py      # Scrapes voting data & co-sponsoring data from Tweede Kamer API
│   └── generate_edgelists.py     # Generates pre-filtered edge lists
└── results/                       # Analysis outputs (auto-generated)
    ├── edge_lists/                # Pre-processed edge lists (study1_*, study2_*)
    ├── visualizations/            # Network plots, distributions, QAP plots
    ├── statistics/                # QAP results, model comparison tables
    ├── models/                    # Final ERGM models (.rds files)
    ├── tests/                     # Stepwise model building & convergence test models
    └── ergm_diagnostics/          # MCMC diagnostics, GOF plots, model summaries
```

---

## Quick Start

### 1. Install Dependencies

**R packages:**
```r
install.packages(c("igraph", "snafun", "sna", "network", "ergm", 
                   "texreg", "lubridate", "knitr"))
```

**Python (optional, only for data fetching):**
```bash
pip install requests pandas
```

### 2. Run Analysis

**Option A: Main Analysis (Recommended)**
```bash
# Run complete analysis pipeline (generates all results)
Rscript final_analysis.R
```

**What `final_analysis.R` produces:**
- QAP correlation test between two period networks
- 2 final ERGM models
- Network visualizations with fixed layouts
- Agreement rate distribution plots
- ERGM diagnostics (MCMC traces, goodness-of-fit plots, model summaries)
- Model comparison table
- All outputs saved to `results/` directory

**Option B: Comprehensive Model Testing (Optional)**
```bash
# Run extensive stepwise model building and testing
Rscript ergm_testing.R
```

**What `ergm_testing.R` produces:**
- 4 convergence test models (pre/post × mean/Q3 thresholds)
- 24 stepwise models (12 for pre-election, 12 for post-formation):
  - 4 exogenous term combinations
  - 4 endogenous term combinations
  - 4 GWESP decay parameter tests (0.7, 0.75, 0.8, 0.85)
- All test models saved to `results/tests/`
- Diagnostics for convergence models in `results/ergm_diagnostics/convergence_models/`

**Note:** `ergm_testing.R` is used for model development and produces the data visualized in Appendices C and G of the report. It is not required to reproduce the main results.

### 3. Render Report (Optional)

```bash
cd report/
quarto render SNA4DSprojectTemplate2025.qmd
```

Requires Quarto: https://quarto.org/docs/get-started/

---

## Data Sources

| Data Type | Source | Access Method |
|-----------|--------|---------------|
| Voting records | Tweede Kamer OData API | `scripts/fetch_voting_data.py` |
| Ideological positions | Kieskompas 2023 visualizations | Manual plot digitization |
| Co-sponsorship | Tweede Kamer API (Document endpoint) | Automated scraping |
| Coalition history | Parlement.com | Manual collection (last 20 years) |

---

## Temporal Periods

- **Pre-Election Period:** November 22, 2022 - November 21, 2023 (364 days)
- **Post-Formation Period:** July 5, 2024 - July 4, 2025 (365 days)
- **Election Date:** November 22, 2023
- **Cabinet Formation Date:** July 5, 2024
