# Electoral Cycle Network Analysis: Dutch Parliament 2023-2024

## Overview

Social network analysis of co-voting patterns between political parties in the Dutch House of Representatives. Compares party cooperation networks from **1 year before the 2023 election** vs. **1 year after the 2024 cabinet formation**.

---

## Repository Structure

```
├── README.md                      # This file
├── final_analysis.R               # ⭐ Main analysis script (reproduces entire project)
├── final_notebook.Rmd             # Alternative notebook version
├── report/                        # Quarto report documents
│   ├── SNA4DSprojectTemplate2025.qmd
│   ├── r-references.bib
│   └── _output/                   # Rendered report files
├── data/                          # Raw data
│   ├── voting_data_2023_preelection.csv
│   ├── voting_data_clean.csv
│   ├── political_axes_data.csv
│   ├── coauthoring_data_2023_preelection.json
│   ├── coauthoring_data_2024_postformation.json
│   └── nrtimes_coalition_together.csv
├── scripts/                       # Utility scripts
│   ├── fetch_voting_data.py      # Data fetching
│   └── generate_edgelists.py     # Edge list preprocessing
├── oldScripts/                    # Legacy R scripts (archived)
├── oldNotebooks/                  # Previous analysis versions
│   ├── analysis.Rmd
│   └── analysis_backup.Rmd
└── results/                       # Analysis outputs (auto-generated)
    ├── edge_lists/                # Pre-filtered edge lists (study1_*, study2_*)
    ├── visualizations/            # Network plots, distributions, QAP plots
    ├── statistics/                # QAP results, network metrics
    ├── models/                    # Saved ERGM models (.rds files)
    └── ergm_diagnostics/          # MCMC, GOF, and model summaries
```

---

## Quick Start

### 1. Fetch Data (Optional)

```bash
python3 scripts/fetch_voting_data.py
```

### 2. Run Analysis

```bash
# Run complete analysis pipeline (generates all results)
Rscript final_analysis.R

# Or open in RStudio and run the script
```

**What `final_analysis.R` produces:**
- QAP correlation analysis and plots
- 4 ERGM models (Q3/Mean thresholds × Pre/Post periods)
- Network visualizations and agreement rate distributions  
- ERGM diagnostics (MCMC, GOF, summaries)
- All results saved to `results/` directory

### 3. Render Report

```bash
cd report/
quarto render SNA4DSprojectTemplate2025.qmd
```

---

## Research Design

### Study 1: QAP Analysis
- **Method:** Quadratic Assignment Procedure (QAP)
- **Question:** Does network structure change between pre-election and post-formation periods?
- **Networks:** Fully connected (all 17 parties), z-score normalized weights

### Study 2: Binarized ERGM Analysis
- **Method:** Binarized Exponential Random Graph Model (ERGM)
- **Question:** What factors predict voting agreement between parties?
- **Networks:** 4 models (Q3/Mean thresholds × Pre/Post periods, ~15 parties each)
- **Vertex attributes:** `left_right` (ideology)
- **Edge covariates:** `cosponsor_matrix`, `coalition_matrix`
- **Model terms:** `edges`, `absdiff("left_right")`, `kstar(3)`, `gwesp(0.5)`, `edgecov`

---

## Temporal Periods

- **Election Date:** November 22, 2023
- **Cabinet Formation:** July 5, 2024
- **Pre-Election:** November 22, 2022 - November 21, 2023
- **Post-Formation:** July 5, 2024 - July 4, 2025

---

## Data Sources

- **Voting Data:** Tweede Kamer OData API
- **Ideology Data:** Kieskompas 2021 & 2023
- **Co-Sponsorship Data:** Tweede Kamer API (Document endpoint)
- **Coalition Data:** Historical coalition records self collected

---

## Requirements

### R Packages
```r
install.packages(c("igraph", "snafun", "sna", "network", "ergm", "texreg"))
```

### Python (for data fetching - optional)
```bash
pip install requests pandas
```

### Report Rendering
- Quarto: https://quarto.org/docs/get-started/

---

## Network Construction

- **Nodes:** Political parties (Study 1: 17 parties, Study 2: ~15 parties per period)
- **Edges:** Co-voting agreements (same vote on same motion)  
- **Edge Weight:** Agreement rates (fraction of shared votes)
- **Study 1:** Fully connected networks (zeros → 1e-6) for QAP analysis
- **Study 2:** Binarized networks at Q3/Mean thresholds for ERGM analysis
- **Pre-generated:** Edge lists already filtered and ready in `results/edge_lists/`

---

## Contact

For questions or issues, please open an issue in the repository.
