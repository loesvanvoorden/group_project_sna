# Electoral Cycle Network Analysis: Dutch Parliament 2023-2024

## Overview

Social network analysis of co-voting patterns between political parties in the Dutch House of Representatives. Compares party cooperation networks from **1 year before the 2023 election** vs. **1 year after the 2024 cabinet formation**.

---

## Repository Structure

```
├── README.md                      # This file
├── analysis.Rmd                   # ⭐ Main analysis (R Markdown)
├── report/                        # Quarto report documents
│   ├── SNA4DSprojectTemplate2025.qmd
│   └── r-references.bib
├── data/                          # Raw data
│   ├── voting_data_2023_preelection.csv
│   ├── voting_data_clean.csv
│   ├── political_axes_data.csv
│   ├── coauthoring_data_2023_preelection.json
│   ├── coauthoring_data_2024_postformation.json
│   └── nrtimes_coalition_together.csv
├── scripts/                       # Utility scripts
│   └── fetch_voting_data.py
├── oldScripts/                    # Legacy scripts (archived)
└── results/                       # Analysis outputs
    ├── visualizations/
    ├── statistics/
    ├── adjacency_matrices/
    └── edge_lists/
```

---

## Quick Start

### 1. Fetch Data (Optional)

```bash
python3 scripts/fetch_voting_data.py
```

### 2. Run Analysis

```bash
# Render analysis (HTML output)
Rscript -e "rmarkdown::render('analysis.R')"

# Or open in RStudio and click "Knit"
```

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
- **Method:**  Binarized Exponential Random Graph Model (ERGM)
- **Question:** What factors predict voting agreement between parties?
- **Networks:** Pre and post election specific (15 parties per network),
- **Vertex attributes:** `left_right` (ideology)
- **Edge attributes:** `cosponsor_count`, `coalition_count`

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
install.packages(c("lubridate", "igraph", "snafun", "knitr", "jsonlite"))
```

### Python (for data fetching)
```bash
pip install requests pandas
```

### Report Rendering
- Quarto: https://quarto.org/docs/get-started/

---

## Network Construction

- **Nodes:** Political parties (17 parties)
- **Edges:** Co-voting agreements (same vote on same motion)
- **Edge Weight:** Number of agreements (raw) or z-score normalized
- **Study 1:** Fully connected networks (zeros → 1e-6)
- **Study 2:** Sparse networks containing only parties present within time window

---

## Contact

For questions or issues, please open an issue in the repository.
