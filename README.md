# Electoral Cycle Network Analysis: Dutch Parliament 2023-2024

## Research Question

**Do co-voting patterns between parties change across the electoral cycle?**

---

## Study Design

| Period | Timeframe | Event |
|--------|-----------|-------|
| **Far from Election** | Q1+Q2 2023 | Normal operations (Rutte IV) |
| **Close to Election** | Q3+Q4 2023 | **Election: Nov 22, 2023** |
| **Post Formation** | Q3+Q4 2024 | **New cabinet: July 2, 2024** (Schoof I) |

---

## Key Findings

### 📊 Network Evolution

| Metric | Far | Close | Post |
|--------|-----|-------|------|
| **Parties** | 23 | 22 | 15 |
| **Density** | 75% | 91% | **100%** |
| **Components** | 4 | 2 | **1** |
| **Agreement Rate** | 63.6% | **56.2%** ⬇️ | 62.2% |
| **Strong Ties (z>1)** | 16.8% | 15.7% | **20.0%** ⬆️ |

### 🔑 Main Findings

1. **Pre-election cooperation dip**: Agreement drops to 56.2% as parties differentiate
2. **Post-formation consolidation**: Network becomes fully connected (1 component, 100% density)
3. **Selective cooperation**: 20% strong ties post-formation (highest proportion), despite 82% drop in raw volume

**Interpretation:** Parties strategically reduce cooperation before elections (differentiation), then shift to focused partnerships post-formation (governance).

---

## Repository Structure

```
├── README.md
├── data/                          # Raw voting data
├── scripts/                       # R analysis scripts
│   ├── three_period_network_analysis.R              # Raw weights
│   ├── three_period_network_analysis_normalized.R   # Z-scores ⭐
│   ├── analyze_components.R
│   └── generate_network_statistics.R
├── results/
│   ├── visualizations/            # PDF plots
│   ├── statistics/                # CSV tables
│   └── edge_lists/                # Network data
└── archive/                       # Old analyses
```

---

## Quick Start

### Run Main Analysis (Z-Score Normalized)
```bash
Rscript scripts/three_period_network_analysis_normalized.R
```

**Key output:** `results/visualizations/network_comparison_normalized.pdf`

### Generate Full Statistics
```bash
Rscript scripts/generate_network_statistics.R
```

**Key output:** `results/statistics/comprehensive_network_statistics.csv`

---

## Methodology

### Network Construction
- **Nodes:** Political parties
- **Edges:** Co-voting ties (min 5 shared votes)
- **Weight:** Number of agreements between parties

### Z-Score Normalization

**Why?** Post-formation has 65% fewer votes → raw counts incomparable

**Solution:** Standardize within each period: `z = (weight - mean) / sd`

**Result:** Compare cooperation **patterns** not just volumes

**Interpretation:**
- `z > 1.0`: Strong tie (above average)
- `z > 2.0`: Very strong tie (top ~2%)

---

## Key Visualizations

| File | Content | Best For |
|------|---------|----------|
| `network_comparison_normalized.pdf` ⭐ | Z-score networks | **Answering RQ** |
| `network_comparison_three_periods.pdf` | Raw weight networks | Volume comparison |
| `raw_vs_normalized_comparison.pdf` | Side-by-side | Understanding normalization |
| `comprehensive_network_statistics.csv` | All metrics | Statistical reporting |

**Node colors:** 🔴 Left, 🟠 Center, 🔵 Right

---

## Political Science Interpretation

### Why Cooperation Dips Before Elections

1. **Electoral differentiation**: Parties need to distinguish themselves
2. **Base signaling**: Core voters want parties to "stand firm"
3. **Coalition bargaining**: Avoid appearing "too cozy" to maintain leverage
4. **Campaign mode**: Focus shifts from governance to position-taking

### Why Structure Improves Post-Formation

1. **Governance imperative**: New coalition must pass legislation
2. **Selective cooperation**: Fewer votes but more focused partnerships
3. **Complete integration**: All parties connected (100% density)

---

## Requirements

```r
install.packages(c("dplyr", "lubridate", "igraph", "ggplot2", "tidyr"))
```

---

## Data Sources

- **Tweede Kamer OData API**
- 2023: 69,544 votes across 3,309 motions
- 2024: 12,821 votes across 1,952 motions (Q3-Q4 only)

---

**Analysis demonstrates strategic modulation of parliamentary cooperation across the electoral cycle in Dutch multiparty politics.** 🇳🇱📊
