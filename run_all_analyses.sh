#!/bin/bash

echo "==============================================================================="
echo "RUNNING ALL NETWORK ANALYSES"
echo "==============================================================================="
echo ""

echo "3/8: One-Month Comparison (Raw)..."
Rscript scripts/one_month_pre_election_vs_post_formation.R
echo ""

echo "4/8: One-Month Comparison (Normalized)..."
Rscript scripts/one_month_pre_election_vs_post_formation_normalized.R
echo ""

echo "5/8: Three-Month Comparison (Raw)..."
Rscript scripts/three_month_pre_election_vs_post_formation.R
echo ""

echo "6/8: Three-Month Comparison (Normalized)..."
Rscript scripts/three_month_pre_election_vs_post_formation_normalized.R
echo ""

echo "7/8: One-Year Comparison (Raw)..."
Rscript scripts/pre_election_vs_post_formation_analysis.R
echo ""

echo "8/8: One-Year Comparison (Normalized)..."
Rscript scripts/pre_election_vs_post_formation_analysis_normalized.R
echo ""

echo "==============================================================================="
echo "ALL ANALYSES COMPLETE!"
echo "==============================================================================="
echo ""
echo "Generated visualizations in: results/visualizations/"
echo "Generated statistics in: results/statistics/"
echo "Generated edge lists in: results/edge_lists/"
