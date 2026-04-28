#!/usr/bin/env bash
# chmod +x run_pipeline.sh
# ./run_pipeline.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo " Reform Pathway Analysis - full pipeline"
echo " Working directory: $SCRIPT_DIR"

mkdir -p paper_outputs/tables
mkdir -p paper_outputs/figures
mkdir -p data/processed

echo ""
echo "[1/5] Data preparation ..."
python src/python/01_data_prep.py

echo ""
echo "[2/5] NMK ASCM estimation (tables 1–3, figures 1–4) ..."
python src/python/02_ascm_estimation.py

echo ""
echo "[3/5] Cross-country estimation (table 4, figures 5–8) ..."
python src/python/03_cross_country.py

echo ""
echo "[4/5] Inference: LOO + placebo in space + placebo in time ..."
python src/python/04_inference.py

echo ""
echo "[5/5] Sensitivity: donor-pool + predictor/specification checks ..."
python src/python/05_sensitivity.py

echo ""
echo " Pipeline complete."
echo " Tables in paper_outputs/tables/"
echo " Figures in paper_outputs/figures/"

if command -v Rscript &>/dev/null; then
    echo ""
    echo "Handling R requirements ..."
    Rscript -e "pkgs <- c('bsts','CausalImpact','synthdid'); if (!all(sapply(pkgs, require, character.only=TRUE, quietly=TRUE))) { source('src/r/requirements.R') }"

    echo ""
    echo "[R 1/3] BSTS ..."
    Rscript src/r/01_bsts.R

    echo ""
    echo "[R 2/3] SDiD (raw log GDP pc) ..."
    Rscript src/r/02_sdid_raw.R

    echo ""
    echo "[R 3/3] SDiD (residualized) ..."
    Rscript src/r/03_sdid_resid.R
else
    echo ""
    echo "[R] Skipping R scripts (Rscript not found)."
fi