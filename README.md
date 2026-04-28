## The Cost of Non-Membership: Estimating the Foregone GDP Growth of Western Balkan Countries from EU Exclusion, 2004-2013


### Requirements

**Python 3.10+**

```bash
pip install -r requirements.txt
```

**R 4.2+** (only needed for the BSTS and SDiD robustness checks, not required for the core ASCM results)
Packages are installed within the sh script.

## How to reproduce all results

From the project root, run:

```
chmod +x run_pipeline.sh
./run_pipeline.sh
```

This runs the following steps in order:

| Step | Script                             | What it does                                                 |
|------|------------------------------------|--------------------------------------------------------------|
| 1 | `src/python/01_data_prep.py`       | Reads PWT, builds the balanced panel, exports processed CSVs |
| 2 | `src/python/02_ascm_estimation.py` | NMK main ASCM - Tables 1, 2, 3 and Figures 1–4               |
| 3 | `src/python/03_cross_country.py`   | All Balkans ASCM - Table 4 and Figures 5–8                   |
| 4 | `src/python/04_inference.py`       | LOO + placebo tests - Tables 5, 6, 7 and Figures 9–11        |
| 5 | `src/r/requirements.R`             | Installs R requirements                                      |
| 6 | `src/r/01_bsts.R`                  | BSTS robustness (not core)                                   |
| 7 | `src/r/02_sdid_raw.R`              | SDiD on raw log GDP per capita (not core)                    |
| 8 | `src/r/03_sdid_resid.R`            | SDiD on residualised outcome (not core)                      |

All outputs are written to:

```
paper_outputs/
  tables/    CSV files for every table in the paper
  figures/   PNG files for every figure in the paper
```


If you want to change the sample, treatment year, donor pool, or any other parameter, edit `src/python/00_config.py`. That is the only file you should need to touch.

---

## Output files

The core paper outputs are:

| File | Content |
|------|---------|
| `tables/table_1_predictors.csv` | Pre-treatment predictor balance (NMK) |
| `tables/table_2_nm_weights.csv` | ASCM donor weights for North Macedonia |
| `tables/table_3_nm_main_results.csv` | Main ATT estimates for North Macedonia |
| `tables/table_4_cross_country_results.csv` | ATT estimates for all Western Balkans |
| `tables/table_5_placebo_space.csv` | Placebo-in-space results and rank p-values |
| `tables/table_6_leave_one_out.csv` | Leave-one-out donor robustness |
| `tables/table_7_placebo_time.csv` | Placebo-in-time results |
