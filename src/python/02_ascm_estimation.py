import sys
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

sys.path.insert(0, str(Path(__file__).resolve().parent))
from config_loader import cfg
from ascm_core import build_predictors, build_outcome_matrix, fit_ascm_panel

print("Loading panel_balanced.csv ...")
df = pd.read_csv(cfg.DATA_PROCESSED / "panel_balanced.csv")
print(f"  {df['country'].nunique()} countries, {df['year'].nunique()} years, {len(df)} rows")

NMK_SAMPLE = cfg.EU_2004_DONORS + [cfg.TREATED_UNIT]
DONORS     = cfg.EU_2004_DONORS
df_nmk     = df[df["country"].isin(NMK_SAMPLE)].copy()


def run_nmk(outcome):
    Y = build_outcome_matrix(df_nmk, NMK_SAMPLE, cfg.YEARS_ALL, outcome)
    X = build_predictors(df_nmk, cfg.YEARS_PRE, outcome).reindex(NMK_SAMPLE)
    return fit_ascm_panel(
        Y.loc[DONORS, :], Y.loc[cfg.TREATED_UNIT, :],
        X.loc[DONORS],    X.loc[[cfg.TREATED_UNIT]],
        cfg.YEARS_PRE, cfg.YEARS_POST,
    )


def block_mean(series, years):
    yrs = [y for y in years if y in series.index]
    return float(series.loc[yrs].mean()) if yrs else np.nan


def exp_pct(x):
    return 100.0 * (np.exp(x) - 1.0)


print("\nBuilding Table 1: pre-treatment predictor matrix ...")
X_pre = build_predictors(df_nmk, cfg.YEARS_PRE, "log_gdp_pc").reindex(NMK_SAMPLE)
t1 = X_pre.copy()
t1.index.name = "country"
t1.to_csv(cfg.PAPER_TABLES / "table_1_predictors.csv")
print(f"  Saved: table_1_predictors.csv")
print(t1.to_string())

print("\nBuilding Table 2: donor weights ...")
res_main = run_nmk("log_gdp_pc")
w = res_main["w"]
t2 = w.reset_index()
t2.columns = ["donor", "weight"]
t2 = t2.sort_values("weight", ascending=False).reset_index(drop=True)
t2.to_csv(cfg.PAPER_TABLES / "table_2_nm_weights.csv", index=False)
print(f"  Saved: table_2_nm_weights.csv")
print(t2.to_string(index=False))

print("\nBuilding Table 3: main ASCM estimates for North Macedonia ...")
outcomes_main = [
    ("log_gdp_pc",     "Log GDP per capita"),
    ("log_gdp_emp",    "Log GDP per worker"),
    ("trade_openness", "Trade openness"),
]

rows = []
for outcome, label in outcomes_main:
    r = run_nmk(outcome)
    pre_rmse  = r["pre_rmse"]
    post_rmse = r["post_rmse"]
    att_avg   = r["att_post_avg"]
    att_b1    = block_mean(r["gap_post"], cfg.BLOCK1)
    att_b2    = block_mean(r["gap_post"], cfg.BLOCK2)
    row = {
        "outcome":       label,
        "att_post_avg":  att_avg,
        "att_2004_2008": att_b1,
        "att_2009_2013": att_b2,
        "pre_rmse":      pre_rmse,
        "post_rmse":     post_rmse,
        "rmspe_ratio":   post_rmse / pre_rmse if pre_rmse > 0 else np.inf,
    }
    if outcome.startswith("log_"):
        row["att_post_avg_pct"]  = exp_pct(att_avg)
        row["att_2004_2008_pct"] = exp_pct(att_b1)
        row["att_2009_2013_pct"] = exp_pct(att_b2)
    rows.append(row)

t3 = pd.DataFrame(rows)
t3.to_csv(cfg.PAPER_TABLES / "table_3_nm_main_results.csv", index=False)
print(f"  Saved: table_3_nm_main_results.csv")
print(t3.to_string(index=False))

print("\nBuilding Figure 1: SCM vs ASCM ...")
r = res_main
fig, ax = plt.subplots(figsize=(11, 6))
ax.plot(r["y_pre"].index,    r["y_pre"].values,    marker="o", label="Treated (pre)")
ax.plot(r["y_post"].index,   r["y_post"].values,   marker="o", label="Treated (post)")
ax.plot(r["syn_pre"].index,  r["syn_pre"].values,  linestyle="--", color="tab:orange", label="SCM counterfactual")
ax.plot(r["syn_post"].index, r["syn_post"].values, linestyle="--", color="tab:orange")
ax.plot(r["cf_pre"].index,   r["cf_pre"].values,   linestyle="-.", color="tab:green",  label="ASCM counterfactual")
ax.plot(r["cf_post"].index,  r["cf_post"].values,  linestyle="-.", color="tab:green")
ax.axvline(x=cfg.TREAT_YEAR - 0.5, linestyle=":", color="grey", label="Treatment (2004)")
ax.set_xlabel("Year"); ax.set_ylabel("Log GDP per capita")
ax.set_title("North Macedonia: SCM vs ASCM")
ax.grid(True); ax.legend()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_1_nm_scm_ascm.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_1_nm_scm_ascm.png")

fig_specs = [
    ("log_gdp_pc",     "Log GDP per capita", "fig_2_nm_gdp_pc.png"),
    ("log_gdp_emp",    "Log GDP per worker", "fig_3_nm_gdp_emp.png"),
    ("trade_openness", "Trade openness",     "fig_4_nm_trade.png"),
]

for outcome, ylab, fname in fig_specs:
    print(f"Building figure: {fname} ...")
    r = run_nmk(outcome)
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.plot(r["y_pre"].index,  r["y_pre"].values,  marker="o", label=f"North Macedonia actual")
    ax.plot(r["y_post"].index, r["y_post"].values, marker="o")
    ax.plot(r["cf_pre"].index, r["cf_pre"].values, linestyle="--", label="Synthetic benchmark")
    ax.plot(r["cf_post"].index,r["cf_post"].values,linestyle="--")
    ax.axvline(cfg.TREAT_YEAR - 0.5, linestyle=":", color="gray")
    ax.set_xlabel("Year"); ax.set_ylabel(ylab)
    ax.set_title(f"North Macedonia: {ylab} vs synthetic benchmark")
    ax.grid(True); ax.legend()
    fig.tight_layout()
    fig.savefig(cfg.PAPER_FIGURES / fname, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"  Saved: {fname}")

print("\n02_ascm_estimation.py complete.")