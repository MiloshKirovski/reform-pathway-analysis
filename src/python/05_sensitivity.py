import sys
from pathlib import Path
from itertools import combinations
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import warnings

from IPython.utils.coloransi import value
from sklearn.exceptions import UndefinedMetricWarning

warnings.filterwarnings("ignore", category=UndefinedMetricWarning)
warnings.filterwarnings("ignore", category=UserWarning)

sys.path.insert(0, str(Path(__file__).resolve().parent))
from config_loader import cfg
from ascm_core import (
    build_predictors,
    build_outcome_matrix,
    fit_ascm_panel,
    scm_weights,
)


df = pd.read_csv(cfg.DATA_PROCESSED / "panel_balanced.csv")
print(f"{df['country'].nunique()} countries, {len(df)} rows")

NMK_SAMPLE = cfg.EU_2004_DONORS + [cfg.TREATED_UNIT]
DONORS = cfg.EU_2004_DONORS
df_nmk = df[df["country"].isin(NMK_SAMPLE)].copy()

def _block(s, yrs):
    yrs = [y for y in yrs if y in s.index]
    return float(s.loc[yrs].mean()) if yrs else np.nan


def run_ascm(target, donors, outcome="log_gdp_pc", pre_years=None, post_years=None, df_source=None,
             predictor_fn=None, alphas=None):
    if pre_years is None: pre_years = cfg.YEARS_PRE
    if post_years is None: post_years = cfg.YEARS_POST
    if df_source is None: df_source = df_nmk
    if predictor_fn is None: predictor_fn = lambda d, pyrs: build_predictors(d, pyrs, outcome)

    years = list(pre_years) + list(post_years)
    case = donors + [target]
    sub = df_source[df_source["country"].isin(case)].copy()
    Y = build_outcome_matrix(sub, case, years, outcome)
    X = predictor_fn(sub, pre_years).reindex(case)
    kwargs = {}
    if alphas is not None:
        kwargs["alphas"] = alphas
    return fit_ascm_panel(
        Y.loc[donors, :], Y.loc[target, :],
        X.loc[donors, :], X.loc[[target], :],
        years_pre=pre_years, years_post=post_years,
        **kwargs,
    )


def summarize(res, label):
    return {
        "spec": label,
        "att_post_avg": float(res["gap_post"].mean()),
        "att_2004_2008": _block(res["gap_post"], cfg.BLOCK1),
        "att_2009_2013": _block(res["gap_post"], cfg.BLOCK2),
        "pre_rmse": res["pre_rmse"],
        "post_rmse": res["post_rmse"],
        "rmspe_ratio": res["post_rmse"] / res["pre_rmse"] if res["pre_rmse"] > 0 else np.inf,
    }


print("\nRefitting baseline ASCM for reference")
baseline = run_ascm(cfg.TREATED_UNIT, DONORS)
baseline_row = summarize(baseline, "Baseline (full 10-donor pool)")
print(f"\tbaseline ATT = {baseline_row['att_post_avg']:.3f}  (2009-2013: {baseline_row['att_2009_2013']:.3f})")

print("\nBuilding Table 8: restricted donor pools")
DONOR_SUBSETS = [
    ("Low-income 2004 entrants (EE, LV, LT, PL)", ["Estonia", "Latvia", "Lithuania", "Poland"]),
    ("High-income 2004 entrants (SI, CZ, MT, CY)", ["Slovenia", "Czechia", "Malta", "Cyprus"]),
    ("Visegrad (PL, HU, CZ, SK)", ["Poland", "Hungary", "Czechia", "Slovakia"]),
    ("Baltics only (EE, LV, LT)", ["Estonia", "Latvia", "Lithuania"]),
    ("Central European 5 (PL, HU, CZ, SK, SI)", ["Poland", "Hungary", "Czechia", "Slovakia", "Slovenia"]),
    ("Post-communist donors only (exclude MT, CY)", [d for d in DONORS if d not in ("Malta", "Cyprus")]),
]

rows = [baseline_row]
for label, donor_pool in DONOR_SUBSETS:
    try:
        res = run_ascm(cfg.TREATED_UNIT, donor_pool)
        rows.append(summarize(res, label))
    except Exception as e:
        print(f"  WARNING: subset '{label}' failed: {e}")

t8 = pd.DataFrame(rows)
t8.to_csv(cfg.PAPER_TABLES / "table_8_donor_subsets.csv", index=False)
print(f"\tSaved: table_8_donor_subsets.csv")
print(t8.to_string(index=False))

fig, ax = plt.subplots(figsize=(10, 5))
colors = ["tab:blue" if r["spec"].startswith("Baseline") else "lightgrey" for r in rows]
ax.barh(t8["spec"], t8["att_2009_2013"], color=colors)
ax.axvline(baseline_row["att_2009_2013"], linestyle="--", color="tab:blue",
           label=f"Baseline ({baseline_row['att_2009_2013']:.3f})")
ax.set_xlabel("2009-2013 average gap (log points)")
ax.set_title("North Macedonia: donor-pool sensitivity (log GDP per capita)")
ax.legend()
ax.invert_yaxis()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_16_nm_donor_subsets.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"\tSaved: fig_16_nm_donor_subsets.png")


print("\nTable 9: leave-two-out donor sensitivity")

rows = []
for d1, d2 in combinations(DONORS, 2):
    keep = [d for d in DONORS if d not in (d1, d2)]
    try:
        res = run_ascm(cfg.TREATED_UNIT, keep)
        rows.append({
            "dropped_1":     d1,
            "dropped_2":     d2,
            "att_post_avg":  float(res["gap_post"].mean()),
            "att_2004_2008": _block(res["gap_post"], cfg.BLOCK1),
            "att_2009_2013": _block(res["gap_post"], cfg.BLOCK2),
            "pre_rmse":      res["pre_rmse"],
            "post_rmse":     res["post_rmse"],
            "rmspe_ratio":   res["post_rmse"] / res["pre_rmse"] if res["pre_rmse"] > 0 else np.inf,
        })
    except Exception as e:
        print(f"\tWARNING: drop ({d1},{d2}) failed: {e}")

t9 = pd.DataFrame(rows).sort_values("att_post_avg").reset_index(drop=True)
t9.to_csv(cfg.PAPER_TABLES / "table_9_leave_two_out.csv", index=False)
print(f"\tSaved: table_9_leave_two_out.csv ({len(t9)} combinations)")
print("\tSummary of ATT full-period across all pairs:")
print(f"\tmin    = {t9['att_post_avg'].min():.3f}")
print(f"\tmedian = {t9['att_post_avg'].median():.3f}")
print(f"\tmax    = {t9['att_post_avg'].max():.3f}")
print(f"\tshare with ATT < 0: {(t9['att_post_avg'] < 0).mean():.2%}")

fig, ax = plt.subplots(figsize=(9, 5))
ax.hist(t9["att_post_avg"], bins=15, edgecolor="black", color="lightgrey")
ax.axvline(baseline_row["att_post_avg"], linestyle="--", color="tab:blue",
           label=f"Baseline ({baseline_row['att_post_avg']:.3f})")
ax.axvline(0, linestyle=":", color="black")
ax.set_xlabel("ATT, full period (log points)")
ax.set_ylabel("Number of leave-two-out combinations")
ax.set_title(f"Leave-two-out distribution of ATT ({len(t9)} combinations)")
ax.legend()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_17_nm_leave_two_out.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_17_nm_leave_two_out.png")


print("\nBuilding Table 10: predictor / specification sensitivity")

def _predictors_outcome_lags_only(d, pre_years, outcome="log_gdp_pc"):
    sub = d[d["year"].isin(pre_years)].copy()
    X = sub.pivot(index="country", columns="year", value=outcome)
    return X.reindex(columns=[y for y in pre_years if y in X.columns]).astype(float).sort_index()

def _predictors_structural_only(d, pre_years, outcome="log_gdp_pc"):
    sub = d[d["year"].isin(pre_years)].copy().sort_values(["country", "year"])
    X = pd.DataFrame(index=sorted(sub["country"].unique()))
    X["inv_pre_avg"]        = sub.groupby("country")["csh_i"].mean()
    X["trade_open_pre_avg"] = sub.groupby("country")["trade_openness"].mean()
    X["gdp_emp"]            = sub.groupby("country")["log_gdp_emp"].mean()
    pop = sub.pivot(index="country", columns="year", values="pop").astype(float)
    X["pop_growth_pre_avg"] = np.log(pop).diff(axis=1).mean(axis=1)
    return X.astype(float).sort_index()

def _predictors_three_lags(d, pre_years, outcome="log_gdp_pc"):
    base = build_predictors(d, pre_years, outcome)
    outcome_cols = [c for c in base.columns if isinstance(c, (int, np.integer))]
    structural_cols = [c for c in base.columns if c not in outcome_cols]
    if len(outcome_cols) >= 3:
        keep = [outcome_cols[0], outcome_cols[len(outcome_cols) // 2], outcome_cols[-1]]
    else:
        keep = outcome_cols
    return base[keep + structural_cols]


def _run_scm_only(target, donors, outcome="log_gdp_pc", pre_years=None, post_years=None):
    if pre_years is None: pre_years = cfg.YEARS_PRE
    if post_years is None: post_years = cfg.YEARS_POST
    years = list(pre_years) + list(post_years)
    case = donors + [target]
    sub = df_nmk[df_nmk["country"].isin(case)].copy()
    Y = build_outcome_matrix(sub, case, years, outcome)
    Y0 = Y.loc[donors, :].values.astype(float)
    y1 = Y.loc[target, :].values.astype(float)
    T0 = len(pre_years)
    w, _ = scm_weights(Y0[:, :T0], y1[:T0])
    syn = Y0.T @ w
    gap = y1 - syn
    pre_rmse = float(np.sqrt(np.mean(gap[:T0] ** 2)))
    post_rmse = float(np.sqrt(np.mean(gap[T0:] ** 2)))
    idx_pre = list(pre_years)
    idx_post = list(post_years)
    return {
        "gap_pre": pd.Series(gap[:T0], index=idx_pre),
        "gap_post": pd.Series(gap[T0:], index=idx_post),
        "pre_rmse": pre_rmse,
        "post_rmse": post_rmse,
    }

rows = [baseline_row]

# a) outcome lags only
try:
    res = run_ascm(cfg.TREATED_UNIT, DONORS, predictor_fn=_predictors_outcome_lags_only)
    rows.append(summarize(res, "Outcome lags only (1990-2003)"))
except Exception as e:
    print(f"\tWARNING: outcome-lags-only failed: {e}")

# b) structural predictors only
try:
    res = run_ascm(cfg.TREATED_UNIT, DONORS, predictor_fn=_predictors_structural_only)
    rows.append(summarize(res, "Structural predictors only"))
except Exception as e:
    print(f"\tWARNING: structural-only failed: {e}")

# c) three outcome lags + structural
try:
    res = run_ascm(cfg.TREATED_UNIT, DONORS, predictor_fn=_predictors_three_lags)
    rows.append(summarize(res, "Three outcome lags + structural"))
except Exception as e:
    print(f"\tWARNING: three-lags failed: {e}")

# d) SCM without augmentation (diagnostic reference)
try:
    res = _run_scm_only(cfg.TREATED_UNIT, DONORS)
    rows.append({
        "spec": "SCM only (no ridge augmentation)",
        "att_post_avg": float(res["gap_post"].mean()),
        "att_2004_2008": _block(res["gap_post"], cfg.BLOCK1),
        "att_2009_2013": _block(res["gap_post"], cfg.BLOCK2),
        "pre_rmse": res["pre_rmse"],
        "post_rmse": res["post_rmse"],
        "rmspe_ratio": res["post_rmse"] / res["pre_rmse"] if res["pre_rmse"] > 0 else np.inf,
    })
except Exception as e:
    print(f"\tWARNING: SCM-only failed: {e}")

t10 = pd.DataFrame(rows)
t10.to_csv(cfg.PAPER_TABLES / "table_10_predictor_sensitivity.csv", index=False)
print(f"\tSaved: table_10_predictor_sensitivity.csv")
print(t10.to_string(index=False))

fig, ax = plt.subplots(figsize=(10, 5))
colors = ["tab:blue" if r["spec"].startswith("Baseline") else "lightgrey" for r in rows]
ax.barh(t10["spec"], t10["att_2009_2013"], color=colors)
ax.axvline(baseline_row["att_2009_2013"], linestyle="--", color="tab:blue",
           label=f"Baseline ({baseline_row['att_2009_2013']:.3f})")
ax.set_xlabel("2009-2013 average gap (log points)")
ax.set_title("North Macedonia: predictor / specification sensitivity")
ax.legend()
ax.invert_yaxis()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_18_nm_predictor_sensitivity.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"\tSaved: fig_18_nm_predictor_sensitivity.png")


print("\nBuilding Table 11: pre-period start-year sensitivity")

PRE_START_YEARS = [1990, 1992, 1995, 1998, 2000]

rows = []
for start in PRE_START_YEARS:
    pre_years = list(range(start, cfg.TREAT_YEAR))
    try:
        res = run_ascm(cfg.TREATED_UNIT, DONORS, pre_years=pre_years)
        label = f"Pre-period {start}-{cfg.TREAT_YEAR - 1} ({len(pre_years)} yrs)"
        row = summarize(res, label)
        row["pre_start"] = start
        row["pre_len"] = len(pre_years)
        rows.append(row)
    except Exception as e:
        print(f"  WARNING: pre-start {start} failed: {e}")

t11 = pd.DataFrame(rows)
t11.to_csv(cfg.PAPER_TABLES / "table_11_preperiod_sensitivity.csv", index=False)
print(f"\tSaved: table_11_preperiod_sensitivity.csv")
print(t11.to_string(index=False))

fig, ax = plt.subplots(figsize=(9, 5))
ax.plot(t11["pre_start"], t11["att_post_avg"], marker="o", label="ATT full period")
ax.plot(t11["pre_start"], t11["att_2009_2013"], marker="s", label="ATT 2009-2013")
ax.axhline(baseline_row["att_post_avg"], linestyle="--", color="tab:blue", alpha=0.5, label="Baseline ATT full")
ax.set_xlabel("Pre-period start year")
ax.set_ylabel("Gap (log points)")
ax.set_title("North Macedonia: ATT by pre-period start year")
ax.grid(True)
ax.legend()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_19_nm_preperiod_sensitivity.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"\tSaved: fig_19_nm_preperiod_sensitivity.png")


print("\nBuilding Table 12: ridge alpha grid sensitivity")

ALPHA_GRIDS = [
    ("Baseline grid (1e-4 to 1e4, 60 pts)", np.logspace(-4, 4, 60)),
    ("Narrow grid (1e-2 to 1e2, 40 pts)", np.logspace(-2, 2, 40)),
    ("Wide grid (1e-6 to 1e6, 80 pts)", np.logspace(-6, 6, 80)),
    ("Fixed alpha = 0.01", np.array([0.01])),
    ("Fixed alpha = 1.0", np.array([1.0])),
    ("Fixed alpha = 100", np.array([100.0])),
]

rows = []
for label, alphas in ALPHA_GRIDS:
    try:
        res = run_ascm(cfg.TREATED_UNIT, DONORS, alphas=alphas)
        rows.append(summarize(res, label))
    except Exception as e:
        print(f"\tWARNING: alpha grid '{label}' failed: {e}")

t12 = pd.DataFrame(rows)
t12.to_csv(cfg.PAPER_TABLES / "table_12_ridge_sensitivity.csv", index=False)
print(f"\tSaved: table_12_ridge_sensitivity.csv")
print(t12.to_string(index=False))

print("\n05_sensitivity.py complete.")
