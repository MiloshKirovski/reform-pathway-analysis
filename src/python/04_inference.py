import sys
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import warnings
from sklearn.exceptions import UndefinedMetricWarning

warnings.filterwarnings("ignore", category=UndefinedMetricWarning)
warnings.filterwarnings("ignore", category=UserWarning)

sys.path.insert(0, str(Path(__file__).resolve().parent))
from config_loader import cfg
from ascm_core import build_predictors, build_outcome_matrix, fit_ascm_panel

print("Loading panel_balanced.csv ...")
df = pd.read_csv(cfg.DATA_PROCESSED / "panel_balanced.csv")
print(f"  {df['country'].nunique()} countries, {len(df)} rows")

NMK_SAMPLE = cfg.EU_2004_DONORS + [cfg.TREATED_UNIT]
DONORS     = cfg.EU_2004_DONORS
df_nmk     = df[df["country"].isin(NMK_SAMPLE)].copy()


def _rmse(x):
    return float(np.sqrt(np.mean(np.asarray(x, float) ** 2)))


def _block(s, yrs):
    yrs = [y for y in yrs if y in s.index]
    return float(s.loc[yrs].mean()) if yrs else np.nan


def run_ascm(target, donors, outcome="log_gdp_pc", pre_years=None, post_years=None, df_source=None):
    if pre_years  is None: pre_years  = cfg.YEARS_PRE
    if post_years is None: post_years = cfg.YEARS_POST
    if df_source  is None: df_source  = df_nmk
    years = list(pre_years) + list(post_years)
    case  = donors + [target]
    sub   = df_source[df_source["country"].isin(case)].copy()
    Y = build_outcome_matrix(sub, case, years, outcome)
    X = build_predictors(sub, pre_years, outcome).reindex(case)
    return fit_ascm_panel(
        Y.loc[donors, :], Y.loc[target, :],
        X.loc[donors, :], X.loc[[target], :],
        years_pre=pre_years, years_post=post_years,
    )

print("\nBuilding Table 5: placebo in space ...")
rows = []
for pseudo_target in NMK_SAMPLE:
    pseudo_donors = [c for c in NMK_SAMPLE if c != pseudo_target]
    try:
        r = run_ascm(pseudo_target, pseudo_donors)
    except Exception as e:
        print(f"  WARNING: {pseudo_target} failed: {e}")
        continue
    rows.append({
        "country":       pseudo_target,
        "att_post_avg":  float(r["gap_post"].mean()),
        "att_2004_2008": _block(r["gap_post"], cfg.BLOCK1),
        "att_2009_2013": _block(r["gap_post"], cfg.BLOCK2),
        "pre_rmse":      r["pre_rmse"],
        "post_rmse":     r["post_rmse"],
        "rmspe_ratio":   r["post_rmse"] / r["pre_rmse"] if r["pre_rmse"] > 0 else np.inf,
    })

t5 = pd.DataFrame(rows)
t5 = t5.reindex(t5["att_2009_2013"].abs().sort_values(ascending=False).index).reset_index(drop=True)

nm_abs  = abs(t5.loc[t5["country"] == cfg.TREATED_UNIT, "att_2009_2013"].iloc[0])
rank    = int((t5["att_2009_2013"].abs() >= nm_abs).sum())
p_value = rank / len(t5)
t5["placebo_rank"]    = (t5["att_2009_2013"].abs().rank(ascending=False)).astype(int)
t5["placebo_p_value"] = t5["att_2009_2013"].abs().rank(ascending=False) / len(t5)

t5.to_csv(cfg.PAPER_TABLES / "table_5_placebo_space.csv", index=False)
print(f"  Saved: table_5_placebo_space.csv")
print(f"  NMK rank: {rank} / {len(t5)}  (p = {p_value:.3f})")
print(t5[["country", "att_post_avg", "att_2004_2008", "att_2009_2013", "pre_rmse", "post_rmse", "rmspe_ratio"]].to_string(index=False))

colors = ["tab:blue" if c == cfg.TREATED_UNIT else "lightgrey" for c in t5["country"]]
fig, ax = plt.subplots(figsize=(10, 4))
ax.bar(t5["country"], t5["att_2009_2013"], color=colors)
ax.axhline(0, linestyle="--")
ax.set_ylabel("2009-2013 average gap")
ax.set_title(f"Placebo in space | NMK rank={rank}/{len(t5)}, p={p_value:.3f}")
plt.xticks(rotation=45, ha="right")
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_13_nm_placebo_space.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_13_nm_placebo_space.png")

print("\nBuilding Table 6: leave-one-out ...")
rows = []
base = run_ascm(cfg.TREATED_UNIT, DONORS)
rows.append({
    "dropped_donor":  "None (baseline)",
    "att_post_avg":   float(base["gap_post"].mean()),
    "att_2004_2008":  _block(base["gap_post"], cfg.BLOCK1),
    "att_2009_2013":  _block(base["gap_post"], cfg.BLOCK2),
    "pre_rmse":       base["pre_rmse"],
    "post_rmse":      base["post_rmse"],
    "rmspe_ratio":    base["post_rmse"] / base["pre_rmse"] if base["pre_rmse"] > 0 else np.inf,
})

for dropped in DONORS:
    keep = [d for d in DONORS if d != dropped]
    r = run_ascm(cfg.TREATED_UNIT, keep)
    rows.append({
        "dropped_donor":  dropped,
        "att_post_avg":   float(r["gap_post"].mean()),
        "att_2004_2008":  _block(r["gap_post"], cfg.BLOCK1),
        "att_2009_2013":  _block(r["gap_post"], cfg.BLOCK2),
        "pre_rmse":       r["pre_rmse"],
        "post_rmse":      r["post_rmse"],
        "rmspe_ratio":    r["post_rmse"] / r["pre_rmse"] if r["pre_rmse"] > 0 else np.inf,
    })

t6 = pd.DataFrame(rows).sort_values("att_2009_2013").reset_index(drop=True)
t6.to_csv(cfg.PAPER_TABLES / "table_6_leave_one_out.csv", index=False)
print(f"  Saved: table_6_leave_one_out.csv")
print(t6.to_string(index=False))

baseline_val = t6.loc[t6["dropped_donor"] == "None (baseline)", "att_2009_2013"].iloc[0]
fig, ax = plt.subplots(figsize=(10, 4))
ax.axhline(baseline_val, linestyle="--", label="Baseline")
ax.plot(t6["dropped_donor"], t6["att_2009_2013"], marker="o")
ax.set_ylabel("2009-2013 average gap")
ax.set_title("North Macedonia: leave-one-out robustness (log GDP per capita)")
plt.xticks(rotation=45, ha="right")
ax.legend(); ax.grid(True)
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_14_nm_loo.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_14_nm_loo.png")

print("\nBuilding Table 7: placebo in time ...")
START_YEAR   = 1990
POST_HORIZON = 10
MIN_PRE      = 10

years_avail  = sorted(df_nmk["year"].unique().tolist())
max_year     = max(years_avail)
candidate_t0 = [
    t0 for t0 in years_avail
    if (t0 >= START_YEAR + MIN_PRE) and (t0 + POST_HORIZON - 1 <= max_year)
]
print(f"  Candidate placebo treatment years: {candidate_t0}")

rows = []
for t0 in candidate_t0:
    pre_t  = list(range(START_YEAR, t0))
    post_t = list(range(t0, t0 + POST_HORIZON))
    early  = post_t[:5]
    late   = post_t[5:]

    X_t = build_predictors(df_nmk, pre_t, "log_gdp_pc").reindex(NMK_SAMPLE)
    if X_t.isna().any().any():
        rows.append({"placebo_year": t0, "error": "missing predictors"}); continue
    Y_t = build_outcome_matrix(df_nmk, NMK_SAMPLE, pre_t + post_t, "log_gdp_pc")
    if Y_t.isna().any().any():
        rows.append({"placebo_year": t0, "error": "missing outcomes"}); continue
    try:
        r = fit_ascm_panel(
            Y_t.loc[DONORS, :], Y_t.loc[cfg.TREATED_UNIT, :],
            X_t.loc[DONORS, :], X_t.loc[[cfg.TREATED_UNIT], :],
            years_pre=pre_t, years_post=post_t,
        )
        att_early = float(r["gap_post"].loc[[y for y in early if y in r["gap_post"].index]].mean())
        att_late  = float(r["gap_post"].loc[[y for y in late  if y in r["gap_post"].index]].mean())
        rows.append({
            "placebo_year":  t0,
            "att_post_avg":  float(r["gap_post"].mean()),
            "att_early":     att_early,
            "att_late":      att_late,
            "pre_rmse":      r["pre_rmse"],
            "post_rmse":     r["post_rmse"],
            "rmspe_ratio":   r["post_rmse"] / r["pre_rmse"] if r["pre_rmse"] > 0 else np.inf,
        })
    except Exception as e:
        rows.append({"placebo_year": t0, "error": str(e)})

t7 = pd.DataFrame(rows).sort_values("placebo_year").reset_index(drop=True)
t7.to_csv(cfg.PAPER_TABLES / "table_7_placebo_time.csv", index=False)
print(f"  Saved: table_7_placebo_time.csv")
print(t7.to_string(index=False))

ok = t7.dropna(subset=["att_post_avg"])
fig, ax = plt.subplots(figsize=(8, 4))
ax.plot(ok["placebo_year"], ok["att_post_avg"], marker="o")
ax.axvline(cfg.TREAT_YEAR, linestyle=":", color="gray", label=f"True treatment ({cfg.TREAT_YEAR})")
ax.set_xlabel("Placebo treatment year")
ax.set_ylabel("Average post gap (log GDP per capita)")
ax.set_title("North Macedonia: placebo in time")
ax.legend(); ax.grid(True)
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_15_nm_placebo_time.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_15_nm_placebo_time.png")

print("\n04_inference.py complete.")