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
print(f"  {df['country'].nunique()} countries, {len(df)} rows")

ALL_COUNTRIES = cfg.EU_2004_DONORS + cfg.BALKANS
df_all = df[df["country"].isin(ALL_COUNTRIES)].copy()


def run_case(target, outcome="log_gdp_pc", donor_pool=None, pre_years=None, post_years=None):
    if donor_pool is None:
        donor_pool = cfg.EU_2004_DONORS
    if pre_years is None:
        pre_years = cfg.YEARS_PRE
    if post_years is None:
        post_years = cfg.YEARS_POST
    years = list(pre_years) + list(post_years)
    case_countries = donor_pool + [target]
    sub = df_all[df_all["country"].isin(case_countries)].copy()
    Y = build_outcome_matrix(sub, case_countries, years, outcome)
    X = build_predictors(sub, pre_years, outcome).reindex(case_countries)
    donors = [c for c in case_countries if c != target]
    return fit_ascm_panel(
        Y.loc[donors, :], Y.loc[target, :],
        X.loc[donors, :], X.loc[[target], :],
        years_pre=pre_years, years_post=post_years,
    )


def exp_pct(x):
    return 100.0 * (np.exp(x) - 1.0)


def block_mean(s, yrs):
    yrs = [y for y in yrs if y in s.index]
    return float(s.loc[yrs].mean()) if yrs else np.nan


print("Building Table 4: cross-country results (all Balkans, log GDP per capita) ...")

rows = []
for target in cfg.BALKANS:
    try:
        r = run_case(target, "log_gdp_pc")
    except Exception as e:
        print(f"  WARNING: {target} failed: {e}")
        continue

    pre_rmse  = r["pre_rmse"]
    post_rmse = r["post_rmse"]
    att_avg   = r["att_post_avg"]
    att_b1    = block_mean(r["gap_post"], cfg.BLOCK1)
    att_b2    = block_mean(r["gap_post"], cfg.BLOCK2)
    w_sorted  = r["w"].sort_values(ascending=False)
    rows.append({
        "country":        target,
        "att_post_avg":   att_avg,
        "att_2004_2008":  att_b1,
        "att_2009_2013":  att_b2,
        "pre_rmse":       pre_rmse,
        "post_rmse":      post_rmse,
        "rmspe_ratio":    post_rmse / pre_rmse if pre_rmse > 0 else np.inf,
        "att_post_avg_pct":  exp_pct(att_avg),
        "att_2004_2008_pct": exp_pct(att_b1),
        "att_2009_2013_pct": exp_pct(att_b2),
        "top_donors":  ", ".join(w_sorted.head(5).index.tolist()),
        "top_weights": ", ".join([f"{v:.3f}" for v in w_sorted.head(5).values]),
    })

t4 = pd.DataFrame(rows)
t4.to_csv(cfg.PAPER_TABLES / "table_4_cross_country_results.csv", index=False)
print(f"  Saved: table_4_cross_country_results.csv")
print(t4[["country", "att_post_avg", "att_2004_2008", "att_2009_2013", "pre_rmse", "rmspe_ratio"]].to_string(index=False))

print("Building Figure 5: Balkans gap series ...")
MAIN_TARGETS = ["North Macedonia", "Albania", "Serbia", "Montenegro"]

fig, ax = plt.subplots(figsize=(10, 5))
for target in MAIN_TARGETS:
    r   = run_case(target, "log_gdp_pc")
    gap = pd.concat([r["gap_pre"], r["gap_post"]])
    ax.plot(gap.index, gap.values, marker="o", label=target)
ax.axhline(0, linestyle="--")
ax.axvline(cfg.TREAT_YEAR - 0.5, linestyle=":", color="gray")
ax.set_xlabel("Year"); ax.set_ylabel("Gap in log GDP per capita")
ax.set_title("Western Balkans: GDP per capita gap series")
ax.grid(True); ax.legend()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_5_balkans_gap_gdp_pc.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_5_balkans_gap_gdp_pc.png")

print("Building Figures 6-7: Balkans average ...")


def build_balkan_avg(outcome):
    actuals, synths, gaps = [], [], []
    for target in cfg.BALKANS:
        try:
            r = run_case(target, outcome)
        except Exception:
            continue
        actuals.append(pd.concat([r["y_pre"],   r["y_post"]]).rename(target))
        synths.append( pd.concat([r["cf_pre"],  r["cf_post"]]).rename(target))
        gaps.append(   pd.concat([r["gap_pre"], r["gap_post"]]).rename(target))
    return (
        pd.concat(actuals, axis=1).mean(axis=1),
        pd.concat(synths,  axis=1).mean(axis=1),
        pd.concat(gaps,    axis=1).mean(axis=1),
    )


actual_avg, synth_avg, gap_avg = build_balkan_avg("log_gdp_pc")

fig, ax = plt.subplots(figsize=(10, 5))
ax.plot(actual_avg.index, actual_avg.values, marker="o", label="Western Balkans average actual")
ax.plot(synth_avg.index,  synth_avg.values,  linestyle="--", label="Western Balkans average synthetic benchmark")
ax.axvline(cfg.TREAT_YEAR - 0.5, linestyle=":", color="gray")
ax.set_xlabel("Year"); ax.set_ylabel("Log GDP per capita")
ax.set_title("Western Balkans average: GDP per capita vs synthetic benchmark")
ax.grid(True); ax.legend()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_6_balkans_avg_actual_synth.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_6_balkans_avg_actual_synth.png")

fig, ax = plt.subplots(figsize=(10, 5))
ax.plot(gap_avg.index, gap_avg.values, marker="o")
ax.axhline(0, linestyle="--")
ax.axvline(cfg.TREAT_YEAR - 0.5, linestyle=":", color="gray")
ax.set_xlabel("Year"); ax.set_ylabel("Average gap in log GDP per capita")
ax.set_title("Western Balkans average GDP per capita gap")
ax.grid(True)
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_7_balkans_avg_gap.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_7_balkans_avg_gap.png")

print("Building Figure 8: bar chart ...")
bar_rows = []
for target in cfg.BALKANS:
    try:
        r = run_case(target, "log_gdp_pc")
    except Exception:
        continue
    yrs = [y for y in range(2004, 2014) if y in r["gap_post"].index]
    bar_rows.append({"country": target, "avg_gap": float(r["gap_post"].loc[yrs].mean())})

bar_df = pd.DataFrame(bar_rows).sort_values("avg_gap")
fig, ax = plt.subplots(figsize=(9, 5))
ax.bar(bar_df["country"], bar_df["avg_gap"])
ax.axhline(0, linestyle="--")
ax.set_ylabel("Average 2004-2013 gap (log points)")
ax.set_title("Western Balkans: average 2004-2013 GDP per capita gap")
plt.xticks(rotation=30, ha="right")
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_8_balkans_bar_gap.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_8_balkans_bar_gap.png")

print("Building GDP per worker and trade openness figures ...")

fig, ax = plt.subplots(figsize=(10, 5))
for target in MAIN_TARGETS:
    r   = run_case(target, "log_gdp_emp")
    gap = pd.concat([r["gap_pre"], r["gap_post"]])
    ax.plot(gap.index, gap.values, marker="o", label=target)
ax.axhline(0, linestyle="--")
ax.axvline(cfg.TREAT_YEAR - 0.5, linestyle=":", color="gray")
ax.set_xlabel("Year"); ax.set_ylabel("Gap in log GDP per worker")
ax.set_title("Western Balkans: GDP per worker gap series")
ax.grid(True); ax.legend()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_9_balkans_gap_gdp_emp.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_9_balkans_gap_gdp_emp.png")

bar_rows = []
for target in cfg.BALKANS:
    try:
        r = run_case(target, "log_gdp_emp")
    except Exception:
        continue
    yrs = [y for y in range(2004, 2014) if y in r["gap_post"].index]
    bar_rows.append({"country": target, "avg_gap": float(r["gap_post"].loc[yrs].mean())})
bar_df = pd.DataFrame(bar_rows).sort_values("avg_gap")
fig, ax = plt.subplots(figsize=(9, 5))
ax.bar(bar_df["country"], bar_df["avg_gap"])
ax.axhline(0, linestyle="--")
ax.set_ylabel("Average 2004-2013 gap (log points)")
ax.set_title("Western Balkans: average 2004-2013 GDP per worker gap")
plt.xticks(rotation=30, ha="right")
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_10_balkans_bar_gdp_emp.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_10_balkans_bar_gdp_emp.png")

fig, ax = plt.subplots(figsize=(10, 5))
for target in MAIN_TARGETS:
    r   = run_case(target, "trade_openness")
    gap = pd.concat([r["gap_pre"], r["gap_post"]])
    ax.plot(gap.index, gap.values, marker="o", label=target)
ax.axhline(0, linestyle="--")
ax.axvline(cfg.TREAT_YEAR - 0.5, linestyle=":", color="gray")
ax.set_xlabel("Year"); ax.set_ylabel("Gap in trade openness")
ax.set_title("Western Balkans: trade openness gap series")
ax.grid(True); ax.legend()
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_11_balkans_gap_trade.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_11_balkans_gap_trade.png")

bar_rows = []
for target in cfg.BALKANS:
    try:
        r = run_case(target, "trade_openness")
    except Exception:
        continue
    yrs = [y for y in range(2004, 2014) if y in r["gap_post"].index]
    bar_rows.append({"country": target, "avg_gap": float(r["gap_post"].loc[yrs].mean())})
bar_df = pd.DataFrame(bar_rows).sort_values("avg_gap")
fig, ax = plt.subplots(figsize=(9, 5))
ax.bar(bar_df["country"], bar_df["avg_gap"])
ax.axhline(0, linestyle="--")
ax.set_ylabel("Average 2004-2013 gap (trade openness)")
ax.set_title("Western Balkans: average 2004-2013 trade openness gap")
plt.xticks(rotation=30, ha="right")
fig.tight_layout()
fig.savefig(cfg.PAPER_FIGURES / "fig_12_balkans_bar_trade.png", dpi=300, bbox_inches="tight")
plt.close(fig)
print(f"  Saved: fig_12_balkans_bar_trade.png")

print("\n03_cross_country.py complete.")