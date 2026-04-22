import numpy as np
import pandas as pd
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from config_loader import cfg

print(f"Reading PWT from: {cfg.PWT_XLSX}")
df_raw = pd.read_excel(cfg.PWT_XLSX, sheet_name="Data")
print(f"  Raw rows: {len(df_raw)}")

ALL_COUNTRIES = cfg.EU_2004_DONORS + cfg.BALKANS

need_cols = ["country", "year", "csh_x", "csh_m", "csh_i", "pop", "rgdpe", "emp"]
df = df_raw[need_cols].copy()
df = df[df["country"].isin(ALL_COUNTRIES)]
print(f"  After country filter ({len(ALL_COUNTRIES)} countries): {len(df)} rows")

df = df[df["year"].isin(cfg.YEARS_ALL)]
print(f"  After year filter ({cfg.YEARS_ALL[0]}–{cfg.YEARS_ALL[-1]}): {len(df)} rows")

df = df[(df["pop"] > 0) & (df["emp"] > 0) & (df["rgdpe"] > 0)]
print(f"  After positive pop/emp/rgdpe filter: {len(df)} rows")

df["trade_openness"] = df["csh_x"] + df["csh_m"]
df["gdp_pc"]        = df["rgdpe"] / df["pop"]
df["gdp_emp"]       = df["rgdpe"] / df["emp"]
df["log_gdp_pc"]    = np.log(df["gdp_pc"].astype(float))
df["log_gdp_emp"]   = np.log(df["gdp_emp"].astype(float))
df["log_pop"]       = np.log(df["pop"].astype(float))
df["log_emp"]       = np.log(df["emp"].astype(float))
df["inv_share"]     = df["csh_i"]

unit_counts = df.groupby("country")["year"].nunique()
balanced_countries = unit_counts[unit_counts == len(cfg.YEARS_ALL)].index.tolist()
dropped = set(ALL_COUNTRIES) - set(balanced_countries)
if dropped:
    print(f"  WARNING: dropping unbalanced countries: {dropped}")
df = df[df["country"].isin(balanced_countries)]
print(f"  After balance check: {df['country'].nunique()} countries, {len(df)} rows")

pivot_check = df.pivot(index="country", columns="year", values="log_gdp_pc")
if pivot_check.isna().any().any():
    raise ValueError("Panel still has NaNs after balance filter. Check source data.")

df = df.sort_values(["country", "year"]).reset_index(drop=True)

export_cols = [
    "country", "year",
    "log_gdp_pc", "log_gdp_emp", "trade_openness",
    "gdp_pc", "csh_i", "inv_share", "pop", "emp",
    "log_pop", "log_emp", "rgdpe",
]
df[export_cols].to_csv(cfg.DATA_PROCESSED / "panel_balanced.csv", index=False)
print(f"  Saved: {cfg.DATA_PROCESSED / 'panel_balanced.csv'}")

df_sdid = df.loc[df["country"].isin(cfg.EU_2004_DONORS + [cfg.TREATED_UNIT]),
                 ["country", "year", "log_gdp_pc"]].copy()
df_sdid.columns = ["unit", "time", "Y"]
df_sdid["treated"] = (
    (df_sdid["unit"] == cfg.TREATED_UNIT) &
    (df_sdid["time"] >= cfg.TREAT_YEAR)
).astype(int)
df_sdid.to_csv(cfg.DATA_PROCESSED / "pwt_sdid_nm_raw.csv", index=False)
print(f"  Saved: {cfg.DATA_PROCESSED / 'pwt_sdid_nm_raw.csv'}")

df_sdid_r = df.loc[df["country"].isin(cfg.EU_2004_DONORS + [cfg.TREATED_UNIT])].copy()
df_sdid_r["unit"] = df_sdid_r["country"]
df_sdid_r["time"] = df_sdid_r["year"]
df_sdid_r["treated"] = (
    (df_sdid_r["unit"] == cfg.TREATED_UNIT) &
    (df_sdid_r["time"] >= cfg.TREAT_YEAR)
).astype(int)

covars = ["inv_share", "log_pop", "log_emp"]
mask_pre = ~(
    (df_sdid_r["unit"] == cfg.TREATED_UNIT) &
    (df_sdid_r["time"] >= cfg.TREAT_YEAR)
)
df_fit = df_sdid_r.loc[mask_pre, ["log_gdp_pc"] + covars].dropna()
X_fit  = np.column_stack([np.ones(len(df_fit))] + [df_fit[c].values for c in covars])
beta, *_ = np.linalg.lstsq(X_fit, df_fit["log_gdp_pc"].values, rcond=None)

df_all_cov = df_sdid_r[["log_gdp_pc"] + covars].dropna()
X_all = np.column_stack([np.ones(len(df_all_cov))] + [df_all_cov[c].values for c in covars])
df_sdid_r["Y_resid"] = np.nan
df_sdid_r.loc[df_all_cov.index, "Y_resid"] = df_all_cov["log_gdp_pc"].values - (X_all @ beta)

rich_cols = ["unit", "time", "treated", "log_gdp_pc", "Y_resid",
             "trade_openness", "inv_share", "log_pop", "log_emp", "log_gdp_emp"]
df_sdid_r[rich_cols].sort_values(["unit", "time"]).to_csv(
    cfg.DATA_PROCESSED / "pwt_sdid_nm_rich_res.csv", index=False)
print(f"  Saved: {cfg.DATA_PROCESSED / 'pwt_sdid_nm_rich_res.csv'}")

def build_bsts_wide(treated, donors, years, out_path):
    countries = [treated] + donors
    d = df_raw[df_raw["country"].isin(countries) & df_raw["year"].isin(years)].copy()
    d = d[(d["pop"] > 0) & (d["rgdpe"] > 0)]
    d["log_gdp_pc"] = np.log(d["rgdpe"] / d["pop"])
    wide = (
        d.pivot(index="year", columns="country", values="log_gdp_pc")
         .reindex(index=years, columns=countries)
    )
    ok_donors = [c for c in donors if c in wide.columns and wide[c].notna().all()]
    wide = wide[[treated] + ok_donors].dropna()
    wide = wide.rename(columns={treated: "y"})
    wide = wide.rename(columns={c: f"x_{c}" for c in ok_donors})
    wide.to_csv(out_path, index=True)
    print(f"  Saved: {out_path}  ({len(ok_donors)} donors, {len(wide)} years)")

build_bsts_wide(cfg.TREATED_UNIT, cfg.DONORS_NON_EU_BSTS,
                cfg.YEARS_BSTS, cfg.DATA_PROCESSED / "pwt_bsts_nm_non_eu.csv")
build_bsts_wide(cfg.TREATED_UNIT, cfg.DONORS_EU2004_BSTS,
                cfg.YEARS_BSTS, cfg.DATA_PROCESSED / "pwt_bsts_nm_eu2004.csv")

print("\n01_data_prep.py complete.")