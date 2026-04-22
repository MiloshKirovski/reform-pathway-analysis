import numpy as np
import pandas as pd
from scipy.optimize import minimize
from sklearn.linear_model import RidgeCV
from sklearn.preprocessing import StandardScaler


def build_predictors(df, pre_years, outcome_col="log_gdp_pc"):
    d = df[df["year"].isin(pre_years)].copy().sort_values(["country", "year"])

    X = pd.DataFrame(index=sorted(d["country"].unique()))

    ylag = d.pivot(index="country", columns="year", values=outcome_col)
    yrs  = [y for y in pre_years if y in ylag.columns]
    X[yrs] = ylag[yrs]

    X["inv_pre_avg"]      = d.groupby("country")["csh_i"].mean()
    X["trade_open_pre_avg"] = d.groupby("country")["trade_openness"].mean()
    X["gdp_emp"]          = d.groupby("country")["log_gdp_emp"].mean()

    pop = d.pivot(index="country", columns="year", values="pop").astype(float)
    pop_growth = np.log(pop).diff(axis=1)
    X["pop_growth_pre_avg"] = pop_growth.mean(axis=1)

    return X.astype(float).sort_index()


def scm_weights(Y0_pre, y1_pre, maxiter=20_000, ftol=1e-12):
    Y0 = np.asarray(Y0_pre, float)
    y1 = np.asarray(y1_pre, float).ravel()
    N0 = Y0.shape[0]

    def obj(w):
        d = y1 - Y0.T @ w
        return float(d @ d)

    res = minimize(
        obj,
        np.ones(N0) / N0,
        method="SLSQP",
        bounds=[(0.0, 1.0)] * N0,
        constraints=[{"type": "eq", "fun": lambda w: np.sum(w) - 1.0}],
        options={"maxiter": maxiter, "ftol": ftol},
    )
    if not res.success:
        raise RuntimeError(f"SCM optimization failed: {res.message}")

    w = res.x
    a = float(np.mean(y1 - Y0.T @ w))
    return w, a


def fit_ascm_panel(Y0, Y1, X0, X1, years_pre, years_post, alphas=None):
    if alphas is None:
        alphas = np.logspace(-4, 4, 60)

    if isinstance(Y1, pd.DataFrame):
        if Y1.shape[0] != 1:
            raise ValueError("Y1 must be a Series or a single-row DataFrame.")
        y1_series = Y1.iloc[0]
    else:
        y1_series = Y1

    years_all = list(years_pre) + list(years_post)
    T0        = len(years_pre)

    Y0_mat = Y0[years_all].values.astype(float)
    y1_vec = y1_series[years_all].values.astype(float)

    scaler = StandardScaler()
    X0s    = scaler.fit_transform(X0.values.astype(float))
    X1s    = scaler.transform(X1.values.astype(float))[0]

    m0 = np.empty_like(Y0_mat)
    m1 = np.empty(len(years_all))

    for j, t in enumerate(years_all):
        cv    = min(5, X0.shape[0])
        model = RidgeCV(alphas=alphas, fit_intercept=True, cv=cv)
        model.fit(X0s, Y0_mat[:, j])
        m0[:, j] = model.predict(X0s)
        m1[j]    = model.predict(X1s.reshape(1, -1))[0]

    R0_pre = (Y0_mat - m0)[:, :T0]
    r1_pre = (y1_vec - m1)[:T0]
    w, _   = scm_weights(R0_pre, r1_pre)

    syn_pre  = Y0_mat[:, :T0].T @ w
    syn_post = Y0_mat[:, T0:].T @ w
    adj_pre  = m1[:T0] - (m0[:, :T0].T @ w)
    adj_post = m1[T0:] - (m0[:, T0:].T @ w)

    cf_pre  = syn_pre  + adj_pre
    cf_post = syn_post + adj_post

    y1_pre  = y1_vec[:T0]
    y1_post = y1_vec[T0:]

    gap_pre  = y1_pre  - cf_pre
    gap_post = y1_post - cf_post

    return {
        "w":            pd.Series(w, index=X0.index, name="weight"),
        "y_pre":        pd.Series(y1_pre,  index=years_pre,  name="actual"),
        "y_post":       pd.Series(y1_post, index=years_post, name="actual"),
        "cf_pre":       pd.Series(cf_pre,  index=years_pre,  name="cf"),
        "cf_post":      pd.Series(cf_post, index=years_post, name="cf"),
        "syn_pre":      pd.Series(syn_pre,  index=years_pre,  name="scm"),
        "syn_post":     pd.Series(syn_post, index=years_post, name="scm"),
        "gap_pre":      pd.Series(gap_pre,  index=years_pre,  name="gap"),
        "gap_post":     pd.Series(gap_post, index=years_post, name="gap"),
        "att_post_avg": float(np.mean(gap_post)),
        "pre_rmse":     float(np.sqrt(np.mean(gap_pre  ** 2))),
        "post_rmse":    float(np.sqrt(np.mean(gap_post ** 2))),
    }


def build_outcome_matrix(df, countries, years, outcome):
    return (
        df.pivot(index="country", columns="year", values=outcome)
          .reindex(index=countries, columns=years)
    )