library(synthdid)

run_sdid_with_effects <- function(df, block1 = 2004:2008, block2 = 2009:2013, mass = 0.99) {
  setup <- panel.matrices(df)
  est <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  
  eff_post <- as.numeric(synthdid_effect_curve(est))
  
  years <- sort(unique(df$time))
  post_years <- years[(setup$T0 + 1):length(years)]
  eff_post_ts <- setNames(eff_post, post_years)
  
  att_post_avg <- mean(eff_post_ts, na.rm = TRUE)
  att_block1 <- mean(eff_post_ts[as.character(block1)], na.rm = TRUE)
  att_block2 <- mean(eff_post_ts[as.character(block2)], na.rm = TRUE)
  
  omega_tbl <- synthdid_controls(est, weight.type = "omega", mass = mass)
  lambda_tbl <- synthdid_controls(est, weight.type = "lambda", mass = mass)
  
  list(
    estimate = est,
    eff_post = eff_post_ts,
    att_post_avg = att_post_avg,
    att_2004_2008 = att_block1,
    att_2009_2013 = att_block2,
    omega_table = omega_tbl,
    lambda_table = lambda_tbl
  )
}

plot_sdid_effect <- function(res, block1 = 2004:2008, block2 = 2009:2013) {
  eff <- res$eff_post
  yrs <- as.numeric(names(eff))
  
  plot(
    yrs, eff, type = "b", pch = 19,
    xlab = "Year",
    ylab = "SDiD Effect (log GDP per capita)",
    main = "SDiD Effect Curve: North Macedonia"
  )
  abline(h = 0, lty = 2, col = "gray")
  abline(v = max(block1), lty = 3, col = "gray")
  text(mean(block1), max(eff, na.rm = TRUE), "2004–08", pos = 3)
  text(mean(block2), max(eff, na.rm = TRUE), "2009–13", pos = 3)
}

block_placebo_ci <- function(df, years_block, nboot = 200) {
  setup <- panel.matrices(df)
  est <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  
  eff <- as.numeric(synthdid_effect_curve(est))
  yrs <- sort(unique(df$time))
  post_years <- yrs[(setup$T0 + 1):length(yrs)]
  
  idx <- which(post_years %in% years_block)
  tau_block <- mean(eff[idx], na.rm = TRUE)
  
  se <- sqrt(vcov(est, method = "placebo", nboot = nboot))
  
  c(
    estimate = tau_block,
    ci_low = tau_block - 1.96 * se,
    ci_high = tau_block + 1.96 * se
  )
}

sdid_loo <- function(df, block = 2009:2013) {
  units <- setdiff(unique(df$unit), "North Macedonia")
  out <- data.frame(unit_dropped = units, att = NA_real_)
  
  for (i in seq_along(units)) {
    df_i <- subset(df, unit != units[i])
    res_i <- run_sdid_with_effects(df_i, block1 = block, block2 = block)
    out$att[i] <- res_i$att_2009_2013
  }
  
  out
}

sdid_donor_drop <- function(df, block = 2009:2013, max_drop = 3) {
  res0 <- run_sdid_with_effects(df)
  donors <- rownames(res0$omega_table)
  
  out <- data.frame(step = 0:max_drop, att = NA_real_)
  out$att[1] <- res0$att_2009_2013
  
  df_curr <- df
  
  for (k in 1:max_drop) {
    drop_unit <- donors[k]
    df_curr <- subset(df_curr, unit != drop_unit)
    res_k <- run_sdid_with_effects(df_curr)
    out$att[k + 1] <- res_k$att_2009_2013
  }
  
  out
}

df <- read.csv("pwt_sdid_nm_raw.csv")
df_raw <- df[, c("unit", "time", "Y", "treated")]
df_raw <- na.omit(df_raw)

res <- run_sdid_with_effects(df_raw, block1 = 2004:2008, block2 = 2009:2013, mass = 0.99)

print(res$att_post_avg)
print(res$att_2004_2008)
print(res$att_2009_2013)
print(res$omega_table)
print(res$lambda_table)

plot_sdid_effect(res)

print(block_placebo_ci(df_raw, 2004:2008))
print(block_placebo_ci(df_raw, 2009:2013))

loo_res <- sdid_loo(df_raw, block = 2009:2013)
print(loo_res)

donor_drop_res <- sdid_donor_drop(df_raw, block = 2009:2013, max_drop = 3)
print(donor_drop_res)