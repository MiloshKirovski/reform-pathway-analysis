library(synthdid)

run_sdid_with_effects <- function(df, block1 = 2004:2008, block2 = 2009:2014, mass = 0.95) {
  setup <- panel.matrices(df)
  est <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  
  eff_post <- as.numeric(synthdid_effect_curve(est))  
  
  years <- sort(unique(df$time))
  T0 <- setup$T0
  post_years <- years[(T0 + 1):length(years)]
  
  eff_post_ts <- setNames(eff_post, post_years)
  
  att_block1 <- mean(eff_post_ts[as.character(block1)], na.rm = TRUE)
  att_block2 <- mean(eff_post_ts[as.character(block2)], na.rm = TRUE)
  
  omega_tbl  <- synthdid_controls(est, weight.type = "omega",  mass = mass)
  lambda_tbl <- synthdid_controls(est, weight.type = "lambda", mass = mass)
  
  list(
    estimate = est,
    eff_post = eff_post_ts,
    att_2004_2008 = att_block1,
    att_2009_2014 = att_block2,
    omega_table = omega_tbl,
    lambda_table = lambda_tbl
  )
}

plot_sdid_effect <- function(res, block1 = 2004:2008, block2 = 2009:2014) {
  eff <- res$eff_post
  yrs <- as.numeric(names(eff))
  
  plot(
    yrs, eff, type = "b", pch = 19,
    xlab = "Year", ylab = "SDiD Effect (residualized GDP)",
    main = "SDiD Effect Curve: North Macedonia"
  )
  abline(h = 0, lty = 2, col = "gray")
  abline(v = max(block1), lty = 3, col = "gray")
  text(mean(block1), max(eff), "2004–08", pos = 3)
  text(mean(block2), max(eff), "2009–14", pos = 3)
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
    ci_low  = tau_block - 1.96 * se,
    ci_high = tau_block + 1.96 * se
  )
}

sdid_loo <- function(df, block = 2009:2014) {
  units <- setdiff(unique(df$unit), "North Macedonia")
  
  out <- data.frame(unit_dropped = units, att = NA_real_)
  
  for (i in seq_along(units)) {
    df_i <- subset(df, unit != units[i])
    res_i <- run_sdid_with_effects(df_i, block1 = block, block2 = block)
    out$att[i] <- res_i$att_2009_2014
  }
  out
}

sdid_pit <- function(df, block = 2009:2014, max_drop = 3) {
  res0 <- run_sdid_with_effects(df)
  donors <- rownames(res0$omega_table)
  
  out <- data.frame(step = 0:max_drop, att = NA_real_)
  out$att[1] <- res0$att_2009_2014
  
  df_curr <- df
  
  for (k in 1:max_drop) {
    drop_unit <- donors[k]
    df_curr <- subset(df_curr, unit != drop_unit)
    res_k <- run_sdid_with_effects(df_curr)
    out$att[k + 1] <- res_k$att_2009_2014
  }
  out
}

df <- read.csv("pwt_sdid_nm_rich_res.csv")

df_res <- df[, c("unit","time","Y_resid","treated")]
names(df_res) <- c("unit","time","Y","treated")

res <- run_sdid_with_effects(df_res, block1 = 2004:2008, block2 = 2009:2014, mass = 0.99)

res$att_2004_2008
res$att_2009_2014
res$omega_table
res$lambda_table

plot_sdid_effect(res)

block_placebo_ci(df_res, 2004:2008)
block_placebo_ci(df_res, 2009:2014)

loo_res <- sdid_loo(df_res)
loo_res

pit_res <- sdid_pit(df_res)
pit_res

