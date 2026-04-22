library(synthdid)

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
HERE <- dirname(script_path)
ROOT        <- normalizePath(file.path(HERE, "..", ".."))
DATA_IN     <- file.path(ROOT, "data", "processed")
TABLES_OUT  <- file.path(ROOT, "paper_outputs", "tables")
FIGURES_OUT <- file.path(ROOT, "paper_outputs", "figures")
dir.create(TABLES_OUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_OUT, showWarnings = FALSE, recursive = TRUE)

run_sdid <- function(df, block1 = 2004:2008, block2 = 2009:2014, mass = 0.95) {
  setup    <- panel.matrices(df)
  est      <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  eff_post <- as.numeric(synthdid_effect_curve(est))
  years    <- sort(unique(df$time))
  post_years  <- years[(setup$T0 + 1):length(years)]
  eff_post_ts <- setNames(eff_post, post_years)
  att_block1  <- mean(eff_post_ts[as.character(block1)], na.rm = TRUE)
  att_block2  <- mean(eff_post_ts[as.character(block2)], na.rm = TRUE)
  omega_tbl   <- synthdid_controls(est, weight.type = "omega",  mass = mass)
  lambda_tbl  <- synthdid_controls(est, weight.type = "lambda", mass = mass)
  list(
    estimate      = est,
    eff_post      = eff_post_ts,
    att_2004_2008 = att_block1,
    att_2009_2014 = att_block2,
    omega_table   = omega_tbl,
    lambda_table  = lambda_tbl
  )
}

plot_effect_curve <- function(res, block1 = 2004:2008, block2 = 2009:2014) {
  eff <- res$eff_post
  yrs <- as.numeric(names(eff))
  plot(yrs, eff, type = "b", pch = 19,
       xlab = "Year", ylab = "SDiD Effect (residualized GDP)",
       main = "SDiD Effect Curve: North Macedonia (residualized)")
  abline(h = 0, lty = 2, col = "gray")
  abline(v = max(block1), lty = 3, col = "gray")
  text(mean(block1), max(eff, na.rm = TRUE), "2004-08", pos = 3)
  text(mean(block2), max(eff, na.rm = TRUE), "2009-14", pos = 3)
}

block_placebo_ci <- function(df, years_block, nboot = 200) {
  setup      <- panel.matrices(df)
  est        <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  eff        <- as.numeric(synthdid_effect_curve(est))
  yrs        <- sort(unique(df$time))
  post_years <- yrs[(setup$T0 + 1):length(yrs)]
  idx        <- which(post_years %in% years_block)
  tau_block  <- mean(eff[idx], na.rm = TRUE)
  se         <- sqrt(vcov(est, method = "placebo", nboot = nboot))
  c(estimate = tau_block, ci_low = tau_block - 1.96 * se, ci_high = tau_block + 1.96 * se)
}

sdid_loo <- function(df, block = 2009:2014) {
  units <- setdiff(unique(df$unit), "North Macedonia")
  out   <- data.frame(unit_dropped = units, att = NA_real_)
  for (i in seq_along(units)) {
    df_i       <- subset(df, unit != units[i])
    res_i      <- run_sdid(df_i, block1 = block, block2 = block)
    out$att[i] <- res_i$att_2009_2014
  }
  out
}

sdid_pit <- function(df, block = 2009:2014, max_drop = 3) {
  res0   <- run_sdid(df)
  donors <- rownames(res0$omega_table)
  out    <- data.frame(step = 0:max_drop, att = NA_real_)
  out$att[1] <- res0$att_2009_2014
  df_curr <- df
  for (k in 1:max_drop) {
    df_curr       <- subset(df_curr, unit != donors[k])
    res_k         <- run_sdid(df_curr)
    out$att[k + 1] <- res_k$att_2009_2014
  }
  out
}

df_full  <- read.csv(file.path(DATA_IN, "pwt_sdid_nm_rich_res.csv"))
df_resid <- df_full[, c("unit", "time", "Y_resid", "treated")]
names(df_resid) <- c("unit", "time", "Y", "treated")
df_resid <- na.omit(df_resid)

res <- run_sdid(df_resid, block1 = 2004:2008, block2 = 2009:2014, mass = 0.95)

cat("att_2004_2008: ", res$att_2004_2008, "\n")
cat("att_2009_2014: ", res$att_2009_2014, "\n")
cat("Omega (unit weights)\n");  print(res$omega_table)
cat("Lambda (time weights)\n"); print(res$lambda_table)

png(file.path(FIGURES_OUT, "fig_sdid_resid_effect_curve.png"), width = 1200, height = 700, res = 150)
plot_effect_curve(res)
dev.off()
cat("Saved: fig_sdid_resid_effect_curve.png\n")

png(file.path(FIGURES_OUT, "fig_sdid_resid_synthdid.png"), width = 1200, height = 700, res = 150)
plot(res$estimate)
dev.off()
cat("Saved: fig_sdid_resid_synthdid.png\n")

ci_b1 <- block_placebo_ci(df_resid, 2004:2008)
ci_b2 <- block_placebo_ci(df_resid, 2009:2014)
cat("Placebo CI 2004-2008\n"); print(ci_b1)
cat("Placebo CI 2009-2014\n"); print(ci_b2)

loo_res <- sdid_loo(df_resid, block = 2009:2014)
cat("LOO\n"); print(loo_res)

pit_res <- sdid_pit(df_resid, block = 2009:2014, max_drop = 3)
cat("Donor drop (PIT)\n"); print(pit_res)

smry <- data.frame(
  att_2004_2008    = res$att_2004_2008,
  att_2009_2014    = res$att_2009_2014,
  ci_b1_low        = ci_b1["ci_low"],
  ci_b1_high       = ci_b1["ci_high"],
  ci_b2_low        = ci_b2["ci_low"],
  ci_b2_high       = ci_b2["ci_high"],
  loo_att_min      = min(loo_res$att, na.rm = TRUE),
  loo_att_max      = max(loo_res$att, na.rm = TRUE),
  donor_drop_step1 = pit_res$att[pit_res$step == 1],
  donor_drop_step2 = pit_res$att[pit_res$step == 2],
  donor_drop_step3 = pit_res$att[pit_res$step == 3]
)
write.csv(smry, file.path(TABLES_OUT, "table_sdid_resid_summary.csv"), row.names = FALSE)
cat("Saved: table_sdid_resid_summary.csv\n")

cat("\n03_sdid_resid.R complete.\n")