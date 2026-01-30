library(synthdid)

df <- read.csv("pwt_sdid_nm_rich.csv")

df_raw <- df[, c("unit","time","Y","treated")]
setup_raw <- panel.matrices(df_raw)
tau_raw <- synthdid_estimate(setup_raw$Y, setup_raw$N0, setup_raw$T0)

df_res <- df[, c("unit","time","Y_resid","treated")]
names(df_res) <- c("unit","time","Y","treated")
setup_res <- panel.matrices(df_res)
tau_res <- synthdid_estimate(setup_res$Y, setup_res$N0, setup_res$T0)

se_raw <- sqrt(vcov(tau_raw, method="placebo"))
se_res <- sqrt(vcov(tau_res, method="placebo"))

cat(sprintf("RAW   tau: %1.3f  CI: (%1.3f, %1.3f)\n", tau_raw, tau_raw-1.96*se_raw, tau_raw+1.96*se_raw))
cat(sprintf("RESID tau: %1.3f  CI: (%1.3f, %1.3f)\n", tau_res, tau_res-1.96*se_res, tau_res+1.96*se_res))

plot(tau_raw)
plot(tau_res)
