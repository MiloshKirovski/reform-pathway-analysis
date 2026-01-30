suppressPackageStartupMessages({
  library(zoo)
  library(bsts)
  library(CausalImpact)
})

load_bsts_wide <- function(csv_path) {
  d <- read.csv(csv_path, check.names = FALSE)
  
  d$date <- as.Date(sprintf("%d-01-01", as.integer(d$year)))
  
  keep <- setdiff(names(d), c("year", "date"))
  d <- d[, c("date", keep), drop = FALSE]
  
  num_cols <- setdiff(names(d), "date")
  d[num_cols] <- lapply(d[num_cols], function(x) as.numeric(x))
  
  z <- zoo(d[num_cols], order.by = d$date)
  list(z = z, years = as.integer(format(d$date, "%Y")))
}

infer_periods <- function(z, treat_year = 2004L) {
  yrs <- as.integer(format(index(z), "%Y"))
  list(
    pre_start  = sprintf("%d-01-01", min(yrs)),
    pre_end    = sprintf("%d-01-01", treat_year - 1L),
    post_start = sprintf("%d-01-01", treat_year),
    post_end   = sprintf("%d-01-01", max(yrs))
  )
}

run_causalimpact_bsts <- function(z,
                                  pre_start, pre_end,
                                  post_start, post_end,
                                  expected_model_size = 3,
                                  niter = 20000,
                                  seed = 1) {
  set.seed(seed)
  
  dates <- index(z)
  pre_start  <- as.Date(pre_start)
  pre_end    <- as.Date(pre_end)
  post_start <- as.Date(post_start)
  post_end   <- as.Date(post_end)
  
  y_all <- as.numeric(z[, "y"])
  X_all <- as.data.frame(z[, setdiff(colnames(z), "y"), drop = FALSE])
  
  post_idx <- dates >= post_start & dates <= post_end
  y_masked <- y_all
  y_masked[post_idx] <- NA_real_
  
  df <- data.frame(y = y_masked, X_all, check.names = TRUE)
  
  ss <- AddLocalLevel(list(), df$y)
  
  x <- model.matrix(~ 0 + ., data = X_all)
  prior <- SpikeSlabPrior(x = x, y = df$y, expected.model.size = expected_model_size)
  
  bsts_fit <- bsts(
    formula = y ~ 0 + .,
    data = df,
    state.specification = ss,
    niter = niter,
    prior = prior,
    ping = 2000
  )
  
  impact <- CausalImpact(bsts.model = bsts_fit, post.period.response = y_all[post_idx])
  impact$meta$dates_user <- dates
  impact
}

summarize_block <- function(impact, start_date, end_date) {
  s <- impact$series
  d <- as.Date(impact$meta$dates_user)
  
  idx <- d >= as.Date(start_date) & d <= as.Date(end_date)
  
  pe <- as.numeric(s[, "point.effect"][idx])
  pp <- as.numeric(s[, "point.pred"][idx])
  
  list(
    n = sum(idx),
    avg_point_effect = mean(pe, na.rm = TRUE),
    cum_point_effect = sum(pe, na.rm = TRUE),
    avg_rel_effect   = mean(pe / pp, na.rm = TRUE)
  )
}

pre_fit_diagnostics <- function(impact, pre_start, pre_end) {
  s <- impact$series
  d <- as.Date(impact$meta$dates_user)
  
  pre_idx <- d >= as.Date(pre_start) & d <= as.Date(pre_end)
  
  obs_col <- if ("response" %in% colnames(s)) "response" else "y"
  resid_pre <- as.numeric(s[, obs_col][pre_idx] - s[, "point.pred"][pre_idx])
  
  list(
    n_pre = sum(pre_idx),
    sd_pre_resid = sd(resid_pre, na.rm = TRUE),
    rmse_pre = sqrt(mean(resid_pre^2, na.rm = TRUE)),
    mae_pre  = mean(abs(resid_pre), na.rm = TRUE)
  )
}

inclusion_probs <- function(impact, burn = 0) {
  coef_draws <- impact$model$bsts.model$coefficients
  if (burn > 0) coef_draws <- coef_draws[(as.integer(burn) + 1):nrow(coef_draws), , drop = FALSE]
  sort(colMeans(coef_draws != 0), decreasing = TRUE)
}

csv1 <- "pwt_bsts_nm_non_eu_1995_2014.csv"
obj1 <- load_bsts_wide(csv1)
per1 <- infer_periods(obj1$z, treat_year = 2004L)

impact1 <- run_causalimpact_bsts(
  obj1$z,
  pre_start  = per1$pre_start,  pre_end  = per1$pre_end,
  post_start = per1$post_start, post_end = per1$post_end,
  expected_model_size = 3,
  niter = 20000,
  seed = 1
)

plot(impact1)
print(summary(impact1))
print(summarize_block(impact1, "2004-01-01", "2008-01-01"))
print(summarize_block(impact1, "2009-01-01", "2014-01-01"))
print(head(inclusion_probs(impact1, burn = 2000), 15))
print(pre_fit_diagnostics(impact1, per1$pre_start, per1$pre_end))

csv2 <- "pwt_bsts_nm_eu2004_1995_2014.csv"
obj2 <- load_bsts_wide(csv2)
per2 <- infer_periods(obj2$z, treat_year = 2004L)

impact2 <- run_causalimpact_bsts(
  obj2$z,
  pre_start  = per2$pre_start,  pre_end  = per2$pre_end,
  post_start = per2$post_start, post_end = per2$post_end,
  expected_model_size = 3,
  niter = 20000,
  seed = 1
)

plot(impact2)
print(summary(impact2))
print(head(inclusion_probs(impact2, burn = 2000), 15))
print(pre_fit_diagnostics(impact2, per2$pre_start, per2$pre_end))
