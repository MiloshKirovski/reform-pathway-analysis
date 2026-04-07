library(bsts)
library(CausalImpact)
library(zoo)

load_data <- function(csv_path) {
  d <- read.csv(csv_path, check.names = FALSE)
  if (!("year" %in% names(d))) {
    d$year <- as.integer(rownames(d))
    rownames(d) <- NULL
  }
  d$year <- as.integer(d$year)
  d$date <- as.Date(paste0(d$year, "-01-01"))
  d <- d[order(d$date), ]
  if (!("y" %in% names(d))) stop("Missing y column.")
  x_cols <- grep("^x_", names(d), value = TRUE)
  if (length(x_cols) == 0) stop("No x_ donor columns.")
  if (any(d$y <= 0, na.rm = TRUE)) stop("Non-positive y.")
  for (cc in x_cols) if (any(d[[cc]] <= 0, na.rm = TRUE)) stop(paste("Non-positive", cc))
  d$y <- log(d$y)
  for (cc in x_cols) d[[cc]] <- log(d[[cc]])
  pre_idx <- d$date >= as.Date("1990-01-01") & d$date <= as.Date("2003-01-01")
  miss_pre <- sapply(x_cols, function(cc) sum(is.na(d[[cc]][pre_idx])))
  x_cols <- x_cols[miss_pre <= 2]
  if (length(x_cols) == 0) stop("All donors dropped (missing in pre).")
  for (cc in x_cols) d[[cc]] <- na.approx(d[[cc]], x = d$date, na.rm = FALSE)
  d$y <- na.approx(d$y, x = d$date, na.rm = FALSE)
  if (any(is.na(d$y[pre_idx]))) stop("Missing y in pre.")
  if (any(sapply(x_cols, function(cc) any(is.na(d[[cc]][pre_idx]))))) stop("Missing donor in pre after interpolation.")
  list(
    df = d,
    x_cols = x_cols,
    z = zoo(d[, c("y", x_cols)], order.by = d$date)
  )
}

run_impact <- function(z, expected_model_size = 3, niter = 20000, seed = 1, alpha = 0.05) {
  set.seed(seed)
  dates <- index(z)
  y_all <- as.numeric(z[, 1])
  X_all <- as.matrix(z[, -1, drop = FALSE])
  pre_idx  <- dates >= as.Date("1990-01-01") & dates <= as.Date("2003-01-01")
  post_idx <- dates >= as.Date("2004-01-01") & dates <= as.Date("2014-01-01")
  y_post_true <- y_all[post_idx]
  y_all_na <- y_all
  y_all_na[post_idx] <- NA
  y_pre <- y_all[pre_idx]
  X_pre <- X_all[pre_idx, , drop = FALSE]
  ss <- AddLocalLinearTrend(list(), y_pre)
  # ss <- AddLocalLevel(list(), y_pre)
  prior <- SpikeSlabPrior(
    x = X_pre,
    y = y_pre,
    expected.model.size = expected_model_size,
    prior.information.weight = 1
  )
  bsts_fit <- bsts(
    y_all_na ~ -1 + X_all,
    state.specification = ss,
    niter = niter,
    prior = prior
  )
  impact <- CausalImpact(
    bsts.model = bsts_fit,
    post.period.response = y_post_true,
    alpha = alpha
  )
  impact$meta <- list(dates = dates)
  impact
}

summarize_block <- function(impact, start_date, end_date) {
  s <- impact$series
  d <- impact$meta$dates
  
  idx <- d >= as.Date(start_date) & d <= as.Date(end_date)
  if (!any(idx)) stop("Block window selects 0 rows.")
  
  pe <- as.numeric(s$point.effect)
  pred <- as.numeric(s$point.pred)
  rel <- pe / pred
  
  list(
    n = sum(idx),
    avg_point_effect = mean(pe[idx], na.rm = TRUE),
    cum_point_effect = sum(pe[idx], na.rm = TRUE),
    avg_rel_effect   = mean(rel[idx], na.rm = TRUE)
  )
}

inclusion_probs <- function(impact) {
  b <- impact$model$bsts.model
  sort(colMeans(b$coefficients != 0), decreasing = TRUE)
}

run_scenario <- function(csv_path, expected_model_size = 3, niter = 20000, seed = 1, alpha = 0.05) {
  obj <- load_data(csv_path)
  cat("Years in file:", format(min(obj$df$date), "%Y"), "to", format(max(obj$df$date), "%Y"), "\n")
  cat("Pre n:", sum(obj$df$date >= as.Date("1990-01-01") & obj$df$date <= as.Date("2003-01-01")), "\n")
  cat("Post n:", sum(obj$df$date >= as.Date("2004-01-01") & obj$df$date <= as.Date("2014-01-01")), "\n")
  impact <- run_impact(
    obj$z,
    expected_model_size = expected_model_size,
    niter = niter,
    seed = seed,
    alpha = alpha
  )
  plot(impact)
  print(summary(impact))
  print(summarize_block(impact, "2004-01-01", "2008-01-01"))
  print(summarize_block(impact, "2009-01-01", "2014-01-01"))
  print(head(inclusion_probs(impact), 50))
  invisible(impact)
}

impact1 <- run_scenario("pwt_bsts_nm_non_eu_1995_2014.csv", expected_model_size = 3, niter = 20000, seed = 1)
impact2 <- run_scenario("pwt_bsts_nm_eu2004_1995_2014.csv", expected_model_size = 3, niter = 20000, seed = 1)
plot(impact1)
plot(impact2)


