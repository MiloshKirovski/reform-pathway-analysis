library(bsts)
library(CausalImpact)
library(zoo)

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
HERE <- dirname(script_path)
ROOT        <- normalizePath(file.path(HERE, "..", ".."))
DATA_IN     <- file.path(ROOT, "data", "processed")
TABLES_OUT  <- file.path(ROOT, "paper_outputs", "tables")
FIGURES_OUT <- file.path(ROOT, "paper_outputs", "figures")
dir.create(TABLES_OUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_OUT, showWarnings = FALSE, recursive = TRUE)

load_bsts_data <- function(csv_path) {
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
    df     = d,
    x_cols = x_cols,
    z      = zoo(d[, c("y", x_cols)], order.by = d$date)
  )
}

run_impact <- function(z, expected_model_size = 3, niter = 20000, seed = 1, alpha = 0.05) {
  set.seed(seed)
  dates      <- index(z)
  y_all      <- as.numeric(z[, 1])
  X_all      <- as.matrix(z[, -1, drop = FALSE])
  pre_idx    <- dates >= as.Date("1990-01-01") & dates <= as.Date("2003-01-01")
  post_idx   <- dates >= as.Date("2004-01-01") & dates <= as.Date("2014-01-01")
  y_post_true <- y_all[post_idx]
  y_all_na   <- y_all
  y_all_na[post_idx] <- NA
  y_pre <- y_all[pre_idx]
  X_pre <- X_all[pre_idx, , drop = FALSE]
  ss    <- AddLocalLinearTrend(list(), y_pre)
  prior <- SpikeSlabPrior(
    x = X_pre, y = y_pre,
    expected.model.size   = expected_model_size,
    prior.information.weight = 1
  )
  bsts_fit <- bsts(y_all_na ~ -1 + X_all, state.specification = ss, niter = niter, prior = prior)
  impact   <- CausalImpact(bsts.model = bsts_fit, post.period.response = y_post_true, alpha = alpha)
  impact$meta <- list(dates = dates)
  impact
}

summarize_block <- function(impact, start_date, end_date) {
  s   <- impact$series
  d   <- impact$meta$dates
  idx <- d >= as.Date(start_date) & d <= as.Date(end_date)
  if (!any(idx)) stop("Block window selects 0 rows.")
  pe   <- as.numeric(s$point.effect)
  pred <- as.numeric(s$point.pred)
  rel  <- pe / pred
  list(
    n                = sum(idx),
    avg_point_effect = mean(pe[idx],  na.rm = TRUE),
    cum_point_effect = sum(pe[idx],   na.rm = TRUE),
    avg_rel_effect   = mean(rel[idx], na.rm = TRUE)
  )
}

inclusion_probs <- function(impact) {
  b <- impact$model$bsts.model
  sort(colMeans(b$coefficients != 0), decreasing = TRUE)
}

run_scenario <- function(label, csv_path, expected_model_size = 3, niter = 20000, seed = 1, alpha = 0.05) {
  cat(label, "\n")
  obj <- load_bsts_data(csv_path)
  cat("Years:", format(min(obj$df$date), "%Y"), "to", format(max(obj$df$date), "%Y"), "\n")
  cat("Pre n:",  sum(obj$df$date >= as.Date("1990-01-01") & obj$df$date <= as.Date("2003-01-01")), "\n")
  cat("Post n:", sum(obj$df$date >= as.Date("2004-01-01") & obj$df$date <= as.Date("2014-01-01")), "\n")

  impact <- run_impact(obj$z, expected_model_size = expected_model_size, niter = niter, seed = seed, alpha = alpha)

  cat("Summary\n"); print(summary(impact))
  cat("Block 2004-2008\n"); print(summarize_block(impact, "2004-01-01", "2008-01-01"))
  cat("Block 2009-2013\n"); print(summarize_block(impact, "2009-01-01", "2013-01-01"))
  cat("Inclusion probs (top 10)\n"); print(head(inclusion_probs(impact), 10))

  png(file.path(FIGURES_OUT, paste0("fig_bsts_", label, ".png")), width = 1400, height = 900, res = 150)
  plot(impact)
  dev.off()
  cat("Saved: fig_bsts_", label, ".png\n", sep = "")

  s <- impact$series
  dates <- impact$meta$dates
  blk1  <- summarize_block(impact, "2004-01-01", "2008-01-01")
  blk2  <- summarize_block(impact, "2009-01-01", "2013-01-01")

  incl     <- inclusion_probs(impact)
  top_incl <- head(incl, 5)
  smry <- data.frame(
    label              = label,
    att_post_avg       = mean(as.numeric(s$point.effect), na.rm = TRUE),
    att_2004_2008      = blk1$avg_point_effect,
    att_2009_2013      = blk2$avg_point_effect,
    cum_effect_2004_08 = blk1$cum_point_effect,
    cum_effect_2009_13 = blk2$cum_point_effect,
    top_donors         = paste(names(top_incl), collapse = "; "),
    top_incl_probs     = paste(round(top_incl, 3), collapse = "; ")
  )
  out_path <- file.path(TABLES_OUT, paste0("table_bsts_", label, ".csv"))
  write.csv(smry, out_path, row.names = FALSE)
  cat("Saved:", out_path, "\n")

  invisible(impact)
}

impact_non_eu <- run_scenario(
  label                = "non_eu",
  csv_path             = file.path(DATA_IN, "pwt_bsts_nm_non_eu.csv"),
  expected_model_size  = 3,
  niter                = 20000,
  seed                 = 1
)

impact_eu2004 <- run_scenario(
  label                = "eu2004",
  csv_path             = file.path(DATA_IN, "pwt_bsts_nm_eu2004.csv"),
  expected_model_size  = 3,
  niter                = 20000,
  seed                 = 1
)

cat("\n01_bsts.R complete.\n")