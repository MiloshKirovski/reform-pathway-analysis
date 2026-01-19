library(bsts)
library(dplyr)
library(readr)
library(tibble)

df <- read_csv("data/bsts_input_with_wgi.csv", show_col_types = FALSE)
df$DATE <- as.numeric(df$DATE)

df <- df %>% 
  group_by(COUNTRY) %>% 
  filter(n() >= 10) %>% 
  ungroup()

df <- df %>%
  group_by(COUNTRY) %>%
  mutate(
    EU_Distance_std = as.numeric(scale(EU_Distance)),
    SC_scaled        = as.numeric(scale(SC)),
    
    Within_SP_scaled = as.numeric(scale(Within_SP)),
    Total_Trade_scaled = as.numeric(scale(Total_Trade)),
    PIGT_scaled        = as.numeric(scale(PIGT)),
    NETD_scaled        = as.numeric(scale(NETD)),
    estimate_cc        = as.numeric(scale(estimate_cc)),
    estimate_ge        = as.numeric(scale(estimate_ge)),
    estimate_pv        = as.numeric(scale(estimate_pv)),
    estimate_rl        = as.numeric(scale(estimate_rl)),
    estimate_rq        = as.numeric(scale(estimate_rq)),
    estimate_va        = as.numeric(scale(estimate_va)),
  ) %>% ungroup()

predictors <- c("SC_scaled", "Within_SP_scaled", "Total_Trade_scaled", "PIGT_scaled", "NETD_scaled",
                "estimate_cc", "estimate_ge", "estimate_pv", "estimate_rl",
                "estimate_rq", "estimate_va")

fit_bsts_country <- function(country_data, predictors, niter = 2000) {
  y <- country_data$EU_Distance_std
  x <- country_data %>% select(all_of(predictors))
  
  ss <- list()
  ss <- AddLocalLinearTrend(ss, y)
  
  bsts_model <- bsts(
    y ~ ., 
    state.specification = ss,
    data = x,
    niter = niter,
    ping = 0,
    seed = 123
  )
  
  burn <- SuggestBurn(0.1, bsts_model)
  
  coef_summary <- summary(bsts_model, burn = burn)$coefficients
  
  predictions <- colMeans(bsts_model$state.contributions[-(1:burn), "regression", ])
  residuals <- y - predictions
  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  r_squared <- 1 - (ss_res / ss_tot)
  
  result <- tibble(
    COUNTRY = country_data$COUNTRY[1],
    N = nrow(country_data),
    R2 = r_squared
  )
  
  for(pred in predictors) {
    result[[paste0(pred, "_coef")]] <- coef_summary[pred, "mean"]
    result[[paste0(pred, "_sd")]]   <- coef_summary[pred, "sd"]
    result[[paste0(pred, "_incprob")]] <- coef_summary[pred, "inc.prob"]
  }
  
  return(list(model = bsts_model, results = result))
}

top_countries <- df %>% count(COUNTRY) %>% arrange(desc(n)) %>% pull(COUNTRY)

all_results <- list()
all_models <- list()

for(country in top_countries) {
  cat("Fitting BSTS for:", country)
  
  country_data <- df %>% filter(COUNTRY == country) %>% arrange(DATE)
  if(nrow(country_data) < 10) {
    cat("Skipping", country, "- insufficient data\n")
    next
  }
  
  fit <- fit_bsts_country(country_data, predictors)
  
  all_models[[country]] <- fit$model
  all_results[[country]] <- fit$results
  
  cat("BSTS fitted for", country, "\n")
}

bsts_results <- bind_rows(all_results)
print(bsts_results)

write.csv(bsts_results, "results/bsts_country_results.csv", row.names = FALSE)
df <- read.csv("results/bsts_country_results.csv", stringsAsFactors = FALSE)
View(df)

