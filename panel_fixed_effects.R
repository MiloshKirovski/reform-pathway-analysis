library(fixest)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

df <- read_csv("data/bsts_input_with_wgi.csv", show_col_types = FALSE)
df$DATE <- as.numeric(df$DATE)

df <- df %>% 
  group_by(COUNTRY) %>% 
  filter(n() >= 10) %>% 
  ungroup()

cat("PANEL REGRESSION ANALYSIS USING FIXEST\n")
cat("Total observations:", nrow(df), "\n")
cat("Countries:", length(unique(df$COUNTRY)), "\n")
cat("Time periods:", min(df$DATE), "-", max(df$DATE), "\n")
cat("Observations per country:\n")
print(table(df$COUNTRY))

cat("MODEL 1: BASELINE (Economic Variables + Country FE)\n")

fe_model1 <- feols(
  EU_Distance ~ SC + Within_SP + Total_Trade + PIGT + NETD | COUNTRY,
  data = df,
  vcov = "hetero"  
)

summary(fe_model1)

cat("MODEL 2: WITH KEY GOVERNANCE INDICATORS\n")

fe_model2 <- feols(
  EU_Distance ~ SC + Within_SP + Total_Trade + PIGT + NETD + 
    estimate_rl + estimate_rq + estimate_pv + estimate_va | COUNTRY,
  data = df,
  vcov = "hetero"
)

summary(fe_model2)

cat("MODEL 3: SC + Within_SP focus\n")

fe_model3 <- feols(
  EU_Distance ~ SC + Within_SP + PIGT | COUNTRY,
  data = df,
  vcov = "hetero"
)

summary(fe_model3)

cat("MODEL 4: GOVERNANCE INDICATORS ONLY\n")

fe_model4 <- feols(
  EU_Distance ~ estimate_cc + estimate_ge + estimate_rl + estimate_rq + 
    estimate_pv + estimate_va | COUNTRY,
  data = df,
  vcov = "hetero"
)

summary(fe_model4)

cat("MODEL COMPARISON TABLE\n")
etable(fe_model1, fe_model2, fe_model3, fe_model4,
       title = "Fixed Effects Models: EU Convergence Determinants",
       headers = c("Baseline", "With Governance", "Focus on Structure", "Governance Only"),
       dict = c(SC = "Structural Change",
                Within_SP = "Within-Sector Prod",
                Total_Trade = "Total Trade",
                PIGT = "Price Deflator",
                NETD = "Employment",
                estimate_cc = "Control of Corruption",
                estimate_ge = "Govt Effectiveness",
                estimate_rl = "Rule of Law",
                estimate_rq = "Regulatory Quality",
                estimate_pv = "Political Stability",
                estimate_va = "Voice and Accountability"),
       file = "results/fe_regression_table.tex")

etable(fe_model1, fe_model2, fe_model3, fe_model4,
       title = "Fixed Effects Models: EU Convergence Determinants",
       headers = c("Baseline", "With Governance", "Parsimonious", "Governance Only"),
       dict = c(SC = "Structural Change",
                Within_SP = "Within-Sector Prod",
                Total_Trade = "Total Trade",
                PIGT = "Price Deflator",
                NETD = "Employment",
                estimate_cc = "Control of Corruption",
                estimate_ge = "Govt Effectiveness",
                estimate_rl = "Rule of Law",
                estimate_rq = "Regulatory Quality",
                estimate_pv = "Political Stability",
                estimate_va = "Voice and Accountability"))

cat("\nSaved: results/fe_regression_table.tex\n")

coef_data <- data.frame(
  variable = names(coef(fe_model2)),
  coefficient = as.numeric(coef(fe_model2)),
  se = se(fe_model2)
) %>%
  mutate(
    ci_lower = coefficient - 1.96 * se,
    ci_upper = coefficient + 1.96 * se,
    significant = !(ci_lower < 0 & ci_upper > 0)
  )

coef_data$variable_clean <- recode(coef_data$variable,
                                   "SC" = "Structural Change",
                                   "Within_SP" = "Within-Sector Prod",
                                   "Total_Trade" = "Total Trade",
                                   "PIGT" = "Price Deflator",
                                   "NETD" = "Employment",
                                   "estimate_rl" = "Rule of Law",
                                   "estimate_rq" = "Regulatory Quality"
)

p_coef <- ggplot(coef_data, aes(x = reorder(variable_clean, coefficient), 
                                y = coefficient)) +
  geom_point(aes(color = significant), size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = significant), 
                width = 0.3, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "steelblue"),
                     labels = c("Not Significant", "Significant (p<0.05)")) +
  labs(
    title = "Fixed Effects Model: Determinants of EU Convergence",
    subtitle = "Within-country effects (controlling for time-invariant characteristics)",
    x = "",
    y = "Coefficient Estimate (95% CI)",
    color = "Statistical Significance"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray40", size = 11)
  )

ggsave("results/fe_coefficients.png", p_coef, width = 11, height = 7, dpi = 300)
cat("Saved: results/fe_coefficients.png\n")


country_fe <- fixef(fe_model2)$COUNTRY
fe_df <- data.frame(
  COUNTRY = names(country_fe),
  fixed_effect = as.numeric(country_fe)
) %>%
  arrange(fixed_effect)

cat("COUNTRY-SPECIFIC FIXED EFFECTS\n")
cat("Baseline EU distance after controlling for all predictors - Higher -> Closer to EU on average\n")
print(fe_df)

p_fe <- ggplot(fe_df, aes(x = reorder(COUNTRY, fixed_effect), y = fixed_effect)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Country-Specific Fixed Effects",
    subtitle = "Baseline EU distance (structural differences across countries)",
    x = "Country",
    y = "Fixed Effect"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  theme(plot.title = element_text(face = "bold"))

ggsave("results/fe_country_effects.png", p_fe, width = 10, height = 6, dpi = 300)
cat("Saved: results/fe_country_effects.png\n")

cat("DIAGNOSTIC CHECKS\n")

df$residuals <- resid(fe_model2)
df$fitted <- fitted(fe_model2)

png("results/fe_diagnostics.png", width = 1200, height = 900)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(df$fitted, df$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values",
     pch = 20, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lty = 2, lwd = 2)
lowess_fit <- lowess(df$fitted, df$residuals)
lines(lowess_fit, col = "blue", lwd = 2)

qqnorm(df$residuals, main = "Normal Q-Q Plot", pch = 20, col = rgb(0, 0, 1, 0.5))
qqline(df$residuals, col = "red", lwd = 2)

plot(df$fitted, sqrt(abs(df$residuals)),
     xlab = "Fitted Values", ylab = "√|Residuals|",
     main = "Scale-Location",
     pch = 20, col = rgb(0, 0, 1, 0.5))
lowess_fit2 <- lowess(df$fitted, sqrt(abs(df$residuals)))
lines(lowess_fit2, col = "blue", lwd = 2)

boxplot(residuals ~ COUNTRY, data = df,
        main = "Residuals by Country",
        las = 2,
        col = "lightblue",
        ylab = "Residuals")
abline(h = 0, col = "red", lty = 2, lwd = 2)

dev.off()
cat("Saved: results/fe_diagnostics.png\n")

cat("ROBUSTNESS CHECK: ADDING TIME TRENDS\n")

fe_model_trend <- feols(
  EU_Distance ~ SC + Within_SP + Total_Trade + PIGT + NETD + 
    estimate_rl + estimate_rq | COUNTRY + COUNTRY[DATE],
  data = df,
  vcov = "hetero"
)

summary(fe_model_trend)

cat("HETEROGENEITY ANALYSIS: INTERACTION WITH INITIAL GDP\n")

initial_gdp <- df %>%
  group_by(COUNTRY) %>%
  filter(DATE == min(DATE)) %>%
  select(COUNTRY, initial_gdp = EU_Distance)

df <- df %>%
  left_join(initial_gdp, by = "COUNTRY")

fe_model_interact <- feols(
  EU_Distance ~ SC + Within_SP + PIGT + 
    SC:initial_gdp + Within_SP:initial_gdp | COUNTRY,
  data = df,
  vcov = "hetero"
)

summary(fe_model_interact)

summary_stats <- data.frame(
  Model = c("Baseline", "With Governance", "Parsimonious", "Governance Only", "With Trends"),
  R2 = c(r2(fe_model1, type="r2"), r2(fe_model2, type="r2"), r2(fe_model3, type="r2"), r2(fe_model4, type="r2"), r2(fe_model_trend, type="r2")),
  R2_within = c(
    r2(fe_model1, type = "wr2"),
    r2(fe_model2, type = "wr2"),
    r2(fe_model3, type = "wr2"),
    r2(fe_model4, type = "wr2"),
    r2(fe_model_trend, type = "wr2")
  ),
  N = c(nobs(fe_model1), nobs(fe_model2), nobs(fe_model3), 
        nobs(fe_model4), nobs(fe_model_trend))
)

cat("MODEL FIT SUMMARY\n")
print(summary_stats)

write_csv(summary_stats, "results/model_fit_summary.csv")
cat("Saved: results/model_fit_summary.csv\n")

significant_vars <- coef_data %>% 
  filter(significant) %>% 
  arrange(desc(abs(coefficient)))

if(nrow(significant_vars) > 0) {
  cat("SIGNIFICANT PREDICTORS (p < 0.05):\n\n")
  for(i in 1:nrow(significant_vars)) {
    var <- significant_vars$variable_clean[i]
    coef <- significant_vars$coefficient[i]
    direction <- ifelse(coef > 0, "AWAY FROM", "TOWARD")
    cat(sprintf("  %s\n", var))
    cat(sprintf("    Coefficient: %.6f\n", coef))
    cat(sprintf("    Effect: Moves %s EU convergence\n", direction))
    cat("\n")
  }
} else {
  cat("NO VARIABLES SIGNIFICANT AT p < 0.05\n")
}