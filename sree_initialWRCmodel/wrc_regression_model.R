# WRC+ Regression Analysis
# Comparing OLS, Ridge, LASSO, and Elastic Net
# Train: 2024, Test: 2025

library(tidyverse)
library(corrr)
library(broom)
library(ggplot2)
library(janitor)
library(glmnet)

# Load and clean data
load_data <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(paste("File not found:", filepath))
  }

  data <- read.csv(filepath, na.strings = c("", "NA"), stringsAsFactors = FALSE) %>%
    clean_names() %>%
    mutate(across(
      -c(player_name, player_id, year),
      ~ suppressWarnings(as.numeric(as.character(.)))
    ))

  # Rename wrc columns to wrc_plus if needed
  if ("wrc" %in% names(data)) {
    data <- data %>% rename(wrc_plus = wrc)
  } else if ("wrc." %in% names(data)) {
    data <- data %>% rename(wrc_plus = wrc.)
  }

  return(data)
}

# Define predictors - using same vars as XGBoost model for consistency
get_predictors <- function(data) {
<<<<<<< Updated upstream
  # Use the same potential predictors as the XGBoost model for consistency
  potential_predictors <- c(
    "k_percent", "bb_percent", "on_base_percent", "isolated_power",
=======
  vars <- c(
    "k_percent", "bb_percent", "on_base_percent", "isolated_power", "woba",
>>>>>>> Stashed changes
    "avg_swing_speed", "attack_angle", "exit_velocity_avg", "launch_angle_avg",
    "barrel_batted_rate", "hard_hit_percent", "z_swing_percent", "oz_swing_percent",
    "meatball_swing_percent", "whiff_percent", "flyballs_percent", "linedrives_percent"
  )

  intersect(vars, names(data))
}

# Train regularized regression with CV
train_model <- function(x_train, y_train, alpha, name) {
  cat(paste0("\nTraining ", name, " (alpha=", alpha, ")...\n"))

  set.seed(2024)
  cv_fit <- cv.glmnet(x_train, y_train,
    alpha = alpha, nfolds = 10,
    standardize = TRUE, family = "gaussian"
  )

  cat("Best lambda:", cv_fit$lambda.min, "\n")
  return(cv_fit)
}

# Calculate metrics
eval_model <- function(actual, predicted, name) {
  data.frame(
    Model = name,
    R2 = round(cor(actual, predicted)^2, 4),
    RMSE = round(sqrt(mean((actual - predicted)^2)), 4),
    MAE = round(mean(abs(actual - predicted)), 4)
  )
}

# Load data
data_path <- "onlyCompleteData.csv"
if (!file.exists(data_path) && file.exists(paste0("../", data_path))) {
  data_path <- paste0("../", data_path)
}

player_data <- load_data(data_path)
predictors <- get_predictors(player_data)

# Split by year and drop NAs
train <- player_data %>%
  filter(year == 2024) %>%
  select(player_name, player_id, year, wrc_plus, all_of(predictors)) %>%
  drop_na()

test <- player_data %>%
  filter(year == 2025) %>%
  select(player_name, player_id, year, wrc_plus, all_of(predictors)) %>%
  drop_na()

cat("\nTraining on", nrow(train), "players (2024)\n")
cat("Testing on", nrow(test), "players (2025)\n")

# Prepare matrices for glmnet
x_train <- as.matrix(train[, predictors])
y_train <- train$wrc_plus
x_test <- as.matrix(test[, predictors])
y_test <- test$wrc_plus

# Fit models
# OLS baseline
ols <- lm(wrc_plus ~ ., data = train[, c("wrc_plus", predictors)])
ols_pred <- predict(ols, newdata = test)

# Ridge (L2)
ridge <- train_model(x_train, y_train, alpha = 0, "Ridge")
ridge_pred <- as.numeric(predict(ridge, s = "lambda.min", newx = x_test))

# LASSO (L1)
lasso <- train_model(x_train, y_train, alpha = 1, "LASSO")
lasso_pred <- as.numeric(predict(lasso, s = "lambda.min", newx = x_test))

# Elastic Net (mix of L1 and L2)
enet <- train_model(x_train, y_train, alpha = 0.5, "Elastic Net")
enet_pred <- as.numeric(predict(enet, s = "lambda.min", newx = x_test))

# Compare performance
results <- rbind(
  eval_model(y_test, ols_pred, "OLS"),
  eval_model(y_test, ridge_pred, "Ridge"),
  eval_model(y_test, lasso_pred, "LASSO"),
  eval_model(y_test, enet_pred, "Elastic Net")
)

cat("\n=== Model Comparison (2025 Test) ===\n")
print(results)

# Check which features LASSO kept
cat("\n=== LASSO Features (non-zero) ===\n")
lasso_coefs <- coef(lasso, s = "lambda.min")
lasso_df <- data.frame(
  Term = rownames(lasso_coefs),
  Estimate = as.numeric(lasso_coefs)
) %>%
  filter(Estimate != 0, Term != "(Intercept)") %>%
  arrange(desc(abs(Estimate)))

print(lasso_df)

# Save residuals for all models
cat("\nSaving residual files...\n")

# OLS residuals
ols_results <- data.frame(
  player_name = test$player_name,
  actual_wrc_plus = y_test,
  predicted_wrc_plus = ols_pred,
  residual = y_test - ols_pred
) %>%
  arrange(desc(abs(residual)))

write_csv(ols_results, "residuals_ols_2025.csv")

# Ridge residuals
ridge_results <- data.frame(
  player_name = test$player_name,
  actual_wrc_plus = y_test,
  predicted_wrc_plus = ridge_pred,
  residual = y_test - ridge_pred
) %>%
  arrange(desc(abs(residual)))

write_csv(ridge_results, "residuals_ridge_2025.csv")

# LASSO residuals
lasso_results <- data.frame(
  player_name = test$player_name,
  actual_wrc_plus = y_test,
  predicted_wrc_plus = lasso_pred,
  residual = y_test - lasso_pred
) %>%
  arrange(desc(abs(residual)))

write_csv(lasso_results, "residuals_lasso_2025.csv")

# Elastic Net residuals
enet_results <- data.frame(
  player_name = test$player_name,
  actual_wrc_plus = y_test,
  predicted_wrc_plus = enet_pred,
  residual = y_test - enet_pred
) %>%
  arrange(desc(abs(residual)))

write_csv(enet_results, "residuals_enet_2025.csv")

cat("  ✓ residuals_ols_2025.csv\n")
cat("  ✓ residuals_ridge_2025.csv\n")
cat("  ✓ residuals_lasso_2025.csv\n")
cat("  ✓ residuals_enet_2025.csv\n")

# Keep LASSO for plotting
results_df <- lasso_results

# Plot
p <- ggplot(results_df, aes(x = actual_wrc_plus, y = predicted_wrc_plus)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "LASSO: Predicted vs Actual WRC+ (2025)",
    x = "Actual WRC+",
    y = "Predicted WRC+",
    subtitle = paste("R² =", results[results$Model == "LASSO", "R2"])
  ) +
  theme_minimal()

print(p)

# Combine all residuals for summary stats
all_res <- data.frame(
  OLS = y_test - ols_pred,
  Ridge = y_test - ridge_pred,
  LASSO = y_test - lasso_pred,
  ElasticNet = y_test - enet_pred
)

# ---- Final Summary Statistics for All Models ----
cat("\n")
cat("=================================================================\n")
cat("                   FINAL MODEL SUMMARY                           \n")
cat("=================================================================\n\n")

# Performance metrics
cat("--- PERFORMANCE METRICS (2025 Test Data) ---\n")
print(results)

# Model-specific details
cat("\n--- MODEL DETAILS ---\n\n")

# OLS
ols_features <- sum(!is.na(coef(ols)))
cat("OLS:\n")
cat("  Features used:", ols_features, "\n")
cat("  Adjusted R²:", round(summary(ols)$adj.r.squared, 4), "\n")
cat("  Residual std error:", round(sd(all_res$OLS), 4), "\n")
cat("  Mean absolute residual:", round(mean(abs(all_res$OLS)), 4), "\n\n")

# Ridge
ridge_features <- length(predictors) # Ridge uses all features (shrinks but doesn't zero out)
ridge_lambda <- ridge$lambda.min
cat("Ridge:\n")
cat("  Lambda (min CV error):", round(ridge_lambda, 4), "\n")
cat("  Features used:", ridge_features, "(all, with shrinkage)\n")
cat("  Residual std error:", round(sd(all_res$Ridge), 4), "\n")
cat("  Mean absolute residual:", round(mean(abs(all_res$Ridge)), 4), "\n\n")

# LASSO
lasso_nonzero <- sum(as.numeric(coef(lasso, s = "lambda.min")) != 0) - 1 # -1 for intercept
lasso_lambda <- lasso$lambda.min
cat("LASSO:\n")
cat("  Lambda (min CV error):", round(lasso_lambda, 4), "\n")
cat("  Features selected:", lasso_nonzero, "out of", length(predictors), "\n")
cat("  Residual std error:", round(sd(all_res$LASSO), 4), "\n")
cat("  Mean absolute residual:", round(mean(abs(all_res$LASSO)), 4), "\n\n")

# Elastic Net
enet_nonzero <- sum(as.numeric(coef(enet, s = "lambda.min")) != 0) - 1
enet_lambda <- enet$lambda.min
cat("Elastic Net:\n")
cat("  Lambda (min CV error):", round(enet_lambda, 4), "\n")
cat("  Alpha (mixing parameter): 0.5\n")
cat("  Features selected:", enet_nonzero, "out of", length(predictors), "\n")
cat("  Residual std error:", round(sd(all_res$ElasticNet), 4), "\n")
cat("  Mean absolute residual:", round(mean(abs(all_res$ElasticNet)), 4), "\n\n")

# Residual comparison
cat("--- RESIDUAL ANALYSIS (All Models) ---\n")
residual_summary <- data.frame(
  Model = c("OLS", "Ridge", "LASSO", "Elastic Net"),
  Min = c(min(all_res$OLS), min(all_res$Ridge), min(all_res$LASSO), min(all_res$ElasticNet)),
  Q1 = c(
    quantile(all_res$OLS, 0.25), quantile(all_res$Ridge, 0.25),
    quantile(all_res$LASSO, 0.25), quantile(all_res$ElasticNet, 0.25)
  ),
  Median = c(median(all_res$OLS), median(all_res$Ridge), median(all_res$LASSO), median(all_res$ElasticNet)),
  Mean = c(mean(all_res$OLS), mean(all_res$Ridge), mean(all_res$LASSO), mean(all_res$ElasticNet)),
  Q3 = c(
    quantile(all_res$OLS, 0.75), quantile(all_res$Ridge, 0.75),
    quantile(all_res$LASSO, 0.75), quantile(all_res$ElasticNet, 0.75)
  ),
  Max = c(max(all_res$OLS), max(all_res$Ridge), max(all_res$LASSO), max(all_res$ElasticNet)),
  SD = c(sd(all_res$OLS), sd(all_res$Ridge), sd(all_res$LASSO), sd(all_res$ElasticNet))
)
residual_summary[, 2:8] <- round(residual_summary[, 2:8], 2)
print(residual_summary)

# Best model recommendation
cat("\n--- RECOMMENDATION ---\n")
best_r2 <- results[which.max(results$R2), ]
best_mae <- results[which.min(results$MAE), ]
cat("Best R²:", best_r2$Model, "(", best_r2$R2, ")\n")
cat("Best MAE:", best_mae$Model, "(", best_mae$MAE, ")\n")

if (best_r2$Model == best_mae$Model) {
  cat("\n✓ Best overall model:", best_r2$Model, "\n")
} else {
  cat("\n→ Trade-off between R² and MAE - consider both", best_r2$Model, "and", best_mae$Model, "\n")
}
