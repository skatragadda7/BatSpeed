# ============================================================================
# WRC+ Regression Model (OLS, Ridge, LASSO, Elastic Net)
# ============================================================================
# Training on 2024 data, testing on 2025 data
# ============================================================================

library(tidyverse)
library(corrr)
library(broom)
library(ggplot2)
library(janitor)
library(glmnet)

# ============================================================================
# Helper Functions
# ============================================================================

load_and_clean_data <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(paste("Could not find", filepath, "in the current directory"))
  }

  cat("Loaded data from:", filepath, "\n")
  data <- read.csv(filepath, na.strings = c("", "NA"), stringsAsFactors = FALSE)

  data <- data %>%
    clean_names() %>%
    mutate(across(
      .cols = -c(player_name, player_id, year),
      .fns = ~ {
        if (is.logical(.)) {
          as.numeric(.)
        } else {
          suppressWarnings(as.numeric(as.character(.)))
        }
      }
    ))

  # Standardize response variable name
  if (!"wrc_plus" %in% names(data)) {
    if ("wrc" %in% names(data)) {
      data <- data %>% rename(wrc_plus = wrc)
    } else if ("wrc." %in% names(data)) {
      data <- data %>% rename(wrc_plus = wrc.)
    }
  }

  return(data)
}

get_predictors <- function(data) {
  # Use the same potential predictors as the XGBoost model for consistency
  potential_predictors <- c(
    "k_percent", "bb_percent", "on_base_percent", "isolated_power", "woba",
    "avg_swing_speed", "attack_angle", "exit_velocity_avg", "launch_angle_avg",
    "barrel_batted_rate", "hard_hit_percent", "z_swing_percent", "oz_swing_percent",
    "meatball_swing_percent", "whiff_percent", "flyballs_percent", "linedrives_percent"
  )

  available <- intersect(potential_predictors, names(data))
  return(available)
}

train_glmnet_model <- function(x_train, y_train, alpha, model_name) {
  cat(paste0("\nTraining ", model_name, " (alpha=", alpha, ")...\n"))

  set.seed(2024)
  cv_model <- cv.glmnet(
    x = x_train,
    y = y_train,
    alpha = alpha,
    nfolds = 10,
    standardize = TRUE,
    family = "gaussian"
  )

  cat("Best lambda:", cv_model$lambda.min, "\n")
  return(cv_model)
}

evaluate_model <- function(actual, predicted, model_name) {
  r2 <- cor(actual, predicted)^2
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))

  return(data.frame(
    Model = model_name,
    R2 = round(r2, 4),
    RMSE = round(rmse, 4),
    MAE = round(mae, 4)
  ))
}

# ============================================================================
# Main Execution
# ============================================================================

# 1. Load Data
# Using onlyCompleteData.csv as Merged_stats_wrc.csv was missing
data_path <- "onlyCompleteData.csv"
# Adjust path if running from subdirectory
if (!file.exists(data_path) && file.exists(paste0("../", data_path))) {
  data_path <- paste0("../", data_path)
}

player_data <- load_and_clean_data(data_path)
predictors <- get_predictors(player_data)
response_var <- "wrc_plus"

# 2. Split Data
# For linear models, we generally need to handle NAs.
# Dropping rows with NAs in predictors or response.
cols_to_keep <- c("player_name", "player_id", "year", response_var, predictors)

train_data <- player_data %>%
  filter(year == 2024) %>%
  select(all_of(cols_to_keep)) %>%
  drop_na()

test_data <- player_data %>%
  filter(year == 2025) %>%
  select(all_of(cols_to_keep)) %>%
  drop_na()

cat("\nTraining data (2024):", nrow(train_data), "players\n")
cat("Testing data (2025):", nrow(test_data), "players\n")

# 3. Prepare Matrices for GLMNET
x_train <- as.matrix(train_data[, predictors])
y_train <- train_data[[response_var]]
x_test <- as.matrix(test_data[, predictors])
y_test <- test_data[[response_var]]

# 4. Train Models

# --- OLS (Baseline) ---
# Using all predictors
f <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
ols_model <- lm(f, data = train_data)
ols_pred <- predict(ols_model, newdata = test_data)

# --- Ridge (Alpha = 0) ---
ridge_model <- train_glmnet_model(x_train, y_train, alpha = 0, "Ridge")
ridge_pred <- as.numeric(predict(ridge_model, s = "lambda.min", newx = x_test))

# --- LASSO (Alpha = 1) ---
lasso_model <- train_glmnet_model(x_train, y_train, alpha = 1, "LASSO")
lasso_pred <- as.numeric(predict(lasso_model, s = "lambda.min", newx = x_test))

# --- Elastic Net (Alpha = 0.5) ---
enet_model <- train_glmnet_model(x_train, y_train, alpha = 0.5, "Elastic Net")
enet_pred <- as.numeric(predict(enet_model, s = "lambda.min", newx = x_test))

# 5. Evaluate and Compare
results <- rbind(
  evaluate_model(y_test, ols_pred, "OLS"),
  evaluate_model(y_test, ridge_pred, "Ridge"),
  evaluate_model(y_test, lasso_pred, "LASSO"),
  evaluate_model(y_test, enet_pred, "Elastic Net")
)

cat("\n=== Model Comparison (2025 Test Data) ===\n")
print(results)

# 6. Inspect LASSO Coefficients (Feature Selection)
cat("\n=== LASSO Selected Features (Non-zero Coefficients) ===\n")
lasso_coefs <- coef(lasso_model, s = "lambda.min")
# Convert sparse matrix to regular matrix and filter
lasso_coefs_df <- data.frame(
  Term = rownames(lasso_coefs),
  Estimate = as.numeric(lasso_coefs)
) %>%
  filter(Estimate != 0, Term != "(Intercept)") %>%
  arrange(desc(abs(Estimate)))

print(lasso_coefs_df)

# 7. Save Best Predictions (assuming LASSO or Elastic Net is usually best, but let's save LASSO)
final_results_df <- data.frame(
  player_name = test_data$player_name,
  actual_wrc_plus = y_test,
  predicted_wrc_plus = lasso_pred,
  residual = y_test - lasso_pred
) %>%
  arrange(desc(abs(residual)))

write_csv(final_results_df, "wrc_predictions_lasso_2025.csv")
cat("\nLASSO predictions saved to 'wrc_predictions_lasso_2025.csv'\n")

# 8. Plot Best Model
p <- ggplot(final_results_df, aes(x = actual_wrc_plus, y = predicted_wrc_plus)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "LASSO: Predicted vs Actual WRC+ (2025 Data)",
    x = "Actual WRC+",
    y = "Predicted WRC+",
    subtitle = paste("R² =", results[results$Model == "LASSO", "R2"])
  ) +
  theme_minimal()

print(p)
