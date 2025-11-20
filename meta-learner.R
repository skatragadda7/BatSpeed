# ============================================================================
# WRC+ Regression + Random Forest + Model Stacking
# Training on 2024 data, Testing on 2025
# With GitHub Paths Included
# ============================================================================

library(tidyverse)
library(corrr)
library(broom)
library(ggplot2)
library(janitor)
library(glmnet)
library(randomForest)

# ============================================================================
# 1. Load & Clean Data
# ============================================================================

# --- GitHub Raw CSV Path ---
github_path <- "https://raw.githubusercontent.com/skatragadda7/BatSpeed/main/onlyCompleteData.csv"

load_and_clean_data <- function(path) {
  cat("Loading data from:", path, "\n")
  
  data <- read.csv(path, na.strings = c("", "NA"), stringsAsFactors = FALSE)

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

  # Standardize response variable name to wrc_plus
  if (!"wrc_plus" %in% names(data)) {
    if ("wrc" %in% names(data)) {
      data <- data %>% rename(wrc_plus = wrc)
    } else if ("wrc." %in% names(data)) {
      data <- data %>% rename(wrc_plus = wrc.)
    }
  }

  return(data)
}

player_data <- load_and_clean_data(github_path)

# ============================================================================
# 2. Select Predictors
# ============================================================================

get_predictors <- function(data) {
  potential_predictors <- c(
    "k_percent", "bb_percent", "on_base_percent", "isolated_power", "woba",
    "avg_swing_speed", "attack_angle", "exit_velocity_avg", "launch_angle_avg",
    "barrel_batted_rate", "hard_hit_percent", "z_swing_percent",
    "oz_swing_percent", "meatball_swing_percent", "whiff_percent",
    "flyballs_percent", "linedrives_percent"
  )

  return(intersect(potential_predictors, names(data)))
}

predictors <- get_predictors(player_data)
response_var <- "wrc_plus"

cat("\nPredictors used:\n")
print(predictors)

# ============================================================================
# 3. Train/Test Split (2024 = Train, 2025 = Test)
# ============================================================================

cols_to_keep <- c("player_name", "player_id", "year", response_var, predictors)

train_data <- player_data %>%
  filter(year == 2024) %>%
  select(all_of(cols_to_keep)) %>%
  drop_na()

test_data <- player_data %>%
  filter(year == 2025) %>%
  select(all_of(cols_to_keep)) %>%
  drop_na()

cat("\nTraining rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")

# Matrices for glmnet
x_train <- as.matrix(train_data[, predictors])
y_train <- train_data[[response_var]]
x_test  <- as.matrix(test_data[, predictors])
y_test  <- test_data[[response_var]]

# ============================================================================
# 4. Helper Functions for Models
# ============================================================================

train_glmnet_model <- function(x_train, y_train, alpha, name) {
  cat(paste0("\nTraining ", name, "...\n"))

  set.seed(2024)
  cv_model <- cv.glmnet(
    x = x_train,
    y = y_train,
    alpha = alpha,
    nfolds = 10,
    standardize = TRUE,
    family = "gaussian"
  )

  cat(name, "best lambda:", cv_model$lambda.min, "\n")
  return(cv_model)
}

evaluate_model <- function(actual, predicted, name) {
  r2 <- cor(actual, predicted)^2
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))

  return(data.frame(
    Model = name,
    R2 = round(r2, 4),
    RMSE = round(rmse, 4),
    MAE = round(mae, 4)
  ))
}

# ============================================================================
# 5. Train Base Models (OLS, Ridge, LASSO, ENet, Random Forest)
# ============================================================================

# --- OLS ---
f <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
cat("\nTraining OLS...\n")
ols_model <- lm(f, data = train_data)

ols_pred_train <- predict(ols_model, train_data)
ols_pred_test  <- predict(ols_model, test_data)

# --- Ridge ---
ridge_model <- train_glmnet_model(x_train, y_train, alpha = 0, name = "Ridge")
ridge_pred_train <- predict(ridge_model, s = "lambda.min", newx = x_train)
ridge_pred_test  <- predict(ridge_model, s = "lambda.min", newx = x_test)

# --- LASSO ---
lasso_model <- train_glmnet_model(x_train, y_train, alpha = 1, name = "LASSO")
lasso_pred_train <- predict(lasso_model, s = "lambda.min", newx = x_train)
lasso_pred_test  <- predict(lasso_model, s = "lambda.min", newx = x_test)

# --- Elastic Net ---
enet_model <- train_glmnet_model(x_train, y_train, alpha = 0.5, name = "Elastic Net")
enet_pred_train <- predict(enet_model, s = "lambda.min", newx = x_train)
enet_pred_test  <- predict(enet_model, s = "lambda.min", newx = x_test)

# --- Random Forest ---
cat("\nTraining Random Forest...\n")

set.seed(2025)
rf_model <- randomForest(
  x = train_data[, predictors],
  y = y_train,
  mtry = floor(length(predictors) / 2),
  ntree = 500,
  importance = TRUE
)

rf_pred_train <- predict(rf_model, train_data)
rf_pred_test  <- predict(rf_model, test_data)

# ============================================================================
# 6. Evaluate Base Models
# ============================================================================

results <- rbind(
  evaluate_model(y_test, ols_pred_test,   "OLS"),
  evaluate_model(y_test, ridge_pred_test, "Ridge"),
  evaluate_model(y_test, lasso_pred_test, "LASSO"),
  evaluate_model(y_test, enet_pred_test,  "Elastic Net"),
  evaluate_model(y_test, rf_pred_test,    "Random Forest")
)

cat("\n=== Base Model Performance (2025) ===\n")
print(results)

# ============================================================================
# 7. STACKING (Meta-Learner)
# ============================================================================

# Stack training data
stack_train <- data.frame(
  y = y_train,
  ols = as.numeric(ols_pred_train),
  ridge = as.numeric(ridge_pred_train),
  lasso = as.numeric(lasso_pred_train),
  enet = as.numeric(enet_pred_train),
  rf = as.numeric(rf_pred_train)
)

# Stack test data
stack_test <- data.frame(
  y = y_test,
  ols = as.numeric(ols_pred_test),
  ridge = as.numeric(ridge_pred_test),
  lasso = as.numeric(lasso_pred_test),
  enet = as.numeric(enet_pred_test),
  rf = as.numeric(rf_pred_test)
)

cat("\nTraining meta-learner...\n")
meta_model <- lm(y ~ ., data = stack_train)

cat("\n=== Meta-Learner Weights ===\n")
print(summary(meta_model))

# Predictions from stacked model
stack_pred_test <- predict(meta_model, newdata = stack_test)

# Evaluate stacking
stack_results <- evaluate_model(y_test, stack_pred_test, "Stacked Model")

results_all <- rbind(results, stack_results)

cat("\n=== Final Model Comparison (with Stacking) ===\n")
print(results_all)

# ============================================================================
# 8. Save Predictions
# ============================================================================

stacked_df <- data.frame(
  player_name = test_data$player_name,
  actual = y_test,
  predicted = stack_pred_test,
  diff = y_test - stack_pred_test
)

write_csv(stacked_df, "stacked_predictions_2025.csv")
cat("\nSaved stacked predictions to stacked_predictions_2025.csv\n")

# ============================================================================
# 9. Plot
# ============================================================================

ggplot(stacked_df, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Stacked Model: Predicted vs Actual WRC+ (2025)",
    x = "Actual WRC+",
    y = "Predicted WRC+"
  ) +
  theme_minimal()
