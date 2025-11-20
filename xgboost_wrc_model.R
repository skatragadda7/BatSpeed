# ============================================================================
# XGBoost Regression Model for Predicting WRC+ (Weighted Runs Created Plus)
# ============================================================================
# This script builds, tunes, and evaluates an XGBoost regression model
# to predict a baseball player's wrc_plus using quantitative variables.
# Training on 2024 data, testing on 2025 data.
# ============================================================================

# ============================================================================
# STEP 1: Load Libraries
# ============================================================================
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(xgboost)

# ============================================================================
# Helper Functions
# ============================================================================

load_and_clean_data <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(paste("Could not find", filepath, "in the current directory"))
  }

  cat("Loaded data from:", filepath, "\n")
  data <- read.csv(filepath, na.strings = c("", "NA"), stringsAsFactors = FALSE)

  # Clean column names and convert to numeric (except ID columns)
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
    } else {
      warning("Could not find wrc_plus, wrc, or wrc. column.")
    }
  }

  return(data)
}

get_predictors <- function(data) {
  potential_predictors <- c(
    "k_percent", "bb_percent", "on_base_percent", "isolated_power", "woba",
    "avg_swing_speed", "attack_angle", "exit_velocity_avg", "launch_angle_avg",
    "barrel_batted_rate", "hard_hit_percent", "z_swing_percent", "oz_swing_percent",
    "meatball_swing_percent", "whiff_percent", "flyballs_percent", "linedrives_percent"
  )

  available <- intersect(potential_predictors, names(data))
  missing <- setdiff(potential_predictors, names(data))

  if (length(missing) > 0) {
    cat("\nWarning: The following predictors are not found in the data:\n")
    cat(paste(missing, collapse = ", "), "\n")
  }

  cat("\nUsing", length(available), "predictors:\n")
  cat(paste(available, collapse = ", "), "\n")

  return(available)
}

prepare_matrices <- function(train_data, test_data, predictors, response_var) {
  # Prepare Training Data
  train_x <- train_data %>%
    select(all_of(predictors)) %>%
    as.matrix()
  storage.mode(train_x) <- "numeric"
  train_y <- as.numeric(train_data[[response_var]])

  # Prepare Testing Data
  test_x <- test_data %>%
    select(all_of(predictors)) %>%
    as.matrix()
  storage.mode(test_x) <- "numeric"
  test_y <- as.numeric(test_data[[response_var]])

  # Create DMatrix objects
  dtrain <- xgb.DMatrix(data = train_x, label = train_y)
  dtest <- xgb.DMatrix(data = test_x, label = test_y)

  return(list(dtrain = dtrain, dtest = dtest, test_y = test_y, train_x = train_x, test_x = test_x))
}

tune_hyperparameters <- function(dtrain) {
  cat("\nRunning Grid Search for Hyperparameters...\n")

  # Define grid
  param_grid <- expand.grid(
    max_depth = c(3, 6),
    eta = c(0.05, 0.1, 0.3),
    subsample = c(0.8, 1.0),
    colsample_bytree = c(0.8, 1.0),
    min_child_weight = c(1, 3)
  )

  best_params <- list()
  best_rmse <- Inf
  best_nrounds <- 0

  # Loop through grid
  for (i in 1:nrow(param_grid)) {
    params <- list(
      objective = "reg:squarederror",
      max_depth = param_grid$max_depth[i],
      eta = param_grid$eta[i],
      subsample = param_grid$subsample[i],
      colsample_bytree = param_grid$colsample_bytree[i],
      min_child_weight = param_grid$min_child_weight[i]
    )

    cv_model <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 200,
      nfold = 5,
      early_stopping_rounds = 10,
      verbose = FALSE
    )

    min_rmse <- min(cv_model$evaluation_log$test_rmse_mean)

    if (min_rmse < best_rmse) {
      best_rmse <- min_rmse
      best_params <- params
      best_nrounds <- cv_model$best_iteration
    }
  }

  cat("\nBest Parameters Found:\n")
  print(unlist(best_params))
  cat("Best CV RMSE:", best_rmse, "\n")
  cat("Optimal nrounds:", best_nrounds, "\n")

  return(list(params = best_params, nrounds = best_nrounds))
}

evaluate_model <- function(model, dtest, test_data, test_y) {
  preds <- predict(model, newdata = dtest)

  r_squared <- cor(test_y, preds)^2
  rmse <- sqrt(mean((test_y - preds)^2))
  mae <- mean(abs(test_y - preds))

  cat("\n=== Model Evaluation (2025 Test Data) ===\n")
  cat("R-squared:", round(r_squared, 4), "\n")
  cat("RMSE:", round(rmse, 4), "\n")
  cat("MAE:", round(mae, 4), "\n")

  results <- data.frame(
    player_name = test_data$player_name,
    actual_wrc_plus = test_y,
    predicted_wrc_plus = preds,
    residual = test_y - preds
  ) %>% arrange(desc(abs(residual)))

  return(results)
}

# ============================================================================
# Main Execution
# ============================================================================

# 1. Load Data
data_path <- "onlyCompleteData.csv"
player_data <- load_and_clean_data(data_path)

# 2. Define Predictors and Response
response_var <- "wrc_plus"
predictors <- get_predictors(player_data)

# 3. Split Data (2024 Train, 2025 Test)
# IMPORTANT: Only dropping NAs for the response variable.
# XGBoost handles missing predictors automatically.
train_data <- player_data %>%
  filter(year == 2024) %>%
  drop_na(all_of(response_var))

test_data <- player_data %>%
  filter(year == 2025) %>%
  drop_na(all_of(response_var))

cat("\nTraining data (2024):", nrow(train_data), "players\n")
cat("Testing data (2025):", nrow(test_data), "players\n")

# 4. Prepare Matrices
matrices <- prepare_matrices(train_data, test_data, predictors, response_var)

# 5. Tune Hyperparameters
tuning_results <- tune_hyperparameters(matrices$dtrain)

# 6. Train Final Model
cat("\nTraining final model...\n")
final_model <- xgb.train(
  params = tuning_results$params,
  data = matrices$dtrain,
  nrounds = tuning_results$nrounds,
  verbose = 0
)

# 7. Evaluate
results_df <- evaluate_model(final_model, matrices$dtest, test_data, matrices$test_y)

# 8. Feature Importance
cat("\n=== Feature Importance ===\n")
importance_matrix <- xgb.importance(model = final_model)
print(head(importance_matrix, 10))
xgb.plot.importance(importance_matrix = head(importance_matrix, 10))

# 9. Top/Bottom Predictions
cat("\nTop 10 predictions (by absolute residual):\n")
print(head(results_df, 10))

cat("\nBottom 10 predictions (by absolute residual):\n")
print(tail(results_df, 10))
