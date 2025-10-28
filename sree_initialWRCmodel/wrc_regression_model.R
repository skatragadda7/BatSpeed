# WRC+ Regression Model
# Training on 2024 data, testing on 2025 data

library(tidyverse)
library(corrr)
library(broom)
library(ggplot2)
library(janitor)

player_data <- read_csv("Data/Merged_stats_wrc.csv")
player_data <- player_data %>%
  clean_names() %>%
  mutate(across(
    .cols = -c(player_name, player_id, year),
    .fns = ~ as.numeric(as.character(.))
  )) %>%
  rename(wrc_plus = wrc)

cat("\nMissing values by year:\n")
player_data %>%
  group_by(year) %>%
  summarise(across(everything(), ~sum(is.na(.))), .groups = 'drop') %>%
  pivot_longer(-year, names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

# Filter for 2024 2025 data
train_data <- player_data %>%
  filter(year == 2024) %>%
  drop_na()

test_data <- player_data %>%
  filter(year == 2025) %>%
  drop_na()

cat("\nTraining data (2024):", nrow(train_data), "players\n")
cat("Testing data (2025):", nrow(test_data), "players\n")

# Find common players between 2024 and 2025
common_players <- intersect(train_data$player_id, test_data$player_id)
cat("Common players between years:", length(common_players), "\n")

numeric_vars <- train_data %>%
  select(where(is.numeric), -player_id, -year)
cor_matrix <- numeric_vars %>%
  correlate()

# Find top 10 predictors most correlated with WRC+
top_predictors <- cor_matrix %>%
  focus(wrc_plus) %>%
  arrange(desc(abs(wrc_plus))) %>%
  head(11) %>%  
  filter(term != "wrc_plus") %>%  
  head(10)  

cat("\nTop 10 predictors for WRC+:\n")
print(top_predictors)

# Create formula for regression
predictor_names <- top_predictors$term
formula_str <- paste("wrc_plus ~", paste(predictor_names, collapse = " + "))
cat("\nRegression formula:\n", formula_str, "\n")

# Fit the model on 2024 data
model <- lm(as.formula(formula_str), data = train_data)

# Model summary
model_summary <- summary(model)
print(model_summary)

# Residuals vs Fitted
residual_plot <- ggplot(data = data.frame(
  fitted = fitted(model),
  residuals = residuals(model)
), aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

print(residual_plot)

# Q-Q plot for normality
qq_plot <- ggplot(data = data.frame(residuals = residuals(model)), 
                  aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal()

print(qq_plot)

# Predict on 2025 data
test_predictions <- predict(model, newdata = test_data)


actual_2025 <- test_data$wrc_plus
predicted_2025 <- test_predictions
r_squared_2025 <- cor(actual_2025, predicted_2025)^2
rmse_2025 <- sqrt(mean((actual_2025 - predicted_2025)^2))
mae_2025 <- mean(abs(actual_2025 - predicted_2025))

results_df <- data.frame(
  player_name = test_data$player_name,
  actual_wrc_plus = actual_2025,
  predicted_wrc_plus = predicted_2025,
  residual = actual_2025 - predicted_2025
) %>%
  arrange(desc(abs(residual)))

cat("R-squared:", round(r_squared_2025, 4), "\n")
cat("RMSE:", round(rmse_2025, 4), "\n")
cat("MAE:", round(mae_2025, 4), "\n")


prediction_plot <- ggplot(results_df, aes(x = actual_wrc_plus, y = predicted_wrc_plus)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Predicted vs Actual WRC+ (2025 Data)",
       x = "Actual WRC+", y = "Predicted WRC+",
       subtitle = paste("R² =", round(r_squared_2025, 3))) +
  theme_minimal()

print(prediction_plot)

# Show predictions
print(head(results_df, 10))
print(tail(results_df, 10))

# Model coefficients
coef_df <- tidy(model) %>%
  arrange(desc(abs(estimate)))
print(coef_df)

# Save results
write_csv(results_df, "wrc_predictions_2025.csv")
cat("\nResults saved to 'wrc_predictions_2025.csv'\n")

# Summary statistics
cat("Training R-squared (2024):", round(model_summary$r.squared, 4), "\n")
cat("Training Adjusted R-squared (2024):", round(model_summary$adj.r.squared, 4), "\n")
cat("Test R-squared (2025):", round(r_squared_2025, 4), "\n")
cat("Test RMSE (2025):", round(rmse_2025, 4), "\n")
cat("Test MAE (2025):", round(mae_2025, 4), "\n")

# ===========================
# RIDGE REGRESSION ADD-ON (CV on 2024, predict 2025)
# ===========================

install.packages("glmnet")

library(glmnet)
if (!exists("predictor_names")) {
  numeric_vars_ridge <- train_data %>%
    select(where(is.numeric), -player_id, -year)
  cor_matrix_ridge <- cor(numeric_vars_ridge, use = "pairwise.complete.obs")
  # pick top 10 absolute correlations with wrc_plus (excluding itself)
  cor_target <- sort(abs(cor_matrix_ridge[,"wrc_plus"]), decreasing = TRUE)
  predictor_names <- setdiff(names(cor_target)[2:11], "wrc_plus")
}

# Build design matrices (glmnet needs matrix; it will one-hot any factors)
ridge_x_train <- model.matrix(
  reformulate(predictor_names), data = train_data
)[, -1, drop = FALSE]   # drop intercept column
ridge_y_train <- train_data$wrc_plus

ridge_x_test <- model.matrix(
  reformulate(predictor_names), data = test_data
)[, -1, drop = FALSE]

set.seed(2024)
ridge_cv <- cv.glmnet(
  x = ridge_x_train,
  y = ridge_y_train,
  alpha = 0,           # alpha = 0 → RIDGE
  nfolds = 10,
  standardize = TRUE,  # default TRUE; explicit for clarity
  family = "gaussian"
)
# lambda best to use
cat("\n[Ridge] Best lambda (min):", ridge_cv$lambda.min, "\n")
cat("[Ridge] 1-SE lambda:", ridge_cv$lambda.1se, "\n")

# Prediction for 2025 with both lambda involved
ridge_pred_min <- as.numeric(predict(ridge_cv, s = "lambda.min", newx = ridge_x_test))
ridge_pred_1se <- as.numeric(predict(ridge_cv, s = "lambda.1se", newx = ridge_x_test))

# accuracy of RMSE, MAE and R2
ridge_rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
ridge_mae  <- function(actual, pred) mean(abs(actual - pred))
ridge_r2   <- function(actual, pred) cor(actual, pred)^2

ridge_actual_2025 <- test_data$wrc_plus
#creation of scalar, 
ridge_r2_min  <- ridge_r2(ridge_actual_2025, ridge_pred_min)
ridge_rmse_min<- ridge_rmse(ridge_actual_2025, ridge_pred_min)
ridge_mae_min <- ridge_mae(ridge_actual_2025, ridge_pred_min)

ridge_r2_1se  <- ridge_r2(ridge_actual_2025, ridge_pred_1se)
ridge_rmse_1se<- ridge_rmse(ridge_actual_2025, ridge_pred_1se)
ridge_mae_1se <- ridge_mae(ridge_actual_2025, ridge_pred_1se)
#print the performance of each 
cat("\n[Ridge @ lambda.min]  R^2:", round(ridge_r2_min, 4),
    " RMSE:", round(ridge_rmse_min, 4),
    " MAE:", round(ridge_mae_min, 4), "\n")

cat("[Ridge @ lambda.1se] R^2:", round(ridge_r2_1se, 4),
    " RMSE:", round(ridge_rmse_1se, 4),
    " MAE:", round(ridge_mae_1se, 4), "\n")

# Make result table (lambda.min by default)
ridge_results_df <- tibble(
  player_name = test_data$player_name,
  actual_wrc_plus = ridge_actual_2025,
  predicted_wrc_plus_ridge = ridge_pred_min,
  residual_ridge = actual_wrc_plus - predicted_wrc_plus_ridge
) %>%
  arrange(desc(abs(residual_ridge)))

# Pred vs Actual plot (lambda.min)
ridge_pred_plot <- ggplot(
  ridge_results_df,
  aes(x = actual_wrc_plus, y = predicted_wrc_plus_ridge)
) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Ridge: Predicted vs Actual WRC+ (2025, λ = lambda.min)",
    x = "Actual WRC+",
    y = "Predicted WRC+",
    subtitle = paste0("R² = ", round(ridge_r2_min, 3),
                      " | RMSE = ", round(ridge_rmse_min, 3),
                      " | MAE = ", round(ridge_mae_min, 3))
  ) +
  theme_minimal()
print(ridge_pred_plot)

# Coefficients of lambda.min and terms by estimate
ridge_coef_mat <- as.matrix(coef(ridge_cv, s = "lambda.min"))
ridge_coef_df <- tibble(
  term = rownames(ridge_coef_mat),
  estimate = as.numeric(ridge_coef_mat[,1])
) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(abs(estimate)))

cat("\n[Ridge] Coefficients (lambda.min):\n")
print(ridge_coef_df)

# Side-by-side of old, lambda min and 1se(simpler model, more stability, less precise)
if (exists("r_squared_2025") && exists("rmse_2025") && exists("mae_2025")) {
  ridge_compare <- tibble::tibble(
    metric = c("R2", "RMSE", "MAE"),
    OLS = c(round(r_squared_2025, 4), round(rmse_2025, 4), round(mae_2025, 4)),
    Ridge_lambda_min = c(round(ridge_r2_min, 4), round(ridge_rmse_min, 4), round(ridge_mae_min, 4)),
    Ridge_lambda_1se = c(round(ridge_r2_1se, 4), round(ridge_rmse_1se, 4), round(ridge_mae_1se, 4))
  )
  cat("\nComparison (OLS vs Ridge):\n")
  print(ridge_compare)
}

