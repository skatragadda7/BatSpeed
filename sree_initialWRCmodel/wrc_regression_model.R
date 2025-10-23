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
