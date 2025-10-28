dat <- read.csv("onlyCompleteData.csv", na.strings = c("", "NA"))

# --- 2) Pick target + predictors (7 total) ---
target <- "woba"
predictors <- c(
  "attack_angle",
  "avg_bat_speed",
  "avg_swing_speed",
  "barrel_batted_rate",
  "hard_hit_percent",
  "sweet_spot_percent",
  "launch_angle_avg"
)

# Keep only needed columns and drop rows with missing values
model_df <- na.omit(dat[, c(target, predictors)])

# --- 3) Fit linear regression ---
form <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
fit <- lm(form, data = model_df)

# --- 4) Inspect results ---
summary(fit)


#Teddy predictor for 2025 based on the fit model
# 1) Clean names once and keep only the columns you actually need
player_data_fit <- dat %>%
  clean_names()

cols_needed <- c("player_name", "player_id", "year", target, predictors)
cols_have   <- intersect(cols_needed, names(player_data_fit))

model_df <- player_data_fit %>%
  select(all_of(cols_have))

# 2) Make sure year is numeric (or at least comparable)
if (!is.numeric(model_df$year)) {
  suppressWarnings(model_df$year <- as.numeric(model_df$year))
}

# 3) Keep only predictors that are present
predictors_in <- intersect(predictors, names(model_df))

# 4) Check what’s missing in 2024 for just the needed columns
missing_2024 <- model_df %>%
  filter(year == 2024) %>%
  summarise(across(all_of(c(target, predictors_in)), ~ sum(is.na(.))))

print(missing_2024)

# 5) Build the 2024 training set, dropping NAs ONLY in the needed columns
train_data_fit <- model_df %>%
  filter(year == 2024) %>%
  drop_na(all_of(c(target, predictors_in)))

cat("Rows in train_data_fit:", nrow(train_data_fit), "\n")

# 6) If still zero, show which columns are killing rows
if (nrow(train_data_fit) == 0) {
  na_counts <- model_df %>%
    filter(year == 2024) %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(all_of(c(target, predictors_in)),
                 names_to = "col", values_to = "val") %>%
    mutate(is_na = is.na(val)) %>%
    group_by(col) %>%
    summarise(na_rows = sum(is_na), .groups = "drop") %>%
    arrange(desc(na_rows))
  print(na_counts)
  stop("No complete cases for 2024 across required columns. See na_counts above.")
}
test_data_fit <- model_df %>%
  filter(year == 2025) %>%
  drop_na(all_of(c(target, predictors)))
# 7) Fit the model
form <- reformulate(termlabels = predictors_in, response = target)
fit1 <- lm(form, data = train_data_fit)
summary(fit1)

residual_plot_fit <- ggplot(data = data.frame(
  fitted = fitted(fit1),
  residuals = residuals(fit1)
), aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

print(residual_plot_fit)

# Q-Q plot for normality
qq_plot <- ggplot(data = data.frame(residuals = residuals(fit1)), 
                  aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal()

print(qq_plot)

test_predictions_fit <- predict(fit1, newdata = test_data_fit)


actual_2025 <- test_data_fit$woba
predicted_2025 <- test_predictions_fit
r_squared_2025 <- cor(actual_2025, predicted_2025)^2
rmse_2025 <- sqrt(mean((actual_2025 - predicted_2025)^2))
mae_2025 <- mean(abs(actual_2025 - predicted_2025))

results_df_fit <- data.frame(
  player_name = test_data_fit$player_name,
  actual_woba = actual_2025,
  predicted_woba= predicted_2025,
  residual = actual_2025 - predicted_2025
) %>%
  arrange(desc(abs(residual)))

cat("R-squared:", round(r_squared_2025, 4), "\n")
cat("RMSE:", round(rmse_2025, 4), "\n")
cat("MAE:", round(mae_2025, 4), "\n")


prediction_plot_fit <- ggplot(results_df_fit, aes(x = actual_woba, y = predicted_woba)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Predicted vs Actual WRC+ (2025 Data)",
       x = "Actual WRC+", y = "Predicted WRC+",
       subtitle = paste("R² =", round(r_squared_2025, 3))) +
  theme_minimal()

print(prediction_plot_fit)

# Show predictions
print(head(results_df_fit, 10))
print(tail(results_df_fit, 10))

# Model coefficients
coef_df_fit <- tidy(fit1) %>%
  arrange(desc(abs(estimate)))
print(coef_df_fit)
fit_summary<- summary(fit1)
# Summary statistics
cat("Training R-squared (2024):", round(fit_summary$r.squared, 4), "\n")
cat("Training Adjusted R-squared (2024):", round(fit_summary$adj.r.squared, 4), "\n")
cat("Test R-squared (2025):", round(r_squared_2025, 4), "\n")
cat("Test RMSE (2025):", round(rmse_2025, 4), "\n")
cat("Test MAE (2025):", round(mae_2025, 4), "\n")

