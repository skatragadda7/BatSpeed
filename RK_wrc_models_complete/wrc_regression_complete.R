library(tidyverse)
library(readxl)
library(tidymodels)
library(GGally)
library(corrr)
library(modelr)
library(car)
library(yardstick)

# Data

# fangraphs <- read.csv('fangraphs-leaderboards.csv')
# fangraphs <- fangraphs %>% select(Season, Name, wRC., ) %>% filter(Season %in% c("2024", "2025")) %>% arrange(Name, Season)
# head(fangraphs)

# indep_vars_data <- read.csv('onlyCompleteData.csv')
# indep_vars_data <- indep_vars_data %>% select(player.name:player_age, k_percent, isolated_power, avg_swing_speed, squared_up_contact,
# avg_swing_length, attack_angle, ideal_angle_rate, exit_velocity_avg,
# launch_angle_avg, sweet_spot_percent, barrel_batted_rate, hard_hit_percent,
# z_swing_percent, oz_swing_percent, meatball_swing_percent, whiff_percent,
# pull_percent, straightaway_percent, opposite_percent, groundballs_percent, flyballs_percent, linedrives_percent, popups_percent) %>% select(-WRC.)
# head(indep_vars_data)

# df <- indep_vars_data %>%
# left_join(fangraphs, by = c("player.name" = "Name", "year" = "Season")) %>%
# relocate(wRC., .before = "k_percent") %>%
# rename(WRC. = wRC.) %>% distinct()

# head(df)

df <- read.csv("onlyCompleteData.csv")


train <- df %>% filter(year == "2024")
test <- df %>% filter(year == "2025")

train <- train %>% select(-c(player.name:player_age), WRC.)
test <- test %>% select(-c(player.name:player_age), WRC.)

# Exploratory Data Analysis

train.cor <- train %>%
  select(where(is.numeric)) %>%
  correlate() %>%
  rearrange()

wrc_corrs <- train.cor %>%
  select(term, WRC.) %>%
  arrange(desc(abs(WRC.)))
wrc_corrs


train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[1]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[1]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[2]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[3]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[4]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[5]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

# Model Building

LinReg.mod1 <- lm(
  WRC. ~ isolated_power
    + barrel_batted_rate
    + exit_velocity_avg
    + hard_hit_percent
    + avg_swing_speed
    + flyballs_percent
    + sweet_spot_percent
    + groundballs_percent
    + oz_swing_percent
    + avg_swing_length,
  data = train
)
summary(LinReg.mod1)

vif(LinReg.mod1)

drop1(LinReg.mod1, test = "F")


LinReg.mod2 <- lm(
  WRC. ~ isolated_power
    + barrel_batted_rate
    + exit_velocity_avg
    + hard_hit_percent
    #+ avg_swing_speed
    + flyballs_percent
    + sweet_spot_percent
    + groundballs_percent
    + oz_swing_percent
    + avg_swing_length,
  data = train
)
summary(LinReg.mod2)

vif(LinReg.mod2)

drop1(LinReg.mod2, test = "F")

LinReg.mod3 <- lm(
  WRC. ~ isolated_power
    + barrel_batted_rate
    + exit_velocity_avg
    + hard_hit_percent
    #+ avg_swing_speed
    + flyballs_percent
    + sweet_spot_percent
    + groundballs_percent
    + oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod3)

vif(LinReg.mod3)

drop1(LinReg.mod3, test = "F")

LinReg.mod4 <- lm(
  WRC. ~ isolated_power
    + barrel_batted_rate
    + exit_velocity_avg
    + hard_hit_percent
    #+ avg_swing_speed
    + flyballs_percent
    + sweet_spot_percent
    #+ groundballs_percent
    + oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod4)

vif(LinReg.mod4)

drop1(LinReg.mod4, test = "F")


LinReg.mod5 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg
    + hard_hit_percent
    #+ avg_swing_speed
    + flyballs_percent
    + sweet_spot_percent
    #+ groundballs_percent
    + oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod5)

vif(LinReg.mod5)

drop1(LinReg.mod5, test = "F")



LinReg.mod6 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg
    + hard_hit_percent
    #+ avg_swing_speed
    + flyballs_percent
    + sweet_spot_percent,
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod6)

vif(LinReg.mod6)

drop1(LinReg.mod6, test = "F")

# Examining Curved Relationships

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[1]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[3]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[4]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[6]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)

train %>%
  ggplot(aes(x = .data[[wrc_corrs$term[7]]], y = WRC.)) +
  geom_point() +
  geom_smooth(method = lm)


# Continuing Model Building


LinReg.mod7 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg + I(exit_velocity_avg^2)
    + hard_hit_percent + I(hard_hit_percent^2)
    #+ avg_swing_speed
    + flyballs_percent + I(flyballs_percent^2)
    + sweet_spot_percent + I(sweet_spot_percent^2),
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod7)

vif(LinReg.mod7)

drop1(LinReg.mod7, test = "F")

# Interactions written out

LinReg.mod <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg
    + hard_hit_percent
    + avg_swing_speed
    + isolated_power * exit_velocity_avg
    + isolated_power * hard_hit_percent
    + isolated_power * avg_swing_speed
    + exit_velocity_avg * hard_hit_percent
    + exit_velocity_avg * avg_swing_speed
    + hard_hit_percent * avg_swing_speed
    + isolated_power * exit_velocity_avg * hard_hit_percent * avg_swing_speed,
  #+ flyballs_percent
  #+ opposite_percent,
  #+ groundballs_percent
  #+ pull_percent,
  #+ launch_angle_avg,
  data = train
)
summary(LinReg.mod)

vif(LinReg.mod)

drop1(LinReg.mod, test = "F")

LinReg.mod8 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg + I(exit_velocity_avg^2)
    + hard_hit_percent + I(hard_hit_percent^2)
    #+ avg_swing_speed
    + flyballs_percent #+ I(flyballs_percent^2)
    + sweet_spot_percent + I(sweet_spot_percent^2),
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod8)

vif(LinReg.mod8)

drop1(LinReg.mod8, test = "F")

LinReg.mod9 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg + I(exit_velocity_avg^2)
    + hard_hit_percent + I(hard_hit_percent^2)
    #+ avg_swing_speed
    + flyballs_percent #+ I(flyballs_percent^2)
    + sweet_spot_percent + I(sweet_spot_percent^2)
    + isolated_power * exit_velocity_avg
    + isolated_power * hard_hit_percent
    + isolated_power * avg_swing_speed
    + exit_velocity_avg * hard_hit_percent
    + exit_velocity_avg * avg_swing_speed
    + hard_hit_percent * avg_swing_speed
    + isolated_power * exit_velocity_avg * hard_hit_percent * avg_swing_speed,
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod9)

vif(LinReg.mod9)

drop1(LinReg.mod9, test = "F")

LinReg.mod10 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg + I(exit_velocity_avg^2)
    + hard_hit_percent + I(hard_hit_percent^2)
    #+ avg_swing_speed
    + flyballs_percent #+ I(flyballs_percent^2)
    + sweet_spot_percent + I(sweet_spot_percent^2)
    + isolated_power * exit_velocity_avg
    + isolated_power * hard_hit_percent
    + isolated_power * avg_swing_speed
    + exit_velocity_avg * hard_hit_percent
    + exit_velocity_avg * avg_swing_speed
    + hard_hit_percent * avg_swing_speed,
  #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod10)

vif(LinReg.mod10, type = "predictor")

drop1(LinReg.mod10, test = "F")

LinReg.mod11 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg + I(exit_velocity_avg^2)
    + hard_hit_percent + I(hard_hit_percent^2)
    #+ avg_swing_speed
    + flyballs_percent #+ I(flyballs_percent^2)
    + sweet_spot_percent + I(sweet_spot_percent^2)
    + isolated_power * exit_velocity_avg
    #+ isolated_power*hard_hit_percent
    + isolated_power * avg_swing_speed
    + exit_velocity_avg * hard_hit_percent
    + exit_velocity_avg * avg_swing_speed
    + hard_hit_percent * avg_swing_speed,
  #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod11)

LinReg.mod12 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg + I(exit_velocity_avg^2)
    + hard_hit_percent + I(hard_hit_percent^2)
    #+ avg_swing_speed
    + flyballs_percent #+ I(flyballs_percent^2)
    + sweet_spot_percent + I(sweet_spot_percent^2)
    + isolated_power * exit_velocity_avg
    #+ isolated_power*hard_hit_percent
    + isolated_power * avg_swing_speed
    + exit_velocity_avg * hard_hit_percent
    #+ exit_velocity_avg*avg_swing_speed
    + hard_hit_percent * avg_swing_speed,
  #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod12)

vif(LinReg.mod12, type = "predictor")

drop1(LinReg.mod12, test = "F")

LinReg.mod13 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg + I(exit_velocity_avg^2)
    + hard_hit_percent + I(hard_hit_percent^2)
    #+ avg_swing_speed
    + flyballs_percent #+ I(flyballs_percent^2)
    + sweet_spot_percent + I(sweet_spot_percent^2)
    + isolated_power * exit_velocity_avg
    #+ isolated_power*hard_hit_percent
    + isolated_power * avg_swing_speed
    #+ exit_velocity_avg*hard_hit_percent
    #+ exit_velocity_avg*avg_swing_speed
    + hard_hit_percent * avg_swing_speed,
  #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod13)


LinReg.mod14 <- lm(
  WRC. ~ isolated_power
    #+ barrel_batted_rate
    + exit_velocity_avg + I(exit_velocity_avg^2)
    + hard_hit_percent #+ I(hard_hit_percent^2)
    #+ avg_swing_speed
    + flyballs_percent #+ I(flyballs_percent^2)
    + sweet_spot_percent + I(sweet_spot_percent^2)
    + isolated_power * exit_velocity_avg
    #+ isolated_power*hard_hit_percent
    + isolated_power * avg_swing_speed
    #+ exit_velocity_avg*hard_hit_percent
    #+ exit_velocity_avg*avg_swing_speed
    + hard_hit_percent * avg_swing_speed,
  #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
  #+ groundballs_percent
  #+ oz_swing_percent,
  #+ avg_swing_length,
  data = train
)
summary(LinReg.mod14)

vif(LinReg.mod14, type = "predictor")

drop1(LinReg.mod14, test = "F")



library(glmnet)

cat("\n=== Training Ridge Regression Model ===\n")

ridge_predictors <- c(
  "isolated_power", "exit_velocity_avg", "hard_hit_percent",
  "flyballs_percent", "sweet_spot_percent", "avg_swing_speed"
)

x_train_ridge <- as.matrix(train[, ridge_predictors])
y_train_ridge <- train$WRC.

set.seed(2024)
ridge_cv <- cv.glmnet(x_train_ridge, y_train_ridge,
  alpha = 0,
  nfolds = 10,
  standardize = TRUE,
  family = "gaussian"
)

cat("Best lambda (min CV error):", ridge_cv$lambda.min, "\n")
cat("Lambda at 1-SE rule:", ridge_cv$lambda.1se, "\n")

ridge_coefs <- coef(ridge_cv, s = "lambda.min")
cat("\n=== Ridge Coefficients (lambda.min) ===\n")
print(ridge_coefs)

plot(ridge_cv, main = "Ridge Regression: Cross-Validation")

ridge_pred_train <- predict(ridge_cv, s = "lambda.min", newx = x_train_ridge)

ridge_train_r2 <- cor(y_train_ridge, ridge_pred_train)^2
ridge_train_rmse <- sqrt(mean((y_train_ridge - ridge_pred_train)^2))
ridge_train_mae <- mean(abs(y_train_ridge - ridge_pred_train))

cat("\n=== Ridge Performance on 2024 (Training) ===\n")
cat("R²:", round(ridge_train_r2, 4), "\n")
cat("RMSE:", round(ridge_train_rmse, 4), "\n")
cat("MAE:", round(ridge_train_mae, 4), "\n")



# Evaluating Predictions for 2024

prediction_data_2024 <- df %>%
  filter(year == "2024") %>%
  select(player.name:WRC.) %>%
  distinct() %>%
  mutate(pred_WRC. = LinReg.mod14$fitted.values, errors = LinReg.mod14$residuals) %>%
  mutate(sum_pred_error = pred_WRC. + errors) %>%
  mutate(check = (sum_pred_error == WRC.))

head(prediction_data_2024)

# Residual Plots

residualPlots(LinReg.mod14, tests = F)
plot(LinReg.mod14, which = c(1, 2))
hist(LinReg.mod14$residuals)

# Actual vs Predicted

ggplot(prediction_data_2024, aes(x = WRC., y = pred_WRC.)) +
  # Create a diagonal line:
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted WRC.", x = "WRC.", title = "Predictions for 2024") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

# Metrics

ames_metrics <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)

ames_metrics(prediction_data_2024, truth = WRC., estimate = pred_WRC.)

AIC(LinReg.mod14)
BIC(LinReg.mod14)


# Predictions for 2025

prediction_data_2025 <- df %>%
  filter(year == "2025") %>%
  select(player.name:WRC.) %>%
  distinct() %>%
  mutate(pred_WRC. = predict(LinReg.mod14, newdata = test
  %>% select(
      isolated_power, exit_velocity_avg,
      hard_hit_percent, flyballs_percent,
      sweet_spot_percent, avg_swing_speed
    ))) %>%
  mutate(errors = WRC. - pred_WRC.) %>%
  mutate(sum_pred_error = pred_WRC. + errors) %>%
  mutate(check = (sum_pred_error == WRC.))


hist(prediction_data_2025$errors)
plot(prediction_data_2025$pred_WRC., prediction_data_2025$errors)

ggplot(prediction_data_2025, aes(x = WRC., y = pred_WRC.)) +
  # Create a diagonal line:
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted WRC.", x = "WRC.", title = "Predictions for 2025") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

# Metrics


ames_metrics(prediction_data_2025, truth = WRC., estimate = pred_WRC.)



cat("\n=== Ridge Performance on 2025 (Test) ===\n")

x_test_ridge <- as.matrix(test[, ridge_predictors])
y_test_ridge <- test$WRC.

ridge_pred_test <- predict(ridge_cv, s = "lambda.min", newx = x_test_ridge)

ridge_test_r2 <- cor(y_test_ridge, ridge_pred_test)^2
ridge_test_rmse <- sqrt(mean((y_test_ridge - ridge_pred_test)^2))
ridge_test_mae <- mean(abs(y_test_ridge - ridge_pred_test))

cat("R²:", round(ridge_test_r2, 4), "\n")
cat("RMSE:", round(ridge_test_rmse, 4), "\n")
cat("MAE:", round(ridge_test_mae, 4), "\n")

ridge_prediction_data_2025 <- df %>%
  filter(year == "2025") %>%
  select(player.name:WRC.) %>%
  distinct() %>%
  mutate(pred_WRC. = as.numeric(ridge_pred_test)) %>%
  mutate(errors = WRC. - pred_WRC.) %>%
  mutate(sum_pred_error = pred_WRC. + errors) %>%
  mutate(check = (sum_pred_error == WRC.))

cat("\n=== Model Comparison: LinReg.mod14 vs Ridge (2025 Test) ===\n")
comparison_2025 <- data.frame(
  Model = c("LinReg.mod14", "Ridge"),
  R2 = c(cor(prediction_data_2025$WRC., prediction_data_2025$pred_WRC.)^2, ridge_test_r2),
  RMSE = c(sqrt(mean((prediction_data_2025$WRC. - prediction_data_2025$pred_WRC.)^2)), ridge_test_rmse),
  MAE = c(mean(abs(prediction_data_2025$WRC. - prediction_data_2025$pred_WRC.)), ridge_test_mae)
)
comparison_2025[, 2:4] <- round(comparison_2025[, 2:4], 4)
print(comparison_2025)

ggplot(ridge_prediction_data_2025, aes(x = WRC., y = pred_WRC.)) +
  geom_abline(lty = 2, color = "red") +
  geom_point(alpha = 0.5) +
  labs(
    y = "Predicted WRC. (Ridge)", x = "Actual WRC.",
    title = "Ridge Regression: Predictions for 2025",
    subtitle = paste("R² =", round(ridge_test_r2, 4))
  ) +
  coord_obs_pred() +
  theme_minimal()


# Classification Stats for 2024 and 2025

prediction_data_2024 <- prediction_data_2024 %>%
  mutate(WRC._above_mean = case_when(
    `WRC.` >= mean(prediction_data_2024$WRC.) ~ 1,
    `WRC.` < mean(prediction_data_2024$WRC.) ~ 0
  )) %>%
  mutate(pred_WRC._above_mean = case_when(
    `pred_WRC.` >= mean(prediction_data_2024$WRC.) ~ 1,
    `pred_WRC.` < mean(prediction_data_2024$WRC.) ~ 0
  )) %>%
  mutate(
    WRC._above_mean = as.factor(WRC._above_mean),
    pred_WRC._above_mean = as.factor(pred_WRC._above_mean)
  )


conf_mat(prediction_data_2024, truth = `WRC._above_mean`, estimate = `pred_WRC._above_mean`)

# accuracy

(42 + 28) / (42 + 28 + 5 + 10)

# precision

(28) / (28 + 10)

# recall

(28) / (28 + 5)


prediction_data_2025 <- prediction_data_2025 %>%
  mutate(WRC._above_mean = case_when(
    `WRC.` >= mean(prediction_data_2025$WRC.) ~ 1,
    `WRC.` < mean(prediction_data_2025$WRC.) ~ 0
  )) %>%
  mutate(pred_WRC._above_mean = case_when(
    `pred_WRC.` >= mean(prediction_data_2025$WRC.) ~ 1,
    `pred_WRC.` < mean(prediction_data_2025$WRC.) ~ 0
  )) %>%
  mutate(
    WRC._above_mean = as.factor(WRC._above_mean),
    pred_WRC._above_mean = as.factor(pred_WRC._above_mean)
  )


conf_mat(prediction_data_2025, truth = `WRC._above_mean`, estimate = `pred_WRC._above_mean`)

# accuracy

(52 + 50) / (50 + 52 + 20 + 17)

# precision

(52) / (52 + 17)

# recall

(52) / (52 + 20)



# Comparing with Random Forest (assuming correct, but will have to check with Cameron)

RF_predictions_2024 <- read.csv("RF_predictions_2024.csv")

RF_predictions_2025 <- read.csv("RF_predictions_2025.csv")


# 2024

ames_metrics <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)

ames_metrics(RF_predictions_2024, truth = actual, estimate = prediction)


hist(RF_predictions_2024$diff)

# Actual vs Predicted

ggplot(RF_predictions_2024, aes(x = actual, y = prediction)) +
  # Create a diagonal line:
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted WRC.", x = "WRC.", title = "Predictions for 2024") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

# 2025

ames_metrics <- yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)

ames_metrics(RF_predictions_2025, truth = actual, estimate = prediction)


hist(RF_predictions_2025$diff)

# Actual vs Predicted

ggplot(RF_predictions_2025, aes(x = actual, y = prediction)) +
  # Create a diagonal line:
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  labs(y = "Predicted WRC.", x = "WRC.", title = "Predictions for 2025") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()


# Both Models' Metrics

LinReg_metrics_2024 <- ames_metrics(prediction_data_2024, truth = WRC., estimate = pred_WRC.)


LinReg_metrics_2025 <- ames_metrics(prediction_data_2025, truth = WRC., estimate = pred_WRC.)

RF_metrics_2024 <- ames_metrics(RF_predictions_2024, truth = actual, estimate = prediction)

RF_metrics_2025 <- ames_metrics(RF_predictions_2025, truth = actual, estimate = prediction)

Ridge_metrics_2025 <- data.frame(
  .metric = c("rmse", "rsq", "mae"),
  .estimator = c("standard", "standard", "standard"),
  .estimate = c(ridge_test_rmse, ridge_test_r2, ridge_test_mae)
)


model_comparisons <- data.frame(
  Model = c("Linear Regression", "Linear Regression", "Ridge"),
  Year = c("2024", "2025", "2025"),
  RMSE = c(
    LinReg_metrics_2024$.estimate[1], # RF_metrics_2024$.estimate[1],
    LinReg_metrics_2025$.estimate[1], Ridge_metrics_2025$.estimate[1] # RF_metrics_2025$.estimate[1]
  ),
  R2 = c(
    LinReg_metrics_2024$.estimate[2], # RF_metrics_2024$.estimate[2],
    LinReg_metrics_2025$.estimate[2], Ridge_metrics_2025$.estimate[2] # RF_metrics_2025$.estimate[2]
  ),
  MAE = c(
    LinReg_metrics_2024$.estimate[3], # RF_metrics_2024$.estimate[3],
    LinReg_metrics_2025$.estimate[3], Ridge_metrics_2025$.estimate[3] # RF_metrics_2025$.estimate[3]
  )
)
model_comparisons

model_comparisons_pivoted <- model_comparisons %>%
  pivot_longer(
    cols = c(R2, RMSE, MAE),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = Value
  ) %>%
  arrange(Metric, Model)

model_comparisons_pivoted
