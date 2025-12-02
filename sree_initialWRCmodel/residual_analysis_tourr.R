# Interactive Residual Analysis using tourr
# Run this after wrc_regression_model.R to explore residual patterns

library(tidyverse)
library(janitor)
library(glmnet)

if (!require("tourr", quietly = TRUE)) {
    install.packages("tourr")
}
library(tourr)

cat("=== Interactive Residual Analysis with tourr ===\n")
cat("Close each tour window to continue\n\n")

# Load data
data_path <- "onlyCompleteData.csv"
if (!file.exists(data_path) && file.exists(paste0("../", data_path))) {
    data_path <- paste0("../", data_path)
}

player_data <- read.csv(data_path, na.strings = c("", "NA"), stringsAsFactors = FALSE) %>%
    clean_names() %>%
    mutate(across(
        -c(player_name, player_id, year),
        ~ suppressWarnings(as.numeric(as.character(.)))
    ))

# Rename wrc columns
if ("wrc" %in% names(player_data)) {
    player_data <- player_data %>% rename(wrc_plus = wrc)
} else if ("wrc." %in% names(player_data)) {
    player_data <- player_data %>% rename(wrc_plus = wrc.)
}

# Get predictors
predictors <- c(
    "k_percent", "bb_percent", "on_base_percent", "isolated_power", "woba",
    "avg_swing_speed", "attack_angle", "exit_velocity_avg", "launch_angle_avg",
    "barrel_batted_rate", "hard_hit_percent", "z_swing_percent", "oz_swing_percent",
    "meatball_swing_percent", "whiff_percent", "flyballs_percent", "linedrives_percent"
)
predictors <- intersect(predictors, names(player_data))

# Split data
train <- player_data %>%
    filter(year == 2024) %>%
    select(player_name, player_id, year, wrc_plus, all_of(predictors)) %>%
    drop_na()

test <- player_data %>%
    filter(year == 2025) %>%
    select(player_name, player_id, year, wrc_plus, all_of(predictors)) %>%
    drop_na()

# Prepare matrices
x_train <- as.matrix(train[, predictors])
y_train <- train$wrc_plus
x_test <- as.matrix(test[, predictors])
y_test <- test$wrc_plus

# Re-run models to get residuals
cat("Re-training models to generate residuals...\n")

# OLS
ols <- lm(wrc_plus ~ ., data = train[, c("wrc_plus", predictors)])
ols_pred <- predict(ols, newdata = test)

# Ridge
set.seed(2024)
ridge <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10, standardize = TRUE)
ridge_pred <- as.numeric(predict(ridge, s = "lambda.min", newx = x_test))

# LASSO
set.seed(2024)
lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10, standardize = TRUE)
lasso_pred <- as.numeric(predict(lasso, s = "lambda.min", newx = x_test))

# Elastic Net
set.seed(2024)
enet <- cv.glmnet(x_train, y_train, alpha = 0.5, nfolds = 10, standardize = TRUE)
enet_pred <- as.numeric(predict(enet, s = "lambda.min", newx = x_test))

# Get LASSO coefficients for important features
lasso_coefs <- coef(lasso, s = "lambda.min")
lasso_df <- data.frame(
    Term = rownames(lasso_coefs),
    Estimate = as.numeric(lasso_coefs)
) %>%
    filter(Estimate != 0, Term != "(Intercept)") %>%
    arrange(desc(abs(Estimate)))

# Combine all residuals
all_res <- data.frame(
    OLS = y_test - ols_pred,
    Ridge = y_test - ridge_pred,
    LASSO = y_test - lasso_pred,
    ElasticNet = y_test - enet_pred,
    predicted = lasso_pred,
    actual = y_test
)

tour_df <- cbind(all_res, x_test)

# ---- Tour Visualizations ----

# Tour 1: Residuals vs predictors (check for patterns)
cat("\n1. Residuals vs predictors (should be random)\n")
res_pred_data <- tour_df[, c("LASSO", predictors)]

res_size <- abs(res_pred_data$LASSO)
res_colors <- cut(res_size,
    breaks = quantile(res_size, probs = c(0, 0.75, 0.9, 1)),
    labels = c("normal", "large", "outlier"),
    include.lowest = TRUE
)

animate_xy(res_pred_data, col = as.numeric(res_colors), axes = "bottomleft")

# Tour 2: Compare all model residuals
cat("\n2. All model residuals comparison\n")
model_comp <- data.frame(
    LASSO = all_res$LASSO,
    Ridge = all_res$Ridge,
    OLS = all_res$OLS,
    ElasticNet = all_res$ElasticNet,
    predicted = all_res$predicted,
    actual = all_res$actual
)

worst_model <- apply(abs(model_comp[, 1:4]), 1, which.max)
animate_xy(model_comp, col = worst_model, axes = "bottomleft")

# Tour 3: Focus on extreme residuals with top predictors
cat("\n3. Extreme residuals with important features\n")
top_vars <- lasso_df$Term[1:min(5, nrow(lasso_df))]
important <- intersect(top_vars, predictors)

if (length(important) > 0) {
    extreme_data <- tour_df[, c("LASSO", important)]

    extreme_thresh <- quantile(abs(tour_df$LASSO), 0.9)
    is_extreme <- abs(tour_df$LASSO) > extreme_thresh
    extreme_cols <- ifelse(is_extreme, 2, 1)

    animate_xy(extreme_data, col = extreme_cols, axes = "bottomleft")
}

# Tour 4: Pattern check (over vs under predictions)
cat("\n4. Over vs under-prediction patterns\n")
pattern_df <- data.frame(
    residual = all_res$LASSO,
    predicted = all_res$predicted,
    actual = all_res$actual,
    abs_residual = abs(all_res$LASSO),
    sq_residual = all_res$LASSO^2
)

sign_dir <- ifelse(pattern_df$residual > 0, 1, 2)
animate_xy(pattern_df, col = sign_dir, axes = "bottomleft")

# ---- Summary Statistics ----

# Top outliers
results_df <- data.frame(
    player_name = test$player_name,
    actual_wrc_plus = y_test,
    predicted_wrc_plus = lasso_pred,
    residual = y_test - lasso_pred
) %>%
    arrange(desc(abs(residual)))

cat("\n=== Top 10 Outliers ===\n")
print(results_df %>% head(10))

# Residual-predictor correlations
cat("\n=== Residual-Predictor Correlations ===\n")
res_cors <- sapply(predictors, function(p) {
    if (p %in% colnames(tour_df)) {
        cor(tour_df$LASSO, tour_df[[p]], use = "complete.obs")
    } else {
        NA
    }
})

cor_df <- data.frame(
    Predictor = names(res_cors),
    Correlation = round(res_cors, 4)
) %>% arrange(desc(abs(Correlation)))

print(cor_df)

# Normality test
cat("\n=== Residual Normality Test ===\n")
shapiro <- shapiro.test(all_res$LASSO)
cat(
    "Shapiro-Wilk: W =", round(shapiro$statistic, 4),
    ", p =", format(shapiro$p.value, scientific = TRUE), "\n"
)
cat("(p > 0.05 = normal)\n")

cat("\nResidual stats:\n")
print(summary(all_res$LASSO))

cat("\n=== Analysis Complete ===\n")
