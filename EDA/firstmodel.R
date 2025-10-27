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
