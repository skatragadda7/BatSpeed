library(tidyverse)
library(readxl)
library(tidymodels)
library(GGally)
library(corrr)
library(modelr)
library(car)

#Data 

fangraphs <- read.csv('fangraphs-leaderboards.csv')
fangraphs <- fangraphs %>% select(Season, Name, wRC., ) %>% filter(Season %in% c("2024", "2025")) %>% arrange(Name, Season)
head(fangraphs)

indep_vars_data <- read.csv('onlyCompleteData.csv')
indep_vars_data <- indep_vars_data %>% select(player.name:player_age, k_percent, isolated_power, avg_swing_speed, squared_up_contact, 
                                              avg_swing_length, attack_angle, ideal_angle_rate, exit_velocity_avg, 
                                              launch_angle_avg, sweet_spot_percent, barrel_batted_rate, hard_hit_percent, 
                                              z_swing_percent, oz_swing_percent, meatball_swing_percent, whiff_percent, 
                                              pull_percent, straightaway_percent, opposite_percent, groundballs_percent, flyballs_percent, linedrives_percent, popups_percent) %>% select(-WRC.)
head(indep_vars_data)

df <- indep_vars_data %>% left_join(fangraphs, by = c("player.name" = "Name", "year" = "Season")) %>% relocate(wRC., .before = "k_percent") %>% rename(WRC. = wRC.)
head(df)


train <- df %>% filter(year == "2024")
test <- df %>% filter(year == "2025")

train <- train %>% select(-c(player.name:player_age), WRC.) %>% distinct()
test <- test %>% select(-c(player.name:player_age), WRC.) %>% distinct()

# Exploratory Data Analysis

train.cor <- train %>%
  select(where(is.numeric)) %>%
  correlate() %>% rearrange() 

wrc_corrs <- train.cor %>% select(term, WRC.) %>% arrange(desc(abs(WRC.)))
wrc_corrs

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

LinReg.mod1 <- lm(WRC. ~ isolated_power 
                  + barrel_batted_rate 
                  + exit_velocity_avg 
                  + hard_hit_percent 
                  + avg_swing_speed
                  + flyballs_percent
                  + sweet_spot_percent
                  + groundballs_percent
                  + oz_swing_percent
                  + avg_swing_length, 
                  data = train)
summary(LinReg.mod1)

vif(LinReg.mod1)

drop1(LinReg.mod1, test="F")


LinReg.mod2 <- lm(WRC. ~ isolated_power 
                  + barrel_batted_rate 
                  + exit_velocity_avg 
                  + hard_hit_percent 
                  #+ avg_swing_speed
                  + flyballs_percent
                  + sweet_spot_percent
                  + groundballs_percent
                  + oz_swing_percent
                  + avg_swing_length, 
                  data = train)
summary(LinReg.mod2)

vif(LinReg.mod2)

drop1(LinReg.mod2, test="F")

LinReg.mod3 <- lm(WRC. ~ isolated_power 
                  + barrel_batted_rate 
                  + exit_velocity_avg 
                  + hard_hit_percent 
                  #+ avg_swing_speed
                  + flyballs_percent
                  + sweet_spot_percent
                  + groundballs_percent
                  + oz_swing_percent,
                  #+ avg_swing_length, 
                  data = train)
summary(LinReg.mod3)

vif(LinReg.mod3)

drop1(LinReg.mod3, test="F")

LinReg.mod4 <- lm(WRC. ~ isolated_power 
                  + barrel_batted_rate 
                  + exit_velocity_avg 
                  + hard_hit_percent 
                  #+ avg_swing_speed
                  + flyballs_percent
                  + sweet_spot_percent
                  #+ groundballs_percent
                  + oz_swing_percent,
                  #+ avg_swing_length, 
                  data = train)
summary(LinReg.mod4)

vif(LinReg.mod4)

drop1(LinReg.mod4, test="F")

# Potential Model 1

LinReg.mod5 <- lm(WRC. ~ isolated_power 
                  #+ barrel_batted_rate 
                  + exit_velocity_avg 
                  + hard_hit_percent 
                  #+ avg_swing_speed
                  + flyballs_percent
                  + sweet_spot_percent
                  #+ groundballs_percent
                  + oz_swing_percent,
                  #+ avg_swing_length, 
                  data = train)
summary(LinReg.mod5)

vif(LinReg.mod5)

drop1(LinReg.mod5, test="F")



LinReg.mod6 <- lm(WRC. ~ isolated_power 
                  #+ barrel_batted_rate 
                  + exit_velocity_avg 
                  + hard_hit_percent 
                  #+ avg_swing_speed
                  + flyballs_percent
                  + sweet_spot_percent,
                  #+ groundballs_percent
                  #+ oz_swing_percent,
                  #+ avg_swing_length, 
                  data = train)
summary(LinReg.mod6)

vif(LinReg.mod6)

drop1(LinReg.mod6, test="F")

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


LinReg.mod7 <- lm(WRC. ~ isolated_power 
                  #+ barrel_batted_rate 
                  + exit_velocity_avg + I(exit_velocity_avg^2)
                  + hard_hit_percent + I(hard_hit_percent^2)
                  #+ avg_swing_speed
                  + flyballs_percent + I(flyballs_percent^2)
                  + sweet_spot_percent + I(sweet_spot_percent^2),
                  #+ groundballs_percent
                  #+ oz_swing_percent,
                  #+ avg_swing_length, 
                  data = train)
summary(LinReg.mod7)

vif(LinReg.mod7)

drop1(LinReg.mod7, test="F")

#Interactions written out

LinReg.mod <- lm(WRC. ~ isolated_power 
                  #+ barrel_batted_rate 
                  + exit_velocity_avg 
                  + hard_hit_percent 
                  + avg_swing_speed
                  + isolated_power*exit_velocity_avg
                  + isolated_power*hard_hit_percent
                  + isolated_power*avg_swing_speed
                  + exit_velocity_avg*hard_hit_percent
                  + exit_velocity_avg*avg_swing_speed
                  + hard_hit_percent*avg_swing_speed
                  + isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
                  #+ flyballs_percent
                  #+ opposite_percent,
                  #+ groundballs_percent
                  #+ pull_percent,
                  #+ launch_angle_avg, 
                  data = train)
summary(LinReg.mod)

vif(LinReg.mod)

drop1(LinReg.mod, test="F")

LinReg.mod8 <- lm(WRC. ~ isolated_power 
                  #+ barrel_batted_rate 
                  + exit_velocity_avg + I(exit_velocity_avg^2)
                  + hard_hit_percent + I(hard_hit_percent^2)
                  #+ avg_swing_speed
                  + flyballs_percent #+ I(flyballs_percent^2)
                  + sweet_spot_percent + I(sweet_spot_percent^2),
                  #+ groundballs_percent
                  #+ oz_swing_percent,
                  #+ avg_swing_length, 
                  data = train)
summary(LinReg.mod8)

vif(LinReg.mod8)

drop1(LinReg.mod8, test="F")

LinReg.mod9 <- lm(WRC. ~ isolated_power 
                  #+ barrel_batted_rate 
                  + exit_velocity_avg + I(exit_velocity_avg^2)
                  + hard_hit_percent + I(hard_hit_percent^2)
                  #+ avg_swing_speed
                  + flyballs_percent #+ I(flyballs_percent^2)
                  + sweet_spot_percent + I(sweet_spot_percent^2)
                  + isolated_power*exit_velocity_avg
                  + isolated_power*hard_hit_percent
                  + isolated_power*avg_swing_speed
                  + exit_velocity_avg*hard_hit_percent
                  + exit_velocity_avg*avg_swing_speed
                  + hard_hit_percent*avg_swing_speed
                  + isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
                  #+ groundballs_percent
                  #+ oz_swing_percent,
                  #+ avg_swing_length, 
                  data = train)
summary(LinReg.mod9)

vif(LinReg.mod9)

drop1(LinReg.mod9, test="F")

LinReg.mod10 <- lm(WRC. ~ isolated_power 
                   #+ barrel_batted_rate 
                   + exit_velocity_avg + I(exit_velocity_avg^2)
                   + hard_hit_percent + I(hard_hit_percent^2)
                   #+ avg_swing_speed
                   + flyballs_percent #+ I(flyballs_percent^2)
                   + sweet_spot_percent + I(sweet_spot_percent^2)
                   + isolated_power*exit_velocity_avg
                   + isolated_power*hard_hit_percent
                   + isolated_power*avg_swing_speed
                   + exit_velocity_avg*hard_hit_percent
                   + exit_velocity_avg*avg_swing_speed
                   + hard_hit_percent*avg_swing_speed,
                   #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
                   #+ groundballs_percent
                   #+ oz_swing_percent,
                   #+ avg_swing_length, 
                   data = train)
summary(LinReg.mod10)

vif(LinReg.mod10, type = 'predictor')

drop1(LinReg.mod10, test="F")

LinReg.mod11 <- lm(WRC. ~ isolated_power 
                   #+ barrel_batted_rate 
                   + exit_velocity_avg + I(exit_velocity_avg^2)
                   + hard_hit_percent + I(hard_hit_percent^2)
                   #+ avg_swing_speed
                   + flyballs_percent #+ I(flyballs_percent^2)
                   + sweet_spot_percent + I(sweet_spot_percent^2)
                   + isolated_power*exit_velocity_avg
                   #+ isolated_power*hard_hit_percent
                   + isolated_power*avg_swing_speed
                   + exit_velocity_avg*hard_hit_percent
                   + exit_velocity_avg*avg_swing_speed
                   + hard_hit_percent*avg_swing_speed,
                   #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
                   #+ groundballs_percent
                   #+ oz_swing_percent,
                   #+ avg_swing_length, 
                   data = train)
summary(LinReg.mod11)

LinReg.mod12 <- lm(WRC. ~ isolated_power 
                   #+ barrel_batted_rate 
                   + exit_velocity_avg + I(exit_velocity_avg^2)
                   + hard_hit_percent + I(hard_hit_percent^2)
                   #+ avg_swing_speed
                   + flyballs_percent #+ I(flyballs_percent^2)
                   + sweet_spot_percent + I(sweet_spot_percent^2)
                   + isolated_power*exit_velocity_avg
                   #+ isolated_power*hard_hit_percent
                   + isolated_power*avg_swing_speed
                   + exit_velocity_avg*hard_hit_percent
                   #+ exit_velocity_avg*avg_swing_speed
                   + hard_hit_percent*avg_swing_speed,
                   #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
                   #+ groundballs_percent
                   #+ oz_swing_percent,
                   #+ avg_swing_length, 
                   data = train)
summary(LinReg.mod12)

vif(LinReg.mod12, type = 'predictor')

drop1(LinReg.mod12, test="F")

LinReg.mod13 <- lm(WRC. ~ isolated_power 
                   #+ barrel_batted_rate 
                   + exit_velocity_avg + I(exit_velocity_avg^2)
                   + hard_hit_percent + I(hard_hit_percent^2)
                   #+ avg_swing_speed
                   + flyballs_percent #+ I(flyballs_percent^2)
                   + sweet_spot_percent + I(sweet_spot_percent^2)
                   + isolated_power*exit_velocity_avg
                   #+ isolated_power*hard_hit_percent
                   + isolated_power*avg_swing_speed
                   #+ exit_velocity_avg*hard_hit_percent
                   #+ exit_velocity_avg*avg_swing_speed
                   + hard_hit_percent*avg_swing_speed,
                   #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
                   #+ groundballs_percent
                   #+ oz_swing_percent,
                   #+ avg_swing_length, 
                   data = train)
summary(LinReg.mod13)


LinReg.mod14 <- lm(WRC. ~ isolated_power 
                   #+ barrel_batted_rate 
                   + exit_velocity_avg + I(exit_velocity_avg^2)
                   + hard_hit_percent #+ I(hard_hit_percent^2)
                   #+ avg_swing_speed
                   + flyballs_percent #+ I(flyballs_percent^2)
                   + sweet_spot_percent + I(sweet_spot_percent^2)
                   + isolated_power*exit_velocity_avg
                   #+ isolated_power*hard_hit_percent
                   + isolated_power*avg_swing_speed
                   #+ exit_velocity_avg*hard_hit_percent
                   #+ exit_velocity_avg*avg_swing_speed
                   + hard_hit_percent*avg_swing_speed,
                   #+ isolated_power*exit_velocity_avg*hard_hit_percent*avg_swing_speed,
                   #+ groundballs_percent
                   #+ oz_swing_percent,
                   #+ avg_swing_length, 
                   data = train)
summary(LinReg.mod14)

vif(LinReg.mod14, type = 'predictor')

drop1(LinReg.mod14, test="F")


# Prediction

prediction_data_2024 <- df %>% filter(year == "2024") %>% 
  select(player.name:WRC.) %>% 
  distinct() %>% 
  mutate(pred_WRC. = LinReg.mod14$fitted.values, errors = LinReg.mod14$residuals) %>% 
  mutate(sum_pred_error = pred_WRC. + errors) %>%
  mutate(check = (sum_pred_error == WRC.))

head(prediction_data_2024)


plot(LinReg.mod14$residuals)
hist(LinReg.mod14$residuals)
plot(LinReg.mod14$fitted.values, LinReg.mod14$residuals)

residualPlots(LinReg.mod14)

prediction_data_2025 <- df %>% filter(year=="2025") %>% 
  select(player.name:WRC.) %>% 
  distinct() %>% 
  mutate(pred_WRC. = predict(LinReg.mod14, newdata = test 
                             %>% select(isolated_power, exit_velocity_avg, 
                                        hard_hit_percent, flyballs_percent, 
                                        sweet_spot_percent, avg_swing_speed))) %>% 
  mutate(errors = WRC. - pred_WRC.) %>% 
  mutate(sum_pred_error = pred_WRC. + errors) %>% 
  mutate(check = (sum_pred_error == WRC.))

head(prediction_data_2025)

plot(prediction_data_2025$errors)
hist(prediction_data_2025$errors)
plot(prediction_data_2025$pred_WRC., prediction_data_2025$errors)








