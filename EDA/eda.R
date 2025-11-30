library(tidyverse)
library(readxl)
library(tidyverse)
library(readxl)
library(tidymodels)
library(GGally)
library(corrr)
library(modelr)
library(car)
library(janitor)

rmse <- yardstick::rmse

stats_full <- read_excel("stats.xlsx", sheet="stats")

stats_full <- stats_full %>% mutate(log_woba = log(woba),
                                    woba_reciprical = woba^-1)

bat_tracking_2024 <- read_excel("bat-tracking-2024.xlsx")

bat_tracking_2025 <- read_excel("bat-tracking-2025.xlsx")

stats_validation <- stats_full %>% filter(year %in% 2025)

stats <- stats_full %>% filter(year %in% 2024)

stats <- stats %>% left_join(bat_tracking_2024, by = c("player_id" = "id"))

stats_validation <- stats_validation %>% left_join(bat_tracking_2025, by = c("player_id" = "id"))

stats <- stats %>% select(-year.y) %>% rename(year = year.x)

stats_validation <- stats_validation %>% select(-year.y) %>% rename(year = year.x)

# Differences in Players

only_2025 <- setdiff(stats_validation$player_id, stats$player_id)
length(only_2025)

only_2024 <- setdiff(stats$player_id, stats_validation$player_id)
length(only_2024)

both_24_25 <- intersect(stats$player_id, stats_validation$player_id)
length(both_24_25)

only_2024_full <- setdiff(stats$player_id, bat_tracking_2024$id)
length(only_2024_full)

only_2025_full <- setdiff(stats_validation$player_id, bat_tracking_2025$id)
length(only_2025_full)


#both 2024 and 2025 have players unique to those years
#we will be using 2024 to predict 2025
#for those with data only in 2025, we can perhaps just use their stats for 2025 
#and see if the prediction is accurate

#3 players in 2024 stats are not in the 2024 bat tracking data
#Charlie Blackmon, J.D. Martinez, Mitch Haniger

# Charlie Blackmon is a hitter
# J.D. Martinez is outfielder and designated hitter
# Mitch Haniger hitter who plays as an outfielder and has also served as a designated hitter

# Split into training and testing

stats_split <- stats %>%
  initial_split(prop = 0.8)

stats.train <- training(stats_split)
stats.test <- testing(stats_split)


# woba as dependent



#Distribution of woba and xwoba

hist(stats.train$xwoba) #more symmetric than woba, but still right skew

hist(stats.train$woba) #not symmetric, very big right skew

hist(sqrt(stats.train$woba)) 

hist(log(stats.train$woba))

hist((stats.train$woba^-1))


# Empirical Density Plots


#woba 

plot(density(stats.train$woba), xlab = "woba", ylab = "Density", main = "Empirical 
     Density Function of woba")


x <- seq(-0.29, 0.5, by = 0.001)
y <- dnorm(x, mean = mean(stats.train$woba, na.rm = T), sd = sd(stats$woba, na.rm = T))

lines(x, y, lty=2)

# woba log

plot(density(log(stats.train$woba)), xlab = "woba", ylab = "Density", main = "Empirical 
     Density Function of log(woba)")


x <- seq(-1.4, -0.5, by = 0.001)
y <- dnorm(x, mean = mean(log(stats.train$woba), na.rm = T), sd = sd(log(stats$woba), na.rm = T))

lines(x, y, lty=2)


# woba reciprical

plot(density((stats.train$woba^-1)), xlab = "woba", ylab = "Density", 
     main = "Empirical Density Function of woba^-1")


x <- seq(2, 3.8, by = 0.001)
y <- dnorm(x, mean = mean((stats.train$woba^-1), na.rm = T), sd = sd((stats$woba)^-1, na.rm = T))

lines(x, y, lty=2)

#xwoba


plot(density(stats.train$xwoba), xlab = "xwoba", ylab = "Density", main = "Empirical 
     Density Function of xwoba")


x <- seq(-0.29, 0.5, by = 0.001)
y <- dnorm(x, mean = mean(stats$xwoba, na.rm = T), sd = sd(stats$xwoba, na.rm = T))

lines(x, y, lty=2)

# EDA

## Correlation matrix for quantitative variables

stats.cor <- stats.train %>% select(-c(player_id, `last_name, first_name`, year)) %>%
  select(where(is.numeric)) %>%
  correlate() %>% select(term, woba, xwoba, log_woba, woba_reciprical) %>% drop_na()


stats.cor.woba <- stats.cor %>% select(term, woba) %>% arrange(desc(abs(woba)))

stats.cor.log_woba <- stats.cor %>% select(term, log_woba) %>% arrange(desc(abs(log_woba)))

stats.cor.woba_reciprical <- stats.cor %>% select(term, woba_reciprical) %>% arrange(desc(abs(woba_reciprical)))


# Linear Regression

stats.train.woba <- stats.train %>% select(woba, stats.cor.woba$term[1:11])

LinReg.mod1 <- lm(woba ~ ., 
                  data = stats.train.woba)
summary(LinReg.mod1)

LinReg.mod2 <- lm(woba ~ on_base_plus_slg + 
                    on_base_percent + 
                    xslg + 
                    xobp + 
                    isolated_power + 
                    xiso +
                    b_rbi
                  , 
                  data = stats.train.woba)
summary(LinReg.mod2)



#Correlations


#Scatterplots and correlations

# 

# xslg could be response variable

# Multicollinearity: all above 0.595

stats %>%
  select(stats.cor$term[1:4],woba) %>%
  ggpairs()

# 

# More Unique variables: b_rbi, isolated power, xiso

# No Multicollinearity: 3 / 6 above 0.668

stats %>%
  select(stats.cor$term[5:8],woba) %>%
  ggpairs()

# 

# More unique variables: all

# Multicollinearity: all above 0.674

stats %>%
  select(stats.cor$term[9:12],woba) %>%
  ggpairs()

# 

# More unique variables: all

# Multicollinearity: all above 0.7

stats %>%
  select(stats.cor$term[13:16],woba) %>%
  ggpairs()

# 

# More unique variables: all

# Multicollinearity: all below 0.37 in magnitude

stats %>%
  select(stats.cor$term[17:20],woba) %>%
  ggpairs()

# Neither launch angle or attack angle or squared up have strong linear association

glm <- glm(woba ~ , data = stats, family = Gamma("inverse"))


mean((simdata$Y - predict(glm, newdata = simdata, type = "response"))^2)





# Random Forest

library(tidyverse)
library(readxl)
library(tidymodels)
library(modelr)
library(randomForest)

set.seed(4996)


## Modify data:
## 1. Remove model variable
## 2. Remove all rows with any NAs: they cannot be used
## 3. Make SmartWay a factor variable
stats <- stats %>%
  select(-Model) %>%
  drop_na() %>%
  mutate(SmartWay = as_factor(SmartWay))


## Divide data: training/validation/testing
veh.3div <- veh %>%
  initial_split(prop = 0.6)

veh.3div2 <- veh.3div %>%
  testing() %>%
  initial_split(prop = 0.5)

veh.train3 <- training(veh.3div)
veh.validate3 <- training(veh.3div2)
veh.test3 <- testing(veh.3div2)

veh.train3$SmartWay <- as.factor(veh.train3$SmartWay)
veh.validate3$SmartWay <- as.factor(veh.validate3$SmartWay)
## Build random forest
## Without air pollution score
## Two variables are eligible for a split at each node
RF.mod1 <- randomForest(SmartWay ~ Displ + Cyl + Trans + Drive + Fuel + Cert.Region + Stnd + 
                          Veh.Class + City.MPG + Hwy.MPG + Cmb.MPG + Greenhouse.Gas.Score,
                        data = veh.train3,
                        mtry = 2, importance = TRUE)


## Random forest information
## No. of variables shows the number of variables available for each split
## OOB estimate of error rate is an estimate of misclassification
## Misclassification error shown for each level with confusion matrix
RF.mod1


## Importance of predictor variables
## The values show the percent decrease in the following measures when each variable is not 
## available for a split 
##   a. prediction accuracy for each level of the response variable
##   b. overall prediction accuracy
##   c. node purity
RF.mod1 %>% 
  importance()


## Add predicted values to VALIDATION data
## Change default column names
## Potential issue: 
##   When the data is split, it is possible that a category that is present in one set is not 
##   present in other sets. In this case, prediction will not work.
RF.add <- veh.validate3 %>%
  gather_predictions(RF.mod1, type = "class") %>%
  rename(pred_SmartWay = pred)  %>%
  mutate(pred_SmartWay = as.factor( pred_SmartWay ))


## Confusion matrix
## Use validation data
RF.add %>%
  filter(model == "RF.mod1") %>%
  conf_mat(truth = SmartWay, estimate = pred_SmartWay)


## Accuracy, precision, and recall
## Use validation data
all_metrics <- metric_set(accuracy, precision, recall)

RF.add %>%
  group_by(model) %>%
  all_metrics(truth = SmartWay, estimate = pred_SmartWay)

#mean(stats$xwoba)

#mean(stats$woba)

#mlb_2024_avgs <- data.frame(
  #ba = .243,
 # xba = .242,
  #slg = .399,
 # xslg = .396,
 # woba = .310,
 # xwoba = .312
#)

#stats <- stats %>% mutate(
  #above_2024_mlb_xwoba_avg = ifelse(xwoba >= mlb_2024_avgs$xwoba, 1, 0)
#)





















# Dependent: xwoba


#stats <- stats %>%
  #drop_na()



stats

#Scatterplots and correlations

# "xslg", "on_base_plus_slg", "xobp",  "xiso" 

# xslg could be response variable

# Multicollinearity: all except xiso and xopb, xslg and xopb, every other above .7

stats %>%
  select(stats.cor$term[1:4],xwoba) %>%
  ggpairs()

#"on_base_percent", "exit_velocity_avg", "avg_hyper_speed", "xba"

# More Unique variables: exit_velocity_avg, avg_hyper_speed

# No Multicollinearity: all except exit_velocity_avg and avg_hyper_speed, xba and on_base_percentage

stats %>%
  select(stats.cor$term[5:8],xwoba) %>%
  ggpairs()

# blasts_swing, hard_hit_percent, avg_best_speed, isolated_power

# More unique variables: all

# Multicollinearity: all above 0.7 except isolated_power and blasts_swing

stats %>%
  select(stats.cor$term[9:12],xwoba) %>%
  ggpairs()

# barrel_batted_rate, blasts_contact, b_rbi, fast_swing_rate

# More unique variables: all

# Multicollinearity: 3 / 6 pairs above 0.7, the rest are .56 and below

stats %>%
  select(stats.cor$term[13:16],xwoba) %>%
  ggpairs()

# avg_swing_speed, bb_percent, poorlyweak_percent, pitch_count

# More unique variables: all

# Multicollinearity: all below 0.37 in magnitude

stats %>%
  select(stats.cor$term[17:20],xwoba) %>%
  ggpairs()

# solidcontact_percent, f_strike_percent, pa, xslgdiff

# More unique variables: all

# Multicollinearity: all below 0.2 in magnitude

stats %>%
  select(stats.cor$term[21:24],xwoba) %>%
  ggpairs()

# Neither launch angle or attack angle or squared up have strong linear association




