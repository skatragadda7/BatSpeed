library(tidyverse)
data1 <- read.csv('stats1.csv')
data2 <- read.csv('batting-stance.csv')

data1 <-data1 %>% left_join(data2,by=c('player_id','id'))

data1 <- data1 %>% mutate(zswing_minus_out = z_swing_percent-oz_swing_percent) %>% 
  mutate(meat_minus_out=meatball_swing_percent-oz_swing_percent)


# somewhat of a relationship
ggplot(data1,aes(x=zswing_minus_out,y=isolated_power)) + geom_point() + geom_smooth(method='lm')

cor(data1$isolated_power,data1$zswing_minus_out) # 0.25

# some relationship
ggplot(data1,aes(x=meat_minus_out,y=isolated_power)) + geom_point() + geom_smooth(method='lm')

cor(data1$isolated_power,data1$meat_minus_out) # 0.201

summary(data1$z_swing_percent)  # highest mean value of swing_percent which makes sense

summary(data1$oz_swing_percent) # lowest mean value

summary(data1$meatball_swing_percent) # 2nd highest mean value

summary(data1$zswing_minus_out)

summary(data1$meat_minus_out) #higher mean value than zone minus out of zone

ggplot(data1,aes(x=woba)) + geom_density() # is right skew

ggplot(data1,aes(x=log(woba))) + geom_density() # is right skew

ggplot(data1,aes(x=sqrt(woba))) + geom_density() # is right skew

