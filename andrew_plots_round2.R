######################################################################################
# Round 2 of Making Graphs for our Final Report #
# Andrew, Started 3/4/20 #
#####################################################################################

### Packages ###

library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(lubridate) # load lubridate to work with dates and times
library(ggthemes) # ggplot themes
library(plm)# regression analysis package

################

### Grams lost versus steps taken, forcing regression to 0 ###

# Running a regression #

grams_steps_lm0 <- lm(mass_change ~ 0 + steps, data = full_data_joined)
summary(grams_steps_lm0)

# P value very very small ( < .001).  Best p we've found so far. Multiple R squared is .334

# Adding trendline to graph #

grams_steps_scatter_lm <- ggplot(full_data_joined, aes(x=`steps to miles`, y=mass_change))+
  geom_point()+
  theme_bw()+
  labs(x = "Distance Travelled (miles)", y = "Mass Change (g)")+
  geom_abline(intercept=0, slope=grams_steps_lm0$coefficients[1], color='indianred1', size=1.1)+
  geom_hline(yintercept=c(0), color="darkblue")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

grams_steps_scatter_lm

######################################################################33

### Multivariate Regression ###

grams_steps_multi <- lm(mass_change ~ weight + steps, data = full_data_joined)
summary(grams_steps_multi)

# R-squared very small, p value much bigger than other models (p = 0.07) 


