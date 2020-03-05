######################################################################################
# Round 2 of Making Graphs for our Final Report #
# Andrew, Started 3/4/20 #
######################################################################################

### Packages ###

library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(lubridate) # load lubridate to work with dates and times
library(ggthemes) # ggplot themes
library(plm)# regression analysis package

################

### Grams lost versus steps taken, forcing regression to 0 ###

# Running a regression #

grams_steps_lm0 <- lm(mass_change ~ 0 + steps, data = tread_joined_weight_fixed)
summary(grams_steps_lm0)

# P value very very small ( < .001).  Best p we've found so far. Multiple R squared is .334

# Adding trendline to graph #

grams_steps_scatter_lm <- ggplot(tread_joined_weight_fixed, aes(x=`steps to miles`, y=mass_change))+
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

# First with lm. All observations #

mass_multi_lm1 <- lm(formula = mass_change ~ 0 + weight + `steps to miles`, data = tread_joined_weight_fixed) #linear model with weight as a contributing factor

summary(mass_multi_lm1)

lmeq1 = function(x){coef(mass_multi_lm1)[2]*x+coef(mass_multi_lm1)[1]}

#### p >> .001, R-squared now .415

mass_multi_lm1_plot <- ggplot(tread_joined_weight_fixed,aes(x=`steps to miles`,y=mass_change,color=weight))+
  geom_point() +
  stat_function(fun=lmeq1,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "All observations, forced to zero", y = "Mass Change (g)", x = "Distance Travelled (Miles)")

mass_multi_lm1_plot

### Now, removing all positive values for mass change

zero = 0

mass_joined_weight_fixed_nopos <- tread_joined_weight_fixed %>% 
  filter(mass_change < zero)

mass_multi_lm2 <- lm(formula = mass_change ~ 0 + weight + `steps to miles`, data = mass_joined_weight_fixed_nopos) #linear model with weight as a contributing factor

summary(mass_multi_lm2)

# p-value << 0.001. R-squared now .5395

lmeq2 = function(x){coef(mass_multi_lm2)[2]*x+coef(mass_multi_lm2)[1]} 

mass_multi_lm2_plot <- ggplot(mass_joined_weight_fixed_nopos,aes(x=`steps to miles`,y=mass_change,color=weight))+
  geom_point() +
  stat_function(fun=lmeq2,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to Zero, NO positive Values", y = "Mass Change (g)", x = "Distance Travelled (Miles)")

mass_multi_lm2_plot

########################################################################################

### Outliers using Cook's D ###

# Question for the team: Should this be based on rate of mass loss instead of total mass lost?

#####Find the outliers using Cook's D!#####
#graphs outliers but I can't figure out which observation is which
cooksdplot1 <- ols_plot_cooksd_chart(mass_multi_lm1)
cooksdplot2 <- ols_plot_cooksd_chart(mass_multi_lm2)

#find outlier values, this just prints the outliers from a boxplot
average_mass_outliers <- boxplot(mass_joined_weight_fixed_nopos$mass_change)
outvals = boxplot(mass_joined_weight_fixed_nopos$mass_change)$out
outvals # the outliers in nopos are -4.93965, -5.82240, -7.16285, -10.59740, -7.88275, -10.46510, -8.12925, -4.69680, -4.45265

### Make data frame with all Cook's D outliers removed

mass_joined_fixed_noout <- mass_joined_weight_fixed_nopos %>%
  filter(mass_change > -4.45265) 

### Fitting a model with outliers removed

mass_multi_lm3 <- lm(formula = mass_change ~ 0 + weight + `steps to miles`, data = mass_joined_fixed_noout) #linear model with weight as a contributing factor

summary(mass_multi_lm3)
# p << .001, R-squared = 0.7249

lmeq3 = function(x){coef(mass_multi_lm3)[2]*x+coef(mass_multi_lm3)[1]}

mass_multi_lm3_plot <- ggplot(mass_joined_fixed_noout,aes(x=`steps to miles`,y=mass_change,color=weight))+
  geom_point() +
  stat_function(fun=lmeq3,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to zero, No Outliers OR Positive Values", y = "Mass Change (g)", x = "Distance Travelled (Miles)")

mass_multi_lm3_plot
