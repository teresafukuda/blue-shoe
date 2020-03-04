#### Analysis started 3/4/20, looking at the data again using Allison's suggestions###########

###### multivariate linear model using weight ###

# First we need to join the data set with presurvey data that has the weight

library(tidyverse)
library(janitor)
library(estimatr)
library(olsrr)



pre_survey_data <- read_csv("Pre Survey Data.csv") 

pre_survey_data_weight <- pre_survey_data %>% 
  clean_names(.) %>% 
  select(weight, name) %>%
  mutate_if(is.character, str_to_upper) %>% #make everything upper case
  

name_ID_weight <- full_join(pre_survey_data_weight,clean_shoe_ID) %>% #join together name ID and weight in one data frame
  drop_na(weight)


tread_joined_weight <- full_join(name_ID_weight,full_data_joined_tread_norm) 
write_csv(tread_joined_weight, 'tread_joined_weight.csv') #DANNY CHEUNG was duplicated, tried to trouble shoot and couldn't figure it out so I fixed it in a CSV

#reload the fixed csv
tread_joined_weight_fixed <- read_csv("tread_joined_weight.csv")

#run the multivariate model using lm and lmrobust

weight_steps_lm1 <- lm(formula = average_tread_mass ~ 0 + weight + `steps to miles`, data = tread_joined_weight_fixed) #linear model with weight as a contributing factor

summary(weight_steps_lm1)
#pvalue<0.01

lm1equation = function(x){coef(weight_steps_lm1)[2]*x+coef(weight_steps_lm1)[1]} #multivariate equation above to be plotted in gplot, ask Allison if I am using the right coefficients

weight_steps_lm1_plot <- ggplot(tread_joined_weight_fixed,aes(x=`steps to miles`,y=average_tread_mass,color=weight))+
  geom_point() +
  stat_function(fun=lm1equation,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "All observations, forced to zero", y = "Tread Derived Mass Loss", x = "Distance Travelled (Steps to Miles")


weight_steps_lm1_plot


######Do the same thing after removing all positive values 
zero = 0

tread_joined_weight_fixed_nopos <- tread_joined_weight_fixed %>% 
  filter(average_tread_mass < zero)

weight_steps_lm2 <- lm(formula = average_tread_mass ~ 0 + weight + `steps to miles`, data = tread_joined_weight_fixed_nopos) #linear model with weight as a contributing factor

summary(weight_steps_lm2)
#pvalue<0.01

lm2equation = function(x){coef(weight_steps_lm2)[2]*x+coef(weight_steps_lm2)[1]} #multivariate equation above to be plotted in gplot, ask Allison if I am using the right coefficients

weight_steps_lm2_plot <- ggplot(tread_joined_weight_fixed_nopos,aes(x=`steps to miles`,y=average_tread_mass,color=weight))+
  geom_point() +
  stat_function(fun=lm2equation,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to Zer, NO positive Values", y = "Tread Derived Mass Loss", x = "Distance Travelled (Steps to Miles")

weight_steps_lm2_plot


#####Find the outliers using Cook's D!#####
#graphs outliers but I can't figure out which observation is which
cooksdplot1 <- ols_plot_cooksd_chart(weight_steps_lm1)
cooksdplot2 <- ols_plot_cooksd_chart(weight_steps_lm2)

#find outlier values, this just prints the outliers from a boxplot
average_tread_outliers <- boxplot(tread_joined_weight_fixed_nopos$average_tread_mass)
outvals = boxplot(tread_joined_weight_fixed_nopos$average_tread_mass)$out
outvals # the outliers in nopos is -6.636, -6.206

#make a data frame with no outliers OR positive values and run the model again and graph
tread_joined_fixed_noout <- tread_joined_weight_fixed_nopos %>%
  filter(shoe_ID != "L42") %>% 
  filter(shoe_ID != "R40") #this removed the outliers from 

weight_steps_lm3 <- lm(formula = average_tread_mass ~ 0 + weight + `steps to miles`, data = tread_joined_fixed_noout) #linear model with weight as a contributing factor

summary(weight_steps_lm3)
#pvalue<0.01

lm3equation = function(x){coef(weight_steps_lm3)[2]*x+coef(weight_steps_lm3)[1]}

weight_steps_lm3_plot <- ggplot(tread_joined_fixed_noout,aes(x=`steps to miles`,y=average_tread_mass,color=weight))+
  geom_point() +
  stat_function(fun=lm2equation,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to zero, No Outliers OR Positive Values", y = "Tread Derived Mass Loss", x = "Distance Travelled (Steps to Miles")

weight_steps_lm3_plot



