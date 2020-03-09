#### Analysis started 3/4/20, looking at the data again using Allison's suggestions###########

###### multivariate linear model using weight ###

# First we need to join the data set with presurvey data that has the weight

library(tidyverse)
library(janitor)
library(estimatr)
library(olsrr)



pre_survey_data <- read_csv("Pre Survey Data.csv") #read in survey data

pre_survey_data_weight <- pre_survey_data %>% 
  clean_names(.) %>% 
  select(weight, name) %>%
  mutate_if(is.character, str_to_upper) %>% #make everything upper case
  

name_ID_weight <- full_join(pre_survey_data_weight,clean_shoe_ID) %>% #join together name ID and weight in one data frame
  drop_na(weight)


tread_joined_weight <- full_join(name_ID_weight,full_data_joined_tread_norm) 
write_csv(tread_joined_weight, 'tread_joined_weight.csv') #DANNY CHEUNG was duplicated, tried to trouble shoot and couldn't figure it out so I fixed it in a CSV

#reload the fixed csv and remove all the individuals that didn't submit data
tread_joined_weight_fixed <- read_csv("tread_joined_weight.csv") %>% 
  filter(name != "CONTROL") %>% 
  filter(name != "BRI WINKLER") %>% 
  filter(name != "THOMAS BUTERA") %>% 
  filter(name != "TIMMY HUYNH") %>% 
  filter(name != "CURTIS BAUMANN") %>%
  filter(name != "SHIVA HASSON") %>%
  filter(name != "GARY FOX") %>% 
  filter(name != "LINDA HUYNH") 

#run the multivariate model using lm and lmrobust

weight_steps_lm1 <- lm(formula = average_tread_mass ~ 0 + weight + `steps to miles`, data = tread_joined_weight_fixed) #linear model with weight as a contributing factor

summary(weight_steps_lm1)
#pvalue<0.01
#weight -0.007312 
#steps to miles -0.006762 

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
#weight -0.009199 
#steps to miles -0.005944
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
outvals # the outliers in nopos is -8.43, -10.372

#make a data frame with no outliers OR positive values and run the model again and graph
tread_joined_fixed_noout <- tread_joined_weight_fixed_nopos %>%
  filter(shoe_ID != "L50") %>% 
  filter(shoe_ID != "R50") #this removed the outliers from data fram no positive DF


weight_steps_lm3 <- lm_robust(formula = average_tread_mass ~ 0 + weight + `steps to miles`, data = tread_joined_fixed_noout) #linear model with weight as a contributing factor

summary(weight_steps_lm3)
#pvalue<0.01
#weight -0.009206 
#steps to miles -0.004774

lm3equation = function(x){coef(weight_steps_lm3)[2]*x+coef(weight_steps_lm3)[1]}

weight_steps_lm3_plot <- ggplot(tread_joined_fixed_noout,aes(x=`steps to miles`,y=average_tread_mass,color=weight))+
  geom_point() +
  stat_function(fun=lm3equation,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-8, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to zero, No Outliers OR Positive Values", y = "Tread Derived Mass Loss", x = "Distance Travelled (Steps to Miles")

weight_steps_lm3_plot

####Count the sample sizes that we have for each group

tread_sample_size <- tread_joined_weight_fixed %>% 
  drop_na(average_tread_mass) %>% 
  group_by(model) %>% 
  summarize(
    sample_size = length(model)
  )

model
sample_size
#ADIDAS ASTRARUN (60A)	8
#ADIDAS F/22 PRIME KNIT	5
#ADIDAS NIZZA(50A SYN) WHITE	6
#ADIDAS NIZZA(60A) BLACK	14
#ADIDAS TERREX BOAT	8
#ADIDAS ULTRABOOST PARLEY BLACK	9
#ADIDAS ULTRABOOST PARLEY WHITE	9
#ADIDAS ULTRATECH	5
#PARLEY PRIME LTD ADIZERO BLACK	10
#PARLEY PRIME LTD ADIZERO WHITE	8
#ULTRA BOOST STELLA	6
  
#Sample size no positives
tread_sample_size_nopos <- tread_joined_weight_fixed_nopos %>% 
  group_by(model) %>% 
  summarize(
    sample_size = length(model)
  )

#ADIDAS ASTRARUN (60A)	8
#ADIDAS F/22 PRIME KNIT	5
#ADIDAS NIZZA(50A SYN) WHITE	6
#ADIDAS NIZZA(60A) BLACK	14
#ADIDAS TERREX BOAT	8
#ADIDAS ULTRABOOST PARLEY BLACK	9
#ADIDAS ULTRABOOST PARLEY WHITE	9
#ADIDAS ULTRATECH	5
#ADIDAS PRIME LTD ADIZERO BLACK	10
#ADIDAS PRIME LTD ADIZERO WHITE	8
#ADIDAS	ULTRA BOOST STELLA	6

#Table for sample sizes for the remaining sample numbers after no positives and 
tread_sample_size_noout_nopos <- tread_joined_fixed_noout %>% 
  group_by(model) %>% 
  summarize(
    sample_size = length(model)
  )
#ADIDAS ASTRARUN (60A)	8
#ADIDAS F/22 PRIME KNIT	5
#ADIDAS NIZZA(50A SYN) WHITE	8
#ADIDAS NIZZA(60A) BLACK	9
#ADIDAS TERREX BOAT	10
#ADIDAS ULTRABOOST PARLEY BLACK	9
#ADIDAS ULTRABOOST PARLEY WHITE	9
#ADIDAS ULTRATECH	5
#PARLEY PRIME LTD ADIZERO BLACK	9
#PARLEY PRIME LTD ADIZERO WHITE	7
#ULTRA BOOST STELLA	4
#NA is for filtered out individuals

